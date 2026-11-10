{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module Main (main) where

import           Control.Monad.Cont.Class          (MonadCont (..))
import           Control.Monad.Exception           (Exception, ExceptionT,
                                                    MonadException (..),
                                                    SomeException,
                                                    fromException,
                                                    runExceptionT)
import           Control.Monad.Exception.Instances ()
import           Control.Monad.RWS.Class           (MonadRWS)
import           Control.Monad.Reader.Class        (MonadReader (..))
import           Control.Monad.State.Class         (MonadState (..), modify)
import           Control.Monad.Trans.Cont          (Cont, runCont)
import           Control.Monad.Trans.RWS           (runRWS)
import           Control.Monad.Trans.Reader        (Reader, runReader)
import qualified Control.Monad.Trans.State         as S
import           Control.Monad.Trans.Writer        (Writer, runWriter)
import           Control.Monad.Writer.Class        (MonadWriter (..))
import           Test.Tasty                        (TestTree, defaultMain,
                                                    testGroup)
import           Test.Tasty.HUnit                  (testCase, (@?=))

main :: IO ()
main = defaultMain $ testGroup "ExceptionT instances"
    [ stateTests
    , readerTests
    , writerTests
    , contTests
    , rwsTests
    ]

data TestException = TestException
    deriving (Eq, Show)

instance Exception TestException

-- | Keep only the 'TestException' that a computation raised, so that results
-- can be compared.
caught :: Either SomeException a -> Either (Maybe TestException) a
caught = either (Left . fromException) Right

stateTests :: TestTree
stateTests = testGroup "MonadState"
    [ testCase "get returns the state" $
        runS get 1 @?= (Right 1, 1)
    , testCase "put replaces the state" $
        runS (put 2) 1 @?= (Right (), 2)
    , testCase "modify updates the state" $
        runS (modify (+ 1) >> get) 1 @?= (Right 2, 2)
    , testCase "an exception keeps earlier updates" $
        runS (put 2 >> throw TestException >> put 3) 1
            @?= (Left (Just TestException), 2)
    , testCase "a handler sees updates made before the exception" $
        runS ((put 2 >> throw TestException >> put 3 >> get)
                  `catch` \TestException -> get) 1
            @?= (Right 2, 2)
    ]
  where
    runS :: ExceptionT (S.State Int) a
         -> Int
         -> (Either (Maybe TestException) a, Int)
    runS m s = let (a, s') = S.runState (runExceptionT m) s in (caught a, s')

readerTests :: TestTree
readerTests = testGroup "MonadReader"
    [ testCase "ask returns the environment" $
        runR ask 1 @?= Right 1
    , testCase "local changes the environment only in its scope" $
        runR ((,) <$> local (+ 1) ask <*> ask) 1 @?= Right (2, 1)
    , testCase "local propagates exceptions" $
        runR (local (+ 1) (throw TestException >> ask)) 1
            @?= Left (Just TestException)
    , testCase "a handler outside local sees the outer environment" $
        runR (local (+ 1) (throw TestException >> ask)
                  `catch` \TestException -> ask) 1
            @?= Right 1
    ]
  where
    runR :: ExceptionT (Reader Int) a -> Int -> Either (Maybe TestException) a
    runR m r = caught (runReader (runExceptionT m) r)

writerTests :: TestTree
writerTests = testGroup "MonadWriter"
    [ testCase "tell appends output" $
        runW (tell "a" >> tell "b") @?= (Right (), "ab")
    , testCase "an exception keeps earlier output" $
        runW (tell "a" >> throw TestException >> tell "b")
            @?= (Left (Just TestException), "a")
    , testCase "listen returns the output of its argument" $
        runW (tell "x" >> listen (tell "a" >> tell "b"))
            @?= (Right ((), "ab"), "xab")
    , testCase "listen propagates exceptions and keeps output" $
        runW (listen (tell "a" >> throw TestException >> tell "b"))
            @?= (Left (Just TestException), "a")
    , testCase "pass applies the returned function to the output" $
        runW (pass (tell "ab" >> return ((), reverse))) @?= (Right (), "ba")
    , testCase "pass propagates exceptions and keeps output" $
        runW (pass (tell "ab" >> throw TestException >> return ((), reverse)))
            @?= (Left (Just TestException), "ab")
    ]
  where
    runW :: ExceptionT (Writer String) a
         -> (Either (Maybe TestException) a, String)
    runW m = let (a, w) = runWriter (runExceptionT m) in (caught a, w)

contTests :: TestTree
contTests = testGroup "MonadCont"
    [ testCase "callCC returns a normal result" $
        runC (callCC (\_ -> return 1)) @?= Right 1
    , testCase "an escape skips the rest of the callCC argument" $
        runC (do x <- callCC (\k -> k 1 >> throw TestException)
                 return (x + 1))
            @?= Right 2
    , testCase "callCC propagates exceptions" $
        runC (do x <- callCC (\_ -> throw TestException >> return 1)
                 return (x + 1))
            @?= Left (Just TestException)
    ]
  where
    runC :: ExceptionT (Cont (Either (Maybe TestException) Int)) Int
         -> Either (Maybe TestException) Int
    runC m = runCont (runExceptionT m) caught

rwsTests :: TestTree
rwsTests = testGroup "MonadRWS"
    [ testCase "reader, writer, and state operations combine" $
        let (a, s, w) = runRWS (runExceptionT rws) 2 3
        in (caught a, s, w) @?= (Right 5, 5, "x")
    ]
  where
    rws :: (MonadRWS m, EnvType m ~ Int, WriterType m ~ String,
            StateType m ~ Int)
        => m Int
    rws = do
        r <- ask
        tell "x"
        modify (+ r)
        get
