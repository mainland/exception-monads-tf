{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      :  Control.Monad.Exception.Instances
-- Copyright   :  (c) Harvard University 2008-2010
--             :  (c) Geoffrey Mainland 2011-2014
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu

module Control.Monad.Exception.Instances where

import Control.Monad.Cont (MonadCont(..))
import Control.Monad.Exception (ExceptionT(..),
                                runExceptionT)
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Writer.Class (MonadWriter(..))
import Control.Monad.Trans.Class (MonadTrans(..))

--
-- monads-tf instances for transformed monads.
--

instance (MonadCont m) => MonadCont (ExceptionT m) where
    callCC f = ExceptionT $
        callCC $ \c ->
        runExceptionT (f (\a -> ExceptionT $ c (Right a)))

instance (MonadRWS m) => MonadRWS (ExceptionT m)

instance (MonadReader m) => MonadReader (ExceptionT m) where
    type EnvType (ExceptionT m) = EnvType m

    ask       = lift ask
    local f m = ExceptionT $ local f (runExceptionT m)

instance (MonadState m) => MonadState (ExceptionT m) where
    type StateType (ExceptionT m) = StateType m

    get = lift get
    put = lift . put

instance (MonadWriter m) => MonadWriter (ExceptionT m) where
    type WriterType (ExceptionT m) = WriterType m

    tell     = lift . tell
    listen m = ExceptionT $ do
        (a, w) <- listen (runExceptionT m)
        case a of
          Left  l -> return $ Left l
          Right r -> return $ Right (r, w)
    pass m   = ExceptionT $ pass $ do
        a <- runExceptionT m
        case a of
          Left l       -> return (Left l, id)
          Right (r, f) -> return (Right r, f)
