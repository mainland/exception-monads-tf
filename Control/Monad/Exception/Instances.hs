-- Copyright (c) 2008-2010
--         The President and Fellows of Harvard College.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the University nor the names of its contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE UNIVERSITY OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.

--------------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Exception.Instances
-- Copyright   :  (c) Harvard University 2008-2010
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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
