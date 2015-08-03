{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : Control.Monad.Error.AWS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module provides functions with a 'MonadError' constraint where the error
-- type is required to be an instance of 'AWSError'. This allows the
-- implicit lifting of errors into the base monad without having to explicitly
-- handle the 'Either' result that functions in "Control.Monad.Trans.AWS" return.
--
-- You can use 'catching' to catch specific or general errors using the
-- 'AWSError' 'Prism's. This strategy can also be used to obtain the pre-@1.0@
-- @*Catch@ function behaviour.
module Control.Monad.Error.AWS
    (
    -- * Running AWS Actions
      AWST
    , runAWST

    -- * Sending Requests
    -- ** Synchronous
    , Free.send
    , Free.await
    , Free.paginate
    -- ** Overriding Service Configuration
    , Free.sendWith
    , Free.awaitWith
    , Free.paginateWith
    -- ** Asynchronous
    -- $async
    -- ** Errors
    , hoistError
    , trying
    , catching
    ) where

import           Control.Monad.Catch          (MonadCatch)
import           Control.Monad.Error.Class    (MonadError)
import           Control.Monad.Error.Lens     (catching, throwing, trying)
import qualified Control.Monad.Trans.AWS      as AWST
import           Control.Monad.Trans.Except   (ExceptT, runExceptT)
import           Control.Monad.Trans.Resource
import           Network.AWS.Env
import qualified Network.AWS.Free             as Free
import           Network.AWS.Prelude

type AWST m = AWST.AWST (ExceptT Error m)

runAWST :: (MonadCatch m, MonadResource m, AWSEnv r)
        => r
        -> AWST m a
        -> m (Either Error a)
runAWST e = runExceptT . AWST.execAWST hoistError e

hoistError :: (MonadError e m, AWSError e) => Either Error a -> m a
hoistError = either (throwing _Error) return
