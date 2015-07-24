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
    -- * Backwards Compatibility
      AWST
    , runAWST

    -- * Lifted EC2 Metadata
    , metadata
    , dynamic
    , userdata

    -- * Lifted Requests
    , send
    , await
    , paginate

    -- * Throwing and Catching Errors
    , hoistError
    , catching
    , throwing
    ) where

import           Control.Applicative
import           Control.Monad.Catch          (MonadCatch)
import           Control.Monad.Error.Lens     (catching, throwing)
import           Control.Monad.Except
import qualified Control.Monad.Trans.AWS      as AWST
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.Resource
import           Data.Bifunctor
import           Data.Conduit                 (Source, (=$=))
import qualified Data.Conduit.List            as Conduit
import           Network.AWS.EC2.Metadata     (Dynamic, Metadata)
import           Network.AWS.Env
import           Network.AWS.Free             (Command)
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Waiter

-- | Type alias to ease transition for consumers of pre-@1.0@ amazonka.
type AWST m = AWST.AWST (ExceptT Error m)
{-# DEPRECATED AWST "Exists for backwards compatibility with pre-@1.0@." #-}

-- | Convenience function to ease transition for consumers of pre-@1.0@ amazonka.
runAWST :: (MonadCatch m, MonadResource m, AWSEnv r)
        => r
        -> AWST m a
        -> m (Either Error a)
runAWST e = runExceptT . AWST.runAWST e
{-# DEPRECATED runAWST "Exists for backwards compatibility with pre-@1.0@." #-}

hoistError :: (Applicative m, MonadError e m, AWSError e) => Either Error a -> m a
hoistError = either (throwing _Error) pure

-- | /See:/ 'AWST.metadata'
metadata :: ( MonadFree Command m
            , Applicative m
            , MonadError e m
            , AWSError e
            )
         => Metadata
         -> m (ByteString)
metadata = AWST.metadata >=> hoistError . first HTTPError

-- | /See:/ 'AWST.dynamic'
dynamic :: ( MonadFree Command m
           , Applicative m
           , MonadError e m
           , AWSError e
           )
        => Dynamic
        -> m ByteString
dynamic = AWST.dynamic >=> hoistError . first HTTPError

-- | /See:/ 'AWST.userdata'
userdata :: ( MonadFree Command m
            , Applicative m
            , MonadError e m
            , AWSError e
            )
         => m (Maybe ByteString)
userdata = AWST.userdata >>= hoistError . first HTTPError

-- | /See:/ 'AWST.send'
send :: ( MonadFree Command m
        , Applicative m
        , MonadError e m
        , AWSError e
        , AWSRequest a
        )
     => a
     -> m (Rs a)
send = AWST.send >=> hoistError

-- | /See:/ 'AWST.paginate'
paginate :: ( MonadFree Command m
            , Applicative m
            , MonadError e m
            , AWSError e
            , AWSPager a
            )
         => a
         -> Source m (Rs a)
paginate x = AWST.paginate x =$= Conduit.mapM hoistError

-- | /See:/ 'AWST.await'
await :: ( MonadFree Command m
         , Applicative m
         , MonadError e m
         , AWSError e
         , AWSRequest a
         )
      => Wait a
      -> a
      -> m (Rs a)
await w = AWST.await w >=> hoistError
