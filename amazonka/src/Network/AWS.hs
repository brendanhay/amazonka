{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : Network.AWS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The core module for making requests to the various AWS services.
module Network.AWS where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.Catch             (MonadCatch)
import           Control.Monad.Except
import           Control.Monad.Morph
import           Control.Monad.Reader
import qualified Control.Monad.State.Lazy        as LS
import qualified Control.Monad.State.Strict      as S
import           Control.Monad.Trans.AWS         as AWST
import           Control.Monad.Trans.AWS         (AWST, runAWST)
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Free.Church
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Resource
import           Data.Conduit                    hiding (await)
import           Data.Time                       (getCurrentTime)
import           Network.AWS.Data.Time
import           Network.AWS.Env                 hiding (once, timeout, within)
import qualified Network.AWS.Env                 as Env
import           Network.AWS.Error
import           Network.AWS.Free.IO
import           Network.AWS.Free.Program
import           Network.AWS.Free.Pure
import           Network.AWS.Pager
import           Network.AWS.Prelude
import qualified Network.AWS.Presign             as Sign
import           Network.AWS.Request             (requestURL)
import           Network.AWS.Waiter

type AWS = AWST IO

class Monad m => MonadAWS m where
    liftAWS :: AWS a -> m a

instance MonadAWS AWS where
    liftAWS = id

instance MonadAWS m => MonadAWS (IdentityT   m) where liftAWS = lift . liftAWS
instance MonadAWS m => MonadAWS (MaybeT      m) where liftAWS = lift . liftAWS
instance MonadAWS m => MonadAWS (ExceptT   e m) where liftAWS = lift . liftAWS
instance MonadAWS m => MonadAWS (ReaderT   r m) where liftAWS = lift . liftAWS
instance MonadAWS m => MonadAWS (S.StateT  s m) where liftAWS = lift . liftAWS
instance MonadAWS m => MonadAWS (LS.StateT s m) where liftAWS = lift . liftAWS

runAWS :: (MonadCatch m, MonadResource m) => Env -> AWS a -> m a
runAWS e = liftResourceT . runAWST e . hoist (withInternalState . const)

-- | Scope an action within the specific 'Region'.
within :: MonadAWS m => Region -> AWS a -> m a
within r = liftAWS . Env.within r

-- | Scope an action such that any retry logic for the 'Service' is
-- ignored and any requests will at most be sent once.
once :: MonadAWS m => AWS a -> m a
once = liftAWS . Env.once

-- | Scope an action such that any HTTP response use this timeout value.
timeout :: MonadAWS m => Seconds -> AWS a -> m a
timeout s = liftAWS . Env.timeout s

send :: (MonadAWS m, AWSRequest a) => a -> m (Either Error (Rs a))
send = liftAWS . AWST.send

paginate :: (MonadAWS m, AWSPager a) => a -> Source m (Either Error (Rs a))
paginate = hoist liftAWS . AWST.paginate

await :: (MonadAWS m, AWSRequest a) => Wait a -> a -> m (Either Error (Rs a))
await w = liftAWS . AWST.await w
