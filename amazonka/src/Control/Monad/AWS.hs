{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}


{-# LANGUAGE MultiParamTypeClasses            #-}


-- Module      : Control.Monad.AWS
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Control.Monad.AWS
    (
    -- * AWS Monad
      AWS
    -- ** Concrete operations
    , runAWS
    , mapAWS

    -- ** Generalised operations
    , hoistAWS

    -- * Regionalisation
    , within

    -- * Synchronous requests
    -- ** Strict
    , send
    , sendCatch
    -- ** Streaming
    , with
    , withCatch
    -- ** Pagination
    , paginate
    , paginateCatch

    -- * Asynchronous actions
    , async
    , wait
    , waitCatch

    -- * Signing URLs
    , presign
    ) where

import           Control.Applicative
import           Control.Concurrent.Async    (Async)
import qualified Control.Concurrent.Async    as Async
import           Control.Lens
import           Control.Monad.Base
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Data.ByteString             (ByteString)
import           Data.Time
import           Network.AWS                 (Env(..), envRegion)
import qualified Network.AWS                 as AWS
import           Network.AWS.Types
import           Network.HTTP.Client

newtype AWS a = AWS { _unAWS :: ReaderT Env (ExceptT Error IO) a }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader Env
        , MonadError Error
        )

runAWS :: Auth -> Region -> Manager -> Logging -> AWS a -> IO (Either Error a)
runAWS a r m l (AWS k) = runExceptT (runReaderT k (Env a r m l))

mapAWS :: (IO (Either Error a) -> IO (Either Error b)) -> AWS a -> AWS b
mapAWS f = AWS . mapReaderT (mapExceptT f) . _unAWS

-- | HoistAWS an 'Either' throwing the 'Left' case, and returning the 'Right'.
hoistAWS :: (MonadError Error m, AWSError e) => Either e a -> m a
hoistAWS = either (throwError . awsError) return

-- | Pass the current environment to a function.
withEnv :: MonadReader Env m => (Env -> m a) -> m a
withEnv f = ask >>= f

-- | Regionalise a monadic action within the specific 'Region'.
within :: MonadReader Env m => Region -> m a -> m a
within r = local (envRegion .~ r)

send :: ( MonadBaseControl IO m
        , MonadReader Env m
        , MonadError Error m
        , AWSRequest a
        )
     => a
     -> m (Rs a)
send = hoistAWS <=< sendCatch

sendCatch :: ( MonadBaseControl IO m
             , MonadReader Env m
             , AWSRequest a
             )
          => a
          -> m (Either (Er (Sv a)) (Rs a))
sendCatch rq = withEnv $ \e -> AWS.send e rq

with :: ( MonadBaseControl IO m
        , MonadReader Env m
        , MonadError Error m
        , AWSRequest a
        )
     => a
     -> (Rs a -> m ByteString -> m b)
     -> m b
with rq = hoistAWS <=< withCatch rq

withCatch :: ( MonadBaseControl IO m
             , MonadReader Env m
             , AWSRequest a
             )
          => a
          -> (Rs a -> m ByteString -> m b)
          -> m (Either (Er (Sv a)) b)
withCatch rq f = withEnv $ \e -> AWS.with e rq f

paginate :: ( MonadBaseControl IO m
            , MonadReader Env m
            , MonadError Error m
            , AWSPager a
            )
         => a
         -> m (Rs a, Maybe a)
paginate = hoistAWS <=< paginateCatch

paginateCatch :: ( MonadBaseControl IO m
                 , MonadReader Env m
                 , AWSPager a
                 )
              => a
              -> m (Either (Er (Sv a)) (Rs a, Maybe a))
paginateCatch rq = withEnv $ \e -> AWS.paginate e rq

async :: (MonadBase IO m, MonadReader Env m) => AWS a -> m (Async (Either Error a))
async (AWS k) = ask >>= liftBase . Async.async . runExceptT . runReaderT k

wait :: (MonadBase IO m, MonadError Error m) => Async (Either Error a) -> m a
wait = hoistAWS <=< waitCatch

waitCatch :: MonadBase IO m => Async (Either Error a) -> m (Either Error a)
waitCatch = liftBase . Async.wait

presign :: ( MonadBase IO m
           , MonadReader Env m
           , AWSRequest a
           , AWSPresigner (Sg (Sv a))
           )
        => a
        -> UTCTime
        -> Int
        -> m (Signed a (Sg (Sv a)))
presign rq t x = withEnv $ \Env{..} ->
    AWS.presign _envAuth _envRegion (request rq) t x
