{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

-- Module      : Control.Monad.AWS
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Control.Monad.AWS where

import           Control.Applicative
import           Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as Async
import           Control.Error
import           Control.Exception.Lifted
import           Control.Lens
import           Control.Monad.Base
import           Control.Monad.Base
import           Control.Monad.Error             (MonadError, ErrorT, throwError)
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.List              (ListT)
import qualified Control.Monad.RWS.Lazy          as LRWS
import           Control.Monad.RWS.Strict        (RWST)
import           Control.Monad.Reader
import qualified Control.Monad.State.Lazy        as LState
import           Control.Monad.State.Strict      (StateT)
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Except
import qualified Control.Monad.Writer.Lazy       as LWriter
import           Control.Monad.Writer.Strict     (WriterT)
import           Data.ByteString                 (ByteString)
import           Data.Monoid
import           Data.Time
import           Network.AWS                     (Env(..), envRegion)
import qualified Network.AWS                     as AWS
import           Network.AWS.Auth
import           Network.AWS.Signing.Types       as AWS
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

mapAWS :: (IO (Either Error a) -> IO (Either Error b)) -> AWS a -> AWS b
mapAWS f = AWS . mapReaderT (mapExceptT f) . _unAWS

runAWS :: Auth -> Region -> Manager -> Logging -> AWS a -> IO (Either Error a)
runAWS a r m l (AWS k) = runExceptT (runReaderT k (Env a r m l))

-- | Pass the current environment to a function.
withEnv :: MonadReader Env m => (Env -> m a) -> m a
withEnv f = ask >>= f

-- | Regionalise an action inside a specific 'Region'.
within :: MonadReader Env m => Region -> m a -> m a
within r = local (envRegion .~ r)

-- | Hoist an 'Either' throwing the 'Left' case, and returning the 'Right'.
hoist :: (MonadError Error m, AWSError e) => Either e a -> m a
hoist = either (throwError . awsError) return

send :: ( MonadBaseControl IO m
        , MonadReader Env m
        , MonadError Error m
        , AWSRequest a
        )
     => a
     -> m (Rs a)
send = hoist <=< sendCatch

sendCatch :: ( MonadBaseControl IO m
             , MonadReader Env m
             , AWSRequest a
             )
          => a
          -> m (Either (Er (Sv a)) (Rs a))
sendCatch rq = withEnv $ \e -> AWS.send e rq

paginate :: ( MonadBaseControl IO m
            , MonadReader Env m
            , MonadError Error m
            , AWSPager a
            )
         => a
         -> m (Rs a, Maybe a)
paginate = hoist <=< paginateCatch

paginateCatch :: ( MonadBaseControl IO m
                 , MonadReader Env m
                 , AWSPager a
                 )
              => a
              -> m (Either (Er (Sv a)) (Rs a, Maybe a))
paginateCatch rq = withEnv $ \e -> AWS.paginate e rq

with :: ( MonadBaseControl IO m
        , MonadReader Env m
        , MonadError Error m
        , AWSRequest a
        )
     => a
     -> (Rs a -> m ByteString -> m b)
     -> m b
with rq = hoist <=< withCatch rq

withCatch :: ( MonadBaseControl IO m
             , MonadReader Env m
             , AWSRequest a
             )
          => a
          -> (Rs a -> m ByteString -> m b)
          -> m (Either (Er (Sv a)) b)
withCatch rq f = withEnv $ \e -> AWS.with e rq f

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

async :: (MonadBase IO m, MonadReader Env m => AWS a -> m (Async (Either Error a))
async (AWS r) = ask >>= liftBase . Async.async . runExceptT . runReaderT r

wait :: (MonadBase IO m, MonadError Error m) => Async (Either Error a) -> m a
wait = hoist <=< waitCatch

waitCatch :: MonadBase IO m => Async (Either Error a) -> m (Either Error a)
waitCatch = liftBase . Async.wait
