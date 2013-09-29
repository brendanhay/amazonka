{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- |
module Network.AWS
    (
    -- * Credentials
      Credentials(..)
    , credentials

    -- * AWS Context
    , runAWS

    -- * Regions
    , within

    -- * Pagination
    , paginate
    , paginateCatch

    -- * Synchronous
    , send
    , send_
    , sendCatch

    -- * Asynchronous
    , sendAsync
    , waitAsync
    , async
    , wait
    , wait_

    -- * Re-exported
    , module Network.AWS.Internal.Types
    ) where

import           Control.Applicative
import qualified Control.Conget.Async   as A
import           Control.Error
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.Aeson                 as Aeson
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy       as LBS
import           Network.AWS.EC2.Metadata
import           Network.AWS.Internal
import           Network.AWS.Internal.Types
import           Network.Http.Client
import           OpenSSL                    (withOpenSSL)
import           Pipes                      hiding (next)
import qualified System.IO.Streams          as Streams

data Credentials
    = FromKeys ByteString ByteString
    | FromRole ByteString

credentials :: (Applicative m, MonadIO m)
            => Credentials
            -> EitherT Error m Auth
credentials cred = case cred of
    FromKeys acc sec -> right $ Auth acc sec Nothing
    FromRole role    -> do
        m <- LBS.fromStrict <$> metadata (SecurityCredentials role)
        hoistEither . fmapL Error $ Aeson.eitherDecode m

runAWS :: Env -> AWS a -> IO (Either Error a)
runAWS env aws = withOpenSSL . runEitherT $ runReaderT (unwrap aws) env

-- | Run an 'AWS' operation inside a specific 'Region'.
within :: Region -> AWS a -> AWS a
within reg = AWS . local (\e -> e { awsRegion = Just reg }) . unwrap

-- | Create a pipes 'Producer' which yields the initial and subsequent repsonses
-- for requests that supported pagination.
paginate :: (Rq a, Pg a, ToError (Er a))
         => a
         -> Producer' (Rs a) AWS ()
paginate = paginateCatch ~> either (lift . liftEitherT . left . toError) yield

paginateCatch :: (Rq a, Pg a, ToError (Er a))
              => a
              -> Producer' (Either (Er a) (Rs a)) AWS ()
paginateCatch = go . Just
  where
    go Nothing   = return ()
    go (Just rq) = do
        rs <- lift $ sendCatch rq
        yield rs
        either (const $ return ()) (go . next rq) rs

-- | Send a request and return the associated response type.
send :: (Rq a, ToError (Er a)) => a -> AWS (Rs a)
send = (hoistError . fmapL toError =<<) . sendCatch

-- | Send a request ignoring the succesful response but propagating errors.
send_ :: (Rq a, ToError (Er a)) => a -> AWS ()
send_ = void . send

sendCatch :: Rq a => a -> AWS (Either (Er a) (Rs a))
sendCatch rq = do
    sig <- sign $ request rq
    whenDebug . print $ srqRequest sig
    res <- liftEitherT $ fromMaybe "" <$> sync sig
    whenDebug $ BS.putStrLn res
    hoistError $ response rq res
  where
    sync = fmapLT Ex . syncIO . req

    req SignedRequest{..} =
        bracket (establishConnection srqUrl) closeConnection $ \c -> do
            sendRequest c srqRequest =<< body srqPayload
            receiveResponse c $ const Streams.read

    body = maybe (return emptyBody)
        (fmap inputStreamBody . Streams.fromByteString)

sendAsync :: (Rq a, ToError (Er a))
          => a
          -> AWS (A.Async (Either Error (Either (Er a) (Rs a))))
sendAsync = async . sendCatch

waitAsync :: (Rq a, ToError (Er a))
          => A.Async (Either Error (Either (Er a) (Rs a)))
          -> AWS (Rs a)
waitAsync a = wait a >>= hoistError . fmapL toError

async :: AWS a -> AWS (A.Async (Either Error a))
async aws = getEnv >>= liftIO . A.async . flip runAWS aws

wait :: A.Async (Either Error a) -> AWS a
wait a = liftIO (A.waitCatch a) >>= hoistError . join . fmapL toError

wait_ :: A.Async (Either Error a) -> AWS ()
wait_ a = wait a >> return ()
