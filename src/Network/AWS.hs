{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

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
    -- * AWS Context
      AWS
    , runAWS

    -- * Credentials
    , Credentials      (..)

    -- * Regions
    , Region           (..)
    , within
    , getRegion

    -- * Debugging
    , getDebug
    , whenDebug

    -- * Synchronous Requests
    , send
    , sendCatch

    -- -- * Asynchronous Requests
    -- , async
    -- , wait
    -- , waitCatch

    -- -- * Paginated Requests
    -- , paginate
    -- , paginateCatch

    -- -- * Errors
    -- , ToError          (..)
    -- , AWSError         (..)
    -- , hoistError
    -- , liftEitherT

    -- -- * Types
    -- , AvailabilityZone (..)
    -- , Body             (..)
    -- , InstanceType     (..)
    -- , Items            (..)
    -- , Members          (..)
    ) where

import           Control.Applicative
import qualified Control.Concurrent.Async   as A
import           Control.Error
import           Control.Exception
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.Trans.Reader
import           Network.AWS.Internal

-- | Send a request and return the associated response type.
send :: (Rq a, ToError (Er a)) => a -> AWS (Rs a)
send = (hoistError . fmapL toError =<<) . sendCatch

sendCatch :: Rq a => a -> AWS (Either (Er a) (Rs a))
sendCatch rq = undefined -- do
  --   sig <- request rq
  --   dbg <- getDebug
  --   rs  <- sync $ perform sig dbg
  --   hoistError rs
  -- where
  --   sync = liftEitherT . fmapLT Ex . syncIO

  --   perform Signed{..} dbg =
  --       bracket (establishConnection sHost) closeConnection $ \c -> do
  --           when dbg $ do
  --               print sRequest
  --               print sBody
  --           b <- body sBody
  --           sendRequest c sRequest b

-- -- instead of calling receiveResponse here,
-- -- pass the connection back, and for s3 like responses provide the same signature
-- -- as the resulting body, think about finally/onException

--             response rq $ Response dbg code receiveResponse c receive

--     body (Strict   bs) = inputStreamBody <$> Streams.fromByteString bs
--     body (Streaming s) = return $ inputStreamBody s
--     body Empty         = return emptyBody

--     receive rs i = do
--         let code = getStatusCode rs
--             msg  = getStatusMessage rs
--         when dbg $ print rs
--         response rq $ Response dbg code msg (retrieveHeaders $ getHeaders rs) 

--             -- else do
--             --     bs <- BS.concat <$> Streams.toList i
--             --     return . Left . Err . BS.unpack $
--             --         BS.concat [BS.pack $ show c, " ", m, "\n", bs]

-- async :: AWS a -> AWS (A.Async (Either AWSError a))
-- async aws = AWS ask >>= liftIO . A.async . runEnv aws

-- wait :: ToError e => A.Async (Either AWSError (Either e a)) -> AWS a
-- wait a = waitCatch a >>= hoistError . fmapL toError

-- waitCatch :: A.Async (Either AWSError a) -> AWS a
-- waitCatch a = liftIO (A.waitCatch a) >>= hoistError . join . fmapL toError

-- -- | Create a pipes 'Producer' which yields the initial and subsequent repsonses
-- -- for requests that support pagination.
-- paginate :: (Rq a, Pg a, ToError (Er a))
--          => a
--          -> Producer' (Rs a) AWS ()
-- paginate = paginateCatch ~> either (lift . liftEitherT . left . toError) yield

-- paginateCatch :: (Rq a, Pg a, ToError (Er a))
--               => a
--               -> Producer' (Either (Er a) (Rs a)) AWS ()
-- paginateCatch = go . Just
--   where
--     go Nothing   = return ()
--     go (Just rq) = do
--         rs <- lift $ sendCatch rq
--         yield rs
--         either (const $ return ()) (go . next rq) rs
