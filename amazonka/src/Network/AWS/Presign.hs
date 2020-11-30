{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Network.AWS.Presign
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- This module contains functions for presigning requests using 'MonadIO' and
-- not one of the AWS specific transformers.
--
-- It is intended for use directly with "Network.AWS.Auth" when only presigning
-- and no other AWS actions are required.
module Network.AWS.Presign where

import Control.Monad
import Control.Monad.IO.Class
import Network.AWS.Data.Headers (hExpect)
import Network.AWS.Data.Time
import Network.AWS.Lens ((%~), (&))
import Network.AWS.Prelude
import Network.AWS.Request (requestURL)
import Network.AWS.Types
import Network.HTTP.Types (Header)

-- | Presign an URL that is valid from the specified time until the
-- number of seconds expiry has elapsed.
--
-- /See:/ 'presign', 'presignWith'
presignURL ::
  (MonadIO m, AWSRequest a) =>
  Auth ->
  Region ->
  -- | Signing time.
  UTCTime ->
  -- | Expiry time.
  Seconds ->
  -- | Request to presign.
  a ->
  m ByteString
presignURL a r e ts = liftM requestURL . presign a r e ts

-- | Presign an HTTP request that is valid from the specified time until the
-- number of seconds expiry has elapsed.
--
-- /See:/ 'presignWith', 'presignWithHeaders'
presign ::
  (MonadIO m, AWSRequest a) =>
  Auth ->
  Region ->
  -- | Signing time.
  UTCTime ->
  -- | Expiry time.
  Seconds ->
  -- | Request to presign.
  a ->
  m ClientRequest
presign = presignWith id

-- | A variant of 'presign' that allows modifying the default 'Service'
-- definition used to configure the request.
--
-- /See:/ 'presignWithHeaders'
presignWith ::
  (MonadIO m, AWSRequest a) =>
  -- | Modify the default service configuration.
  (Service -> Service) ->
  Auth ->
  Region ->
  -- | Signing time.
  UTCTime ->
  -- | Expiry time.
  Seconds ->
  -- | Request to presign.
  a ->
  m ClientRequest
presignWith = presignWithHeaders defaultHeaders

-- | Modification to the headers that is applied by default (in 'presignWith');
-- removes the "Expect" header which is added to every 'PutObject'.
defaultHeaders :: [Header] -> [Header]
defaultHeaders = filter ((/= hExpect) . fst)

-- | A variant of 'presign' that allows modifying the default 'Headers'
-- and the default 'Service' definition used to configure the request.
presignWithHeaders ::
  (MonadIO m, AWSRequest a) =>
  -- | Modify the default headers.
  ([Header] -> [Header]) ->
  -- | Modify the default service configuration.
  (Service -> Service) ->
  Auth ->
  Region ->
  -- | Signing time.
  UTCTime ->
  -- | Expiry time.
  Seconds ->
  -- | Request to presign.
  a ->
  m ClientRequest
presignWithHeaders f g a r ts ex x =
  withAuth a $ \ae ->
    return $! sgRequest $
      rqPresign ex (request x & rqHeaders %~ f & rqService %~ g) ae r ts
