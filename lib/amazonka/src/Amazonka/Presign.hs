-- |
-- Module      : Amazonka.Presign
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- This module contains functions for presigning requests using 'MonadIO' and
-- not one of the AWS specific transformers.
--
-- It is intended for use directly with "Amazonka.Auth" when only presigning
-- and no other AWS actions are required.

{-# LANGUAGE BangPatterns #-}

module Amazonka.Presign where

import Amazonka.Data
import Amazonka.Prelude
import Amazonka.Request (clientRequestURL)
import Amazonka.Types hiding (presign)
import qualified Network.HTTP.Types as HTTP

-- | Presign an URL that is valid from the specified time until the
-- number of seconds expiry has elapsed.
--
-- /See:/ 'presign', 'presignWith'
presignURL ::
  (AWSRequest a) =>
  AuthEnv ->
  Region ->
  -- | Signing time.
  UTCTime ->
  -- | Expiry time.
  Seconds ->
  -- | Request to presign.
  a ->
  ByteString
presignURL a r e ts = clientRequestURL . presign a r e ts

-- | Presign an HTTP request that is valid from the specified time until the
-- number of seconds expiry has elapsed.
--
-- /See:/ 'presignWith', 'presignWithHeaders'
presign ::
  (AWSRequest a) =>
  AuthEnv ->
  Region ->
  -- | Signing time.
  UTCTime ->
  -- | Expiry time.
  Seconds ->
  -- | Request to presign.
  a ->
  ClientRequest
presign =
  presignWith id

-- | A variant of 'presign' that allows modifying the default 'Service'
-- definition used to configure the request.
--
-- /See:/ 'presignWithHeaders'
presignWith ::
  (AWSRequest a) =>
  -- | Modify the default service configuration.
  (Service -> Service) ->
  AuthEnv ->
  Region ->
  -- | Signing time.
  UTCTime ->
  -- | Expiry time.
  Seconds ->
  -- | Request to presign.
  a ->
  ClientRequest
presignWith = presignWithHeaders defaultHeaders

-- | Modification to the headers that is applied by default (in 'presignWith');
-- removes the "Expect" header which is added to every 'PutObject'.
defaultHeaders :: [HTTP.Header] -> [HTTP.Header]
defaultHeaders = filter ((/= hExpect) . fst)

-- | A variant of 'presign' that allows modifying the default 'Headers'
-- and the default 'Service' definition used to configure the request.
presignWithHeaders ::
  forall a.
  (AWSRequest a) =>
  -- | Modify the default headers.
  ([Header] -> [Header]) ->
  -- | Modify the default service configuration.
  (Service -> Service) ->
  AuthEnv ->
  Region ->
  -- | Signing time.
  UTCTime ->
  -- | Expiry time.
  Seconds ->
  -- | Request to presign.
  a ->
  ClientRequest
presignWithHeaders f g ae r ts ex x =
  let rq@Request {headers} = request g x
      rq' :: Request a
      rq' = rq {headers = f headers}
      !creq = signedRequest $ requestPresign ex rq' ae r ts
   in creq
