-- |
-- Module      : Amazonka.Core.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Re-export lenses and other optics for types in @amazonka-core@. You
-- will probably find record updates,
-- [generic-lens](https://hackage.haskell.org/package/generic-lens),
-- [generic-optics](https://hackage.haskell.org/package/generic-optics),
-- or (GHC >=9.2) @-XOverloadedRecordDot@ more ergonomic than these.
module Amazonka.Core.Lens
  ( -- * Amazonka.Data.Body

    -- ** ResponseBody
    _ResponseBody,

    -- ** ChunkSize
    _ChunkSize,

    -- ** ChunkedBody
    chunkedBody_size,
    chunkedBody_length,
    chunkedBody_body,

    -- * Amazonka.Data.Time
    _Time,

    -- * Amazonka.Types

    -- ** Abbrev
    _Abbrev,

    -- ** AccessKey
    _AccessKey,

    -- ** AuthEnv
    authEnv_accessKeyId,
    authEnv_secretAccessKey,
    authEnv_sessionToken,
    authEnv_expiration,

    -- ** Endpoint
    endpoint_host,
    endpoint_basePath,
    endpoint_secure,
    endpoint_port,
    endpoint_scope,

    -- ** ErrorCode
    _ErrorCode,

    -- ** ErrorMessage
    _ErrorMessage,

    -- ** Request
    request_service,
    request_method,
    request_path,
    request_query,
    request_headers,
    request_body,

    -- ** RequestId
    _RequestId,

    -- ** Retry
    retry_base,
    retry_growth,
    retry_attempts,
    retry_check,

    -- ** SecretKey
    _SecretKey,

    -- ** SessionToken
    _SessionToken,

    -- ** SerializeError
    serializeError_abbrev,
    serializeError_status,
    serializeError_body,
    serializeError_message,

    -- ** Service
    service_abbrev,
    service_signer,
    service_signingName,
    service_version,
    service_s3AddressingStyle,
    service_endpointPrefix,
    service_endpoint,
    service_timeout,
    service_check,
    service_error,
    service_retry,

    -- ** ServiceError
    serviceError_abbrev,
    serviceError_status,
    serviceError_headers,
    serviceError_code,
    serviceError_message,
    serviceError_requestId,

    -- ** Signed
    signed_signedMeta,
    signed_signedRequest,

    -- * Amazonka.Waiter

    -- ** Wait
    wait_name,
    wait_attempts,
    wait_delay,
    wait_acceptors,
  )
where

import Amazonka.Data.Body
import Amazonka.Data.Time
import Amazonka.Types
import Amazonka.Waiter
