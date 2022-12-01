{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.BackupStorage.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BackupStorage.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _KMSInvalidKeyUsageException,
    _DataAlreadyExistsException,
    _ServiceUnavailableException,
    _ResourceNotFoundException,
    _NotReadableInputStreamException,
    _ServiceInternalException,
    _ThrottlingException,
    _IllegalArgumentException,
    _RetryableException,

    -- * DataChecksumAlgorithm
    DataChecksumAlgorithm (..),

    -- * SummaryChecksumAlgorithm
    SummaryChecksumAlgorithm (..),

    -- * BackupObject
    BackupObject (..),
    newBackupObject,
    backupObject_chunksCount,
    backupObject_metadataString,
    backupObject_name,
    backupObject_objectChecksum,
    backupObject_objectChecksumAlgorithm,
    backupObject_objectToken,

    -- * Chunk
    Chunk (..),
    newChunk,
    chunk_index,
    chunk_length,
    chunk_checksum,
    chunk_checksumAlgorithm,
    chunk_chunkToken,
  )
where

import Amazonka.BackupStorage.Types.BackupObject
import Amazonka.BackupStorage.Types.Chunk
import Amazonka.BackupStorage.Types.DataChecksumAlgorithm
import Amazonka.BackupStorage.Types.SummaryChecksumAlgorithm
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-04-10@ of the Amazon Backup Storage SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "BackupStorage",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "backupstorage",
      Core.signingName = "backup-storage",
      Core.version = "2018-04-10",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "BackupStorage",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | Prism for AccessDeniedException' errors.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | Non-retryable exception. Indicates the KMS key usage is incorrect. See
-- exception message for details.
_KMSInvalidKeyUsageException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSInvalidKeyUsageException =
  Core._MatchServiceError
    defaultService
    "KMSInvalidKeyUsageException"
    Prelude.. Core.hasStatus 400

-- | Non-retryable exception. Attempted to create already existing object or
-- chunk. This message contains a checksum of already presented data.
_DataAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DataAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "DataAlreadyExistsException"
    Prelude.. Core.hasStatus 400

-- | Retryable exception, indicates internal server error.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | Non-retryable exception. Attempted to make an operation on non-existing
-- or expired resource.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | Retryalble exception. Indicated issues while reading an input stream due
-- to the networking issues or connection drop on the client side.
_NotReadableInputStreamException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotReadableInputStreamException =
  Core._MatchServiceError
    defaultService
    "NotReadableInputStreamException"
    Prelude.. Core.hasStatus 400

-- | Deprecated. To be removed from the model.
_ServiceInternalException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceInternalException =
  Core._MatchServiceError
    defaultService
    "ServiceInternalException"
    Prelude.. Core.hasStatus 500

-- | Increased rate over throttling limits. Can be retried with exponential
-- backoff.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | Non-retryable exception, indicates client error (wrong argument passed
-- to API). See exception message for details.
_IllegalArgumentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IllegalArgumentException =
  Core._MatchServiceError
    defaultService
    "IllegalArgumentException"
    Prelude.. Core.hasStatus 400

-- | Retryable exception. In general indicates internal failure that can be
-- fixed by retry.
_RetryableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RetryableException =
  Core._MatchServiceError
    defaultService
    "RetryableException"
    Prelude.. Core.hasStatus 500
