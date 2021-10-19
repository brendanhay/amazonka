{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.QLDB.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QLDB.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidParameterException,
    _ResourcePreconditionNotMetException,
    _ResourceAlreadyExistsException,
    _ResourceNotFoundException,
    _LimitExceededException,
    _ResourceInUseException,

    -- * EncryptionStatus
    EncryptionStatus (..),

    -- * ErrorCause
    ErrorCause (..),

    -- * ExportStatus
    ExportStatus (..),

    -- * LedgerState
    LedgerState (..),

    -- * PermissionsMode
    PermissionsMode (..),

    -- * S3ObjectEncryptionType
    S3ObjectEncryptionType (..),

    -- * StreamStatus
    StreamStatus (..),

    -- * JournalKinesisStreamDescription
    JournalKinesisStreamDescription (..),
    newJournalKinesisStreamDescription,
    journalKinesisStreamDescription_creationTime,
    journalKinesisStreamDescription_arn,
    journalKinesisStreamDescription_inclusiveStartTime,
    journalKinesisStreamDescription_errorCause,
    journalKinesisStreamDescription_exclusiveEndTime,
    journalKinesisStreamDescription_ledgerName,
    journalKinesisStreamDescription_roleArn,
    journalKinesisStreamDescription_streamId,
    journalKinesisStreamDescription_status,
    journalKinesisStreamDescription_kinesisConfiguration,
    journalKinesisStreamDescription_streamName,

    -- * JournalS3ExportDescription
    JournalS3ExportDescription (..),
    newJournalS3ExportDescription,
    journalS3ExportDescription_ledgerName,
    journalS3ExportDescription_exportId,
    journalS3ExportDescription_exportCreationTime,
    journalS3ExportDescription_status,
    journalS3ExportDescription_inclusiveStartTime,
    journalS3ExportDescription_exclusiveEndTime,
    journalS3ExportDescription_s3ExportConfiguration,
    journalS3ExportDescription_roleArn,

    -- * KinesisConfiguration
    KinesisConfiguration (..),
    newKinesisConfiguration,
    kinesisConfiguration_aggregationEnabled,
    kinesisConfiguration_streamArn,

    -- * LedgerEncryptionDescription
    LedgerEncryptionDescription (..),
    newLedgerEncryptionDescription,
    ledgerEncryptionDescription_inaccessibleKmsKeyDateTime,
    ledgerEncryptionDescription_kmsKeyArn,
    ledgerEncryptionDescription_encryptionStatus,

    -- * LedgerSummary
    LedgerSummary (..),
    newLedgerSummary,
    ledgerSummary_state,
    ledgerSummary_name,
    ledgerSummary_creationDateTime,

    -- * S3EncryptionConfiguration
    S3EncryptionConfiguration (..),
    newS3EncryptionConfiguration,
    s3EncryptionConfiguration_kmsKeyArn,
    s3EncryptionConfiguration_objectEncryptionType,

    -- * S3ExportConfiguration
    S3ExportConfiguration (..),
    newS3ExportConfiguration,
    s3ExportConfiguration_bucket,
    s3ExportConfiguration_prefix,
    s3ExportConfiguration_encryptionConfiguration,

    -- * ValueHolder
    ValueHolder (..),
    newValueHolder,
    valueHolder_ionText,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.QLDB.Types.EncryptionStatus
import Network.AWS.QLDB.Types.ErrorCause
import Network.AWS.QLDB.Types.ExportStatus
import Network.AWS.QLDB.Types.JournalKinesisStreamDescription
import Network.AWS.QLDB.Types.JournalS3ExportDescription
import Network.AWS.QLDB.Types.KinesisConfiguration
import Network.AWS.QLDB.Types.LedgerEncryptionDescription
import Network.AWS.QLDB.Types.LedgerState
import Network.AWS.QLDB.Types.LedgerSummary
import Network.AWS.QLDB.Types.PermissionsMode
import Network.AWS.QLDB.Types.S3EncryptionConfiguration
import Network.AWS.QLDB.Types.S3ExportConfiguration
import Network.AWS.QLDB.Types.S3ObjectEncryptionType
import Network.AWS.QLDB.Types.StreamStatus
import Network.AWS.QLDB.Types.ValueHolder
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2019-01-02@ of the Amazon QLDB SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "QLDB",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "qldb",
      Core._serviceSigningName = "qldb",
      Core._serviceVersion = "2019-01-02",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "QLDB",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | One or more parameters in the request aren\'t valid.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"
    Prelude.. Core.hasStatus 400

-- | The operation failed because a condition wasn\'t satisfied in advance.
_ResourcePreconditionNotMetException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourcePreconditionNotMetException =
  Core._MatchServiceError
    defaultService
    "ResourcePreconditionNotMetException"
    Prelude.. Core.hasStatus 412

-- | The specified resource already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"
    Prelude.. Core.hasStatus 409

-- | The specified resource doesn\'t exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | You have reached the limit on the maximum number of resources allowed.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 400

-- | The specified resource can\'t be modified at this time.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"
    Prelude.. Core.hasStatus 409
