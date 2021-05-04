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
    _ResourceAlreadyExistsException,
    _InvalidParameterException,
    _ResourceInUseException,
    _LimitExceededException,
    _ResourceNotFoundException,
    _ResourcePreconditionNotMetException,

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
    journalKinesisStreamDescription_inclusiveStartTime,
    journalKinesisStreamDescription_errorCause,
    journalKinesisStreamDescription_arn,
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.QLDB.Types.ErrorCause
import Network.AWS.QLDB.Types.ExportStatus
import Network.AWS.QLDB.Types.JournalKinesisStreamDescription
import Network.AWS.QLDB.Types.JournalS3ExportDescription
import Network.AWS.QLDB.Types.KinesisConfiguration
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
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "QLDB",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcEndpointPrefix = "qldb",
      Prelude._svcSigningName = "qldb",
      Prelude._svcVersion = "2019-01-02",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError = Prelude.parseJSONError "QLDB",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified resource already exists.
_ResourceAlreadyExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceAlreadyExistsException =
  Prelude._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"
    Prelude.. Prelude.hasStatus 409

-- | One or more parameters in the request aren\'t valid.
_InvalidParameterException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidParameterException =
  Prelude._MatchServiceError
    defaultService
    "InvalidParameterException"
    Prelude.. Prelude.hasStatus 400

-- | The specified resource can\'t be modified at this time.
_ResourceInUseException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceInUseException =
  Prelude._MatchServiceError
    defaultService
    "ResourceInUseException"
    Prelude.. Prelude.hasStatus 409

-- | You have reached the limit on the maximum number of resources allowed.
_LimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Prelude.hasStatus 400

-- | The specified resource doesn\'t exist.
_ResourceNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Prelude.hasStatus 404

-- | The operation failed because a condition wasn\'t satisfied in advance.
_ResourcePreconditionNotMetException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourcePreconditionNotMetException =
  Prelude._MatchServiceError
    defaultService
    "ResourcePreconditionNotMetException"
    Prelude.. Prelude.hasStatus 412
