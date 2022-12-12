{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ImportExport.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImportExport.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _BucketPermissionException,
    _CanceledJobIdException,
    _CreateJobQuotaExceededException,
    _ExpiredJobIdException,
    _InvalidAccessKeyIdException,
    _InvalidAddressException,
    _InvalidCustomsException,
    _InvalidFileSystemException,
    _InvalidJobIdException,
    _InvalidManifestFieldException,
    _InvalidParameterException,
    _InvalidVersionException,
    _MalformedManifestException,
    _MissingCustomsException,
    _MissingManifestFieldException,
    _MissingParameterException,
    _MultipleRegionsException,
    _NoSuchBucketException,
    _UnableToCancelJobIdException,
    _UnableToUpdateJobIdException,

    -- * JobType
    JobType (..),

    -- * Artifact
    Artifact (..),
    newArtifact,
    artifact_description,
    artifact_url,

    -- * Job
    Job (..),
    newJob,
    job_jobType,
    job_jobId,
    job_isCanceled,
    job_creationDate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ImportExport.Types.Artifact
import Amazonka.ImportExport.Types.Job
import Amazonka.ImportExport.Types.JobType
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V2 as Sign

-- | API version @2010-06-01@ of the Amazon Import/Export SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "ImportExport",
      Core.signer = Sign.v2,
      Core.endpointPrefix = "importexport",
      Core.signingName = "importexport",
      Core.version = "2010-06-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseXMLError "ImportExport",
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
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | The account specified does not have the appropriate bucket permissions.
_BucketPermissionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BucketPermissionException =
  Core._MatchServiceError
    defaultService
    "BucketPermissionException"

-- | The specified job ID has been canceled and is no longer valid.
_CanceledJobIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CanceledJobIdException =
  Core._MatchServiceError
    defaultService
    "CanceledJobIdException"

-- | Each account can create only a certain number of jobs per day. If you
-- need to create more than this, please contact
-- awsimportexport\@amazon.com to explain your particular use case.
_CreateJobQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CreateJobQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "CreateJobQuotaExceededException"

-- | Indicates that the specified job has expired out of the system.
_ExpiredJobIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ExpiredJobIdException =
  Core._MatchServiceError
    defaultService
    "ExpiredJobIdException"

-- | The AWS Access Key ID specified in the request did not match the
-- manifest\'s accessKeyId value. The manifest and the request
-- authentication must use the same AWS Access Key ID.
_InvalidAccessKeyIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAccessKeyIdException =
  Core._MatchServiceError
    defaultService
    "InvalidAccessKeyIdException"

-- | The address specified in the manifest is invalid.
_InvalidAddressException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAddressException =
  Core._MatchServiceError
    defaultService
    "InvalidAddressException"

-- | One or more customs parameters was invalid. Please correct and resubmit.
_InvalidCustomsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCustomsException =
  Core._MatchServiceError
    defaultService
    "InvalidCustomsException"

-- | File system specified in export manifest is invalid.
_InvalidFileSystemException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidFileSystemException =
  Core._MatchServiceError
    defaultService
    "InvalidFileSystemException"

-- | The JOBID was missing, not found, or not associated with the AWS
-- account.
_InvalidJobIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidJobIdException =
  Core._MatchServiceError
    defaultService
    "InvalidJobIdException"

-- | One or more manifest fields was invalid. Please correct and resubmit.
_InvalidManifestFieldException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidManifestFieldException =
  Core._MatchServiceError
    defaultService
    "InvalidManifestFieldException"

-- | One or more parameters had an invalid value.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | The client tool version is invalid.
_InvalidVersionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidVersionException =
  Core._MatchServiceError
    defaultService
    "InvalidVersionException"

-- | Your manifest is not well-formed.
_MalformedManifestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MalformedManifestException =
  Core._MatchServiceError
    defaultService
    "MalformedManifestException"

-- | One or more required customs parameters was missing from the manifest.
_MissingCustomsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MissingCustomsException =
  Core._MatchServiceError
    defaultService
    "MissingCustomsException"

-- | One or more required fields were missing from the manifest file. Please
-- correct and resubmit.
_MissingManifestFieldException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MissingManifestFieldException =
  Core._MatchServiceError
    defaultService
    "MissingManifestFieldException"

-- | One or more required parameters was missing from the request.
_MissingParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MissingParameterException =
  Core._MatchServiceError
    defaultService
    "MissingParameterException"

-- | Your manifest file contained buckets from multiple regions. A job is
-- restricted to buckets from one region. Please correct and resubmit.
_MultipleRegionsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MultipleRegionsException =
  Core._MatchServiceError
    defaultService
    "MultipleRegionsException"

-- | The specified bucket does not exist. Create the specified bucket or
-- change the manifest\'s bucket, exportBucket, or logBucket field to a
-- bucket that the account, as specified by the manifest\'s Access Key ID,
-- has write permissions to.
_NoSuchBucketException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NoSuchBucketException =
  Core._MatchServiceError
    defaultService
    "NoSuchBucketException"

-- | AWS Import\/Export cannot cancel the job
_UnableToCancelJobIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnableToCancelJobIdException =
  Core._MatchServiceError
    defaultService
    "UnableToCancelJobIdException"

-- | AWS Import\/Export cannot update the job
_UnableToUpdateJobIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnableToUpdateJobIdException =
  Core._MatchServiceError
    defaultService
    "UnableToUpdateJobIdException"
