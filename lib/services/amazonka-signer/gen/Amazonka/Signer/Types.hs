{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Signer.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Signer.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _NotFoundException,
    _ResourceNotFoundException,
    _ConflictException,
    _ThrottlingException,
    _ServiceLimitExceededException,
    _BadRequestException,
    _ValidationException,
    _InternalServiceErrorException,
    _TooManyRequestsException,

    -- * Category
    Category (..),

    -- * EncryptionAlgorithm
    EncryptionAlgorithm (..),

    -- * HashAlgorithm
    HashAlgorithm (..),

    -- * ImageFormat
    ImageFormat (..),

    -- * SigningProfileStatus
    SigningProfileStatus (..),

    -- * SigningStatus
    SigningStatus (..),

    -- * ValidityType
    ValidityType (..),

    -- * Destination
    Destination (..),
    newDestination,
    destination_s3,

    -- * EncryptionAlgorithmOptions
    EncryptionAlgorithmOptions (..),
    newEncryptionAlgorithmOptions,
    encryptionAlgorithmOptions_allowedValues,
    encryptionAlgorithmOptions_defaultValue,

    -- * HashAlgorithmOptions
    HashAlgorithmOptions (..),
    newHashAlgorithmOptions,
    hashAlgorithmOptions_allowedValues,
    hashAlgorithmOptions_defaultValue,

    -- * Permission
    Permission (..),
    newPermission,
    permission_principal,
    permission_statementId,
    permission_profileVersion,
    permission_action,

    -- * S3Destination
    S3Destination (..),
    newS3Destination,
    s3Destination_bucketName,
    s3Destination_prefix,

    -- * S3SignedObject
    S3SignedObject (..),
    newS3SignedObject,
    s3SignedObject_key,
    s3SignedObject_bucketName,

    -- * S3Source
    S3Source (..),
    newS3Source,
    s3Source_bucketName,
    s3Source_key,
    s3Source_version,

    -- * SignatureValidityPeriod
    SignatureValidityPeriod (..),
    newSignatureValidityPeriod,
    signatureValidityPeriod_type,
    signatureValidityPeriod_value,

    -- * SignedObject
    SignedObject (..),
    newSignedObject,
    signedObject_s3,

    -- * SigningConfiguration
    SigningConfiguration (..),
    newSigningConfiguration,
    signingConfiguration_encryptionAlgorithmOptions,
    signingConfiguration_hashAlgorithmOptions,

    -- * SigningConfigurationOverrides
    SigningConfigurationOverrides (..),
    newSigningConfigurationOverrides,
    signingConfigurationOverrides_encryptionAlgorithm,
    signingConfigurationOverrides_hashAlgorithm,

    -- * SigningImageFormat
    SigningImageFormat (..),
    newSigningImageFormat,
    signingImageFormat_supportedFormats,
    signingImageFormat_defaultFormat,

    -- * SigningJob
    SigningJob (..),
    newSigningJob,
    signingJob_jobOwner,
    signingJob_jobInvoker,
    signingJob_profileName,
    signingJob_signedObject,
    signingJob_platformDisplayName,
    signingJob_jobId,
    signingJob_status,
    signingJob_profileVersion,
    signingJob_signingMaterial,
    signingJob_source,
    signingJob_isRevoked,
    signingJob_signatureExpiresAt,
    signingJob_createdAt,
    signingJob_platformId,

    -- * SigningJobRevocationRecord
    SigningJobRevocationRecord (..),
    newSigningJobRevocationRecord,
    signingJobRevocationRecord_reason,
    signingJobRevocationRecord_revokedAt,
    signingJobRevocationRecord_revokedBy,

    -- * SigningMaterial
    SigningMaterial (..),
    newSigningMaterial,
    signingMaterial_certificateArn,

    -- * SigningPlatform
    SigningPlatform (..),
    newSigningPlatform,
    signingPlatform_signingImageFormat,
    signingPlatform_partner,
    signingPlatform_signingConfiguration,
    signingPlatform_displayName,
    signingPlatform_target,
    signingPlatform_category,
    signingPlatform_revocationSupported,
    signingPlatform_platformId,
    signingPlatform_maxSizeInMB,

    -- * SigningPlatformOverrides
    SigningPlatformOverrides (..),
    newSigningPlatformOverrides,
    signingPlatformOverrides_signingImageFormat,
    signingPlatformOverrides_signingConfiguration,

    -- * SigningProfile
    SigningProfile (..),
    newSigningProfile,
    signingProfile_tags,
    signingProfile_signatureValidityPeriod,
    signingProfile_profileName,
    signingProfile_signingParameters,
    signingProfile_profileVersionArn,
    signingProfile_platformDisplayName,
    signingProfile_arn,
    signingProfile_status,
    signingProfile_profileVersion,
    signingProfile_signingMaterial,
    signingProfile_platformId,

    -- * SigningProfileRevocationRecord
    SigningProfileRevocationRecord (..),
    newSigningProfileRevocationRecord,
    signingProfileRevocationRecord_revocationEffectiveFrom,
    signingProfileRevocationRecord_revokedAt,
    signingProfileRevocationRecord_revokedBy,

    -- * Source
    Source (..),
    newSource,
    source_s3,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign
import Amazonka.Signer.Types.Category
import Amazonka.Signer.Types.Destination
import Amazonka.Signer.Types.EncryptionAlgorithm
import Amazonka.Signer.Types.EncryptionAlgorithmOptions
import Amazonka.Signer.Types.HashAlgorithm
import Amazonka.Signer.Types.HashAlgorithmOptions
import Amazonka.Signer.Types.ImageFormat
import Amazonka.Signer.Types.Permission
import Amazonka.Signer.Types.S3Destination
import Amazonka.Signer.Types.S3SignedObject
import Amazonka.Signer.Types.S3Source
import Amazonka.Signer.Types.SignatureValidityPeriod
import Amazonka.Signer.Types.SignedObject
import Amazonka.Signer.Types.SigningConfiguration
import Amazonka.Signer.Types.SigningConfigurationOverrides
import Amazonka.Signer.Types.SigningImageFormat
import Amazonka.Signer.Types.SigningJob
import Amazonka.Signer.Types.SigningJobRevocationRecord
import Amazonka.Signer.Types.SigningMaterial
import Amazonka.Signer.Types.SigningPlatform
import Amazonka.Signer.Types.SigningPlatformOverrides
import Amazonka.Signer.Types.SigningProfile
import Amazonka.Signer.Types.SigningProfileRevocationRecord
import Amazonka.Signer.Types.SigningProfileStatus
import Amazonka.Signer.Types.SigningStatus
import Amazonka.Signer.Types.Source
import Amazonka.Signer.Types.ValidityType

-- | API version @2017-08-25@ of the Amazon Signer SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Signer",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "signer",
      Core.signingName = "signer",
      Core.version = "2017-08-25",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Signer",
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

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The signing profile was not found.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | A specified resource could not be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The resource encountered a conflicting state.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The request was denied due to request throttling.
--
-- Instead of this error, @TooManyRequestsException@ should be used.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The client is making a request that exceeds service limits.
_ServiceLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceLimitExceededException"
    Prelude.. Core.hasStatus 402

-- | The request contains invalid parameters for the ARN or tags. This
-- exception also occurs when you call a tagging API on a cancelled signing
-- profile.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | You signing certificate could not be validated.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | An internal error occurred.
_InternalServiceErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServiceErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServiceErrorException"
    Prelude.. Core.hasStatus 500

-- | The allowed number of job-signing requests has been exceeded.
--
-- This error supersedes the error @ThrottlingException@.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429
