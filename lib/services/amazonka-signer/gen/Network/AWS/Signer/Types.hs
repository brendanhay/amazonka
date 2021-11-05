{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Signer.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Signer.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _ConflictException,
    _ServiceLimitExceededException,
    _NotFoundException,
    _ThrottlingException,
    _TooManyRequestsException,
    _InternalServiceErrorException,
    _ResourceNotFoundException,
    _BadRequestException,

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
    permission_action,
    permission_profileVersion,
    permission_principal,
    permission_statementId,

    -- * S3Destination
    S3Destination (..),
    newS3Destination,
    s3Destination_prefix,
    s3Destination_bucketName,

    -- * S3SignedObject
    S3SignedObject (..),
    newS3SignedObject,
    s3SignedObject_bucketName,
    s3SignedObject_key,

    -- * S3Source
    S3Source (..),
    newS3Source,
    s3Source_bucketName,
    s3Source_key,
    s3Source_version,

    -- * SignatureValidityPeriod
    SignatureValidityPeriod (..),
    newSignatureValidityPeriod,
    signatureValidityPeriod_value,
    signatureValidityPeriod_type,

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
    signingConfigurationOverrides_hashAlgorithm,
    signingConfigurationOverrides_encryptionAlgorithm,

    -- * SigningImageFormat
    SigningImageFormat (..),
    newSigningImageFormat,
    signingImageFormat_supportedFormats,
    signingImageFormat_defaultFormat,

    -- * SigningJob
    SigningJob (..),
    newSigningJob,
    signingJob_status,
    signingJob_platformDisplayName,
    signingJob_jobId,
    signingJob_createdAt,
    signingJob_signingMaterial,
    signingJob_isRevoked,
    signingJob_signatureExpiresAt,
    signingJob_profileVersion,
    signingJob_profileName,
    signingJob_signedObject,
    signingJob_platformId,
    signingJob_source,
    signingJob_jobInvoker,
    signingJob_jobOwner,

    -- * SigningJobRevocationRecord
    SigningJobRevocationRecord (..),
    newSigningJobRevocationRecord,
    signingJobRevocationRecord_revokedBy,
    signingJobRevocationRecord_revokedAt,
    signingJobRevocationRecord_reason,

    -- * SigningMaterial
    SigningMaterial (..),
    newSigningMaterial,
    signingMaterial_certificateArn,

    -- * SigningPlatform
    SigningPlatform (..),
    newSigningPlatform,
    signingPlatform_category,
    signingPlatform_signingConfiguration,
    signingPlatform_partner,
    signingPlatform_revocationSupported,
    signingPlatform_signingImageFormat,
    signingPlatform_platformId,
    signingPlatform_displayName,
    signingPlatform_maxSizeInMB,
    signingPlatform_target,

    -- * SigningPlatformOverrides
    SigningPlatformOverrides (..),
    newSigningPlatformOverrides,
    signingPlatformOverrides_signingConfiguration,
    signingPlatformOverrides_signingImageFormat,

    -- * SigningProfile
    SigningProfile (..),
    newSigningProfile,
    signingProfile_status,
    signingProfile_platformDisplayName,
    signingProfile_arn,
    signingProfile_signingMaterial,
    signingProfile_profileVersion,
    signingProfile_profileName,
    signingProfile_profileVersionArn,
    signingProfile_platformId,
    signingProfile_signatureValidityPeriod,
    signingProfile_signingParameters,
    signingProfile_tags,

    -- * SigningProfileRevocationRecord
    SigningProfileRevocationRecord (..),
    newSigningProfileRevocationRecord,
    signingProfileRevocationRecord_revokedBy,
    signingProfileRevocationRecord_revocationEffectiveFrom,
    signingProfileRevocationRecord_revokedAt,

    -- * Source
    Source (..),
    newSource,
    source_s3,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.Signer.Types.Category
import Network.AWS.Signer.Types.Destination
import Network.AWS.Signer.Types.EncryptionAlgorithm
import Network.AWS.Signer.Types.EncryptionAlgorithmOptions
import Network.AWS.Signer.Types.HashAlgorithm
import Network.AWS.Signer.Types.HashAlgorithmOptions
import Network.AWS.Signer.Types.ImageFormat
import Network.AWS.Signer.Types.Permission
import Network.AWS.Signer.Types.S3Destination
import Network.AWS.Signer.Types.S3SignedObject
import Network.AWS.Signer.Types.S3Source
import Network.AWS.Signer.Types.SignatureValidityPeriod
import Network.AWS.Signer.Types.SignedObject
import Network.AWS.Signer.Types.SigningConfiguration
import Network.AWS.Signer.Types.SigningConfigurationOverrides
import Network.AWS.Signer.Types.SigningImageFormat
import Network.AWS.Signer.Types.SigningJob
import Network.AWS.Signer.Types.SigningJobRevocationRecord
import Network.AWS.Signer.Types.SigningMaterial
import Network.AWS.Signer.Types.SigningPlatform
import Network.AWS.Signer.Types.SigningPlatformOverrides
import Network.AWS.Signer.Types.SigningProfile
import Network.AWS.Signer.Types.SigningProfileRevocationRecord
import Network.AWS.Signer.Types.SigningProfileStatus
import Network.AWS.Signer.Types.SigningStatus
import Network.AWS.Signer.Types.Source
import Network.AWS.Signer.Types.ValidityType

-- | API version @2017-08-25@ of the Amazon Signer SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Signer",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "signer",
      Core._serviceSigningName = "signer",
      Core._serviceVersion = "2017-08-25",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "Signer",
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

-- | You signing certificate could not be validated.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The resource encountered a conflicting state.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The client is making a request that exceeds service limits.
_ServiceLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceLimitExceededException"
    Prelude.. Core.hasStatus 402

-- | The signing profile was not found.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request was denied due to request throttling.
--
-- Instead of this error, @TooManyRequestsException@ should be used.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The allowed number of job-signing requests has been exceeded.
--
-- This error supersedes the error @ThrottlingException@.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429

-- | An internal error occurred.
_InternalServiceErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServiceErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServiceErrorException"
    Prelude.. Core.hasStatus 500

-- | A specified resource could not be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request contains invalid parameters for the ARN or tags. This
-- exception also occurs when you call a tagging API on a cancelled signing
-- profile.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400
