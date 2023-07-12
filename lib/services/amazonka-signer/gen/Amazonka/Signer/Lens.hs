{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Signer.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Signer.Lens
  ( -- * Operations

    -- ** AddProfilePermission
    addProfilePermission_profileVersion,
    addProfilePermission_revisionId,
    addProfilePermission_action,
    addProfilePermission_principal,
    addProfilePermission_statementId,
    addProfilePermission_profileName,
    addProfilePermissionResponse_revisionId,
    addProfilePermissionResponse_httpStatus,

    -- ** CancelSigningProfile
    cancelSigningProfile_profileName,

    -- ** DescribeSigningJob
    describeSigningJob_jobId,
    describeSigningJobResponse_completedAt,
    describeSigningJobResponse_createdAt,
    describeSigningJobResponse_jobId,
    describeSigningJobResponse_jobInvoker,
    describeSigningJobResponse_jobOwner,
    describeSigningJobResponse_overrides,
    describeSigningJobResponse_platformDisplayName,
    describeSigningJobResponse_platformId,
    describeSigningJobResponse_profileName,
    describeSigningJobResponse_profileVersion,
    describeSigningJobResponse_requestedBy,
    describeSigningJobResponse_revocationRecord,
    describeSigningJobResponse_signatureExpiresAt,
    describeSigningJobResponse_signedObject,
    describeSigningJobResponse_signingMaterial,
    describeSigningJobResponse_signingParameters,
    describeSigningJobResponse_source,
    describeSigningJobResponse_status,
    describeSigningJobResponse_statusReason,
    describeSigningJobResponse_httpStatus,

    -- ** GetSigningPlatform
    getSigningPlatform_platformId,
    getSigningPlatformResponse_category,
    getSigningPlatformResponse_displayName,
    getSigningPlatformResponse_maxSizeInMB,
    getSigningPlatformResponse_partner,
    getSigningPlatformResponse_platformId,
    getSigningPlatformResponse_revocationSupported,
    getSigningPlatformResponse_signingConfiguration,
    getSigningPlatformResponse_signingImageFormat,
    getSigningPlatformResponse_target,
    getSigningPlatformResponse_httpStatus,

    -- ** GetSigningProfile
    getSigningProfile_profileOwner,
    getSigningProfile_profileName,
    getSigningProfileResponse_arn,
    getSigningProfileResponse_overrides,
    getSigningProfileResponse_platformDisplayName,
    getSigningProfileResponse_platformId,
    getSigningProfileResponse_profileName,
    getSigningProfileResponse_profileVersion,
    getSigningProfileResponse_profileVersionArn,
    getSigningProfileResponse_revocationRecord,
    getSigningProfileResponse_signatureValidityPeriod,
    getSigningProfileResponse_signingMaterial,
    getSigningProfileResponse_signingParameters,
    getSigningProfileResponse_status,
    getSigningProfileResponse_statusReason,
    getSigningProfileResponse_tags,
    getSigningProfileResponse_httpStatus,

    -- ** ListProfilePermissions
    listProfilePermissions_nextToken,
    listProfilePermissions_profileName,
    listProfilePermissionsResponse_nextToken,
    listProfilePermissionsResponse_permissions,
    listProfilePermissionsResponse_policySizeBytes,
    listProfilePermissionsResponse_revisionId,
    listProfilePermissionsResponse_httpStatus,

    -- ** ListSigningJobs
    listSigningJobs_isRevoked,
    listSigningJobs_jobInvoker,
    listSigningJobs_maxResults,
    listSigningJobs_nextToken,
    listSigningJobs_platformId,
    listSigningJobs_requestedBy,
    listSigningJobs_signatureExpiresAfter,
    listSigningJobs_signatureExpiresBefore,
    listSigningJobs_status,
    listSigningJobsResponse_jobs,
    listSigningJobsResponse_nextToken,
    listSigningJobsResponse_httpStatus,

    -- ** ListSigningPlatforms
    listSigningPlatforms_category,
    listSigningPlatforms_maxResults,
    listSigningPlatforms_nextToken,
    listSigningPlatforms_partner,
    listSigningPlatforms_target,
    listSigningPlatformsResponse_nextToken,
    listSigningPlatformsResponse_platforms,
    listSigningPlatformsResponse_httpStatus,

    -- ** ListSigningProfiles
    listSigningProfiles_includeCanceled,
    listSigningProfiles_maxResults,
    listSigningProfiles_nextToken,
    listSigningProfiles_platformId,
    listSigningProfiles_statuses,
    listSigningProfilesResponse_nextToken,
    listSigningProfilesResponse_profiles,
    listSigningProfilesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutSigningProfile
    putSigningProfile_overrides,
    putSigningProfile_signatureValidityPeriod,
    putSigningProfile_signingMaterial,
    putSigningProfile_signingParameters,
    putSigningProfile_tags,
    putSigningProfile_profileName,
    putSigningProfile_platformId,
    putSigningProfileResponse_arn,
    putSigningProfileResponse_profileVersion,
    putSigningProfileResponse_profileVersionArn,
    putSigningProfileResponse_httpStatus,

    -- ** RemoveProfilePermission
    removeProfilePermission_revisionId,
    removeProfilePermission_profileName,
    removeProfilePermission_statementId,
    removeProfilePermissionResponse_revisionId,
    removeProfilePermissionResponse_httpStatus,

    -- ** RevokeSignature
    revokeSignature_jobOwner,
    revokeSignature_reason,
    revokeSignature_jobId,

    -- ** RevokeSigningProfile
    revokeSigningProfile_profileVersion,
    revokeSigningProfile_reason,
    revokeSigningProfile_effectiveTime,
    revokeSigningProfile_profileName,

    -- ** StartSigningJob
    startSigningJob_profileOwner,
    startSigningJob_source,
    startSigningJob_destination,
    startSigningJob_profileName,
    startSigningJob_clientRequestToken,
    startSigningJobResponse_jobId,
    startSigningJobResponse_jobOwner,
    startSigningJobResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- * Types

    -- ** Destination
    destination_s3,

    -- ** EncryptionAlgorithmOptions
    encryptionAlgorithmOptions_allowedValues,
    encryptionAlgorithmOptions_defaultValue,

    -- ** HashAlgorithmOptions
    hashAlgorithmOptions_allowedValues,
    hashAlgorithmOptions_defaultValue,

    -- ** Permission
    permission_action,
    permission_principal,
    permission_profileVersion,
    permission_statementId,

    -- ** S3Destination
    s3Destination_bucketName,
    s3Destination_prefix,

    -- ** S3SignedObject
    s3SignedObject_bucketName,
    s3SignedObject_key,

    -- ** S3Source
    s3Source_bucketName,
    s3Source_key,
    s3Source_version,

    -- ** SignatureValidityPeriod
    signatureValidityPeriod_type,
    signatureValidityPeriod_value,

    -- ** SignedObject
    signedObject_s3,

    -- ** SigningConfiguration
    signingConfiguration_encryptionAlgorithmOptions,
    signingConfiguration_hashAlgorithmOptions,

    -- ** SigningConfigurationOverrides
    signingConfigurationOverrides_encryptionAlgorithm,
    signingConfigurationOverrides_hashAlgorithm,

    -- ** SigningImageFormat
    signingImageFormat_supportedFormats,
    signingImageFormat_defaultFormat,

    -- ** SigningJob
    signingJob_createdAt,
    signingJob_isRevoked,
    signingJob_jobId,
    signingJob_jobInvoker,
    signingJob_jobOwner,
    signingJob_platformDisplayName,
    signingJob_platformId,
    signingJob_profileName,
    signingJob_profileVersion,
    signingJob_signatureExpiresAt,
    signingJob_signedObject,
    signingJob_signingMaterial,
    signingJob_source,
    signingJob_status,

    -- ** SigningJobRevocationRecord
    signingJobRevocationRecord_reason,
    signingJobRevocationRecord_revokedAt,
    signingJobRevocationRecord_revokedBy,

    -- ** SigningMaterial
    signingMaterial_certificateArn,

    -- ** SigningPlatform
    signingPlatform_category,
    signingPlatform_displayName,
    signingPlatform_maxSizeInMB,
    signingPlatform_partner,
    signingPlatform_platformId,
    signingPlatform_revocationSupported,
    signingPlatform_signingConfiguration,
    signingPlatform_signingImageFormat,
    signingPlatform_target,

    -- ** SigningPlatformOverrides
    signingPlatformOverrides_signingConfiguration,
    signingPlatformOverrides_signingImageFormat,

    -- ** SigningProfile
    signingProfile_arn,
    signingProfile_platformDisplayName,
    signingProfile_platformId,
    signingProfile_profileName,
    signingProfile_profileVersion,
    signingProfile_profileVersionArn,
    signingProfile_signatureValidityPeriod,
    signingProfile_signingMaterial,
    signingProfile_signingParameters,
    signingProfile_status,
    signingProfile_tags,

    -- ** SigningProfileRevocationRecord
    signingProfileRevocationRecord_revocationEffectiveFrom,
    signingProfileRevocationRecord_revokedAt,
    signingProfileRevocationRecord_revokedBy,

    -- ** Source
    source_s3,
  )
where

import Amazonka.Signer.AddProfilePermission
import Amazonka.Signer.CancelSigningProfile
import Amazonka.Signer.DescribeSigningJob
import Amazonka.Signer.GetSigningPlatform
import Amazonka.Signer.GetSigningProfile
import Amazonka.Signer.ListProfilePermissions
import Amazonka.Signer.ListSigningJobs
import Amazonka.Signer.ListSigningPlatforms
import Amazonka.Signer.ListSigningProfiles
import Amazonka.Signer.ListTagsForResource
import Amazonka.Signer.PutSigningProfile
import Amazonka.Signer.RemoveProfilePermission
import Amazonka.Signer.RevokeSignature
import Amazonka.Signer.RevokeSigningProfile
import Amazonka.Signer.StartSigningJob
import Amazonka.Signer.TagResource
import Amazonka.Signer.Types.Destination
import Amazonka.Signer.Types.EncryptionAlgorithmOptions
import Amazonka.Signer.Types.HashAlgorithmOptions
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
import Amazonka.Signer.Types.Source
import Amazonka.Signer.UntagResource
