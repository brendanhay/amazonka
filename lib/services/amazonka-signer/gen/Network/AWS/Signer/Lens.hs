{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Signer.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Signer.Lens
  ( -- * Operations

    -- ** StartSigningJob
    startSigningJob_profileOwner,
    startSigningJob_source,
    startSigningJob_destination,
    startSigningJob_profileName,
    startSigningJob_clientRequestToken,
    startSigningJobResponse_jobId,
    startSigningJobResponse_jobOwner,
    startSigningJobResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** RevokeSigningProfile
    revokeSigningProfile_profileVersion,
    revokeSigningProfile_reason,
    revokeSigningProfile_effectiveTime,
    revokeSigningProfile_profileName,

    -- ** CancelSigningProfile
    cancelSigningProfile_profileName,

    -- ** PutSigningProfile
    putSigningProfile_overrides,
    putSigningProfile_signingMaterial,
    putSigningProfile_signatureValidityPeriod,
    putSigningProfile_signingParameters,
    putSigningProfile_tags,
    putSigningProfile_profileName,
    putSigningProfile_platformId,
    putSigningProfileResponse_arn,
    putSigningProfileResponse_profileVersion,
    putSigningProfileResponse_profileVersionArn,
    putSigningProfileResponse_httpStatus,

    -- ** AddProfilePermission
    addProfilePermission_profileVersion,
    addProfilePermission_revisionId,
    addProfilePermission_action,
    addProfilePermission_principal,
    addProfilePermission_statementId,
    addProfilePermission_profileName,
    addProfilePermissionResponse_revisionId,
    addProfilePermissionResponse_httpStatus,

    -- ** ListSigningProfiles
    listSigningProfiles_nextToken,
    listSigningProfiles_platformId,
    listSigningProfiles_statuses,
    listSigningProfiles_includeCanceled,
    listSigningProfiles_maxResults,
    listSigningProfilesResponse_profiles,
    listSigningProfilesResponse_nextToken,
    listSigningProfilesResponse_httpStatus,

    -- ** ListProfilePermissions
    listProfilePermissions_nextToken,
    listProfilePermissions_profileName,
    listProfilePermissionsResponse_policySizeBytes,
    listProfilePermissionsResponse_nextToken,
    listProfilePermissionsResponse_permissions,
    listProfilePermissionsResponse_revisionId,
    listProfilePermissionsResponse_httpStatus,

    -- ** RevokeSignature
    revokeSignature_jobOwner,
    revokeSignature_reason,
    revokeSignature_jobId,

    -- ** GetSigningPlatform
    getSigningPlatform_platformId,
    getSigningPlatformResponse_category,
    getSigningPlatformResponse_signingConfiguration,
    getSigningPlatformResponse_partner,
    getSigningPlatformResponse_revocationSupported,
    getSigningPlatformResponse_signingImageFormat,
    getSigningPlatformResponse_platformId,
    getSigningPlatformResponse_displayName,
    getSigningPlatformResponse_maxSizeInMB,
    getSigningPlatformResponse_target,
    getSigningPlatformResponse_httpStatus,

    -- ** ListSigningPlatforms
    listSigningPlatforms_category,
    listSigningPlatforms_partner,
    listSigningPlatforms_nextToken,
    listSigningPlatforms_maxResults,
    listSigningPlatforms_target,
    listSigningPlatformsResponse_platforms,
    listSigningPlatformsResponse_nextToken,
    listSigningPlatformsResponse_httpStatus,

    -- ** ListSigningJobs
    listSigningJobs_status,
    listSigningJobs_signatureExpiresAfter,
    listSigningJobs_requestedBy,
    listSigningJobs_isRevoked,
    listSigningJobs_nextToken,
    listSigningJobs_platformId,
    listSigningJobs_jobInvoker,
    listSigningJobs_signatureExpiresBefore,
    listSigningJobs_maxResults,
    listSigningJobsResponse_jobs,
    listSigningJobsResponse_nextToken,
    listSigningJobsResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** RemoveProfilePermission
    removeProfilePermission_revisionId,
    removeProfilePermission_profileName,
    removeProfilePermission_statementId,
    removeProfilePermissionResponse_revisionId,
    removeProfilePermissionResponse_httpStatus,

    -- ** GetSigningProfile
    getSigningProfile_profileOwner,
    getSigningProfile_profileName,
    getSigningProfileResponse_status,
    getSigningProfileResponse_overrides,
    getSigningProfileResponse_platformDisplayName,
    getSigningProfileResponse_arn,
    getSigningProfileResponse_signingMaterial,
    getSigningProfileResponse_profileVersion,
    getSigningProfileResponse_profileName,
    getSigningProfileResponse_profileVersionArn,
    getSigningProfileResponse_platformId,
    getSigningProfileResponse_revocationRecord,
    getSigningProfileResponse_statusReason,
    getSigningProfileResponse_signatureValidityPeriod,
    getSigningProfileResponse_signingParameters,
    getSigningProfileResponse_tags,
    getSigningProfileResponse_httpStatus,

    -- ** DescribeSigningJob
    describeSigningJob_jobId,
    describeSigningJobResponse_status,
    describeSigningJobResponse_overrides,
    describeSigningJobResponse_platformDisplayName,
    describeSigningJobResponse_jobId,
    describeSigningJobResponse_createdAt,
    describeSigningJobResponse_signingMaterial,
    describeSigningJobResponse_requestedBy,
    describeSigningJobResponse_signatureExpiresAt,
    describeSigningJobResponse_profileVersion,
    describeSigningJobResponse_profileName,
    describeSigningJobResponse_signedObject,
    describeSigningJobResponse_platformId,
    describeSigningJobResponse_source,
    describeSigningJobResponse_revocationRecord,
    describeSigningJobResponse_jobInvoker,
    describeSigningJobResponse_completedAt,
    describeSigningJobResponse_statusReason,
    describeSigningJobResponse_jobOwner,
    describeSigningJobResponse_signingParameters,
    describeSigningJobResponse_httpStatus,

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
    permission_profileVersion,
    permission_principal,
    permission_statementId,

    -- ** S3Destination
    s3Destination_prefix,
    s3Destination_bucketName,

    -- ** S3SignedObject
    s3SignedObject_bucketName,
    s3SignedObject_key,

    -- ** S3Source
    s3Source_bucketName,
    s3Source_key,
    s3Source_version,

    -- ** SignatureValidityPeriod
    signatureValidityPeriod_value,
    signatureValidityPeriod_type,

    -- ** SignedObject
    signedObject_s3,

    -- ** SigningConfiguration
    signingConfiguration_encryptionAlgorithmOptions,
    signingConfiguration_hashAlgorithmOptions,

    -- ** SigningConfigurationOverrides
    signingConfigurationOverrides_hashAlgorithm,
    signingConfigurationOverrides_encryptionAlgorithm,

    -- ** SigningImageFormat
    signingImageFormat_supportedFormats,
    signingImageFormat_defaultFormat,

    -- ** SigningJob
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

    -- ** SigningJobRevocationRecord
    signingJobRevocationRecord_revokedBy,
    signingJobRevocationRecord_revokedAt,
    signingJobRevocationRecord_reason,

    -- ** SigningMaterial
    signingMaterial_certificateArn,

    -- ** SigningPlatform
    signingPlatform_category,
    signingPlatform_signingConfiguration,
    signingPlatform_partner,
    signingPlatform_revocationSupported,
    signingPlatform_signingImageFormat,
    signingPlatform_platformId,
    signingPlatform_displayName,
    signingPlatform_maxSizeInMB,
    signingPlatform_target,

    -- ** SigningPlatformOverrides
    signingPlatformOverrides_signingConfiguration,
    signingPlatformOverrides_signingImageFormat,

    -- ** SigningProfile
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

    -- ** SigningProfileRevocationRecord
    signingProfileRevocationRecord_revokedBy,
    signingProfileRevocationRecord_revocationEffectiveFrom,
    signingProfileRevocationRecord_revokedAt,

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
