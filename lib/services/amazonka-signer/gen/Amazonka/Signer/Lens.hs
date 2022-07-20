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
    describeSigningJobResponse_jobOwner,
    describeSigningJobResponse_jobInvoker,
    describeSigningJobResponse_profileName,
    describeSigningJobResponse_signingParameters,
    describeSigningJobResponse_signedObject,
    describeSigningJobResponse_platformDisplayName,
    describeSigningJobResponse_statusReason,
    describeSigningJobResponse_jobId,
    describeSigningJobResponse_status,
    describeSigningJobResponse_profileVersion,
    describeSigningJobResponse_revocationRecord,
    describeSigningJobResponse_signingMaterial,
    describeSigningJobResponse_source,
    describeSigningJobResponse_requestedBy,
    describeSigningJobResponse_signatureExpiresAt,
    describeSigningJobResponse_completedAt,
    describeSigningJobResponse_createdAt,
    describeSigningJobResponse_platformId,
    describeSigningJobResponse_overrides,
    describeSigningJobResponse_httpStatus,

    -- ** GetSigningPlatform
    getSigningPlatform_platformId,
    getSigningPlatformResponse_signingImageFormat,
    getSigningPlatformResponse_partner,
    getSigningPlatformResponse_signingConfiguration,
    getSigningPlatformResponse_displayName,
    getSigningPlatformResponse_target,
    getSigningPlatformResponse_category,
    getSigningPlatformResponse_revocationSupported,
    getSigningPlatformResponse_platformId,
    getSigningPlatformResponse_maxSizeInMB,
    getSigningPlatformResponse_httpStatus,

    -- ** GetSigningProfile
    getSigningProfile_profileOwner,
    getSigningProfile_profileName,
    getSigningProfileResponse_tags,
    getSigningProfileResponse_signatureValidityPeriod,
    getSigningProfileResponse_profileName,
    getSigningProfileResponse_signingParameters,
    getSigningProfileResponse_profileVersionArn,
    getSigningProfileResponse_platformDisplayName,
    getSigningProfileResponse_arn,
    getSigningProfileResponse_statusReason,
    getSigningProfileResponse_status,
    getSigningProfileResponse_profileVersion,
    getSigningProfileResponse_revocationRecord,
    getSigningProfileResponse_signingMaterial,
    getSigningProfileResponse_platformId,
    getSigningProfileResponse_overrides,
    getSigningProfileResponse_httpStatus,

    -- ** ListProfilePermissions
    listProfilePermissions_nextToken,
    listProfilePermissions_profileName,
    listProfilePermissionsResponse_nextToken,
    listProfilePermissionsResponse_permissions,
    listProfilePermissionsResponse_revisionId,
    listProfilePermissionsResponse_policySizeBytes,
    listProfilePermissionsResponse_httpStatus,

    -- ** ListSigningJobs
    listSigningJobs_signatureExpiresBefore,
    listSigningJobs_nextToken,
    listSigningJobs_jobInvoker,
    listSigningJobs_status,
    listSigningJobs_maxResults,
    listSigningJobs_isRevoked,
    listSigningJobs_requestedBy,
    listSigningJobs_signatureExpiresAfter,
    listSigningJobs_platformId,
    listSigningJobsResponse_nextToken,
    listSigningJobsResponse_jobs,
    listSigningJobsResponse_httpStatus,

    -- ** ListSigningPlatforms
    listSigningPlatforms_partner,
    listSigningPlatforms_nextToken,
    listSigningPlatforms_target,
    listSigningPlatforms_maxResults,
    listSigningPlatforms_category,
    listSigningPlatformsResponse_nextToken,
    listSigningPlatformsResponse_platforms,
    listSigningPlatformsResponse_httpStatus,

    -- ** ListSigningProfiles
    listSigningProfiles_nextToken,
    listSigningProfiles_includeCanceled,
    listSigningProfiles_statuses,
    listSigningProfiles_maxResults,
    listSigningProfiles_platformId,
    listSigningProfilesResponse_nextToken,
    listSigningProfilesResponse_profiles,
    listSigningProfilesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutSigningProfile
    putSigningProfile_tags,
    putSigningProfile_signatureValidityPeriod,
    putSigningProfile_signingParameters,
    putSigningProfile_signingMaterial,
    putSigningProfile_overrides,
    putSigningProfile_profileName,
    putSigningProfile_platformId,
    putSigningProfileResponse_profileVersionArn,
    putSigningProfileResponse_arn,
    putSigningProfileResponse_profileVersion,
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
    startSigningJobResponse_jobOwner,
    startSigningJobResponse_jobId,
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
    permission_principal,
    permission_statementId,
    permission_profileVersion,
    permission_action,

    -- ** S3Destination
    s3Destination_bucketName,
    s3Destination_prefix,

    -- ** S3SignedObject
    s3SignedObject_key,
    s3SignedObject_bucketName,

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

    -- ** SigningJobRevocationRecord
    signingJobRevocationRecord_reason,
    signingJobRevocationRecord_revokedAt,
    signingJobRevocationRecord_revokedBy,

    -- ** SigningMaterial
    signingMaterial_certificateArn,

    -- ** SigningPlatform
    signingPlatform_signingImageFormat,
    signingPlatform_partner,
    signingPlatform_signingConfiguration,
    signingPlatform_displayName,
    signingPlatform_target,
    signingPlatform_category,
    signingPlatform_revocationSupported,
    signingPlatform_platformId,
    signingPlatform_maxSizeInMB,

    -- ** SigningPlatformOverrides
    signingPlatformOverrides_signingImageFormat,
    signingPlatformOverrides_signingConfiguration,

    -- ** SigningProfile
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
