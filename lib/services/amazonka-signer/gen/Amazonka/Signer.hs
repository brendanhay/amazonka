{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Signer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-08-25@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS Signer is a fully managed code signing service to help you ensure
-- the trust and integrity of your code.
--
-- AWS Signer supports the following applications:
--
-- With /code signing for AWS Lambda/, you can sign AWS Lambda deployment
-- packages. Integrated support is provided for Amazon S3, Amazon
-- CloudWatch, and AWS CloudTrail. In order to sign code, you create a
-- signing profile and then use Signer to sign Lambda zip files in S3.
--
-- With /code signing for IoT/, you can sign code for any IoT device that
-- is supported by AWS. IoT code signing is available for
-- <http://docs.aws.amazon.com/freertos/latest/userguide/ Amazon FreeRTOS>
-- and
-- <http://docs.aws.amazon.com/iot/latest/developerguide/ AWS IoT Device Management>,
-- and is integrated with
-- <http://docs.aws.amazon.com/acm/latest/userguide/ AWS Certificate Manager (ACM)>.
-- In order to sign code, you import a third-party code signing certificate
-- using ACM, and use that to sign updates in Amazon FreeRTOS and AWS IoT
-- Device Management.
--
-- For more information about AWS Signer, see the
-- <http://docs.aws.amazon.com/signer/latest/developerguide/Welcome.html AWS Signer Developer Guide>.
module Amazonka.Signer
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** BadRequestException
    _BadRequestException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServiceErrorException
    _InternalServiceErrorException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceLimitExceededException
    _ServiceLimitExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- ** SuccessfulSigningJob
    newSuccessfulSigningJob,

    -- * Operations
    -- $operations

    -- ** AddProfilePermission
    AddProfilePermission (AddProfilePermission'),
    newAddProfilePermission,
    AddProfilePermissionResponse (AddProfilePermissionResponse'),
    newAddProfilePermissionResponse,

    -- ** CancelSigningProfile
    CancelSigningProfile (CancelSigningProfile'),
    newCancelSigningProfile,
    CancelSigningProfileResponse (CancelSigningProfileResponse'),
    newCancelSigningProfileResponse,

    -- ** DescribeSigningJob
    DescribeSigningJob (DescribeSigningJob'),
    newDescribeSigningJob,
    DescribeSigningJobResponse (DescribeSigningJobResponse'),
    newDescribeSigningJobResponse,

    -- ** GetSigningPlatform
    GetSigningPlatform (GetSigningPlatform'),
    newGetSigningPlatform,
    GetSigningPlatformResponse (GetSigningPlatformResponse'),
    newGetSigningPlatformResponse,

    -- ** GetSigningProfile
    GetSigningProfile (GetSigningProfile'),
    newGetSigningProfile,
    GetSigningProfileResponse (GetSigningProfileResponse'),
    newGetSigningProfileResponse,

    -- ** ListProfilePermissions
    ListProfilePermissions (ListProfilePermissions'),
    newListProfilePermissions,
    ListProfilePermissionsResponse (ListProfilePermissionsResponse'),
    newListProfilePermissionsResponse,

    -- ** ListSigningJobs (Paginated)
    ListSigningJobs (ListSigningJobs'),
    newListSigningJobs,
    ListSigningJobsResponse (ListSigningJobsResponse'),
    newListSigningJobsResponse,

    -- ** ListSigningPlatforms (Paginated)
    ListSigningPlatforms (ListSigningPlatforms'),
    newListSigningPlatforms,
    ListSigningPlatformsResponse (ListSigningPlatformsResponse'),
    newListSigningPlatformsResponse,

    -- ** ListSigningProfiles (Paginated)
    ListSigningProfiles (ListSigningProfiles'),
    newListSigningProfiles,
    ListSigningProfilesResponse (ListSigningProfilesResponse'),
    newListSigningProfilesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PutSigningProfile
    PutSigningProfile (PutSigningProfile'),
    newPutSigningProfile,
    PutSigningProfileResponse (PutSigningProfileResponse'),
    newPutSigningProfileResponse,

    -- ** RemoveProfilePermission
    RemoveProfilePermission (RemoveProfilePermission'),
    newRemoveProfilePermission,
    RemoveProfilePermissionResponse (RemoveProfilePermissionResponse'),
    newRemoveProfilePermissionResponse,

    -- ** RevokeSignature
    RevokeSignature (RevokeSignature'),
    newRevokeSignature,
    RevokeSignatureResponse (RevokeSignatureResponse'),
    newRevokeSignatureResponse,

    -- ** RevokeSigningProfile
    RevokeSigningProfile (RevokeSigningProfile'),
    newRevokeSigningProfile,
    RevokeSigningProfileResponse (RevokeSigningProfileResponse'),
    newRevokeSigningProfileResponse,

    -- ** StartSigningJob
    StartSigningJob (StartSigningJob'),
    newStartSigningJob,
    StartSigningJobResponse (StartSigningJobResponse'),
    newStartSigningJobResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- * Types

    -- ** Category
    Category (..),

    -- ** EncryptionAlgorithm
    EncryptionAlgorithm (..),

    -- ** HashAlgorithm
    HashAlgorithm (..),

    -- ** ImageFormat
    ImageFormat (..),

    -- ** SigningProfileStatus
    SigningProfileStatus (..),

    -- ** SigningStatus
    SigningStatus (..),

    -- ** ValidityType
    ValidityType (..),

    -- ** Destination
    Destination (Destination'),
    newDestination,

    -- ** EncryptionAlgorithmOptions
    EncryptionAlgorithmOptions (EncryptionAlgorithmOptions'),
    newEncryptionAlgorithmOptions,

    -- ** HashAlgorithmOptions
    HashAlgorithmOptions (HashAlgorithmOptions'),
    newHashAlgorithmOptions,

    -- ** Permission
    Permission (Permission'),
    newPermission,

    -- ** S3Destination
    S3Destination (S3Destination'),
    newS3Destination,

    -- ** S3SignedObject
    S3SignedObject (S3SignedObject'),
    newS3SignedObject,

    -- ** S3Source
    S3Source (S3Source'),
    newS3Source,

    -- ** SignatureValidityPeriod
    SignatureValidityPeriod (SignatureValidityPeriod'),
    newSignatureValidityPeriod,

    -- ** SignedObject
    SignedObject (SignedObject'),
    newSignedObject,

    -- ** SigningConfiguration
    SigningConfiguration (SigningConfiguration'),
    newSigningConfiguration,

    -- ** SigningConfigurationOverrides
    SigningConfigurationOverrides (SigningConfigurationOverrides'),
    newSigningConfigurationOverrides,

    -- ** SigningImageFormat
    SigningImageFormat (SigningImageFormat'),
    newSigningImageFormat,

    -- ** SigningJob
    SigningJob (SigningJob'),
    newSigningJob,

    -- ** SigningJobRevocationRecord
    SigningJobRevocationRecord (SigningJobRevocationRecord'),
    newSigningJobRevocationRecord,

    -- ** SigningMaterial
    SigningMaterial (SigningMaterial'),
    newSigningMaterial,

    -- ** SigningPlatform
    SigningPlatform (SigningPlatform'),
    newSigningPlatform,

    -- ** SigningPlatformOverrides
    SigningPlatformOverrides (SigningPlatformOverrides'),
    newSigningPlatformOverrides,

    -- ** SigningProfile
    SigningProfile (SigningProfile'),
    newSigningProfile,

    -- ** SigningProfileRevocationRecord
    SigningProfileRevocationRecord (SigningProfileRevocationRecord'),
    newSigningProfileRevocationRecord,

    -- ** Source
    Source (Source'),
    newSource,
  )
where

import Amazonka.Signer.AddProfilePermission
import Amazonka.Signer.CancelSigningProfile
import Amazonka.Signer.DescribeSigningJob
import Amazonka.Signer.GetSigningPlatform
import Amazonka.Signer.GetSigningProfile
import Amazonka.Signer.Lens
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
import Amazonka.Signer.Types
import Amazonka.Signer.UntagResource
import Amazonka.Signer.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Signer'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
