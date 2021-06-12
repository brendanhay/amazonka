{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroupsTagging.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _PaginationTokenExpiredException,
    _ThrottledException,
    _ConstraintViolationException,
    _InternalServiceException,
    _ConcurrentModificationException,
    _InvalidParameterException,

    -- * GroupByAttribute
    GroupByAttribute (..),

    -- * ResourceErrorCode
    ResourceErrorCode (..),

    -- * TargetIdType
    TargetIdType (..),

    -- * ComplianceDetails
    ComplianceDetails (..),
    newComplianceDetails,
    complianceDetails_complianceStatus,
    complianceDetails_noncompliantKeys,
    complianceDetails_keysWithNoncompliantValues,

    -- * FailureInfo
    FailureInfo (..),
    newFailureInfo,
    failureInfo_statusCode,
    failureInfo_errorMessage,
    failureInfo_errorCode,

    -- * ResourceTagMapping
    ResourceTagMapping (..),
    newResourceTagMapping,
    resourceTagMapping_resourceARN,
    resourceTagMapping_complianceDetails,
    resourceTagMapping_tags,

    -- * Summary
    Summary (..),
    newSummary,
    summary_targetId,
    summary_resourceType,
    summary_lastUpdated,
    summary_targetIdType,
    summary_nonCompliantResources,
    summary_region,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TagFilter
    TagFilter (..),
    newTagFilter,
    tagFilter_key,
    tagFilter_values,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.ResourceGroupsTagging.Types.ComplianceDetails
import Network.AWS.ResourceGroupsTagging.Types.FailureInfo
import Network.AWS.ResourceGroupsTagging.Types.GroupByAttribute
import Network.AWS.ResourceGroupsTagging.Types.ResourceErrorCode
import Network.AWS.ResourceGroupsTagging.Types.ResourceTagMapping
import Network.AWS.ResourceGroupsTagging.Types.Summary
import Network.AWS.ResourceGroupsTagging.Types.Tag
import Network.AWS.ResourceGroupsTagging.Types.TagFilter
import Network.AWS.ResourceGroupsTagging.Types.TargetIdType
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-01-26@ of the Amazon Resource Groups Tagging API SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "ResourceGroupsTagging",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "tagging",
      Core._serviceSigningName = "tagging",
      Core._serviceVersion = "2017-01-26",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Core.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "ResourceGroupsTagging",
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
      | Lens.has (Core.hasStatus 504) e =
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | A @PaginationToken@ is valid for a maximum of 15 minutes. Your request
-- was denied because the specified @PaginationToken@ has expired.
_PaginationTokenExpiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PaginationTokenExpiredException =
  Core._MatchServiceError
    defaultService
    "PaginationTokenExpiredException"

-- | The request was denied to limit the frequency of submitted requests.
_ThrottledException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ThrottledException =
  Core._MatchServiceError
    defaultService
    "ThrottledException"

-- | The request was denied because performing this operation violates a
-- constraint.
--
-- Some of the reasons in the following list might not apply to this
-- specific operation.
--
-- -   You must meet the prerequisites for using tag policies. For
--     information, see
--     <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies-prereqs.html Prerequisites and Permissions for Using Tag Policies>
--     in the /AWS Organizations User Guide./
--
-- -   You must enable the tag policies service principal
--     (@tagpolicies.tag.amazonaws.com@) to integrate with AWS
--     Organizations For information, see
--     <http://docs.aws.amazon.com/organizations/latest/APIReference/API_EnableAWSServiceAccess.html EnableAWSServiceAccess>.
--
-- -   You must have a tag policy attached to the organization root, an OU,
--     or an account.
_ConstraintViolationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConstraintViolationException =
  Core._MatchServiceError
    defaultService
    "ConstraintViolationException"

-- | The request processing failed because of an unknown error, exception, or
-- failure. You can retry the request.
_InternalServiceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServiceException =
  Core._MatchServiceError
    defaultService
    "InternalServiceException"

-- | The target of the operation is currently being modified by a different
-- request. Try again later.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | This error indicates one of the following:
--
-- -   A parameter is missing.
--
-- -   A malformed string was supplied for the request parameter.
--
-- -   An out-of-range value was supplied for the request parameter.
--
-- -   The target ID is invalid, unsupported, or doesn\'t exist.
--
-- -   You can\'t access the Amazon S3 bucket for report storage. For more
--     information, see
--     <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies-prereqs.html#bucket-policies-org-report Additional Requirements for Organization-wide Tag Compliance Reports>
--     in the /AWS Organizations User Guide./
_InvalidParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"
