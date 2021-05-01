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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev =
        "ResourceGroupsTagging",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcEndpointPrefix = "tagging",
      Prelude._svcSigningName = "tagging",
      Prelude._svcVersion = "2017-01-26",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "ResourceGroupsTagging",
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

-- | A @PaginationToken@ is valid for a maximum of 15 minutes. Your request
-- was denied because the specified @PaginationToken@ has expired.
_PaginationTokenExpiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_PaginationTokenExpiredException =
  Prelude._MatchServiceError
    defaultService
    "PaginationTokenExpiredException"

-- | The request was denied to limit the frequency of submitted requests.
_ThrottledException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ThrottledException =
  Prelude._MatchServiceError
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
_ConstraintViolationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ConstraintViolationException =
  Prelude._MatchServiceError
    defaultService
    "ConstraintViolationException"

-- | The request processing failed because of an unknown error, exception, or
-- failure. You can retry the request.
_InternalServiceException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InternalServiceException =
  Prelude._MatchServiceError
    defaultService
    "InternalServiceException"

-- | The target of the operation is currently being modified by a different
-- request. Try again later.
_ConcurrentModificationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ConcurrentModificationException =
  Prelude._MatchServiceError
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
_InvalidParameterException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidParameterException =
  Prelude._MatchServiceError
    defaultService
    "InvalidParameterException"
