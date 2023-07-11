{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ResourceGroupsTagging.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResourceGroupsTagging.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ConcurrentModificationException,
    _ConstraintViolationException,
    _InternalServiceException,
    _InvalidParameterException,
    _PaginationTokenExpiredException,
    _ThrottledException,

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
    complianceDetails_keysWithNoncompliantValues,
    complianceDetails_noncompliantKeys,

    -- * FailureInfo
    FailureInfo (..),
    newFailureInfo,
    failureInfo_errorCode,
    failureInfo_errorMessage,
    failureInfo_statusCode,

    -- * ResourceTagMapping
    ResourceTagMapping (..),
    newResourceTagMapping,
    resourceTagMapping_complianceDetails,
    resourceTagMapping_resourceARN,
    resourceTagMapping_tags,

    -- * Summary
    Summary (..),
    newSummary,
    summary_lastUpdated,
    summary_nonCompliantResources,
    summary_region,
    summary_resourceType,
    summary_targetId,
    summary_targetIdType,

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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResourceGroupsTagging.Types.ComplianceDetails
import Amazonka.ResourceGroupsTagging.Types.FailureInfo
import Amazonka.ResourceGroupsTagging.Types.GroupByAttribute
import Amazonka.ResourceGroupsTagging.Types.ResourceErrorCode
import Amazonka.ResourceGroupsTagging.Types.ResourceTagMapping
import Amazonka.ResourceGroupsTagging.Types.Summary
import Amazonka.ResourceGroupsTagging.Types.Tag
import Amazonka.ResourceGroupsTagging.Types.TagFilter
import Amazonka.ResourceGroupsTagging.Types.TargetIdType
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-01-26@ of the Amazon Resource Groups Tagging API SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "ResourceGroupsTagging",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "tagging",
      Core.signingName = "tagging",
      Core.version = "2017-01-26",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error =
        Core.parseJSONError "ResourceGroupsTagging",
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

-- | The target of the operation is currently being modified by a different
-- request. Try again later.
_ConcurrentModificationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | The request was denied because performing this operation violates a
-- constraint.
--
-- Some of the reasons in the following list might not apply to this
-- specific operation.
--
-- -   You must meet the prerequisites for using tag policies. For
--     information, see
--     <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies-prereqs.html Prerequisites and Permissions for Using Tag Policies>
--     in the /Organizations User Guide./
--
-- -   You must enable the tag policies service principal
--     (@tagpolicies.tag.amazonaws.com@) to integrate with Organizations
--     For information, see
--     <https://docs.aws.amazon.com/organizations/latest/APIReference/API_EnableAWSServiceAccess.html EnableAWSServiceAccess>.
--
-- -   You must have a tag policy attached to the organization root, an OU,
--     or an account.
_ConstraintViolationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConstraintViolationException =
  Core._MatchServiceError
    defaultService
    "ConstraintViolationException"

-- | The request processing failed because of an unknown error, exception, or
-- failure. You can retry the request.
_InternalServiceException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServiceException =
  Core._MatchServiceError
    defaultService
    "InternalServiceException"

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
--     <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies-prereqs.html#bucket-policies-org-report Additional Requirements for Organization-wide Tag Compliance Reports>
--     in the /Organizations User Guide./
_InvalidParameterException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | A @PaginationToken@ is valid for a maximum of 15 minutes. Your request
-- was denied because the specified @PaginationToken@ has expired.
_PaginationTokenExpiredException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PaginationTokenExpiredException =
  Core._MatchServiceError
    defaultService
    "PaginationTokenExpiredException"

-- | The request was denied to limit the frequency of submitted requests.
_ThrottledException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottledException =
  Core._MatchServiceError
    defaultService
    "ThrottledException"
