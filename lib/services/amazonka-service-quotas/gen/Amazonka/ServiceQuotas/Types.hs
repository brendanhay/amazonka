{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ServiceQuotas.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceQuotas.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AWSServiceAccessNotEnabledException,
    _AccessDeniedException,
    _DependencyAccessDeniedException,
    _IllegalArgumentException,
    _InvalidPaginationTokenException,
    _InvalidResourceStateException,
    _NoAvailableOrganizationException,
    _NoSuchResourceException,
    _OrganizationNotInAllFeaturesModeException,
    _QuotaExceededException,
    _ResourceAlreadyExistsException,
    _ServiceException,
    _ServiceQuotaTemplateNotInUseException,
    _TagPolicyViolationException,
    _TemplatesNotAvailableInRegionException,
    _TooManyRequestsException,
    _TooManyTagsException,

    -- * ErrorCode
    ErrorCode (..),

    -- * PeriodUnit
    PeriodUnit (..),

    -- * RequestStatus
    RequestStatus (..),

    -- * ServiceQuotaTemplateAssociationStatus
    ServiceQuotaTemplateAssociationStatus (..),

    -- * ErrorReason
    ErrorReason (..),
    newErrorReason,
    errorReason_errorCode,
    errorReason_errorMessage,

    -- * MetricInfo
    MetricInfo (..),
    newMetricInfo,
    metricInfo_metricDimensions,
    metricInfo_metricName,
    metricInfo_metricNamespace,
    metricInfo_metricStatisticRecommendation,

    -- * QuotaPeriod
    QuotaPeriod (..),
    newQuotaPeriod,
    quotaPeriod_periodUnit,
    quotaPeriod_periodValue,

    -- * RequestedServiceQuotaChange
    RequestedServiceQuotaChange (..),
    newRequestedServiceQuotaChange,
    requestedServiceQuotaChange_caseId,
    requestedServiceQuotaChange_created,
    requestedServiceQuotaChange_desiredValue,
    requestedServiceQuotaChange_globalQuota,
    requestedServiceQuotaChange_id,
    requestedServiceQuotaChange_lastUpdated,
    requestedServiceQuotaChange_quotaArn,
    requestedServiceQuotaChange_quotaCode,
    requestedServiceQuotaChange_quotaName,
    requestedServiceQuotaChange_requester,
    requestedServiceQuotaChange_serviceCode,
    requestedServiceQuotaChange_serviceName,
    requestedServiceQuotaChange_status,
    requestedServiceQuotaChange_unit,

    -- * ServiceInfo
    ServiceInfo (..),
    newServiceInfo,
    serviceInfo_serviceCode,
    serviceInfo_serviceName,

    -- * ServiceQuota
    ServiceQuota (..),
    newServiceQuota,
    serviceQuota_adjustable,
    serviceQuota_errorReason,
    serviceQuota_globalQuota,
    serviceQuota_period,
    serviceQuota_quotaArn,
    serviceQuota_quotaCode,
    serviceQuota_quotaName,
    serviceQuota_serviceCode,
    serviceQuota_serviceName,
    serviceQuota_unit,
    serviceQuota_usageMetric,
    serviceQuota_value,

    -- * ServiceQuotaIncreaseRequestInTemplate
    ServiceQuotaIncreaseRequestInTemplate (..),
    newServiceQuotaIncreaseRequestInTemplate,
    serviceQuotaIncreaseRequestInTemplate_awsRegion,
    serviceQuotaIncreaseRequestInTemplate_desiredValue,
    serviceQuotaIncreaseRequestInTemplate_globalQuota,
    serviceQuotaIncreaseRequestInTemplate_quotaCode,
    serviceQuotaIncreaseRequestInTemplate_quotaName,
    serviceQuotaIncreaseRequestInTemplate_serviceCode,
    serviceQuotaIncreaseRequestInTemplate_serviceName,
    serviceQuotaIncreaseRequestInTemplate_unit,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceQuotas.Types.ErrorCode
import Amazonka.ServiceQuotas.Types.ErrorReason
import Amazonka.ServiceQuotas.Types.MetricInfo
import Amazonka.ServiceQuotas.Types.PeriodUnit
import Amazonka.ServiceQuotas.Types.QuotaPeriod
import Amazonka.ServiceQuotas.Types.RequestStatus
import Amazonka.ServiceQuotas.Types.RequestedServiceQuotaChange
import Amazonka.ServiceQuotas.Types.ServiceInfo
import Amazonka.ServiceQuotas.Types.ServiceQuota
import Amazonka.ServiceQuotas.Types.ServiceQuotaIncreaseRequestInTemplate
import Amazonka.ServiceQuotas.Types.ServiceQuotaTemplateAssociationStatus
import Amazonka.ServiceQuotas.Types.Tag
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2019-06-24@ of the Amazon Quotas SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "ServiceQuotas",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "servicequotas",
      Core.signingName = "servicequotas",
      Core.version = "2019-06-24",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "ServiceQuotas",
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

-- | The action you attempted is not allowed unless Service Access with
-- Service Quotas is enabled in your organization.
_AWSServiceAccessNotEnabledException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AWSServiceAccessNotEnabledException =
  Core._MatchServiceError
    defaultService
    "AWSServiceAccessNotEnabledException"

-- | You do not have sufficient permission to perform this action.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | You can\'t perform this action because a dependency does not have
-- access.
_DependencyAccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DependencyAccessDeniedException =
  Core._MatchServiceError
    defaultService
    "DependencyAccessDeniedException"

-- | Invalid input was provided.
_IllegalArgumentException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_IllegalArgumentException =
  Core._MatchServiceError
    defaultService
    "IllegalArgumentException"

-- | Invalid input was provided.
_InvalidPaginationTokenException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidPaginationTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidPaginationTokenException"

-- | The resource is in an invalid state.
_InvalidResourceStateException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidResourceStateException =
  Core._MatchServiceError
    defaultService
    "InvalidResourceStateException"

-- | The account making this call is not a member of an organization.
_NoAvailableOrganizationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoAvailableOrganizationException =
  Core._MatchServiceError
    defaultService
    "NoAvailableOrganizationException"

-- | The specified resource does not exist.
_NoSuchResourceException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoSuchResourceException =
  Core._MatchServiceError
    defaultService
    "NoSuchResourceException"

-- | The organization that your account belongs to is not in All Features
-- mode.
_OrganizationNotInAllFeaturesModeException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_OrganizationNotInAllFeaturesModeException =
  Core._MatchServiceError
    defaultService
    "OrganizationNotInAllFeaturesModeException"

-- | You have exceeded your service quota. To perform the requested action,
-- remove some of the relevant resources, or use Service Quotas to request
-- a service quota increase.
_QuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_QuotaExceededException =
  Core._MatchServiceError
    defaultService
    "QuotaExceededException"

-- | The specified resource already exists.
_ResourceAlreadyExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"

-- | Something went wrong.
_ServiceException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceException =
  Core._MatchServiceError
    defaultService
    "ServiceException"

-- | The quota request template is not associated with your organization.
_ServiceQuotaTemplateNotInUseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaTemplateNotInUseException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaTemplateNotInUseException"

-- | The specified tag is a reserved word and cannot be used.
_TagPolicyViolationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TagPolicyViolationException =
  Core._MatchServiceError
    defaultService
    "TagPolicyViolationException"

-- | The Service Quotas template is not available in this AWS Region.
_TemplatesNotAvailableInRegionException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TemplatesNotAvailableInRegionException =
  Core._MatchServiceError
    defaultService
    "TemplatesNotAvailableInRegionException"

-- | Due to throttling, the request was denied. Slow down the rate of request
-- calls, or request an increase for this quota.
_TooManyRequestsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"

-- | You\'ve exceeded the number of tags allowed for a resource. For more
-- information, see
-- <https://docs.aws.amazon.com/servicequotas/latest/userguide/sq-tagging.html#sq-tagging-restrictions Tag restrictions>
-- in the /Service Quotas User Guide/.
_TooManyTagsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"
