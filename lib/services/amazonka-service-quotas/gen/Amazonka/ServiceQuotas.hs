{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.ServiceQuotas
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-06-24@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- With Service Quotas, you can view and manage your quotas easily as your
-- AWS workloads grow. Quotas, also referred to as limits, are the maximum
-- number of resources that you can create in your AWS account. For more
-- information, see the
-- <https://docs.aws.amazon.com/servicequotas/latest/userguide/ Service Quotas User Guide>.
module Amazonka.ServiceQuotas
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AWSServiceAccessNotEnabledException
    _AWSServiceAccessNotEnabledException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** DependencyAccessDeniedException
    _DependencyAccessDeniedException,

    -- ** IllegalArgumentException
    _IllegalArgumentException,

    -- ** InvalidPaginationTokenException
    _InvalidPaginationTokenException,

    -- ** InvalidResourceStateException
    _InvalidResourceStateException,

    -- ** NoAvailableOrganizationException
    _NoAvailableOrganizationException,

    -- ** NoSuchResourceException
    _NoSuchResourceException,

    -- ** OrganizationNotInAllFeaturesModeException
    _OrganizationNotInAllFeaturesModeException,

    -- ** QuotaExceededException
    _QuotaExceededException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** ServiceException
    _ServiceException,

    -- ** ServiceQuotaTemplateNotInUseException
    _ServiceQuotaTemplateNotInUseException,

    -- ** TagPolicyViolationException
    _TagPolicyViolationException,

    -- ** TemplatesNotAvailableInRegionException
    _TemplatesNotAvailableInRegionException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateServiceQuotaTemplate
    AssociateServiceQuotaTemplate (AssociateServiceQuotaTemplate'),
    newAssociateServiceQuotaTemplate,
    AssociateServiceQuotaTemplateResponse (AssociateServiceQuotaTemplateResponse'),
    newAssociateServiceQuotaTemplateResponse,

    -- ** DeleteServiceQuotaIncreaseRequestFromTemplate
    DeleteServiceQuotaIncreaseRequestFromTemplate (DeleteServiceQuotaIncreaseRequestFromTemplate'),
    newDeleteServiceQuotaIncreaseRequestFromTemplate,
    DeleteServiceQuotaIncreaseRequestFromTemplateResponse (DeleteServiceQuotaIncreaseRequestFromTemplateResponse'),
    newDeleteServiceQuotaIncreaseRequestFromTemplateResponse,

    -- ** DisassociateServiceQuotaTemplate
    DisassociateServiceQuotaTemplate (DisassociateServiceQuotaTemplate'),
    newDisassociateServiceQuotaTemplate,
    DisassociateServiceQuotaTemplateResponse (DisassociateServiceQuotaTemplateResponse'),
    newDisassociateServiceQuotaTemplateResponse,

    -- ** GetAWSDefaultServiceQuota
    GetAWSDefaultServiceQuota (GetAWSDefaultServiceQuota'),
    newGetAWSDefaultServiceQuota,
    GetAWSDefaultServiceQuotaResponse (GetAWSDefaultServiceQuotaResponse'),
    newGetAWSDefaultServiceQuotaResponse,

    -- ** GetAssociationForServiceQuotaTemplate
    GetAssociationForServiceQuotaTemplate (GetAssociationForServiceQuotaTemplate'),
    newGetAssociationForServiceQuotaTemplate,
    GetAssociationForServiceQuotaTemplateResponse (GetAssociationForServiceQuotaTemplateResponse'),
    newGetAssociationForServiceQuotaTemplateResponse,

    -- ** GetRequestedServiceQuotaChange
    GetRequestedServiceQuotaChange (GetRequestedServiceQuotaChange'),
    newGetRequestedServiceQuotaChange,
    GetRequestedServiceQuotaChangeResponse (GetRequestedServiceQuotaChangeResponse'),
    newGetRequestedServiceQuotaChangeResponse,

    -- ** GetServiceQuota
    GetServiceQuota (GetServiceQuota'),
    newGetServiceQuota,
    GetServiceQuotaResponse (GetServiceQuotaResponse'),
    newGetServiceQuotaResponse,

    -- ** GetServiceQuotaIncreaseRequestFromTemplate
    GetServiceQuotaIncreaseRequestFromTemplate (GetServiceQuotaIncreaseRequestFromTemplate'),
    newGetServiceQuotaIncreaseRequestFromTemplate,
    GetServiceQuotaIncreaseRequestFromTemplateResponse (GetServiceQuotaIncreaseRequestFromTemplateResponse'),
    newGetServiceQuotaIncreaseRequestFromTemplateResponse,

    -- ** ListAWSDefaultServiceQuotas (Paginated)
    ListAWSDefaultServiceQuotas (ListAWSDefaultServiceQuotas'),
    newListAWSDefaultServiceQuotas,
    ListAWSDefaultServiceQuotasResponse (ListAWSDefaultServiceQuotasResponse'),
    newListAWSDefaultServiceQuotasResponse,

    -- ** ListRequestedServiceQuotaChangeHistory (Paginated)
    ListRequestedServiceQuotaChangeHistory (ListRequestedServiceQuotaChangeHistory'),
    newListRequestedServiceQuotaChangeHistory,
    ListRequestedServiceQuotaChangeHistoryResponse (ListRequestedServiceQuotaChangeHistoryResponse'),
    newListRequestedServiceQuotaChangeHistoryResponse,

    -- ** ListRequestedServiceQuotaChangeHistoryByQuota (Paginated)
    ListRequestedServiceQuotaChangeHistoryByQuota (ListRequestedServiceQuotaChangeHistoryByQuota'),
    newListRequestedServiceQuotaChangeHistoryByQuota,
    ListRequestedServiceQuotaChangeHistoryByQuotaResponse (ListRequestedServiceQuotaChangeHistoryByQuotaResponse'),
    newListRequestedServiceQuotaChangeHistoryByQuotaResponse,

    -- ** ListServiceQuotaIncreaseRequestsInTemplate (Paginated)
    ListServiceQuotaIncreaseRequestsInTemplate (ListServiceQuotaIncreaseRequestsInTemplate'),
    newListServiceQuotaIncreaseRequestsInTemplate,
    ListServiceQuotaIncreaseRequestsInTemplateResponse (ListServiceQuotaIncreaseRequestsInTemplateResponse'),
    newListServiceQuotaIncreaseRequestsInTemplateResponse,

    -- ** ListServiceQuotas (Paginated)
    ListServiceQuotas (ListServiceQuotas'),
    newListServiceQuotas,
    ListServiceQuotasResponse (ListServiceQuotasResponse'),
    newListServiceQuotasResponse,

    -- ** ListServices (Paginated)
    ListServices (ListServices'),
    newListServices,
    ListServicesResponse (ListServicesResponse'),
    newListServicesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PutServiceQuotaIncreaseRequestIntoTemplate
    PutServiceQuotaIncreaseRequestIntoTemplate (PutServiceQuotaIncreaseRequestIntoTemplate'),
    newPutServiceQuotaIncreaseRequestIntoTemplate,
    PutServiceQuotaIncreaseRequestIntoTemplateResponse (PutServiceQuotaIncreaseRequestIntoTemplateResponse'),
    newPutServiceQuotaIncreaseRequestIntoTemplateResponse,

    -- ** RequestServiceQuotaIncrease
    RequestServiceQuotaIncrease (RequestServiceQuotaIncrease'),
    newRequestServiceQuotaIncrease,
    RequestServiceQuotaIncreaseResponse (RequestServiceQuotaIncreaseResponse'),
    newRequestServiceQuotaIncreaseResponse,

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

    -- ** ErrorCode
    ErrorCode (..),

    -- ** PeriodUnit
    PeriodUnit (..),

    -- ** RequestStatus
    RequestStatus (..),

    -- ** ServiceQuotaTemplateAssociationStatus
    ServiceQuotaTemplateAssociationStatus (..),

    -- ** ErrorReason
    ErrorReason (ErrorReason'),
    newErrorReason,

    -- ** MetricInfo
    MetricInfo (MetricInfo'),
    newMetricInfo,

    -- ** QuotaPeriod
    QuotaPeriod (QuotaPeriod'),
    newQuotaPeriod,

    -- ** RequestedServiceQuotaChange
    RequestedServiceQuotaChange (RequestedServiceQuotaChange'),
    newRequestedServiceQuotaChange,

    -- ** ServiceInfo
    ServiceInfo (ServiceInfo'),
    newServiceInfo,

    -- ** ServiceQuota
    ServiceQuota (ServiceQuota'),
    newServiceQuota,

    -- ** ServiceQuotaIncreaseRequestInTemplate
    ServiceQuotaIncreaseRequestInTemplate (ServiceQuotaIncreaseRequestInTemplate'),
    newServiceQuotaIncreaseRequestInTemplate,

    -- ** Tag
    Tag (Tag'),
    newTag,
  )
where

import Amazonka.ServiceQuotas.AssociateServiceQuotaTemplate
import Amazonka.ServiceQuotas.DeleteServiceQuotaIncreaseRequestFromTemplate
import Amazonka.ServiceQuotas.DisassociateServiceQuotaTemplate
import Amazonka.ServiceQuotas.GetAWSDefaultServiceQuota
import Amazonka.ServiceQuotas.GetAssociationForServiceQuotaTemplate
import Amazonka.ServiceQuotas.GetRequestedServiceQuotaChange
import Amazonka.ServiceQuotas.GetServiceQuota
import Amazonka.ServiceQuotas.GetServiceQuotaIncreaseRequestFromTemplate
import Amazonka.ServiceQuotas.Lens
import Amazonka.ServiceQuotas.ListAWSDefaultServiceQuotas
import Amazonka.ServiceQuotas.ListRequestedServiceQuotaChangeHistory
import Amazonka.ServiceQuotas.ListRequestedServiceQuotaChangeHistoryByQuota
import Amazonka.ServiceQuotas.ListServiceQuotaIncreaseRequestsInTemplate
import Amazonka.ServiceQuotas.ListServiceQuotas
import Amazonka.ServiceQuotas.ListServices
import Amazonka.ServiceQuotas.ListTagsForResource
import Amazonka.ServiceQuotas.PutServiceQuotaIncreaseRequestIntoTemplate
import Amazonka.ServiceQuotas.RequestServiceQuotaIncrease
import Amazonka.ServiceQuotas.TagResource
import Amazonka.ServiceQuotas.Types
import Amazonka.ServiceQuotas.UntagResource
import Amazonka.ServiceQuotas.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ServiceQuotas'.

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
