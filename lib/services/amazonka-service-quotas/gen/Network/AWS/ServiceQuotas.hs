{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.ServiceQuotas
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.ServiceQuotas
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** TagPolicyViolationException
    _TagPolicyViolationException,

    -- ** NoAvailableOrganizationException
    _NoAvailableOrganizationException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** TemplatesNotAvailableInRegionException
    _TemplatesNotAvailableInRegionException,

    -- ** DependencyAccessDeniedException
    _DependencyAccessDeniedException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** AWSServiceAccessNotEnabledException
    _AWSServiceAccessNotEnabledException,

    -- ** InvalidResourceStateException
    _InvalidResourceStateException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** QuotaExceededException
    _QuotaExceededException,

    -- ** ServiceException
    _ServiceException,

    -- ** IllegalArgumentException
    _IllegalArgumentException,

    -- ** ServiceQuotaTemplateNotInUseException
    _ServiceQuotaTemplateNotInUseException,

    -- ** OrganizationNotInAllFeaturesModeException
    _OrganizationNotInAllFeaturesModeException,

    -- ** InvalidPaginationTokenException
    _InvalidPaginationTokenException,

    -- ** NoSuchResourceException
    _NoSuchResourceException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListServices (Paginated)
    ListServices (ListServices'),
    newListServices,
    ListServicesResponse (ListServicesResponse'),
    newListServicesResponse,

    -- ** ListAWSDefaultServiceQuotas (Paginated)
    ListAWSDefaultServiceQuotas (ListAWSDefaultServiceQuotas'),
    newListAWSDefaultServiceQuotas,
    ListAWSDefaultServiceQuotasResponse (ListAWSDefaultServiceQuotasResponse'),
    newListAWSDefaultServiceQuotasResponse,

    -- ** GetAssociationForServiceQuotaTemplate
    GetAssociationForServiceQuotaTemplate (GetAssociationForServiceQuotaTemplate'),
    newGetAssociationForServiceQuotaTemplate,
    GetAssociationForServiceQuotaTemplateResponse (GetAssociationForServiceQuotaTemplateResponse'),
    newGetAssociationForServiceQuotaTemplateResponse,

    -- ** AssociateServiceQuotaTemplate
    AssociateServiceQuotaTemplate (AssociateServiceQuotaTemplate'),
    newAssociateServiceQuotaTemplate,
    AssociateServiceQuotaTemplateResponse (AssociateServiceQuotaTemplateResponse'),
    newAssociateServiceQuotaTemplateResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** GetServiceQuota
    GetServiceQuota (GetServiceQuota'),
    newGetServiceQuota,
    GetServiceQuotaResponse (GetServiceQuotaResponse'),
    newGetServiceQuotaResponse,

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

    -- ** GetServiceQuotaIncreaseRequestFromTemplate
    GetServiceQuotaIncreaseRequestFromTemplate (GetServiceQuotaIncreaseRequestFromTemplate'),
    newGetServiceQuotaIncreaseRequestFromTemplate,
    GetServiceQuotaIncreaseRequestFromTemplateResponse (GetServiceQuotaIncreaseRequestFromTemplateResponse'),
    newGetServiceQuotaIncreaseRequestFromTemplateResponse,

    -- ** DisassociateServiceQuotaTemplate
    DisassociateServiceQuotaTemplate (DisassociateServiceQuotaTemplate'),
    newDisassociateServiceQuotaTemplate,
    DisassociateServiceQuotaTemplateResponse (DisassociateServiceQuotaTemplateResponse'),
    newDisassociateServiceQuotaTemplateResponse,

    -- ** DeleteServiceQuotaIncreaseRequestFromTemplate
    DeleteServiceQuotaIncreaseRequestFromTemplate (DeleteServiceQuotaIncreaseRequestFromTemplate'),
    newDeleteServiceQuotaIncreaseRequestFromTemplate,
    DeleteServiceQuotaIncreaseRequestFromTemplateResponse (DeleteServiceQuotaIncreaseRequestFromTemplateResponse'),
    newDeleteServiceQuotaIncreaseRequestFromTemplateResponse,

    -- ** ListServiceQuotas (Paginated)
    ListServiceQuotas (ListServiceQuotas'),
    newListServiceQuotas,
    ListServiceQuotasResponse (ListServiceQuotasResponse'),
    newListServiceQuotasResponse,

    -- ** ListRequestedServiceQuotaChangeHistory (Paginated)
    ListRequestedServiceQuotaChangeHistory (ListRequestedServiceQuotaChangeHistory'),
    newListRequestedServiceQuotaChangeHistory,
    ListRequestedServiceQuotaChangeHistoryResponse (ListRequestedServiceQuotaChangeHistoryResponse'),
    newListRequestedServiceQuotaChangeHistoryResponse,

    -- ** ListServiceQuotaIncreaseRequestsInTemplate (Paginated)
    ListServiceQuotaIncreaseRequestsInTemplate (ListServiceQuotaIncreaseRequestsInTemplate'),
    newListServiceQuotaIncreaseRequestsInTemplate,
    ListServiceQuotaIncreaseRequestsInTemplateResponse (ListServiceQuotaIncreaseRequestsInTemplateResponse'),
    newListServiceQuotaIncreaseRequestsInTemplateResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** ListRequestedServiceQuotaChangeHistoryByQuota (Paginated)
    ListRequestedServiceQuotaChangeHistoryByQuota (ListRequestedServiceQuotaChangeHistoryByQuota'),
    newListRequestedServiceQuotaChangeHistoryByQuota,
    ListRequestedServiceQuotaChangeHistoryByQuotaResponse (ListRequestedServiceQuotaChangeHistoryByQuotaResponse'),
    newListRequestedServiceQuotaChangeHistoryByQuotaResponse,

    -- ** GetRequestedServiceQuotaChange
    GetRequestedServiceQuotaChange (GetRequestedServiceQuotaChange'),
    newGetRequestedServiceQuotaChange,
    GetRequestedServiceQuotaChangeResponse (GetRequestedServiceQuotaChangeResponse'),
    newGetRequestedServiceQuotaChangeResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** GetAWSDefaultServiceQuota
    GetAWSDefaultServiceQuota (GetAWSDefaultServiceQuota'),
    newGetAWSDefaultServiceQuota,
    GetAWSDefaultServiceQuotaResponse (GetAWSDefaultServiceQuotaResponse'),
    newGetAWSDefaultServiceQuotaResponse,

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

import Network.AWS.ServiceQuotas.AssociateServiceQuotaTemplate
import Network.AWS.ServiceQuotas.DeleteServiceQuotaIncreaseRequestFromTemplate
import Network.AWS.ServiceQuotas.DisassociateServiceQuotaTemplate
import Network.AWS.ServiceQuotas.GetAWSDefaultServiceQuota
import Network.AWS.ServiceQuotas.GetAssociationForServiceQuotaTemplate
import Network.AWS.ServiceQuotas.GetRequestedServiceQuotaChange
import Network.AWS.ServiceQuotas.GetServiceQuota
import Network.AWS.ServiceQuotas.GetServiceQuotaIncreaseRequestFromTemplate
import Network.AWS.ServiceQuotas.Lens
import Network.AWS.ServiceQuotas.ListAWSDefaultServiceQuotas
import Network.AWS.ServiceQuotas.ListRequestedServiceQuotaChangeHistory
import Network.AWS.ServiceQuotas.ListRequestedServiceQuotaChangeHistoryByQuota
import Network.AWS.ServiceQuotas.ListServiceQuotaIncreaseRequestsInTemplate
import Network.AWS.ServiceQuotas.ListServiceQuotas
import Network.AWS.ServiceQuotas.ListServices
import Network.AWS.ServiceQuotas.ListTagsForResource
import Network.AWS.ServiceQuotas.PutServiceQuotaIncreaseRequestIntoTemplate
import Network.AWS.ServiceQuotas.RequestServiceQuotaIncrease
import Network.AWS.ServiceQuotas.TagResource
import Network.AWS.ServiceQuotas.Types
import Network.AWS.ServiceQuotas.UntagResource
import Network.AWS.ServiceQuotas.Waiters

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
