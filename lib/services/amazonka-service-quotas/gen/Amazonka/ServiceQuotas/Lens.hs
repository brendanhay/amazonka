{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ServiceQuotas.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceQuotas.Lens
  ( -- * Operations

    -- ** AssociateServiceQuotaTemplate
    associateServiceQuotaTemplateResponse_httpStatus,

    -- ** DeleteServiceQuotaIncreaseRequestFromTemplate
    deleteServiceQuotaIncreaseRequestFromTemplate_serviceCode,
    deleteServiceQuotaIncreaseRequestFromTemplate_quotaCode,
    deleteServiceQuotaIncreaseRequestFromTemplate_awsRegion,
    deleteServiceQuotaIncreaseRequestFromTemplateResponse_httpStatus,

    -- ** DisassociateServiceQuotaTemplate
    disassociateServiceQuotaTemplateResponse_httpStatus,

    -- ** GetAWSDefaultServiceQuota
    getAWSDefaultServiceQuota_serviceCode,
    getAWSDefaultServiceQuota_quotaCode,
    getAWSDefaultServiceQuotaResponse_quota,
    getAWSDefaultServiceQuotaResponse_httpStatus,

    -- ** GetAssociationForServiceQuotaTemplate
    getAssociationForServiceQuotaTemplateResponse_serviceQuotaTemplateAssociationStatus,
    getAssociationForServiceQuotaTemplateResponse_httpStatus,

    -- ** GetRequestedServiceQuotaChange
    getRequestedServiceQuotaChange_requestId,
    getRequestedServiceQuotaChangeResponse_requestedQuota,
    getRequestedServiceQuotaChangeResponse_httpStatus,

    -- ** GetServiceQuota
    getServiceQuota_serviceCode,
    getServiceQuota_quotaCode,
    getServiceQuotaResponse_quota,
    getServiceQuotaResponse_httpStatus,

    -- ** GetServiceQuotaIncreaseRequestFromTemplate
    getServiceQuotaIncreaseRequestFromTemplate_serviceCode,
    getServiceQuotaIncreaseRequestFromTemplate_quotaCode,
    getServiceQuotaIncreaseRequestFromTemplate_awsRegion,
    getServiceQuotaIncreaseRequestFromTemplateResponse_serviceQuotaIncreaseRequestInTemplate,
    getServiceQuotaIncreaseRequestFromTemplateResponse_httpStatus,

    -- ** ListAWSDefaultServiceQuotas
    listAWSDefaultServiceQuotas_nextToken,
    listAWSDefaultServiceQuotas_maxResults,
    listAWSDefaultServiceQuotas_serviceCode,
    listAWSDefaultServiceQuotasResponse_quotas,
    listAWSDefaultServiceQuotasResponse_nextToken,
    listAWSDefaultServiceQuotasResponse_httpStatus,

    -- ** ListRequestedServiceQuotaChangeHistory
    listRequestedServiceQuotaChangeHistory_nextToken,
    listRequestedServiceQuotaChangeHistory_serviceCode,
    listRequestedServiceQuotaChangeHistory_status,
    listRequestedServiceQuotaChangeHistory_maxResults,
    listRequestedServiceQuotaChangeHistoryResponse_nextToken,
    listRequestedServiceQuotaChangeHistoryResponse_requestedQuotas,
    listRequestedServiceQuotaChangeHistoryResponse_httpStatus,

    -- ** ListRequestedServiceQuotaChangeHistoryByQuota
    listRequestedServiceQuotaChangeHistoryByQuota_nextToken,
    listRequestedServiceQuotaChangeHistoryByQuota_status,
    listRequestedServiceQuotaChangeHistoryByQuota_maxResults,
    listRequestedServiceQuotaChangeHistoryByQuota_serviceCode,
    listRequestedServiceQuotaChangeHistoryByQuota_quotaCode,
    listRequestedServiceQuotaChangeHistoryByQuotaResponse_nextToken,
    listRequestedServiceQuotaChangeHistoryByQuotaResponse_requestedQuotas,
    listRequestedServiceQuotaChangeHistoryByQuotaResponse_httpStatus,

    -- ** ListServiceQuotaIncreaseRequestsInTemplate
    listServiceQuotaIncreaseRequestsInTemplate_nextToken,
    listServiceQuotaIncreaseRequestsInTemplate_serviceCode,
    listServiceQuotaIncreaseRequestsInTemplate_maxResults,
    listServiceQuotaIncreaseRequestsInTemplate_awsRegion,
    listServiceQuotaIncreaseRequestsInTemplateResponse_nextToken,
    listServiceQuotaIncreaseRequestsInTemplateResponse_serviceQuotaIncreaseRequestInTemplateList,
    listServiceQuotaIncreaseRequestsInTemplateResponse_httpStatus,

    -- ** ListServiceQuotas
    listServiceQuotas_nextToken,
    listServiceQuotas_maxResults,
    listServiceQuotas_serviceCode,
    listServiceQuotasResponse_quotas,
    listServiceQuotasResponse_nextToken,
    listServiceQuotasResponse_httpStatus,

    -- ** ListServices
    listServices_nextToken,
    listServices_maxResults,
    listServicesResponse_nextToken,
    listServicesResponse_services,
    listServicesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutServiceQuotaIncreaseRequestIntoTemplate
    putServiceQuotaIncreaseRequestIntoTemplate_quotaCode,
    putServiceQuotaIncreaseRequestIntoTemplate_serviceCode,
    putServiceQuotaIncreaseRequestIntoTemplate_awsRegion,
    putServiceQuotaIncreaseRequestIntoTemplate_desiredValue,
    putServiceQuotaIncreaseRequestIntoTemplateResponse_serviceQuotaIncreaseRequestInTemplate,
    putServiceQuotaIncreaseRequestIntoTemplateResponse_httpStatus,

    -- ** RequestServiceQuotaIncrease
    requestServiceQuotaIncrease_serviceCode,
    requestServiceQuotaIncrease_quotaCode,
    requestServiceQuotaIncrease_desiredValue,
    requestServiceQuotaIncreaseResponse_requestedQuota,
    requestServiceQuotaIncreaseResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- * Types

    -- ** ErrorReason
    errorReason_errorMessage,
    errorReason_errorCode,

    -- ** MetricInfo
    metricInfo_metricStatisticRecommendation,
    metricInfo_metricName,
    metricInfo_metricDimensions,
    metricInfo_metricNamespace,

    -- ** QuotaPeriod
    quotaPeriod_periodUnit,
    quotaPeriod_periodValue,

    -- ** RequestedServiceQuotaChange
    requestedServiceQuotaChange_quotaArn,
    requestedServiceQuotaChange_globalQuota,
    requestedServiceQuotaChange_quotaCode,
    requestedServiceQuotaChange_caseId,
    requestedServiceQuotaChange_created,
    requestedServiceQuotaChange_serviceCode,
    requestedServiceQuotaChange_status,
    requestedServiceQuotaChange_id,
    requestedServiceQuotaChange_lastUpdated,
    requestedServiceQuotaChange_quotaName,
    requestedServiceQuotaChange_serviceName,
    requestedServiceQuotaChange_requester,
    requestedServiceQuotaChange_unit,
    requestedServiceQuotaChange_desiredValue,

    -- ** ServiceInfo
    serviceInfo_serviceCode,
    serviceInfo_serviceName,

    -- ** ServiceQuota
    serviceQuota_quotaArn,
    serviceQuota_globalQuota,
    serviceQuota_quotaCode,
    serviceQuota_usageMetric,
    serviceQuota_adjustable,
    serviceQuota_errorReason,
    serviceQuota_period,
    serviceQuota_serviceCode,
    serviceQuota_quotaName,
    serviceQuota_serviceName,
    serviceQuota_unit,
    serviceQuota_value,

    -- ** ServiceQuotaIncreaseRequestInTemplate
    serviceQuotaIncreaseRequestInTemplate_globalQuota,
    serviceQuotaIncreaseRequestInTemplate_quotaCode,
    serviceQuotaIncreaseRequestInTemplate_serviceCode,
    serviceQuotaIncreaseRequestInTemplate_quotaName,
    serviceQuotaIncreaseRequestInTemplate_awsRegion,
    serviceQuotaIncreaseRequestInTemplate_serviceName,
    serviceQuotaIncreaseRequestInTemplate_unit,
    serviceQuotaIncreaseRequestInTemplate_desiredValue,

    -- ** Tag
    tag_key,
    tag_value,
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
import Amazonka.ServiceQuotas.Types.ErrorReason
import Amazonka.ServiceQuotas.Types.MetricInfo
import Amazonka.ServiceQuotas.Types.QuotaPeriod
import Amazonka.ServiceQuotas.Types.RequestedServiceQuotaChange
import Amazonka.ServiceQuotas.Types.ServiceInfo
import Amazonka.ServiceQuotas.Types.ServiceQuota
import Amazonka.ServiceQuotas.Types.ServiceQuotaIncreaseRequestInTemplate
import Amazonka.ServiceQuotas.Types.Tag
import Amazonka.ServiceQuotas.UntagResource
