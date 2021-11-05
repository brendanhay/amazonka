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

    -- ** ListServices
    listServices_nextToken,
    listServices_maxResults,
    listServicesResponse_nextToken,
    listServicesResponse_services,
    listServicesResponse_httpStatus,

    -- ** ListAWSDefaultServiceQuotas
    listAWSDefaultServiceQuotas_nextToken,
    listAWSDefaultServiceQuotas_maxResults,
    listAWSDefaultServiceQuotas_serviceCode,
    listAWSDefaultServiceQuotasResponse_nextToken,
    listAWSDefaultServiceQuotasResponse_quotas,
    listAWSDefaultServiceQuotasResponse_httpStatus,

    -- ** GetAssociationForServiceQuotaTemplate
    getAssociationForServiceQuotaTemplateResponse_serviceQuotaTemplateAssociationStatus,
    getAssociationForServiceQuotaTemplateResponse_httpStatus,

    -- ** AssociateServiceQuotaTemplate
    associateServiceQuotaTemplateResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** GetServiceQuota
    getServiceQuota_serviceCode,
    getServiceQuota_quotaCode,
    getServiceQuotaResponse_quota,
    getServiceQuotaResponse_httpStatus,

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

    -- ** GetServiceQuotaIncreaseRequestFromTemplate
    getServiceQuotaIncreaseRequestFromTemplate_serviceCode,
    getServiceQuotaIncreaseRequestFromTemplate_quotaCode,
    getServiceQuotaIncreaseRequestFromTemplate_awsRegion,
    getServiceQuotaIncreaseRequestFromTemplateResponse_serviceQuotaIncreaseRequestInTemplate,
    getServiceQuotaIncreaseRequestFromTemplateResponse_httpStatus,

    -- ** DisassociateServiceQuotaTemplate
    disassociateServiceQuotaTemplateResponse_httpStatus,

    -- ** DeleteServiceQuotaIncreaseRequestFromTemplate
    deleteServiceQuotaIncreaseRequestFromTemplate_serviceCode,
    deleteServiceQuotaIncreaseRequestFromTemplate_quotaCode,
    deleteServiceQuotaIncreaseRequestFromTemplate_awsRegion,
    deleteServiceQuotaIncreaseRequestFromTemplateResponse_httpStatus,

    -- ** ListServiceQuotas
    listServiceQuotas_nextToken,
    listServiceQuotas_maxResults,
    listServiceQuotas_serviceCode,
    listServiceQuotasResponse_nextToken,
    listServiceQuotasResponse_quotas,
    listServiceQuotasResponse_httpStatus,

    -- ** ListRequestedServiceQuotaChangeHistory
    listRequestedServiceQuotaChangeHistory_status,
    listRequestedServiceQuotaChangeHistory_nextToken,
    listRequestedServiceQuotaChangeHistory_serviceCode,
    listRequestedServiceQuotaChangeHistory_maxResults,
    listRequestedServiceQuotaChangeHistoryResponse_nextToken,
    listRequestedServiceQuotaChangeHistoryResponse_requestedQuotas,
    listRequestedServiceQuotaChangeHistoryResponse_httpStatus,

    -- ** ListServiceQuotaIncreaseRequestsInTemplate
    listServiceQuotaIncreaseRequestsInTemplate_nextToken,
    listServiceQuotaIncreaseRequestsInTemplate_awsRegion,
    listServiceQuotaIncreaseRequestsInTemplate_serviceCode,
    listServiceQuotaIncreaseRequestsInTemplate_maxResults,
    listServiceQuotaIncreaseRequestsInTemplateResponse_serviceQuotaIncreaseRequestInTemplateList,
    listServiceQuotaIncreaseRequestsInTemplateResponse_nextToken,
    listServiceQuotaIncreaseRequestsInTemplateResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** ListRequestedServiceQuotaChangeHistoryByQuota
    listRequestedServiceQuotaChangeHistoryByQuota_status,
    listRequestedServiceQuotaChangeHistoryByQuota_nextToken,
    listRequestedServiceQuotaChangeHistoryByQuota_maxResults,
    listRequestedServiceQuotaChangeHistoryByQuota_serviceCode,
    listRequestedServiceQuotaChangeHistoryByQuota_quotaCode,
    listRequestedServiceQuotaChangeHistoryByQuotaResponse_nextToken,
    listRequestedServiceQuotaChangeHistoryByQuotaResponse_requestedQuotas,
    listRequestedServiceQuotaChangeHistoryByQuotaResponse_httpStatus,

    -- ** GetRequestedServiceQuotaChange
    getRequestedServiceQuotaChange_requestId,
    getRequestedServiceQuotaChangeResponse_requestedQuota,
    getRequestedServiceQuotaChangeResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** GetAWSDefaultServiceQuota
    getAWSDefaultServiceQuota_serviceCode,
    getAWSDefaultServiceQuota_quotaCode,
    getAWSDefaultServiceQuotaResponse_quota,
    getAWSDefaultServiceQuotaResponse_httpStatus,

    -- * Types

    -- ** ErrorReason
    errorReason_errorCode,
    errorReason_errorMessage,

    -- ** MetricInfo
    metricInfo_metricDimensions,
    metricInfo_metricName,
    metricInfo_metricStatisticRecommendation,
    metricInfo_metricNamespace,

    -- ** QuotaPeriod
    quotaPeriod_periodUnit,
    quotaPeriod_periodValue,

    -- ** RequestedServiceQuotaChange
    requestedServiceQuotaChange_status,
    requestedServiceQuotaChange_lastUpdated,
    requestedServiceQuotaChange_globalQuota,
    requestedServiceQuotaChange_created,
    requestedServiceQuotaChange_desiredValue,
    requestedServiceQuotaChange_quotaArn,
    requestedServiceQuotaChange_caseId,
    requestedServiceQuotaChange_serviceName,
    requestedServiceQuotaChange_id,
    requestedServiceQuotaChange_serviceCode,
    requestedServiceQuotaChange_quotaCode,
    requestedServiceQuotaChange_unit,
    requestedServiceQuotaChange_requester,
    requestedServiceQuotaChange_quotaName,

    -- ** ServiceInfo
    serviceInfo_serviceName,
    serviceInfo_serviceCode,

    -- ** ServiceQuota
    serviceQuota_globalQuota,
    serviceQuota_period,
    serviceQuota_value,
    serviceQuota_quotaArn,
    serviceQuota_usageMetric,
    serviceQuota_errorReason,
    serviceQuota_adjustable,
    serviceQuota_serviceName,
    serviceQuota_serviceCode,
    serviceQuota_quotaCode,
    serviceQuota_unit,
    serviceQuota_quotaName,

    -- ** ServiceQuotaIncreaseRequestInTemplate
    serviceQuotaIncreaseRequestInTemplate_globalQuota,
    serviceQuotaIncreaseRequestInTemplate_desiredValue,
    serviceQuotaIncreaseRequestInTemplate_serviceName,
    serviceQuotaIncreaseRequestInTemplate_awsRegion,
    serviceQuotaIncreaseRequestInTemplate_serviceCode,
    serviceQuotaIncreaseRequestInTemplate_quotaCode,
    serviceQuotaIncreaseRequestInTemplate_unit,
    serviceQuotaIncreaseRequestInTemplate_quotaName,

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
