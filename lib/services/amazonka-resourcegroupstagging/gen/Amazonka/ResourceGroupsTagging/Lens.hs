{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ResourceGroupsTagging.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResourceGroupsTagging.Lens
  ( -- * Operations

    -- ** DescribeReportCreation
    describeReportCreationResponse_errorMessage,
    describeReportCreationResponse_s3Location,
    describeReportCreationResponse_status,
    describeReportCreationResponse_httpStatus,

    -- ** GetComplianceSummary
    getComplianceSummary_groupBy,
    getComplianceSummary_maxResults,
    getComplianceSummary_paginationToken,
    getComplianceSummary_regionFilters,
    getComplianceSummary_resourceTypeFilters,
    getComplianceSummary_tagKeyFilters,
    getComplianceSummary_targetIdFilters,
    getComplianceSummaryResponse_paginationToken,
    getComplianceSummaryResponse_summaryList,
    getComplianceSummaryResponse_httpStatus,

    -- ** GetResources
    getResources_excludeCompliantResources,
    getResources_includeComplianceDetails,
    getResources_paginationToken,
    getResources_resourceARNList,
    getResources_resourceTypeFilters,
    getResources_resourcesPerPage,
    getResources_tagFilters,
    getResources_tagsPerPage,
    getResourcesResponse_paginationToken,
    getResourcesResponse_resourceTagMappingList,
    getResourcesResponse_httpStatus,

    -- ** GetTagKeys
    getTagKeys_paginationToken,
    getTagKeysResponse_paginationToken,
    getTagKeysResponse_tagKeys,
    getTagKeysResponse_httpStatus,

    -- ** GetTagValues
    getTagValues_paginationToken,
    getTagValues_key,
    getTagValuesResponse_paginationToken,
    getTagValuesResponse_tagValues,
    getTagValuesResponse_httpStatus,

    -- ** StartReportCreation
    startReportCreation_s3Bucket,
    startReportCreationResponse_httpStatus,

    -- ** TagResources
    tagResources_resourceARNList,
    tagResources_tags,
    tagResourcesResponse_failedResourcesMap,
    tagResourcesResponse_httpStatus,

    -- ** UntagResources
    untagResources_resourceARNList,
    untagResources_tagKeys,
    untagResourcesResponse_failedResourcesMap,
    untagResourcesResponse_httpStatus,

    -- * Types

    -- ** ComplianceDetails
    complianceDetails_complianceStatus,
    complianceDetails_keysWithNoncompliantValues,
    complianceDetails_noncompliantKeys,

    -- ** FailureInfo
    failureInfo_errorCode,
    failureInfo_errorMessage,
    failureInfo_statusCode,

    -- ** ResourceTagMapping
    resourceTagMapping_complianceDetails,
    resourceTagMapping_resourceARN,
    resourceTagMapping_tags,

    -- ** Summary
    summary_lastUpdated,
    summary_nonCompliantResources,
    summary_region,
    summary_resourceType,
    summary_targetId,
    summary_targetIdType,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TagFilter
    tagFilter_key,
    tagFilter_values,
  )
where

import Amazonka.ResourceGroupsTagging.DescribeReportCreation
import Amazonka.ResourceGroupsTagging.GetComplianceSummary
import Amazonka.ResourceGroupsTagging.GetResources
import Amazonka.ResourceGroupsTagging.GetTagKeys
import Amazonka.ResourceGroupsTagging.GetTagValues
import Amazonka.ResourceGroupsTagging.StartReportCreation
import Amazonka.ResourceGroupsTagging.TagResources
import Amazonka.ResourceGroupsTagging.Types.ComplianceDetails
import Amazonka.ResourceGroupsTagging.Types.FailureInfo
import Amazonka.ResourceGroupsTagging.Types.ResourceTagMapping
import Amazonka.ResourceGroupsTagging.Types.Summary
import Amazonka.ResourceGroupsTagging.Types.Tag
import Amazonka.ResourceGroupsTagging.Types.TagFilter
import Amazonka.ResourceGroupsTagging.UntagResources
