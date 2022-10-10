{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ResourceGroupsTagging.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResourceGroupsTagging.Lens
  ( -- * Operations

    -- ** DescribeReportCreation
    describeReportCreationResponse_errorMessage,
    describeReportCreationResponse_status,
    describeReportCreationResponse_s3Location,
    describeReportCreationResponse_httpStatus,

    -- ** GetComplianceSummary
    getComplianceSummary_tagKeyFilters,
    getComplianceSummary_targetIdFilters,
    getComplianceSummary_paginationToken,
    getComplianceSummary_groupBy,
    getComplianceSummary_maxResults,
    getComplianceSummary_resourceTypeFilters,
    getComplianceSummary_regionFilters,
    getComplianceSummaryResponse_paginationToken,
    getComplianceSummaryResponse_summaryList,
    getComplianceSummaryResponse_httpStatus,

    -- ** GetResources
    getResources_paginationToken,
    getResources_tagsPerPage,
    getResources_resourceARNList,
    getResources_includeComplianceDetails,
    getResources_tagFilters,
    getResources_excludeCompliantResources,
    getResources_resourcesPerPage,
    getResources_resourceTypeFilters,
    getResourcesResponse_paginationToken,
    getResourcesResponse_resourceTagMappingList,
    getResourcesResponse_httpStatus,

    -- ** GetTagKeys
    getTagKeys_paginationToken,
    getTagKeysResponse_tagKeys,
    getTagKeysResponse_paginationToken,
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
    complianceDetails_noncompliantKeys,
    complianceDetails_complianceStatus,
    complianceDetails_keysWithNoncompliantValues,

    -- ** FailureInfo
    failureInfo_errorMessage,
    failureInfo_errorCode,
    failureInfo_statusCode,

    -- ** ResourceTagMapping
    resourceTagMapping_tags,
    resourceTagMapping_complianceDetails,
    resourceTagMapping_resourceARN,

    -- ** Summary
    summary_resourceType,
    summary_targetId,
    summary_targetIdType,
    summary_lastUpdated,
    summary_region,
    summary_nonCompliantResources,

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
