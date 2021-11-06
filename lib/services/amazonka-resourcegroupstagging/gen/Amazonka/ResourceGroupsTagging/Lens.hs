{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ResourceGroupsTagging.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResourceGroupsTagging.Lens
  ( -- * Operations

    -- ** GetComplianceSummary
    getComplianceSummary_groupBy,
    getComplianceSummary_paginationToken,
    getComplianceSummary_targetIdFilters,
    getComplianceSummary_resourceTypeFilters,
    getComplianceSummary_regionFilters,
    getComplianceSummary_tagKeyFilters,
    getComplianceSummary_maxResults,
    getComplianceSummaryResponse_paginationToken,
    getComplianceSummaryResponse_summaryList,
    getComplianceSummaryResponse_httpStatus,

    -- ** DescribeReportCreation
    describeReportCreationResponse_status,
    describeReportCreationResponse_s3Location,
    describeReportCreationResponse_errorMessage,
    describeReportCreationResponse_httpStatus,

    -- ** GetTagKeys
    getTagKeys_paginationToken,
    getTagKeysResponse_paginationToken,
    getTagKeysResponse_tagKeys,
    getTagKeysResponse_httpStatus,

    -- ** TagResources
    tagResources_resourceARNList,
    tagResources_tags,
    tagResourcesResponse_failedResourcesMap,
    tagResourcesResponse_httpStatus,

    -- ** GetTagValues
    getTagValues_paginationToken,
    getTagValues_key,
    getTagValuesResponse_paginationToken,
    getTagValuesResponse_tagValues,
    getTagValuesResponse_httpStatus,

    -- ** StartReportCreation
    startReportCreation_s3Bucket,
    startReportCreationResponse_httpStatus,

    -- ** GetResources
    getResources_paginationToken,
    getResources_resourcesPerPage,
    getResources_excludeCompliantResources,
    getResources_includeComplianceDetails,
    getResources_resourceTypeFilters,
    getResources_tagFilters,
    getResources_resourceARNList,
    getResources_tagsPerPage,
    getResourcesResponse_paginationToken,
    getResourcesResponse_resourceTagMappingList,
    getResourcesResponse_httpStatus,

    -- ** UntagResources
    untagResources_resourceARNList,
    untagResources_tagKeys,
    untagResourcesResponse_failedResourcesMap,
    untagResourcesResponse_httpStatus,

    -- * Types

    -- ** ComplianceDetails
    complianceDetails_keysWithNoncompliantValues,
    complianceDetails_complianceStatus,
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
    summary_targetId,
    summary_lastUpdated,
    summary_resourceType,
    summary_nonCompliantResources,
    summary_targetIdType,
    summary_region,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TagFilter
    tagFilter_values,
    tagFilter_key,
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
