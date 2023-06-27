{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.KendraRanking.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KendraRanking.Lens
  ( -- * Operations

    -- ** CreateRescoreExecutionPlan
    createRescoreExecutionPlan_capacityUnits,
    createRescoreExecutionPlan_clientToken,
    createRescoreExecutionPlan_description,
    createRescoreExecutionPlan_tags,
    createRescoreExecutionPlan_name,
    createRescoreExecutionPlanResponse_httpStatus,
    createRescoreExecutionPlanResponse_id,
    createRescoreExecutionPlanResponse_arn,

    -- ** DeleteRescoreExecutionPlan
    deleteRescoreExecutionPlan_id,

    -- ** DescribeRescoreExecutionPlan
    describeRescoreExecutionPlan_id,
    describeRescoreExecutionPlanResponse_arn,
    describeRescoreExecutionPlanResponse_capacityUnits,
    describeRescoreExecutionPlanResponse_createdAt,
    describeRescoreExecutionPlanResponse_description,
    describeRescoreExecutionPlanResponse_errorMessage,
    describeRescoreExecutionPlanResponse_id,
    describeRescoreExecutionPlanResponse_name,
    describeRescoreExecutionPlanResponse_status,
    describeRescoreExecutionPlanResponse_updatedAt,
    describeRescoreExecutionPlanResponse_httpStatus,

    -- ** ListRescoreExecutionPlans
    listRescoreExecutionPlans_maxResults,
    listRescoreExecutionPlans_nextToken,
    listRescoreExecutionPlansResponse_nextToken,
    listRescoreExecutionPlansResponse_summaryItems,
    listRescoreExecutionPlansResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** Rescore
    rescore_rescoreExecutionPlanId,
    rescore_searchQuery,
    rescore_documents,
    rescoreResponse_rescoreId,
    rescoreResponse_resultItems,
    rescoreResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateRescoreExecutionPlan
    updateRescoreExecutionPlan_capacityUnits,
    updateRescoreExecutionPlan_description,
    updateRescoreExecutionPlan_name,
    updateRescoreExecutionPlan_id,

    -- * Types

    -- ** CapacityUnitsConfiguration
    capacityUnitsConfiguration_rescoreCapacityUnits,

    -- ** Document
    document_body,
    document_groupId,
    document_title,
    document_tokenizedBody,
    document_tokenizedTitle,
    document_id,
    document_originalScore,

    -- ** RescoreExecutionPlanSummary
    rescoreExecutionPlanSummary_createdAt,
    rescoreExecutionPlanSummary_id,
    rescoreExecutionPlanSummary_name,
    rescoreExecutionPlanSummary_status,
    rescoreExecutionPlanSummary_updatedAt,

    -- ** RescoreResultItem
    rescoreResultItem_documentId,
    rescoreResultItem_score,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Amazonka.KendraRanking.CreateRescoreExecutionPlan
import Amazonka.KendraRanking.DeleteRescoreExecutionPlan
import Amazonka.KendraRanking.DescribeRescoreExecutionPlan
import Amazonka.KendraRanking.ListRescoreExecutionPlans
import Amazonka.KendraRanking.ListTagsForResource
import Amazonka.KendraRanking.Rescore
import Amazonka.KendraRanking.TagResource
import Amazonka.KendraRanking.Types.CapacityUnitsConfiguration
import Amazonka.KendraRanking.Types.Document
import Amazonka.KendraRanking.Types.RescoreExecutionPlanSummary
import Amazonka.KendraRanking.Types.RescoreResultItem
import Amazonka.KendraRanking.Types.Tag
import Amazonka.KendraRanking.UntagResource
import Amazonka.KendraRanking.UpdateRescoreExecutionPlan
