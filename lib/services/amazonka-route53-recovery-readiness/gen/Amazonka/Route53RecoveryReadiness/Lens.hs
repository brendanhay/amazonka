{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Route53RecoveryReadiness.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryReadiness.Lens
  ( -- * Operations

    -- ** CreateCell
    createCell_cells,
    createCell_tags,
    createCell_cellName,
    createCellResponse_cellArn,
    createCellResponse_cellName,
    createCellResponse_cells,
    createCellResponse_parentReadinessScopes,
    createCellResponse_tags,
    createCellResponse_httpStatus,

    -- ** CreateCrossAccountAuthorization
    createCrossAccountAuthorization_crossAccountAuthorization,
    createCrossAccountAuthorizationResponse_crossAccountAuthorization,
    createCrossAccountAuthorizationResponse_httpStatus,

    -- ** CreateReadinessCheck
    createReadinessCheck_tags,
    createReadinessCheck_resourceSetName,
    createReadinessCheck_readinessCheckName,
    createReadinessCheckResponse_readinessCheckArn,
    createReadinessCheckResponse_readinessCheckName,
    createReadinessCheckResponse_resourceSet,
    createReadinessCheckResponse_tags,
    createReadinessCheckResponse_httpStatus,

    -- ** CreateRecoveryGroup
    createRecoveryGroup_cells,
    createRecoveryGroup_tags,
    createRecoveryGroup_recoveryGroupName,
    createRecoveryGroupResponse_cells,
    createRecoveryGroupResponse_recoveryGroupArn,
    createRecoveryGroupResponse_recoveryGroupName,
    createRecoveryGroupResponse_tags,
    createRecoveryGroupResponse_httpStatus,

    -- ** CreateResourceSet
    createResourceSet_tags,
    createResourceSet_resourceSetType,
    createResourceSet_resourceSetName,
    createResourceSet_resources,
    createResourceSetResponse_resourceSetArn,
    createResourceSetResponse_resourceSetName,
    createResourceSetResponse_resourceSetType,
    createResourceSetResponse_resources,
    createResourceSetResponse_tags,
    createResourceSetResponse_httpStatus,

    -- ** DeleteCell
    deleteCell_cellName,

    -- ** DeleteCrossAccountAuthorization
    deleteCrossAccountAuthorization_crossAccountAuthorization,
    deleteCrossAccountAuthorizationResponse_httpStatus,

    -- ** DeleteReadinessCheck
    deleteReadinessCheck_readinessCheckName,

    -- ** DeleteRecoveryGroup
    deleteRecoveryGroup_recoveryGroupName,

    -- ** DeleteResourceSet
    deleteResourceSet_resourceSetName,

    -- ** GetArchitectureRecommendations
    getArchitectureRecommendations_maxResults,
    getArchitectureRecommendations_nextToken,
    getArchitectureRecommendations_recoveryGroupName,
    getArchitectureRecommendationsResponse_lastAuditTimestamp,
    getArchitectureRecommendationsResponse_nextToken,
    getArchitectureRecommendationsResponse_recommendations,
    getArchitectureRecommendationsResponse_httpStatus,

    -- ** GetCell
    getCell_cellName,
    getCellResponse_cellArn,
    getCellResponse_cellName,
    getCellResponse_cells,
    getCellResponse_parentReadinessScopes,
    getCellResponse_tags,
    getCellResponse_httpStatus,

    -- ** GetCellReadinessSummary
    getCellReadinessSummary_maxResults,
    getCellReadinessSummary_nextToken,
    getCellReadinessSummary_cellName,
    getCellReadinessSummaryResponse_nextToken,
    getCellReadinessSummaryResponse_readiness,
    getCellReadinessSummaryResponse_readinessChecks,
    getCellReadinessSummaryResponse_httpStatus,

    -- ** GetReadinessCheck
    getReadinessCheck_readinessCheckName,
    getReadinessCheckResponse_readinessCheckArn,
    getReadinessCheckResponse_readinessCheckName,
    getReadinessCheckResponse_resourceSet,
    getReadinessCheckResponse_tags,
    getReadinessCheckResponse_httpStatus,

    -- ** GetReadinessCheckResourceStatus
    getReadinessCheckResourceStatus_maxResults,
    getReadinessCheckResourceStatus_nextToken,
    getReadinessCheckResourceStatus_readinessCheckName,
    getReadinessCheckResourceStatus_resourceIdentifier,
    getReadinessCheckResourceStatusResponse_nextToken,
    getReadinessCheckResourceStatusResponse_readiness,
    getReadinessCheckResourceStatusResponse_rules,
    getReadinessCheckResourceStatusResponse_httpStatus,

    -- ** GetReadinessCheckStatus
    getReadinessCheckStatus_maxResults,
    getReadinessCheckStatus_nextToken,
    getReadinessCheckStatus_readinessCheckName,
    getReadinessCheckStatusResponse_messages,
    getReadinessCheckStatusResponse_nextToken,
    getReadinessCheckStatusResponse_readiness,
    getReadinessCheckStatusResponse_resources,
    getReadinessCheckStatusResponse_httpStatus,

    -- ** GetRecoveryGroup
    getRecoveryGroup_recoveryGroupName,
    getRecoveryGroupResponse_cells,
    getRecoveryGroupResponse_recoveryGroupArn,
    getRecoveryGroupResponse_recoveryGroupName,
    getRecoveryGroupResponse_tags,
    getRecoveryGroupResponse_httpStatus,

    -- ** GetRecoveryGroupReadinessSummary
    getRecoveryGroupReadinessSummary_maxResults,
    getRecoveryGroupReadinessSummary_nextToken,
    getRecoveryGroupReadinessSummary_recoveryGroupName,
    getRecoveryGroupReadinessSummaryResponse_nextToken,
    getRecoveryGroupReadinessSummaryResponse_readiness,
    getRecoveryGroupReadinessSummaryResponse_readinessChecks,
    getRecoveryGroupReadinessSummaryResponse_httpStatus,

    -- ** GetResourceSet
    getResourceSet_resourceSetName,
    getResourceSetResponse_resourceSetArn,
    getResourceSetResponse_resourceSetName,
    getResourceSetResponse_resourceSetType,
    getResourceSetResponse_resources,
    getResourceSetResponse_tags,
    getResourceSetResponse_httpStatus,

    -- ** ListCells
    listCells_maxResults,
    listCells_nextToken,
    listCellsResponse_cells,
    listCellsResponse_nextToken,
    listCellsResponse_httpStatus,

    -- ** ListCrossAccountAuthorizations
    listCrossAccountAuthorizations_maxResults,
    listCrossAccountAuthorizations_nextToken,
    listCrossAccountAuthorizationsResponse_crossAccountAuthorizations,
    listCrossAccountAuthorizationsResponse_nextToken,
    listCrossAccountAuthorizationsResponse_httpStatus,

    -- ** ListReadinessChecks
    listReadinessChecks_maxResults,
    listReadinessChecks_nextToken,
    listReadinessChecksResponse_nextToken,
    listReadinessChecksResponse_readinessChecks,
    listReadinessChecksResponse_httpStatus,

    -- ** ListRecoveryGroups
    listRecoveryGroups_maxResults,
    listRecoveryGroups_nextToken,
    listRecoveryGroupsResponse_nextToken,
    listRecoveryGroupsResponse_recoveryGroups,
    listRecoveryGroupsResponse_httpStatus,

    -- ** ListResourceSets
    listResourceSets_maxResults,
    listResourceSets_nextToken,
    listResourceSetsResponse_nextToken,
    listResourceSetsResponse_resourceSets,
    listResourceSetsResponse_httpStatus,

    -- ** ListRules
    listRules_maxResults,
    listRules_nextToken,
    listRules_resourceType,
    listRulesResponse_nextToken,
    listRulesResponse_rules,
    listRulesResponse_httpStatus,

    -- ** ListTagsForResources
    listTagsForResources_resourceArn,
    listTagsForResourcesResponse_tags,
    listTagsForResourcesResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_tagKeys,
    untagResource_resourceArn,

    -- ** UpdateCell
    updateCell_cellName,
    updateCell_cells,
    updateCellResponse_cellArn,
    updateCellResponse_cellName,
    updateCellResponse_cells,
    updateCellResponse_parentReadinessScopes,
    updateCellResponse_tags,
    updateCellResponse_httpStatus,

    -- ** UpdateReadinessCheck
    updateReadinessCheck_readinessCheckName,
    updateReadinessCheck_resourceSetName,
    updateReadinessCheckResponse_readinessCheckArn,
    updateReadinessCheckResponse_readinessCheckName,
    updateReadinessCheckResponse_resourceSet,
    updateReadinessCheckResponse_tags,
    updateReadinessCheckResponse_httpStatus,

    -- ** UpdateRecoveryGroup
    updateRecoveryGroup_recoveryGroupName,
    updateRecoveryGroup_cells,
    updateRecoveryGroupResponse_cells,
    updateRecoveryGroupResponse_recoveryGroupArn,
    updateRecoveryGroupResponse_recoveryGroupName,
    updateRecoveryGroupResponse_tags,
    updateRecoveryGroupResponse_httpStatus,

    -- ** UpdateResourceSet
    updateResourceSet_resourceSetName,
    updateResourceSet_resourceSetType,
    updateResourceSet_resources,
    updateResourceSetResponse_resourceSetArn,
    updateResourceSetResponse_resourceSetName,
    updateResourceSetResponse_resourceSetType,
    updateResourceSetResponse_resources,
    updateResourceSetResponse_tags,
    updateResourceSetResponse_httpStatus,

    -- * Types

    -- ** CellOutput
    cellOutput_tags,
    cellOutput_parentReadinessScopes,
    cellOutput_cellArn,
    cellOutput_cellName,
    cellOutput_cells,

    -- ** DNSTargetResource
    dNSTargetResource_domainName,
    dNSTargetResource_hostedZoneArn,
    dNSTargetResource_recordSetId,
    dNSTargetResource_recordType,
    dNSTargetResource_targetResource,

    -- ** ListRulesOutput
    listRulesOutput_ruleDescription,
    listRulesOutput_ruleId,
    listRulesOutput_resourceType,

    -- ** Message
    message_messageText,

    -- ** NLBResource
    nLBResource_arn,

    -- ** R53ResourceRecord
    r53ResourceRecord_domainName,
    r53ResourceRecord_recordSetId,

    -- ** ReadinessCheckOutput
    readinessCheckOutput_readinessCheckName,
    readinessCheckOutput_tags,
    readinessCheckOutput_readinessCheckArn,
    readinessCheckOutput_resourceSet,

    -- ** ReadinessCheckSummary
    readinessCheckSummary_readiness,
    readinessCheckSummary_readinessCheckName,

    -- ** Recommendation
    recommendation_recommendationText,

    -- ** RecoveryGroupOutput
    recoveryGroupOutput_tags,
    recoveryGroupOutput_recoveryGroupArn,
    recoveryGroupOutput_recoveryGroupName,
    recoveryGroupOutput_cells,

    -- ** Resource
    resource_componentId,
    resource_dnsTargetResource,
    resource_readinessScopes,
    resource_resourceArn,

    -- ** ResourceResult
    resourceResult_componentId,
    resourceResult_resourceArn,
    resourceResult_readiness,
    resourceResult_lastCheckedTimestamp,

    -- ** ResourceSetOutput
    resourceSetOutput_tags,
    resourceSetOutput_resourceSetType,
    resourceSetOutput_resourceSetName,
    resourceSetOutput_resourceSetArn,
    resourceSetOutput_resources,

    -- ** RuleResult
    ruleResult_messages,
    ruleResult_readiness,
    ruleResult_ruleId,
    ruleResult_lastCheckedTimestamp,

    -- ** TargetResource
    targetResource_nLBResource,
    targetResource_r53Resource,
  )
where

import Amazonka.Route53RecoveryReadiness.CreateCell
import Amazonka.Route53RecoveryReadiness.CreateCrossAccountAuthorization
import Amazonka.Route53RecoveryReadiness.CreateReadinessCheck
import Amazonka.Route53RecoveryReadiness.CreateRecoveryGroup
import Amazonka.Route53RecoveryReadiness.CreateResourceSet
import Amazonka.Route53RecoveryReadiness.DeleteCell
import Amazonka.Route53RecoveryReadiness.DeleteCrossAccountAuthorization
import Amazonka.Route53RecoveryReadiness.DeleteReadinessCheck
import Amazonka.Route53RecoveryReadiness.DeleteRecoveryGroup
import Amazonka.Route53RecoveryReadiness.DeleteResourceSet
import Amazonka.Route53RecoveryReadiness.GetArchitectureRecommendations
import Amazonka.Route53RecoveryReadiness.GetCell
import Amazonka.Route53RecoveryReadiness.GetCellReadinessSummary
import Amazonka.Route53RecoveryReadiness.GetReadinessCheck
import Amazonka.Route53RecoveryReadiness.GetReadinessCheckResourceStatus
import Amazonka.Route53RecoveryReadiness.GetReadinessCheckStatus
import Amazonka.Route53RecoveryReadiness.GetRecoveryGroup
import Amazonka.Route53RecoveryReadiness.GetRecoveryGroupReadinessSummary
import Amazonka.Route53RecoveryReadiness.GetResourceSet
import Amazonka.Route53RecoveryReadiness.ListCells
import Amazonka.Route53RecoveryReadiness.ListCrossAccountAuthorizations
import Amazonka.Route53RecoveryReadiness.ListReadinessChecks
import Amazonka.Route53RecoveryReadiness.ListRecoveryGroups
import Amazonka.Route53RecoveryReadiness.ListResourceSets
import Amazonka.Route53RecoveryReadiness.ListRules
import Amazonka.Route53RecoveryReadiness.ListTagsForResources
import Amazonka.Route53RecoveryReadiness.TagResource
import Amazonka.Route53RecoveryReadiness.Types.CellOutput
import Amazonka.Route53RecoveryReadiness.Types.DNSTargetResource
import Amazonka.Route53RecoveryReadiness.Types.ListRulesOutput
import Amazonka.Route53RecoveryReadiness.Types.Message
import Amazonka.Route53RecoveryReadiness.Types.NLBResource
import Amazonka.Route53RecoveryReadiness.Types.R53ResourceRecord
import Amazonka.Route53RecoveryReadiness.Types.ReadinessCheckOutput
import Amazonka.Route53RecoveryReadiness.Types.ReadinessCheckSummary
import Amazonka.Route53RecoveryReadiness.Types.Recommendation
import Amazonka.Route53RecoveryReadiness.Types.RecoveryGroupOutput
import Amazonka.Route53RecoveryReadiness.Types.Resource
import Amazonka.Route53RecoveryReadiness.Types.ResourceResult
import Amazonka.Route53RecoveryReadiness.Types.ResourceSetOutput
import Amazonka.Route53RecoveryReadiness.Types.RuleResult
import Amazonka.Route53RecoveryReadiness.Types.TargetResource
import Amazonka.Route53RecoveryReadiness.UntagResource
import Amazonka.Route53RecoveryReadiness.UpdateCell
import Amazonka.Route53RecoveryReadiness.UpdateReadinessCheck
import Amazonka.Route53RecoveryReadiness.UpdateRecoveryGroup
import Amazonka.Route53RecoveryReadiness.UpdateResourceSet
