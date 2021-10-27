{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53RecoveryReadiness.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53RecoveryReadiness.Lens
  ( -- * Operations

    -- ** CreateResourceSet
    createResourceSet_tags,
    createResourceSet_resourceSetType,
    createResourceSet_resourceSetName,
    createResourceSet_resources,
    createResourceSetResponse_resourceSetName,
    createResourceSetResponse_resourceSetType,
    createResourceSetResponse_resources,
    createResourceSetResponse_resourceSetArn,
    createResourceSetResponse_tags,
    createResourceSetResponse_httpStatus,

    -- ** GetReadinessCheckStatus
    getReadinessCheckStatus_nextToken,
    getReadinessCheckStatus_maxResults,
    getReadinessCheckStatus_readinessCheckName,
    getReadinessCheckStatusResponse_readiness,
    getReadinessCheckStatusResponse_resources,
    getReadinessCheckStatusResponse_nextToken,
    getReadinessCheckStatusResponse_messages,
    getReadinessCheckStatusResponse_httpStatus,

    -- ** GetCellReadinessSummary
    getCellReadinessSummary_nextToken,
    getCellReadinessSummary_maxResults,
    getCellReadinessSummary_cellName,
    getCellReadinessSummaryResponse_readinessChecks,
    getCellReadinessSummaryResponse_readiness,
    getCellReadinessSummaryResponse_nextToken,
    getCellReadinessSummaryResponse_httpStatus,

    -- ** UpdateCell
    updateCell_cellName,
    updateCell_cells,
    updateCellResponse_cells,
    updateCellResponse_parentReadinessScopes,
    updateCellResponse_cellName,
    updateCellResponse_cellArn,
    updateCellResponse_tags,
    updateCellResponse_httpStatus,

    -- ** DeleteCell
    deleteCell_cellName,

    -- ** UpdateReadinessCheck
    updateReadinessCheck_readinessCheckName,
    updateReadinessCheck_resourceSetName,
    updateReadinessCheckResponse_readinessCheckName,
    updateReadinessCheckResponse_resourceSet,
    updateReadinessCheckResponse_readinessCheckArn,
    updateReadinessCheckResponse_tags,
    updateReadinessCheckResponse_httpStatus,

    -- ** DeleteReadinessCheck
    deleteReadinessCheck_readinessCheckName,

    -- ** ListCells
    listCells_nextToken,
    listCells_maxResults,
    listCellsResponse_cells,
    listCellsResponse_nextToken,
    listCellsResponse_httpStatus,

    -- ** ListReadinessChecks
    listReadinessChecks_nextToken,
    listReadinessChecks_maxResults,
    listReadinessChecksResponse_readinessChecks,
    listReadinessChecksResponse_nextToken,
    listReadinessChecksResponse_httpStatus,

    -- ** ListRules
    listRules_resourceType,
    listRules_nextToken,
    listRules_maxResults,
    listRulesResponse_rules,
    listRulesResponse_nextToken,
    listRulesResponse_httpStatus,

    -- ** CreateReadinessCheck
    createReadinessCheck_tags,
    createReadinessCheck_resourceSetName,
    createReadinessCheck_readinessCheckName,
    createReadinessCheckResponse_readinessCheckName,
    createReadinessCheckResponse_resourceSet,
    createReadinessCheckResponse_readinessCheckArn,
    createReadinessCheckResponse_tags,
    createReadinessCheckResponse_httpStatus,

    -- ** CreateCell
    createCell_cells,
    createCell_tags,
    createCell_cellName,
    createCellResponse_cells,
    createCellResponse_parentReadinessScopes,
    createCellResponse_cellName,
    createCellResponse_cellArn,
    createCellResponse_tags,
    createCellResponse_httpStatus,

    -- ** GetRecoveryGroup
    getRecoveryGroup_recoveryGroupName,
    getRecoveryGroupResponse_cells,
    getRecoveryGroupResponse_recoveryGroupName,
    getRecoveryGroupResponse_recoveryGroupArn,
    getRecoveryGroupResponse_tags,
    getRecoveryGroupResponse_httpStatus,

    -- ** ListRecoveryGroups
    listRecoveryGroups_nextToken,
    listRecoveryGroups_maxResults,
    listRecoveryGroupsResponse_recoveryGroups,
    listRecoveryGroupsResponse_nextToken,
    listRecoveryGroupsResponse_httpStatus,

    -- ** ListCrossAccountAuthorizations
    listCrossAccountAuthorizations_nextToken,
    listCrossAccountAuthorizations_maxResults,
    listCrossAccountAuthorizationsResponse_crossAccountAuthorizations,
    listCrossAccountAuthorizationsResponse_nextToken,
    listCrossAccountAuthorizationsResponse_httpStatus,

    -- ** GetCell
    getCell_cellName,
    getCellResponse_cells,
    getCellResponse_parentReadinessScopes,
    getCellResponse_cellName,
    getCellResponse_cellArn,
    getCellResponse_tags,
    getCellResponse_httpStatus,

    -- ** CreateCrossAccountAuthorization
    createCrossAccountAuthorization_crossAccountAuthorization,
    createCrossAccountAuthorizationResponse_crossAccountAuthorization,
    createCrossAccountAuthorizationResponse_httpStatus,

    -- ** CreateRecoveryGroup
    createRecoveryGroup_cells,
    createRecoveryGroup_tags,
    createRecoveryGroup_recoveryGroupName,
    createRecoveryGroupResponse_cells,
    createRecoveryGroupResponse_recoveryGroupName,
    createRecoveryGroupResponse_recoveryGroupArn,
    createRecoveryGroupResponse_tags,
    createRecoveryGroupResponse_httpStatus,

    -- ** GetReadinessCheck
    getReadinessCheck_readinessCheckName,
    getReadinessCheckResponse_readinessCheckName,
    getReadinessCheckResponse_resourceSet,
    getReadinessCheckResponse_readinessCheckArn,
    getReadinessCheckResponse_tags,
    getReadinessCheckResponse_httpStatus,

    -- ** GetReadinessCheckResourceStatus
    getReadinessCheckResourceStatus_nextToken,
    getReadinessCheckResourceStatus_maxResults,
    getReadinessCheckResourceStatus_readinessCheckName,
    getReadinessCheckResourceStatus_resourceIdentifier,
    getReadinessCheckResourceStatusResponse_rules,
    getReadinessCheckResourceStatusResponse_readiness,
    getReadinessCheckResourceStatusResponse_nextToken,
    getReadinessCheckResourceStatusResponse_httpStatus,

    -- ** ListResourceSets
    listResourceSets_nextToken,
    listResourceSets_maxResults,
    listResourceSetsResponse_resourceSets,
    listResourceSetsResponse_nextToken,
    listResourceSetsResponse_httpStatus,

    -- ** GetArchitectureRecommendations
    getArchitectureRecommendations_nextToken,
    getArchitectureRecommendations_maxResults,
    getArchitectureRecommendations_recoveryGroupName,
    getArchitectureRecommendationsResponse_lastAuditTimestamp,
    getArchitectureRecommendationsResponse_nextToken,
    getArchitectureRecommendationsResponse_recommendations,
    getArchitectureRecommendationsResponse_httpStatus,

    -- ** DeleteCrossAccountAuthorization
    deleteCrossAccountAuthorization_crossAccountAuthorization,
    deleteCrossAccountAuthorizationResponse_httpStatus,

    -- ** DeleteRecoveryGroup
    deleteRecoveryGroup_recoveryGroupName,

    -- ** UpdateRecoveryGroup
    updateRecoveryGroup_recoveryGroupName,
    updateRecoveryGroup_cells,
    updateRecoveryGroupResponse_cells,
    updateRecoveryGroupResponse_recoveryGroupName,
    updateRecoveryGroupResponse_recoveryGroupArn,
    updateRecoveryGroupResponse_tags,
    updateRecoveryGroupResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_tagKeys,
    untagResource_resourceArn,

    -- ** GetRecoveryGroupReadinessSummary
    getRecoveryGroupReadinessSummary_nextToken,
    getRecoveryGroupReadinessSummary_maxResults,
    getRecoveryGroupReadinessSummary_recoveryGroupName,
    getRecoveryGroupReadinessSummaryResponse_readinessChecks,
    getRecoveryGroupReadinessSummaryResponse_readiness,
    getRecoveryGroupReadinessSummaryResponse_nextToken,
    getRecoveryGroupReadinessSummaryResponse_httpStatus,

    -- ** GetResourceSet
    getResourceSet_resourceSetName,
    getResourceSetResponse_resourceSetName,
    getResourceSetResponse_resourceSetType,
    getResourceSetResponse_resources,
    getResourceSetResponse_resourceSetArn,
    getResourceSetResponse_tags,
    getResourceSetResponse_httpStatus,

    -- ** ListTagsForResources
    listTagsForResources_resourceArn,
    listTagsForResourcesResponse_tags,
    listTagsForResourcesResponse_httpStatus,

    -- ** UpdateResourceSet
    updateResourceSet_resourceSetName,
    updateResourceSet_resourceSetType,
    updateResourceSet_resources,
    updateResourceSetResponse_resourceSetName,
    updateResourceSetResponse_resourceSetType,
    updateResourceSetResponse_resources,
    updateResourceSetResponse_resourceSetArn,
    updateResourceSetResponse_tags,
    updateResourceSetResponse_httpStatus,

    -- ** DeleteResourceSet
    deleteResourceSet_resourceSetName,

    -- * Types

    -- ** CellOutput
    cellOutput_tags,
    cellOutput_parentReadinessScopes,
    cellOutput_cellArn,
    cellOutput_cellName,
    cellOutput_cells,

    -- ** DNSTargetResource
    dNSTargetResource_hostedZoneArn,
    dNSTargetResource_recordType,
    dNSTargetResource_targetResource,
    dNSTargetResource_domainName,
    dNSTargetResource_recordSetId,

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
    resource_readinessScopes,
    resource_resourceArn,
    resource_componentId,
    resource_dnsTargetResource,

    -- ** ResourceResult
    resourceResult_resourceArn,
    resourceResult_componentId,
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
    targetResource_r53Resource,
    targetResource_nLBResource,
  )
where

import Network.AWS.Route53RecoveryReadiness.CreateCell
import Network.AWS.Route53RecoveryReadiness.CreateCrossAccountAuthorization
import Network.AWS.Route53RecoveryReadiness.CreateReadinessCheck
import Network.AWS.Route53RecoveryReadiness.CreateRecoveryGroup
import Network.AWS.Route53RecoveryReadiness.CreateResourceSet
import Network.AWS.Route53RecoveryReadiness.DeleteCell
import Network.AWS.Route53RecoveryReadiness.DeleteCrossAccountAuthorization
import Network.AWS.Route53RecoveryReadiness.DeleteReadinessCheck
import Network.AWS.Route53RecoveryReadiness.DeleteRecoveryGroup
import Network.AWS.Route53RecoveryReadiness.DeleteResourceSet
import Network.AWS.Route53RecoveryReadiness.GetArchitectureRecommendations
import Network.AWS.Route53RecoveryReadiness.GetCell
import Network.AWS.Route53RecoveryReadiness.GetCellReadinessSummary
import Network.AWS.Route53RecoveryReadiness.GetReadinessCheck
import Network.AWS.Route53RecoveryReadiness.GetReadinessCheckResourceStatus
import Network.AWS.Route53RecoveryReadiness.GetReadinessCheckStatus
import Network.AWS.Route53RecoveryReadiness.GetRecoveryGroup
import Network.AWS.Route53RecoveryReadiness.GetRecoveryGroupReadinessSummary
import Network.AWS.Route53RecoveryReadiness.GetResourceSet
import Network.AWS.Route53RecoveryReadiness.ListCells
import Network.AWS.Route53RecoveryReadiness.ListCrossAccountAuthorizations
import Network.AWS.Route53RecoveryReadiness.ListReadinessChecks
import Network.AWS.Route53RecoveryReadiness.ListRecoveryGroups
import Network.AWS.Route53RecoveryReadiness.ListResourceSets
import Network.AWS.Route53RecoveryReadiness.ListRules
import Network.AWS.Route53RecoveryReadiness.ListTagsForResources
import Network.AWS.Route53RecoveryReadiness.TagResource
import Network.AWS.Route53RecoveryReadiness.Types.CellOutput
import Network.AWS.Route53RecoveryReadiness.Types.DNSTargetResource
import Network.AWS.Route53RecoveryReadiness.Types.ListRulesOutput
import Network.AWS.Route53RecoveryReadiness.Types.Message
import Network.AWS.Route53RecoveryReadiness.Types.NLBResource
import Network.AWS.Route53RecoveryReadiness.Types.R53ResourceRecord
import Network.AWS.Route53RecoveryReadiness.Types.ReadinessCheckOutput
import Network.AWS.Route53RecoveryReadiness.Types.ReadinessCheckSummary
import Network.AWS.Route53RecoveryReadiness.Types.Recommendation
import Network.AWS.Route53RecoveryReadiness.Types.RecoveryGroupOutput
import Network.AWS.Route53RecoveryReadiness.Types.Resource
import Network.AWS.Route53RecoveryReadiness.Types.ResourceResult
import Network.AWS.Route53RecoveryReadiness.Types.ResourceSetOutput
import Network.AWS.Route53RecoveryReadiness.Types.RuleResult
import Network.AWS.Route53RecoveryReadiness.Types.TargetResource
import Network.AWS.Route53RecoveryReadiness.UntagResource
import Network.AWS.Route53RecoveryReadiness.UpdateCell
import Network.AWS.Route53RecoveryReadiness.UpdateReadinessCheck
import Network.AWS.Route53RecoveryReadiness.UpdateRecoveryGroup
import Network.AWS.Route53RecoveryReadiness.UpdateResourceSet
