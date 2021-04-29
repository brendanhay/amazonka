{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Lens
  ( -- * Operations

    -- ** StartAssessmentRun
    startAssessmentRun_assessmentRunName,
    startAssessmentRun_assessmentTemplateArn,
    startAssessmentRunResponse_httpStatus,
    startAssessmentRunResponse_assessmentRunArn,

    -- ** DeleteAssessmentTemplate
    deleteAssessmentTemplate_assessmentTemplateArn,

    -- ** SubscribeToEvent
    subscribeToEvent_resourceArn,
    subscribeToEvent_event,
    subscribeToEvent_topicArn,

    -- ** StopAssessmentRun
    stopAssessmentRun_stopAction,
    stopAssessmentRun_assessmentRunArn,

    -- ** GetTelemetryMetadata
    getTelemetryMetadata_assessmentRunArn,
    getTelemetryMetadataResponse_httpStatus,
    getTelemetryMetadataResponse_telemetryMetadata,

    -- ** ListFindings
    listFindings_nextToken,
    listFindings_maxResults,
    listFindings_assessmentRunArns,
    listFindings_filter,
    listFindingsResponse_nextToken,
    listFindingsResponse_httpStatus,
    listFindingsResponse_findingArns,

    -- ** DescribeAssessmentTargets
    describeAssessmentTargets_assessmentTargetArns,
    describeAssessmentTargetsResponse_httpStatus,
    describeAssessmentTargetsResponse_assessmentTargets,
    describeAssessmentTargetsResponse_failedItems,

    -- ** UpdateAssessmentTarget
    updateAssessmentTarget_resourceGroupArn,
    updateAssessmentTarget_assessmentTargetArn,
    updateAssessmentTarget_assessmentTargetName,

    -- ** ListAssessmentTargets
    listAssessmentTargets_nextToken,
    listAssessmentTargets_maxResults,
    listAssessmentTargets_filter,
    listAssessmentTargetsResponse_nextToken,
    listAssessmentTargetsResponse_httpStatus,
    listAssessmentTargetsResponse_assessmentTargetArns,

    -- ** ListAssessmentRuns
    listAssessmentRuns_nextToken,
    listAssessmentRuns_maxResults,
    listAssessmentRuns_filter,
    listAssessmentRuns_assessmentTemplateArns,
    listAssessmentRunsResponse_nextToken,
    listAssessmentRunsResponse_httpStatus,
    listAssessmentRunsResponse_assessmentRunArns,

    -- ** DeleteAssessmentTarget
    deleteAssessmentTarget_assessmentTargetArn,

    -- ** AddAttributesToFindings
    addAttributesToFindings_findingArns,
    addAttributesToFindings_attributes,
    addAttributesToFindingsResponse_httpStatus,
    addAttributesToFindingsResponse_failedItems,

    -- ** CreateAssessmentTarget
    createAssessmentTarget_resourceGroupArn,
    createAssessmentTarget_assessmentTargetName,
    createAssessmentTargetResponse_httpStatus,
    createAssessmentTargetResponse_assessmentTargetArn,

    -- ** GetExclusionsPreview
    getExclusionsPreview_nextToken,
    getExclusionsPreview_maxResults,
    getExclusionsPreview_locale,
    getExclusionsPreview_assessmentTemplateArn,
    getExclusionsPreview_previewToken,
    getExclusionsPreviewResponse_nextToken,
    getExclusionsPreviewResponse_exclusionPreviews,
    getExclusionsPreviewResponse_httpStatus,
    getExclusionsPreviewResponse_previewStatus,

    -- ** DescribeResourceGroups
    describeResourceGroups_resourceGroupArns,
    describeResourceGroupsResponse_httpStatus,
    describeResourceGroupsResponse_resourceGroups,
    describeResourceGroupsResponse_failedItems,

    -- ** PreviewAgents
    previewAgents_nextToken,
    previewAgents_maxResults,
    previewAgents_previewAgentsArn,
    previewAgentsResponse_nextToken,
    previewAgentsResponse_httpStatus,
    previewAgentsResponse_agentPreviews,

    -- ** ListExclusions
    listExclusions_nextToken,
    listExclusions_maxResults,
    listExclusions_assessmentRunArn,
    listExclusionsResponse_nextToken,
    listExclusionsResponse_httpStatus,
    listExclusionsResponse_exclusionArns,

    -- ** CreateAssessmentTemplate
    createAssessmentTemplate_userAttributesForFindings,
    createAssessmentTemplate_assessmentTargetArn,
    createAssessmentTemplate_assessmentTemplateName,
    createAssessmentTemplate_durationInSeconds,
    createAssessmentTemplate_rulesPackageArns,
    createAssessmentTemplateResponse_httpStatus,
    createAssessmentTemplateResponse_assessmentTemplateArn,

    -- ** DescribeCrossAccountAccessRole
    describeCrossAccountAccessRoleResponse_httpStatus,
    describeCrossAccountAccessRoleResponse_roleArn,
    describeCrossAccountAccessRoleResponse_valid,
    describeCrossAccountAccessRoleResponse_registeredAt,

    -- ** SetTagsForResource
    setTagsForResource_tags,
    setTagsForResource_resourceArn,

    -- ** DescribeExclusions
    describeExclusions_locale,
    describeExclusions_exclusionArns,
    describeExclusionsResponse_httpStatus,
    describeExclusionsResponse_exclusions,
    describeExclusionsResponse_failedItems,

    -- ** ListAssessmentTemplates
    listAssessmentTemplates_nextToken,
    listAssessmentTemplates_maxResults,
    listAssessmentTemplates_assessmentTargetArns,
    listAssessmentTemplates_filter,
    listAssessmentTemplatesResponse_nextToken,
    listAssessmentTemplatesResponse_httpStatus,
    listAssessmentTemplatesResponse_assessmentTemplateArns,

    -- ** ListAssessmentRunAgents
    listAssessmentRunAgents_nextToken,
    listAssessmentRunAgents_maxResults,
    listAssessmentRunAgents_filter,
    listAssessmentRunAgents_assessmentRunArn,
    listAssessmentRunAgentsResponse_nextToken,
    listAssessmentRunAgentsResponse_httpStatus,
    listAssessmentRunAgentsResponse_assessmentRunAgents,

    -- ** DescribeAssessmentRuns
    describeAssessmentRuns_assessmentRunArns,
    describeAssessmentRunsResponse_httpStatus,
    describeAssessmentRunsResponse_assessmentRuns,
    describeAssessmentRunsResponse_failedItems,

    -- ** DescribeRulesPackages
    describeRulesPackages_locale,
    describeRulesPackages_rulesPackageArns,
    describeRulesPackagesResponse_httpStatus,
    describeRulesPackagesResponse_rulesPackages,
    describeRulesPackagesResponse_failedItems,

    -- ** CreateExclusionsPreview
    createExclusionsPreview_assessmentTemplateArn,
    createExclusionsPreviewResponse_httpStatus,
    createExclusionsPreviewResponse_previewToken,

    -- ** CreateResourceGroup
    createResourceGroup_resourceGroupTags,
    createResourceGroupResponse_httpStatus,
    createResourceGroupResponse_resourceGroupArn,

    -- ** UnsubscribeFromEvent
    unsubscribeFromEvent_resourceArn,
    unsubscribeFromEvent_event,
    unsubscribeFromEvent_topicArn,

    -- ** RemoveAttributesFromFindings
    removeAttributesFromFindings_findingArns,
    removeAttributesFromFindings_attributeKeys,
    removeAttributesFromFindingsResponse_httpStatus,
    removeAttributesFromFindingsResponse_failedItems,

    -- ** DeleteAssessmentRun
    deleteAssessmentRun_assessmentRunArn,

    -- ** RegisterCrossAccountAccessRole
    registerCrossAccountAccessRole_roleArn,

    -- ** ListEventSubscriptions
    listEventSubscriptions_resourceArn,
    listEventSubscriptions_nextToken,
    listEventSubscriptions_maxResults,
    listEventSubscriptionsResponse_nextToken,
    listEventSubscriptionsResponse_httpStatus,
    listEventSubscriptionsResponse_subscriptions,

    -- ** GetAssessmentReport
    getAssessmentReport_assessmentRunArn,
    getAssessmentReport_reportFileFormat,
    getAssessmentReport_reportType,
    getAssessmentReportResponse_url,
    getAssessmentReportResponse_httpStatus,
    getAssessmentReportResponse_status,

    -- ** ListRulesPackages
    listRulesPackages_nextToken,
    listRulesPackages_maxResults,
    listRulesPackagesResponse_nextToken,
    listRulesPackagesResponse_httpStatus,
    listRulesPackagesResponse_rulesPackageArns,

    -- ** DescribeFindings
    describeFindings_locale,
    describeFindings_findingArns,
    describeFindingsResponse_httpStatus,
    describeFindingsResponse_findings,
    describeFindingsResponse_failedItems,

    -- ** DescribeAssessmentTemplates
    describeAssessmentTemplates_assessmentTemplateArns,
    describeAssessmentTemplatesResponse_httpStatus,
    describeAssessmentTemplatesResponse_assessmentTemplates,
    describeAssessmentTemplatesResponse_failedItems,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_httpStatus,
    listTagsForResourceResponse_tags,

    -- * Types

    -- ** AgentFilter
    agentFilter_agentHealths,
    agentFilter_agentHealthCodes,

    -- ** AgentPreview
    agentPreview_hostname,
    agentPreview_agentVersion,
    agentPreview_kernelVersion,
    agentPreview_operatingSystem,
    agentPreview_agentHealth,
    agentPreview_autoScalingGroup,
    agentPreview_ipv4Address,
    agentPreview_agentId,

    -- ** AssessmentRun
    assessmentRun_startedAt,
    assessmentRun_completedAt,
    assessmentRun_arn,
    assessmentRun_name,
    assessmentRun_assessmentTemplateArn,
    assessmentRun_state,
    assessmentRun_durationInSeconds,
    assessmentRun_rulesPackageArns,
    assessmentRun_userAttributesForFindings,
    assessmentRun_createdAt,
    assessmentRun_stateChangedAt,
    assessmentRun_dataCollected,
    assessmentRun_stateChanges,
    assessmentRun_notifications,
    assessmentRun_findingCounts,

    -- ** AssessmentRunAgent
    assessmentRunAgent_agentHealthDetails,
    assessmentRunAgent_autoScalingGroup,
    assessmentRunAgent_agentId,
    assessmentRunAgent_assessmentRunArn,
    assessmentRunAgent_agentHealth,
    assessmentRunAgent_agentHealthCode,
    assessmentRunAgent_telemetryMetadata,

    -- ** AssessmentRunFilter
    assessmentRunFilter_states,
    assessmentRunFilter_rulesPackageArns,
    assessmentRunFilter_durationRange,
    assessmentRunFilter_stateChangeTimeRange,
    assessmentRunFilter_startTimeRange,
    assessmentRunFilter_namePattern,
    assessmentRunFilter_completionTimeRange,

    -- ** AssessmentRunNotification
    assessmentRunNotification_message,
    assessmentRunNotification_snsPublishStatusCode,
    assessmentRunNotification_snsTopicArn,
    assessmentRunNotification_date,
    assessmentRunNotification_event,
    assessmentRunNotification_error,

    -- ** AssessmentRunStateChange
    assessmentRunStateChange_stateChangedAt,
    assessmentRunStateChange_state,

    -- ** AssessmentTarget
    assessmentTarget_resourceGroupArn,
    assessmentTarget_arn,
    assessmentTarget_name,
    assessmentTarget_createdAt,
    assessmentTarget_updatedAt,

    -- ** AssessmentTargetFilter
    assessmentTargetFilter_assessmentTargetNamePattern,

    -- ** AssessmentTemplate
    assessmentTemplate_lastAssessmentRunArn,
    assessmentTemplate_arn,
    assessmentTemplate_name,
    assessmentTemplate_assessmentTargetArn,
    assessmentTemplate_durationInSeconds,
    assessmentTemplate_rulesPackageArns,
    assessmentTemplate_userAttributesForFindings,
    assessmentTemplate_assessmentRunCount,
    assessmentTemplate_createdAt,

    -- ** AssessmentTemplateFilter
    assessmentTemplateFilter_rulesPackageArns,
    assessmentTemplateFilter_durationRange,
    assessmentTemplateFilter_namePattern,

    -- ** AssetAttributes
    assetAttributes_hostname,
    assetAttributes_agentId,
    assetAttributes_amiId,
    assetAttributes_tags,
    assetAttributes_ipv4Addresses,
    assetAttributes_networkInterfaces,
    assetAttributes_autoScalingGroup,
    assetAttributes_schemaVersion,

    -- ** Attribute
    attribute_value,
    attribute_key,

    -- ** DurationRange
    durationRange_minSeconds,
    durationRange_maxSeconds,

    -- ** EventSubscription
    eventSubscription_event,
    eventSubscription_subscribedAt,

    -- ** Exclusion
    exclusion_attributes,
    exclusion_arn,
    exclusion_title,
    exclusion_description,
    exclusion_recommendation,
    exclusion_scopes,

    -- ** ExclusionPreview
    exclusionPreview_attributes,
    exclusionPreview_title,
    exclusionPreview_description,
    exclusionPreview_recommendation,
    exclusionPreview_scopes,

    -- ** FailedItemDetails
    failedItemDetails_failureCode,
    failedItemDetails_retryable,

    -- ** Finding
    finding_assetAttributes,
    finding_severity,
    finding_title,
    finding_assetType,
    finding_numericSeverity,
    finding_id,
    finding_service,
    finding_serviceAttributes,
    finding_confidence,
    finding_recommendation,
    finding_indicatorOfCompromise,
    finding_description,
    finding_schemaVersion,
    finding_arn,
    finding_attributes,
    finding_userAttributes,
    finding_createdAt,
    finding_updatedAt,

    -- ** FindingFilter
    findingFilter_agentIds,
    findingFilter_rulesPackageArns,
    findingFilter_creationTimeRange,
    findingFilter_severities,
    findingFilter_attributes,
    findingFilter_userAttributes,
    findingFilter_autoScalingGroups,
    findingFilter_ruleNames,

    -- ** InspectorServiceAttributes
    inspectorServiceAttributes_rulesPackageArn,
    inspectorServiceAttributes_assessmentRunArn,
    inspectorServiceAttributes_schemaVersion,

    -- ** NetworkInterface
    networkInterface_privateIpAddresses,
    networkInterface_ipv6Addresses,
    networkInterface_securityGroups,
    networkInterface_publicDnsName,
    networkInterface_subnetId,
    networkInterface_networkInterfaceId,
    networkInterface_privateDnsName,
    networkInterface_vpcId,
    networkInterface_publicIp,
    networkInterface_privateIpAddress,

    -- ** PrivateIp
    privateIp_privateDnsName,
    privateIp_privateIpAddress,

    -- ** ResourceGroup
    resourceGroup_arn,
    resourceGroup_tags,
    resourceGroup_createdAt,

    -- ** ResourceGroupTag
    resourceGroupTag_value,
    resourceGroupTag_key,

    -- ** RulesPackage
    rulesPackage_description,
    rulesPackage_arn,
    rulesPackage_name,
    rulesPackage_version,
    rulesPackage_provider,

    -- ** Scope
    scope_key,
    scope_value,

    -- ** SecurityGroup
    securityGroup_groupName,
    securityGroup_groupId,

    -- ** Subscription
    subscription_resourceArn,
    subscription_topicArn,
    subscription_eventSubscriptions,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** TelemetryMetadata
    telemetryMetadata_dataSize,
    telemetryMetadata_messageType,
    telemetryMetadata_count,

    -- ** TimestampRange
    timestampRange_beginDate,
    timestampRange_endDate,
  )
where

import Network.AWS.Inspector.AddAttributesToFindings
import Network.AWS.Inspector.CreateAssessmentTarget
import Network.AWS.Inspector.CreateAssessmentTemplate
import Network.AWS.Inspector.CreateExclusionsPreview
import Network.AWS.Inspector.CreateResourceGroup
import Network.AWS.Inspector.DeleteAssessmentRun
import Network.AWS.Inspector.DeleteAssessmentTarget
import Network.AWS.Inspector.DeleteAssessmentTemplate
import Network.AWS.Inspector.DescribeAssessmentRuns
import Network.AWS.Inspector.DescribeAssessmentTargets
import Network.AWS.Inspector.DescribeAssessmentTemplates
import Network.AWS.Inspector.DescribeCrossAccountAccessRole
import Network.AWS.Inspector.DescribeExclusions
import Network.AWS.Inspector.DescribeFindings
import Network.AWS.Inspector.DescribeResourceGroups
import Network.AWS.Inspector.DescribeRulesPackages
import Network.AWS.Inspector.GetAssessmentReport
import Network.AWS.Inspector.GetExclusionsPreview
import Network.AWS.Inspector.GetTelemetryMetadata
import Network.AWS.Inspector.ListAssessmentRunAgents
import Network.AWS.Inspector.ListAssessmentRuns
import Network.AWS.Inspector.ListAssessmentTargets
import Network.AWS.Inspector.ListAssessmentTemplates
import Network.AWS.Inspector.ListEventSubscriptions
import Network.AWS.Inspector.ListExclusions
import Network.AWS.Inspector.ListFindings
import Network.AWS.Inspector.ListRulesPackages
import Network.AWS.Inspector.ListTagsForResource
import Network.AWS.Inspector.PreviewAgents
import Network.AWS.Inspector.RegisterCrossAccountAccessRole
import Network.AWS.Inspector.RemoveAttributesFromFindings
import Network.AWS.Inspector.SetTagsForResource
import Network.AWS.Inspector.StartAssessmentRun
import Network.AWS.Inspector.StopAssessmentRun
import Network.AWS.Inspector.SubscribeToEvent
import Network.AWS.Inspector.Types.AgentFilter
import Network.AWS.Inspector.Types.AgentPreview
import Network.AWS.Inspector.Types.AssessmentRun
import Network.AWS.Inspector.Types.AssessmentRunAgent
import Network.AWS.Inspector.Types.AssessmentRunFilter
import Network.AWS.Inspector.Types.AssessmentRunNotification
import Network.AWS.Inspector.Types.AssessmentRunStateChange
import Network.AWS.Inspector.Types.AssessmentTarget
import Network.AWS.Inspector.Types.AssessmentTargetFilter
import Network.AWS.Inspector.Types.AssessmentTemplate
import Network.AWS.Inspector.Types.AssessmentTemplateFilter
import Network.AWS.Inspector.Types.AssetAttributes
import Network.AWS.Inspector.Types.Attribute
import Network.AWS.Inspector.Types.DurationRange
import Network.AWS.Inspector.Types.EventSubscription
import Network.AWS.Inspector.Types.Exclusion
import Network.AWS.Inspector.Types.ExclusionPreview
import Network.AWS.Inspector.Types.FailedItemDetails
import Network.AWS.Inspector.Types.Finding
import Network.AWS.Inspector.Types.FindingFilter
import Network.AWS.Inspector.Types.InspectorServiceAttributes
import Network.AWS.Inspector.Types.NetworkInterface
import Network.AWS.Inspector.Types.PrivateIp
import Network.AWS.Inspector.Types.ResourceGroup
import Network.AWS.Inspector.Types.ResourceGroupTag
import Network.AWS.Inspector.Types.RulesPackage
import Network.AWS.Inspector.Types.Scope
import Network.AWS.Inspector.Types.SecurityGroup
import Network.AWS.Inspector.Types.Subscription
import Network.AWS.Inspector.Types.Tag
import Network.AWS.Inspector.Types.TelemetryMetadata
import Network.AWS.Inspector.Types.TimestampRange
import Network.AWS.Inspector.UnsubscribeFromEvent
import Network.AWS.Inspector.UpdateAssessmentTarget
