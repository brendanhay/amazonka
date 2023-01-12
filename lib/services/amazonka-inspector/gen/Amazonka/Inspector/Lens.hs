{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Inspector.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector.Lens
  ( -- * Operations

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

    -- ** CreateAssessmentTemplate
    createAssessmentTemplate_userAttributesForFindings,
    createAssessmentTemplate_assessmentTargetArn,
    createAssessmentTemplate_assessmentTemplateName,
    createAssessmentTemplate_durationInSeconds,
    createAssessmentTemplate_rulesPackageArns,
    createAssessmentTemplateResponse_httpStatus,
    createAssessmentTemplateResponse_assessmentTemplateArn,

    -- ** CreateExclusionsPreview
    createExclusionsPreview_assessmentTemplateArn,
    createExclusionsPreviewResponse_httpStatus,
    createExclusionsPreviewResponse_previewToken,

    -- ** CreateResourceGroup
    createResourceGroup_resourceGroupTags,
    createResourceGroupResponse_httpStatus,
    createResourceGroupResponse_resourceGroupArn,

    -- ** DeleteAssessmentRun
    deleteAssessmentRun_assessmentRunArn,

    -- ** DeleteAssessmentTarget
    deleteAssessmentTarget_assessmentTargetArn,

    -- ** DeleteAssessmentTemplate
    deleteAssessmentTemplate_assessmentTemplateArn,

    -- ** DescribeAssessmentRuns
    describeAssessmentRuns_assessmentRunArns,
    describeAssessmentRunsResponse_httpStatus,
    describeAssessmentRunsResponse_assessmentRuns,
    describeAssessmentRunsResponse_failedItems,

    -- ** DescribeAssessmentTargets
    describeAssessmentTargets_assessmentTargetArns,
    describeAssessmentTargetsResponse_httpStatus,
    describeAssessmentTargetsResponse_assessmentTargets,
    describeAssessmentTargetsResponse_failedItems,

    -- ** DescribeAssessmentTemplates
    describeAssessmentTemplates_assessmentTemplateArns,
    describeAssessmentTemplatesResponse_httpStatus,
    describeAssessmentTemplatesResponse_assessmentTemplates,
    describeAssessmentTemplatesResponse_failedItems,

    -- ** DescribeCrossAccountAccessRole
    describeCrossAccountAccessRoleResponse_httpStatus,
    describeCrossAccountAccessRoleResponse_roleArn,
    describeCrossAccountAccessRoleResponse_valid,
    describeCrossAccountAccessRoleResponse_registeredAt,

    -- ** DescribeExclusions
    describeExclusions_locale,
    describeExclusions_exclusionArns,
    describeExclusionsResponse_httpStatus,
    describeExclusionsResponse_exclusions,
    describeExclusionsResponse_failedItems,

    -- ** DescribeFindings
    describeFindings_locale,
    describeFindings_findingArns,
    describeFindingsResponse_httpStatus,
    describeFindingsResponse_findings,
    describeFindingsResponse_failedItems,

    -- ** DescribeResourceGroups
    describeResourceGroups_resourceGroupArns,
    describeResourceGroupsResponse_httpStatus,
    describeResourceGroupsResponse_resourceGroups,
    describeResourceGroupsResponse_failedItems,

    -- ** DescribeRulesPackages
    describeRulesPackages_locale,
    describeRulesPackages_rulesPackageArns,
    describeRulesPackagesResponse_httpStatus,
    describeRulesPackagesResponse_rulesPackages,
    describeRulesPackagesResponse_failedItems,

    -- ** GetAssessmentReport
    getAssessmentReport_assessmentRunArn,
    getAssessmentReport_reportFileFormat,
    getAssessmentReport_reportType,
    getAssessmentReportResponse_url,
    getAssessmentReportResponse_httpStatus,
    getAssessmentReportResponse_status,

    -- ** GetExclusionsPreview
    getExclusionsPreview_locale,
    getExclusionsPreview_maxResults,
    getExclusionsPreview_nextToken,
    getExclusionsPreview_assessmentTemplateArn,
    getExclusionsPreview_previewToken,
    getExclusionsPreviewResponse_exclusionPreviews,
    getExclusionsPreviewResponse_nextToken,
    getExclusionsPreviewResponse_httpStatus,
    getExclusionsPreviewResponse_previewStatus,

    -- ** GetTelemetryMetadata
    getTelemetryMetadata_assessmentRunArn,
    getTelemetryMetadataResponse_httpStatus,
    getTelemetryMetadataResponse_telemetryMetadata,

    -- ** ListAssessmentRunAgents
    listAssessmentRunAgents_filter,
    listAssessmentRunAgents_maxResults,
    listAssessmentRunAgents_nextToken,
    listAssessmentRunAgents_assessmentRunArn,
    listAssessmentRunAgentsResponse_nextToken,
    listAssessmentRunAgentsResponse_httpStatus,
    listAssessmentRunAgentsResponse_assessmentRunAgents,

    -- ** ListAssessmentRuns
    listAssessmentRuns_assessmentTemplateArns,
    listAssessmentRuns_filter,
    listAssessmentRuns_maxResults,
    listAssessmentRuns_nextToken,
    listAssessmentRunsResponse_nextToken,
    listAssessmentRunsResponse_httpStatus,
    listAssessmentRunsResponse_assessmentRunArns,

    -- ** ListAssessmentTargets
    listAssessmentTargets_filter,
    listAssessmentTargets_maxResults,
    listAssessmentTargets_nextToken,
    listAssessmentTargetsResponse_nextToken,
    listAssessmentTargetsResponse_httpStatus,
    listAssessmentTargetsResponse_assessmentTargetArns,

    -- ** ListAssessmentTemplates
    listAssessmentTemplates_assessmentTargetArns,
    listAssessmentTemplates_filter,
    listAssessmentTemplates_maxResults,
    listAssessmentTemplates_nextToken,
    listAssessmentTemplatesResponse_nextToken,
    listAssessmentTemplatesResponse_httpStatus,
    listAssessmentTemplatesResponse_assessmentTemplateArns,

    -- ** ListEventSubscriptions
    listEventSubscriptions_maxResults,
    listEventSubscriptions_nextToken,
    listEventSubscriptions_resourceArn,
    listEventSubscriptionsResponse_nextToken,
    listEventSubscriptionsResponse_httpStatus,
    listEventSubscriptionsResponse_subscriptions,

    -- ** ListExclusions
    listExclusions_maxResults,
    listExclusions_nextToken,
    listExclusions_assessmentRunArn,
    listExclusionsResponse_nextToken,
    listExclusionsResponse_httpStatus,
    listExclusionsResponse_exclusionArns,

    -- ** ListFindings
    listFindings_assessmentRunArns,
    listFindings_filter,
    listFindings_maxResults,
    listFindings_nextToken,
    listFindingsResponse_nextToken,
    listFindingsResponse_httpStatus,
    listFindingsResponse_findingArns,

    -- ** ListRulesPackages
    listRulesPackages_maxResults,
    listRulesPackages_nextToken,
    listRulesPackagesResponse_nextToken,
    listRulesPackagesResponse_httpStatus,
    listRulesPackagesResponse_rulesPackageArns,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_httpStatus,
    listTagsForResourceResponse_tags,

    -- ** PreviewAgents
    previewAgents_maxResults,
    previewAgents_nextToken,
    previewAgents_previewAgentsArn,
    previewAgentsResponse_nextToken,
    previewAgentsResponse_httpStatus,
    previewAgentsResponse_agentPreviews,

    -- ** RegisterCrossAccountAccessRole
    registerCrossAccountAccessRole_roleArn,

    -- ** RemoveAttributesFromFindings
    removeAttributesFromFindings_findingArns,
    removeAttributesFromFindings_attributeKeys,
    removeAttributesFromFindingsResponse_httpStatus,
    removeAttributesFromFindingsResponse_failedItems,

    -- ** SetTagsForResource
    setTagsForResource_tags,
    setTagsForResource_resourceArn,

    -- ** StartAssessmentRun
    startAssessmentRun_assessmentRunName,
    startAssessmentRun_assessmentTemplateArn,
    startAssessmentRunResponse_httpStatus,
    startAssessmentRunResponse_assessmentRunArn,

    -- ** StopAssessmentRun
    stopAssessmentRun_stopAction,
    stopAssessmentRun_assessmentRunArn,

    -- ** SubscribeToEvent
    subscribeToEvent_resourceArn,
    subscribeToEvent_event,
    subscribeToEvent_topicArn,

    -- ** UnsubscribeFromEvent
    unsubscribeFromEvent_resourceArn,
    unsubscribeFromEvent_event,
    unsubscribeFromEvent_topicArn,

    -- ** UpdateAssessmentTarget
    updateAssessmentTarget_resourceGroupArn,
    updateAssessmentTarget_assessmentTargetArn,
    updateAssessmentTarget_assessmentTargetName,

    -- * Types

    -- ** AgentFilter
    agentFilter_agentHealths,
    agentFilter_agentHealthCodes,

    -- ** AgentPreview
    agentPreview_agentHealth,
    agentPreview_agentVersion,
    agentPreview_autoScalingGroup,
    agentPreview_hostname,
    agentPreview_ipv4Address,
    agentPreview_kernelVersion,
    agentPreview_operatingSystem,
    agentPreview_agentId,

    -- ** AssessmentRun
    assessmentRun_completedAt,
    assessmentRun_startedAt,
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
    assessmentRunFilter_completionTimeRange,
    assessmentRunFilter_durationRange,
    assessmentRunFilter_namePattern,
    assessmentRunFilter_rulesPackageArns,
    assessmentRunFilter_startTimeRange,
    assessmentRunFilter_stateChangeTimeRange,
    assessmentRunFilter_states,

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
    assessmentTemplateFilter_durationRange,
    assessmentTemplateFilter_namePattern,
    assessmentTemplateFilter_rulesPackageArns,

    -- ** AssetAttributes
    assetAttributes_agentId,
    assetAttributes_amiId,
    assetAttributes_autoScalingGroup,
    assetAttributes_hostname,
    assetAttributes_ipv4Addresses,
    assetAttributes_networkInterfaces,
    assetAttributes_tags,
    assetAttributes_schemaVersion,

    -- ** Attribute
    attribute_value,
    attribute_key,

    -- ** DurationRange
    durationRange_maxSeconds,
    durationRange_minSeconds,

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
    finding_assetType,
    finding_confidence,
    finding_description,
    finding_id,
    finding_indicatorOfCompromise,
    finding_numericSeverity,
    finding_recommendation,
    finding_schemaVersion,
    finding_service,
    finding_serviceAttributes,
    finding_severity,
    finding_title,
    finding_arn,
    finding_attributes,
    finding_userAttributes,
    finding_createdAt,
    finding_updatedAt,

    -- ** FindingFilter
    findingFilter_agentIds,
    findingFilter_attributes,
    findingFilter_autoScalingGroups,
    findingFilter_creationTimeRange,
    findingFilter_ruleNames,
    findingFilter_rulesPackageArns,
    findingFilter_severities,
    findingFilter_userAttributes,

    -- ** InspectorServiceAttributes
    inspectorServiceAttributes_assessmentRunArn,
    inspectorServiceAttributes_rulesPackageArn,
    inspectorServiceAttributes_schemaVersion,

    -- ** NetworkInterface
    networkInterface_ipv6Addresses,
    networkInterface_networkInterfaceId,
    networkInterface_privateDnsName,
    networkInterface_privateIpAddress,
    networkInterface_privateIpAddresses,
    networkInterface_publicDnsName,
    networkInterface_publicIp,
    networkInterface_securityGroups,
    networkInterface_subnetId,
    networkInterface_vpcId,

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
    securityGroup_groupId,
    securityGroup_groupName,

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

import Amazonka.Inspector.AddAttributesToFindings
import Amazonka.Inspector.CreateAssessmentTarget
import Amazonka.Inspector.CreateAssessmentTemplate
import Amazonka.Inspector.CreateExclusionsPreview
import Amazonka.Inspector.CreateResourceGroup
import Amazonka.Inspector.DeleteAssessmentRun
import Amazonka.Inspector.DeleteAssessmentTarget
import Amazonka.Inspector.DeleteAssessmentTemplate
import Amazonka.Inspector.DescribeAssessmentRuns
import Amazonka.Inspector.DescribeAssessmentTargets
import Amazonka.Inspector.DescribeAssessmentTemplates
import Amazonka.Inspector.DescribeCrossAccountAccessRole
import Amazonka.Inspector.DescribeExclusions
import Amazonka.Inspector.DescribeFindings
import Amazonka.Inspector.DescribeResourceGroups
import Amazonka.Inspector.DescribeRulesPackages
import Amazonka.Inspector.GetAssessmentReport
import Amazonka.Inspector.GetExclusionsPreview
import Amazonka.Inspector.GetTelemetryMetadata
import Amazonka.Inspector.ListAssessmentRunAgents
import Amazonka.Inspector.ListAssessmentRuns
import Amazonka.Inspector.ListAssessmentTargets
import Amazonka.Inspector.ListAssessmentTemplates
import Amazonka.Inspector.ListEventSubscriptions
import Amazonka.Inspector.ListExclusions
import Amazonka.Inspector.ListFindings
import Amazonka.Inspector.ListRulesPackages
import Amazonka.Inspector.ListTagsForResource
import Amazonka.Inspector.PreviewAgents
import Amazonka.Inspector.RegisterCrossAccountAccessRole
import Amazonka.Inspector.RemoveAttributesFromFindings
import Amazonka.Inspector.SetTagsForResource
import Amazonka.Inspector.StartAssessmentRun
import Amazonka.Inspector.StopAssessmentRun
import Amazonka.Inspector.SubscribeToEvent
import Amazonka.Inspector.Types.AgentFilter
import Amazonka.Inspector.Types.AgentPreview
import Amazonka.Inspector.Types.AssessmentRun
import Amazonka.Inspector.Types.AssessmentRunAgent
import Amazonka.Inspector.Types.AssessmentRunFilter
import Amazonka.Inspector.Types.AssessmentRunNotification
import Amazonka.Inspector.Types.AssessmentRunStateChange
import Amazonka.Inspector.Types.AssessmentTarget
import Amazonka.Inspector.Types.AssessmentTargetFilter
import Amazonka.Inspector.Types.AssessmentTemplate
import Amazonka.Inspector.Types.AssessmentTemplateFilter
import Amazonka.Inspector.Types.AssetAttributes
import Amazonka.Inspector.Types.Attribute
import Amazonka.Inspector.Types.DurationRange
import Amazonka.Inspector.Types.EventSubscription
import Amazonka.Inspector.Types.Exclusion
import Amazonka.Inspector.Types.ExclusionPreview
import Amazonka.Inspector.Types.FailedItemDetails
import Amazonka.Inspector.Types.Finding
import Amazonka.Inspector.Types.FindingFilter
import Amazonka.Inspector.Types.InspectorServiceAttributes
import Amazonka.Inspector.Types.NetworkInterface
import Amazonka.Inspector.Types.PrivateIp
import Amazonka.Inspector.Types.ResourceGroup
import Amazonka.Inspector.Types.ResourceGroupTag
import Amazonka.Inspector.Types.RulesPackage
import Amazonka.Inspector.Types.Scope
import Amazonka.Inspector.Types.SecurityGroup
import Amazonka.Inspector.Types.Subscription
import Amazonka.Inspector.Types.Tag
import Amazonka.Inspector.Types.TelemetryMetadata
import Amazonka.Inspector.Types.TimestampRange
import Amazonka.Inspector.UnsubscribeFromEvent
import Amazonka.Inspector.UpdateAssessmentTarget
