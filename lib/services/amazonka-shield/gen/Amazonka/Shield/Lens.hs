{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Shield.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Shield.Lens
  ( -- * Operations

    -- ** AssociateDRTLogBucket
    associateDRTLogBucket_logBucket,
    associateDRTLogBucketResponse_httpStatus,

    -- ** AssociateDRTRole
    associateDRTRole_roleArn,
    associateDRTRoleResponse_httpStatus,

    -- ** AssociateHealthCheck
    associateHealthCheck_protectionId,
    associateHealthCheck_healthCheckArn,
    associateHealthCheckResponse_httpStatus,

    -- ** AssociateProactiveEngagementDetails
    associateProactiveEngagementDetails_emergencyContactList,
    associateProactiveEngagementDetailsResponse_httpStatus,

    -- ** CreateProtection
    createProtection_tags,
    createProtection_name,
    createProtection_resourceArn,
    createProtectionResponse_protectionId,
    createProtectionResponse_httpStatus,

    -- ** CreateProtectionGroup
    createProtectionGroup_tags,
    createProtectionGroup_resourceType,
    createProtectionGroup_members,
    createProtectionGroup_protectionGroupId,
    createProtectionGroup_aggregation,
    createProtectionGroup_pattern,
    createProtectionGroupResponse_httpStatus,

    -- ** CreateSubscription
    createSubscriptionResponse_httpStatus,

    -- ** DeleteProtection
    deleteProtection_protectionId,
    deleteProtectionResponse_httpStatus,

    -- ** DeleteProtectionGroup
    deleteProtectionGroup_protectionGroupId,
    deleteProtectionGroupResponse_httpStatus,

    -- ** DescribeAttack
    describeAttack_attackId,
    describeAttackResponse_attack,
    describeAttackResponse_httpStatus,

    -- ** DescribeAttackStatistics
    describeAttackStatisticsResponse_httpStatus,
    describeAttackStatisticsResponse_timeRange,
    describeAttackStatisticsResponse_dataItems,

    -- ** DescribeDRTAccess
    describeDRTAccessResponse_roleArn,
    describeDRTAccessResponse_logBucketList,
    describeDRTAccessResponse_httpStatus,

    -- ** DescribeEmergencyContactSettings
    describeEmergencyContactSettingsResponse_emergencyContactList,
    describeEmergencyContactSettingsResponse_httpStatus,

    -- ** DescribeProtection
    describeProtection_protectionId,
    describeProtection_resourceArn,
    describeProtectionResponse_protection,
    describeProtectionResponse_httpStatus,

    -- ** DescribeProtectionGroup
    describeProtectionGroup_protectionGroupId,
    describeProtectionGroupResponse_httpStatus,
    describeProtectionGroupResponse_protectionGroup,

    -- ** DescribeSubscription
    describeSubscriptionResponse_subscription,
    describeSubscriptionResponse_httpStatus,

    -- ** DisableApplicationLayerAutomaticResponse
    disableApplicationLayerAutomaticResponse_resourceArn,
    disableApplicationLayerAutomaticResponseResponse_httpStatus,

    -- ** DisableProactiveEngagement
    disableProactiveEngagementResponse_httpStatus,

    -- ** DisassociateDRTLogBucket
    disassociateDRTLogBucket_logBucket,
    disassociateDRTLogBucketResponse_httpStatus,

    -- ** DisassociateDRTRole
    disassociateDRTRoleResponse_httpStatus,

    -- ** DisassociateHealthCheck
    disassociateHealthCheck_protectionId,
    disassociateHealthCheck_healthCheckArn,
    disassociateHealthCheckResponse_httpStatus,

    -- ** EnableApplicationLayerAutomaticResponse
    enableApplicationLayerAutomaticResponse_resourceArn,
    enableApplicationLayerAutomaticResponse_action,
    enableApplicationLayerAutomaticResponseResponse_httpStatus,

    -- ** EnableProactiveEngagement
    enableProactiveEngagementResponse_httpStatus,

    -- ** GetSubscriptionState
    getSubscriptionStateResponse_httpStatus,
    getSubscriptionStateResponse_subscriptionState,

    -- ** ListAttacks
    listAttacks_nextToken,
    listAttacks_endTime,
    listAttacks_maxResults,
    listAttacks_startTime,
    listAttacks_resourceArns,
    listAttacksResponse_nextToken,
    listAttacksResponse_attackSummaries,
    listAttacksResponse_httpStatus,

    -- ** ListProtectionGroups
    listProtectionGroups_nextToken,
    listProtectionGroups_maxResults,
    listProtectionGroups_inclusionFilters,
    listProtectionGroupsResponse_nextToken,
    listProtectionGroupsResponse_httpStatus,
    listProtectionGroupsResponse_protectionGroups,

    -- ** ListProtections
    listProtections_nextToken,
    listProtections_maxResults,
    listProtections_inclusionFilters,
    listProtectionsResponse_nextToken,
    listProtectionsResponse_protections,
    listProtectionsResponse_httpStatus,

    -- ** ListResourcesInProtectionGroup
    listResourcesInProtectionGroup_nextToken,
    listResourcesInProtectionGroup_maxResults,
    listResourcesInProtectionGroup_protectionGroupId,
    listResourcesInProtectionGroupResponse_nextToken,
    listResourcesInProtectionGroupResponse_httpStatus,
    listResourcesInProtectionGroupResponse_resourceArns,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateApplicationLayerAutomaticResponse
    updateApplicationLayerAutomaticResponse_resourceArn,
    updateApplicationLayerAutomaticResponse_action,
    updateApplicationLayerAutomaticResponseResponse_httpStatus,

    -- ** UpdateEmergencyContactSettings
    updateEmergencyContactSettings_emergencyContactList,
    updateEmergencyContactSettingsResponse_httpStatus,

    -- ** UpdateProtectionGroup
    updateProtectionGroup_resourceType,
    updateProtectionGroup_members,
    updateProtectionGroup_protectionGroupId,
    updateProtectionGroup_aggregation,
    updateProtectionGroup_pattern,
    updateProtectionGroupResponse_httpStatus,

    -- ** UpdateSubscription
    updateSubscription_autoRenew,
    updateSubscriptionResponse_httpStatus,

    -- * Types

    -- ** ApplicationLayerAutomaticResponseConfiguration
    applicationLayerAutomaticResponseConfiguration_status,
    applicationLayerAutomaticResponseConfiguration_action,

    -- ** AttackDetail
    attackDetail_attackId,
    attackDetail_subResources,
    attackDetail_endTime,
    attackDetail_mitigations,
    attackDetail_resourceArn,
    attackDetail_attackCounters,
    attackDetail_attackProperties,
    attackDetail_startTime,

    -- ** AttackProperty
    attackProperty_total,
    attackProperty_attackLayer,
    attackProperty_topContributors,
    attackProperty_attackPropertyIdentifier,
    attackProperty_unit,

    -- ** AttackStatisticsDataItem
    attackStatisticsDataItem_attackVolume,
    attackStatisticsDataItem_attackCount,

    -- ** AttackSummary
    attackSummary_attackId,
    attackSummary_endTime,
    attackSummary_attackVectors,
    attackSummary_resourceArn,
    attackSummary_startTime,

    -- ** AttackVectorDescription
    attackVectorDescription_vectorType,

    -- ** AttackVolume
    attackVolume_requestsPerSecond,
    attackVolume_bitsPerSecond,
    attackVolume_packetsPerSecond,

    -- ** AttackVolumeStatistics
    attackVolumeStatistics_max,

    -- ** BlockAction

    -- ** Contributor
    contributor_name,
    contributor_value,

    -- ** CountAction

    -- ** EmergencyContact
    emergencyContact_contactNotes,
    emergencyContact_phoneNumber,
    emergencyContact_emailAddress,

    -- ** InclusionProtectionFilters
    inclusionProtectionFilters_resourceTypes,
    inclusionProtectionFilters_protectionNames,
    inclusionProtectionFilters_resourceArns,

    -- ** InclusionProtectionGroupFilters
    inclusionProtectionGroupFilters_aggregations,
    inclusionProtectionGroupFilters_patterns,
    inclusionProtectionGroupFilters_resourceTypes,
    inclusionProtectionGroupFilters_protectionGroupIds,

    -- ** Limit
    limit_type,
    limit_max,

    -- ** Mitigation
    mitigation_mitigationName,

    -- ** Protection
    protection_name,
    protection_applicationLayerAutomaticResponseConfiguration,
    protection_id,
    protection_protectionArn,
    protection_resourceArn,
    protection_healthCheckIds,

    -- ** ProtectionGroup
    protectionGroup_resourceType,
    protectionGroup_protectionGroupArn,
    protectionGroup_protectionGroupId,
    protectionGroup_aggregation,
    protectionGroup_pattern,
    protectionGroup_members,

    -- ** ProtectionGroupArbitraryPatternLimits
    protectionGroupArbitraryPatternLimits_maxMembers,

    -- ** ProtectionGroupLimits
    protectionGroupLimits_maxProtectionGroups,
    protectionGroupLimits_patternTypeLimits,

    -- ** ProtectionGroupPatternTypeLimits
    protectionGroupPatternTypeLimits_arbitraryPatternLimits,

    -- ** ProtectionLimits
    protectionLimits_protectedResourceTypeLimits,

    -- ** ResponseAction
    responseAction_count,
    responseAction_block,

    -- ** SubResourceSummary
    subResourceSummary_type,
    subResourceSummary_counters,
    subResourceSummary_id,
    subResourceSummary_attackVectors,

    -- ** Subscription
    subscription_subscriptionArn,
    subscription_autoRenew,
    subscription_limits,
    subscription_endTime,
    subscription_timeCommitmentInSeconds,
    subscription_startTime,
    subscription_proactiveEngagementStatus,
    subscription_subscriptionLimits,

    -- ** SubscriptionLimits
    subscriptionLimits_protectionLimits,
    subscriptionLimits_protectionGroupLimits,

    -- ** SummarizedAttackVector
    summarizedAttackVector_vectorCounters,
    summarizedAttackVector_vectorType,

    -- ** SummarizedCounter
    summarizedCounter_name,
    summarizedCounter_max,
    summarizedCounter_average,
    summarizedCounter_sum,
    summarizedCounter_n,
    summarizedCounter_unit,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TimeRange
    timeRange_toExclusive,
    timeRange_fromInclusive,
  )
where

import Amazonka.Shield.AssociateDRTLogBucket
import Amazonka.Shield.AssociateDRTRole
import Amazonka.Shield.AssociateHealthCheck
import Amazonka.Shield.AssociateProactiveEngagementDetails
import Amazonka.Shield.CreateProtection
import Amazonka.Shield.CreateProtectionGroup
import Amazonka.Shield.CreateSubscription
import Amazonka.Shield.DeleteProtection
import Amazonka.Shield.DeleteProtectionGroup
import Amazonka.Shield.DescribeAttack
import Amazonka.Shield.DescribeAttackStatistics
import Amazonka.Shield.DescribeDRTAccess
import Amazonka.Shield.DescribeEmergencyContactSettings
import Amazonka.Shield.DescribeProtection
import Amazonka.Shield.DescribeProtectionGroup
import Amazonka.Shield.DescribeSubscription
import Amazonka.Shield.DisableApplicationLayerAutomaticResponse
import Amazonka.Shield.DisableProactiveEngagement
import Amazonka.Shield.DisassociateDRTLogBucket
import Amazonka.Shield.DisassociateDRTRole
import Amazonka.Shield.DisassociateHealthCheck
import Amazonka.Shield.EnableApplicationLayerAutomaticResponse
import Amazonka.Shield.EnableProactiveEngagement
import Amazonka.Shield.GetSubscriptionState
import Amazonka.Shield.ListAttacks
import Amazonka.Shield.ListProtectionGroups
import Amazonka.Shield.ListProtections
import Amazonka.Shield.ListResourcesInProtectionGroup
import Amazonka.Shield.ListTagsForResource
import Amazonka.Shield.TagResource
import Amazonka.Shield.Types.ApplicationLayerAutomaticResponseConfiguration
import Amazonka.Shield.Types.AttackDetail
import Amazonka.Shield.Types.AttackProperty
import Amazonka.Shield.Types.AttackStatisticsDataItem
import Amazonka.Shield.Types.AttackSummary
import Amazonka.Shield.Types.AttackVectorDescription
import Amazonka.Shield.Types.AttackVolume
import Amazonka.Shield.Types.AttackVolumeStatistics
import Amazonka.Shield.Types.BlockAction
import Amazonka.Shield.Types.Contributor
import Amazonka.Shield.Types.CountAction
import Amazonka.Shield.Types.EmergencyContact
import Amazonka.Shield.Types.InclusionProtectionFilters
import Amazonka.Shield.Types.InclusionProtectionGroupFilters
import Amazonka.Shield.Types.Limit
import Amazonka.Shield.Types.Mitigation
import Amazonka.Shield.Types.Protection
import Amazonka.Shield.Types.ProtectionGroup
import Amazonka.Shield.Types.ProtectionGroupArbitraryPatternLimits
import Amazonka.Shield.Types.ProtectionGroupLimits
import Amazonka.Shield.Types.ProtectionGroupPatternTypeLimits
import Amazonka.Shield.Types.ProtectionLimits
import Amazonka.Shield.Types.ResponseAction
import Amazonka.Shield.Types.SubResourceSummary
import Amazonka.Shield.Types.Subscription
import Amazonka.Shield.Types.SubscriptionLimits
import Amazonka.Shield.Types.SummarizedAttackVector
import Amazonka.Shield.Types.SummarizedCounter
import Amazonka.Shield.Types.Tag
import Amazonka.Shield.Types.TimeRange
import Amazonka.Shield.UntagResource
import Amazonka.Shield.UpdateApplicationLayerAutomaticResponse
import Amazonka.Shield.UpdateEmergencyContactSettings
import Amazonka.Shield.UpdateProtectionGroup
import Amazonka.Shield.UpdateSubscription
