{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Lens
  ( -- * Operations

    -- ** CreateSubscription
    createSubscriptionResponse_httpStatus,

    -- ** DescribeEmergencyContactSettings
    describeEmergencyContactSettingsResponse_emergencyContactList,
    describeEmergencyContactSettingsResponse_httpStatus,

    -- ** DescribeProtectionGroup
    describeProtectionGroup_protectionGroupId,
    describeProtectionGroupResponse_httpStatus,
    describeProtectionGroupResponse_protectionGroup,

    -- ** DescribeSubscription
    describeSubscriptionResponse_subscription,
    describeSubscriptionResponse_httpStatus,

    -- ** AssociateHealthCheck
    associateHealthCheck_protectionId,
    associateHealthCheck_healthCheckArn,
    associateHealthCheckResponse_httpStatus,

    -- ** DescribeDRTAccess
    describeDRTAccessResponse_roleArn,
    describeDRTAccessResponse_logBucketList,
    describeDRTAccessResponse_httpStatus,

    -- ** ListResourcesInProtectionGroup
    listResourcesInProtectionGroup_nextToken,
    listResourcesInProtectionGroup_maxResults,
    listResourcesInProtectionGroup_protectionGroupId,
    listResourcesInProtectionGroupResponse_nextToken,
    listResourcesInProtectionGroupResponse_httpStatus,
    listResourcesInProtectionGroupResponse_resourceArns,

    -- ** DisableProactiveEngagement
    disableProactiveEngagementResponse_httpStatus,

    -- ** EnableProactiveEngagement
    enableProactiveEngagementResponse_httpStatus,

    -- ** DisassociateDRTLogBucket
    disassociateDRTLogBucket_logBucket,
    disassociateDRTLogBucketResponse_httpStatus,

    -- ** UpdateSubscription
    updateSubscription_autoRenew,
    updateSubscriptionResponse_httpStatus,

    -- ** AssociateDRTRole
    associateDRTRole_roleArn,
    associateDRTRoleResponse_httpStatus,

    -- ** CreateProtection
    createProtection_name,
    createProtection_resourceArn,
    createProtectionResponse_protectionId,
    createProtectionResponse_httpStatus,

    -- ** AssociateDRTLogBucket
    associateDRTLogBucket_logBucket,
    associateDRTLogBucketResponse_httpStatus,

    -- ** DisassociateDRTRole
    disassociateDRTRoleResponse_httpStatus,

    -- ** ListAttacks
    listAttacks_nextToken,
    listAttacks_maxResults,
    listAttacks_startTime,
    listAttacks_endTime,
    listAttacks_resourceArns,
    listAttacksResponse_nextToken,
    listAttacksResponse_attackSummaries,
    listAttacksResponse_httpStatus,

    -- ** DescribeProtection
    describeProtection_resourceArn,
    describeProtection_protectionId,
    describeProtectionResponse_protection,
    describeProtectionResponse_httpStatus,

    -- ** DescribeAttackStatistics
    describeAttackStatisticsResponse_httpStatus,
    describeAttackStatisticsResponse_timeRange,
    describeAttackStatisticsResponse_dataItems,

    -- ** DisassociateHealthCheck
    disassociateHealthCheck_protectionId,
    disassociateHealthCheck_healthCheckArn,
    disassociateHealthCheckResponse_httpStatus,

    -- ** CreateProtectionGroup
    createProtectionGroup_resourceType,
    createProtectionGroup_members,
    createProtectionGroup_protectionGroupId,
    createProtectionGroup_aggregation,
    createProtectionGroup_pattern,
    createProtectionGroupResponse_httpStatus,

    -- ** ListProtectionGroups
    listProtectionGroups_nextToken,
    listProtectionGroups_maxResults,
    listProtectionGroupsResponse_nextToken,
    listProtectionGroupsResponse_httpStatus,
    listProtectionGroupsResponse_protectionGroups,

    -- ** DescribeAttack
    describeAttack_attackId,
    describeAttackResponse_attack,
    describeAttackResponse_httpStatus,

    -- ** UpdateEmergencyContactSettings
    updateEmergencyContactSettings_emergencyContactList,
    updateEmergencyContactSettingsResponse_httpStatus,

    -- ** DeleteProtectionGroup
    deleteProtectionGroup_protectionGroupId,
    deleteProtectionGroupResponse_httpStatus,

    -- ** UpdateProtectionGroup
    updateProtectionGroup_resourceType,
    updateProtectionGroup_members,
    updateProtectionGroup_protectionGroupId,
    updateProtectionGroup_aggregation,
    updateProtectionGroup_pattern,
    updateProtectionGroupResponse_httpStatus,

    -- ** ListProtections
    listProtections_nextToken,
    listProtections_maxResults,
    listProtectionsResponse_nextToken,
    listProtectionsResponse_protections,
    listProtectionsResponse_httpStatus,

    -- ** DeleteProtection
    deleteProtection_protectionId,
    deleteProtectionResponse_httpStatus,

    -- ** AssociateProactiveEngagementDetails
    associateProactiveEngagementDetails_emergencyContactList,
    associateProactiveEngagementDetailsResponse_httpStatus,

    -- ** GetSubscriptionState
    getSubscriptionStateResponse_httpStatus,
    getSubscriptionStateResponse_subscriptionState,

    -- * Types

    -- ** AttackDetail
    attackDetail_resourceArn,
    attackDetail_attackProperties,
    attackDetail_startTime,
    attackDetail_endTime,
    attackDetail_attackCounters,
    attackDetail_mitigations,
    attackDetail_attackId,
    attackDetail_subResources,

    -- ** AttackProperty
    attackProperty_unit,
    attackProperty_total,
    attackProperty_attackPropertyIdentifier,
    attackProperty_attackLayer,
    attackProperty_topContributors,

    -- ** AttackStatisticsDataItem
    attackStatisticsDataItem_attackVolume,
    attackStatisticsDataItem_attackCount,

    -- ** AttackSummary
    attackSummary_resourceArn,
    attackSummary_startTime,
    attackSummary_endTime,
    attackSummary_attackId,
    attackSummary_attackVectors,

    -- ** AttackVectorDescription
    attackVectorDescription_vectorType,

    -- ** AttackVolume
    attackVolume_packetsPerSecond,
    attackVolume_bitsPerSecond,
    attackVolume_requestsPerSecond,

    -- ** AttackVolumeStatistics
    attackVolumeStatistics_max,

    -- ** Contributor
    contributor_name,
    contributor_value,

    -- ** EmergencyContact
    emergencyContact_phoneNumber,
    emergencyContact_contactNotes,
    emergencyContact_emailAddress,

    -- ** Limit
    limit_max,
    limit_type,

    -- ** Mitigation
    mitigation_mitigationName,

    -- ** Protection
    protection_resourceArn,
    protection_id,
    protection_name,
    protection_healthCheckIds,

    -- ** ProtectionGroup
    protectionGroup_resourceType,
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

    -- ** SubResourceSummary
    subResourceSummary_counters,
    subResourceSummary_id,
    subResourceSummary_type,
    subResourceSummary_attackVectors,

    -- ** Subscription
    subscription_autoRenew,
    subscription_proactiveEngagementStatus,
    subscription_startTime,
    subscription_endTime,
    subscription_limits,
    subscription_timeCommitmentInSeconds,
    subscription_subscriptionLimits,

    -- ** SubscriptionLimits
    subscriptionLimits_protectionLimits,
    subscriptionLimits_protectionGroupLimits,

    -- ** SummarizedAttackVector
    summarizedAttackVector_vectorCounters,
    summarizedAttackVector_vectorType,

    -- ** SummarizedCounter
    summarizedCounter_unit,
    summarizedCounter_n,
    summarizedCounter_sum,
    summarizedCounter_name,
    summarizedCounter_max,
    summarizedCounter_average,

    -- ** TimeRange
    timeRange_fromInclusive,
    timeRange_toExclusive,
  )
where

import Network.AWS.Shield.AssociateDRTLogBucket
import Network.AWS.Shield.AssociateDRTRole
import Network.AWS.Shield.AssociateHealthCheck
import Network.AWS.Shield.AssociateProactiveEngagementDetails
import Network.AWS.Shield.CreateProtection
import Network.AWS.Shield.CreateProtectionGroup
import Network.AWS.Shield.CreateSubscription
import Network.AWS.Shield.DeleteProtection
import Network.AWS.Shield.DeleteProtectionGroup
import Network.AWS.Shield.DescribeAttack
import Network.AWS.Shield.DescribeAttackStatistics
import Network.AWS.Shield.DescribeDRTAccess
import Network.AWS.Shield.DescribeEmergencyContactSettings
import Network.AWS.Shield.DescribeProtection
import Network.AWS.Shield.DescribeProtectionGroup
import Network.AWS.Shield.DescribeSubscription
import Network.AWS.Shield.DisableProactiveEngagement
import Network.AWS.Shield.DisassociateDRTLogBucket
import Network.AWS.Shield.DisassociateDRTRole
import Network.AWS.Shield.DisassociateHealthCheck
import Network.AWS.Shield.EnableProactiveEngagement
import Network.AWS.Shield.GetSubscriptionState
import Network.AWS.Shield.ListAttacks
import Network.AWS.Shield.ListProtectionGroups
import Network.AWS.Shield.ListProtections
import Network.AWS.Shield.ListResourcesInProtectionGroup
import Network.AWS.Shield.Types.AttackDetail
import Network.AWS.Shield.Types.AttackProperty
import Network.AWS.Shield.Types.AttackStatisticsDataItem
import Network.AWS.Shield.Types.AttackSummary
import Network.AWS.Shield.Types.AttackVectorDescription
import Network.AWS.Shield.Types.AttackVolume
import Network.AWS.Shield.Types.AttackVolumeStatistics
import Network.AWS.Shield.Types.Contributor
import Network.AWS.Shield.Types.EmergencyContact
import Network.AWS.Shield.Types.Limit
import Network.AWS.Shield.Types.Mitigation
import Network.AWS.Shield.Types.Protection
import Network.AWS.Shield.Types.ProtectionGroup
import Network.AWS.Shield.Types.ProtectionGroupArbitraryPatternLimits
import Network.AWS.Shield.Types.ProtectionGroupLimits
import Network.AWS.Shield.Types.ProtectionGroupPatternTypeLimits
import Network.AWS.Shield.Types.ProtectionLimits
import Network.AWS.Shield.Types.SubResourceSummary
import Network.AWS.Shield.Types.Subscription
import Network.AWS.Shield.Types.SubscriptionLimits
import Network.AWS.Shield.Types.SummarizedAttackVector
import Network.AWS.Shield.Types.SummarizedCounter
import Network.AWS.Shield.Types.TimeRange
import Network.AWS.Shield.UpdateEmergencyContactSettings
import Network.AWS.Shield.UpdateProtectionGroup
import Network.AWS.Shield.UpdateSubscription
