{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Shield
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2016-06-02@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Shield Advanced
--
-- This is the /Shield Advanced API Reference/. This guide is for
-- developers who need detailed information about the Shield Advanced API
-- actions, data types, and errors. For detailed information about WAF and
-- Shield Advanced features and an overview of how to use the WAF and
-- Shield Advanced APIs, see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/ WAF and Shield Developer Guide>.
module Amazonka.Shield
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** AccessDeniedForDependencyException
    _AccessDeniedForDependencyException,

    -- ** InternalErrorException
    _InternalErrorException,

    -- ** InvalidOperationException
    _InvalidOperationException,

    -- ** InvalidPaginationTokenException
    _InvalidPaginationTokenException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** InvalidResourceException
    _InvalidResourceException,

    -- ** LimitsExceededException
    _LimitsExceededException,

    -- ** LockedSubscriptionException
    _LockedSubscriptionException,

    -- ** NoAssociatedRoleException
    _NoAssociatedRoleException,

    -- ** OptimisticLockException
    _OptimisticLockException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateDRTLogBucket
    AssociateDRTLogBucket (AssociateDRTLogBucket'),
    newAssociateDRTLogBucket,
    AssociateDRTLogBucketResponse (AssociateDRTLogBucketResponse'),
    newAssociateDRTLogBucketResponse,

    -- ** AssociateDRTRole
    AssociateDRTRole (AssociateDRTRole'),
    newAssociateDRTRole,
    AssociateDRTRoleResponse (AssociateDRTRoleResponse'),
    newAssociateDRTRoleResponse,

    -- ** AssociateHealthCheck
    AssociateHealthCheck (AssociateHealthCheck'),
    newAssociateHealthCheck,
    AssociateHealthCheckResponse (AssociateHealthCheckResponse'),
    newAssociateHealthCheckResponse,

    -- ** AssociateProactiveEngagementDetails
    AssociateProactiveEngagementDetails (AssociateProactiveEngagementDetails'),
    newAssociateProactiveEngagementDetails,
    AssociateProactiveEngagementDetailsResponse (AssociateProactiveEngagementDetailsResponse'),
    newAssociateProactiveEngagementDetailsResponse,

    -- ** CreateProtection
    CreateProtection (CreateProtection'),
    newCreateProtection,
    CreateProtectionResponse (CreateProtectionResponse'),
    newCreateProtectionResponse,

    -- ** CreateProtectionGroup
    CreateProtectionGroup (CreateProtectionGroup'),
    newCreateProtectionGroup,
    CreateProtectionGroupResponse (CreateProtectionGroupResponse'),
    newCreateProtectionGroupResponse,

    -- ** CreateSubscription
    CreateSubscription (CreateSubscription'),
    newCreateSubscription,
    CreateSubscriptionResponse (CreateSubscriptionResponse'),
    newCreateSubscriptionResponse,

    -- ** DeleteProtection
    DeleteProtection (DeleteProtection'),
    newDeleteProtection,
    DeleteProtectionResponse (DeleteProtectionResponse'),
    newDeleteProtectionResponse,

    -- ** DeleteProtectionGroup
    DeleteProtectionGroup (DeleteProtectionGroup'),
    newDeleteProtectionGroup,
    DeleteProtectionGroupResponse (DeleteProtectionGroupResponse'),
    newDeleteProtectionGroupResponse,

    -- ** DescribeAttack
    DescribeAttack (DescribeAttack'),
    newDescribeAttack,
    DescribeAttackResponse (DescribeAttackResponse'),
    newDescribeAttackResponse,

    -- ** DescribeAttackStatistics
    DescribeAttackStatistics (DescribeAttackStatistics'),
    newDescribeAttackStatistics,
    DescribeAttackStatisticsResponse (DescribeAttackStatisticsResponse'),
    newDescribeAttackStatisticsResponse,

    -- ** DescribeDRTAccess
    DescribeDRTAccess (DescribeDRTAccess'),
    newDescribeDRTAccess,
    DescribeDRTAccessResponse (DescribeDRTAccessResponse'),
    newDescribeDRTAccessResponse,

    -- ** DescribeEmergencyContactSettings
    DescribeEmergencyContactSettings (DescribeEmergencyContactSettings'),
    newDescribeEmergencyContactSettings,
    DescribeEmergencyContactSettingsResponse (DescribeEmergencyContactSettingsResponse'),
    newDescribeEmergencyContactSettingsResponse,

    -- ** DescribeProtection
    DescribeProtection (DescribeProtection'),
    newDescribeProtection,
    DescribeProtectionResponse (DescribeProtectionResponse'),
    newDescribeProtectionResponse,

    -- ** DescribeProtectionGroup
    DescribeProtectionGroup (DescribeProtectionGroup'),
    newDescribeProtectionGroup,
    DescribeProtectionGroupResponse (DescribeProtectionGroupResponse'),
    newDescribeProtectionGroupResponse,

    -- ** DescribeSubscription
    DescribeSubscription (DescribeSubscription'),
    newDescribeSubscription,
    DescribeSubscriptionResponse (DescribeSubscriptionResponse'),
    newDescribeSubscriptionResponse,

    -- ** DisableApplicationLayerAutomaticResponse
    DisableApplicationLayerAutomaticResponse (DisableApplicationLayerAutomaticResponse'),
    newDisableApplicationLayerAutomaticResponse,
    DisableApplicationLayerAutomaticResponseResponse (DisableApplicationLayerAutomaticResponseResponse'),
    newDisableApplicationLayerAutomaticResponseResponse,

    -- ** DisableProactiveEngagement
    DisableProactiveEngagement (DisableProactiveEngagement'),
    newDisableProactiveEngagement,
    DisableProactiveEngagementResponse (DisableProactiveEngagementResponse'),
    newDisableProactiveEngagementResponse,

    -- ** DisassociateDRTLogBucket
    DisassociateDRTLogBucket (DisassociateDRTLogBucket'),
    newDisassociateDRTLogBucket,
    DisassociateDRTLogBucketResponse (DisassociateDRTLogBucketResponse'),
    newDisassociateDRTLogBucketResponse,

    -- ** DisassociateDRTRole
    DisassociateDRTRole (DisassociateDRTRole'),
    newDisassociateDRTRole,
    DisassociateDRTRoleResponse (DisassociateDRTRoleResponse'),
    newDisassociateDRTRoleResponse,

    -- ** DisassociateHealthCheck
    DisassociateHealthCheck (DisassociateHealthCheck'),
    newDisassociateHealthCheck,
    DisassociateHealthCheckResponse (DisassociateHealthCheckResponse'),
    newDisassociateHealthCheckResponse,

    -- ** EnableApplicationLayerAutomaticResponse
    EnableApplicationLayerAutomaticResponse (EnableApplicationLayerAutomaticResponse'),
    newEnableApplicationLayerAutomaticResponse,
    EnableApplicationLayerAutomaticResponseResponse (EnableApplicationLayerAutomaticResponseResponse'),
    newEnableApplicationLayerAutomaticResponseResponse,

    -- ** EnableProactiveEngagement
    EnableProactiveEngagement (EnableProactiveEngagement'),
    newEnableProactiveEngagement,
    EnableProactiveEngagementResponse (EnableProactiveEngagementResponse'),
    newEnableProactiveEngagementResponse,

    -- ** GetSubscriptionState
    GetSubscriptionState (GetSubscriptionState'),
    newGetSubscriptionState,
    GetSubscriptionStateResponse (GetSubscriptionStateResponse'),
    newGetSubscriptionStateResponse,

    -- ** ListAttacks (Paginated)
    ListAttacks (ListAttacks'),
    newListAttacks,
    ListAttacksResponse (ListAttacksResponse'),
    newListAttacksResponse,

    -- ** ListProtectionGroups
    ListProtectionGroups (ListProtectionGroups'),
    newListProtectionGroups,
    ListProtectionGroupsResponse (ListProtectionGroupsResponse'),
    newListProtectionGroupsResponse,

    -- ** ListProtections (Paginated)
    ListProtections (ListProtections'),
    newListProtections,
    ListProtectionsResponse (ListProtectionsResponse'),
    newListProtectionsResponse,

    -- ** ListResourcesInProtectionGroup
    ListResourcesInProtectionGroup (ListResourcesInProtectionGroup'),
    newListResourcesInProtectionGroup,
    ListResourcesInProtectionGroupResponse (ListResourcesInProtectionGroupResponse'),
    newListResourcesInProtectionGroupResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateApplicationLayerAutomaticResponse
    UpdateApplicationLayerAutomaticResponse (UpdateApplicationLayerAutomaticResponse'),
    newUpdateApplicationLayerAutomaticResponse,
    UpdateApplicationLayerAutomaticResponseResponse (UpdateApplicationLayerAutomaticResponseResponse'),
    newUpdateApplicationLayerAutomaticResponseResponse,

    -- ** UpdateEmergencyContactSettings
    UpdateEmergencyContactSettings (UpdateEmergencyContactSettings'),
    newUpdateEmergencyContactSettings,
    UpdateEmergencyContactSettingsResponse (UpdateEmergencyContactSettingsResponse'),
    newUpdateEmergencyContactSettingsResponse,

    -- ** UpdateProtectionGroup
    UpdateProtectionGroup (UpdateProtectionGroup'),
    newUpdateProtectionGroup,
    UpdateProtectionGroupResponse (UpdateProtectionGroupResponse'),
    newUpdateProtectionGroupResponse,

    -- ** UpdateSubscription
    UpdateSubscription (UpdateSubscription'),
    newUpdateSubscription,
    UpdateSubscriptionResponse (UpdateSubscriptionResponse'),
    newUpdateSubscriptionResponse,

    -- * Types

    -- ** ApplicationLayerAutomaticResponseStatus
    ApplicationLayerAutomaticResponseStatus (..),

    -- ** AttackLayer
    AttackLayer (..),

    -- ** AttackPropertyIdentifier
    AttackPropertyIdentifier (..),

    -- ** AutoRenew
    AutoRenew (..),

    -- ** ProactiveEngagementStatus
    ProactiveEngagementStatus (..),

    -- ** ProtectedResourceType
    ProtectedResourceType (..),

    -- ** ProtectionGroupAggregation
    ProtectionGroupAggregation (..),

    -- ** ProtectionGroupPattern
    ProtectionGroupPattern (..),

    -- ** SubResourceType
    SubResourceType (..),

    -- ** SubscriptionState
    SubscriptionState (..),

    -- ** Unit
    Unit (..),

    -- ** ApplicationLayerAutomaticResponseConfiguration
    ApplicationLayerAutomaticResponseConfiguration (ApplicationLayerAutomaticResponseConfiguration'),
    newApplicationLayerAutomaticResponseConfiguration,

    -- ** AttackDetail
    AttackDetail (AttackDetail'),
    newAttackDetail,

    -- ** AttackProperty
    AttackProperty (AttackProperty'),
    newAttackProperty,

    -- ** AttackStatisticsDataItem
    AttackStatisticsDataItem (AttackStatisticsDataItem'),
    newAttackStatisticsDataItem,

    -- ** AttackSummary
    AttackSummary (AttackSummary'),
    newAttackSummary,

    -- ** AttackVectorDescription
    AttackVectorDescription (AttackVectorDescription'),
    newAttackVectorDescription,

    -- ** AttackVolume
    AttackVolume (AttackVolume'),
    newAttackVolume,

    -- ** AttackVolumeStatistics
    AttackVolumeStatistics (AttackVolumeStatistics'),
    newAttackVolumeStatistics,

    -- ** BlockAction
    BlockAction (BlockAction'),
    newBlockAction,

    -- ** Contributor
    Contributor (Contributor'),
    newContributor,

    -- ** CountAction
    CountAction (CountAction'),
    newCountAction,

    -- ** EmergencyContact
    EmergencyContact (EmergencyContact'),
    newEmergencyContact,

    -- ** InclusionProtectionFilters
    InclusionProtectionFilters (InclusionProtectionFilters'),
    newInclusionProtectionFilters,

    -- ** InclusionProtectionGroupFilters
    InclusionProtectionGroupFilters (InclusionProtectionGroupFilters'),
    newInclusionProtectionGroupFilters,

    -- ** Limit
    Limit (Limit'),
    newLimit,

    -- ** Mitigation
    Mitigation (Mitigation'),
    newMitigation,

    -- ** Protection
    Protection (Protection'),
    newProtection,

    -- ** ProtectionGroup
    ProtectionGroup (ProtectionGroup'),
    newProtectionGroup,

    -- ** ProtectionGroupArbitraryPatternLimits
    ProtectionGroupArbitraryPatternLimits (ProtectionGroupArbitraryPatternLimits'),
    newProtectionGroupArbitraryPatternLimits,

    -- ** ProtectionGroupLimits
    ProtectionGroupLimits (ProtectionGroupLimits'),
    newProtectionGroupLimits,

    -- ** ProtectionGroupPatternTypeLimits
    ProtectionGroupPatternTypeLimits (ProtectionGroupPatternTypeLimits'),
    newProtectionGroupPatternTypeLimits,

    -- ** ProtectionLimits
    ProtectionLimits (ProtectionLimits'),
    newProtectionLimits,

    -- ** ResponseAction
    ResponseAction (ResponseAction'),
    newResponseAction,

    -- ** SubResourceSummary
    SubResourceSummary (SubResourceSummary'),
    newSubResourceSummary,

    -- ** Subscription
    Subscription (Subscription'),
    newSubscription,

    -- ** SubscriptionLimits
    SubscriptionLimits (SubscriptionLimits'),
    newSubscriptionLimits,

    -- ** SummarizedAttackVector
    SummarizedAttackVector (SummarizedAttackVector'),
    newSummarizedAttackVector,

    -- ** SummarizedCounter
    SummarizedCounter (SummarizedCounter'),
    newSummarizedCounter,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TimeRange
    TimeRange (TimeRange'),
    newTimeRange,
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
import Amazonka.Shield.Lens
import Amazonka.Shield.ListAttacks
import Amazonka.Shield.ListProtectionGroups
import Amazonka.Shield.ListProtections
import Amazonka.Shield.ListResourcesInProtectionGroup
import Amazonka.Shield.ListTagsForResource
import Amazonka.Shield.TagResource
import Amazonka.Shield.Types
import Amazonka.Shield.UntagResource
import Amazonka.Shield.UpdateApplicationLayerAutomaticResponse
import Amazonka.Shield.UpdateEmergencyContactSettings
import Amazonka.Shield.UpdateProtectionGroup
import Amazonka.Shield.UpdateSubscription
import Amazonka.Shield.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Shield'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
