{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Shield Advanced__
--
-- This is the /AWS Shield Advanced API Reference/ . This guide is for developers who need detailed information about the AWS Shield Advanced API actions, data types, and errors. For detailed information about AWS WAF and AWS Shield Advanced features and an overview of how to use the AWS WAF and AWS Shield Advanced APIs, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF and AWS Shield Developer Guide> .
module Network.AWS.Shield
  ( -- * Service configuration
    shieldService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateDRTLogBucket
    module Network.AWS.Shield.AssociateDRTLogBucket,

    -- ** DisassociateDRTRole
    module Network.AWS.Shield.DisassociateDRTRole,

    -- ** CreateSubscription
    module Network.AWS.Shield.CreateSubscription,

    -- ** ListProtections (Paginated)
    module Network.AWS.Shield.ListProtections,

    -- ** AssociateDRTRole
    module Network.AWS.Shield.AssociateDRTRole,

    -- ** UpdateSubscription
    module Network.AWS.Shield.UpdateSubscription,

    -- ** DisassociateDRTLogBucket
    module Network.AWS.Shield.DisassociateDRTLogBucket,

    -- ** AssociateProactiveEngagementDetails
    module Network.AWS.Shield.AssociateProactiveEngagementDetails,

    -- ** DescribeAttack
    module Network.AWS.Shield.DescribeAttack,

    -- ** ListProtectionGroups
    module Network.AWS.Shield.ListProtectionGroups,

    -- ** EnableProactiveEngagement
    module Network.AWS.Shield.EnableProactiveEngagement,

    -- ** UpdateEmergencyContactSettings
    module Network.AWS.Shield.UpdateEmergencyContactSettings,

    -- ** CreateProtectionGroup
    module Network.AWS.Shield.CreateProtectionGroup,

    -- ** DisableProactiveEngagement
    module Network.AWS.Shield.DisableProactiveEngagement,

    -- ** DisassociateHealthCheck
    module Network.AWS.Shield.DisassociateHealthCheck,

    -- ** ListResourcesInProtectionGroup
    module Network.AWS.Shield.ListResourcesInProtectionGroup,

    -- ** DescribeProtection
    module Network.AWS.Shield.DescribeProtection,

    -- ** ListAttacks (Paginated)
    module Network.AWS.Shield.ListAttacks,

    -- ** DescribeEmergencyContactSettings
    module Network.AWS.Shield.DescribeEmergencyContactSettings,

    -- ** CreateProtection
    module Network.AWS.Shield.CreateProtection,

    -- ** DeleteProtection
    module Network.AWS.Shield.DeleteProtection,

    -- ** GetSubscriptionState
    module Network.AWS.Shield.GetSubscriptionState,

    -- ** DeleteProtectionGroup
    module Network.AWS.Shield.DeleteProtectionGroup,

    -- ** UpdateProtectionGroup
    module Network.AWS.Shield.UpdateProtectionGroup,

    -- ** DescribeAttackStatistics
    module Network.AWS.Shield.DescribeAttackStatistics,

    -- ** DescribeDRTAccess
    module Network.AWS.Shield.DescribeDRTAccess,

    -- ** DescribeSubscription
    module Network.AWS.Shield.DescribeSubscription,

    -- ** AssociateHealthCheck
    module Network.AWS.Shield.AssociateHealthCheck,

    -- ** DescribeProtectionGroup
    module Network.AWS.Shield.DescribeProtectionGroup,

    -- * Types

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

    -- ** AttackDetail
    AttackDetail (..),
    mkAttackDetail,
    adAttackId,
    adStartTime,
    adSubResources,
    adMitigations,
    adAttackProperties,
    adAttackCounters,
    adResourceARN,
    adEndTime,

    -- ** AttackProperty
    AttackProperty (..),
    mkAttackProperty,
    apAttackLayer,
    apTopContributors,
    apAttackPropertyIdentifier,
    apTotal,
    apUnit,

    -- ** AttackStatisticsDataItem
    AttackStatisticsDataItem (..),
    mkAttackStatisticsDataItem,
    asdiAttackVolume,
    asdiAttackCount,

    -- ** AttackSummary
    AttackSummary (..),
    mkAttackSummary,
    asAttackVectors,
    asAttackId,
    asStartTime,
    asResourceARN,
    asEndTime,

    -- ** AttackVectorDescription
    AttackVectorDescription (..),
    mkAttackVectorDescription,
    avdVectorType,

    -- ** AttackVolume
    AttackVolume (..),
    mkAttackVolume,
    avPacketsPerSecond,
    avRequestsPerSecond,
    avBitsPerSecond,

    -- ** AttackVolumeStatistics
    AttackVolumeStatistics (..),
    mkAttackVolumeStatistics,
    avsMax,

    -- ** Contributor
    Contributor (..),
    mkContributor,
    cValue,
    cName,

    -- ** EmergencyContact
    EmergencyContact (..),
    mkEmergencyContact,
    ecPhoneNumber,
    ecContactNotes,
    ecEmailAddress,

    -- ** Limit
    Limit (..),
    mkLimit,
    lMax,
    lType,

    -- ** Mitigation
    Mitigation (..),
    mkMitigation,
    mMitigationName,

    -- ** Protection
    Protection (..),
    mkProtection,
    pHealthCheckIds,
    pResourceARN,
    pName,
    pId,

    -- ** ProtectionGroup
    ProtectionGroup (..),
    mkProtectionGroup,
    pgResourceType,
    pgProtectionGroupId,
    pgAggregation,
    pgPattern,
    pgMembers,

    -- ** ProtectionGroupArbitraryPatternLimits
    ProtectionGroupArbitraryPatternLimits (..),
    mkProtectionGroupArbitraryPatternLimits,
    pgaplMaxMembers,

    -- ** ProtectionGroupLimits
    ProtectionGroupLimits (..),
    mkProtectionGroupLimits,
    pglMaxProtectionGroups,
    pglPatternTypeLimits,

    -- ** ProtectionGroupPatternTypeLimits
    ProtectionGroupPatternTypeLimits (..),
    mkProtectionGroupPatternTypeLimits,
    pgptlArbitraryPatternLimits,

    -- ** ProtectionLimits
    ProtectionLimits (..),
    mkProtectionLimits,
    plProtectedResourceTypeLimits,

    -- ** SubResourceSummary
    SubResourceSummary (..),
    mkSubResourceSummary,
    srsCounters,
    srsAttackVectors,
    srsId,
    srsType,

    -- ** Subscription
    Subscription (..),
    mkSubscription,
    sTimeCommitmentInSeconds,
    sStartTime,
    sLimits,
    sAutoRenew,
    sEndTime,
    sProactiveEngagementStatus,
    sSubscriptionLimits,

    -- ** SubscriptionLimits
    SubscriptionLimits (..),
    mkSubscriptionLimits,
    slProtectionLimits,
    slProtectionGroupLimits,

    -- ** SummarizedAttackVector
    SummarizedAttackVector (..),
    mkSummarizedAttackVector,
    savVectorCounters,
    savVectorType,

    -- ** SummarizedCounter
    SummarizedCounter (..),
    mkSummarizedCounter,
    scMax,
    scAverage,
    scN,
    scName,
    scSum,
    scUnit,

    -- ** TimeRange
    TimeRange (..),
    mkTimeRange,
    trFromInclusive,
    trToExclusive,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
  )
where

import qualified Network.AWS.Prelude as Lude
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
import Network.AWS.Shield.Types
import Network.AWS.Shield.UpdateEmergencyContactSettings
import Network.AWS.Shield.UpdateProtectionGroup
import Network.AWS.Shield.UpdateSubscription
import Network.AWS.Shield.Waiters

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
