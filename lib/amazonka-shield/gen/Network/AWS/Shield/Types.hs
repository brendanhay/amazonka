-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types
  ( -- * Service configuration
    shieldService,

    -- * Errors

    -- * AttackLayer
    AttackLayer (..),

    -- * AttackPropertyIdentifier
    AttackPropertyIdentifier (..),

    -- * AutoRenew
    AutoRenew (..),

    -- * ProactiveEngagementStatus
    ProactiveEngagementStatus (..),

    -- * ProtectedResourceType
    ProtectedResourceType (..),

    -- * ProtectionGroupAggregation
    ProtectionGroupAggregation (..),

    -- * ProtectionGroupPattern
    ProtectionGroupPattern (..),

    -- * SubResourceType
    SubResourceType (..),

    -- * SubscriptionState
    SubscriptionState (..),

    -- * Unit
    Unit (..),

    -- * AttackDetail
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

    -- * AttackProperty
    AttackProperty (..),
    mkAttackProperty,
    apAttackLayer,
    apTopContributors,
    apAttackPropertyIdentifier,
    apTotal,
    apUnit,

    -- * AttackStatisticsDataItem
    AttackStatisticsDataItem (..),
    mkAttackStatisticsDataItem,
    asdiAttackVolume,
    asdiAttackCount,

    -- * AttackSummary
    AttackSummary (..),
    mkAttackSummary,
    asAttackVectors,
    asAttackId,
    asStartTime,
    asResourceARN,
    asEndTime,

    -- * AttackVectorDescription
    AttackVectorDescription (..),
    mkAttackVectorDescription,
    avdVectorType,

    -- * AttackVolume
    AttackVolume (..),
    mkAttackVolume,
    avPacketsPerSecond,
    avRequestsPerSecond,
    avBitsPerSecond,

    -- * AttackVolumeStatistics
    AttackVolumeStatistics (..),
    mkAttackVolumeStatistics,
    avsMax,

    -- * Contributor
    Contributor (..),
    mkContributor,
    cValue,
    cName,

    -- * EmergencyContact
    EmergencyContact (..),
    mkEmergencyContact,
    ecPhoneNumber,
    ecContactNotes,
    ecEmailAddress,

    -- * Limit
    Limit (..),
    mkLimit,
    lMax,
    lType,

    -- * Mitigation
    Mitigation (..),
    mkMitigation,
    mMitigationName,

    -- * Protection
    Protection (..),
    mkProtection,
    pHealthCheckIds,
    pResourceARN,
    pName,
    pId,

    -- * ProtectionGroup
    ProtectionGroup (..),
    mkProtectionGroup,
    pgResourceType,
    pgProtectionGroupId,
    pgAggregation,
    pgPattern,
    pgMembers,

    -- * ProtectionGroupArbitraryPatternLimits
    ProtectionGroupArbitraryPatternLimits (..),
    mkProtectionGroupArbitraryPatternLimits,
    pgaplMaxMembers,

    -- * ProtectionGroupLimits
    ProtectionGroupLimits (..),
    mkProtectionGroupLimits,
    pglMaxProtectionGroups,
    pglPatternTypeLimits,

    -- * ProtectionGroupPatternTypeLimits
    ProtectionGroupPatternTypeLimits (..),
    mkProtectionGroupPatternTypeLimits,
    pgptlArbitraryPatternLimits,

    -- * ProtectionLimits
    ProtectionLimits (..),
    mkProtectionLimits,
    plProtectedResourceTypeLimits,

    -- * SubResourceSummary
    SubResourceSummary (..),
    mkSubResourceSummary,
    srsCounters,
    srsAttackVectors,
    srsId,
    srsType,

    -- * Subscription
    Subscription (..),
    mkSubscription,
    sTimeCommitmentInSeconds,
    sStartTime,
    sLimits,
    sAutoRenew,
    sEndTime,
    sProactiveEngagementStatus,
    sSubscriptionLimits,

    -- * SubscriptionLimits
    SubscriptionLimits (..),
    mkSubscriptionLimits,
    slProtectionLimits,
    slProtectionGroupLimits,

    -- * SummarizedAttackVector
    SummarizedAttackVector (..),
    mkSummarizedAttackVector,
    savVectorCounters,
    savVectorType,

    -- * SummarizedCounter
    SummarizedCounter (..),
    mkSummarizedCounter,
    scMax,
    scAverage,
    scN,
    scName,
    scSum,
    scUnit,

    -- * TimeRange
    TimeRange (..),
    mkTimeRange,
    trFromInclusive,
    trToExclusive,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Shield.Types.AttackDetail
import Network.AWS.Shield.Types.AttackLayer
import Network.AWS.Shield.Types.AttackProperty
import Network.AWS.Shield.Types.AttackPropertyIdentifier
import Network.AWS.Shield.Types.AttackStatisticsDataItem
import Network.AWS.Shield.Types.AttackSummary
import Network.AWS.Shield.Types.AttackVectorDescription
import Network.AWS.Shield.Types.AttackVolume
import Network.AWS.Shield.Types.AttackVolumeStatistics
import Network.AWS.Shield.Types.AutoRenew
import Network.AWS.Shield.Types.Contributor
import Network.AWS.Shield.Types.EmergencyContact
import Network.AWS.Shield.Types.Limit
import Network.AWS.Shield.Types.Mitigation
import Network.AWS.Shield.Types.ProactiveEngagementStatus
import Network.AWS.Shield.Types.ProtectedResourceType
import Network.AWS.Shield.Types.Protection
import Network.AWS.Shield.Types.ProtectionGroup
import Network.AWS.Shield.Types.ProtectionGroupAggregation
import Network.AWS.Shield.Types.ProtectionGroupArbitraryPatternLimits
import Network.AWS.Shield.Types.ProtectionGroupLimits
import Network.AWS.Shield.Types.ProtectionGroupPattern
import Network.AWS.Shield.Types.ProtectionGroupPatternTypeLimits
import Network.AWS.Shield.Types.ProtectionLimits
import Network.AWS.Shield.Types.SubResourceSummary
import Network.AWS.Shield.Types.SubResourceType
import Network.AWS.Shield.Types.Subscription
import Network.AWS.Shield.Types.SubscriptionLimits
import Network.AWS.Shield.Types.SubscriptionState
import Network.AWS.Shield.Types.SummarizedAttackVector
import Network.AWS.Shield.Types.SummarizedCounter
import Network.AWS.Shield.Types.TimeRange
import Network.AWS.Shield.Types.Unit
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-06-02@ of the Amazon Shield SDK configuration.
shieldService :: Lude.Service
shieldService =
  Lude.Service
    { Lude._svcAbbrev = "Shield",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "shield",
      Lude._svcVersion = "2016-06-02",
      Lude._svcEndpoint = Lude.defaultEndpoint shieldService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "Shield",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
