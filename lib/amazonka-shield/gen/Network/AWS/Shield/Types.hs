{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types
  ( -- * Service Configuration
    shield,

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
    AttackDetail,
    attackDetail,
    adAttackId,
    adStartTime,
    adSubResources,
    adMitigations,
    adAttackProperties,
    adAttackCounters,
    adResourceARN,
    adEndTime,

    -- * AttackProperty
    AttackProperty,
    attackProperty,
    apAttackLayer,
    apTopContributors,
    apAttackPropertyIdentifier,
    apTotal,
    apUnit,

    -- * AttackStatisticsDataItem
    AttackStatisticsDataItem,
    attackStatisticsDataItem,
    asdiAttackVolume,
    asdiAttackCount,

    -- * AttackSummary
    AttackSummary,
    attackSummary,
    asAttackVectors,
    asAttackId,
    asStartTime,
    asResourceARN,
    asEndTime,

    -- * AttackVectorDescription
    AttackVectorDescription,
    attackVectorDescription,
    avdVectorType,

    -- * AttackVolume
    AttackVolume,
    attackVolume,
    avPacketsPerSecond,
    avRequestsPerSecond,
    avBitsPerSecond,

    -- * AttackVolumeStatistics
    AttackVolumeStatistics,
    attackVolumeStatistics,
    avsMax,

    -- * Contributor
    Contributor,
    contributor,
    cValue,
    cName,

    -- * EmergencyContact
    EmergencyContact,
    emergencyContact,
    ecPhoneNumber,
    ecContactNotes,
    ecEmailAddress,

    -- * Limit
    Limit,
    limit,
    lMax,
    lType,

    -- * Mitigation
    Mitigation,
    mitigation,
    mMitigationName,

    -- * Protection
    Protection,
    protection,
    pHealthCheckIds,
    pResourceARN,
    pName,
    pId,

    -- * ProtectionGroup
    ProtectionGroup,
    protectionGroup,
    pgResourceType,
    pgProtectionGroupId,
    pgAggregation,
    pgPattern,
    pgMembers,

    -- * ProtectionGroupArbitraryPatternLimits
    ProtectionGroupArbitraryPatternLimits,
    protectionGroupArbitraryPatternLimits,
    pgaplMaxMembers,

    -- * ProtectionGroupLimits
    ProtectionGroupLimits,
    protectionGroupLimits,
    pglMaxProtectionGroups,
    pglPatternTypeLimits,

    -- * ProtectionGroupPatternTypeLimits
    ProtectionGroupPatternTypeLimits,
    protectionGroupPatternTypeLimits,
    pgptlArbitraryPatternLimits,

    -- * ProtectionLimits
    ProtectionLimits,
    protectionLimits,
    plProtectedResourceTypeLimits,

    -- * SubResourceSummary
    SubResourceSummary,
    subResourceSummary,
    srsCounters,
    srsAttackVectors,
    srsId,
    srsType,

    -- * Subscription
    Subscription,
    subscription,
    sTimeCommitmentInSeconds,
    sStartTime,
    sLimits,
    sAutoRenew,
    sEndTime,
    sProactiveEngagementStatus,
    sSubscriptionLimits,

    -- * SubscriptionLimits
    SubscriptionLimits,
    subscriptionLimits,
    slProtectionLimits,
    slProtectionGroupLimits,

    -- * SummarizedAttackVector
    SummarizedAttackVector,
    summarizedAttackVector,
    savVectorCounters,
    savVectorType,

    -- * SummarizedCounter
    SummarizedCounter,
    summarizedCounter,
    scMax,
    scAverage,
    scN,
    scName,
    scSum,
    scUnit,

    -- * TimeRange
    TimeRange,
    timeRange,
    trFromInclusive,
    trToExclusive,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
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
import Network.AWS.Sign.V4

-- | API version @2016-06-02@ of the Amazon Shield SDK configuration.
shield :: Service
shield =
  Service
    { _svcAbbrev = "Shield",
      _svcSigner = v4,
      _svcPrefix = "shield",
      _svcVersion = "2016-06-02",
      _svcEndpoint = defaultEndpoint shield,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "Shield",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
