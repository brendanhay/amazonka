{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _OptimisticLockException,
    _InvalidResourceException,
    _InvalidPaginationTokenException,
    _InvalidOperationException,
    _ResourceAlreadyExistsException,
    _InternalErrorException,
    _NoAssociatedRoleException,
    _InvalidParameterException,
    _AccessDeniedException,
    _LockedSubscriptionException,
    _ResourceNotFoundException,
    _AccessDeniedForDependencyException,
    _LimitsExceededException,

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
    newAttackDetail,
    attackDetail_resourceArn,
    attackDetail_attackProperties,
    attackDetail_startTime,
    attackDetail_endTime,
    attackDetail_attackCounters,
    attackDetail_mitigations,
    attackDetail_attackId,
    attackDetail_subResources,

    -- * AttackProperty
    AttackProperty (..),
    newAttackProperty,
    attackProperty_unit,
    attackProperty_total,
    attackProperty_attackPropertyIdentifier,
    attackProperty_attackLayer,
    attackProperty_topContributors,

    -- * AttackStatisticsDataItem
    AttackStatisticsDataItem (..),
    newAttackStatisticsDataItem,
    attackStatisticsDataItem_attackVolume,
    attackStatisticsDataItem_attackCount,

    -- * AttackSummary
    AttackSummary (..),
    newAttackSummary,
    attackSummary_resourceArn,
    attackSummary_startTime,
    attackSummary_endTime,
    attackSummary_attackId,
    attackSummary_attackVectors,

    -- * AttackVectorDescription
    AttackVectorDescription (..),
    newAttackVectorDescription,
    attackVectorDescription_vectorType,

    -- * AttackVolume
    AttackVolume (..),
    newAttackVolume,
    attackVolume_packetsPerSecond,
    attackVolume_bitsPerSecond,
    attackVolume_requestsPerSecond,

    -- * AttackVolumeStatistics
    AttackVolumeStatistics (..),
    newAttackVolumeStatistics,
    attackVolumeStatistics_max,

    -- * Contributor
    Contributor (..),
    newContributor,
    contributor_name,
    contributor_value,

    -- * EmergencyContact
    EmergencyContact (..),
    newEmergencyContact,
    emergencyContact_phoneNumber,
    emergencyContact_contactNotes,
    emergencyContact_emailAddress,

    -- * Limit
    Limit (..),
    newLimit,
    limit_max,
    limit_type,

    -- * Mitigation
    Mitigation (..),
    newMitigation,
    mitigation_mitigationName,

    -- * Protection
    Protection (..),
    newProtection,
    protection_resourceArn,
    protection_id,
    protection_name,
    protection_healthCheckIds,

    -- * ProtectionGroup
    ProtectionGroup (..),
    newProtectionGroup,
    protectionGroup_resourceType,
    protectionGroup_protectionGroupId,
    protectionGroup_aggregation,
    protectionGroup_pattern,
    protectionGroup_members,

    -- * ProtectionGroupArbitraryPatternLimits
    ProtectionGroupArbitraryPatternLimits (..),
    newProtectionGroupArbitraryPatternLimits,
    protectionGroupArbitraryPatternLimits_maxMembers,

    -- * ProtectionGroupLimits
    ProtectionGroupLimits (..),
    newProtectionGroupLimits,
    protectionGroupLimits_maxProtectionGroups,
    protectionGroupLimits_patternTypeLimits,

    -- * ProtectionGroupPatternTypeLimits
    ProtectionGroupPatternTypeLimits (..),
    newProtectionGroupPatternTypeLimits,
    protectionGroupPatternTypeLimits_arbitraryPatternLimits,

    -- * ProtectionLimits
    ProtectionLimits (..),
    newProtectionLimits,
    protectionLimits_protectedResourceTypeLimits,

    -- * SubResourceSummary
    SubResourceSummary (..),
    newSubResourceSummary,
    subResourceSummary_counters,
    subResourceSummary_id,
    subResourceSummary_type,
    subResourceSummary_attackVectors,

    -- * Subscription
    Subscription (..),
    newSubscription,
    subscription_autoRenew,
    subscription_proactiveEngagementStatus,
    subscription_startTime,
    subscription_endTime,
    subscription_limits,
    subscription_timeCommitmentInSeconds,
    subscription_subscriptionLimits,

    -- * SubscriptionLimits
    SubscriptionLimits (..),
    newSubscriptionLimits,
    subscriptionLimits_protectionLimits,
    subscriptionLimits_protectionGroupLimits,

    -- * SummarizedAttackVector
    SummarizedAttackVector (..),
    newSummarizedAttackVector,
    summarizedAttackVector_vectorCounters,
    summarizedAttackVector_vectorType,

    -- * SummarizedCounter
    SummarizedCounter (..),
    newSummarizedCounter,
    summarizedCounter_unit,
    summarizedCounter_n,
    summarizedCounter_sum,
    summarizedCounter_name,
    summarizedCounter_max,
    summarizedCounter_average,

    -- * TimeRange
    TimeRange (..),
    newTimeRange,
    timeRange_fromInclusive,
    timeRange_toExclusive,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
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
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Shield",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "shield",
      Core._serviceSigningName = "shield",
      Core._serviceVersion = "2016-06-02",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Core.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "Shield",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has (Core.hasStatus 504) e =
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | Exception that indicates that the resource state has been modified by
-- another client. Retrieve the resource and then retry your request.
_OptimisticLockException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OptimisticLockException =
  Core._MatchServiceError
    defaultService
    "OptimisticLockException"

-- | Exception that indicates that the resource is invalid. You might not
-- have access to the resource, or the resource might not exist.
_InvalidResourceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidResourceException =
  Core._MatchServiceError
    defaultService
    "InvalidResourceException"

-- | Exception that indicates that the NextToken specified in the request is
-- invalid. Submit the request using the NextToken value that was returned
-- in the response.
_InvalidPaginationTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidPaginationTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidPaginationTokenException"

-- | Exception that indicates that the operation would not cause any change
-- to occur.
_InvalidOperationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidOperationException =
  Core._MatchServiceError
    defaultService
    "InvalidOperationException"

-- | Exception indicating the specified resource already exists. If
-- available, this exception includes details in additional properties.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"

-- | Exception that indicates that a problem occurred with the service
-- infrastructure. You can retry the request.
_InternalErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalErrorException =
  Core._MatchServiceError
    defaultService
    "InternalErrorException"

-- | The ARN of the role that you specifed does not exist.
_NoAssociatedRoleException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoAssociatedRoleException =
  Core._MatchServiceError
    defaultService
    "NoAssociatedRoleException"

-- | Exception that indicates that the parameters passed to the API are
-- invalid. If available, this exception includes details in additional
-- properties.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | Exception that indicates the specified @AttackId@ does not exist, or the
-- requester does not have the appropriate permissions to access the
-- @AttackId@.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | You are trying to update a subscription that has not yet completed the
-- 1-year commitment. You can change the @AutoRenew@ parameter during the
-- last 30 days of your subscription. This exception indicates that you are
-- attempting to change @AutoRenew@ prior to that period.
_LockedSubscriptionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LockedSubscriptionException =
  Core._MatchServiceError
    defaultService
    "LockedSubscriptionException"

-- | Exception indicating the specified resource does not exist. If
-- available, this exception includes details in additional properties.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | In order to grant the necessary access to the DDoS Response Team (DRT),
-- the user submitting the request must have the @iam:PassRole@ permission.
-- This error indicates the user did not have the appropriate permissions.
-- For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use_passrole.html Granting a User Permissions to Pass a Role to an AWS Service>.
_AccessDeniedForDependencyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccessDeniedForDependencyException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedForDependencyException"

-- | Exception that indicates that the operation would exceed a limit.
--
-- @Type@ is the type of limit that would be exceeded.
--
-- @Limit@ is the threshold that would be exceeded.
_LimitsExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitsExceededException =
  Core._MatchServiceError
    defaultService
    "LimitsExceededException"
