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
    _InvalidResourceException,
    _AccessDeniedException,
    _InvalidParameterException,
    _LimitsExceededException,
    _InternalErrorException,
    _ResourceAlreadyExistsException,
    _OptimisticLockException,
    _NoAssociatedRoleException,
    _AccessDeniedForDependencyException,
    _InvalidOperationException,
    _LockedSubscriptionException,
    _ResourceNotFoundException,
    _InvalidPaginationTokenException,

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
    attackDetail_attackId,
    attackDetail_startTime,
    attackDetail_subResources,
    attackDetail_mitigations,
    attackDetail_attackProperties,
    attackDetail_attackCounters,
    attackDetail_resourceArn,
    attackDetail_endTime,

    -- * AttackProperty
    AttackProperty (..),
    newAttackProperty,
    attackProperty_attackLayer,
    attackProperty_topContributors,
    attackProperty_attackPropertyIdentifier,
    attackProperty_total,
    attackProperty_unit,

    -- * AttackStatisticsDataItem
    AttackStatisticsDataItem (..),
    newAttackStatisticsDataItem,
    attackStatisticsDataItem_attackVolume,
    attackStatisticsDataItem_attackCount,

    -- * AttackSummary
    AttackSummary (..),
    newAttackSummary,
    attackSummary_attackVectors,
    attackSummary_attackId,
    attackSummary_startTime,
    attackSummary_resourceArn,
    attackSummary_endTime,

    -- * AttackVectorDescription
    AttackVectorDescription (..),
    newAttackVectorDescription,
    attackVectorDescription_vectorType,

    -- * AttackVolume
    AttackVolume (..),
    newAttackVolume,
    attackVolume_packetsPerSecond,
    attackVolume_requestsPerSecond,
    attackVolume_bitsPerSecond,

    -- * AttackVolumeStatistics
    AttackVolumeStatistics (..),
    newAttackVolumeStatistics,
    attackVolumeStatistics_max,

    -- * Contributor
    Contributor (..),
    newContributor,
    contributor_value,
    contributor_name,

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
    protection_protectionArn,
    protection_healthCheckIds,
    protection_resourceArn,
    protection_name,
    protection_id,

    -- * ProtectionGroup
    ProtectionGroup (..),
    newProtectionGroup,
    protectionGroup_resourceType,
    protectionGroup_protectionGroupArn,
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
    subResourceSummary_attackVectors,
    subResourceSummary_id,
    subResourceSummary_type,

    -- * Subscription
    Subscription (..),
    newSubscription,
    subscription_timeCommitmentInSeconds,
    subscription_startTime,
    subscription_limits,
    subscription_autoRenew,
    subscription_endTime,
    subscription_proactiveEngagementStatus,
    subscription_subscriptionArn,
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
    summarizedCounter_max,
    summarizedCounter_average,
    summarizedCounter_n,
    summarizedCounter_name,
    summarizedCounter_sum,
    summarizedCounter_unit,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * TimeRange
    TimeRange (..),
    newTimeRange,
    timeRange_fromInclusive,
    timeRange_toExclusive,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
import Network.AWS.Shield.Types.Tag
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
      Core._serviceTimeout = Prelude.Just 70,
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
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | Exception that indicates that the resource is invalid. You might not
-- have access to the resource, or the resource might not exist.
_InvalidResourceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidResourceException =
  Core._MatchServiceError
    defaultService
    "InvalidResourceException"

-- | Exception that indicates the specified @AttackId@ does not exist, or the
-- requester does not have the appropriate permissions to access the
-- @AttackId@.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | Exception that indicates that the parameters passed to the API are
-- invalid. If available, this exception includes details in additional
-- properties.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | Exception that indicates that the operation would exceed a limit.
_LimitsExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitsExceededException =
  Core._MatchServiceError
    defaultService
    "LimitsExceededException"

-- | Exception that indicates that a problem occurred with the service
-- infrastructure. You can retry the request.
_InternalErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalErrorException =
  Core._MatchServiceError
    defaultService
    "InternalErrorException"

-- | Exception indicating the specified resource already exists. If
-- available, this exception includes details in additional properties.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"

-- | Exception that indicates that the resource state has been modified by
-- another client. Retrieve the resource and then retry your request.
_OptimisticLockException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OptimisticLockException =
  Core._MatchServiceError
    defaultService
    "OptimisticLockException"

-- | The ARN of the role that you specifed does not exist.
_NoAssociatedRoleException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NoAssociatedRoleException =
  Core._MatchServiceError
    defaultService
    "NoAssociatedRoleException"

-- | In order to grant the necessary access to the Shield Response Team (SRT)
-- the user submitting the request must have the @iam:PassRole@ permission.
-- This error indicates the user did not have the appropriate permissions.
-- For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use_passrole.html Granting a User Permissions to Pass a Role to an Amazon Web Services Service>.
_AccessDeniedForDependencyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedForDependencyException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedForDependencyException"

-- | Exception that indicates that the operation would not cause any change
-- to occur.
_InvalidOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOperationException =
  Core._MatchServiceError
    defaultService
    "InvalidOperationException"

-- | You are trying to update a subscription that has not yet completed the
-- 1-year commitment. You can change the @AutoRenew@ parameter during the
-- last 30 days of your subscription. This exception indicates that you are
-- attempting to change @AutoRenew@ prior to that period.
_LockedSubscriptionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LockedSubscriptionException =
  Core._MatchServiceError
    defaultService
    "LockedSubscriptionException"

-- | Exception indicating the specified resource does not exist. If
-- available, this exception includes details in additional properties.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | Exception that indicates that the NextToken specified in the request is
-- invalid. Submit the request using the NextToken value that was returned
-- in the response.
_InvalidPaginationTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPaginationTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidPaginationTokenException"
