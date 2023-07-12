{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Shield.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Shield.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _AccessDeniedForDependencyException,
    _InternalErrorException,
    _InvalidOperationException,
    _InvalidPaginationTokenException,
    _InvalidParameterException,
    _InvalidResourceException,
    _LimitsExceededException,
    _LockedSubscriptionException,
    _NoAssociatedRoleException,
    _OptimisticLockException,
    _ResourceAlreadyExistsException,
    _ResourceNotFoundException,

    -- * ApplicationLayerAutomaticResponseStatus
    ApplicationLayerAutomaticResponseStatus (..),

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

    -- * ApplicationLayerAutomaticResponseConfiguration
    ApplicationLayerAutomaticResponseConfiguration (..),
    newApplicationLayerAutomaticResponseConfiguration,
    applicationLayerAutomaticResponseConfiguration_status,
    applicationLayerAutomaticResponseConfiguration_action,

    -- * AttackDetail
    AttackDetail (..),
    newAttackDetail,
    attackDetail_attackCounters,
    attackDetail_attackId,
    attackDetail_attackProperties,
    attackDetail_endTime,
    attackDetail_mitigations,
    attackDetail_resourceArn,
    attackDetail_startTime,
    attackDetail_subResources,

    -- * AttackProperty
    AttackProperty (..),
    newAttackProperty,
    attackProperty_attackLayer,
    attackProperty_attackPropertyIdentifier,
    attackProperty_topContributors,
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
    attackSummary_attackId,
    attackSummary_attackVectors,
    attackSummary_endTime,
    attackSummary_resourceArn,
    attackSummary_startTime,

    -- * AttackVectorDescription
    AttackVectorDescription (..),
    newAttackVectorDescription,
    attackVectorDescription_vectorType,

    -- * AttackVolume
    AttackVolume (..),
    newAttackVolume,
    attackVolume_bitsPerSecond,
    attackVolume_packetsPerSecond,
    attackVolume_requestsPerSecond,

    -- * AttackVolumeStatistics
    AttackVolumeStatistics (..),
    newAttackVolumeStatistics,
    attackVolumeStatistics_max,

    -- * BlockAction
    BlockAction (..),
    newBlockAction,

    -- * Contributor
    Contributor (..),
    newContributor,
    contributor_name,
    contributor_value,

    -- * CountAction
    CountAction (..),
    newCountAction,

    -- * EmergencyContact
    EmergencyContact (..),
    newEmergencyContact,
    emergencyContact_contactNotes,
    emergencyContact_phoneNumber,
    emergencyContact_emailAddress,

    -- * InclusionProtectionFilters
    InclusionProtectionFilters (..),
    newInclusionProtectionFilters,
    inclusionProtectionFilters_protectionNames,
    inclusionProtectionFilters_resourceArns,
    inclusionProtectionFilters_resourceTypes,

    -- * InclusionProtectionGroupFilters
    InclusionProtectionGroupFilters (..),
    newInclusionProtectionGroupFilters,
    inclusionProtectionGroupFilters_aggregations,
    inclusionProtectionGroupFilters_patterns,
    inclusionProtectionGroupFilters_protectionGroupIds,
    inclusionProtectionGroupFilters_resourceTypes,

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
    protection_applicationLayerAutomaticResponseConfiguration,
    protection_healthCheckIds,
    protection_id,
    protection_name,
    protection_protectionArn,
    protection_resourceArn,

    -- * ProtectionGroup
    ProtectionGroup (..),
    newProtectionGroup,
    protectionGroup_protectionGroupArn,
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

    -- * ResponseAction
    ResponseAction (..),
    newResponseAction,
    responseAction_block,
    responseAction_count,

    -- * SubResourceSummary
    SubResourceSummary (..),
    newSubResourceSummary,
    subResourceSummary_attackVectors,
    subResourceSummary_counters,
    subResourceSummary_id,
    subResourceSummary_type,

    -- * Subscription
    Subscription (..),
    newSubscription,
    subscription_autoRenew,
    subscription_endTime,
    subscription_limits,
    subscription_proactiveEngagementStatus,
    subscription_startTime,
    subscription_subscriptionArn,
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
    summarizedCounter_average,
    summarizedCounter_max,
    summarizedCounter_n,
    summarizedCounter_name,
    summarizedCounter_sum,
    summarizedCounter_unit,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TimeRange
    TimeRange (..),
    newTimeRange,
    timeRange_fromInclusive,
    timeRange_toExclusive,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Shield.Types.ApplicationLayerAutomaticResponseConfiguration
import Amazonka.Shield.Types.ApplicationLayerAutomaticResponseStatus
import Amazonka.Shield.Types.AttackDetail
import Amazonka.Shield.Types.AttackLayer
import Amazonka.Shield.Types.AttackProperty
import Amazonka.Shield.Types.AttackPropertyIdentifier
import Amazonka.Shield.Types.AttackStatisticsDataItem
import Amazonka.Shield.Types.AttackSummary
import Amazonka.Shield.Types.AttackVectorDescription
import Amazonka.Shield.Types.AttackVolume
import Amazonka.Shield.Types.AttackVolumeStatistics
import Amazonka.Shield.Types.AutoRenew
import Amazonka.Shield.Types.BlockAction
import Amazonka.Shield.Types.Contributor
import Amazonka.Shield.Types.CountAction
import Amazonka.Shield.Types.EmergencyContact
import Amazonka.Shield.Types.InclusionProtectionFilters
import Amazonka.Shield.Types.InclusionProtectionGroupFilters
import Amazonka.Shield.Types.Limit
import Amazonka.Shield.Types.Mitigation
import Amazonka.Shield.Types.ProactiveEngagementStatus
import Amazonka.Shield.Types.ProtectedResourceType
import Amazonka.Shield.Types.Protection
import Amazonka.Shield.Types.ProtectionGroup
import Amazonka.Shield.Types.ProtectionGroupAggregation
import Amazonka.Shield.Types.ProtectionGroupArbitraryPatternLimits
import Amazonka.Shield.Types.ProtectionGroupLimits
import Amazonka.Shield.Types.ProtectionGroupPattern
import Amazonka.Shield.Types.ProtectionGroupPatternTypeLimits
import Amazonka.Shield.Types.ProtectionLimits
import Amazonka.Shield.Types.ResponseAction
import Amazonka.Shield.Types.SubResourceSummary
import Amazonka.Shield.Types.SubResourceType
import Amazonka.Shield.Types.Subscription
import Amazonka.Shield.Types.SubscriptionLimits
import Amazonka.Shield.Types.SubscriptionState
import Amazonka.Shield.Types.SummarizedAttackVector
import Amazonka.Shield.Types.SummarizedCounter
import Amazonka.Shield.Types.Tag
import Amazonka.Shield.Types.TimeRange
import Amazonka.Shield.Types.Unit
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2016-06-02@ of the Amazon Shield SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Shield",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "shield",
      Core.signingName = "shield",
      Core.version = "2016-06-02",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Shield",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | Exception that indicates the specified @AttackId@ does not exist, or the
-- requester does not have the appropriate permissions to access the
-- @AttackId@.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | In order to grant the necessary access to the Shield Response Team (SRT)
-- the user submitting the request must have the @iam:PassRole@ permission.
-- This error indicates the user did not have the appropriate permissions.
-- For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use_passrole.html Granting a User Permissions to Pass a Role to an Amazon Web Services Service>.
_AccessDeniedForDependencyException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedForDependencyException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedForDependencyException"

-- | Exception that indicates that a problem occurred with the service
-- infrastructure. You can retry the request.
_InternalErrorException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalErrorException =
  Core._MatchServiceError
    defaultService
    "InternalErrorException"

-- | Exception that indicates that the operation would not cause any change
-- to occur.
_InvalidOperationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidOperationException =
  Core._MatchServiceError
    defaultService
    "InvalidOperationException"

-- | Exception that indicates that the @NextToken@ specified in the request
-- is invalid. Submit the request using the @NextToken@ value that was
-- returned in the prior response.
_InvalidPaginationTokenException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidPaginationTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidPaginationTokenException"

-- | Exception that indicates that the parameters passed to the API are
-- invalid. If available, this exception includes details in additional
-- properties.
_InvalidParameterException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | Exception that indicates that the resource is invalid. You might not
-- have access to the resource, or the resource might not exist.
_InvalidResourceException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidResourceException =
  Core._MatchServiceError
    defaultService
    "InvalidResourceException"

-- | Exception that indicates that the operation would exceed a limit.
_LimitsExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitsExceededException =
  Core._MatchServiceError
    defaultService
    "LimitsExceededException"

-- | You are trying to update a subscription that has not yet completed the
-- 1-year commitment. You can change the @AutoRenew@ parameter during the
-- last 30 days of your subscription. This exception indicates that you are
-- attempting to change @AutoRenew@ prior to that period.
_LockedSubscriptionException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LockedSubscriptionException =
  Core._MatchServiceError
    defaultService
    "LockedSubscriptionException"

-- | The ARN of the role that you specified does not exist.
_NoAssociatedRoleException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoAssociatedRoleException =
  Core._MatchServiceError
    defaultService
    "NoAssociatedRoleException"

-- | Exception that indicates that the resource state has been modified by
-- another client. Retrieve the resource and then retry your request.
_OptimisticLockException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_OptimisticLockException =
  Core._MatchServiceError
    defaultService
    "OptimisticLockException"

-- | Exception indicating the specified resource already exists. If
-- available, this exception includes details in additional properties.
_ResourceAlreadyExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"

-- | Exception indicating the specified resource does not exist. If
-- available, this exception includes details in additional properties.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
