{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Shield.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Shield.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ResourceAlreadyExistsException,
    _NoAssociatedRoleException,
    _InvalidOperationException,
    _InvalidResourceException,
    _AccessDeniedException,
    _InternalErrorException,
    _InvalidPaginationTokenException,
    _ResourceNotFoundException,
    _AccessDeniedForDependencyException,
    _OptimisticLockException,
    _LimitsExceededException,
    _LockedSubscriptionException,
    _InvalidParameterException,

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
    attackDetail_attackId,
    attackDetail_subResources,
    attackDetail_endTime,
    attackDetail_mitigations,
    attackDetail_resourceArn,
    attackDetail_attackCounters,
    attackDetail_attackProperties,
    attackDetail_startTime,

    -- * AttackProperty
    AttackProperty (..),
    newAttackProperty,
    attackProperty_total,
    attackProperty_attackLayer,
    attackProperty_topContributors,
    attackProperty_attackPropertyIdentifier,
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
    attackSummary_endTime,
    attackSummary_attackVectors,
    attackSummary_resourceArn,
    attackSummary_startTime,

    -- * AttackVectorDescription
    AttackVectorDescription (..),
    newAttackVectorDescription,
    attackVectorDescription_vectorType,

    -- * AttackVolume
    AttackVolume (..),
    newAttackVolume,
    attackVolume_requestsPerSecond,
    attackVolume_bitsPerSecond,
    attackVolume_packetsPerSecond,

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
    inclusionProtectionFilters_resourceTypes,
    inclusionProtectionFilters_protectionNames,
    inclusionProtectionFilters_resourceArns,

    -- * InclusionProtectionGroupFilters
    InclusionProtectionGroupFilters (..),
    newInclusionProtectionGroupFilters,
    inclusionProtectionGroupFilters_aggregations,
    inclusionProtectionGroupFilters_patterns,
    inclusionProtectionGroupFilters_resourceTypes,
    inclusionProtectionGroupFilters_protectionGroupIds,

    -- * Limit
    Limit (..),
    newLimit,
    limit_type,
    limit_max,

    -- * Mitigation
    Mitigation (..),
    newMitigation,
    mitigation_mitigationName,

    -- * Protection
    Protection (..),
    newProtection,
    protection_name,
    protection_applicationLayerAutomaticResponseConfiguration,
    protection_id,
    protection_protectionArn,
    protection_resourceArn,
    protection_healthCheckIds,

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

    -- * ResponseAction
    ResponseAction (..),
    newResponseAction,
    responseAction_count,
    responseAction_block,

    -- * SubResourceSummary
    SubResourceSummary (..),
    newSubResourceSummary,
    subResourceSummary_type,
    subResourceSummary_counters,
    subResourceSummary_id,
    subResourceSummary_attackVectors,

    -- * Subscription
    Subscription (..),
    newSubscription,
    subscription_subscriptionArn,
    subscription_autoRenew,
    subscription_limits,
    subscription_endTime,
    subscription_timeCommitmentInSeconds,
    subscription_startTime,
    subscription_proactiveEngagementStatus,
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
    summarizedCounter_name,
    summarizedCounter_max,
    summarizedCounter_average,
    summarizedCounter_sum,
    summarizedCounter_n,
    summarizedCounter_unit,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TimeRange
    TimeRange (..),
    newTimeRange,
    timeRange_toExclusive,
    timeRange_fromInclusive,
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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | Exception indicating the specified resource already exists. If
-- available, this exception includes details in additional properties.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"

-- | The ARN of the role that you specified does not exist.
_NoAssociatedRoleException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NoAssociatedRoleException =
  Core._MatchServiceError
    defaultService
    "NoAssociatedRoleException"

-- | Exception that indicates that the operation would not cause any change
-- to occur.
_InvalidOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOperationException =
  Core._MatchServiceError
    defaultService
    "InvalidOperationException"

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

-- | Exception that indicates that a problem occurred with the service
-- infrastructure. You can retry the request.
_InternalErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalErrorException =
  Core._MatchServiceError
    defaultService
    "InternalErrorException"

-- | Exception that indicates that the @NextToken@ specified in the request
-- is invalid. Submit the request using the @NextToken@ value that was
-- returned in the prior response.
_InvalidPaginationTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPaginationTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidPaginationTokenException"

-- | Exception indicating the specified resource does not exist. If
-- available, this exception includes details in additional properties.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

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

-- | Exception that indicates that the resource state has been modified by
-- another client. Retrieve the resource and then retry your request.
_OptimisticLockException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OptimisticLockException =
  Core._MatchServiceError
    defaultService
    "OptimisticLockException"

-- | Exception that indicates that the operation would exceed a limit.
_LimitsExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitsExceededException =
  Core._MatchServiceError
    defaultService
    "LimitsExceededException"

-- | You are trying to update a subscription that has not yet completed the
-- 1-year commitment. You can change the @AutoRenew@ parameter during the
-- last 30 days of your subscription. This exception indicates that you are
-- attempting to change @AutoRenew@ prior to that period.
_LockedSubscriptionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LockedSubscriptionException =
  Core._MatchServiceError
    defaultService
    "LockedSubscriptionException"

-- | Exception that indicates that the parameters passed to the API are
-- invalid. If available, this exception includes details in additional
-- properties.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"
