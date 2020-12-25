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
    mkServiceConfig,

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

    -- * SubscriptionState
    SubscriptionState (..),

    -- * HealthCheckArn
    HealthCheckArn (..),

    -- * ProtectionGroupLimits
    ProtectionGroupLimits (..),
    mkProtectionGroupLimits,
    pglMaxProtectionGroups,
    pglPatternTypeLimits,

    -- * TimeRange
    TimeRange (..),
    mkTimeRange,
    trFromInclusive,
    trToExclusive,

    -- * AttackId
    AttackId (..),

    -- * AttackVolumeStatistics
    AttackVolumeStatistics (..),
    mkAttackVolumeStatistics,
    avsMax,

    -- * SubscriptionLimits
    SubscriptionLimits (..),
    mkSubscriptionLimits,
    slProtectionLimits,
    slProtectionGroupLimits,

    -- * String
    String (..),

    -- * ProtectionGroupAggregation
    ProtectionGroupAggregation (..),

    -- * ProtectedResourceType
    ProtectedResourceType (..),

    -- * Token
    Token (..),

    -- * AttackLayer
    AttackLayer (..),

    -- * Protection
    Protection (..),
    mkProtection,
    pHealthCheckIds,
    pId,
    pName,
    pResourceArn,

    -- * Mitigation
    Mitigation (..),
    mkMitigation,
    mMitigationName,

    -- * ProtectionId
    ProtectionId (..),

    -- * SummarizedAttackVector
    SummarizedAttackVector (..),
    mkSummarizedAttackVector,
    savVectorType,
    savVectorCounters,

    -- * AttackDetail
    AttackDetail (..),
    mkAttackDetail,
    adAttackCounters,
    adAttackId,
    adAttackProperties,
    adEndTime,
    adMitigations,
    adResourceArn,
    adStartTime,
    adSubResources,

    -- * ProtectionGroupId
    ProtectionGroupId (..),

    -- * AttackPropertyIdentifier
    AttackPropertyIdentifier (..),

    -- * AutoRenew
    AutoRenew (..),

    -- * AttackSummary
    AttackSummary (..),
    mkAttackSummary,
    asAttackId,
    asAttackVectors,
    asEndTime,
    asResourceArn,
    asStartTime,

    -- * ProtectionGroup
    ProtectionGroup (..),
    mkProtectionGroup,
    pgProtectionGroupId,
    pgAggregation,
    pgPattern,
    pgMembers,
    pgResourceType,

    -- * ProtectionGroupPattern
    ProtectionGroupPattern (..),

    -- * ResourceArn
    ResourceArn (..),

    -- * PhoneNumber
    PhoneNumber (..),

    -- * AttackProperty
    AttackProperty (..),
    mkAttackProperty,
    apAttackLayer,
    apAttackPropertyIdentifier,
    apTopContributors,
    apTotal,
    apUnit,

    -- * AttackVectorDescription
    AttackVectorDescription (..),
    mkAttackVectorDescription,
    avdVectorType,

    -- * HealthCheckId
    HealthCheckId (..),

    -- * LogBucket
    LogBucket (..),

    -- * EmailAddress
    EmailAddress (..),

    -- * ProtectionGroupPatternTypeLimits
    ProtectionGroupPatternTypeLimits (..),
    mkProtectionGroupPatternTypeLimits,
    pgptlArbitraryPatternLimits,

    -- * SubResourceSummary
    SubResourceSummary (..),
    mkSubResourceSummary,
    srsAttackVectors,
    srsCounters,
    srsId,
    srsType,

    -- * Limit
    Limit (..),
    mkLimit,
    lMax,
    lType,

    -- * ProtectionLimits
    ProtectionLimits (..),
    mkProtectionLimits,
    plProtectedResourceTypeLimits,

    -- * AttackVolume
    AttackVolume (..),
    mkAttackVolume,
    avBitsPerSecond,
    avPacketsPerSecond,
    avRequestsPerSecond,

    -- * ProactiveEngagementStatus
    ProactiveEngagementStatus (..),

    -- * AttackStatisticsDataItem
    AttackStatisticsDataItem (..),
    mkAttackStatisticsDataItem,
    asdiAttackCount,
    asdiAttackVolume,

    -- * SummarizedCounter
    SummarizedCounter (..),
    mkSummarizedCounter,
    scAverage,
    scMax,
    scN,
    scName,
    scSum,
    scUnit,

    -- * ContactNotes
    ContactNotes (..),

    -- * Subscription
    Subscription (..),
    mkSubscription,
    sSubscriptionLimits,
    sAutoRenew,
    sEndTime,
    sLimits,
    sProactiveEngagementStatus,
    sStartTime,
    sTimeCommitmentInSeconds,

    -- * Contributor
    Contributor (..),
    mkContributor,
    cName,
    cValue,

    -- * SubResourceType
    SubResourceType (..),

    -- * Unit
    Unit (..),

    -- * ProtectionGroupArbitraryPatternLimits
    ProtectionGroupArbitraryPatternLimits (..),
    mkProtectionGroupArbitraryPatternLimits,
    pgaplMaxMembers,

    -- * EmergencyContact
    EmergencyContact (..),
    mkEmergencyContact,
    ecEmailAddress,
    ecContactNotes,
    ecPhoneNumber,

    -- * RoleArn
    RoleArn (..),

    -- * NextToken
    NextToken (..),

    -- * Id
    Id (..),

    -- * Name
    Name (..),
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import Network.AWS.Shield.Types.AttackDetail
import Network.AWS.Shield.Types.AttackId
import Network.AWS.Shield.Types.AttackLayer
import Network.AWS.Shield.Types.AttackProperty
import Network.AWS.Shield.Types.AttackPropertyIdentifier
import Network.AWS.Shield.Types.AttackStatisticsDataItem
import Network.AWS.Shield.Types.AttackSummary
import Network.AWS.Shield.Types.AttackVectorDescription
import Network.AWS.Shield.Types.AttackVolume
import Network.AWS.Shield.Types.AttackVolumeStatistics
import Network.AWS.Shield.Types.AutoRenew
import Network.AWS.Shield.Types.ContactNotes
import Network.AWS.Shield.Types.Contributor
import Network.AWS.Shield.Types.EmailAddress
import Network.AWS.Shield.Types.EmergencyContact
import Network.AWS.Shield.Types.HealthCheckArn
import Network.AWS.Shield.Types.HealthCheckId
import Network.AWS.Shield.Types.Id
import Network.AWS.Shield.Types.Limit
import Network.AWS.Shield.Types.LogBucket
import Network.AWS.Shield.Types.Mitigation
import Network.AWS.Shield.Types.Name
import Network.AWS.Shield.Types.NextToken
import Network.AWS.Shield.Types.PhoneNumber
import Network.AWS.Shield.Types.ProactiveEngagementStatus
import Network.AWS.Shield.Types.ProtectedResourceType
import Network.AWS.Shield.Types.Protection
import Network.AWS.Shield.Types.ProtectionGroup
import Network.AWS.Shield.Types.ProtectionGroupAggregation
import Network.AWS.Shield.Types.ProtectionGroupArbitraryPatternLimits
import Network.AWS.Shield.Types.ProtectionGroupId
import Network.AWS.Shield.Types.ProtectionGroupLimits
import Network.AWS.Shield.Types.ProtectionGroupPattern
import Network.AWS.Shield.Types.ProtectionGroupPatternTypeLimits
import Network.AWS.Shield.Types.ProtectionId
import Network.AWS.Shield.Types.ProtectionLimits
import Network.AWS.Shield.Types.ResourceArn
import Network.AWS.Shield.Types.RoleArn
import Network.AWS.Shield.Types.String
import Network.AWS.Shield.Types.SubResourceSummary
import Network.AWS.Shield.Types.SubResourceType
import Network.AWS.Shield.Types.Subscription
import Network.AWS.Shield.Types.SubscriptionLimits
import Network.AWS.Shield.Types.SubscriptionState
import Network.AWS.Shield.Types.SummarizedAttackVector
import Network.AWS.Shield.Types.SummarizedCounter
import Network.AWS.Shield.Types.TimeRange
import Network.AWS.Shield.Types.Token
import Network.AWS.Shield.Types.Unit
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-06-02@ of the Amazon Shield SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "Shield",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "shield",
      Core._svcVersion = "2016-06-02",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "Shield",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | Exception that indicates that the resource is invalid. You might not have access to the resource, or the resource might not exist.
_InvalidResourceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidResourceException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidResourceException"
{-# DEPRECATED _InvalidResourceException "Use generic-lens or generic-optics instead." #-}

-- | Exception that indicates the specified @AttackId@ does not exist, or the requester does not have the appropriate permissions to access the @AttackId@ .
_AccessDeniedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError mkServiceConfig "AccessDeniedException"
{-# DEPRECATED _AccessDeniedException "Use generic-lens or generic-optics instead." #-}

-- | Exception that indicates that the parameters passed to the API are invalid. If available, this exception includes details in additional properties.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidParameterException"
{-# DEPRECATED _InvalidParameterException "Use generic-lens or generic-optics instead." #-}

-- | Exception that indicates that the operation would exceed a limit.
--
-- @Type@ is the type of limit that would be exceeded.
-- @Limit@ is the threshold that would be exceeded.
_LimitsExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitsExceededException =
  Core._MatchServiceError mkServiceConfig "LimitsExceededException"
{-# DEPRECATED _LimitsExceededException "Use generic-lens or generic-optics instead." #-}

-- | Exception that indicates that a problem occurred with the service infrastructure. You can retry the request.
_InternalErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalErrorException =
  Core._MatchServiceError mkServiceConfig "InternalErrorException"
{-# DEPRECATED _InternalErrorException "Use generic-lens or generic-optics instead." #-}

-- | Exception indicating the specified resource already exists. If available, this exception includes details in additional properties.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceAlreadyExistsException"
{-# DEPRECATED _ResourceAlreadyExistsException "Use generic-lens or generic-optics instead." #-}

-- | Exception that indicates that the resource state has been modified by another client. Retrieve the resource and then retry your request.
_OptimisticLockException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OptimisticLockException =
  Core._MatchServiceError mkServiceConfig "OptimisticLockException"
{-# DEPRECATED _OptimisticLockException "Use generic-lens or generic-optics instead." #-}

-- | The ARN of the role that you specifed does not exist.
_NoAssociatedRoleException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoAssociatedRoleException =
  Core._MatchServiceError
    mkServiceConfig
    "NoAssociatedRoleException"
{-# DEPRECATED _NoAssociatedRoleException "Use generic-lens or generic-optics instead." #-}

-- | In order to grant the necessary access to the DDoS Response Team (DRT), the user submitting the request must have the @iam:PassRole@ permission. This error indicates the user did not have the appropriate permissions. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use_passrole.html Granting a User Permissions to Pass a Role to an AWS Service> .
_AccessDeniedForDependencyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccessDeniedForDependencyException =
  Core._MatchServiceError
    mkServiceConfig
    "AccessDeniedForDependencyException"
{-# DEPRECATED _AccessDeniedForDependencyException "Use generic-lens or generic-optics instead." #-}

-- | Exception that indicates that the operation would not cause any change to occur.
_InvalidOperationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidOperationException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidOperationException"
{-# DEPRECATED _InvalidOperationException "Use generic-lens or generic-optics instead." #-}

-- | You are trying to update a subscription that has not yet completed the 1-year commitment. You can change the @AutoRenew@ parameter during the last 30 days of your subscription. This exception indicates that you are attempting to change @AutoRenew@ prior to that period.
_LockedSubscriptionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LockedSubscriptionException =
  Core._MatchServiceError
    mkServiceConfig
    "LockedSubscriptionException"
{-# DEPRECATED _LockedSubscriptionException "Use generic-lens or generic-optics instead." #-}

-- | Exception indicating the specified resource does not exist. If available, this exception includes details in additional properties.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceNotFoundException"
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | Exception that indicates that the NextToken specified in the request is invalid. Submit the request using the NextToken value that was returned in the response.
_InvalidPaginationTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidPaginationTokenException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidPaginationTokenException"
{-# DEPRECATED _InvalidPaginationTokenException "Use generic-lens or generic-optics instead." #-}
