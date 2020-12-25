-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _AccessDeniedException,
    _InvalidParameterException,
    _InternalErrorException,
    _ExpiredNextTokenException,
    _NotFoundException,
    _ResourceLockedException,
    _InvalidNextTokenException,
    _DuplicateRecordException,
    _CreationLimitExceededException,

    -- * InstanceId
    InstanceId (..),

    -- * TargetId
    TargetId (..),

    -- * CalculatedSpend
    CalculatedSpend (..),
    mkCalculatedSpend,
    csActualSpend,
    csForecastedSpend,

    -- * Spend
    Spend (..),
    mkSpend,
    sAmount,
    sUnit,

    -- * Group
    Group (..),

    -- * BudgetPerformanceHistory
    BudgetPerformanceHistory (..),
    mkBudgetPerformanceHistory,
    bphBudgetName,
    bphBudgetType,
    bphBudgetedAndActualAmountsList,
    bphCostFilters,
    bphCostTypes,
    bphTimeUnit,

    -- * ThresholdType
    ThresholdType (..),

    -- * ExecutionType
    ExecutionType (..),

    -- * ActionSubType
    ActionSubType (..),

    -- * Subscriber
    Subscriber (..),
    mkSubscriber,
    sSubscriptionType,
    sAddress,

    -- * Definition
    Definition (..),
    mkDefinition,
    dIamActionDefinition,
    dScpActionDefinition,
    dSsmActionDefinition,

    -- * Notification
    Notification (..),
    mkNotification,
    nNotificationType,
    nComparisonOperator,
    nThreshold,
    nNotificationState,
    nThresholdType,

    -- * PolicyId
    PolicyId (..),

    -- * Budget
    Budget (..),
    mkBudget,
    bBudgetName,
    bTimeUnit,
    bBudgetType,
    bBudgetLimit,
    bCalculatedSpend,
    bCostFilters,
    bCostTypes,
    bLastUpdatedTime,
    bPlannedBudgetLimits,
    bTimePeriod,

    -- * TimePeriod
    TimePeriod (..),
    mkTimePeriod,
    tpEnd,
    tpStart,

    -- * ScpActionDefinition
    ScpActionDefinition (..),
    mkScpActionDefinition,
    sadPolicyId,
    sadTargetIds,

    -- * SubscriptionType
    SubscriptionType (..),

    -- * ActionId
    ActionId (..),

    -- * TimeUnit
    TimeUnit (..),

    -- * Action
    Action (..),
    mkAction,
    aActionId,
    aBudgetName,
    aNotificationType,
    aActionType,
    aActionThreshold,
    aDefinition,
    aExecutionRoleArn,
    aApprovalModel,
    aStatus,
    aSubscribers,

    -- * IamActionDefinition
    IamActionDefinition (..),
    mkIamActionDefinition,
    iadPolicyArn,
    iadGroups,
    iadRoles,
    iadUsers,

    -- * ComparisonOperator
    ComparisonOperator (..),

    -- * User
    User (..),

    -- * ActionThreshold
    ActionThreshold (..),
    mkActionThreshold,
    atActionThresholdValue,
    atActionThresholdType,

    -- * AccountId
    AccountId (..),

    -- * GenericString
    GenericString (..),

    -- * EventType
    EventType (..),

    -- * ActionHistory
    ActionHistory (..),
    mkActionHistory,
    ahTimestamp,
    ahStatus,
    ahEventType,
    ahActionHistoryDetails,

    -- * Role
    Role (..),

    -- * NotificationState
    NotificationState (..),

    -- * BudgetName
    BudgetName (..),

    -- * ActionStatus
    ActionStatus (..),

    -- * BudgetType
    BudgetType (..),

    -- * PolicyArn
    PolicyArn (..),

    -- * NotificationType
    NotificationType (..),

    -- * Region
    Region (..),

    -- * CostTypes
    CostTypes (..),
    mkCostTypes,
    ctIncludeCredit,
    ctIncludeDiscount,
    ctIncludeOtherSubscription,
    ctIncludeRecurring,
    ctIncludeRefund,
    ctIncludeSubscription,
    ctIncludeSupport,
    ctIncludeTax,
    ctIncludeUpfront,
    ctUseAmortized,
    ctUseBlended,

    -- * ApprovalModel
    ApprovalModel (..),

    -- * ActionType
    ActionType (..),

    -- * ActionHistoryDetails
    ActionHistoryDetails (..),
    mkActionHistoryDetails,
    ahdMessage,
    ahdAction,

    -- * SsmActionDefinition
    SsmActionDefinition (..),
    mkSsmActionDefinition,
    sadActionSubType,
    sadRegion,
    sadInstanceIds,

    -- * BudgetedAndActualAmounts
    BudgetedAndActualAmounts (..),
    mkBudgetedAndActualAmounts,
    baaaActualAmount,
    baaaBudgetedAmount,
    baaaTimePeriod,

    -- * NotificationWithSubscribers
    NotificationWithSubscribers (..),
    mkNotificationWithSubscribers,
    nwsNotification,
    nwsSubscribers,

    -- * NextToken
    NextToken (..),

    -- * Amount
    Amount (..),

    -- * Unit
    Unit (..),

    -- * Address
    Address (..),

    -- * ExecutionRoleArn
    ExecutionRoleArn (..),
  )
where

import Network.AWS.Budgets.Types.AccountId
import Network.AWS.Budgets.Types.Action
import Network.AWS.Budgets.Types.ActionHistory
import Network.AWS.Budgets.Types.ActionHistoryDetails
import Network.AWS.Budgets.Types.ActionId
import Network.AWS.Budgets.Types.ActionStatus
import Network.AWS.Budgets.Types.ActionSubType
import Network.AWS.Budgets.Types.ActionThreshold
import Network.AWS.Budgets.Types.ActionType
import Network.AWS.Budgets.Types.Address
import Network.AWS.Budgets.Types.Amount
import Network.AWS.Budgets.Types.ApprovalModel
import Network.AWS.Budgets.Types.Budget
import Network.AWS.Budgets.Types.BudgetName
import Network.AWS.Budgets.Types.BudgetPerformanceHistory
import Network.AWS.Budgets.Types.BudgetType
import Network.AWS.Budgets.Types.BudgetedAndActualAmounts
import Network.AWS.Budgets.Types.CalculatedSpend
import Network.AWS.Budgets.Types.ComparisonOperator
import Network.AWS.Budgets.Types.CostTypes
import Network.AWS.Budgets.Types.Definition
import Network.AWS.Budgets.Types.EventType
import Network.AWS.Budgets.Types.ExecutionRoleArn
import Network.AWS.Budgets.Types.ExecutionType
import Network.AWS.Budgets.Types.GenericString
import Network.AWS.Budgets.Types.Group
import Network.AWS.Budgets.Types.IamActionDefinition
import Network.AWS.Budgets.Types.InstanceId
import Network.AWS.Budgets.Types.NextToken
import Network.AWS.Budgets.Types.Notification
import Network.AWS.Budgets.Types.NotificationState
import Network.AWS.Budgets.Types.NotificationType
import Network.AWS.Budgets.Types.NotificationWithSubscribers
import Network.AWS.Budgets.Types.PolicyArn
import Network.AWS.Budgets.Types.PolicyId
import Network.AWS.Budgets.Types.Region
import Network.AWS.Budgets.Types.Role
import Network.AWS.Budgets.Types.ScpActionDefinition
import Network.AWS.Budgets.Types.Spend
import Network.AWS.Budgets.Types.SsmActionDefinition
import Network.AWS.Budgets.Types.Subscriber
import Network.AWS.Budgets.Types.SubscriptionType
import Network.AWS.Budgets.Types.TargetId
import Network.AWS.Budgets.Types.ThresholdType
import Network.AWS.Budgets.Types.TimePeriod
import Network.AWS.Budgets.Types.TimeUnit
import Network.AWS.Budgets.Types.Unit
import Network.AWS.Budgets.Types.User
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-10-20@ of the Amazon Budgets SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "Budgets",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "budgets",
      Core._svcVersion = "2016-10-20",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "Budgets",
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

-- | You are not authorized to use this operation with the given parameters.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError mkServiceConfig "AccessDeniedException"
{-# DEPRECATED _AccessDeniedException "Use generic-lens or generic-optics instead." #-}

-- | An error on the client occurred. Typically, the cause is an invalid input value.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidParameterException"
{-# DEPRECATED _InvalidParameterException "Use generic-lens or generic-optics instead." #-}

-- | An error on the server occurred during the processing of your request. Try again later.
_InternalErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalErrorException =
  Core._MatchServiceError mkServiceConfig "InternalErrorException"
{-# DEPRECATED _InternalErrorException "Use generic-lens or generic-optics instead." #-}

-- | The pagination token expired.
_ExpiredNextTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ExpiredNextTokenException =
  Core._MatchServiceError
    mkServiceConfig
    "ExpiredNextTokenException"
{-# DEPRECATED _ExpiredNextTokenException "Use generic-lens or generic-optics instead." #-}

-- | We can’t locate the resource that you specified.
_NotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError mkServiceConfig "NotFoundException"
{-# DEPRECATED _NotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The request was received and recognized by the server, but the server rejected that particular method for the requested resource.
_ResourceLockedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceLockedException =
  Core._MatchServiceError mkServiceConfig "ResourceLockedException"
{-# DEPRECATED _ResourceLockedException "Use generic-lens or generic-optics instead." #-}

-- | The pagination token is invalid.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidNextTokenException"
{-# DEPRECATED _InvalidNextTokenException "Use generic-lens or generic-optics instead." #-}

-- | The budget name already exists. Budget names must be unique within an account.
_DuplicateRecordException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateRecordException =
  Core._MatchServiceError
    mkServiceConfig
    "DuplicateRecordException"
{-# DEPRECATED _DuplicateRecordException "Use generic-lens or generic-optics instead." #-}

-- | You've exceeded the notification or subscriber limit.
_CreationLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CreationLimitExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "CreationLimitExceededException"
{-# DEPRECATED _CreationLimitExceededException "Use generic-lens or generic-optics instead." #-}
