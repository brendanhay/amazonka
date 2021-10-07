{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ResourceLockedException,
    _ExpiredNextTokenException,
    _NotFoundException,
    _InternalErrorException,
    _DuplicateRecordException,
    _InvalidNextTokenException,
    _InvalidParameterException,
    _AccessDeniedException,
    _CreationLimitExceededException,

    -- * ActionStatus
    ActionStatus (..),

    -- * ActionSubType
    ActionSubType (..),

    -- * ActionType
    ActionType (..),

    -- * ApprovalModel
    ApprovalModel (..),

    -- * BudgetType
    BudgetType (..),

    -- * ComparisonOperator
    ComparisonOperator (..),

    -- * EventType
    EventType (..),

    -- * ExecutionType
    ExecutionType (..),

    -- * NotificationState
    NotificationState (..),

    -- * NotificationType
    NotificationType (..),

    -- * SubscriptionType
    SubscriptionType (..),

    -- * ThresholdType
    ThresholdType (..),

    -- * TimeUnit
    TimeUnit (..),

    -- * Action
    Action (..),
    newAction,
    action_actionId,
    action_budgetName,
    action_notificationType,
    action_actionType,
    action_actionThreshold,
    action_definition,
    action_executionRoleArn,
    action_approvalModel,
    action_status,
    action_subscribers,

    -- * ActionHistory
    ActionHistory (..),
    newActionHistory,
    actionHistory_timestamp,
    actionHistory_status,
    actionHistory_eventType,
    actionHistory_actionHistoryDetails,

    -- * ActionHistoryDetails
    ActionHistoryDetails (..),
    newActionHistoryDetails,
    actionHistoryDetails_message,
    actionHistoryDetails_action,

    -- * ActionThreshold
    ActionThreshold (..),
    newActionThreshold,
    actionThreshold_actionThresholdValue,
    actionThreshold_actionThresholdType,

    -- * Budget
    Budget (..),
    newBudget,
    budget_timePeriod,
    budget_costFilters,
    budget_costTypes,
    budget_plannedBudgetLimits,
    budget_calculatedSpend,
    budget_budgetLimit,
    budget_lastUpdatedTime,
    budget_budgetName,
    budget_timeUnit,
    budget_budgetType,

    -- * BudgetPerformanceHistory
    BudgetPerformanceHistory (..),
    newBudgetPerformanceHistory,
    budgetPerformanceHistory_budgetedAndActualAmountsList,
    budgetPerformanceHistory_timeUnit,
    budgetPerformanceHistory_costFilters,
    budgetPerformanceHistory_costTypes,
    budgetPerformanceHistory_budgetType,
    budgetPerformanceHistory_budgetName,

    -- * BudgetedAndActualAmounts
    BudgetedAndActualAmounts (..),
    newBudgetedAndActualAmounts,
    budgetedAndActualAmounts_timePeriod,
    budgetedAndActualAmounts_budgetedAmount,
    budgetedAndActualAmounts_actualAmount,

    -- * CalculatedSpend
    CalculatedSpend (..),
    newCalculatedSpend,
    calculatedSpend_forecastedSpend,
    calculatedSpend_actualSpend,

    -- * CostTypes
    CostTypes (..),
    newCostTypes,
    costTypes_includeSubscription,
    costTypes_useAmortized,
    costTypes_includeCredit,
    costTypes_useBlended,
    costTypes_includeSupport,
    costTypes_includeRefund,
    costTypes_includeTax,
    costTypes_includeDiscount,
    costTypes_includeOtherSubscription,
    costTypes_includeUpfront,
    costTypes_includeRecurring,

    -- * Definition
    Definition (..),
    newDefinition,
    definition_iamActionDefinition,
    definition_ssmActionDefinition,
    definition_scpActionDefinition,

    -- * IamActionDefinition
    IamActionDefinition (..),
    newIamActionDefinition,
    iamActionDefinition_groups,
    iamActionDefinition_roles,
    iamActionDefinition_users,
    iamActionDefinition_policyArn,

    -- * Notification
    Notification (..),
    newNotification,
    notification_notificationState,
    notification_thresholdType,
    notification_notificationType,
    notification_comparisonOperator,
    notification_threshold,

    -- * NotificationWithSubscribers
    NotificationWithSubscribers (..),
    newNotificationWithSubscribers,
    notificationWithSubscribers_notification,
    notificationWithSubscribers_subscribers,

    -- * ScpActionDefinition
    ScpActionDefinition (..),
    newScpActionDefinition,
    scpActionDefinition_policyId,
    scpActionDefinition_targetIds,

    -- * Spend
    Spend (..),
    newSpend,
    spend_amount,
    spend_unit,

    -- * SsmActionDefinition
    SsmActionDefinition (..),
    newSsmActionDefinition,
    ssmActionDefinition_actionSubType,
    ssmActionDefinition_region,
    ssmActionDefinition_instanceIds,

    -- * Subscriber
    Subscriber (..),
    newSubscriber,
    subscriber_subscriptionType,
    subscriber_address,

    -- * TimePeriod
    TimePeriod (..),
    newTimePeriod,
    timePeriod_end,
    timePeriod_start,
  )
where

import Network.AWS.Budgets.Types.Action
import Network.AWS.Budgets.Types.ActionHistory
import Network.AWS.Budgets.Types.ActionHistoryDetails
import Network.AWS.Budgets.Types.ActionStatus
import Network.AWS.Budgets.Types.ActionSubType
import Network.AWS.Budgets.Types.ActionThreshold
import Network.AWS.Budgets.Types.ActionType
import Network.AWS.Budgets.Types.ApprovalModel
import Network.AWS.Budgets.Types.Budget
import Network.AWS.Budgets.Types.BudgetPerformanceHistory
import Network.AWS.Budgets.Types.BudgetType
import Network.AWS.Budgets.Types.BudgetedAndActualAmounts
import Network.AWS.Budgets.Types.CalculatedSpend
import Network.AWS.Budgets.Types.ComparisonOperator
import Network.AWS.Budgets.Types.CostTypes
import Network.AWS.Budgets.Types.Definition
import Network.AWS.Budgets.Types.EventType
import Network.AWS.Budgets.Types.ExecutionType
import Network.AWS.Budgets.Types.IamActionDefinition
import Network.AWS.Budgets.Types.Notification
import Network.AWS.Budgets.Types.NotificationState
import Network.AWS.Budgets.Types.NotificationType
import Network.AWS.Budgets.Types.NotificationWithSubscribers
import Network.AWS.Budgets.Types.ScpActionDefinition
import Network.AWS.Budgets.Types.Spend
import Network.AWS.Budgets.Types.SsmActionDefinition
import Network.AWS.Budgets.Types.Subscriber
import Network.AWS.Budgets.Types.SubscriptionType
import Network.AWS.Budgets.Types.ThresholdType
import Network.AWS.Budgets.Types.TimePeriod
import Network.AWS.Budgets.Types.TimeUnit
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-10-20@ of the Amazon Budgets SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Budgets",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "budgets",
      Core._serviceSigningName = "budgets",
      Core._serviceVersion = "2016-10-20",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "Budgets",
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
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
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
      | Prelude.otherwise = Prelude.Nothing

-- | The request was received and recognized by the server, but the server
-- rejected that particular method for the requested resource.
_ResourceLockedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceLockedException =
  Core._MatchServiceError
    defaultService
    "ResourceLockedException"

-- | The pagination token expired.
_ExpiredNextTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ExpiredNextTokenException =
  Core._MatchServiceError
    defaultService
    "ExpiredNextTokenException"

-- | We can’t locate the resource that you specified.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"

-- | An error on the server occurred during the processing of your request.
-- Try again later.
_InternalErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalErrorException =
  Core._MatchServiceError
    defaultService
    "InternalErrorException"

-- | The budget name already exists. Budget names must be unique within an
-- account.
_DuplicateRecordException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateRecordException =
  Core._MatchServiceError
    defaultService
    "DuplicateRecordException"

-- | The pagination token is invalid.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | An error on the client occurred. Typically, the cause is an invalid
-- input value.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | You are not authorized to use this operation with the given parameters.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | You\'ve exceeded the notification or subscriber limit.
_CreationLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CreationLimitExceededException =
  Core._MatchServiceError
    defaultService
    "CreationLimitExceededException"
