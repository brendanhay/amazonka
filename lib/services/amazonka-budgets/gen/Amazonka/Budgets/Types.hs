{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Budgets.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Budgets.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _CreationLimitExceededException,
    _DuplicateRecordException,
    _ExpiredNextTokenException,
    _InternalErrorException,
    _InvalidNextTokenException,
    _InvalidParameterException,
    _NotFoundException,
    _ResourceLockedException,
    _ThrottlingException,

    -- * ActionStatus
    ActionStatus (..),

    -- * ActionSubType
    ActionSubType (..),

    -- * ActionType
    ActionType (..),

    -- * ApprovalModel
    ApprovalModel (..),

    -- * AutoAdjustType
    AutoAdjustType (..),

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

    -- * AutoAdjustData
    AutoAdjustData (..),
    newAutoAdjustData,
    autoAdjustData_historicalOptions,
    autoAdjustData_lastAutoAdjustTime,
    autoAdjustData_autoAdjustType,

    -- * Budget
    Budget (..),
    newBudget,
    budget_autoAdjustData,
    budget_budgetLimit,
    budget_calculatedSpend,
    budget_costFilters,
    budget_costTypes,
    budget_lastUpdatedTime,
    budget_plannedBudgetLimits,
    budget_timePeriod,
    budget_budgetName,
    budget_timeUnit,
    budget_budgetType,

    -- * BudgetNotificationsForAccount
    BudgetNotificationsForAccount (..),
    newBudgetNotificationsForAccount,
    budgetNotificationsForAccount_budgetName,
    budgetNotificationsForAccount_notifications,

    -- * BudgetPerformanceHistory
    BudgetPerformanceHistory (..),
    newBudgetPerformanceHistory,
    budgetPerformanceHistory_budgetName,
    budgetPerformanceHistory_budgetType,
    budgetPerformanceHistory_budgetedAndActualAmountsList,
    budgetPerformanceHistory_costFilters,
    budgetPerformanceHistory_costTypes,
    budgetPerformanceHistory_timeUnit,

    -- * BudgetedAndActualAmounts
    BudgetedAndActualAmounts (..),
    newBudgetedAndActualAmounts,
    budgetedAndActualAmounts_actualAmount,
    budgetedAndActualAmounts_budgetedAmount,
    budgetedAndActualAmounts_timePeriod,

    -- * CalculatedSpend
    CalculatedSpend (..),
    newCalculatedSpend,
    calculatedSpend_forecastedSpend,
    calculatedSpend_actualSpend,

    -- * CostTypes
    CostTypes (..),
    newCostTypes,
    costTypes_includeCredit,
    costTypes_includeDiscount,
    costTypes_includeOtherSubscription,
    costTypes_includeRecurring,
    costTypes_includeRefund,
    costTypes_includeSubscription,
    costTypes_includeSupport,
    costTypes_includeTax,
    costTypes_includeUpfront,
    costTypes_useAmortized,
    costTypes_useBlended,

    -- * Definition
    Definition (..),
    newDefinition,
    definition_iamActionDefinition,
    definition_scpActionDefinition,
    definition_ssmActionDefinition,

    -- * HistoricalOptions
    HistoricalOptions (..),
    newHistoricalOptions,
    historicalOptions_lookBackAvailablePeriods,
    historicalOptions_budgetAdjustmentPeriod,

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

import Amazonka.Budgets.Types.Action
import Amazonka.Budgets.Types.ActionHistory
import Amazonka.Budgets.Types.ActionHistoryDetails
import Amazonka.Budgets.Types.ActionStatus
import Amazonka.Budgets.Types.ActionSubType
import Amazonka.Budgets.Types.ActionThreshold
import Amazonka.Budgets.Types.ActionType
import Amazonka.Budgets.Types.ApprovalModel
import Amazonka.Budgets.Types.AutoAdjustData
import Amazonka.Budgets.Types.AutoAdjustType
import Amazonka.Budgets.Types.Budget
import Amazonka.Budgets.Types.BudgetNotificationsForAccount
import Amazonka.Budgets.Types.BudgetPerformanceHistory
import Amazonka.Budgets.Types.BudgetType
import Amazonka.Budgets.Types.BudgetedAndActualAmounts
import Amazonka.Budgets.Types.CalculatedSpend
import Amazonka.Budgets.Types.ComparisonOperator
import Amazonka.Budgets.Types.CostTypes
import Amazonka.Budgets.Types.Definition
import Amazonka.Budgets.Types.EventType
import Amazonka.Budgets.Types.ExecutionType
import Amazonka.Budgets.Types.HistoricalOptions
import Amazonka.Budgets.Types.IamActionDefinition
import Amazonka.Budgets.Types.Notification
import Amazonka.Budgets.Types.NotificationState
import Amazonka.Budgets.Types.NotificationType
import Amazonka.Budgets.Types.NotificationWithSubscribers
import Amazonka.Budgets.Types.ScpActionDefinition
import Amazonka.Budgets.Types.Spend
import Amazonka.Budgets.Types.SsmActionDefinition
import Amazonka.Budgets.Types.Subscriber
import Amazonka.Budgets.Types.SubscriptionType
import Amazonka.Budgets.Types.ThresholdType
import Amazonka.Budgets.Types.TimePeriod
import Amazonka.Budgets.Types.TimeUnit
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2016-10-20@ of the Amazon Budgets SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Budgets",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "budgets",
      Core.signingName = "budgets",
      Core.version = "2016-10-20",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Budgets",
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

-- | You are not authorized to use this operation with the given parameters.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | You\'ve exceeded the notification or subscriber limit.
_CreationLimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CreationLimitExceededException =
  Core._MatchServiceError
    defaultService
    "CreationLimitExceededException"

-- | The budget name already exists. Budget names must be unique within an
-- account.
_DuplicateRecordException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DuplicateRecordException =
  Core._MatchServiceError
    defaultService
    "DuplicateRecordException"

-- | The pagination token expired.
_ExpiredNextTokenException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ExpiredNextTokenException =
  Core._MatchServiceError
    defaultService
    "ExpiredNextTokenException"

-- | An error on the server occurred during the processing of your request.
-- Try again later.
_InternalErrorException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalErrorException =
  Core._MatchServiceError
    defaultService
    "InternalErrorException"

-- | The pagination token is invalid.
_InvalidNextTokenException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | An error on the client occurred. Typically, the cause is an invalid
-- input value.
_InvalidParameterException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | We can’t locate the resource that you specified.
_NotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"

-- | The request was received and recognized by the server, but the server
-- rejected that particular method for the requested resource.
_ResourceLockedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceLockedException =
  Core._MatchServiceError
    defaultService
    "ResourceLockedException"

-- | The number of API requests has exceeded the maximum allowed API request
-- throttling limit for the account.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
