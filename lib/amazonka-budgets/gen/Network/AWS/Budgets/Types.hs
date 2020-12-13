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
    budgetsService,

    -- * Errors

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
    mkAction,
    aStatus,
    aDefinition,
    aExecutionRoleARN,
    aActionId,
    aActionThreshold,
    aBudgetName,
    aNotificationType,
    aApprovalModel,
    aActionType,
    aSubscribers,

    -- * ActionHistory
    ActionHistory (..),
    mkActionHistory,
    ahStatus,
    ahEventType,
    ahActionHistoryDetails,
    ahTimestamp,

    -- * ActionHistoryDetails
    ActionHistoryDetails (..),
    mkActionHistoryDetails,
    ahdAction,
    ahdMessage,

    -- * ActionThreshold
    ActionThreshold (..),
    mkActionThreshold,
    atActionThresholdValue,
    atActionThresholdType,

    -- * Budget
    Budget (..),
    mkBudget,
    bCalculatedSpend,
    bPlannedBudgetLimits,
    bLastUpdatedTime,
    bBudgetLimit,
    bTimePeriod,
    bTimeUnit,
    bBudgetName,
    bBudgetType,
    bCostTypes,
    bCostFilters,

    -- * BudgetPerformanceHistory
    BudgetPerformanceHistory (..),
    mkBudgetPerformanceHistory,
    bphBudgetedAndActualAmountsList,
    bphTimeUnit,
    bphBudgetName,
    bphBudgetType,
    bphCostTypes,
    bphCostFilters,

    -- * BudgetedAndActualAmounts
    BudgetedAndActualAmounts (..),
    mkBudgetedAndActualAmounts,
    baaaTimePeriod,
    baaaActualAmount,
    baaaBudgetedAmount,

    -- * CalculatedSpend
    CalculatedSpend (..),
    mkCalculatedSpend,
    csForecastedSpend,
    csActualSpend,

    -- * CostTypes
    CostTypes (..),
    mkCostTypes,
    ctUseAmortized,
    ctIncludeRecurring,
    ctUseBlended,
    ctIncludeSupport,
    ctIncludeDiscount,
    ctIncludeSubscription,
    ctIncludeRefund,
    ctIncludeUpfront,
    ctIncludeOtherSubscription,
    ctIncludeTax,
    ctIncludeCredit,

    -- * Definition
    Definition (..),
    mkDefinition,
    dScpActionDefinition,
    dIAMActionDefinition,
    dSsmActionDefinition,

    -- * IAMActionDefinition
    IAMActionDefinition (..),
    mkIAMActionDefinition,
    iadGroups,
    iadRoles,
    iadUsers,
    iadPolicyARN,

    -- * Notification
    Notification (..),
    mkNotification,
    nThresholdType,
    nComparisonOperator,
    nThreshold,
    nNotificationState,
    nNotificationType,

    -- * NotificationWithSubscribers
    NotificationWithSubscribers (..),
    mkNotificationWithSubscribers,
    nwsNotification,
    nwsSubscribers,

    -- * ScpActionDefinition
    ScpActionDefinition (..),
    mkScpActionDefinition,
    sadPolicyId,
    sadTargetIds,

    -- * Spend
    Spend (..),
    mkSpend,
    sAmount,
    sUnit,

    -- * SsmActionDefinition
    SsmActionDefinition (..),
    mkSsmActionDefinition,
    sadActionSubType,
    sadInstanceIds,
    sadRegion,

    -- * Subscriber
    Subscriber (..),
    mkSubscriber,
    sSubscriptionType,
    sAddress,

    -- * TimePeriod
    TimePeriod (..),
    mkTimePeriod,
    tpStart,
    tpEnd,
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
import Network.AWS.Budgets.Types.IAMActionDefinition
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-10-20@ of the Amazon Budgets SDK configuration.
budgetsService :: Lude.Service
budgetsService =
  Lude.Service
    { Lude._svcAbbrev = "Budgets",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "budgets",
      Lude._svcVersion = "2016-10-20",
      Lude._svcEndpoint = Lude.defaultEndpoint budgetsService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "Budgets",
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
