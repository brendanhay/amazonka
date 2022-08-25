{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Budgets.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Budgets.Lens
  ( -- * Operations

    -- ** CreateBudget
    createBudget_notificationsWithSubscribers,
    createBudget_accountId,
    createBudget_budget,
    createBudgetResponse_httpStatus,

    -- ** CreateBudgetAction
    createBudgetAction_accountId,
    createBudgetAction_budgetName,
    createBudgetAction_notificationType,
    createBudgetAction_actionType,
    createBudgetAction_actionThreshold,
    createBudgetAction_definition,
    createBudgetAction_executionRoleArn,
    createBudgetAction_approvalModel,
    createBudgetAction_subscribers,
    createBudgetActionResponse_httpStatus,
    createBudgetActionResponse_accountId,
    createBudgetActionResponse_budgetName,
    createBudgetActionResponse_actionId,

    -- ** CreateNotification
    createNotification_accountId,
    createNotification_budgetName,
    createNotification_notification,
    createNotification_subscribers,
    createNotificationResponse_httpStatus,

    -- ** CreateSubscriber
    createSubscriber_accountId,
    createSubscriber_budgetName,
    createSubscriber_notification,
    createSubscriber_subscriber,
    createSubscriberResponse_httpStatus,

    -- ** DeleteBudget
    deleteBudget_accountId,
    deleteBudget_budgetName,
    deleteBudgetResponse_httpStatus,

    -- ** DeleteBudgetAction
    deleteBudgetAction_accountId,
    deleteBudgetAction_budgetName,
    deleteBudgetAction_actionId,
    deleteBudgetActionResponse_httpStatus,
    deleteBudgetActionResponse_accountId,
    deleteBudgetActionResponse_budgetName,
    deleteBudgetActionResponse_action,

    -- ** DeleteNotification
    deleteNotification_accountId,
    deleteNotification_budgetName,
    deleteNotification_notification,
    deleteNotificationResponse_httpStatus,

    -- ** DeleteSubscriber
    deleteSubscriber_accountId,
    deleteSubscriber_budgetName,
    deleteSubscriber_notification,
    deleteSubscriber_subscriber,
    deleteSubscriberResponse_httpStatus,

    -- ** DescribeBudget
    describeBudget_accountId,
    describeBudget_budgetName,
    describeBudgetResponse_budget,
    describeBudgetResponse_httpStatus,

    -- ** DescribeBudgetAction
    describeBudgetAction_accountId,
    describeBudgetAction_budgetName,
    describeBudgetAction_actionId,
    describeBudgetActionResponse_httpStatus,
    describeBudgetActionResponse_accountId,
    describeBudgetActionResponse_budgetName,
    describeBudgetActionResponse_action,

    -- ** DescribeBudgetActionHistories
    describeBudgetActionHistories_nextToken,
    describeBudgetActionHistories_maxResults,
    describeBudgetActionHistories_timePeriod,
    describeBudgetActionHistories_accountId,
    describeBudgetActionHistories_budgetName,
    describeBudgetActionHistories_actionId,
    describeBudgetActionHistoriesResponse_nextToken,
    describeBudgetActionHistoriesResponse_httpStatus,
    describeBudgetActionHistoriesResponse_actionHistories,

    -- ** DescribeBudgetActionsForAccount
    describeBudgetActionsForAccount_nextToken,
    describeBudgetActionsForAccount_maxResults,
    describeBudgetActionsForAccount_accountId,
    describeBudgetActionsForAccountResponse_nextToken,
    describeBudgetActionsForAccountResponse_httpStatus,
    describeBudgetActionsForAccountResponse_actions,

    -- ** DescribeBudgetActionsForBudget
    describeBudgetActionsForBudget_nextToken,
    describeBudgetActionsForBudget_maxResults,
    describeBudgetActionsForBudget_accountId,
    describeBudgetActionsForBudget_budgetName,
    describeBudgetActionsForBudgetResponse_nextToken,
    describeBudgetActionsForBudgetResponse_httpStatus,
    describeBudgetActionsForBudgetResponse_actions,

    -- ** DescribeBudgetNotificationsForAccount
    describeBudgetNotificationsForAccount_nextToken,
    describeBudgetNotificationsForAccount_maxResults,
    describeBudgetNotificationsForAccount_accountId,
    describeBudgetNotificationsForAccountResponse_nextToken,
    describeBudgetNotificationsForAccountResponse_budgetNotificationsForAccount,
    describeBudgetNotificationsForAccountResponse_httpStatus,

    -- ** DescribeBudgetPerformanceHistory
    describeBudgetPerformanceHistory_nextToken,
    describeBudgetPerformanceHistory_maxResults,
    describeBudgetPerformanceHistory_timePeriod,
    describeBudgetPerformanceHistory_accountId,
    describeBudgetPerformanceHistory_budgetName,
    describeBudgetPerformanceHistoryResponse_nextToken,
    describeBudgetPerformanceHistoryResponse_budgetPerformanceHistory,
    describeBudgetPerformanceHistoryResponse_httpStatus,

    -- ** DescribeBudgets
    describeBudgets_nextToken,
    describeBudgets_maxResults,
    describeBudgets_accountId,
    describeBudgetsResponse_nextToken,
    describeBudgetsResponse_budgets,
    describeBudgetsResponse_httpStatus,

    -- ** DescribeNotificationsForBudget
    describeNotificationsForBudget_nextToken,
    describeNotificationsForBudget_maxResults,
    describeNotificationsForBudget_accountId,
    describeNotificationsForBudget_budgetName,
    describeNotificationsForBudgetResponse_notifications,
    describeNotificationsForBudgetResponse_nextToken,
    describeNotificationsForBudgetResponse_httpStatus,

    -- ** DescribeSubscribersForNotification
    describeSubscribersForNotification_nextToken,
    describeSubscribersForNotification_maxResults,
    describeSubscribersForNotification_accountId,
    describeSubscribersForNotification_budgetName,
    describeSubscribersForNotification_notification,
    describeSubscribersForNotificationResponse_nextToken,
    describeSubscribersForNotificationResponse_subscribers,
    describeSubscribersForNotificationResponse_httpStatus,

    -- ** ExecuteBudgetAction
    executeBudgetAction_accountId,
    executeBudgetAction_budgetName,
    executeBudgetAction_actionId,
    executeBudgetAction_executionType,
    executeBudgetActionResponse_httpStatus,
    executeBudgetActionResponse_accountId,
    executeBudgetActionResponse_budgetName,
    executeBudgetActionResponse_actionId,
    executeBudgetActionResponse_executionType,

    -- ** UpdateBudget
    updateBudget_accountId,
    updateBudget_newBudget,
    updateBudgetResponse_httpStatus,

    -- ** UpdateBudgetAction
    updateBudgetAction_notificationType,
    updateBudgetAction_actionThreshold,
    updateBudgetAction_subscribers,
    updateBudgetAction_executionRoleArn,
    updateBudgetAction_approvalModel,
    updateBudgetAction_definition,
    updateBudgetAction_accountId,
    updateBudgetAction_budgetName,
    updateBudgetAction_actionId,
    updateBudgetActionResponse_httpStatus,
    updateBudgetActionResponse_accountId,
    updateBudgetActionResponse_budgetName,
    updateBudgetActionResponse_oldAction,
    updateBudgetActionResponse_newAction,

    -- ** UpdateNotification
    updateNotification_accountId,
    updateNotification_budgetName,
    updateNotification_oldNotification,
    updateNotification_newNotification,
    updateNotificationResponse_httpStatus,

    -- ** UpdateSubscriber
    updateSubscriber_accountId,
    updateSubscriber_budgetName,
    updateSubscriber_notification,
    updateSubscriber_oldSubscriber,
    updateSubscriber_newSubscriber,
    updateSubscriberResponse_httpStatus,

    -- * Types

    -- ** Action
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

    -- ** ActionHistory
    actionHistory_timestamp,
    actionHistory_status,
    actionHistory_eventType,
    actionHistory_actionHistoryDetails,

    -- ** ActionHistoryDetails
    actionHistoryDetails_message,
    actionHistoryDetails_action,

    -- ** ActionThreshold
    actionThreshold_actionThresholdValue,
    actionThreshold_actionThresholdType,

    -- ** AutoAdjustData
    autoAdjustData_lastAutoAdjustTime,
    autoAdjustData_historicalOptions,
    autoAdjustData_autoAdjustType,

    -- ** Budget
    budget_autoAdjustData,
    budget_budgetLimit,
    budget_plannedBudgetLimits,
    budget_costFilters,
    budget_lastUpdatedTime,
    budget_calculatedSpend,
    budget_timePeriod,
    budget_costTypes,
    budget_budgetName,
    budget_timeUnit,
    budget_budgetType,

    -- ** BudgetNotificationsForAccount
    budgetNotificationsForAccount_notifications,
    budgetNotificationsForAccount_budgetName,

    -- ** BudgetPerformanceHistory
    budgetPerformanceHistory_timeUnit,
    budgetPerformanceHistory_budgetedAndActualAmountsList,
    budgetPerformanceHistory_costFilters,
    budgetPerformanceHistory_budgetType,
    budgetPerformanceHistory_costTypes,
    budgetPerformanceHistory_budgetName,

    -- ** BudgetedAndActualAmounts
    budgetedAndActualAmounts_budgetedAmount,
    budgetedAndActualAmounts_timePeriod,
    budgetedAndActualAmounts_actualAmount,

    -- ** CalculatedSpend
    calculatedSpend_forecastedSpend,
    calculatedSpend_actualSpend,

    -- ** CostTypes
    costTypes_includeSupport,
    costTypes_includeDiscount,
    costTypes_useAmortized,
    costTypes_includeTax,
    costTypes_includeOtherSubscription,
    costTypes_includeRefund,
    costTypes_includeSubscription,
    costTypes_useBlended,
    costTypes_includeRecurring,
    costTypes_includeUpfront,
    costTypes_includeCredit,

    -- ** Definition
    definition_ssmActionDefinition,
    definition_scpActionDefinition,
    definition_iamActionDefinition,

    -- ** HistoricalOptions
    historicalOptions_lookBackAvailablePeriods,
    historicalOptions_budgetAdjustmentPeriod,

    -- ** IamActionDefinition
    iamActionDefinition_users,
    iamActionDefinition_roles,
    iamActionDefinition_groups,
    iamActionDefinition_policyArn,

    -- ** Notification
    notification_thresholdType,
    notification_notificationState,
    notification_notificationType,
    notification_comparisonOperator,
    notification_threshold,

    -- ** NotificationWithSubscribers
    notificationWithSubscribers_notification,
    notificationWithSubscribers_subscribers,

    -- ** ScpActionDefinition
    scpActionDefinition_policyId,
    scpActionDefinition_targetIds,

    -- ** Spend
    spend_amount,
    spend_unit,

    -- ** SsmActionDefinition
    ssmActionDefinition_actionSubType,
    ssmActionDefinition_region,
    ssmActionDefinition_instanceIds,

    -- ** Subscriber
    subscriber_subscriptionType,
    subscriber_address,

    -- ** TimePeriod
    timePeriod_start,
    timePeriod_end,
  )
where

import Amazonka.Budgets.CreateBudget
import Amazonka.Budgets.CreateBudgetAction
import Amazonka.Budgets.CreateNotification
import Amazonka.Budgets.CreateSubscriber
import Amazonka.Budgets.DeleteBudget
import Amazonka.Budgets.DeleteBudgetAction
import Amazonka.Budgets.DeleteNotification
import Amazonka.Budgets.DeleteSubscriber
import Amazonka.Budgets.DescribeBudget
import Amazonka.Budgets.DescribeBudgetAction
import Amazonka.Budgets.DescribeBudgetActionHistories
import Amazonka.Budgets.DescribeBudgetActionsForAccount
import Amazonka.Budgets.DescribeBudgetActionsForBudget
import Amazonka.Budgets.DescribeBudgetNotificationsForAccount
import Amazonka.Budgets.DescribeBudgetPerformanceHistory
import Amazonka.Budgets.DescribeBudgets
import Amazonka.Budgets.DescribeNotificationsForBudget
import Amazonka.Budgets.DescribeSubscribersForNotification
import Amazonka.Budgets.ExecuteBudgetAction
import Amazonka.Budgets.Types.Action
import Amazonka.Budgets.Types.ActionHistory
import Amazonka.Budgets.Types.ActionHistoryDetails
import Amazonka.Budgets.Types.ActionThreshold
import Amazonka.Budgets.Types.AutoAdjustData
import Amazonka.Budgets.Types.Budget
import Amazonka.Budgets.Types.BudgetNotificationsForAccount
import Amazonka.Budgets.Types.BudgetPerformanceHistory
import Amazonka.Budgets.Types.BudgetedAndActualAmounts
import Amazonka.Budgets.Types.CalculatedSpend
import Amazonka.Budgets.Types.CostTypes
import Amazonka.Budgets.Types.Definition
import Amazonka.Budgets.Types.HistoricalOptions
import Amazonka.Budgets.Types.IamActionDefinition
import Amazonka.Budgets.Types.Notification
import Amazonka.Budgets.Types.NotificationWithSubscribers
import Amazonka.Budgets.Types.ScpActionDefinition
import Amazonka.Budgets.Types.Spend
import Amazonka.Budgets.Types.SsmActionDefinition
import Amazonka.Budgets.Types.Subscriber
import Amazonka.Budgets.Types.TimePeriod
import Amazonka.Budgets.UpdateBudget
import Amazonka.Budgets.UpdateBudgetAction
import Amazonka.Budgets.UpdateNotification
import Amazonka.Budgets.UpdateSubscriber
