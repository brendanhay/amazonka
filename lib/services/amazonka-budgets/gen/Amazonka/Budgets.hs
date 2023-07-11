{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Budgets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2016-10-20@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Use the Amazon Web Services Budgets API to plan your service usage,
-- service costs, and instance reservations. This API reference provides
-- descriptions, syntax, and usage examples for each of the actions and
-- data types for the Amazon Web Services Budgets feature.
--
-- Budgets provide you with a way to see the following information:
--
-- -   How close your plan is to your budgeted amount or to the free tier
--     limits
--
-- -   Your usage-to-date, including how much you\'ve used of your Reserved
--     Instances (RIs)
--
-- -   Your current estimated charges from Amazon Web Services, and how
--     much your predicted usage will accrue in charges by the end of the
--     month
--
-- -   How much of your budget has been used
--
-- Amazon Web Services updates your budget status several times a day.
-- Budgets track your unblended costs, subscriptions, refunds, and RIs. You
-- can create the following types of budgets:
--
-- -   __Cost budgets__ - Plan how much you want to spend on a service.
--
-- -   __Usage budgets__ - Plan how much you want to use one or more
--     services.
--
-- -   __RI utilization budgets__ - Define a utilization threshold, and
--     receive alerts when your RI usage falls below that threshold. This
--     lets you see if your RIs are unused or under-utilized.
--
-- -   __RI coverage budgets__ - Define a coverage threshold, and receive
--     alerts when the number of your instance hours that are covered by
--     RIs fall below that threshold. This lets you see how much of your
--     instance usage is covered by a reservation.
--
-- Service Endpoint
--
-- The Amazon Web Services Budgets API provides the following endpoint:
--
-- -   https:\/\/budgets.amazonaws.com
--
-- For information about costs that are associated with the Amazon Web
-- Services Budgets API, see
-- <https://aws.amazon.com/aws-cost-management/pricing/ Amazon Web Services Cost Management Pricing>.
module Amazonka.Budgets
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** CreationLimitExceededException
    _CreationLimitExceededException,

    -- ** DuplicateRecordException
    _DuplicateRecordException,

    -- ** ExpiredNextTokenException
    _ExpiredNextTokenException,

    -- ** InternalErrorException
    _InternalErrorException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** ResourceLockedException
    _ResourceLockedException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateBudget
    CreateBudget (CreateBudget'),
    newCreateBudget,
    CreateBudgetResponse (CreateBudgetResponse'),
    newCreateBudgetResponse,

    -- ** CreateBudgetAction
    CreateBudgetAction (CreateBudgetAction'),
    newCreateBudgetAction,
    CreateBudgetActionResponse (CreateBudgetActionResponse'),
    newCreateBudgetActionResponse,

    -- ** CreateNotification
    CreateNotification (CreateNotification'),
    newCreateNotification,
    CreateNotificationResponse (CreateNotificationResponse'),
    newCreateNotificationResponse,

    -- ** CreateSubscriber
    CreateSubscriber (CreateSubscriber'),
    newCreateSubscriber,
    CreateSubscriberResponse (CreateSubscriberResponse'),
    newCreateSubscriberResponse,

    -- ** DeleteBudget
    DeleteBudget (DeleteBudget'),
    newDeleteBudget,
    DeleteBudgetResponse (DeleteBudgetResponse'),
    newDeleteBudgetResponse,

    -- ** DeleteBudgetAction
    DeleteBudgetAction (DeleteBudgetAction'),
    newDeleteBudgetAction,
    DeleteBudgetActionResponse (DeleteBudgetActionResponse'),
    newDeleteBudgetActionResponse,

    -- ** DeleteNotification
    DeleteNotification (DeleteNotification'),
    newDeleteNotification,
    DeleteNotificationResponse (DeleteNotificationResponse'),
    newDeleteNotificationResponse,

    -- ** DeleteSubscriber
    DeleteSubscriber (DeleteSubscriber'),
    newDeleteSubscriber,
    DeleteSubscriberResponse (DeleteSubscriberResponse'),
    newDeleteSubscriberResponse,

    -- ** DescribeBudget
    DescribeBudget (DescribeBudget'),
    newDescribeBudget,
    DescribeBudgetResponse (DescribeBudgetResponse'),
    newDescribeBudgetResponse,

    -- ** DescribeBudgetAction
    DescribeBudgetAction (DescribeBudgetAction'),
    newDescribeBudgetAction,
    DescribeBudgetActionResponse (DescribeBudgetActionResponse'),
    newDescribeBudgetActionResponse,

    -- ** DescribeBudgetActionHistories (Paginated)
    DescribeBudgetActionHistories (DescribeBudgetActionHistories'),
    newDescribeBudgetActionHistories,
    DescribeBudgetActionHistoriesResponse (DescribeBudgetActionHistoriesResponse'),
    newDescribeBudgetActionHistoriesResponse,

    -- ** DescribeBudgetActionsForAccount (Paginated)
    DescribeBudgetActionsForAccount (DescribeBudgetActionsForAccount'),
    newDescribeBudgetActionsForAccount,
    DescribeBudgetActionsForAccountResponse (DescribeBudgetActionsForAccountResponse'),
    newDescribeBudgetActionsForAccountResponse,

    -- ** DescribeBudgetActionsForBudget (Paginated)
    DescribeBudgetActionsForBudget (DescribeBudgetActionsForBudget'),
    newDescribeBudgetActionsForBudget,
    DescribeBudgetActionsForBudgetResponse (DescribeBudgetActionsForBudgetResponse'),
    newDescribeBudgetActionsForBudgetResponse,

    -- ** DescribeBudgetNotificationsForAccount (Paginated)
    DescribeBudgetNotificationsForAccount (DescribeBudgetNotificationsForAccount'),
    newDescribeBudgetNotificationsForAccount,
    DescribeBudgetNotificationsForAccountResponse (DescribeBudgetNotificationsForAccountResponse'),
    newDescribeBudgetNotificationsForAccountResponse,

    -- ** DescribeBudgetPerformanceHistory (Paginated)
    DescribeBudgetPerformanceHistory (DescribeBudgetPerformanceHistory'),
    newDescribeBudgetPerformanceHistory,
    DescribeBudgetPerformanceHistoryResponse (DescribeBudgetPerformanceHistoryResponse'),
    newDescribeBudgetPerformanceHistoryResponse,

    -- ** DescribeBudgets (Paginated)
    DescribeBudgets (DescribeBudgets'),
    newDescribeBudgets,
    DescribeBudgetsResponse (DescribeBudgetsResponse'),
    newDescribeBudgetsResponse,

    -- ** DescribeNotificationsForBudget (Paginated)
    DescribeNotificationsForBudget (DescribeNotificationsForBudget'),
    newDescribeNotificationsForBudget,
    DescribeNotificationsForBudgetResponse (DescribeNotificationsForBudgetResponse'),
    newDescribeNotificationsForBudgetResponse,

    -- ** DescribeSubscribersForNotification (Paginated)
    DescribeSubscribersForNotification (DescribeSubscribersForNotification'),
    newDescribeSubscribersForNotification,
    DescribeSubscribersForNotificationResponse (DescribeSubscribersForNotificationResponse'),
    newDescribeSubscribersForNotificationResponse,

    -- ** ExecuteBudgetAction
    ExecuteBudgetAction (ExecuteBudgetAction'),
    newExecuteBudgetAction,
    ExecuteBudgetActionResponse (ExecuteBudgetActionResponse'),
    newExecuteBudgetActionResponse,

    -- ** UpdateBudget
    UpdateBudget (UpdateBudget'),
    newUpdateBudget,
    UpdateBudgetResponse (UpdateBudgetResponse'),
    newUpdateBudgetResponse,

    -- ** UpdateBudgetAction
    UpdateBudgetAction (UpdateBudgetAction'),
    newUpdateBudgetAction,
    UpdateBudgetActionResponse (UpdateBudgetActionResponse'),
    newUpdateBudgetActionResponse,

    -- ** UpdateNotification
    UpdateNotification (UpdateNotification'),
    newUpdateNotification,
    UpdateNotificationResponse (UpdateNotificationResponse'),
    newUpdateNotificationResponse,

    -- ** UpdateSubscriber
    UpdateSubscriber (UpdateSubscriber'),
    newUpdateSubscriber,
    UpdateSubscriberResponse (UpdateSubscriberResponse'),
    newUpdateSubscriberResponse,

    -- * Types

    -- ** ActionStatus
    ActionStatus (..),

    -- ** ActionSubType
    ActionSubType (..),

    -- ** ActionType
    ActionType (..),

    -- ** ApprovalModel
    ApprovalModel (..),

    -- ** AutoAdjustType
    AutoAdjustType (..),

    -- ** BudgetType
    BudgetType (..),

    -- ** ComparisonOperator
    ComparisonOperator (..),

    -- ** EventType
    EventType (..),

    -- ** ExecutionType
    ExecutionType (..),

    -- ** NotificationState
    NotificationState (..),

    -- ** NotificationType
    NotificationType (..),

    -- ** SubscriptionType
    SubscriptionType (..),

    -- ** ThresholdType
    ThresholdType (..),

    -- ** TimeUnit
    TimeUnit (..),

    -- ** Action
    Action (Action'),
    newAction,

    -- ** ActionHistory
    ActionHistory (ActionHistory'),
    newActionHistory,

    -- ** ActionHistoryDetails
    ActionHistoryDetails (ActionHistoryDetails'),
    newActionHistoryDetails,

    -- ** ActionThreshold
    ActionThreshold (ActionThreshold'),
    newActionThreshold,

    -- ** AutoAdjustData
    AutoAdjustData (AutoAdjustData'),
    newAutoAdjustData,

    -- ** Budget
    Budget (Budget'),
    newBudget,

    -- ** BudgetNotificationsForAccount
    BudgetNotificationsForAccount (BudgetNotificationsForAccount'),
    newBudgetNotificationsForAccount,

    -- ** BudgetPerformanceHistory
    BudgetPerformanceHistory (BudgetPerformanceHistory'),
    newBudgetPerformanceHistory,

    -- ** BudgetedAndActualAmounts
    BudgetedAndActualAmounts (BudgetedAndActualAmounts'),
    newBudgetedAndActualAmounts,

    -- ** CalculatedSpend
    CalculatedSpend (CalculatedSpend'),
    newCalculatedSpend,

    -- ** CostTypes
    CostTypes (CostTypes'),
    newCostTypes,

    -- ** Definition
    Definition (Definition'),
    newDefinition,

    -- ** HistoricalOptions
    HistoricalOptions (HistoricalOptions'),
    newHistoricalOptions,

    -- ** IamActionDefinition
    IamActionDefinition (IamActionDefinition'),
    newIamActionDefinition,

    -- ** Notification
    Notification (Notification'),
    newNotification,

    -- ** NotificationWithSubscribers
    NotificationWithSubscribers (NotificationWithSubscribers'),
    newNotificationWithSubscribers,

    -- ** ScpActionDefinition
    ScpActionDefinition (ScpActionDefinition'),
    newScpActionDefinition,

    -- ** Spend
    Spend (Spend'),
    newSpend,

    -- ** SsmActionDefinition
    SsmActionDefinition (SsmActionDefinition'),
    newSsmActionDefinition,

    -- ** Subscriber
    Subscriber (Subscriber'),
    newSubscriber,

    -- ** TimePeriod
    TimePeriod (TimePeriod'),
    newTimePeriod,
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
import Amazonka.Budgets.Lens
import Amazonka.Budgets.Types
import Amazonka.Budgets.UpdateBudget
import Amazonka.Budgets.UpdateBudgetAction
import Amazonka.Budgets.UpdateNotification
import Amazonka.Budgets.UpdateSubscriber
import Amazonka.Budgets.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Budgets'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
