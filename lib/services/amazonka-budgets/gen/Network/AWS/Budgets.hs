{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.Budgets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2016-10-20@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- The AWS Budgets API enables you to use AWS Budgets to plan your service
-- usage, service costs, and instance reservations. The API reference
-- provides descriptions, syntax, and usage examples for each of the
-- actions and data types for AWS Budgets.
--
-- Budgets provide you with a way to see the following information:
--
-- -   How close your plan is to your budgeted amount or to the free tier
--     limits
--
-- -   Your usage-to-date, including how much you\'ve used of your Reserved
--     Instances (RIs)
--
-- -   Your current estimated charges from AWS, and how much your predicted
--     usage will accrue in charges by the end of the month
--
-- -   How much of your budget has been used
--
-- AWS updates your budget status several times a day. Budgets track your
-- unblended costs, subscriptions, refunds, and RIs. You can create the
-- following types of budgets:
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
-- The AWS Budgets API provides the following endpoint:
--
-- -   https:\/\/budgets.amazonaws.com
--
-- For information about costs that are associated with the AWS Budgets
-- API, see
-- <https://aws.amazon.com/aws-cost-management/pricing/ AWS Cost Management Pricing>.
module Network.AWS.Budgets
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** InternalErrorException
    _InternalErrorException,

    -- ** ExpiredNextTokenException
    _ExpiredNextTokenException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** ResourceLockedException
    _ResourceLockedException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** DuplicateRecordException
    _DuplicateRecordException,

    -- ** CreationLimitExceededException
    _CreationLimitExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateBudgetAction
    CreateBudgetAction (CreateBudgetAction'),
    newCreateBudgetAction,
    CreateBudgetActionResponse (CreateBudgetActionResponse'),
    newCreateBudgetActionResponse,

    -- ** UpdateBudgetAction
    UpdateBudgetAction (UpdateBudgetAction'),
    newUpdateBudgetAction,
    UpdateBudgetActionResponse (UpdateBudgetActionResponse'),
    newUpdateBudgetActionResponse,

    -- ** DeleteBudgetAction
    DeleteBudgetAction (DeleteBudgetAction'),
    newDeleteBudgetAction,
    DeleteBudgetActionResponse (DeleteBudgetActionResponse'),
    newDeleteBudgetActionResponse,

    -- ** DescribeSubscribersForNotification (Paginated)
    DescribeSubscribersForNotification (DescribeSubscribersForNotification'),
    newDescribeSubscribersForNotification,
    DescribeSubscribersForNotificationResponse (DescribeSubscribersForNotificationResponse'),
    newDescribeSubscribersForNotificationResponse,

    -- ** DescribeNotificationsForBudget (Paginated)
    DescribeNotificationsForBudget (DescribeNotificationsForBudget'),
    newDescribeNotificationsForBudget,
    DescribeNotificationsForBudgetResponse (DescribeNotificationsForBudgetResponse'),
    newDescribeNotificationsForBudgetResponse,

    -- ** DescribeBudgets (Paginated)
    DescribeBudgets (DescribeBudgets'),
    newDescribeBudgets,
    DescribeBudgetsResponse (DescribeBudgetsResponse'),
    newDescribeBudgetsResponse,

    -- ** CreateSubscriber
    CreateSubscriber (CreateSubscriber'),
    newCreateSubscriber,
    CreateSubscriberResponse (CreateSubscriberResponse'),
    newCreateSubscriberResponse,

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

    -- ** DeleteBudget
    DeleteBudget (DeleteBudget'),
    newDeleteBudget,
    DeleteBudgetResponse (DeleteBudgetResponse'),
    newDeleteBudgetResponse,

    -- ** DeleteNotification
    DeleteNotification (DeleteNotification'),
    newDeleteNotification,
    DeleteNotificationResponse (DeleteNotificationResponse'),
    newDeleteNotificationResponse,

    -- ** UpdateNotification
    UpdateNotification (UpdateNotification'),
    newUpdateNotification,
    UpdateNotificationResponse (UpdateNotificationResponse'),
    newUpdateNotificationResponse,

    -- ** DescribeBudgetActionsForAccount (Paginated)
    DescribeBudgetActionsForAccount (DescribeBudgetActionsForAccount'),
    newDescribeBudgetActionsForAccount,
    DescribeBudgetActionsForAccountResponse (DescribeBudgetActionsForAccountResponse'),
    newDescribeBudgetActionsForAccountResponse,

    -- ** DescribeBudgetPerformanceHistory (Paginated)
    DescribeBudgetPerformanceHistory (DescribeBudgetPerformanceHistory'),
    newDescribeBudgetPerformanceHistory,
    DescribeBudgetPerformanceHistoryResponse (DescribeBudgetPerformanceHistoryResponse'),
    newDescribeBudgetPerformanceHistoryResponse,

    -- ** DescribeBudgetActionHistories (Paginated)
    DescribeBudgetActionHistories (DescribeBudgetActionHistories'),
    newDescribeBudgetActionHistories,
    DescribeBudgetActionHistoriesResponse (DescribeBudgetActionHistoriesResponse'),
    newDescribeBudgetActionHistoriesResponse,

    -- ** DescribeBudget
    DescribeBudget (DescribeBudget'),
    newDescribeBudget,
    DescribeBudgetResponse (DescribeBudgetResponse'),
    newDescribeBudgetResponse,

    -- ** UpdateSubscriber
    UpdateSubscriber (UpdateSubscriber'),
    newUpdateSubscriber,
    UpdateSubscriberResponse (UpdateSubscriberResponse'),
    newUpdateSubscriberResponse,

    -- ** DeleteSubscriber
    DeleteSubscriber (DeleteSubscriber'),
    newDeleteSubscriber,
    DeleteSubscriberResponse (DeleteSubscriberResponse'),
    newDeleteSubscriberResponse,

    -- ** DescribeBudgetActionsForBudget (Paginated)
    DescribeBudgetActionsForBudget (DescribeBudgetActionsForBudget'),
    newDescribeBudgetActionsForBudget,
    DescribeBudgetActionsForBudgetResponse (DescribeBudgetActionsForBudgetResponse'),
    newDescribeBudgetActionsForBudgetResponse,

    -- ** DescribeBudgetAction
    DescribeBudgetAction (DescribeBudgetAction'),
    newDescribeBudgetAction,
    DescribeBudgetActionResponse (DescribeBudgetActionResponse'),
    newDescribeBudgetActionResponse,

    -- ** CreateBudget
    CreateBudget (CreateBudget'),
    newCreateBudget,
    CreateBudgetResponse (CreateBudgetResponse'),
    newCreateBudgetResponse,

    -- ** CreateNotification
    CreateNotification (CreateNotification'),
    newCreateNotification,
    CreateNotificationResponse (CreateNotificationResponse'),
    newCreateNotificationResponse,

    -- * Types

    -- ** ActionStatus
    ActionStatus (..),

    -- ** ActionSubType
    ActionSubType (..),

    -- ** ActionType
    ActionType (..),

    -- ** ApprovalModel
    ApprovalModel (..),

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

    -- ** Budget
    Budget (Budget'),
    newBudget,

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

import Network.AWS.Budgets.CreateBudget
import Network.AWS.Budgets.CreateBudgetAction
import Network.AWS.Budgets.CreateNotification
import Network.AWS.Budgets.CreateSubscriber
import Network.AWS.Budgets.DeleteBudget
import Network.AWS.Budgets.DeleteBudgetAction
import Network.AWS.Budgets.DeleteNotification
import Network.AWS.Budgets.DeleteSubscriber
import Network.AWS.Budgets.DescribeBudget
import Network.AWS.Budgets.DescribeBudgetAction
import Network.AWS.Budgets.DescribeBudgetActionHistories
import Network.AWS.Budgets.DescribeBudgetActionsForAccount
import Network.AWS.Budgets.DescribeBudgetActionsForBudget
import Network.AWS.Budgets.DescribeBudgetPerformanceHistory
import Network.AWS.Budgets.DescribeBudgets
import Network.AWS.Budgets.DescribeNotificationsForBudget
import Network.AWS.Budgets.DescribeSubscribersForNotification
import Network.AWS.Budgets.ExecuteBudgetAction
import Network.AWS.Budgets.Lens
import Network.AWS.Budgets.Types
import Network.AWS.Budgets.UpdateBudget
import Network.AWS.Budgets.UpdateBudgetAction
import Network.AWS.Budgets.UpdateNotification
import Network.AWS.Budgets.UpdateSubscriber
import Network.AWS.Budgets.Waiters

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
