{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Budgets enable you to plan your service usage, service costs, and your RI utilization. You can also track how close your plan is to your budgeted amount or to the free tier limits. Budgets provide you with a quick way to see your usage-to-date and current estimated charges from AWS and to see how much your predicted usage accrues in charges by the end of the month. Budgets also compare current estimates and charges to the amount that you indicated you want to use or spend and lets you see how much of your budget has been used. AWS updates your budget status several times a day. Budgets track your unblended costs, subscriptions, and refunds. You can create the following types of budgets:
--
--
--     * Cost budgets allow you to say how much you want to spend on a service.
--
--     * Usage budgets allow you to say how many hours you want to use for one or more services.
--
--     * RI utilization budgets allow you to define a utilization threshold and receive alerts when RIs are tracking below that threshold.
--
--
--
-- You can create up to 20,000 budgets per AWS master account. Your first two budgets are free of charge. Each additional budget costs $0.02 per day. You can set up optional notifications that warn you if you exceed, or are forecasted to exceed, your budgeted amount. You can have notifications sent to an Amazon SNS topic, to an email address, or to both. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/budgets-sns-policy.html Creating an Amazon SNS Topic for Budget Notifications> . AWS Free Tier usage alerts via AWS Budgets are provided for you, and do not count toward your budget limits.
--
-- Service Endpoint
--
-- The AWS Budgets API provides the following endpoint:
--
--     * https://budgets.amazonaws.com
--
--
--
-- For information about costs associated with the AWS Budgets API, see <https://aws.amazon.com/aws-cost-management/pricing/ AWS Cost Management Pricing> .
--
module Network.AWS.Budgets
    (
    -- * Service Configuration
      budgets

    -- * Errors
    -- $errors

    -- ** InvalidParameterException
    , _InvalidParameterException

    -- ** InternalErrorException
    , _InternalErrorException

    -- ** ExpiredNextTokenException
    , _ExpiredNextTokenException

    -- ** NotFoundException
    , _NotFoundException

    -- ** InvalidNextTokenException
    , _InvalidNextTokenException

    -- ** DuplicateRecordException
    , _DuplicateRecordException

    -- ** CreationLimitExceededException
    , _CreationLimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeSubscribersForNotification
    , module Network.AWS.Budgets.DescribeSubscribersForNotification

    -- ** DescribeNotificationsForBudget
    , module Network.AWS.Budgets.DescribeNotificationsForBudget

    -- ** DescribeBudgets
    , module Network.AWS.Budgets.DescribeBudgets

    -- ** CreateSubscriber
    , module Network.AWS.Budgets.CreateSubscriber

    -- ** UpdateBudget
    , module Network.AWS.Budgets.UpdateBudget

    -- ** DeleteBudget
    , module Network.AWS.Budgets.DeleteBudget

    -- ** DeleteNotification
    , module Network.AWS.Budgets.DeleteNotification

    -- ** UpdateNotification
    , module Network.AWS.Budgets.UpdateNotification

    -- ** DescribeBudget
    , module Network.AWS.Budgets.DescribeBudget

    -- ** UpdateSubscriber
    , module Network.AWS.Budgets.UpdateSubscriber

    -- ** DeleteSubscriber
    , module Network.AWS.Budgets.DeleteSubscriber

    -- ** CreateBudget
    , module Network.AWS.Budgets.CreateBudget

    -- ** CreateNotification
    , module Network.AWS.Budgets.CreateNotification

    -- * Types

    -- ** BudgetType
    , BudgetType (..)

    -- ** ComparisonOperator
    , ComparisonOperator (..)

    -- ** NotificationType
    , NotificationType (..)

    -- ** SubscriptionType
    , SubscriptionType (..)

    -- ** ThresholdType
    , ThresholdType (..)

    -- ** TimeUnit
    , TimeUnit (..)

    -- ** Budget
    , Budget
    , budget
    , bCalculatedSpend
    , bBudgetLimit
    , bTimePeriod
    , bCostTypes
    , bCostFilters
    , bBudgetName
    , bTimeUnit
    , bBudgetType

    -- ** CalculatedSpend
    , CalculatedSpend
    , calculatedSpend
    , csForecastedSpend
    , csActualSpend

    -- ** CostTypes
    , CostTypes
    , costTypes
    , ctUseAmortized
    , ctIncludeRecurring
    , ctUseBlended
    , ctIncludeSupport
    , ctIncludeDiscount
    , ctIncludeSubscription
    , ctIncludeRefund
    , ctIncludeUpfront
    , ctIncludeOtherSubscription
    , ctIncludeTax
    , ctIncludeCredit

    -- ** Notification
    , Notification
    , notification
    , nThresholdType
    , nNotificationType
    , nComparisonOperator
    , nThreshold

    -- ** NotificationWithSubscribers
    , NotificationWithSubscribers
    , notificationWithSubscribers
    , nwsNotification
    , nwsSubscribers

    -- ** Spend
    , Spend
    , spend
    , sAmount
    , sUnit

    -- ** Subscriber
    , Subscriber
    , subscriber
    , sSubscriptionType
    , sAddress

    -- ** TimePeriod
    , TimePeriod
    , timePeriod
    , tpStart
    , tpEnd
    ) where

import Network.AWS.Budgets.CreateBudget
import Network.AWS.Budgets.CreateNotification
import Network.AWS.Budgets.CreateSubscriber
import Network.AWS.Budgets.DeleteBudget
import Network.AWS.Budgets.DeleteNotification
import Network.AWS.Budgets.DeleteSubscriber
import Network.AWS.Budgets.DescribeBudget
import Network.AWS.Budgets.DescribeBudgets
import Network.AWS.Budgets.DescribeNotificationsForBudget
import Network.AWS.Budgets.DescribeSubscribersForNotification
import Network.AWS.Budgets.Types
import Network.AWS.Budgets.UpdateBudget
import Network.AWS.Budgets.UpdateNotification
import Network.AWS.Budgets.UpdateSubscriber
import Network.AWS.Budgets.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Budgets'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
