{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- All public APIs for AWS Budgets
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
    , bCostFilters
    , bBudgetName
    , bBudgetLimit
    , bCostTypes
    , bTimeUnit
    , bTimePeriod
    , bBudgetType

    -- ** CalculatedSpend
    , CalculatedSpend
    , calculatedSpend
    , csForecastedSpend
    , csActualSpend

    -- ** CostTypes
    , CostTypes
    , costTypes
    , ctIncludeTax
    , ctIncludeSubscription
    , ctUseBlended

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
