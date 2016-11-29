{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSBudgets
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- All public APIs for AWS Budgets
module Network.AWS.AWSBudgets
    (
    -- * Service Configuration
      awsBudgets

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
    , module Network.AWS.AWSBudgets.DescribeSubscribersForNotification

    -- ** DescribeNotificationsForBudget
    , module Network.AWS.AWSBudgets.DescribeNotificationsForBudget

    -- ** DescribeBudgets
    , module Network.AWS.AWSBudgets.DescribeBudgets

    -- ** CreateSubscriber
    , module Network.AWS.AWSBudgets.CreateSubscriber

    -- ** UpdateBudget
    , module Network.AWS.AWSBudgets.UpdateBudget

    -- ** DeleteBudget
    , module Network.AWS.AWSBudgets.DeleteBudget

    -- ** DeleteNotification
    , module Network.AWS.AWSBudgets.DeleteNotification

    -- ** UpdateNotification
    , module Network.AWS.AWSBudgets.UpdateNotification

    -- ** DescribeBudget
    , module Network.AWS.AWSBudgets.DescribeBudget

    -- ** UpdateSubscriber
    , module Network.AWS.AWSBudgets.UpdateSubscriber

    -- ** DeleteSubscriber
    , module Network.AWS.AWSBudgets.DeleteSubscriber

    -- ** CreateBudget
    , module Network.AWS.AWSBudgets.CreateBudget

    -- ** CreateNotification
    , module Network.AWS.AWSBudgets.CreateNotification

    -- * Types

    -- ** BudgetType
    , BudgetType (..)

    -- ** ComparisonOperator
    , ComparisonOperator (..)

    -- ** NotificationType
    , NotificationType (..)

    -- ** SubscriptionType
    , SubscriptionType (..)

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

import           Network.AWS.AWSBudgets.CreateBudget
import           Network.AWS.AWSBudgets.CreateNotification
import           Network.AWS.AWSBudgets.CreateSubscriber
import           Network.AWS.AWSBudgets.DeleteBudget
import           Network.AWS.AWSBudgets.DeleteNotification
import           Network.AWS.AWSBudgets.DeleteSubscriber
import           Network.AWS.AWSBudgets.DescribeBudget
import           Network.AWS.AWSBudgets.DescribeBudgets
import           Network.AWS.AWSBudgets.DescribeNotificationsForBudget
import           Network.AWS.AWSBudgets.DescribeSubscribersForNotification
import           Network.AWS.AWSBudgets.Types
import           Network.AWS.AWSBudgets.UpdateBudget
import           Network.AWS.AWSBudgets.UpdateNotification
import           Network.AWS.AWSBudgets.UpdateSubscriber
import           Network.AWS.AWSBudgets.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'AWSBudgets'.
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
