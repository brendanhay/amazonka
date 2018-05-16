{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Shield Advanced__
--
-- This is the /AWS Shield Advanced API Reference/ . This guide is for developers who need detailed information about the AWS Shield Advanced API actions, data types, and errors. For detailed information about AWS WAF and AWS Shield Advanced features and an overview of how to use the AWS WAF and AWS Shield Advanced APIs, see the <http://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF and AWS Shield Developer Guide> .
--
module Network.AWS.Shield
    (
    -- * Service Configuration
      shield

    -- * Errors
    -- $errors

    -- ** InvalidResourceException
    , _InvalidResourceException

    -- ** InvalidParameterException
    , _InvalidParameterException

    -- ** LimitsExceededException
    , _LimitsExceededException

    -- ** InternalErrorException
    , _InternalErrorException

    -- ** ResourceAlreadyExistsException
    , _ResourceAlreadyExistsException

    -- ** OptimisticLockException
    , _OptimisticLockException

    -- ** InvalidOperationException
    , _InvalidOperationException

    -- ** LockedSubscriptionException
    , _LockedSubscriptionException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateSubscription
    , module Network.AWS.Shield.CreateSubscription

    -- ** ListProtections (Paginated)
    , module Network.AWS.Shield.ListProtections

    -- ** DeleteSubscription
    , module Network.AWS.Shield.DeleteSubscription

    -- ** DescribeAttack
    , module Network.AWS.Shield.DescribeAttack

    -- ** DescribeProtection
    , module Network.AWS.Shield.DescribeProtection

    -- ** ListAttacks
    , module Network.AWS.Shield.ListAttacks

    -- ** CreateProtection
    , module Network.AWS.Shield.CreateProtection

    -- ** DeleteProtection
    , module Network.AWS.Shield.DeleteProtection

    -- ** GetSubscriptionState
    , module Network.AWS.Shield.GetSubscriptionState

    -- ** DescribeSubscription
    , module Network.AWS.Shield.DescribeSubscription

    -- * Types

    -- ** AttackLayer
    , AttackLayer (..)

    -- ** AttackPropertyIdentifier
    , AttackPropertyIdentifier (..)

    -- ** SubResourceType
    , SubResourceType (..)

    -- ** SubscriptionState
    , SubscriptionState (..)

    -- ** Unit
    , Unit (..)

    -- ** AttackDetail
    , AttackDetail
    , attackDetail
    , adAttackId
    , adStartTime
    , adSubResources
    , adMitigations
    , adAttackProperties
    , adAttackCounters
    , adResourceARN
    , adEndTime

    -- ** AttackProperty
    , AttackProperty
    , attackProperty
    , apAttackLayer
    , apTopContributors
    , apAttackPropertyIdentifier
    , apTotal
    , apUnit

    -- ** AttackSummary
    , AttackSummary
    , attackSummary
    , asAttackVectors
    , asAttackId
    , asStartTime
    , asResourceARN
    , asEndTime

    -- ** AttackVectorDescription
    , AttackVectorDescription
    , attackVectorDescription
    , avdVectorType

    -- ** Contributor
    , Contributor
    , contributor
    , cValue
    , cName

    -- ** Mitigation
    , Mitigation
    , mitigation
    , mMitigationName

    -- ** Protection
    , Protection
    , protection
    , pResourceARN
    , pName
    , pId

    -- ** SubResourceSummary
    , SubResourceSummary
    , subResourceSummary
    , srsCounters
    , srsAttackVectors
    , srsId
    , srsType

    -- ** Subscription
    , Subscription
    , subscription
    , sTimeCommitmentInSeconds
    , sStartTime

    -- ** SummarizedAttackVector
    , SummarizedAttackVector
    , summarizedAttackVector
    , savVectorCounters
    , savVectorType

    -- ** SummarizedCounter
    , SummarizedCounter
    , summarizedCounter
    , scMax
    , scAverage
    , scN
    , scName
    , scSum
    , scUnit

    -- ** TimeRange
    , TimeRange
    , timeRange
    , trFromInclusive
    , trToExclusive
    ) where

import Network.AWS.Shield.CreateProtection
import Network.AWS.Shield.CreateSubscription
import Network.AWS.Shield.DeleteProtection
import Network.AWS.Shield.DeleteSubscription
import Network.AWS.Shield.DescribeAttack
import Network.AWS.Shield.DescribeProtection
import Network.AWS.Shield.DescribeSubscription
import Network.AWS.Shield.GetSubscriptionState
import Network.AWS.Shield.ListAttacks
import Network.AWS.Shield.ListProtections
import Network.AWS.Shield.Types
import Network.AWS.Shield.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Shield'.
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
