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
-- This is the /AWS Shield Advanced API Reference/ . This guide is for developers who need detailed information about the AWS Shield Advanced API actions, data types, and errors. For detailed information about AWS WAF and AWS Shield Advanced features and an overview of how to use the AWS WAF and AWS Shield Advanced APIs, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF and AWS Shield Developer Guide> .
--
module Network.AWS.Shield
    (
    -- * Service Configuration
      shield

    -- * Errors
    -- $errors

    -- ** InvalidResourceException
    , _InvalidResourceException

    -- ** AccessDeniedException
    , _AccessDeniedException

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

    -- ** NoAssociatedRoleException
    , _NoAssociatedRoleException

    -- ** AccessDeniedForDependencyException
    , _AccessDeniedForDependencyException

    -- ** InvalidOperationException
    , _InvalidOperationException

    -- ** LockedSubscriptionException
    , _LockedSubscriptionException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** InvalidPaginationTokenException
    , _InvalidPaginationTokenException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateDRTLogBucket
    , module Network.AWS.Shield.AssociateDRTLogBucket

    -- ** DisassociateDRTRole
    , module Network.AWS.Shield.DisassociateDRTRole

    -- ** CreateSubscription
    , module Network.AWS.Shield.CreateSubscription

    -- ** ListProtections (Paginated)
    , module Network.AWS.Shield.ListProtections

    -- ** AssociateDRTRole
    , module Network.AWS.Shield.AssociateDRTRole

    -- ** UpdateSubscription
    , module Network.AWS.Shield.UpdateSubscription

    -- ** DisassociateDRTLogBucket
    , module Network.AWS.Shield.DisassociateDRTLogBucket

    -- ** DescribeAttack
    , module Network.AWS.Shield.DescribeAttack

    -- ** UpdateEmergencyContactSettings
    , module Network.AWS.Shield.UpdateEmergencyContactSettings

    -- ** DescribeProtection
    , module Network.AWS.Shield.DescribeProtection

    -- ** ListAttacks (Paginated)
    , module Network.AWS.Shield.ListAttacks

    -- ** DescribeEmergencyContactSettings
    , module Network.AWS.Shield.DescribeEmergencyContactSettings

    -- ** CreateProtection
    , module Network.AWS.Shield.CreateProtection

    -- ** DeleteProtection
    , module Network.AWS.Shield.DeleteProtection

    -- ** GetSubscriptionState
    , module Network.AWS.Shield.GetSubscriptionState

    -- ** DescribeDRTAccess
    , module Network.AWS.Shield.DescribeDRTAccess

    -- ** DescribeSubscription
    , module Network.AWS.Shield.DescribeSubscription

    -- * Types

    -- ** AttackLayer
    , AttackLayer (..)

    -- ** AttackPropertyIdentifier
    , AttackPropertyIdentifier (..)

    -- ** AutoRenew
    , AutoRenew (..)

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

    -- ** EmergencyContact
    , EmergencyContact
    , emergencyContact
    , ecEmailAddress

    -- ** Limit
    , Limit
    , limit
    , lMax
    , lType

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
    , sLimits
    , sAutoRenew
    , sEndTime

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

import Network.AWS.Shield.AssociateDRTLogBucket
import Network.AWS.Shield.AssociateDRTRole
import Network.AWS.Shield.CreateProtection
import Network.AWS.Shield.CreateSubscription
import Network.AWS.Shield.DeleteProtection
import Network.AWS.Shield.DescribeAttack
import Network.AWS.Shield.DescribeDRTAccess
import Network.AWS.Shield.DescribeEmergencyContactSettings
import Network.AWS.Shield.DescribeProtection
import Network.AWS.Shield.DescribeSubscription
import Network.AWS.Shield.DisassociateDRTLogBucket
import Network.AWS.Shield.DisassociateDRTRole
import Network.AWS.Shield.GetSubscriptionState
import Network.AWS.Shield.ListAttacks
import Network.AWS.Shield.ListProtections
import Network.AWS.Shield.Types
import Network.AWS.Shield.UpdateEmergencyContactSettings
import Network.AWS.Shield.UpdateSubscription
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
