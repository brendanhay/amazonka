{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The Amazon Connect API Reference provides descriptions, syntax, and usage examples for each of the Amazon Connect actions, data types, parameters, and errors. Amazon Connect is a cloud-based contact center solution that makes it easy to set up and manage a customer contact center and provide reliable customer engagement at any scale.
--
--
-- Throttling limits for the Amazon Connect API operations:
--
-- For the @GetMetricData@ and @GetCurrentMetricData@ operations, a RateLimit of 5 per second, and a BurstLimit of 8 per second.
--
-- For all other operations, a RateLimit of 2 per second, and a BurstLimit of 5 per second.
--
-- You can request an increase to the throttling limits by submitting a <https://console.aws.amazon.com/support/home#/case/create?issueType=service-limit-increase Amazon Connect service limits increase form> . You must be signed in to your AWS account to access the form.
--
module Network.AWS.Connect
    (
    -- * Service Configuration
      connect

    -- * Errors
    -- $errors

    -- ** OutboundContactNotPermittedException
    , _OutboundContactNotPermittedException

    -- ** InvalidParameterException
    , _InvalidParameterException

    -- ** InvalidRequestException
    , _InvalidRequestException

    -- ** DuplicateResourceException
    , _DuplicateResourceException

    -- ** UserNotFoundException
    , _UserNotFoundException

    -- ** DestinationNotAllowedException
    , _DestinationNotAllowedException

    -- ** ContactNotFoundException
    , _ContactNotFoundException

    -- ** ThrottlingException
    , _ThrottlingException

    -- ** InternalServiceException
    , _InternalServiceException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListSecurityProfiles (Paginated)
    , module Network.AWS.Connect.ListSecurityProfiles

    -- ** UpdateUserHierarchy
    , module Network.AWS.Connect.UpdateUserHierarchy

    -- ** UpdateUserRoutingProfile
    , module Network.AWS.Connect.UpdateUserRoutingProfile

    -- ** StartOutboundVoiceContact
    , module Network.AWS.Connect.StartOutboundVoiceContact

    -- ** GetMetricData (Paginated)
    , module Network.AWS.Connect.GetMetricData

    -- ** ListUsers (Paginated)
    , module Network.AWS.Connect.ListUsers

    -- ** ListUserHierarchyGroups (Paginated)
    , module Network.AWS.Connect.ListUserHierarchyGroups

    -- ** GetCurrentMetricData
    , module Network.AWS.Connect.GetCurrentMetricData

    -- ** ListRoutingProfiles (Paginated)
    , module Network.AWS.Connect.ListRoutingProfiles

    -- ** UpdateUserPhoneConfig
    , module Network.AWS.Connect.UpdateUserPhoneConfig

    -- ** DescribeUserHierarchyStructure
    , module Network.AWS.Connect.DescribeUserHierarchyStructure

    -- ** UpdateContactAttributes
    , module Network.AWS.Connect.UpdateContactAttributes

    -- ** UpdateUserSecurityProfiles
    , module Network.AWS.Connect.UpdateUserSecurityProfiles

    -- ** GetContactAttributes
    , module Network.AWS.Connect.GetContactAttributes

    -- ** DescribeUserHierarchyGroup
    , module Network.AWS.Connect.DescribeUserHierarchyGroup

    -- ** DescribeUser
    , module Network.AWS.Connect.DescribeUser

    -- ** CreateUser
    , module Network.AWS.Connect.CreateUser

    -- ** GetFederationToken
    , module Network.AWS.Connect.GetFederationToken

    -- ** StopContact
    , module Network.AWS.Connect.StopContact

    -- ** DeleteUser
    , module Network.AWS.Connect.DeleteUser

    -- ** UpdateUserIdentityInfo
    , module Network.AWS.Connect.UpdateUserIdentityInfo

    -- * Types

    -- ** Channel
    , Channel (..)

    -- ** Comparison
    , Comparison (..)

    -- ** CurrentMetricName
    , CurrentMetricName (..)

    -- ** Grouping
    , Grouping (..)

    -- ** HistoricalMetricName
    , HistoricalMetricName (..)

    -- ** PhoneType
    , PhoneType (..)

    -- ** Statistic
    , Statistic (..)

    -- ** Unit
    , Unit (..)

    -- ** Credentials
    , Credentials
    , credentials
    , cAccessTokenExpiration
    , cAccessToken
    , cRefreshToken
    , cRefreshTokenExpiration

    -- ** CurrentMetric
    , CurrentMetric
    , currentMetric
    , cmName
    , cmUnit

    -- ** CurrentMetricData
    , CurrentMetricData
    , currentMetricData
    , cmdValue
    , cmdMetric

    -- ** CurrentMetricResult
    , CurrentMetricResult
    , currentMetricResult
    , cmrCollections
    , cmrDimensions

    -- ** Dimensions
    , Dimensions
    , dimensions
    , dChannel
    , dQueue

    -- ** Filters
    , Filters
    , filters
    , fQueues
    , fChannels

    -- ** HierarchyGroup
    , HierarchyGroup
    , hierarchyGroup
    , hgARN
    , hgName
    , hgHierarchyPath
    , hgId
    , hgLevelId

    -- ** HierarchyGroupSummary
    , HierarchyGroupSummary
    , hierarchyGroupSummary
    , hgsARN
    , hgsName
    , hgsId

    -- ** HierarchyLevel
    , HierarchyLevel
    , hierarchyLevel
    , hlARN
    , hlName
    , hlId

    -- ** HierarchyPath
    , HierarchyPath
    , hierarchyPath
    , hpLevelFive
    , hpLevelThree
    , hpLevelFour
    , hpLevelTwo
    , hpLevelOne

    -- ** HierarchyStructure
    , HierarchyStructure
    , hierarchyStructure
    , hsLevelFive
    , hsLevelThree
    , hsLevelFour
    , hsLevelTwo
    , hsLevelOne

    -- ** HistoricalMetric
    , HistoricalMetric
    , historicalMetric
    , hmName
    , hmThreshold
    , hmUnit
    , hmStatistic

    -- ** HistoricalMetricData
    , HistoricalMetricData
    , historicalMetricData
    , hmdValue
    , hmdMetric

    -- ** HistoricalMetricResult
    , HistoricalMetricResult
    , historicalMetricResult
    , hmrCollections
    , hmrDimensions

    -- ** QueueReference
    , QueueReference
    , queueReference
    , qrARN
    , qrId

    -- ** RoutingProfileSummary
    , RoutingProfileSummary
    , routingProfileSummary
    , rpsARN
    , rpsName
    , rpsId

    -- ** SecurityProfileSummary
    , SecurityProfileSummary
    , securityProfileSummary
    , spsARN
    , spsName
    , spsId

    -- ** Threshold
    , Threshold
    , threshold
    , tThresholdValue
    , tComparison

    -- ** User
    , User
    , user
    , uRoutingProfileId
    , uDirectoryUserId
    , uARN
    , uIdentityInfo
    , uSecurityProfileIds
    , uUsername
    , uId
    , uHierarchyGroupId
    , uPhoneConfig

    -- ** UserIdentityInfo
    , UserIdentityInfo
    , userIdentityInfo
    , uiiEmail
    , uiiLastName
    , uiiFirstName

    -- ** UserPhoneConfig
    , UserPhoneConfig
    , userPhoneConfig
    , upcAutoAccept
    , upcAfterContactWorkTimeLimit
    , upcDeskPhoneNumber
    , upcPhoneType

    -- ** UserSummary
    , UserSummary
    , userSummary
    , usARN
    , usUsername
    , usId
    ) where

import Network.AWS.Connect.CreateUser
import Network.AWS.Connect.DeleteUser
import Network.AWS.Connect.DescribeUser
import Network.AWS.Connect.DescribeUserHierarchyGroup
import Network.AWS.Connect.DescribeUserHierarchyStructure
import Network.AWS.Connect.GetContactAttributes
import Network.AWS.Connect.GetCurrentMetricData
import Network.AWS.Connect.GetFederationToken
import Network.AWS.Connect.GetMetricData
import Network.AWS.Connect.ListRoutingProfiles
import Network.AWS.Connect.ListSecurityProfiles
import Network.AWS.Connect.ListUserHierarchyGroups
import Network.AWS.Connect.ListUsers
import Network.AWS.Connect.StartOutboundVoiceContact
import Network.AWS.Connect.StopContact
import Network.AWS.Connect.Types
import Network.AWS.Connect.UpdateContactAttributes
import Network.AWS.Connect.UpdateUserHierarchy
import Network.AWS.Connect.UpdateUserIdentityInfo
import Network.AWS.Connect.UpdateUserPhoneConfig
import Network.AWS.Connect.UpdateUserRoutingProfile
import Network.AWS.Connect.UpdateUserSecurityProfiles
import Network.AWS.Connect.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Connect'.
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
