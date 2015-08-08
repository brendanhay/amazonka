{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Simple Notification Service
--
-- Amazon Simple Notification Service (Amazon SNS) is a web service that
-- enables you to build distributed web-enabled applications. Applications
-- can use Amazon SNS to easily push real-time notification messages to
-- interested subscribers over multiple delivery protocols. For more
-- information about this product see
-- <http://aws.amazon.com/sns/ http:\/\/aws.amazon.com\/sns>. For detailed
-- information about Amazon SNS features and their associated API calls,
-- see the
-- <http://docs.aws.amazon.com/sns/latest/dg/ Amazon SNS Developer Guide>.
--
-- We also provide SDKs that enable you to access Amazon SNS from your
-- preferred programming language. The SDKs contain functionality that
-- automatically takes care of tasks such as: cryptographically signing
-- your service requests, retrying requests, and handling error responses.
-- For a list of available SDKs, go to
-- <http://aws.amazon.com/tools/ Tools for Amazon Web Services>.
--
-- /See:/ <http://docs.aws.amazon.com/sns/latest/api/Welcome.html AWS API Reference>
module Network.AWS.SNS
    (
    -- * Service Description
      SNS

    -- * Error Matchers
    -- $errors
    , _EndpointDisabledException
    , _AuthorizationErrorException
    , _InvalidParameterException
    , _SubscriptionLimitExceededException
    , _PlatformApplicationDisabledException
    , _InternalErrorException
    , _NotFoundException
    , _InvalidParameterValueException
    , _TopicLimitExceededException

    -- * Operations
    -- $operations

    -- ** DeleteEndpoint
    , module Network.AWS.SNS.DeleteEndpoint

    -- ** RemovePermission
    , module Network.AWS.SNS.RemovePermission

    -- ** SetPlatformApplicationAttributes
    , module Network.AWS.SNS.SetPlatformApplicationAttributes

    -- ** CreatePlatformEndpoint
    , module Network.AWS.SNS.CreatePlatformEndpoint

    -- ** ListSubscriptionsByTopic (Paginated)
    , module Network.AWS.SNS.ListSubscriptionsByTopic
    -- $pager

    -- ** GetTopicAttributes
    , module Network.AWS.SNS.GetTopicAttributes

    -- ** DeleteTopic
    , module Network.AWS.SNS.DeleteTopic

    -- ** ListTopics (Paginated)
    , module Network.AWS.SNS.ListTopics
    -- $pager

    -- ** CreatePlatformApplication
    , module Network.AWS.SNS.CreatePlatformApplication

    -- ** ListEndpointsByPlatformApplication (Paginated)
    , module Network.AWS.SNS.ListEndpointsByPlatformApplication
    -- $pager

    -- ** GetPlatformApplicationAttributes
    , module Network.AWS.SNS.GetPlatformApplicationAttributes

    -- ** DeletePlatformApplication
    , module Network.AWS.SNS.DeletePlatformApplication

    -- ** ListPlatformApplications (Paginated)
    , module Network.AWS.SNS.ListPlatformApplications
    -- $pager

    -- ** SetTopicAttributes
    , module Network.AWS.SNS.SetTopicAttributes

    -- ** GetEndpointAttributes
    , module Network.AWS.SNS.GetEndpointAttributes

    -- ** AddPermission
    , module Network.AWS.SNS.AddPermission

    -- ** GetSubscriptionAttributes
    , module Network.AWS.SNS.GetSubscriptionAttributes

    -- ** ListSubscriptions (Paginated)
    , module Network.AWS.SNS.ListSubscriptions
    -- $pager

    -- ** CreateTopic
    , module Network.AWS.SNS.CreateTopic

    -- ** Subscribe
    , module Network.AWS.SNS.Subscribe

    -- ** Unsubscribe
    , module Network.AWS.SNS.Unsubscribe

    -- ** SetEndpointAttributes
    , module Network.AWS.SNS.SetEndpointAttributes

    -- ** SetSubscriptionAttributes
    , module Network.AWS.SNS.SetSubscriptionAttributes

    -- ** ConfirmSubscription
    , module Network.AWS.SNS.ConfirmSubscription

    -- ** Publish
    , module Network.AWS.SNS.Publish

    -- * Types

    -- ** Endpoint
    , Endpoint
    , endpoint
    , eAttributes
    , eEndpointARN

    -- ** MessageAttributeValue
    , MessageAttributeValue
    , messageAttributeValue
    , mavBinaryValue
    , mavStringValue
    , mavDataType

    -- ** PlatformApplication
    , PlatformApplication
    , platformApplication
    , paPlatformApplicationARN
    , paAttributes

    -- ** Subscription
    , Subscription
    , subscription
    , sProtocol
    , sOwner
    , sTopicARN
    , sEndpoint
    , sSubscriptionARN

    -- ** Topic
    , Topic
    , topic
    , tTopicARN
    ) where

import           Network.AWS.SNS.AddPermission
import           Network.AWS.SNS.ConfirmSubscription
import           Network.AWS.SNS.CreatePlatformApplication
import           Network.AWS.SNS.CreatePlatformEndpoint
import           Network.AWS.SNS.CreateTopic
import           Network.AWS.SNS.DeleteEndpoint
import           Network.AWS.SNS.DeletePlatformApplication
import           Network.AWS.SNS.DeleteTopic
import           Network.AWS.SNS.GetEndpointAttributes
import           Network.AWS.SNS.GetPlatformApplicationAttributes
import           Network.AWS.SNS.GetSubscriptionAttributes
import           Network.AWS.SNS.GetTopicAttributes
import           Network.AWS.SNS.ListEndpointsByPlatformApplication
import           Network.AWS.SNS.ListPlatformApplications
import           Network.AWS.SNS.ListSubscriptions
import           Network.AWS.SNS.ListSubscriptionsByTopic
import           Network.AWS.SNS.ListTopics
import           Network.AWS.SNS.Publish
import           Network.AWS.SNS.RemovePermission
import           Network.AWS.SNS.SetEndpointAttributes
import           Network.AWS.SNS.SetPlatformApplicationAttributes
import           Network.AWS.SNS.SetSubscriptionAttributes
import           Network.AWS.SNS.SetTopicAttributes
import           Network.AWS.SNS.Subscribe
import           Network.AWS.SNS.Types
import           Network.AWS.SNS.Unsubscribe
import           Network.AWS.SNS.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'SNS'.
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

{- $pager
This operation can return paginated results.
-}
