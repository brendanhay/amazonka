{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Simple Notification Service
--
-- Amazon Simple Notification Service (Amazon SNS) is a web service that
-- enables you to build distributed web-enabled applications. Applications
-- can use Amazon SNS to easily push real-time notification messages to
-- interested subscribers over multiple delivery protocols. For more
-- information about this product see the
-- <http://aws.amazon.com/sns/ Amazon SNS product page>. For detailed
-- information about Amazon SNS features and their associated API calls,
-- see the
-- <https://docs.aws.amazon.com/sns/latest/dg/ Amazon SNS Developer Guide>.
--
-- For information on the permissions you need to use this API, see
-- <https://docs.aws.amazon.com/sns/latest/dg/sns-authentication-and-access-control.html Identity and access management in Amazon SNS>
-- in the /Amazon SNS Developer Guide./
--
-- We also provide SDKs that enable you to access Amazon SNS from your
-- preferred programming language. The SDKs contain functionality that
-- automatically takes care of tasks such as: cryptographically signing
-- your service requests, retrying requests, and handling error responses.
-- For a list of available SDKs, go to
-- <http://aws.amazon.com/tools/ Tools for Amazon Web Services>.
module Network.AWS.SNS
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** KMSThrottlingException
    _KMSThrottlingException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** KMSInvalidStateException
    _KMSInvalidStateException,

    -- ** KMSNotFoundException
    _KMSNotFoundException,

    -- ** ThrottledException
    _ThrottledException,

    -- ** TagLimitExceededException
    _TagLimitExceededException,

    -- ** InternalErrorException
    _InternalErrorException,

    -- ** TopicLimitExceededException
    _TopicLimitExceededException,

    -- ** KMSOptInRequired
    _KMSOptInRequired,

    -- ** ConcurrentAccessException
    _ConcurrentAccessException,

    -- ** TagPolicyException
    _TagPolicyException,

    -- ** PlatformApplicationDisabledException
    _PlatformApplicationDisabledException,

    -- ** SubscriptionLimitExceededException
    _SubscriptionLimitExceededException,

    -- ** StaleTagException
    _StaleTagException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** EndpointDisabledException
    _EndpointDisabledException,

    -- ** InvalidParameterValueException
    _InvalidParameterValueException,

    -- ** AuthorizationErrorException
    _AuthorizationErrorException,

    -- ** FilterPolicyLimitExceededException
    _FilterPolicyLimitExceededException,

    -- ** KMSAccessDeniedException
    _KMSAccessDeniedException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** InvalidSecurityException
    _InvalidSecurityException,

    -- ** KMSDisabledException
    _KMSDisabledException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeletePlatformApplication
    DeletePlatformApplication (DeletePlatformApplication'),
    newDeletePlatformApplication,
    DeletePlatformApplicationResponse (DeletePlatformApplicationResponse'),
    newDeletePlatformApplicationResponse,

    -- ** ConfirmSubscription
    ConfirmSubscription (ConfirmSubscription'),
    newConfirmSubscription,
    ConfirmSubscriptionResponse (ConfirmSubscriptionResponse'),
    newConfirmSubscriptionResponse,

    -- ** Publish
    Publish (Publish'),
    newPublish,
    PublishResponse (PublishResponse'),
    newPublishResponse,

    -- ** OptInPhoneNumber
    OptInPhoneNumber (OptInPhoneNumber'),
    newOptInPhoneNumber,
    OptInPhoneNumberResponse (OptInPhoneNumberResponse'),
    newOptInPhoneNumberResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** Unsubscribe
    Unsubscribe (Unsubscribe'),
    newUnsubscribe,
    UnsubscribeResponse (UnsubscribeResponse'),
    newUnsubscribeResponse,

    -- ** ListSubscriptionsByTopic (Paginated)
    ListSubscriptionsByTopic (ListSubscriptionsByTopic'),
    newListSubscriptionsByTopic,
    ListSubscriptionsByTopicResponse (ListSubscriptionsByTopicResponse'),
    newListSubscriptionsByTopicResponse,

    -- ** GetTopicAttributes
    GetTopicAttributes (GetTopicAttributes'),
    newGetTopicAttributes,
    GetTopicAttributesResponse (GetTopicAttributesResponse'),
    newGetTopicAttributesResponse,

    -- ** SetSMSAttributes
    SetSMSAttributes (SetSMSAttributes'),
    newSetSMSAttributes,
    SetSMSAttributesResponse (SetSMSAttributesResponse'),
    newSetSMSAttributesResponse,

    -- ** SetPlatformApplicationAttributes
    SetPlatformApplicationAttributes (SetPlatformApplicationAttributes'),
    newSetPlatformApplicationAttributes,
    SetPlatformApplicationAttributesResponse (SetPlatformApplicationAttributesResponse'),
    newSetPlatformApplicationAttributesResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** CreatePlatformEndpoint
    CreatePlatformEndpoint (CreatePlatformEndpoint'),
    newCreatePlatformEndpoint,
    CreatePlatformEndpointResponse (CreatePlatformEndpointResponse'),
    newCreatePlatformEndpointResponse,

    -- ** Subscribe
    Subscribe (Subscribe'),
    newSubscribe,
    SubscribeResponse (SubscribeResponse'),
    newSubscribeResponse,

    -- ** ListTopics (Paginated)
    ListTopics (ListTopics'),
    newListTopics,
    ListTopicsResponse (ListTopicsResponse'),
    newListTopicsResponse,

    -- ** ListSubscriptions (Paginated)
    ListSubscriptions (ListSubscriptions'),
    newListSubscriptions,
    ListSubscriptionsResponse (ListSubscriptionsResponse'),
    newListSubscriptionsResponse,

    -- ** GetSubscriptionAttributes
    GetSubscriptionAttributes (GetSubscriptionAttributes'),
    newGetSubscriptionAttributes,
    GetSubscriptionAttributesResponse (GetSubscriptionAttributesResponse'),
    newGetSubscriptionAttributesResponse,

    -- ** DeleteEndpoint
    DeleteEndpoint (DeleteEndpoint'),
    newDeleteEndpoint,
    DeleteEndpointResponse (DeleteEndpointResponse'),
    newDeleteEndpointResponse,

    -- ** AddPermission
    AddPermission (AddPermission'),
    newAddPermission,
    AddPermissionResponse (AddPermissionResponse'),
    newAddPermissionResponse,

    -- ** ListPhoneNumbersOptedOut (Paginated)
    ListPhoneNumbersOptedOut (ListPhoneNumbersOptedOut'),
    newListPhoneNumbersOptedOut,
    ListPhoneNumbersOptedOutResponse (ListPhoneNumbersOptedOutResponse'),
    newListPhoneNumbersOptedOutResponse,

    -- ** GetEndpointAttributes
    GetEndpointAttributes (GetEndpointAttributes'),
    newGetEndpointAttributes,
    GetEndpointAttributesResponse (GetEndpointAttributesResponse'),
    newGetEndpointAttributesResponse,

    -- ** GetPlatformApplicationAttributes
    GetPlatformApplicationAttributes (GetPlatformApplicationAttributes'),
    newGetPlatformApplicationAttributes,
    GetPlatformApplicationAttributesResponse (GetPlatformApplicationAttributesResponse'),
    newGetPlatformApplicationAttributesResponse,

    -- ** SetTopicAttributes
    SetTopicAttributes (SetTopicAttributes'),
    newSetTopicAttributes,
    SetTopicAttributesResponse (SetTopicAttributesResponse'),
    newSetTopicAttributesResponse,

    -- ** ListEndpointsByPlatformApplication (Paginated)
    ListEndpointsByPlatformApplication (ListEndpointsByPlatformApplication'),
    newListEndpointsByPlatformApplication,
    ListEndpointsByPlatformApplicationResponse (ListEndpointsByPlatformApplicationResponse'),
    newListEndpointsByPlatformApplicationResponse,

    -- ** GetSMSAttributes
    GetSMSAttributes (GetSMSAttributes'),
    newGetSMSAttributes,
    GetSMSAttributesResponse (GetSMSAttributesResponse'),
    newGetSMSAttributesResponse,

    -- ** ListPlatformApplications (Paginated)
    ListPlatformApplications (ListPlatformApplications'),
    newListPlatformApplications,
    ListPlatformApplicationsResponse (ListPlatformApplicationsResponse'),
    newListPlatformApplicationsResponse,

    -- ** CreatePlatformApplication
    CreatePlatformApplication (CreatePlatformApplication'),
    newCreatePlatformApplication,
    CreatePlatformApplicationResponse (CreatePlatformApplicationResponse'),
    newCreatePlatformApplicationResponse,

    -- ** SetEndpointAttributes
    SetEndpointAttributes (SetEndpointAttributes'),
    newSetEndpointAttributes,
    SetEndpointAttributesResponse (SetEndpointAttributesResponse'),
    newSetEndpointAttributesResponse,

    -- ** SetSubscriptionAttributes
    SetSubscriptionAttributes (SetSubscriptionAttributes'),
    newSetSubscriptionAttributes,
    SetSubscriptionAttributesResponse (SetSubscriptionAttributesResponse'),
    newSetSubscriptionAttributesResponse,

    -- ** CheckIfPhoneNumberIsOptedOut
    CheckIfPhoneNumberIsOptedOut (CheckIfPhoneNumberIsOptedOut'),
    newCheckIfPhoneNumberIsOptedOut,
    CheckIfPhoneNumberIsOptedOutResponse (CheckIfPhoneNumberIsOptedOutResponse'),
    newCheckIfPhoneNumberIsOptedOutResponse,

    -- ** DeleteTopic
    DeleteTopic (DeleteTopic'),
    newDeleteTopic,
    DeleteTopicResponse (DeleteTopicResponse'),
    newDeleteTopicResponse,

    -- ** CreateTopic
    CreateTopic (CreateTopic'),
    newCreateTopic,
    CreateTopicResponse (CreateTopicResponse'),
    newCreateTopicResponse,

    -- ** RemovePermission
    RemovePermission (RemovePermission'),
    newRemovePermission,
    RemovePermissionResponse (RemovePermissionResponse'),
    newRemovePermissionResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- * Types

    -- ** Endpoint
    Endpoint (Endpoint'),
    newEndpoint,

    -- ** MessageAttributeValue
    MessageAttributeValue (MessageAttributeValue'),
    newMessageAttributeValue,

    -- ** PlatformApplication
    PlatformApplication (PlatformApplication'),
    newPlatformApplication,

    -- ** Subscription
    Subscription (Subscription'),
    newSubscription,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** Topic
    Topic (Topic'),
    newTopic,
  )
where

import Network.AWS.SNS.AddPermission
import Network.AWS.SNS.CheckIfPhoneNumberIsOptedOut
import Network.AWS.SNS.ConfirmSubscription
import Network.AWS.SNS.CreatePlatformApplication
import Network.AWS.SNS.CreatePlatformEndpoint
import Network.AWS.SNS.CreateTopic
import Network.AWS.SNS.DeleteEndpoint
import Network.AWS.SNS.DeletePlatformApplication
import Network.AWS.SNS.DeleteTopic
import Network.AWS.SNS.GetEndpointAttributes
import Network.AWS.SNS.GetPlatformApplicationAttributes
import Network.AWS.SNS.GetSMSAttributes
import Network.AWS.SNS.GetSubscriptionAttributes
import Network.AWS.SNS.GetTopicAttributes
import Network.AWS.SNS.Lens
import Network.AWS.SNS.ListEndpointsByPlatformApplication
import Network.AWS.SNS.ListPhoneNumbersOptedOut
import Network.AWS.SNS.ListPlatformApplications
import Network.AWS.SNS.ListSubscriptions
import Network.AWS.SNS.ListSubscriptionsByTopic
import Network.AWS.SNS.ListTagsForResource
import Network.AWS.SNS.ListTopics
import Network.AWS.SNS.OptInPhoneNumber
import Network.AWS.SNS.Publish
import Network.AWS.SNS.RemovePermission
import Network.AWS.SNS.SetEndpointAttributes
import Network.AWS.SNS.SetPlatformApplicationAttributes
import Network.AWS.SNS.SetSMSAttributes
import Network.AWS.SNS.SetSubscriptionAttributes
import Network.AWS.SNS.SetTopicAttributes
import Network.AWS.SNS.Subscribe
import Network.AWS.SNS.TagResource
import Network.AWS.SNS.Types
import Network.AWS.SNS.Unsubscribe
import Network.AWS.SNS.UntagResource
import Network.AWS.SNS.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'SNS'.

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
