{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.SNS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2010-03-31@ of the AWS service descriptions, licensed under Apache 2.0.
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
module Amazonka.SNS
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** KMSInvalidStateException
    _KMSInvalidStateException,

    -- ** EndpointDisabledException
    _EndpointDisabledException,

    -- ** ValidationException
    _ValidationException,

    -- ** AuthorizationErrorException
    _AuthorizationErrorException,

    -- ** KMSThrottlingException
    _KMSThrottlingException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** SubscriptionLimitExceededException
    _SubscriptionLimitExceededException,

    -- ** PlatformApplicationDisabledException
    _PlatformApplicationDisabledException,

    -- ** KMSOptInRequired
    _KMSOptInRequired,

    -- ** InternalErrorException
    _InternalErrorException,

    -- ** ThrottledException
    _ThrottledException,

    -- ** KMSNotFoundException
    _KMSNotFoundException,

    -- ** InvalidParameterValueException
    _InvalidParameterValueException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** StaleTagException
    _StaleTagException,

    -- ** KMSDisabledException
    _KMSDisabledException,

    -- ** TagPolicyException
    _TagPolicyException,

    -- ** InvalidSecurityException
    _InvalidSecurityException,

    -- ** TopicLimitExceededException
    _TopicLimitExceededException,

    -- ** ConcurrentAccessException
    _ConcurrentAccessException,

    -- ** OptedOutException
    _OptedOutException,

    -- ** TagLimitExceededException
    _TagLimitExceededException,

    -- ** UserErrorException
    _UserErrorException,

    -- ** VerificationException
    _VerificationException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** FilterPolicyLimitExceededException
    _FilterPolicyLimitExceededException,

    -- ** KMSAccessDeniedException
    _KMSAccessDeniedException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListPhoneNumbersOptedOut (Paginated)
    ListPhoneNumbersOptedOut (ListPhoneNumbersOptedOut'),
    newListPhoneNumbersOptedOut,
    ListPhoneNumbersOptedOutResponse (ListPhoneNumbersOptedOutResponse'),
    newListPhoneNumbersOptedOutResponse,

    -- ** DeleteEndpoint
    DeleteEndpoint (DeleteEndpoint'),
    newDeleteEndpoint,
    DeleteEndpointResponse (DeleteEndpointResponse'),
    newDeleteEndpointResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** RemovePermission
    RemovePermission (RemovePermission'),
    newRemovePermission,
    RemovePermissionResponse (RemovePermissionResponse'),
    newRemovePermissionResponse,

    -- ** DeleteTopic
    DeleteTopic (DeleteTopic'),
    newDeleteTopic,
    DeleteTopicResponse (DeleteTopicResponse'),
    newDeleteTopicResponse,

    -- ** SetSMSAttributes
    SetSMSAttributes (SetSMSAttributes'),
    newSetSMSAttributes,
    SetSMSAttributesResponse (SetSMSAttributesResponse'),
    newSetSMSAttributesResponse,

    -- ** ListTopics (Paginated)
    ListTopics (ListTopics'),
    newListTopics,
    ListTopicsResponse (ListTopicsResponse'),
    newListTopicsResponse,

    -- ** VerifySMSSandboxPhoneNumber
    VerifySMSSandboxPhoneNumber (VerifySMSSandboxPhoneNumber'),
    newVerifySMSSandboxPhoneNumber,
    VerifySMSSandboxPhoneNumberResponse (VerifySMSSandboxPhoneNumberResponse'),
    newVerifySMSSandboxPhoneNumberResponse,

    -- ** CreatePlatformEndpoint
    CreatePlatformEndpoint (CreatePlatformEndpoint'),
    newCreatePlatformEndpoint,
    CreatePlatformEndpointResponse (CreatePlatformEndpointResponse'),
    newCreatePlatformEndpointResponse,

    -- ** SetPlatformApplicationAttributes
    SetPlatformApplicationAttributes (SetPlatformApplicationAttributes'),
    newSetPlatformApplicationAttributes,
    SetPlatformApplicationAttributesResponse (SetPlatformApplicationAttributesResponse'),
    newSetPlatformApplicationAttributesResponse,

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

    -- ** CreateSMSSandboxPhoneNumber
    CreateSMSSandboxPhoneNumber (CreateSMSSandboxPhoneNumber'),
    newCreateSMSSandboxPhoneNumber,
    CreateSMSSandboxPhoneNumberResponse (CreateSMSSandboxPhoneNumberResponse'),
    newCreateSMSSandboxPhoneNumberResponse,

    -- ** OptInPhoneNumber
    OptInPhoneNumber (OptInPhoneNumber'),
    newOptInPhoneNumber,
    OptInPhoneNumberResponse (OptInPhoneNumberResponse'),
    newOptInPhoneNumberResponse,

    -- ** DeleteSMSSandboxPhoneNumber
    DeleteSMSSandboxPhoneNumber (DeleteSMSSandboxPhoneNumber'),
    newDeleteSMSSandboxPhoneNumber,
    DeleteSMSSandboxPhoneNumberResponse (DeleteSMSSandboxPhoneNumberResponse'),
    newDeleteSMSSandboxPhoneNumberResponse,

    -- ** ListSMSSandboxPhoneNumbers (Paginated)
    ListSMSSandboxPhoneNumbers (ListSMSSandboxPhoneNumbers'),
    newListSMSSandboxPhoneNumbers,
    ListSMSSandboxPhoneNumbersResponse (ListSMSSandboxPhoneNumbersResponse'),
    newListSMSSandboxPhoneNumbersResponse,

    -- ** CreatePlatformApplication
    CreatePlatformApplication (CreatePlatformApplication'),
    newCreatePlatformApplication,
    CreatePlatformApplicationResponse (CreatePlatformApplicationResponse'),
    newCreatePlatformApplicationResponse,

    -- ** GetPlatformApplicationAttributes
    GetPlatformApplicationAttributes (GetPlatformApplicationAttributes'),
    newGetPlatformApplicationAttributes,
    GetPlatformApplicationAttributesResponse (GetPlatformApplicationAttributesResponse'),
    newGetPlatformApplicationAttributesResponse,

    -- ** ListEndpointsByPlatformApplication (Paginated)
    ListEndpointsByPlatformApplication (ListEndpointsByPlatformApplication'),
    newListEndpointsByPlatformApplication,
    ListEndpointsByPlatformApplicationResponse (ListEndpointsByPlatformApplicationResponse'),
    newListEndpointsByPlatformApplicationResponse,

    -- ** SetTopicAttributes
    SetTopicAttributes (SetTopicAttributes'),
    newSetTopicAttributes,
    SetTopicAttributesResponse (SetTopicAttributesResponse'),
    newSetTopicAttributesResponse,

    -- ** DeletePlatformApplication
    DeletePlatformApplication (DeletePlatformApplication'),
    newDeletePlatformApplication,
    DeletePlatformApplicationResponse (DeletePlatformApplicationResponse'),
    newDeletePlatformApplicationResponse,

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

    -- ** AddPermission
    AddPermission (AddPermission'),
    newAddPermission,
    AddPermissionResponse (AddPermissionResponse'),
    newAddPermissionResponse,

    -- ** GetEndpointAttributes
    GetEndpointAttributes (GetEndpointAttributes'),
    newGetEndpointAttributes,
    GetEndpointAttributesResponse (GetEndpointAttributesResponse'),
    newGetEndpointAttributesResponse,

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

    -- ** CreateTopic
    CreateTopic (CreateTopic'),
    newCreateTopic,
    CreateTopicResponse (CreateTopicResponse'),
    newCreateTopicResponse,

    -- ** CheckIfPhoneNumberIsOptedOut
    CheckIfPhoneNumberIsOptedOut (CheckIfPhoneNumberIsOptedOut'),
    newCheckIfPhoneNumberIsOptedOut,
    CheckIfPhoneNumberIsOptedOutResponse (CheckIfPhoneNumberIsOptedOutResponse'),
    newCheckIfPhoneNumberIsOptedOutResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** Subscribe
    Subscribe (Subscribe'),
    newSubscribe,
    SubscribeResponse (SubscribeResponse'),
    newSubscribeResponse,

    -- ** ListOriginationNumbers (Paginated)
    ListOriginationNumbers (ListOriginationNumbers'),
    newListOriginationNumbers,
    ListOriginationNumbersResponse (ListOriginationNumbersResponse'),
    newListOriginationNumbersResponse,

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

    -- ** GetSMSSandboxAccountStatus
    GetSMSSandboxAccountStatus (GetSMSSandboxAccountStatus'),
    newGetSMSSandboxAccountStatus,
    GetSMSSandboxAccountStatusResponse (GetSMSSandboxAccountStatusResponse'),
    newGetSMSSandboxAccountStatusResponse,

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

    -- * Types

    -- ** LanguageCodeString
    LanguageCodeString (..),

    -- ** NumberCapability
    NumberCapability (..),

    -- ** RouteType
    RouteType (..),

    -- ** SMSSandboxPhoneNumberVerificationStatus
    SMSSandboxPhoneNumberVerificationStatus (..),

    -- ** Endpoint
    Endpoint (Endpoint'),
    newEndpoint,

    -- ** MessageAttributeValue
    MessageAttributeValue (MessageAttributeValue'),
    newMessageAttributeValue,

    -- ** PhoneNumberInformation
    PhoneNumberInformation (PhoneNumberInformation'),
    newPhoneNumberInformation,

    -- ** PlatformApplication
    PlatformApplication (PlatformApplication'),
    newPlatformApplication,

    -- ** SMSSandboxPhoneNumber
    SMSSandboxPhoneNumber (SMSSandboxPhoneNumber'),
    newSMSSandboxPhoneNumber,

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

import Amazonka.SNS.AddPermission
import Amazonka.SNS.CheckIfPhoneNumberIsOptedOut
import Amazonka.SNS.ConfirmSubscription
import Amazonka.SNS.CreatePlatformApplication
import Amazonka.SNS.CreatePlatformEndpoint
import Amazonka.SNS.CreateSMSSandboxPhoneNumber
import Amazonka.SNS.CreateTopic
import Amazonka.SNS.DeleteEndpoint
import Amazonka.SNS.DeletePlatformApplication
import Amazonka.SNS.DeleteSMSSandboxPhoneNumber
import Amazonka.SNS.DeleteTopic
import Amazonka.SNS.GetEndpointAttributes
import Amazonka.SNS.GetPlatformApplicationAttributes
import Amazonka.SNS.GetSMSAttributes
import Amazonka.SNS.GetSMSSandboxAccountStatus
import Amazonka.SNS.GetSubscriptionAttributes
import Amazonka.SNS.GetTopicAttributes
import Amazonka.SNS.Lens
import Amazonka.SNS.ListEndpointsByPlatformApplication
import Amazonka.SNS.ListOriginationNumbers
import Amazonka.SNS.ListPhoneNumbersOptedOut
import Amazonka.SNS.ListPlatformApplications
import Amazonka.SNS.ListSMSSandboxPhoneNumbers
import Amazonka.SNS.ListSubscriptions
import Amazonka.SNS.ListSubscriptionsByTopic
import Amazonka.SNS.ListTagsForResource
import Amazonka.SNS.ListTopics
import Amazonka.SNS.OptInPhoneNumber
import Amazonka.SNS.Publish
import Amazonka.SNS.RemovePermission
import Amazonka.SNS.SetEndpointAttributes
import Amazonka.SNS.SetPlatformApplicationAttributes
import Amazonka.SNS.SetSMSAttributes
import Amazonka.SNS.SetSubscriptionAttributes
import Amazonka.SNS.SetTopicAttributes
import Amazonka.SNS.Subscribe
import Amazonka.SNS.TagResource
import Amazonka.SNS.Types
import Amazonka.SNS.Unsubscribe
import Amazonka.SNS.UntagResource
import Amazonka.SNS.VerifySMSSandboxPhoneNumber
import Amazonka.SNS.Waiters

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
