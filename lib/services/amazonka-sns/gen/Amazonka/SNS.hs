{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.SNS
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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

    -- ** AuthorizationErrorException
    _AuthorizationErrorException,

    -- ** BatchEntryIdsNotDistinctException
    _BatchEntryIdsNotDistinctException,

    -- ** BatchRequestTooLongException
    _BatchRequestTooLongException,

    -- ** ConcurrentAccessException
    _ConcurrentAccessException,

    -- ** EmptyBatchRequestException
    _EmptyBatchRequestException,

    -- ** EndpointDisabledException
    _EndpointDisabledException,

    -- ** FilterPolicyLimitExceededException
    _FilterPolicyLimitExceededException,

    -- ** InternalErrorException
    _InternalErrorException,

    -- ** InvalidBatchEntryIdException
    _InvalidBatchEntryIdException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** InvalidParameterValueException
    _InvalidParameterValueException,

    -- ** InvalidSecurityException
    _InvalidSecurityException,

    -- ** KMSAccessDeniedException
    _KMSAccessDeniedException,

    -- ** KMSDisabledException
    _KMSDisabledException,

    -- ** KMSInvalidStateException
    _KMSInvalidStateException,

    -- ** KMSNotFoundException
    _KMSNotFoundException,

    -- ** KMSOptInRequired
    _KMSOptInRequired,

    -- ** KMSThrottlingException
    _KMSThrottlingException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** OptedOutException
    _OptedOutException,

    -- ** PlatformApplicationDisabledException
    _PlatformApplicationDisabledException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** StaleTagException
    _StaleTagException,

    -- ** SubscriptionLimitExceededException
    _SubscriptionLimitExceededException,

    -- ** TagLimitExceededException
    _TagLimitExceededException,

    -- ** TagPolicyException
    _TagPolicyException,

    -- ** ThrottledException
    _ThrottledException,

    -- ** TooManyEntriesInBatchRequestException
    _TooManyEntriesInBatchRequestException,

    -- ** TopicLimitExceededException
    _TopicLimitExceededException,

    -- ** UserErrorException
    _UserErrorException,

    -- ** ValidationException
    _ValidationException,

    -- ** VerificationException
    _VerificationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AddPermission
    AddPermission (AddPermission'),
    newAddPermission,
    AddPermissionResponse (AddPermissionResponse'),
    newAddPermissionResponse,

    -- ** CheckIfPhoneNumberIsOptedOut
    CheckIfPhoneNumberIsOptedOut (CheckIfPhoneNumberIsOptedOut'),
    newCheckIfPhoneNumberIsOptedOut,
    CheckIfPhoneNumberIsOptedOutResponse (CheckIfPhoneNumberIsOptedOutResponse'),
    newCheckIfPhoneNumberIsOptedOutResponse,

    -- ** ConfirmSubscription
    ConfirmSubscription (ConfirmSubscription'),
    newConfirmSubscription,
    ConfirmSubscriptionResponse (ConfirmSubscriptionResponse'),
    newConfirmSubscriptionResponse,

    -- ** CreatePlatformApplication
    CreatePlatformApplication (CreatePlatformApplication'),
    newCreatePlatformApplication,
    CreatePlatformApplicationResponse (CreatePlatformApplicationResponse'),
    newCreatePlatformApplicationResponse,

    -- ** CreatePlatformEndpoint
    CreatePlatformEndpoint (CreatePlatformEndpoint'),
    newCreatePlatformEndpoint,
    CreatePlatformEndpointResponse (CreatePlatformEndpointResponse'),
    newCreatePlatformEndpointResponse,

    -- ** CreateSMSSandboxPhoneNumber
    CreateSMSSandboxPhoneNumber (CreateSMSSandboxPhoneNumber'),
    newCreateSMSSandboxPhoneNumber,
    CreateSMSSandboxPhoneNumberResponse (CreateSMSSandboxPhoneNumberResponse'),
    newCreateSMSSandboxPhoneNumberResponse,

    -- ** CreateTopic
    CreateTopic (CreateTopic'),
    newCreateTopic,
    CreateTopicResponse (CreateTopicResponse'),
    newCreateTopicResponse,

    -- ** DeleteEndpoint
    DeleteEndpoint (DeleteEndpoint'),
    newDeleteEndpoint,
    DeleteEndpointResponse (DeleteEndpointResponse'),
    newDeleteEndpointResponse,

    -- ** DeletePlatformApplication
    DeletePlatformApplication (DeletePlatformApplication'),
    newDeletePlatformApplication,
    DeletePlatformApplicationResponse (DeletePlatformApplicationResponse'),
    newDeletePlatformApplicationResponse,

    -- ** DeleteSMSSandboxPhoneNumber
    DeleteSMSSandboxPhoneNumber (DeleteSMSSandboxPhoneNumber'),
    newDeleteSMSSandboxPhoneNumber,
    DeleteSMSSandboxPhoneNumberResponse (DeleteSMSSandboxPhoneNumberResponse'),
    newDeleteSMSSandboxPhoneNumberResponse,

    -- ** DeleteTopic
    DeleteTopic (DeleteTopic'),
    newDeleteTopic,
    DeleteTopicResponse (DeleteTopicResponse'),
    newDeleteTopicResponse,

    -- ** GetDataProtectionPolicy
    GetDataProtectionPolicy (GetDataProtectionPolicy'),
    newGetDataProtectionPolicy,
    GetDataProtectionPolicyResponse (GetDataProtectionPolicyResponse'),
    newGetDataProtectionPolicyResponse,

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

    -- ** GetSMSAttributes
    GetSMSAttributes (GetSMSAttributes'),
    newGetSMSAttributes,
    GetSMSAttributesResponse (GetSMSAttributesResponse'),
    newGetSMSAttributesResponse,

    -- ** GetSMSSandboxAccountStatus
    GetSMSSandboxAccountStatus (GetSMSSandboxAccountStatus'),
    newGetSMSSandboxAccountStatus,
    GetSMSSandboxAccountStatusResponse (GetSMSSandboxAccountStatusResponse'),
    newGetSMSSandboxAccountStatusResponse,

    -- ** GetSubscriptionAttributes
    GetSubscriptionAttributes (GetSubscriptionAttributes'),
    newGetSubscriptionAttributes,
    GetSubscriptionAttributesResponse (GetSubscriptionAttributesResponse'),
    newGetSubscriptionAttributesResponse,

    -- ** GetTopicAttributes
    GetTopicAttributes (GetTopicAttributes'),
    newGetTopicAttributes,
    GetTopicAttributesResponse (GetTopicAttributesResponse'),
    newGetTopicAttributesResponse,

    -- ** ListEndpointsByPlatformApplication (Paginated)
    ListEndpointsByPlatformApplication (ListEndpointsByPlatformApplication'),
    newListEndpointsByPlatformApplication,
    ListEndpointsByPlatformApplicationResponse (ListEndpointsByPlatformApplicationResponse'),
    newListEndpointsByPlatformApplicationResponse,

    -- ** ListOriginationNumbers (Paginated)
    ListOriginationNumbers (ListOriginationNumbers'),
    newListOriginationNumbers,
    ListOriginationNumbersResponse (ListOriginationNumbersResponse'),
    newListOriginationNumbersResponse,

    -- ** ListPhoneNumbersOptedOut (Paginated)
    ListPhoneNumbersOptedOut (ListPhoneNumbersOptedOut'),
    newListPhoneNumbersOptedOut,
    ListPhoneNumbersOptedOutResponse (ListPhoneNumbersOptedOutResponse'),
    newListPhoneNumbersOptedOutResponse,

    -- ** ListPlatformApplications (Paginated)
    ListPlatformApplications (ListPlatformApplications'),
    newListPlatformApplications,
    ListPlatformApplicationsResponse (ListPlatformApplicationsResponse'),
    newListPlatformApplicationsResponse,

    -- ** ListSMSSandboxPhoneNumbers (Paginated)
    ListSMSSandboxPhoneNumbers (ListSMSSandboxPhoneNumbers'),
    newListSMSSandboxPhoneNumbers,
    ListSMSSandboxPhoneNumbersResponse (ListSMSSandboxPhoneNumbersResponse'),
    newListSMSSandboxPhoneNumbersResponse,

    -- ** ListSubscriptions (Paginated)
    ListSubscriptions (ListSubscriptions'),
    newListSubscriptions,
    ListSubscriptionsResponse (ListSubscriptionsResponse'),
    newListSubscriptionsResponse,

    -- ** ListSubscriptionsByTopic (Paginated)
    ListSubscriptionsByTopic (ListSubscriptionsByTopic'),
    newListSubscriptionsByTopic,
    ListSubscriptionsByTopicResponse (ListSubscriptionsByTopicResponse'),
    newListSubscriptionsByTopicResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListTopics (Paginated)
    ListTopics (ListTopics'),
    newListTopics,
    ListTopicsResponse (ListTopicsResponse'),
    newListTopicsResponse,

    -- ** OptInPhoneNumber
    OptInPhoneNumber (OptInPhoneNumber'),
    newOptInPhoneNumber,
    OptInPhoneNumberResponse (OptInPhoneNumberResponse'),
    newOptInPhoneNumberResponse,

    -- ** Publish
    Publish (Publish'),
    newPublish,
    PublishResponse (PublishResponse'),
    newPublishResponse,

    -- ** PublishBatch
    PublishBatch (PublishBatch'),
    newPublishBatch,
    PublishBatchResponse (PublishBatchResponse'),
    newPublishBatchResponse,

    -- ** PutDataProtectionPolicy
    PutDataProtectionPolicy (PutDataProtectionPolicy'),
    newPutDataProtectionPolicy,
    PutDataProtectionPolicyResponse (PutDataProtectionPolicyResponse'),
    newPutDataProtectionPolicyResponse,

    -- ** RemovePermission
    RemovePermission (RemovePermission'),
    newRemovePermission,
    RemovePermissionResponse (RemovePermissionResponse'),
    newRemovePermissionResponse,

    -- ** SetEndpointAttributes
    SetEndpointAttributes (SetEndpointAttributes'),
    newSetEndpointAttributes,
    SetEndpointAttributesResponse (SetEndpointAttributesResponse'),
    newSetEndpointAttributesResponse,

    -- ** SetPlatformApplicationAttributes
    SetPlatformApplicationAttributes (SetPlatformApplicationAttributes'),
    newSetPlatformApplicationAttributes,
    SetPlatformApplicationAttributesResponse (SetPlatformApplicationAttributesResponse'),
    newSetPlatformApplicationAttributesResponse,

    -- ** SetSMSAttributes
    SetSMSAttributes (SetSMSAttributes'),
    newSetSMSAttributes,
    SetSMSAttributesResponse (SetSMSAttributesResponse'),
    newSetSMSAttributesResponse,

    -- ** SetSubscriptionAttributes
    SetSubscriptionAttributes (SetSubscriptionAttributes'),
    newSetSubscriptionAttributes,
    SetSubscriptionAttributesResponse (SetSubscriptionAttributesResponse'),
    newSetSubscriptionAttributesResponse,

    -- ** SetTopicAttributes
    SetTopicAttributes (SetTopicAttributes'),
    newSetTopicAttributes,
    SetTopicAttributesResponse (SetTopicAttributesResponse'),
    newSetTopicAttributesResponse,

    -- ** Subscribe
    Subscribe (Subscribe'),
    newSubscribe,
    SubscribeResponse (SubscribeResponse'),
    newSubscribeResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** Unsubscribe
    Unsubscribe (Unsubscribe'),
    newUnsubscribe,
    UnsubscribeResponse (UnsubscribeResponse'),
    newUnsubscribeResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** VerifySMSSandboxPhoneNumber
    VerifySMSSandboxPhoneNumber (VerifySMSSandboxPhoneNumber'),
    newVerifySMSSandboxPhoneNumber,
    VerifySMSSandboxPhoneNumberResponse (VerifySMSSandboxPhoneNumberResponse'),
    newVerifySMSSandboxPhoneNumberResponse,

    -- * Types

    -- ** LanguageCodeString
    LanguageCodeString (..),

    -- ** NumberCapability
    NumberCapability (..),

    -- ** RouteType
    RouteType (..),

    -- ** SMSSandboxPhoneNumberVerificationStatus
    SMSSandboxPhoneNumberVerificationStatus (..),

    -- ** BatchResultErrorEntry
    BatchResultErrorEntry (BatchResultErrorEntry'),
    newBatchResultErrorEntry,

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

    -- ** PublishBatchRequestEntry
    PublishBatchRequestEntry (PublishBatchRequestEntry'),
    newPublishBatchRequestEntry,

    -- ** PublishBatchResultEntry
    PublishBatchResultEntry (PublishBatchResultEntry'),
    newPublishBatchResultEntry,

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
import Amazonka.SNS.GetDataProtectionPolicy
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
import Amazonka.SNS.PublishBatch
import Amazonka.SNS.PutDataProtectionPolicy
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
