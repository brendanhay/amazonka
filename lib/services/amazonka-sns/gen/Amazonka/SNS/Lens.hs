{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SNS.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SNS.Lens
  ( -- * Operations

    -- ** ListPhoneNumbersOptedOut
    listPhoneNumbersOptedOut_nextToken,
    listPhoneNumbersOptedOutResponse_phoneNumbers,
    listPhoneNumbersOptedOutResponse_nextToken,
    listPhoneNumbersOptedOutResponse_httpStatus,

    -- ** DeleteEndpoint
    deleteEndpoint_endpointArn,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** RemovePermission
    removePermission_topicArn,
    removePermission_label,

    -- ** DeleteTopic
    deleteTopic_topicArn,

    -- ** SetSMSAttributes
    setSMSAttributes_attributes,
    setSMSAttributesResponse_httpStatus,

    -- ** ListTopics
    listTopics_nextToken,
    listTopicsResponse_topics,
    listTopicsResponse_nextToken,
    listTopicsResponse_httpStatus,

    -- ** VerifySMSSandboxPhoneNumber
    verifySMSSandboxPhoneNumber_phoneNumber,
    verifySMSSandboxPhoneNumber_oneTimePassword,
    verifySMSSandboxPhoneNumberResponse_httpStatus,

    -- ** CreatePlatformEndpoint
    createPlatformEndpoint_customUserData,
    createPlatformEndpoint_attributes,
    createPlatformEndpoint_platformApplicationArn,
    createPlatformEndpoint_token,
    createPlatformEndpointResponse_endpointArn,
    createPlatformEndpointResponse_httpStatus,

    -- ** SetPlatformApplicationAttributes
    setPlatformApplicationAttributes_platformApplicationArn,
    setPlatformApplicationAttributes_attributes,

    -- ** ListSubscriptionsByTopic
    listSubscriptionsByTopic_nextToken,
    listSubscriptionsByTopic_topicArn,
    listSubscriptionsByTopicResponse_nextToken,
    listSubscriptionsByTopicResponse_subscriptions,
    listSubscriptionsByTopicResponse_httpStatus,

    -- ** GetTopicAttributes
    getTopicAttributes_topicArn,
    getTopicAttributesResponse_attributes,
    getTopicAttributesResponse_httpStatus,

    -- ** CreateSMSSandboxPhoneNumber
    createSMSSandboxPhoneNumber_languageCode,
    createSMSSandboxPhoneNumber_phoneNumber,
    createSMSSandboxPhoneNumberResponse_httpStatus,

    -- ** OptInPhoneNumber
    optInPhoneNumber_phoneNumber,
    optInPhoneNumberResponse_httpStatus,

    -- ** DeleteSMSSandboxPhoneNumber
    deleteSMSSandboxPhoneNumber_phoneNumber,
    deleteSMSSandboxPhoneNumberResponse_httpStatus,

    -- ** ListSMSSandboxPhoneNumbers
    listSMSSandboxPhoneNumbers_nextToken,
    listSMSSandboxPhoneNumbers_maxResults,
    listSMSSandboxPhoneNumbersResponse_nextToken,
    listSMSSandboxPhoneNumbersResponse_httpStatus,
    listSMSSandboxPhoneNumbersResponse_phoneNumbers,

    -- ** CreatePlatformApplication
    createPlatformApplication_name,
    createPlatformApplication_platform,
    createPlatformApplication_attributes,
    createPlatformApplicationResponse_platformApplicationArn,
    createPlatformApplicationResponse_httpStatus,

    -- ** GetPlatformApplicationAttributes
    getPlatformApplicationAttributes_platformApplicationArn,
    getPlatformApplicationAttributesResponse_attributes,
    getPlatformApplicationAttributesResponse_httpStatus,

    -- ** ListEndpointsByPlatformApplication
    listEndpointsByPlatformApplication_nextToken,
    listEndpointsByPlatformApplication_platformApplicationArn,
    listEndpointsByPlatformApplicationResponse_nextToken,
    listEndpointsByPlatformApplicationResponse_endpoints,
    listEndpointsByPlatformApplicationResponse_httpStatus,

    -- ** SetTopicAttributes
    setTopicAttributes_attributeValue,
    setTopicAttributes_topicArn,
    setTopicAttributes_attributeName,

    -- ** DeletePlatformApplication
    deletePlatformApplication_platformApplicationArn,

    -- ** GetSMSAttributes
    getSMSAttributes_attributes,
    getSMSAttributesResponse_attributes,
    getSMSAttributesResponse_httpStatus,

    -- ** ListPlatformApplications
    listPlatformApplications_nextToken,
    listPlatformApplicationsResponse_platformApplications,
    listPlatformApplicationsResponse_nextToken,
    listPlatformApplicationsResponse_httpStatus,

    -- ** AddPermission
    addPermission_topicArn,
    addPermission_label,
    addPermission_aWSAccountId,
    addPermission_actionName,

    -- ** GetEndpointAttributes
    getEndpointAttributes_endpointArn,
    getEndpointAttributesResponse_attributes,
    getEndpointAttributesResponse_httpStatus,

    -- ** ListSubscriptions
    listSubscriptions_nextToken,
    listSubscriptionsResponse_nextToken,
    listSubscriptionsResponse_subscriptions,
    listSubscriptionsResponse_httpStatus,

    -- ** GetSubscriptionAttributes
    getSubscriptionAttributes_subscriptionArn,
    getSubscriptionAttributesResponse_attributes,
    getSubscriptionAttributesResponse_httpStatus,

    -- ** CreateTopic
    createTopic_attributes,
    createTopic_tags,
    createTopic_name,
    createTopicResponse_topicArn,
    createTopicResponse_httpStatus,

    -- ** CheckIfPhoneNumberIsOptedOut
    checkIfPhoneNumberIsOptedOut_phoneNumber,
    checkIfPhoneNumberIsOptedOutResponse_isOptedOut,
    checkIfPhoneNumberIsOptedOutResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** Subscribe
    subscribe_returnSubscriptionArn,
    subscribe_attributes,
    subscribe_endpoint,
    subscribe_topicArn,
    subscribe_protocol,
    subscribeResponse_subscriptionArn,
    subscribeResponse_httpStatus,

    -- ** ListOriginationNumbers
    listOriginationNumbers_nextToken,
    listOriginationNumbers_maxResults,
    listOriginationNumbersResponse_nextToken,
    listOriginationNumbersResponse_phoneNumbers,
    listOriginationNumbersResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** Unsubscribe
    unsubscribe_subscriptionArn,

    -- ** GetSMSSandboxAccountStatus
    getSMSSandboxAccountStatusResponse_httpStatus,
    getSMSSandboxAccountStatusResponse_isInSandbox,

    -- ** SetEndpointAttributes
    setEndpointAttributes_endpointArn,
    setEndpointAttributes_attributes,

    -- ** SetSubscriptionAttributes
    setSubscriptionAttributes_attributeValue,
    setSubscriptionAttributes_subscriptionArn,
    setSubscriptionAttributes_attributeName,

    -- ** ConfirmSubscription
    confirmSubscription_authenticateOnUnsubscribe,
    confirmSubscription_topicArn,
    confirmSubscription_token,
    confirmSubscriptionResponse_subscriptionArn,
    confirmSubscriptionResponse_httpStatus,

    -- ** Publish
    publish_subject,
    publish_targetArn,
    publish_messageAttributes,
    publish_topicArn,
    publish_phoneNumber,
    publish_messageDeduplicationId,
    publish_messageStructure,
    publish_messageGroupId,
    publish_message,
    publishResponse_sequenceNumber,
    publishResponse_messageId,
    publishResponse_httpStatus,

    -- * Types

    -- ** Endpoint
    endpoint_attributes,
    endpoint_endpointArn,

    -- ** MessageAttributeValue
    messageAttributeValue_binaryValue,
    messageAttributeValue_stringValue,
    messageAttributeValue_dataType,

    -- ** PhoneNumberInformation
    phoneNumberInformation_status,
    phoneNumberInformation_iso2CountryCode,
    phoneNumberInformation_createdAt,
    phoneNumberInformation_phoneNumber,
    phoneNumberInformation_numberCapabilities,
    phoneNumberInformation_routeType,

    -- ** PlatformApplication
    platformApplication_platformApplicationArn,
    platformApplication_attributes,

    -- ** SMSSandboxPhoneNumber
    sMSSandboxPhoneNumber_status,
    sMSSandboxPhoneNumber_phoneNumber,

    -- ** Subscription
    subscription_protocol,
    subscription_owner,
    subscription_topicArn,
    subscription_endpoint,
    subscription_subscriptionArn,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Topic
    topic_topicArn,
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
import Amazonka.SNS.Types.Endpoint
import Amazonka.SNS.Types.MessageAttributeValue
import Amazonka.SNS.Types.PhoneNumberInformation
import Amazonka.SNS.Types.PlatformApplication
import Amazonka.SNS.Types.SMSSandboxPhoneNumber
import Amazonka.SNS.Types.Subscription
import Amazonka.SNS.Types.Tag
import Amazonka.SNS.Types.Topic
import Amazonka.SNS.Unsubscribe
import Amazonka.SNS.UntagResource
import Amazonka.SNS.VerifySMSSandboxPhoneNumber
