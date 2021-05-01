{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SNS.Lens
  ( -- * Operations

    -- ** DeletePlatformApplication
    deletePlatformApplication_platformApplicationArn,

    -- ** ConfirmSubscription
    confirmSubscription_authenticateOnUnsubscribe,
    confirmSubscription_topicArn,
    confirmSubscription_token,
    confirmSubscriptionResponse_subscriptionArn,
    confirmSubscriptionResponse_httpStatus,

    -- ** Publish
    publish_phoneNumber,
    publish_messageStructure,
    publish_messageDeduplicationId,
    publish_messageAttributes,
    publish_targetArn,
    publish_subject,
    publish_topicArn,
    publish_messageGroupId,
    publish_message,
    publishResponse_sequenceNumber,
    publishResponse_messageId,
    publishResponse_httpStatus,

    -- ** OptInPhoneNumber
    optInPhoneNumber_phoneNumber,
    optInPhoneNumberResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** Unsubscribe
    unsubscribe_subscriptionArn,

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

    -- ** SetSMSAttributes
    setSMSAttributes_attributes,
    setSMSAttributesResponse_httpStatus,

    -- ** SetPlatformApplicationAttributes
    setPlatformApplicationAttributes_platformApplicationArn,
    setPlatformApplicationAttributes_attributes,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** CreatePlatformEndpoint
    createPlatformEndpoint_customUserData,
    createPlatformEndpoint_attributes,
    createPlatformEndpoint_platformApplicationArn,
    createPlatformEndpoint_token,
    createPlatformEndpointResponse_endpointArn,
    createPlatformEndpointResponse_httpStatus,

    -- ** Subscribe
    subscribe_attributes,
    subscribe_returnSubscriptionArn,
    subscribe_endpoint,
    subscribe_topicArn,
    subscribe_protocol,
    subscribeResponse_subscriptionArn,
    subscribeResponse_httpStatus,

    -- ** ListTopics
    listTopics_nextToken,
    listTopicsResponse_nextToken,
    listTopicsResponse_topics,
    listTopicsResponse_httpStatus,

    -- ** ListSubscriptions
    listSubscriptions_nextToken,
    listSubscriptionsResponse_nextToken,
    listSubscriptionsResponse_subscriptions,
    listSubscriptionsResponse_httpStatus,

    -- ** GetSubscriptionAttributes
    getSubscriptionAttributes_subscriptionArn,
    getSubscriptionAttributesResponse_attributes,
    getSubscriptionAttributesResponse_httpStatus,

    -- ** DeleteEndpoint
    deleteEndpoint_endpointArn,

    -- ** AddPermission
    addPermission_topicArn,
    addPermission_label,
    addPermission_aWSAccountId,
    addPermission_actionName,

    -- ** ListPhoneNumbersOptedOut
    listPhoneNumbersOptedOut_nextToken,
    listPhoneNumbersOptedOutResponse_nextToken,
    listPhoneNumbersOptedOutResponse_phoneNumbers,
    listPhoneNumbersOptedOutResponse_httpStatus,

    -- ** GetEndpointAttributes
    getEndpointAttributes_endpointArn,
    getEndpointAttributesResponse_attributes,
    getEndpointAttributesResponse_httpStatus,

    -- ** GetPlatformApplicationAttributes
    getPlatformApplicationAttributes_platformApplicationArn,
    getPlatformApplicationAttributesResponse_attributes,
    getPlatformApplicationAttributesResponse_httpStatus,

    -- ** SetTopicAttributes
    setTopicAttributes_attributeValue,
    setTopicAttributes_topicArn,
    setTopicAttributes_attributeName,

    -- ** ListEndpointsByPlatformApplication
    listEndpointsByPlatformApplication_nextToken,
    listEndpointsByPlatformApplication_platformApplicationArn,
    listEndpointsByPlatformApplicationResponse_nextToken,
    listEndpointsByPlatformApplicationResponse_endpoints,
    listEndpointsByPlatformApplicationResponse_httpStatus,

    -- ** GetSMSAttributes
    getSMSAttributes_attributes,
    getSMSAttributesResponse_attributes,
    getSMSAttributesResponse_httpStatus,

    -- ** ListPlatformApplications
    listPlatformApplications_nextToken,
    listPlatformApplicationsResponse_nextToken,
    listPlatformApplicationsResponse_platformApplications,
    listPlatformApplicationsResponse_httpStatus,

    -- ** CreatePlatformApplication
    createPlatformApplication_name,
    createPlatformApplication_platform,
    createPlatformApplication_attributes,
    createPlatformApplicationResponse_platformApplicationArn,
    createPlatformApplicationResponse_httpStatus,

    -- ** SetEndpointAttributes
    setEndpointAttributes_endpointArn,
    setEndpointAttributes_attributes,

    -- ** SetSubscriptionAttributes
    setSubscriptionAttributes_attributeValue,
    setSubscriptionAttributes_subscriptionArn,
    setSubscriptionAttributes_attributeName,

    -- ** CheckIfPhoneNumberIsOptedOut
    checkIfPhoneNumberIsOptedOut_phoneNumber,
    checkIfPhoneNumberIsOptedOutResponse_isOptedOut,
    checkIfPhoneNumberIsOptedOutResponse_httpStatus,

    -- ** DeleteTopic
    deleteTopic_topicArn,

    -- ** CreateTopic
    createTopic_attributes,
    createTopic_tags,
    createTopic_name,
    createTopicResponse_topicArn,
    createTopicResponse_httpStatus,

    -- ** RemovePermission
    removePermission_topicArn,
    removePermission_label,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- * Types

    -- ** Endpoint
    endpoint_attributes,
    endpoint_endpointArn,

    -- ** MessageAttributeValue
    messageAttributeValue_stringValue,
    messageAttributeValue_binaryValue,
    messageAttributeValue_dataType,

    -- ** PlatformApplication
    platformApplication_platformApplicationArn,
    platformApplication_attributes,

    -- ** Subscription
    subscription_topicArn,
    subscription_owner,
    subscription_subscriptionArn,
    subscription_protocol,
    subscription_endpoint,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Topic
    topic_topicArn,
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
import Network.AWS.SNS.Types.Endpoint
import Network.AWS.SNS.Types.MessageAttributeValue
import Network.AWS.SNS.Types.PlatformApplication
import Network.AWS.SNS.Types.Subscription
import Network.AWS.SNS.Types.Tag
import Network.AWS.SNS.Types.Topic
import Network.AWS.SNS.Unsubscribe
import Network.AWS.SNS.UntagResource
