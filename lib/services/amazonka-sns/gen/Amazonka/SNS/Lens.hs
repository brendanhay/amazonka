{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SNS.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SNS.Lens
  ( -- * Operations

    -- ** AddPermission
    addPermission_topicArn,
    addPermission_label,
    addPermission_aWSAccountId,
    addPermission_actionName,

    -- ** CheckIfPhoneNumberIsOptedOut
    checkIfPhoneNumberIsOptedOut_phoneNumber,
    checkIfPhoneNumberIsOptedOutResponse_isOptedOut,
    checkIfPhoneNumberIsOptedOutResponse_httpStatus,

    -- ** ConfirmSubscription
    confirmSubscription_authenticateOnUnsubscribe,
    confirmSubscription_topicArn,
    confirmSubscription_token,
    confirmSubscriptionResponse_subscriptionArn,
    confirmSubscriptionResponse_httpStatus,

    -- ** CreatePlatformApplication
    createPlatformApplication_name,
    createPlatformApplication_platform,
    createPlatformApplication_attributes,
    createPlatformApplicationResponse_platformApplicationArn,
    createPlatformApplicationResponse_httpStatus,

    -- ** CreatePlatformEndpoint
    createPlatformEndpoint_attributes,
    createPlatformEndpoint_customUserData,
    createPlatformEndpoint_platformApplicationArn,
    createPlatformEndpoint_token,
    createPlatformEndpointResponse_endpointArn,
    createPlatformEndpointResponse_httpStatus,

    -- ** CreateSMSSandboxPhoneNumber
    createSMSSandboxPhoneNumber_languageCode,
    createSMSSandboxPhoneNumber_phoneNumber,
    createSMSSandboxPhoneNumberResponse_httpStatus,

    -- ** CreateTopic
    createTopic_attributes,
    createTopic_dataProtectionPolicy,
    createTopic_tags,
    createTopic_name,
    createTopicResponse_topicArn,
    createTopicResponse_httpStatus,

    -- ** DeleteEndpoint
    deleteEndpoint_endpointArn,

    -- ** DeletePlatformApplication
    deletePlatformApplication_platformApplicationArn,

    -- ** DeleteSMSSandboxPhoneNumber
    deleteSMSSandboxPhoneNumber_phoneNumber,
    deleteSMSSandboxPhoneNumberResponse_httpStatus,

    -- ** DeleteTopic
    deleteTopic_topicArn,

    -- ** GetDataProtectionPolicy
    getDataProtectionPolicy_resourceArn,
    getDataProtectionPolicyResponse_dataProtectionPolicy,
    getDataProtectionPolicyResponse_httpStatus,

    -- ** GetEndpointAttributes
    getEndpointAttributes_endpointArn,
    getEndpointAttributesResponse_attributes,
    getEndpointAttributesResponse_httpStatus,

    -- ** GetPlatformApplicationAttributes
    getPlatformApplicationAttributes_platformApplicationArn,
    getPlatformApplicationAttributesResponse_attributes,
    getPlatformApplicationAttributesResponse_httpStatus,

    -- ** GetSMSAttributes
    getSMSAttributes_attributes,
    getSMSAttributesResponse_attributes,
    getSMSAttributesResponse_httpStatus,

    -- ** GetSMSSandboxAccountStatus
    getSMSSandboxAccountStatusResponse_httpStatus,
    getSMSSandboxAccountStatusResponse_isInSandbox,

    -- ** GetSubscriptionAttributes
    getSubscriptionAttributes_subscriptionArn,
    getSubscriptionAttributesResponse_attributes,
    getSubscriptionAttributesResponse_httpStatus,

    -- ** GetTopicAttributes
    getTopicAttributes_topicArn,
    getTopicAttributesResponse_attributes,
    getTopicAttributesResponse_httpStatus,

    -- ** ListEndpointsByPlatformApplication
    listEndpointsByPlatformApplication_nextToken,
    listEndpointsByPlatformApplication_platformApplicationArn,
    listEndpointsByPlatformApplicationResponse_endpoints,
    listEndpointsByPlatformApplicationResponse_nextToken,
    listEndpointsByPlatformApplicationResponse_httpStatus,

    -- ** ListOriginationNumbers
    listOriginationNumbers_maxResults,
    listOriginationNumbers_nextToken,
    listOriginationNumbersResponse_nextToken,
    listOriginationNumbersResponse_phoneNumbers,
    listOriginationNumbersResponse_httpStatus,

    -- ** ListPhoneNumbersOptedOut
    listPhoneNumbersOptedOut_nextToken,
    listPhoneNumbersOptedOutResponse_nextToken,
    listPhoneNumbersOptedOutResponse_phoneNumbers,
    listPhoneNumbersOptedOutResponse_httpStatus,

    -- ** ListPlatformApplications
    listPlatformApplications_nextToken,
    listPlatformApplicationsResponse_nextToken,
    listPlatformApplicationsResponse_platformApplications,
    listPlatformApplicationsResponse_httpStatus,

    -- ** ListSMSSandboxPhoneNumbers
    listSMSSandboxPhoneNumbers_maxResults,
    listSMSSandboxPhoneNumbers_nextToken,
    listSMSSandboxPhoneNumbersResponse_nextToken,
    listSMSSandboxPhoneNumbersResponse_httpStatus,
    listSMSSandboxPhoneNumbersResponse_phoneNumbers,

    -- ** ListSubscriptions
    listSubscriptions_nextToken,
    listSubscriptionsResponse_nextToken,
    listSubscriptionsResponse_subscriptions,
    listSubscriptionsResponse_httpStatus,

    -- ** ListSubscriptionsByTopic
    listSubscriptionsByTopic_nextToken,
    listSubscriptionsByTopic_topicArn,
    listSubscriptionsByTopicResponse_nextToken,
    listSubscriptionsByTopicResponse_subscriptions,
    listSubscriptionsByTopicResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTopics
    listTopics_nextToken,
    listTopicsResponse_nextToken,
    listTopicsResponse_topics,
    listTopicsResponse_httpStatus,

    -- ** OptInPhoneNumber
    optInPhoneNumber_phoneNumber,
    optInPhoneNumberResponse_httpStatus,

    -- ** Publish
    publish_messageAttributes,
    publish_messageDeduplicationId,
    publish_messageGroupId,
    publish_messageStructure,
    publish_phoneNumber,
    publish_subject,
    publish_targetArn,
    publish_topicArn,
    publish_message,
    publishResponse_messageId,
    publishResponse_sequenceNumber,
    publishResponse_httpStatus,

    -- ** PublishBatch
    publishBatch_topicArn,
    publishBatch_publishBatchRequestEntries,
    publishBatchResponse_failed,
    publishBatchResponse_successful,
    publishBatchResponse_httpStatus,

    -- ** PutDataProtectionPolicy
    putDataProtectionPolicy_resourceArn,
    putDataProtectionPolicy_dataProtectionPolicy,

    -- ** RemovePermission
    removePermission_topicArn,
    removePermission_label,

    -- ** SetEndpointAttributes
    setEndpointAttributes_endpointArn,
    setEndpointAttributes_attributes,

    -- ** SetPlatformApplicationAttributes
    setPlatformApplicationAttributes_platformApplicationArn,
    setPlatformApplicationAttributes_attributes,

    -- ** SetSMSAttributes
    setSMSAttributes_attributes,
    setSMSAttributesResponse_httpStatus,

    -- ** SetSubscriptionAttributes
    setSubscriptionAttributes_attributeValue,
    setSubscriptionAttributes_subscriptionArn,
    setSubscriptionAttributes_attributeName,

    -- ** SetTopicAttributes
    setTopicAttributes_attributeValue,
    setTopicAttributes_topicArn,
    setTopicAttributes_attributeName,

    -- ** Subscribe
    subscribe_attributes,
    subscribe_endpoint,
    subscribe_returnSubscriptionArn,
    subscribe_topicArn,
    subscribe_protocol,
    subscribeResponse_subscriptionArn,
    subscribeResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** Unsubscribe
    unsubscribe_subscriptionArn,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** VerifySMSSandboxPhoneNumber
    verifySMSSandboxPhoneNumber_phoneNumber,
    verifySMSSandboxPhoneNumber_oneTimePassword,
    verifySMSSandboxPhoneNumberResponse_httpStatus,

    -- * Types

    -- ** BatchResultErrorEntry
    batchResultErrorEntry_message,
    batchResultErrorEntry_id,
    batchResultErrorEntry_code,
    batchResultErrorEntry_senderFault,

    -- ** Endpoint
    endpoint_attributes,
    endpoint_endpointArn,

    -- ** MessageAttributeValue
    messageAttributeValue_binaryValue,
    messageAttributeValue_stringValue,
    messageAttributeValue_dataType,

    -- ** PhoneNumberInformation
    phoneNumberInformation_createdAt,
    phoneNumberInformation_iso2CountryCode,
    phoneNumberInformation_numberCapabilities,
    phoneNumberInformation_phoneNumber,
    phoneNumberInformation_routeType,
    phoneNumberInformation_status,

    -- ** PlatformApplication
    platformApplication_attributes,
    platformApplication_platformApplicationArn,

    -- ** PublishBatchRequestEntry
    publishBatchRequestEntry_messageAttributes,
    publishBatchRequestEntry_messageDeduplicationId,
    publishBatchRequestEntry_messageGroupId,
    publishBatchRequestEntry_messageStructure,
    publishBatchRequestEntry_subject,
    publishBatchRequestEntry_id,
    publishBatchRequestEntry_message,

    -- ** PublishBatchResultEntry
    publishBatchResultEntry_id,
    publishBatchResultEntry_messageId,
    publishBatchResultEntry_sequenceNumber,

    -- ** SMSSandboxPhoneNumber
    sMSSandboxPhoneNumber_phoneNumber,
    sMSSandboxPhoneNumber_status,

    -- ** Subscription
    subscription_endpoint,
    subscription_owner,
    subscription_protocol,
    subscription_subscriptionArn,
    subscription_topicArn,

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
import Amazonka.SNS.GetDataProtectionPolicy
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
import Amazonka.SNS.Types.BatchResultErrorEntry
import Amazonka.SNS.Types.Endpoint
import Amazonka.SNS.Types.MessageAttributeValue
import Amazonka.SNS.Types.PhoneNumberInformation
import Amazonka.SNS.Types.PlatformApplication
import Amazonka.SNS.Types.PublishBatchRequestEntry
import Amazonka.SNS.Types.PublishBatchResultEntry
import Amazonka.SNS.Types.SMSSandboxPhoneNumber
import Amazonka.SNS.Types.Subscription
import Amazonka.SNS.Types.Tag
import Amazonka.SNS.Types.Topic
import Amazonka.SNS.Unsubscribe
import Amazonka.SNS.UntagResource
import Amazonka.SNS.VerifySMSSandboxPhoneNumber
