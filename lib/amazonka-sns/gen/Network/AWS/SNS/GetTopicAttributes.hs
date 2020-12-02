{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.GetTopicAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all of the properties of a topic. Topic properties returned might differ based on the authorization of the user.
module Network.AWS.SNS.GetTopicAttributes
  ( -- * Creating a Request
    getTopicAttributes,
    GetTopicAttributes,

    -- * Request Lenses
    gtaTopicARN,

    -- * Destructuring the Response
    getTopicAttributesResponse,
    GetTopicAttributesResponse,

    -- * Response Lenses
    gtarsAttributes,
    gtarsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types

-- | Input for GetTopicAttributes action.
--
--
--
-- /See:/ 'getTopicAttributes' smart constructor.
newtype GetTopicAttributes = GetTopicAttributes'
  { _gtaTopicARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetTopicAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtaTopicARN' - The ARN of the topic whose properties you want to get.
getTopicAttributes ::
  -- | 'gtaTopicARN'
  Text ->
  GetTopicAttributes
getTopicAttributes pTopicARN_ =
  GetTopicAttributes' {_gtaTopicARN = pTopicARN_}

-- | The ARN of the topic whose properties you want to get.
gtaTopicARN :: Lens' GetTopicAttributes Text
gtaTopicARN = lens _gtaTopicARN (\s a -> s {_gtaTopicARN = a})

instance AWSRequest GetTopicAttributes where
  type Rs GetTopicAttributes = GetTopicAttributesResponse
  request = postQuery sns
  response =
    receiveXMLWrapper
      "GetTopicAttributesResult"
      ( \s h x ->
          GetTopicAttributesResponse'
            <$> ( x .@? "Attributes" .!@ mempty
                    >>= may (parseXMLMap "entry" "key" "value")
                )
            <*> (pure (fromEnum s))
      )

instance Hashable GetTopicAttributes

instance NFData GetTopicAttributes

instance ToHeaders GetTopicAttributes where
  toHeaders = const mempty

instance ToPath GetTopicAttributes where
  toPath = const "/"

instance ToQuery GetTopicAttributes where
  toQuery GetTopicAttributes' {..} =
    mconcat
      [ "Action" =: ("GetTopicAttributes" :: ByteString),
        "Version" =: ("2010-03-31" :: ByteString),
        "TopicArn" =: _gtaTopicARN
      ]

-- | Response for GetTopicAttributes action.
--
--
--
-- /See:/ 'getTopicAttributesResponse' smart constructor.
data GetTopicAttributesResponse = GetTopicAttributesResponse'
  { _gtarsAttributes ::
      !(Maybe (Map Text (Text))),
    _gtarsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetTopicAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtarsAttributes' - A map of the topic's attributes. Attributes in this map include the following:     * @DeliveryPolicy@ – The JSON serialization of the topic's delivery policy.     * @DisplayName@ – The human-readable name used in the @From@ field for notifications to @email@ and @email-json@ endpoints.     * @Owner@ – The AWS account ID of the topic's owner.     * @Policy@ – The JSON serialization of the topic's access control policy.     * @SubscriptionsConfirmed@ – The number of confirmed subscriptions for the topic.     * @SubscriptionsDeleted@ – The number of deleted subscriptions for the topic.     * @SubscriptionsPending@ – The number of subscriptions pending confirmation for the topic.     * @TopicArn@ – The topic's ARN.     * @EffectiveDeliveryPolicy@ – The JSON serialization of the effective delivery policy, taking system defaults into account. The following attribute applies only to <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html server-side-encryption> :     * @KmsMasterKeyId@ - The ID of an AWS-managed customer master key (CMK) for Amazon SNS or a custom CMK. For more information, see <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html#sse-key-terms Key Terms> . For more examples, see <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters KeyId> in the /AWS Key Management Service API Reference/ . The following attributes apply only to <https://docs.aws.amazon.com/sns/latest/dg/sns-fifo-topics.html FIFO topics> :     * @FifoTopic@ – When this is set to @true@ , a FIFO topic is created.     * @ContentBasedDeduplication@ – Enables content-based deduplication for FIFO topics.      * By default, @ContentBasedDeduplication@ is set to @false@ . If you create a FIFO topic and this attribute is @false@ , you must specify a value for the @MessageDeduplicationId@ parameter for the <https://docs.aws.amazon.com/sns/latest/api/API_Publish.html Publish> action.      * When you set @ContentBasedDeduplication@ to @true@ , Amazon SNS uses a SHA-256 hash to generate the @MessageDeduplicationId@ using the body of the message (but not the attributes of the message). (Optional) To override the generated value, you can specify a value for the the @MessageDeduplicationId@ parameter for the @Publish@ action.
--
-- * 'gtarsResponseStatus' - -- | The response status code.
getTopicAttributesResponse ::
  -- | 'gtarsResponseStatus'
  Int ->
  GetTopicAttributesResponse
getTopicAttributesResponse pResponseStatus_ =
  GetTopicAttributesResponse'
    { _gtarsAttributes = Nothing,
      _gtarsResponseStatus = pResponseStatus_
    }

-- | A map of the topic's attributes. Attributes in this map include the following:     * @DeliveryPolicy@ – The JSON serialization of the topic's delivery policy.     * @DisplayName@ – The human-readable name used in the @From@ field for notifications to @email@ and @email-json@ endpoints.     * @Owner@ – The AWS account ID of the topic's owner.     * @Policy@ – The JSON serialization of the topic's access control policy.     * @SubscriptionsConfirmed@ – The number of confirmed subscriptions for the topic.     * @SubscriptionsDeleted@ – The number of deleted subscriptions for the topic.     * @SubscriptionsPending@ – The number of subscriptions pending confirmation for the topic.     * @TopicArn@ – The topic's ARN.     * @EffectiveDeliveryPolicy@ – The JSON serialization of the effective delivery policy, taking system defaults into account. The following attribute applies only to <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html server-side-encryption> :     * @KmsMasterKeyId@ - The ID of an AWS-managed customer master key (CMK) for Amazon SNS or a custom CMK. For more information, see <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html#sse-key-terms Key Terms> . For more examples, see <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters KeyId> in the /AWS Key Management Service API Reference/ . The following attributes apply only to <https://docs.aws.amazon.com/sns/latest/dg/sns-fifo-topics.html FIFO topics> :     * @FifoTopic@ – When this is set to @true@ , a FIFO topic is created.     * @ContentBasedDeduplication@ – Enables content-based deduplication for FIFO topics.      * By default, @ContentBasedDeduplication@ is set to @false@ . If you create a FIFO topic and this attribute is @false@ , you must specify a value for the @MessageDeduplicationId@ parameter for the <https://docs.aws.amazon.com/sns/latest/api/API_Publish.html Publish> action.      * When you set @ContentBasedDeduplication@ to @true@ , Amazon SNS uses a SHA-256 hash to generate the @MessageDeduplicationId@ using the body of the message (but not the attributes of the message). (Optional) To override the generated value, you can specify a value for the the @MessageDeduplicationId@ parameter for the @Publish@ action.
gtarsAttributes :: Lens' GetTopicAttributesResponse (HashMap Text (Text))
gtarsAttributes = lens _gtarsAttributes (\s a -> s {_gtarsAttributes = a}) . _Default . _Map

-- | -- | The response status code.
gtarsResponseStatus :: Lens' GetTopicAttributesResponse Int
gtarsResponseStatus = lens _gtarsResponseStatus (\s a -> s {_gtarsResponseStatus = a})

instance NFData GetTopicAttributesResponse
