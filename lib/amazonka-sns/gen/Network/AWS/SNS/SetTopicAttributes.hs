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
-- Module      : Network.AWS.SNS.SetTopicAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows a topic owner to set an attribute of the topic to a new value.
module Network.AWS.SNS.SetTopicAttributes
  ( -- * Creating a Request
    setTopicAttributes,
    SetTopicAttributes,

    -- * Request Lenses
    staAttributeValue,
    staTopicARN,
    staAttributeName,

    -- * Destructuring the Response
    setTopicAttributesResponse,
    SetTopicAttributesResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types

-- | Input for SetTopicAttributes action.
--
--
--
-- /See:/ 'setTopicAttributes' smart constructor.
data SetTopicAttributes = SetTopicAttributes'
  { _staAttributeValue ::
      !(Maybe Text),
    _staTopicARN :: !Text,
    _staAttributeName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SetTopicAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'staAttributeValue' - The new value for the attribute.
--
-- * 'staTopicARN' - The ARN of the topic to modify.
--
-- * 'staAttributeName' - A map of attributes with their corresponding values. The following lists the names, descriptions, and values of the special request parameters that the @SetTopicAttributes@ action uses:     * @DeliveryPolicy@ – The policy that defines how Amazon SNS retries failed deliveries to HTTP/S endpoints.     * @DisplayName@ – The display name to use for a topic with SMS subscriptions.     * @Policy@ – The policy that defines who can access your topic. By default, only the topic owner can publish or subscribe to the topic. The following attribute applies only to <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html server-side-encryption> :     * @KmsMasterKeyId@ – The ID of an AWS-managed customer master key (CMK) for Amazon SNS or a custom CMK. For more information, see <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html#sse-key-terms Key Terms> . For more examples, see <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters KeyId> in the /AWS Key Management Service API Reference/ .  The following attribute applies only to <https://docs.aws.amazon.com/sns/latest/dg/sns-fifo-topics.html FIFO topics> :     * @ContentBasedDeduplication@ – Enables content-based deduplication for FIFO topics.      * By default, @ContentBasedDeduplication@ is set to @false@ . If you create a FIFO topic and this attribute is @false@ , you must specify a value for the @MessageDeduplicationId@ parameter for the <https://docs.aws.amazon.com/sns/latest/api/API_Publish.html Publish> action.      * When you set @ContentBasedDeduplication@ to @true@ , Amazon SNS uses a SHA-256 hash to generate the @MessageDeduplicationId@ using the body of the message (but not the attributes of the message). (Optional) To override the generated value, you can specify a value for the the @MessageDeduplicationId@ parameter for the @Publish@ action.
setTopicAttributes ::
  -- | 'staTopicARN'
  Text ->
  -- | 'staAttributeName'
  Text ->
  SetTopicAttributes
setTopicAttributes pTopicARN_ pAttributeName_ =
  SetTopicAttributes'
    { _staAttributeValue = Nothing,
      _staTopicARN = pTopicARN_,
      _staAttributeName = pAttributeName_
    }

-- | The new value for the attribute.
staAttributeValue :: Lens' SetTopicAttributes (Maybe Text)
staAttributeValue = lens _staAttributeValue (\s a -> s {_staAttributeValue = a})

-- | The ARN of the topic to modify.
staTopicARN :: Lens' SetTopicAttributes Text
staTopicARN = lens _staTopicARN (\s a -> s {_staTopicARN = a})

-- | A map of attributes with their corresponding values. The following lists the names, descriptions, and values of the special request parameters that the @SetTopicAttributes@ action uses:     * @DeliveryPolicy@ – The policy that defines how Amazon SNS retries failed deliveries to HTTP/S endpoints.     * @DisplayName@ – The display name to use for a topic with SMS subscriptions.     * @Policy@ – The policy that defines who can access your topic. By default, only the topic owner can publish or subscribe to the topic. The following attribute applies only to <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html server-side-encryption> :     * @KmsMasterKeyId@ – The ID of an AWS-managed customer master key (CMK) for Amazon SNS or a custom CMK. For more information, see <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html#sse-key-terms Key Terms> . For more examples, see <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters KeyId> in the /AWS Key Management Service API Reference/ .  The following attribute applies only to <https://docs.aws.amazon.com/sns/latest/dg/sns-fifo-topics.html FIFO topics> :     * @ContentBasedDeduplication@ – Enables content-based deduplication for FIFO topics.      * By default, @ContentBasedDeduplication@ is set to @false@ . If you create a FIFO topic and this attribute is @false@ , you must specify a value for the @MessageDeduplicationId@ parameter for the <https://docs.aws.amazon.com/sns/latest/api/API_Publish.html Publish> action.      * When you set @ContentBasedDeduplication@ to @true@ , Amazon SNS uses a SHA-256 hash to generate the @MessageDeduplicationId@ using the body of the message (but not the attributes of the message). (Optional) To override the generated value, you can specify a value for the the @MessageDeduplicationId@ parameter for the @Publish@ action.
staAttributeName :: Lens' SetTopicAttributes Text
staAttributeName = lens _staAttributeName (\s a -> s {_staAttributeName = a})

instance AWSRequest SetTopicAttributes where
  type Rs SetTopicAttributes = SetTopicAttributesResponse
  request = postQuery sns
  response = receiveNull SetTopicAttributesResponse'

instance Hashable SetTopicAttributes

instance NFData SetTopicAttributes

instance ToHeaders SetTopicAttributes where
  toHeaders = const mempty

instance ToPath SetTopicAttributes where
  toPath = const "/"

instance ToQuery SetTopicAttributes where
  toQuery SetTopicAttributes' {..} =
    mconcat
      [ "Action" =: ("SetTopicAttributes" :: ByteString),
        "Version" =: ("2010-03-31" :: ByteString),
        "AttributeValue" =: _staAttributeValue,
        "TopicArn" =: _staTopicARN,
        "AttributeName" =: _staAttributeName
      ]

-- | /See:/ 'setTopicAttributesResponse' smart constructor.
data SetTopicAttributesResponse = SetTopicAttributesResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SetTopicAttributesResponse' with the minimum fields required to make a request.
setTopicAttributesResponse ::
  SetTopicAttributesResponse
setTopicAttributesResponse = SetTopicAttributesResponse'

instance NFData SetTopicAttributesResponse
