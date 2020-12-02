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
-- Module      : Network.AWS.SNS.CreateTopic
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a topic to which notifications can be published. Users can create at most 100,000 standard topics (at most 1,000 FIFO topics). For more information, see <http://aws.amazon.com/sns/ https://aws.amazon.com/sns> . This action is idempotent, so if the requester already owns a topic with the specified name, that topic's ARN is returned without creating a new topic.
module Network.AWS.SNS.CreateTopic
  ( -- * Creating a Request
    createTopic,
    CreateTopic,

    -- * Request Lenses
    ctAttributes,
    ctTags,
    ctName,

    -- * Destructuring the Response
    createTopicResponse,
    CreateTopicResponse,

    -- * Response Lenses
    ctrsTopicARN,
    ctrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types

-- | Input for CreateTopic action.
--
--
--
-- /See:/ 'createTopic' smart constructor.
data CreateTopic = CreateTopic'
  { _ctAttributes ::
      !(Maybe (Map Text (Text))),
    _ctTags :: !(Maybe [Tag]),
    _ctName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTopic' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctAttributes' - A map of attributes with their corresponding values. The following lists the names, descriptions, and values of the special request parameters that the @CreateTopic@ action uses:     * @DeliveryPolicy@ – The policy that defines how Amazon SNS retries failed deliveries to HTTP/S endpoints.     * @DisplayName@ – The display name to use for a topic with SMS subscriptions.     * @FifoTopic@ – Set to true to create a FIFO topic.     * @Policy@ – The policy that defines who can access your topic. By default, only the topic owner can publish or subscribe to the topic. The following attribute applies only to <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html server-side-encryption> :     * @KmsMasterKeyId@ – The ID of an AWS-managed customer master key (CMK) for Amazon SNS or a custom CMK. For more information, see <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html#sse-key-terms Key Terms> . For more examples, see <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters KeyId> in the /AWS Key Management Service API Reference/ .  The following attributes apply only to <https://docs.aws.amazon.com/sns/latest/dg/sns-fifo-topics.html FIFO topics> :     * @FifoTopic@ – When this is set to @true@ , a FIFO topic is created.     * @ContentBasedDeduplication@ – Enables content-based deduplication for FIFO topics.      * By default, @ContentBasedDeduplication@ is set to @false@ . If you create a FIFO topic and this attribute is @false@ , you must specify a value for the @MessageDeduplicationId@ parameter for the <https://docs.aws.amazon.com/sns/latest/api/API_Publish.html Publish> action.      * When you set @ContentBasedDeduplication@ to @true@ , Amazon SNS uses a SHA-256 hash to generate the @MessageDeduplicationId@ using the body of the message (but not the attributes of the message). (Optional) To override the generated value, you can specify a value for the the @MessageDeduplicationId@ parameter for the @Publish@ action.
--
-- * 'ctTags' - The list of tags to add to a new topic.
--
-- * 'ctName' - The name of the topic you want to create. Constraints: Topic names must be made up of only uppercase and lowercase ASCII letters, numbers, underscores, and hyphens, and must be between 1 and 256 characters long. For a FIFO (first-in-first-out) topic, the name must end with the @.fifo@ suffix.
createTopic ::
  -- | 'ctName'
  Text ->
  CreateTopic
createTopic pName_ =
  CreateTopic'
    { _ctAttributes = Nothing,
      _ctTags = Nothing,
      _ctName = pName_
    }

-- | A map of attributes with their corresponding values. The following lists the names, descriptions, and values of the special request parameters that the @CreateTopic@ action uses:     * @DeliveryPolicy@ – The policy that defines how Amazon SNS retries failed deliveries to HTTP/S endpoints.     * @DisplayName@ – The display name to use for a topic with SMS subscriptions.     * @FifoTopic@ – Set to true to create a FIFO topic.     * @Policy@ – The policy that defines who can access your topic. By default, only the topic owner can publish or subscribe to the topic. The following attribute applies only to <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html server-side-encryption> :     * @KmsMasterKeyId@ – The ID of an AWS-managed customer master key (CMK) for Amazon SNS or a custom CMK. For more information, see <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html#sse-key-terms Key Terms> . For more examples, see <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters KeyId> in the /AWS Key Management Service API Reference/ .  The following attributes apply only to <https://docs.aws.amazon.com/sns/latest/dg/sns-fifo-topics.html FIFO topics> :     * @FifoTopic@ – When this is set to @true@ , a FIFO topic is created.     * @ContentBasedDeduplication@ – Enables content-based deduplication for FIFO topics.      * By default, @ContentBasedDeduplication@ is set to @false@ . If you create a FIFO topic and this attribute is @false@ , you must specify a value for the @MessageDeduplicationId@ parameter for the <https://docs.aws.amazon.com/sns/latest/api/API_Publish.html Publish> action.      * When you set @ContentBasedDeduplication@ to @true@ , Amazon SNS uses a SHA-256 hash to generate the @MessageDeduplicationId@ using the body of the message (but not the attributes of the message). (Optional) To override the generated value, you can specify a value for the the @MessageDeduplicationId@ parameter for the @Publish@ action.
ctAttributes :: Lens' CreateTopic (HashMap Text (Text))
ctAttributes = lens _ctAttributes (\s a -> s {_ctAttributes = a}) . _Default . _Map

-- | The list of tags to add to a new topic.
ctTags :: Lens' CreateTopic [Tag]
ctTags = lens _ctTags (\s a -> s {_ctTags = a}) . _Default . _Coerce

-- | The name of the topic you want to create. Constraints: Topic names must be made up of only uppercase and lowercase ASCII letters, numbers, underscores, and hyphens, and must be between 1 and 256 characters long. For a FIFO (first-in-first-out) topic, the name must end with the @.fifo@ suffix.
ctName :: Lens' CreateTopic Text
ctName = lens _ctName (\s a -> s {_ctName = a})

instance AWSRequest CreateTopic where
  type Rs CreateTopic = CreateTopicResponse
  request = postQuery sns
  response =
    receiveXMLWrapper
      "CreateTopicResult"
      ( \s h x ->
          CreateTopicResponse'
            <$> (x .@? "TopicArn") <*> (pure (fromEnum s))
      )

instance Hashable CreateTopic

instance NFData CreateTopic

instance ToHeaders CreateTopic where
  toHeaders = const mempty

instance ToPath CreateTopic where
  toPath = const "/"

instance ToQuery CreateTopic where
  toQuery CreateTopic' {..} =
    mconcat
      [ "Action" =: ("CreateTopic" :: ByteString),
        "Version" =: ("2010-03-31" :: ByteString),
        "Attributes"
          =: toQuery (toQueryMap "entry" "key" "value" <$> _ctAttributes),
        "Tags" =: toQuery (toQueryList "member" <$> _ctTags),
        "Name" =: _ctName
      ]

-- | Response from CreateTopic action.
--
--
--
-- /See:/ 'createTopicResponse' smart constructor.
data CreateTopicResponse = CreateTopicResponse'
  { _ctrsTopicARN ::
      !(Maybe Text),
    _ctrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTopicResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctrsTopicARN' - The Amazon Resource Name (ARN) assigned to the created topic.
--
-- * 'ctrsResponseStatus' - -- | The response status code.
createTopicResponse ::
  -- | 'ctrsResponseStatus'
  Int ->
  CreateTopicResponse
createTopicResponse pResponseStatus_ =
  CreateTopicResponse'
    { _ctrsTopicARN = Nothing,
      _ctrsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) assigned to the created topic.
ctrsTopicARN :: Lens' CreateTopicResponse (Maybe Text)
ctrsTopicARN = lens _ctrsTopicARN (\s a -> s {_ctrsTopicARN = a})

-- | -- | The response status code.
ctrsResponseStatus :: Lens' CreateTopicResponse Int
ctrsResponseStatus = lens _ctrsResponseStatus (\s a -> s {_ctrsResponseStatus = a})

instance NFData CreateTopicResponse
