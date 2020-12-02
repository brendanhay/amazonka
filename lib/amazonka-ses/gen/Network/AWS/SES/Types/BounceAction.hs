{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.BounceAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.BounceAction where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | When included in a receipt rule, this action rejects the received email by returning a bounce response to the sender and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS).
--
--
-- For information about sending a bounce message in response to a received email, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-bounce.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'bounceAction' smart constructor.
data BounceAction = BounceAction'
  { _baTopicARN :: !(Maybe Text),
    _baStatusCode :: !(Maybe Text),
    _baSmtpReplyCode :: !Text,
    _baMessage :: !Text,
    _baSender :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BounceAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'baTopicARN' - The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when the bounce action is taken. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
--
-- * 'baStatusCode' - The SMTP enhanced status code, as defined by <https://tools.ietf.org/html/rfc3463 RFC 3463> .
--
-- * 'baSmtpReplyCode' - The SMTP reply code, as defined by <https://tools.ietf.org/html/rfc5321 RFC 5321> .
--
-- * 'baMessage' - Human-readable text to include in the bounce message.
--
-- * 'baSender' - The email address of the sender of the bounced email. This is the address from which the bounce message will be sent.
bounceAction ::
  -- | 'baSmtpReplyCode'
  Text ->
  -- | 'baMessage'
  Text ->
  -- | 'baSender'
  Text ->
  BounceAction
bounceAction pSmtpReplyCode_ pMessage_ pSender_ =
  BounceAction'
    { _baTopicARN = Nothing,
      _baStatusCode = Nothing,
      _baSmtpReplyCode = pSmtpReplyCode_,
      _baMessage = pMessage_,
      _baSender = pSender_
    }

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when the bounce action is taken. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
baTopicARN :: Lens' BounceAction (Maybe Text)
baTopicARN = lens _baTopicARN (\s a -> s {_baTopicARN = a})

-- | The SMTP enhanced status code, as defined by <https://tools.ietf.org/html/rfc3463 RFC 3463> .
baStatusCode :: Lens' BounceAction (Maybe Text)
baStatusCode = lens _baStatusCode (\s a -> s {_baStatusCode = a})

-- | The SMTP reply code, as defined by <https://tools.ietf.org/html/rfc5321 RFC 5321> .
baSmtpReplyCode :: Lens' BounceAction Text
baSmtpReplyCode = lens _baSmtpReplyCode (\s a -> s {_baSmtpReplyCode = a})

-- | Human-readable text to include in the bounce message.
baMessage :: Lens' BounceAction Text
baMessage = lens _baMessage (\s a -> s {_baMessage = a})

-- | The email address of the sender of the bounced email. This is the address from which the bounce message will be sent.
baSender :: Lens' BounceAction Text
baSender = lens _baSender (\s a -> s {_baSender = a})

instance FromXML BounceAction where
  parseXML x =
    BounceAction'
      <$> (x .@? "TopicArn")
      <*> (x .@? "StatusCode")
      <*> (x .@ "SmtpReplyCode")
      <*> (x .@ "Message")
      <*> (x .@ "Sender")

instance Hashable BounceAction

instance NFData BounceAction

instance ToQuery BounceAction where
  toQuery BounceAction' {..} =
    mconcat
      [ "TopicArn" =: _baTopicARN,
        "StatusCode" =: _baStatusCode,
        "SmtpReplyCode" =: _baSmtpReplyCode,
        "Message" =: _baMessage,
        "Sender" =: _baSender
      ]
