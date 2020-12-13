{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.BounceAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.BounceAction
  ( BounceAction (..),

    -- * Smart constructor
    mkBounceAction,

    -- * Lenses
    baSmtpReplyCode,
    baSender,
    baTopicARN,
    baMessage,
    baStatusCode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | When included in a receipt rule, this action rejects the received email by returning a bounce response to the sender and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS).
--
-- For information about sending a bounce message in response to a received email, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-bounce.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkBounceAction' smart constructor.
data BounceAction = BounceAction'
  { -- | The SMTP reply code, as defined by <https://tools.ietf.org/html/rfc5321 RFC 5321> .
    smtpReplyCode :: Lude.Text,
    -- | The email address of the sender of the bounced email. This is the address from which the bounce message will be sent.
    sender :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when the bounce action is taken. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
    topicARN :: Lude.Maybe Lude.Text,
    -- | Human-readable text to include in the bounce message.
    message :: Lude.Text,
    -- | The SMTP enhanced status code, as defined by <https://tools.ietf.org/html/rfc3463 RFC 3463> .
    statusCode :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BounceAction' with the minimum fields required to make a request.
--
-- * 'smtpReplyCode' - The SMTP reply code, as defined by <https://tools.ietf.org/html/rfc5321 RFC 5321> .
-- * 'sender' - The email address of the sender of the bounced email. This is the address from which the bounce message will be sent.
-- * 'topicARN' - The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when the bounce action is taken. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
-- * 'message' - Human-readable text to include in the bounce message.
-- * 'statusCode' - The SMTP enhanced status code, as defined by <https://tools.ietf.org/html/rfc3463 RFC 3463> .
mkBounceAction ::
  -- | 'smtpReplyCode'
  Lude.Text ->
  -- | 'sender'
  Lude.Text ->
  -- | 'message'
  Lude.Text ->
  BounceAction
mkBounceAction pSmtpReplyCode_ pSender_ pMessage_ =
  BounceAction'
    { smtpReplyCode = pSmtpReplyCode_,
      sender = pSender_,
      topicARN = Lude.Nothing,
      message = pMessage_,
      statusCode = Lude.Nothing
    }

-- | The SMTP reply code, as defined by <https://tools.ietf.org/html/rfc5321 RFC 5321> .
--
-- /Note:/ Consider using 'smtpReplyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baSmtpReplyCode :: Lens.Lens' BounceAction Lude.Text
baSmtpReplyCode = Lens.lens (smtpReplyCode :: BounceAction -> Lude.Text) (\s a -> s {smtpReplyCode = a} :: BounceAction)
{-# DEPRECATED baSmtpReplyCode "Use generic-lens or generic-optics with 'smtpReplyCode' instead." #-}

-- | The email address of the sender of the bounced email. This is the address from which the bounce message will be sent.
--
-- /Note:/ Consider using 'sender' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baSender :: Lens.Lens' BounceAction Lude.Text
baSender = Lens.lens (sender :: BounceAction -> Lude.Text) (\s a -> s {sender = a} :: BounceAction)
{-# DEPRECATED baSender "Use generic-lens or generic-optics with 'sender' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when the bounce action is taken. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
--
-- /Note:/ Consider using 'topicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baTopicARN :: Lens.Lens' BounceAction (Lude.Maybe Lude.Text)
baTopicARN = Lens.lens (topicARN :: BounceAction -> Lude.Maybe Lude.Text) (\s a -> s {topicARN = a} :: BounceAction)
{-# DEPRECATED baTopicARN "Use generic-lens or generic-optics with 'topicARN' instead." #-}

-- | Human-readable text to include in the bounce message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baMessage :: Lens.Lens' BounceAction Lude.Text
baMessage = Lens.lens (message :: BounceAction -> Lude.Text) (\s a -> s {message = a} :: BounceAction)
{-# DEPRECATED baMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The SMTP enhanced status code, as defined by <https://tools.ietf.org/html/rfc3463 RFC 3463> .
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baStatusCode :: Lens.Lens' BounceAction (Lude.Maybe Lude.Text)
baStatusCode = Lens.lens (statusCode :: BounceAction -> Lude.Maybe Lude.Text) (\s a -> s {statusCode = a} :: BounceAction)
{-# DEPRECATED baStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

instance Lude.FromXML BounceAction where
  parseXML x =
    BounceAction'
      Lude.<$> (x Lude..@ "SmtpReplyCode")
      Lude.<*> (x Lude..@ "Sender")
      Lude.<*> (x Lude..@? "TopicArn")
      Lude.<*> (x Lude..@ "Message")
      Lude.<*> (x Lude..@? "StatusCode")

instance Lude.ToQuery BounceAction where
  toQuery BounceAction' {..} =
    Lude.mconcat
      [ "SmtpReplyCode" Lude.=: smtpReplyCode,
        "Sender" Lude.=: sender,
        "TopicArn" Lude.=: topicARN,
        "Message" Lude.=: message,
        "StatusCode" Lude.=: statusCode
      ]
