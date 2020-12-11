-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.SNSAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.SNSAction
  ( SNSAction (..),

    -- * Smart constructor
    mkSNSAction,

    -- * Lenses
    saEncoding,
    saTopicARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SES.Types.SNSActionEncoding

-- | When included in a receipt rule, this action publishes a notification to Amazon Simple Notification Service (Amazon SNS). This action includes a complete copy of the email content in the Amazon SNS notifications. Amazon SNS notifications for all other actions simply provide information about the email. They do not include the email content itself.
--
-- If you own the Amazon SNS topic, you don't need to do anything to give Amazon SES permission to publish emails to it. However, if you don't own the Amazon SNS topic, you need to attach a policy to the topic to give Amazon SES permissions to access it. For information about giving permissions, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide> .
-- /Important:/ You can only publish emails that are 150 KB or less (including the header) to Amazon SNS. Larger emails will bounce. If you anticipate emails larger than 150 KB, use the S3 action instead.
-- For information about using a receipt rule to publish an Amazon SNS notification, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-sns.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkSNSAction' smart constructor.
data SNSAction = SNSAction'
  { encoding ::
      Lude.Maybe SNSActionEncoding,
    topicARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SNSAction' with the minimum fields required to make a request.
--
-- * 'encoding' - The encoding to use for the email within the Amazon SNS notification. UTF-8 is easier to use, but may not preserve all special characters when a message was encoded with a different encoding format. Base64 preserves all special characters. The default value is UTF-8.
-- * 'topicARN' - The Amazon Resource Name (ARN) of the Amazon SNS topic to notify. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
mkSNSAction ::
  -- | 'topicARN'
  Lude.Text ->
  SNSAction
mkSNSAction pTopicARN_ =
  SNSAction' {encoding = Lude.Nothing, topicARN = pTopicARN_}

-- | The encoding to use for the email within the Amazon SNS notification. UTF-8 is easier to use, but may not preserve all special characters when a message was encoded with a different encoding format. Base64 preserves all special characters. The default value is UTF-8.
--
-- /Note:/ Consider using 'encoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saEncoding :: Lens.Lens' SNSAction (Lude.Maybe SNSActionEncoding)
saEncoding = Lens.lens (encoding :: SNSAction -> Lude.Maybe SNSActionEncoding) (\s a -> s {encoding = a} :: SNSAction)
{-# DEPRECATED saEncoding "Use generic-lens or generic-optics with 'encoding' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to notify. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
--
-- /Note:/ Consider using 'topicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saTopicARN :: Lens.Lens' SNSAction Lude.Text
saTopicARN = Lens.lens (topicARN :: SNSAction -> Lude.Text) (\s a -> s {topicARN = a} :: SNSAction)
{-# DEPRECATED saTopicARN "Use generic-lens or generic-optics with 'topicARN' instead." #-}

instance Lude.FromXML SNSAction where
  parseXML x =
    SNSAction'
      Lude.<$> (x Lude..@? "Encoding") Lude.<*> (x Lude..@ "TopicArn")

instance Lude.ToQuery SNSAction where
  toQuery SNSAction' {..} =
    Lude.mconcat
      ["Encoding" Lude.=: encoding, "TopicArn" Lude.=: topicARN]
