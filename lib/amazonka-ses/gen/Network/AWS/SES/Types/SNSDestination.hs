-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.SNSDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.SNSDestination
  ( SNSDestination (..),

    -- * Smart constructor
    mkSNSDestination,

    -- * Lenses
    sdTopicARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the topic ARN associated with an Amazon Simple Notification Service (Amazon SNS) event destination.
--
-- Event destinations, such as Amazon SNS, are associated with configuration sets, which enable you to publish email sending events. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkSNSDestination' smart constructor.
newtype SNSDestination = SNSDestination' {topicARN :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SNSDestination' with the minimum fields required to make a request.
--
-- * 'topicARN' - The ARN of the Amazon SNS topic that email sending events will be published to. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
mkSNSDestination ::
  -- | 'topicARN'
  Lude.Text ->
  SNSDestination
mkSNSDestination pTopicARN_ =
  SNSDestination' {topicARN = pTopicARN_}

-- | The ARN of the Amazon SNS topic that email sending events will be published to. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
--
-- /Note:/ Consider using 'topicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdTopicARN :: Lens.Lens' SNSDestination Lude.Text
sdTopicARN = Lens.lens (topicARN :: SNSDestination -> Lude.Text) (\s a -> s {topicARN = a} :: SNSDestination)
{-# DEPRECATED sdTopicARN "Use generic-lens or generic-optics with 'topicARN' instead." #-}

instance Lude.FromXML SNSDestination where
  parseXML x = SNSDestination' Lude.<$> (x Lude..@ "TopicARN")

instance Lude.ToQuery SNSDestination where
  toQuery SNSDestination' {..} =
    Lude.mconcat ["TopicARN" Lude.=: topicARN]
