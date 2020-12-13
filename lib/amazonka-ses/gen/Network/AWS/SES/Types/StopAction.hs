{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.StopAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.StopAction
  ( StopAction (..),

    -- * Smart constructor
    mkStopAction,

    -- * Lenses
    sTopicARN,
    sScope,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SES.Types.StopScope

-- | When included in a receipt rule, this action terminates the evaluation of the receipt rule set and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS).
--
-- For information about setting a stop action in a receipt rule, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-stop.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkStopAction' smart constructor.
data StopAction = StopAction'
  { -- | The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when the stop action is taken. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
    topicARN :: Lude.Maybe Lude.Text,
    -- | The scope of the StopAction. The only acceptable value is @RuleSet@ .
    scope :: StopScope
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopAction' with the minimum fields required to make a request.
--
-- * 'topicARN' - The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when the stop action is taken. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
-- * 'scope' - The scope of the StopAction. The only acceptable value is @RuleSet@ .
mkStopAction ::
  -- | 'scope'
  StopScope ->
  StopAction
mkStopAction pScope_ =
  StopAction' {topicARN = Lude.Nothing, scope = pScope_}

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when the stop action is taken. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
--
-- /Note:/ Consider using 'topicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTopicARN :: Lens.Lens' StopAction (Lude.Maybe Lude.Text)
sTopicARN = Lens.lens (topicARN :: StopAction -> Lude.Maybe Lude.Text) (\s a -> s {topicARN = a} :: StopAction)
{-# DEPRECATED sTopicARN "Use generic-lens or generic-optics with 'topicARN' instead." #-}

-- | The scope of the StopAction. The only acceptable value is @RuleSet@ .
--
-- /Note:/ Consider using 'scope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sScope :: Lens.Lens' StopAction StopScope
sScope = Lens.lens (scope :: StopAction -> StopScope) (\s a -> s {scope = a} :: StopAction)
{-# DEPRECATED sScope "Use generic-lens or generic-optics with 'scope' instead." #-}

instance Lude.FromXML StopAction where
  parseXML x =
    StopAction'
      Lude.<$> (x Lude..@? "TopicArn") Lude.<*> (x Lude..@ "Scope")

instance Lude.ToQuery StopAction where
  toQuery StopAction' {..} =
    Lude.mconcat ["TopicArn" Lude.=: topicARN, "Scope" Lude.=: scope]
