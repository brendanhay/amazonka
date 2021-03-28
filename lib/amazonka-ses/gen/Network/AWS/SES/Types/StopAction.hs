{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.StopAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.StopAction
  ( StopAction (..)
  -- * Smart constructor
  , mkStopAction
  -- * Lenses
  , sScope
  , sTopicArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SES.Types.AmazonResourceName as Types
import qualified Network.AWS.SES.Types.StopScope as Types

-- | When included in a receipt rule, this action terminates the evaluation of the receipt rule set and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS).
--
-- For information about setting a stop action in a receipt rule, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-stop.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkStopAction' smart constructor.
data StopAction = StopAction'
  { scope :: Types.StopScope
    -- ^ The scope of the StopAction. The only acceptable value is @RuleSet@ .
  , topicArn :: Core.Maybe Types.AmazonResourceName
    -- ^ The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when the stop action is taken. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopAction' value with any optional fields omitted.
mkStopAction
    :: Types.StopScope -- ^ 'scope'
    -> StopAction
mkStopAction scope = StopAction'{scope, topicArn = Core.Nothing}

-- | The scope of the StopAction. The only acceptable value is @RuleSet@ .
--
-- /Note:/ Consider using 'scope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sScope :: Lens.Lens' StopAction Types.StopScope
sScope = Lens.field @"scope"
{-# INLINEABLE sScope #-}
{-# DEPRECATED scope "Use generic-lens or generic-optics with 'scope' instead"  #-}

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when the stop action is taken. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
--
-- /Note:/ Consider using 'topicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTopicArn :: Lens.Lens' StopAction (Core.Maybe Types.AmazonResourceName)
sTopicArn = Lens.field @"topicArn"
{-# INLINEABLE sTopicArn #-}
{-# DEPRECATED topicArn "Use generic-lens or generic-optics with 'topicArn' instead"  #-}

instance Core.ToQuery StopAction where
        toQuery StopAction{..}
          = Core.toQueryPair "Scope" scope Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TopicArn") topicArn

instance Core.FromXML StopAction where
        parseXML x
          = StopAction' Core.<$>
              (x Core..@ "Scope") Core.<*> x Core..@? "TopicArn"
