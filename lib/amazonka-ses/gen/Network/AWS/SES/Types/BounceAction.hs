{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.BounceAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.BounceAction
  ( BounceAction (..)
  -- * Smart constructor
  , mkBounceAction
  -- * Lenses
  , baSmtpReplyCode
  , baMessage
  , baSender
  , baStatusCode
  , baTopicArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SES.Types.Address as Types
import qualified Network.AWS.SES.Types.BounceMessage as Types
import qualified Network.AWS.SES.Types.BounceSmtpReplyCode as Types
import qualified Network.AWS.SES.Types.StatusCode as Types
import qualified Network.AWS.SES.Types.TopicArn as Types

-- | When included in a receipt rule, this action rejects the received email by returning a bounce response to the sender and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS).
--
-- For information about sending a bounce message in response to a received email, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-bounce.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkBounceAction' smart constructor.
data BounceAction = BounceAction'
  { smtpReplyCode :: Types.BounceSmtpReplyCode
    -- ^ The SMTP reply code, as defined by <https://tools.ietf.org/html/rfc5321 RFC 5321> .
  , message :: Types.BounceMessage
    -- ^ Human-readable text to include in the bounce message.
  , sender :: Types.Address
    -- ^ The email address of the sender of the bounced email. This is the address from which the bounce message will be sent.
  , statusCode :: Core.Maybe Types.StatusCode
    -- ^ The SMTP enhanced status code, as defined by <https://tools.ietf.org/html/rfc3463 RFC 3463> .
  , topicArn :: Core.Maybe Types.TopicArn
    -- ^ The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when the bounce action is taken. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BounceAction' value with any optional fields omitted.
mkBounceAction
    :: Types.BounceSmtpReplyCode -- ^ 'smtpReplyCode'
    -> Types.BounceMessage -- ^ 'message'
    -> Types.Address -- ^ 'sender'
    -> BounceAction
mkBounceAction smtpReplyCode message sender
  = BounceAction'{smtpReplyCode, message, sender,
                  statusCode = Core.Nothing, topicArn = Core.Nothing}

-- | The SMTP reply code, as defined by <https://tools.ietf.org/html/rfc5321 RFC 5321> .
--
-- /Note:/ Consider using 'smtpReplyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baSmtpReplyCode :: Lens.Lens' BounceAction Types.BounceSmtpReplyCode
baSmtpReplyCode = Lens.field @"smtpReplyCode"
{-# INLINEABLE baSmtpReplyCode #-}
{-# DEPRECATED smtpReplyCode "Use generic-lens or generic-optics with 'smtpReplyCode' instead"  #-}

-- | Human-readable text to include in the bounce message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baMessage :: Lens.Lens' BounceAction Types.BounceMessage
baMessage = Lens.field @"message"
{-# INLINEABLE baMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | The email address of the sender of the bounced email. This is the address from which the bounce message will be sent.
--
-- /Note:/ Consider using 'sender' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baSender :: Lens.Lens' BounceAction Types.Address
baSender = Lens.field @"sender"
{-# INLINEABLE baSender #-}
{-# DEPRECATED sender "Use generic-lens or generic-optics with 'sender' instead"  #-}

-- | The SMTP enhanced status code, as defined by <https://tools.ietf.org/html/rfc3463 RFC 3463> .
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baStatusCode :: Lens.Lens' BounceAction (Core.Maybe Types.StatusCode)
baStatusCode = Lens.field @"statusCode"
{-# INLINEABLE baStatusCode #-}
{-# DEPRECATED statusCode "Use generic-lens or generic-optics with 'statusCode' instead"  #-}

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when the bounce action is taken. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
--
-- /Note:/ Consider using 'topicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baTopicArn :: Lens.Lens' BounceAction (Core.Maybe Types.TopicArn)
baTopicArn = Lens.field @"topicArn"
{-# INLINEABLE baTopicArn #-}
{-# DEPRECATED topicArn "Use generic-lens or generic-optics with 'topicArn' instead"  #-}

instance Core.ToQuery BounceAction where
        toQuery BounceAction{..}
          = Core.toQueryPair "SmtpReplyCode" smtpReplyCode Core.<>
              Core.toQueryPair "Message" message
              Core.<> Core.toQueryPair "Sender" sender
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "StatusCode") statusCode
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TopicArn") topicArn

instance Core.FromXML BounceAction where
        parseXML x
          = BounceAction' Core.<$>
              (x Core..@ "SmtpReplyCode") Core.<*> x Core..@ "Message" Core.<*>
                x Core..@ "Sender"
                Core.<*> x Core..@? "StatusCode"
                Core.<*> x Core..@? "TopicArn"
