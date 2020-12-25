{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.SendBounce
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates and sends a bounce message to the sender of an email you received through Amazon SES. You can only use this API on an email up to 24 hours after you receive it.
--
-- For information about receiving email through Amazon SES, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.SendBounce
  ( -- * Creating a request
    SendBounce (..),
    mkSendBounce,

    -- ** Request lenses
    sbOriginalMessageId,
    sbBounceSender,
    sbBouncedRecipientInfoList,
    sbBounceSenderArn,
    sbExplanation,
    sbMessageDsn,

    -- * Destructuring the response
    SendBounceResponse (..),
    mkSendBounceResponse,

    -- ** Response lenses
    sbrrsMessageId,
    sbrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to send a bounce message to the sender of an email you received through Amazon SES.
--
-- /See:/ 'mkSendBounce' smart constructor.
data SendBounce = SendBounce'
  { -- | The message ID of the message to be bounced.
    originalMessageId :: Types.OriginalMessageId,
    -- | The address to use in the "From" header of the bounce message. This must be an identity that you have verified with Amazon SES.
    bounceSender :: Types.BounceSender,
    -- | A list of recipients of the bounced message, including the information required to create the Delivery Status Notifications (DSNs) for the recipients. You must specify at least one @BouncedRecipientInfo@ in the list.
    bouncedRecipientInfoList :: [Types.BouncedRecipientInfo],
    -- | This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to use the address in the "From" header of the bounce. For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
    bounceSenderArn :: Core.Maybe Types.BounceSenderArn,
    -- | Human-readable text for the bounce message to explain the failure. If not specified, the text will be auto-generated based on the bounced recipient information.
    explanation :: Core.Maybe Types.Explanation,
    -- | Message-related DSN fields. If not specified, Amazon SES will choose the values.
    messageDsn :: Core.Maybe Types.MessageDsn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SendBounce' value with any optional fields omitted.
mkSendBounce ::
  -- | 'originalMessageId'
  Types.OriginalMessageId ->
  -- | 'bounceSender'
  Types.BounceSender ->
  SendBounce
mkSendBounce originalMessageId bounceSender =
  SendBounce'
    { originalMessageId,
      bounceSender,
      bouncedRecipientInfoList = Core.mempty,
      bounceSenderArn = Core.Nothing,
      explanation = Core.Nothing,
      messageDsn = Core.Nothing
    }

-- | The message ID of the message to be bounced.
--
-- /Note:/ Consider using 'originalMessageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbOriginalMessageId :: Lens.Lens' SendBounce Types.OriginalMessageId
sbOriginalMessageId = Lens.field @"originalMessageId"
{-# DEPRECATED sbOriginalMessageId "Use generic-lens or generic-optics with 'originalMessageId' instead." #-}

-- | The address to use in the "From" header of the bounce message. This must be an identity that you have verified with Amazon SES.
--
-- /Note:/ Consider using 'bounceSender' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbBounceSender :: Lens.Lens' SendBounce Types.BounceSender
sbBounceSender = Lens.field @"bounceSender"
{-# DEPRECATED sbBounceSender "Use generic-lens or generic-optics with 'bounceSender' instead." #-}

-- | A list of recipients of the bounced message, including the information required to create the Delivery Status Notifications (DSNs) for the recipients. You must specify at least one @BouncedRecipientInfo@ in the list.
--
-- /Note:/ Consider using 'bouncedRecipientInfoList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbBouncedRecipientInfoList :: Lens.Lens' SendBounce [Types.BouncedRecipientInfo]
sbBouncedRecipientInfoList = Lens.field @"bouncedRecipientInfoList"
{-# DEPRECATED sbBouncedRecipientInfoList "Use generic-lens or generic-optics with 'bouncedRecipientInfoList' instead." #-}

-- | This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to use the address in the "From" header of the bounce. For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- /Note:/ Consider using 'bounceSenderArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbBounceSenderArn :: Lens.Lens' SendBounce (Core.Maybe Types.BounceSenderArn)
sbBounceSenderArn = Lens.field @"bounceSenderArn"
{-# DEPRECATED sbBounceSenderArn "Use generic-lens or generic-optics with 'bounceSenderArn' instead." #-}

-- | Human-readable text for the bounce message to explain the failure. If not specified, the text will be auto-generated based on the bounced recipient information.
--
-- /Note:/ Consider using 'explanation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbExplanation :: Lens.Lens' SendBounce (Core.Maybe Types.Explanation)
sbExplanation = Lens.field @"explanation"
{-# DEPRECATED sbExplanation "Use generic-lens or generic-optics with 'explanation' instead." #-}

-- | Message-related DSN fields. If not specified, Amazon SES will choose the values.
--
-- /Note:/ Consider using 'messageDsn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbMessageDsn :: Lens.Lens' SendBounce (Core.Maybe Types.MessageDsn)
sbMessageDsn = Lens.field @"messageDsn"
{-# DEPRECATED sbMessageDsn "Use generic-lens or generic-optics with 'messageDsn' instead." #-}

instance Core.AWSRequest SendBounce where
  type Rs SendBounce = SendBounceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "SendBounce")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "OriginalMessageId" originalMessageId)
                Core.<> (Core.toQueryValue "BounceSender" bounceSender)
                Core.<> ( Core.toQueryValue
                            "BouncedRecipientInfoList"
                            (Core.toQueryList "member" bouncedRecipientInfoList)
                        )
                Core.<> (Core.toQueryValue "BounceSenderArn" Core.<$> bounceSenderArn)
                Core.<> (Core.toQueryValue "Explanation" Core.<$> explanation)
                Core.<> (Core.toQueryValue "MessageDsn" Core.<$> messageDsn)
            )
      }
  response =
    Response.receiveXMLWrapper
      "SendBounceResult"
      ( \s h x ->
          SendBounceResponse'
            Core.<$> (x Core..@? "MessageId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents a unique message ID.
--
-- /See:/ 'mkSendBounceResponse' smart constructor.
data SendBounceResponse = SendBounceResponse'
  { -- | The message ID of the bounce message.
    messageId :: Core.Maybe Types.MessageId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SendBounceResponse' value with any optional fields omitted.
mkSendBounceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SendBounceResponse
mkSendBounceResponse responseStatus =
  SendBounceResponse' {messageId = Core.Nothing, responseStatus}

-- | The message ID of the bounce message.
--
-- /Note:/ Consider using 'messageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbrrsMessageId :: Lens.Lens' SendBounceResponse (Core.Maybe Types.MessageId)
sbrrsMessageId = Lens.field @"messageId"
{-# DEPRECATED sbrrsMessageId "Use generic-lens or generic-optics with 'messageId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbrrsResponseStatus :: Lens.Lens' SendBounceResponse Core.Int
sbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
