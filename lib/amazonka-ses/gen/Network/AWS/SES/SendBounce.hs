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
    sbMessageDsn,
    sbBounceSender,
    sbExplanation,
    sbBouncedRecipientInfoList,
    sbBounceSenderARN,

    -- * Destructuring the response
    SendBounceResponse (..),
    mkSendBounceResponse,

    -- ** Response lenses
    sbrsMessageId,
    sbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to send a bounce message to the sender of an email you received through Amazon SES.
--
-- /See:/ 'mkSendBounce' smart constructor.
data SendBounce = SendBounce'
  { -- | The message ID of the message to be bounced.
    originalMessageId :: Lude.Text,
    -- | Message-related DSN fields. If not specified, Amazon SES will choose the values.
    messageDsn :: Lude.Maybe MessageDsn,
    -- | The address to use in the "From" header of the bounce message. This must be an identity that you have verified with Amazon SES.
    bounceSender :: Lude.Text,
    -- | Human-readable text for the bounce message to explain the failure. If not specified, the text will be auto-generated based on the bounced recipient information.
    explanation :: Lude.Maybe Lude.Text,
    -- | A list of recipients of the bounced message, including the information required to create the Delivery Status Notifications (DSNs) for the recipients. You must specify at least one @BouncedRecipientInfo@ in the list.
    bouncedRecipientInfoList :: [BouncedRecipientInfo],
    -- | This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to use the address in the "From" header of the bounce. For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
    bounceSenderARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendBounce' with the minimum fields required to make a request.
--
-- * 'originalMessageId' - The message ID of the message to be bounced.
-- * 'messageDsn' - Message-related DSN fields. If not specified, Amazon SES will choose the values.
-- * 'bounceSender' - The address to use in the "From" header of the bounce message. This must be an identity that you have verified with Amazon SES.
-- * 'explanation' - Human-readable text for the bounce message to explain the failure. If not specified, the text will be auto-generated based on the bounced recipient information.
-- * 'bouncedRecipientInfoList' - A list of recipients of the bounced message, including the information required to create the Delivery Status Notifications (DSNs) for the recipients. You must specify at least one @BouncedRecipientInfo@ in the list.
-- * 'bounceSenderARN' - This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to use the address in the "From" header of the bounce. For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
mkSendBounce ::
  -- | 'originalMessageId'
  Lude.Text ->
  -- | 'bounceSender'
  Lude.Text ->
  SendBounce
mkSendBounce pOriginalMessageId_ pBounceSender_ =
  SendBounce'
    { originalMessageId = pOriginalMessageId_,
      messageDsn = Lude.Nothing,
      bounceSender = pBounceSender_,
      explanation = Lude.Nothing,
      bouncedRecipientInfoList = Lude.mempty,
      bounceSenderARN = Lude.Nothing
    }

-- | The message ID of the message to be bounced.
--
-- /Note:/ Consider using 'originalMessageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbOriginalMessageId :: Lens.Lens' SendBounce Lude.Text
sbOriginalMessageId = Lens.lens (originalMessageId :: SendBounce -> Lude.Text) (\s a -> s {originalMessageId = a} :: SendBounce)
{-# DEPRECATED sbOriginalMessageId "Use generic-lens or generic-optics with 'originalMessageId' instead." #-}

-- | Message-related DSN fields. If not specified, Amazon SES will choose the values.
--
-- /Note:/ Consider using 'messageDsn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbMessageDsn :: Lens.Lens' SendBounce (Lude.Maybe MessageDsn)
sbMessageDsn = Lens.lens (messageDsn :: SendBounce -> Lude.Maybe MessageDsn) (\s a -> s {messageDsn = a} :: SendBounce)
{-# DEPRECATED sbMessageDsn "Use generic-lens or generic-optics with 'messageDsn' instead." #-}

-- | The address to use in the "From" header of the bounce message. This must be an identity that you have verified with Amazon SES.
--
-- /Note:/ Consider using 'bounceSender' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbBounceSender :: Lens.Lens' SendBounce Lude.Text
sbBounceSender = Lens.lens (bounceSender :: SendBounce -> Lude.Text) (\s a -> s {bounceSender = a} :: SendBounce)
{-# DEPRECATED sbBounceSender "Use generic-lens or generic-optics with 'bounceSender' instead." #-}

-- | Human-readable text for the bounce message to explain the failure. If not specified, the text will be auto-generated based on the bounced recipient information.
--
-- /Note:/ Consider using 'explanation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbExplanation :: Lens.Lens' SendBounce (Lude.Maybe Lude.Text)
sbExplanation = Lens.lens (explanation :: SendBounce -> Lude.Maybe Lude.Text) (\s a -> s {explanation = a} :: SendBounce)
{-# DEPRECATED sbExplanation "Use generic-lens or generic-optics with 'explanation' instead." #-}

-- | A list of recipients of the bounced message, including the information required to create the Delivery Status Notifications (DSNs) for the recipients. You must specify at least one @BouncedRecipientInfo@ in the list.
--
-- /Note:/ Consider using 'bouncedRecipientInfoList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbBouncedRecipientInfoList :: Lens.Lens' SendBounce [BouncedRecipientInfo]
sbBouncedRecipientInfoList = Lens.lens (bouncedRecipientInfoList :: SendBounce -> [BouncedRecipientInfo]) (\s a -> s {bouncedRecipientInfoList = a} :: SendBounce)
{-# DEPRECATED sbBouncedRecipientInfoList "Use generic-lens or generic-optics with 'bouncedRecipientInfoList' instead." #-}

-- | This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to use the address in the "From" header of the bounce. For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- /Note:/ Consider using 'bounceSenderARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbBounceSenderARN :: Lens.Lens' SendBounce (Lude.Maybe Lude.Text)
sbBounceSenderARN = Lens.lens (bounceSenderARN :: SendBounce -> Lude.Maybe Lude.Text) (\s a -> s {bounceSenderARN = a} :: SendBounce)
{-# DEPRECATED sbBounceSenderARN "Use generic-lens or generic-optics with 'bounceSenderARN' instead." #-}

instance Lude.AWSRequest SendBounce where
  type Rs SendBounce = SendBounceResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "SendBounceResult"
      ( \s h x ->
          SendBounceResponse'
            Lude.<$> (x Lude..@? "MessageId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SendBounce where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SendBounce where
  toPath = Lude.const "/"

instance Lude.ToQuery SendBounce where
  toQuery SendBounce' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("SendBounce" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "OriginalMessageId" Lude.=: originalMessageId,
        "MessageDsn" Lude.=: messageDsn,
        "BounceSender" Lude.=: bounceSender,
        "Explanation" Lude.=: explanation,
        "BouncedRecipientInfoList"
          Lude.=: Lude.toQueryList "member" bouncedRecipientInfoList,
        "BounceSenderArn" Lude.=: bounceSenderARN
      ]

-- | Represents a unique message ID.
--
-- /See:/ 'mkSendBounceResponse' smart constructor.
data SendBounceResponse = SendBounceResponse'
  { -- | The message ID of the bounce message.
    messageId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendBounceResponse' with the minimum fields required to make a request.
--
-- * 'messageId' - The message ID of the bounce message.
-- * 'responseStatus' - The response status code.
mkSendBounceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SendBounceResponse
mkSendBounceResponse pResponseStatus_ =
  SendBounceResponse'
    { messageId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The message ID of the bounce message.
--
-- /Note:/ Consider using 'messageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbrsMessageId :: Lens.Lens' SendBounceResponse (Lude.Maybe Lude.Text)
sbrsMessageId = Lens.lens (messageId :: SendBounceResponse -> Lude.Maybe Lude.Text) (\s a -> s {messageId = a} :: SendBounceResponse)
{-# DEPRECATED sbrsMessageId "Use generic-lens or generic-optics with 'messageId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbrsResponseStatus :: Lens.Lens' SendBounceResponse Lude.Int
sbrsResponseStatus = Lens.lens (responseStatus :: SendBounceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SendBounceResponse)
{-# DEPRECATED sbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
