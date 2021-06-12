{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.SendBounce
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates and sends a bounce message to the sender of an email you
-- received through Amazon SES. You can only use this API on an email up to
-- 24 hours after you receive it.
--
-- You cannot use this API to send generic bounces for mail that was not
-- received by Amazon SES.
--
-- For information about receiving email through Amazon SES, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html Amazon SES Developer Guide>.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.SendBounce
  ( -- * Creating a Request
    SendBounce (..),
    newSendBounce,

    -- * Request Lenses
    sendBounce_bounceSenderArn,
    sendBounce_messageDsn,
    sendBounce_explanation,
    sendBounce_originalMessageId,
    sendBounce_bounceSender,
    sendBounce_bouncedRecipientInfoList,

    -- * Destructuring the Response
    SendBounceResponse (..),
    newSendBounceResponse,

    -- * Response Lenses
    sendBounceResponse_messageId,
    sendBounceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to send a bounce message to the sender of an email
-- you received through Amazon SES.
--
-- /See:/ 'newSendBounce' smart constructor.
data SendBounce = SendBounce'
  { -- | This parameter is used only for sending authorization. It is the ARN of
    -- the identity that is associated with the sending authorization policy
    -- that permits you to use the address in the \"From\" header of the
    -- bounce. For more information about sending authorization, see the
    -- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide>.
    bounceSenderArn :: Core.Maybe Core.Text,
    -- | Message-related DSN fields. If not specified, Amazon SES will choose the
    -- values.
    messageDsn :: Core.Maybe MessageDsn,
    -- | Human-readable text for the bounce message to explain the failure. If
    -- not specified, the text will be auto-generated based on the bounced
    -- recipient information.
    explanation :: Core.Maybe Core.Text,
    -- | The message ID of the message to be bounced.
    originalMessageId :: Core.Text,
    -- | The address to use in the \"From\" header of the bounce message. This
    -- must be an identity that you have verified with Amazon SES.
    bounceSender :: Core.Text,
    -- | A list of recipients of the bounced message, including the information
    -- required to create the Delivery Status Notifications (DSNs) for the
    -- recipients. You must specify at least one @BouncedRecipientInfo@ in the
    -- list.
    bouncedRecipientInfoList :: [BouncedRecipientInfo]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SendBounce' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bounceSenderArn', 'sendBounce_bounceSenderArn' - This parameter is used only for sending authorization. It is the ARN of
-- the identity that is associated with the sending authorization policy
-- that permits you to use the address in the \"From\" header of the
-- bounce. For more information about sending authorization, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide>.
--
-- 'messageDsn', 'sendBounce_messageDsn' - Message-related DSN fields. If not specified, Amazon SES will choose the
-- values.
--
-- 'explanation', 'sendBounce_explanation' - Human-readable text for the bounce message to explain the failure. If
-- not specified, the text will be auto-generated based on the bounced
-- recipient information.
--
-- 'originalMessageId', 'sendBounce_originalMessageId' - The message ID of the message to be bounced.
--
-- 'bounceSender', 'sendBounce_bounceSender' - The address to use in the \"From\" header of the bounce message. This
-- must be an identity that you have verified with Amazon SES.
--
-- 'bouncedRecipientInfoList', 'sendBounce_bouncedRecipientInfoList' - A list of recipients of the bounced message, including the information
-- required to create the Delivery Status Notifications (DSNs) for the
-- recipients. You must specify at least one @BouncedRecipientInfo@ in the
-- list.
newSendBounce ::
  -- | 'originalMessageId'
  Core.Text ->
  -- | 'bounceSender'
  Core.Text ->
  SendBounce
newSendBounce pOriginalMessageId_ pBounceSender_ =
  SendBounce'
    { bounceSenderArn = Core.Nothing,
      messageDsn = Core.Nothing,
      explanation = Core.Nothing,
      originalMessageId = pOriginalMessageId_,
      bounceSender = pBounceSender_,
      bouncedRecipientInfoList = Core.mempty
    }

-- | This parameter is used only for sending authorization. It is the ARN of
-- the identity that is associated with the sending authorization policy
-- that permits you to use the address in the \"From\" header of the
-- bounce. For more information about sending authorization, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide>.
sendBounce_bounceSenderArn :: Lens.Lens' SendBounce (Core.Maybe Core.Text)
sendBounce_bounceSenderArn = Lens.lens (\SendBounce' {bounceSenderArn} -> bounceSenderArn) (\s@SendBounce' {} a -> s {bounceSenderArn = a} :: SendBounce)

-- | Message-related DSN fields. If not specified, Amazon SES will choose the
-- values.
sendBounce_messageDsn :: Lens.Lens' SendBounce (Core.Maybe MessageDsn)
sendBounce_messageDsn = Lens.lens (\SendBounce' {messageDsn} -> messageDsn) (\s@SendBounce' {} a -> s {messageDsn = a} :: SendBounce)

-- | Human-readable text for the bounce message to explain the failure. If
-- not specified, the text will be auto-generated based on the bounced
-- recipient information.
sendBounce_explanation :: Lens.Lens' SendBounce (Core.Maybe Core.Text)
sendBounce_explanation = Lens.lens (\SendBounce' {explanation} -> explanation) (\s@SendBounce' {} a -> s {explanation = a} :: SendBounce)

-- | The message ID of the message to be bounced.
sendBounce_originalMessageId :: Lens.Lens' SendBounce Core.Text
sendBounce_originalMessageId = Lens.lens (\SendBounce' {originalMessageId} -> originalMessageId) (\s@SendBounce' {} a -> s {originalMessageId = a} :: SendBounce)

-- | The address to use in the \"From\" header of the bounce message. This
-- must be an identity that you have verified with Amazon SES.
sendBounce_bounceSender :: Lens.Lens' SendBounce Core.Text
sendBounce_bounceSender = Lens.lens (\SendBounce' {bounceSender} -> bounceSender) (\s@SendBounce' {} a -> s {bounceSender = a} :: SendBounce)

-- | A list of recipients of the bounced message, including the information
-- required to create the Delivery Status Notifications (DSNs) for the
-- recipients. You must specify at least one @BouncedRecipientInfo@ in the
-- list.
sendBounce_bouncedRecipientInfoList :: Lens.Lens' SendBounce [BouncedRecipientInfo]
sendBounce_bouncedRecipientInfoList = Lens.lens (\SendBounce' {bouncedRecipientInfoList} -> bouncedRecipientInfoList) (\s@SendBounce' {} a -> s {bouncedRecipientInfoList = a} :: SendBounce) Core.. Lens._Coerce

instance Core.AWSRequest SendBounce where
  type AWSResponse SendBounce = SendBounceResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "SendBounceResult"
      ( \s h x ->
          SendBounceResponse'
            Core.<$> (x Core..@? "MessageId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SendBounce

instance Core.NFData SendBounce

instance Core.ToHeaders SendBounce where
  toHeaders = Core.const Core.mempty

instance Core.ToPath SendBounce where
  toPath = Core.const "/"

instance Core.ToQuery SendBounce where
  toQuery SendBounce' {..} =
    Core.mconcat
      [ "Action" Core.=: ("SendBounce" :: Core.ByteString),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "BounceSenderArn" Core.=: bounceSenderArn,
        "MessageDsn" Core.=: messageDsn,
        "Explanation" Core.=: explanation,
        "OriginalMessageId" Core.=: originalMessageId,
        "BounceSender" Core.=: bounceSender,
        "BouncedRecipientInfoList"
          Core.=: Core.toQueryList "member" bouncedRecipientInfoList
      ]

-- | Represents a unique message ID.
--
-- /See:/ 'newSendBounceResponse' smart constructor.
data SendBounceResponse = SendBounceResponse'
  { -- | The message ID of the bounce message.
    messageId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SendBounceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'messageId', 'sendBounceResponse_messageId' - The message ID of the bounce message.
--
-- 'httpStatus', 'sendBounceResponse_httpStatus' - The response's http status code.
newSendBounceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  SendBounceResponse
newSendBounceResponse pHttpStatus_ =
  SendBounceResponse'
    { messageId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The message ID of the bounce message.
sendBounceResponse_messageId :: Lens.Lens' SendBounceResponse (Core.Maybe Core.Text)
sendBounceResponse_messageId = Lens.lens (\SendBounceResponse' {messageId} -> messageId) (\s@SendBounceResponse' {} a -> s {messageId = a} :: SendBounceResponse)

-- | The response's http status code.
sendBounceResponse_httpStatus :: Lens.Lens' SendBounceResponse Core.Int
sendBounceResponse_httpStatus = Lens.lens (\SendBounceResponse' {httpStatus} -> httpStatus) (\s@SendBounceResponse' {} a -> s {httpStatus = a} :: SendBounceResponse)

instance Core.NFData SendBounceResponse
