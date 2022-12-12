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
-- Module      : Amazonka.SmsVoice.SendVoiceMessage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new voice message and send it to a recipient\'s phone number.
module Amazonka.SmsVoice.SendVoiceMessage
  ( -- * Creating a Request
    SendVoiceMessage (..),
    newSendVoiceMessage,

    -- * Request Lenses
    sendVoiceMessage_callerId,
    sendVoiceMessage_configurationSetName,
    sendVoiceMessage_content,
    sendVoiceMessage_destinationPhoneNumber,
    sendVoiceMessage_originationPhoneNumber,

    -- * Destructuring the Response
    SendVoiceMessageResponse (..),
    newSendVoiceMessageResponse,

    -- * Response Lenses
    sendVoiceMessageResponse_messageId,
    sendVoiceMessageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SmsVoice.Types

-- | SendVoiceMessageRequest
--
-- /See:/ 'newSendVoiceMessage' smart constructor.
data SendVoiceMessage = SendVoiceMessage'
  { -- | The phone number that appears on recipients\' devices when they receive
    -- the message.
    callerId :: Prelude.Maybe Prelude.Text,
    -- | The name of the configuration set that you want to use to send the
    -- message.
    configurationSetName :: Prelude.Maybe Prelude.Text,
    content :: Prelude.Maybe VoiceMessageContent,
    -- | The phone number that you want to send the voice message to.
    destinationPhoneNumber :: Prelude.Maybe Prelude.Text,
    -- | The phone number that Amazon Pinpoint should use to send the voice
    -- message. This isn\'t necessarily the phone number that appears on
    -- recipients\' devices when they receive the message, because you can
    -- specify a CallerId parameter in the request.
    originationPhoneNumber :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendVoiceMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'callerId', 'sendVoiceMessage_callerId' - The phone number that appears on recipients\' devices when they receive
-- the message.
--
-- 'configurationSetName', 'sendVoiceMessage_configurationSetName' - The name of the configuration set that you want to use to send the
-- message.
--
-- 'content', 'sendVoiceMessage_content' - Undocumented member.
--
-- 'destinationPhoneNumber', 'sendVoiceMessage_destinationPhoneNumber' - The phone number that you want to send the voice message to.
--
-- 'originationPhoneNumber', 'sendVoiceMessage_originationPhoneNumber' - The phone number that Amazon Pinpoint should use to send the voice
-- message. This isn\'t necessarily the phone number that appears on
-- recipients\' devices when they receive the message, because you can
-- specify a CallerId parameter in the request.
newSendVoiceMessage ::
  SendVoiceMessage
newSendVoiceMessage =
  SendVoiceMessage'
    { callerId = Prelude.Nothing,
      configurationSetName = Prelude.Nothing,
      content = Prelude.Nothing,
      destinationPhoneNumber = Prelude.Nothing,
      originationPhoneNumber = Prelude.Nothing
    }

-- | The phone number that appears on recipients\' devices when they receive
-- the message.
sendVoiceMessage_callerId :: Lens.Lens' SendVoiceMessage (Prelude.Maybe Prelude.Text)
sendVoiceMessage_callerId = Lens.lens (\SendVoiceMessage' {callerId} -> callerId) (\s@SendVoiceMessage' {} a -> s {callerId = a} :: SendVoiceMessage)

-- | The name of the configuration set that you want to use to send the
-- message.
sendVoiceMessage_configurationSetName :: Lens.Lens' SendVoiceMessage (Prelude.Maybe Prelude.Text)
sendVoiceMessage_configurationSetName = Lens.lens (\SendVoiceMessage' {configurationSetName} -> configurationSetName) (\s@SendVoiceMessage' {} a -> s {configurationSetName = a} :: SendVoiceMessage)

-- | Undocumented member.
sendVoiceMessage_content :: Lens.Lens' SendVoiceMessage (Prelude.Maybe VoiceMessageContent)
sendVoiceMessage_content = Lens.lens (\SendVoiceMessage' {content} -> content) (\s@SendVoiceMessage' {} a -> s {content = a} :: SendVoiceMessage)

-- | The phone number that you want to send the voice message to.
sendVoiceMessage_destinationPhoneNumber :: Lens.Lens' SendVoiceMessage (Prelude.Maybe Prelude.Text)
sendVoiceMessage_destinationPhoneNumber = Lens.lens (\SendVoiceMessage' {destinationPhoneNumber} -> destinationPhoneNumber) (\s@SendVoiceMessage' {} a -> s {destinationPhoneNumber = a} :: SendVoiceMessage)

-- | The phone number that Amazon Pinpoint should use to send the voice
-- message. This isn\'t necessarily the phone number that appears on
-- recipients\' devices when they receive the message, because you can
-- specify a CallerId parameter in the request.
sendVoiceMessage_originationPhoneNumber :: Lens.Lens' SendVoiceMessage (Prelude.Maybe Prelude.Text)
sendVoiceMessage_originationPhoneNumber = Lens.lens (\SendVoiceMessage' {originationPhoneNumber} -> originationPhoneNumber) (\s@SendVoiceMessage' {} a -> s {originationPhoneNumber = a} :: SendVoiceMessage)

instance Core.AWSRequest SendVoiceMessage where
  type
    AWSResponse SendVoiceMessage =
      SendVoiceMessageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SendVoiceMessageResponse'
            Prelude.<$> (x Data..?> "MessageId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SendVoiceMessage where
  hashWithSalt _salt SendVoiceMessage' {..} =
    _salt `Prelude.hashWithSalt` callerId
      `Prelude.hashWithSalt` configurationSetName
      `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` destinationPhoneNumber
      `Prelude.hashWithSalt` originationPhoneNumber

instance Prelude.NFData SendVoiceMessage where
  rnf SendVoiceMessage' {..} =
    Prelude.rnf callerId
      `Prelude.seq` Prelude.rnf configurationSetName
      `Prelude.seq` Prelude.rnf content
      `Prelude.seq` Prelude.rnf destinationPhoneNumber
      `Prelude.seq` Prelude.rnf originationPhoneNumber

instance Data.ToHeaders SendVoiceMessage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SendVoiceMessage where
  toJSON SendVoiceMessage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CallerId" Data..=) Prelude.<$> callerId,
            ("ConfigurationSetName" Data..=)
              Prelude.<$> configurationSetName,
            ("Content" Data..=) Prelude.<$> content,
            ("DestinationPhoneNumber" Data..=)
              Prelude.<$> destinationPhoneNumber,
            ("OriginationPhoneNumber" Data..=)
              Prelude.<$> originationPhoneNumber
          ]
      )

instance Data.ToPath SendVoiceMessage where
  toPath = Prelude.const "/v1/sms-voice/voice/message"

instance Data.ToQuery SendVoiceMessage where
  toQuery = Prelude.const Prelude.mempty

-- | An object that that contains the Message ID of a Voice message that was
-- sent successfully.
--
-- /See:/ 'newSendVoiceMessageResponse' smart constructor.
data SendVoiceMessageResponse = SendVoiceMessageResponse'
  { -- | A unique identifier for the voice message.
    messageId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendVoiceMessageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'messageId', 'sendVoiceMessageResponse_messageId' - A unique identifier for the voice message.
--
-- 'httpStatus', 'sendVoiceMessageResponse_httpStatus' - The response's http status code.
newSendVoiceMessageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SendVoiceMessageResponse
newSendVoiceMessageResponse pHttpStatus_ =
  SendVoiceMessageResponse'
    { messageId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique identifier for the voice message.
sendVoiceMessageResponse_messageId :: Lens.Lens' SendVoiceMessageResponse (Prelude.Maybe Prelude.Text)
sendVoiceMessageResponse_messageId = Lens.lens (\SendVoiceMessageResponse' {messageId} -> messageId) (\s@SendVoiceMessageResponse' {} a -> s {messageId = a} :: SendVoiceMessageResponse)

-- | The response's http status code.
sendVoiceMessageResponse_httpStatus :: Lens.Lens' SendVoiceMessageResponse Prelude.Int
sendVoiceMessageResponse_httpStatus = Lens.lens (\SendVoiceMessageResponse' {httpStatus} -> httpStatus) (\s@SendVoiceMessageResponse' {} a -> s {httpStatus = a} :: SendVoiceMessageResponse)

instance Prelude.NFData SendVoiceMessageResponse where
  rnf SendVoiceMessageResponse' {..} =
    Prelude.rnf messageId
      `Prelude.seq` Prelude.rnf httpStatus
