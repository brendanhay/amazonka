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
-- Module      : Amazonka.PinpointSmsVoiceV2.SendVoiceMessage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you to send a request that sends a text message through Amazon
-- Pinpoint. This operation uses
-- <http://aws.amazon.com/polly/ Amazon Polly> to convert a text script
-- into a voice message.
module Amazonka.PinpointSmsVoiceV2.SendVoiceMessage
  ( -- * Creating a Request
    SendVoiceMessage (..),
    newSendVoiceMessage,

    -- * Request Lenses
    sendVoiceMessage_voiceId,
    sendVoiceMessage_timeToLive,
    sendVoiceMessage_maxPricePerMinute,
    sendVoiceMessage_configurationSetName,
    sendVoiceMessage_context,
    sendVoiceMessage_messageBody,
    sendVoiceMessage_dryRun,
    sendVoiceMessage_messageBodyTextType,
    sendVoiceMessage_destinationPhoneNumber,
    sendVoiceMessage_originationIdentity,

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
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSendVoiceMessage' smart constructor.
data SendVoiceMessage = SendVoiceMessage'
  { -- | The voice for the
    -- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly>
    -- service to use. By default this is set to \"MATTHEW\".
    voiceId :: Prelude.Maybe VoiceId,
    -- | How long the voice message is valid for. By default this is 72 hours.
    timeToLive :: Prelude.Maybe Prelude.Natural,
    -- | The maximum amount to spend per voice message, in US dollars.
    maxPricePerMinute :: Prelude.Maybe Prelude.Text,
    -- | The name of the configuration set to use. This can be either the
    -- ConfigurationSetName or ConfigurationSetArn.
    configurationSetName :: Prelude.Maybe Prelude.Text,
    -- | You can specify custom data in this field. If you do, that data is
    -- logged to the event destination.
    context :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The text to convert to a voice message.
    messageBody :: Prelude.Maybe Prelude.Text,
    -- | When set to true, the message is checked and validated, but isn\'t sent
    -- to the end recipient.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Specifies if the MessageBody field contains text or
    -- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html speech synthesis markup language (SSML)>.
    --
    -- -   TEXT: This is the default value. When used the maximum character
    --     limit is 3000.
    --
    -- -   SSML: When used the maximum character limit is 6000 including SSML
    --     tagging.
    messageBodyTextType :: Prelude.Maybe VoiceMessageBodyTextType,
    -- | The destination phone number in E.164 format.
    destinationPhoneNumber :: Prelude.Text,
    -- | The origination identity to use for the voice call. This can be the
    -- PhoneNumber, PhoneNumberId, PhoneNumberArn, PoolId, or PoolArn.
    originationIdentity :: Prelude.Text
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
-- 'voiceId', 'sendVoiceMessage_voiceId' - The voice for the
-- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly>
-- service to use. By default this is set to \"MATTHEW\".
--
-- 'timeToLive', 'sendVoiceMessage_timeToLive' - How long the voice message is valid for. By default this is 72 hours.
--
-- 'maxPricePerMinute', 'sendVoiceMessage_maxPricePerMinute' - The maximum amount to spend per voice message, in US dollars.
--
-- 'configurationSetName', 'sendVoiceMessage_configurationSetName' - The name of the configuration set to use. This can be either the
-- ConfigurationSetName or ConfigurationSetArn.
--
-- 'context', 'sendVoiceMessage_context' - You can specify custom data in this field. If you do, that data is
-- logged to the event destination.
--
-- 'messageBody', 'sendVoiceMessage_messageBody' - The text to convert to a voice message.
--
-- 'dryRun', 'sendVoiceMessage_dryRun' - When set to true, the message is checked and validated, but isn\'t sent
-- to the end recipient.
--
-- 'messageBodyTextType', 'sendVoiceMessage_messageBodyTextType' - Specifies if the MessageBody field contains text or
-- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html speech synthesis markup language (SSML)>.
--
-- -   TEXT: This is the default value. When used the maximum character
--     limit is 3000.
--
-- -   SSML: When used the maximum character limit is 6000 including SSML
--     tagging.
--
-- 'destinationPhoneNumber', 'sendVoiceMessage_destinationPhoneNumber' - The destination phone number in E.164 format.
--
-- 'originationIdentity', 'sendVoiceMessage_originationIdentity' - The origination identity to use for the voice call. This can be the
-- PhoneNumber, PhoneNumberId, PhoneNumberArn, PoolId, or PoolArn.
newSendVoiceMessage ::
  -- | 'destinationPhoneNumber'
  Prelude.Text ->
  -- | 'originationIdentity'
  Prelude.Text ->
  SendVoiceMessage
newSendVoiceMessage
  pDestinationPhoneNumber_
  pOriginationIdentity_ =
    SendVoiceMessage'
      { voiceId = Prelude.Nothing,
        timeToLive = Prelude.Nothing,
        maxPricePerMinute = Prelude.Nothing,
        configurationSetName = Prelude.Nothing,
        context = Prelude.Nothing,
        messageBody = Prelude.Nothing,
        dryRun = Prelude.Nothing,
        messageBodyTextType = Prelude.Nothing,
        destinationPhoneNumber = pDestinationPhoneNumber_,
        originationIdentity = pOriginationIdentity_
      }

-- | The voice for the
-- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly>
-- service to use. By default this is set to \"MATTHEW\".
sendVoiceMessage_voiceId :: Lens.Lens' SendVoiceMessage (Prelude.Maybe VoiceId)
sendVoiceMessage_voiceId = Lens.lens (\SendVoiceMessage' {voiceId} -> voiceId) (\s@SendVoiceMessage' {} a -> s {voiceId = a} :: SendVoiceMessage)

-- | How long the voice message is valid for. By default this is 72 hours.
sendVoiceMessage_timeToLive :: Lens.Lens' SendVoiceMessage (Prelude.Maybe Prelude.Natural)
sendVoiceMessage_timeToLive = Lens.lens (\SendVoiceMessage' {timeToLive} -> timeToLive) (\s@SendVoiceMessage' {} a -> s {timeToLive = a} :: SendVoiceMessage)

-- | The maximum amount to spend per voice message, in US dollars.
sendVoiceMessage_maxPricePerMinute :: Lens.Lens' SendVoiceMessage (Prelude.Maybe Prelude.Text)
sendVoiceMessage_maxPricePerMinute = Lens.lens (\SendVoiceMessage' {maxPricePerMinute} -> maxPricePerMinute) (\s@SendVoiceMessage' {} a -> s {maxPricePerMinute = a} :: SendVoiceMessage)

-- | The name of the configuration set to use. This can be either the
-- ConfigurationSetName or ConfigurationSetArn.
sendVoiceMessage_configurationSetName :: Lens.Lens' SendVoiceMessage (Prelude.Maybe Prelude.Text)
sendVoiceMessage_configurationSetName = Lens.lens (\SendVoiceMessage' {configurationSetName} -> configurationSetName) (\s@SendVoiceMessage' {} a -> s {configurationSetName = a} :: SendVoiceMessage)

-- | You can specify custom data in this field. If you do, that data is
-- logged to the event destination.
sendVoiceMessage_context :: Lens.Lens' SendVoiceMessage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
sendVoiceMessage_context = Lens.lens (\SendVoiceMessage' {context} -> context) (\s@SendVoiceMessage' {} a -> s {context = a} :: SendVoiceMessage) Prelude.. Lens.mapping Lens.coerced

-- | The text to convert to a voice message.
sendVoiceMessage_messageBody :: Lens.Lens' SendVoiceMessage (Prelude.Maybe Prelude.Text)
sendVoiceMessage_messageBody = Lens.lens (\SendVoiceMessage' {messageBody} -> messageBody) (\s@SendVoiceMessage' {} a -> s {messageBody = a} :: SendVoiceMessage)

-- | When set to true, the message is checked and validated, but isn\'t sent
-- to the end recipient.
sendVoiceMessage_dryRun :: Lens.Lens' SendVoiceMessage (Prelude.Maybe Prelude.Bool)
sendVoiceMessage_dryRun = Lens.lens (\SendVoiceMessage' {dryRun} -> dryRun) (\s@SendVoiceMessage' {} a -> s {dryRun = a} :: SendVoiceMessage)

-- | Specifies if the MessageBody field contains text or
-- <https://docs.aws.amazon.com/polly/latest/dg/what-is.html speech synthesis markup language (SSML)>.
--
-- -   TEXT: This is the default value. When used the maximum character
--     limit is 3000.
--
-- -   SSML: When used the maximum character limit is 6000 including SSML
--     tagging.
sendVoiceMessage_messageBodyTextType :: Lens.Lens' SendVoiceMessage (Prelude.Maybe VoiceMessageBodyTextType)
sendVoiceMessage_messageBodyTextType = Lens.lens (\SendVoiceMessage' {messageBodyTextType} -> messageBodyTextType) (\s@SendVoiceMessage' {} a -> s {messageBodyTextType = a} :: SendVoiceMessage)

-- | The destination phone number in E.164 format.
sendVoiceMessage_destinationPhoneNumber :: Lens.Lens' SendVoiceMessage Prelude.Text
sendVoiceMessage_destinationPhoneNumber = Lens.lens (\SendVoiceMessage' {destinationPhoneNumber} -> destinationPhoneNumber) (\s@SendVoiceMessage' {} a -> s {destinationPhoneNumber = a} :: SendVoiceMessage)

-- | The origination identity to use for the voice call. This can be the
-- PhoneNumber, PhoneNumberId, PhoneNumberArn, PoolId, or PoolArn.
sendVoiceMessage_originationIdentity :: Lens.Lens' SendVoiceMessage Prelude.Text
sendVoiceMessage_originationIdentity = Lens.lens (\SendVoiceMessage' {originationIdentity} -> originationIdentity) (\s@SendVoiceMessage' {} a -> s {originationIdentity = a} :: SendVoiceMessage)

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
    _salt `Prelude.hashWithSalt` voiceId
      `Prelude.hashWithSalt` timeToLive
      `Prelude.hashWithSalt` maxPricePerMinute
      `Prelude.hashWithSalt` configurationSetName
      `Prelude.hashWithSalt` context
      `Prelude.hashWithSalt` messageBody
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` messageBodyTextType
      `Prelude.hashWithSalt` destinationPhoneNumber
      `Prelude.hashWithSalt` originationIdentity

instance Prelude.NFData SendVoiceMessage where
  rnf SendVoiceMessage' {..} =
    Prelude.rnf voiceId
      `Prelude.seq` Prelude.rnf timeToLive
      `Prelude.seq` Prelude.rnf maxPricePerMinute
      `Prelude.seq` Prelude.rnf configurationSetName
      `Prelude.seq` Prelude.rnf context
      `Prelude.seq` Prelude.rnf messageBody
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf messageBodyTextType
      `Prelude.seq` Prelude.rnf destinationPhoneNumber
      `Prelude.seq` Prelude.rnf originationIdentity

instance Data.ToHeaders SendVoiceMessage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PinpointSMSVoiceV2.SendVoiceMessage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SendVoiceMessage where
  toJSON SendVoiceMessage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("VoiceId" Data..=) Prelude.<$> voiceId,
            ("TimeToLive" Data..=) Prelude.<$> timeToLive,
            ("MaxPricePerMinute" Data..=)
              Prelude.<$> maxPricePerMinute,
            ("ConfigurationSetName" Data..=)
              Prelude.<$> configurationSetName,
            ("Context" Data..=) Prelude.<$> context,
            ("MessageBody" Data..=) Prelude.<$> messageBody,
            ("DryRun" Data..=) Prelude.<$> dryRun,
            ("MessageBodyTextType" Data..=)
              Prelude.<$> messageBodyTextType,
            Prelude.Just
              ( "DestinationPhoneNumber"
                  Data..= destinationPhoneNumber
              ),
            Prelude.Just
              ("OriginationIdentity" Data..= originationIdentity)
          ]
      )

instance Data.ToPath SendVoiceMessage where
  toPath = Prelude.const "/"

instance Data.ToQuery SendVoiceMessage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSendVoiceMessageResponse' smart constructor.
data SendVoiceMessageResponse = SendVoiceMessageResponse'
  { -- | The unique identifier for the message.
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
-- 'messageId', 'sendVoiceMessageResponse_messageId' - The unique identifier for the message.
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

-- | The unique identifier for the message.
sendVoiceMessageResponse_messageId :: Lens.Lens' SendVoiceMessageResponse (Prelude.Maybe Prelude.Text)
sendVoiceMessageResponse_messageId = Lens.lens (\SendVoiceMessageResponse' {messageId} -> messageId) (\s@SendVoiceMessageResponse' {} a -> s {messageId = a} :: SendVoiceMessageResponse)

-- | The response's http status code.
sendVoiceMessageResponse_httpStatus :: Lens.Lens' SendVoiceMessageResponse Prelude.Int
sendVoiceMessageResponse_httpStatus = Lens.lens (\SendVoiceMessageResponse' {httpStatus} -> httpStatus) (\s@SendVoiceMessageResponse' {} a -> s {httpStatus = a} :: SendVoiceMessageResponse)

instance Prelude.NFData SendVoiceMessageResponse where
  rnf SendVoiceMessageResponse' {..} =
    Prelude.rnf messageId
      `Prelude.seq` Prelude.rnf httpStatus
