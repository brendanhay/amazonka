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
-- Module      : Amazonka.PinpointSmsVoiceV2.SendTextMessage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new text message and sends it to a recipient\'s phone number.
--
-- SMS throughput limits are measured in Message Parts per Second (MPS).
-- Your MPS limit depends on the destination country of your messages, as
-- well as the type of phone number (origination number) that you use to
-- send the message. For more information, see
-- <https://docs.aws.amazon.com/pinpoint/latest/userguide/channels-sms-limitations-mps.html Message Parts per Second (MPS) limits>
-- in the /Amazon Pinpoint User Guide/.
module Amazonka.PinpointSmsVoiceV2.SendTextMessage
  ( -- * Creating a Request
    SendTextMessage (..),
    newSendTextMessage,

    -- * Request Lenses
    sendTextMessage_originationIdentity,
    sendTextMessage_timeToLive,
    sendTextMessage_messageType,
    sendTextMessage_configurationSetName,
    sendTextMessage_maxPrice,
    sendTextMessage_context,
    sendTextMessage_messageBody,
    sendTextMessage_dryRun,
    sendTextMessage_keyword,
    sendTextMessage_destinationCountryParameters,
    sendTextMessage_destinationPhoneNumber,

    -- * Destructuring the Response
    SendTextMessageResponse (..),
    newSendTextMessageResponse,

    -- * Response Lenses
    sendTextMessageResponse_messageId,
    sendTextMessageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSendTextMessage' smart constructor.
data SendTextMessage = SendTextMessage'
  { -- | The origination identity of the message. This can be either the
    -- PhoneNumber, PhoneNumberId, PhoneNumberArn, SenderId, SenderIdArn,
    -- PoolId, or PoolArn.
    originationIdentity :: Prelude.Maybe Prelude.Text,
    -- | How long the text message is valid for. By default this is 72 hours.
    timeToLive :: Prelude.Maybe Prelude.Natural,
    -- | The type of message. Valid values are TRANSACTIONAL for messages that
    -- are critical or time-sensitive and PROMOTIONAL for messages that aren\'t
    -- critical or time-sensitive.
    messageType :: Prelude.Maybe MessageType,
    -- | The name of the configuration set to use. This can be either the
    -- ConfigurationSetName or ConfigurationSetArn.
    configurationSetName :: Prelude.Maybe Prelude.Text,
    -- | The maximum amount that you want to spend, in US dollars, per each text
    -- message part. A text message can contain multiple parts.
    maxPrice :: Prelude.Maybe Prelude.Text,
    -- | You can specify custom data in this field. If you do, that data is
    -- logged to the event destination.
    context :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The body of the text message.
    messageBody :: Prelude.Maybe Prelude.Text,
    -- | When set to true, the message is checked and validated, but isn\'t sent
    -- to the end recipient.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | When you register a short code in the US, you must specify a program
    -- name. If you don’t have a US short code, omit this attribute.
    keyword :: Prelude.Maybe Prelude.Text,
    -- | This field is used for any country-specific registration requirements.
    -- Currently, this setting is only used when you send messages to
    -- recipients in India using a sender ID. For more information see
    -- <https://docs.aws.amazon.com/pinpoint/latest/userguide/channels-sms-senderid-india.html Special requirements for sending SMS messages to recipients in India>.
    destinationCountryParameters :: Prelude.Maybe (Prelude.HashMap DestinationCountryParameterKey Prelude.Text),
    -- | The destination phone number in E.164 format.
    destinationPhoneNumber :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendTextMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'originationIdentity', 'sendTextMessage_originationIdentity' - The origination identity of the message. This can be either the
-- PhoneNumber, PhoneNumberId, PhoneNumberArn, SenderId, SenderIdArn,
-- PoolId, or PoolArn.
--
-- 'timeToLive', 'sendTextMessage_timeToLive' - How long the text message is valid for. By default this is 72 hours.
--
-- 'messageType', 'sendTextMessage_messageType' - The type of message. Valid values are TRANSACTIONAL for messages that
-- are critical or time-sensitive and PROMOTIONAL for messages that aren\'t
-- critical or time-sensitive.
--
-- 'configurationSetName', 'sendTextMessage_configurationSetName' - The name of the configuration set to use. This can be either the
-- ConfigurationSetName or ConfigurationSetArn.
--
-- 'maxPrice', 'sendTextMessage_maxPrice' - The maximum amount that you want to spend, in US dollars, per each text
-- message part. A text message can contain multiple parts.
--
-- 'context', 'sendTextMessage_context' - You can specify custom data in this field. If you do, that data is
-- logged to the event destination.
--
-- 'messageBody', 'sendTextMessage_messageBody' - The body of the text message.
--
-- 'dryRun', 'sendTextMessage_dryRun' - When set to true, the message is checked and validated, but isn\'t sent
-- to the end recipient.
--
-- 'keyword', 'sendTextMessage_keyword' - When you register a short code in the US, you must specify a program
-- name. If you don’t have a US short code, omit this attribute.
--
-- 'destinationCountryParameters', 'sendTextMessage_destinationCountryParameters' - This field is used for any country-specific registration requirements.
-- Currently, this setting is only used when you send messages to
-- recipients in India using a sender ID. For more information see
-- <https://docs.aws.amazon.com/pinpoint/latest/userguide/channels-sms-senderid-india.html Special requirements for sending SMS messages to recipients in India>.
--
-- 'destinationPhoneNumber', 'sendTextMessage_destinationPhoneNumber' - The destination phone number in E.164 format.
newSendTextMessage ::
  -- | 'destinationPhoneNumber'
  Prelude.Text ->
  SendTextMessage
newSendTextMessage pDestinationPhoneNumber_ =
  SendTextMessage'
    { originationIdentity =
        Prelude.Nothing,
      timeToLive = Prelude.Nothing,
      messageType = Prelude.Nothing,
      configurationSetName = Prelude.Nothing,
      maxPrice = Prelude.Nothing,
      context = Prelude.Nothing,
      messageBody = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      keyword = Prelude.Nothing,
      destinationCountryParameters = Prelude.Nothing,
      destinationPhoneNumber = pDestinationPhoneNumber_
    }

-- | The origination identity of the message. This can be either the
-- PhoneNumber, PhoneNumberId, PhoneNumberArn, SenderId, SenderIdArn,
-- PoolId, or PoolArn.
sendTextMessage_originationIdentity :: Lens.Lens' SendTextMessage (Prelude.Maybe Prelude.Text)
sendTextMessage_originationIdentity = Lens.lens (\SendTextMessage' {originationIdentity} -> originationIdentity) (\s@SendTextMessage' {} a -> s {originationIdentity = a} :: SendTextMessage)

-- | How long the text message is valid for. By default this is 72 hours.
sendTextMessage_timeToLive :: Lens.Lens' SendTextMessage (Prelude.Maybe Prelude.Natural)
sendTextMessage_timeToLive = Lens.lens (\SendTextMessage' {timeToLive} -> timeToLive) (\s@SendTextMessage' {} a -> s {timeToLive = a} :: SendTextMessage)

-- | The type of message. Valid values are TRANSACTIONAL for messages that
-- are critical or time-sensitive and PROMOTIONAL for messages that aren\'t
-- critical or time-sensitive.
sendTextMessage_messageType :: Lens.Lens' SendTextMessage (Prelude.Maybe MessageType)
sendTextMessage_messageType = Lens.lens (\SendTextMessage' {messageType} -> messageType) (\s@SendTextMessage' {} a -> s {messageType = a} :: SendTextMessage)

-- | The name of the configuration set to use. This can be either the
-- ConfigurationSetName or ConfigurationSetArn.
sendTextMessage_configurationSetName :: Lens.Lens' SendTextMessage (Prelude.Maybe Prelude.Text)
sendTextMessage_configurationSetName = Lens.lens (\SendTextMessage' {configurationSetName} -> configurationSetName) (\s@SendTextMessage' {} a -> s {configurationSetName = a} :: SendTextMessage)

-- | The maximum amount that you want to spend, in US dollars, per each text
-- message part. A text message can contain multiple parts.
sendTextMessage_maxPrice :: Lens.Lens' SendTextMessage (Prelude.Maybe Prelude.Text)
sendTextMessage_maxPrice = Lens.lens (\SendTextMessage' {maxPrice} -> maxPrice) (\s@SendTextMessage' {} a -> s {maxPrice = a} :: SendTextMessage)

-- | You can specify custom data in this field. If you do, that data is
-- logged to the event destination.
sendTextMessage_context :: Lens.Lens' SendTextMessage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
sendTextMessage_context = Lens.lens (\SendTextMessage' {context} -> context) (\s@SendTextMessage' {} a -> s {context = a} :: SendTextMessage) Prelude.. Lens.mapping Lens.coerced

-- | The body of the text message.
sendTextMessage_messageBody :: Lens.Lens' SendTextMessage (Prelude.Maybe Prelude.Text)
sendTextMessage_messageBody = Lens.lens (\SendTextMessage' {messageBody} -> messageBody) (\s@SendTextMessage' {} a -> s {messageBody = a} :: SendTextMessage)

-- | When set to true, the message is checked and validated, but isn\'t sent
-- to the end recipient.
sendTextMessage_dryRun :: Lens.Lens' SendTextMessage (Prelude.Maybe Prelude.Bool)
sendTextMessage_dryRun = Lens.lens (\SendTextMessage' {dryRun} -> dryRun) (\s@SendTextMessage' {} a -> s {dryRun = a} :: SendTextMessage)

-- | When you register a short code in the US, you must specify a program
-- name. If you don’t have a US short code, omit this attribute.
sendTextMessage_keyword :: Lens.Lens' SendTextMessage (Prelude.Maybe Prelude.Text)
sendTextMessage_keyword = Lens.lens (\SendTextMessage' {keyword} -> keyword) (\s@SendTextMessage' {} a -> s {keyword = a} :: SendTextMessage)

-- | This field is used for any country-specific registration requirements.
-- Currently, this setting is only used when you send messages to
-- recipients in India using a sender ID. For more information see
-- <https://docs.aws.amazon.com/pinpoint/latest/userguide/channels-sms-senderid-india.html Special requirements for sending SMS messages to recipients in India>.
sendTextMessage_destinationCountryParameters :: Lens.Lens' SendTextMessage (Prelude.Maybe (Prelude.HashMap DestinationCountryParameterKey Prelude.Text))
sendTextMessage_destinationCountryParameters = Lens.lens (\SendTextMessage' {destinationCountryParameters} -> destinationCountryParameters) (\s@SendTextMessage' {} a -> s {destinationCountryParameters = a} :: SendTextMessage) Prelude.. Lens.mapping Lens.coerced

-- | The destination phone number in E.164 format.
sendTextMessage_destinationPhoneNumber :: Lens.Lens' SendTextMessage Prelude.Text
sendTextMessage_destinationPhoneNumber = Lens.lens (\SendTextMessage' {destinationPhoneNumber} -> destinationPhoneNumber) (\s@SendTextMessage' {} a -> s {destinationPhoneNumber = a} :: SendTextMessage)

instance Core.AWSRequest SendTextMessage where
  type
    AWSResponse SendTextMessage =
      SendTextMessageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SendTextMessageResponse'
            Prelude.<$> (x Data..?> "MessageId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SendTextMessage where
  hashWithSalt _salt SendTextMessage' {..} =
    _salt `Prelude.hashWithSalt` originationIdentity
      `Prelude.hashWithSalt` timeToLive
      `Prelude.hashWithSalt` messageType
      `Prelude.hashWithSalt` configurationSetName
      `Prelude.hashWithSalt` maxPrice
      `Prelude.hashWithSalt` context
      `Prelude.hashWithSalt` messageBody
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` keyword
      `Prelude.hashWithSalt` destinationCountryParameters
      `Prelude.hashWithSalt` destinationPhoneNumber

instance Prelude.NFData SendTextMessage where
  rnf SendTextMessage' {..} =
    Prelude.rnf originationIdentity
      `Prelude.seq` Prelude.rnf timeToLive
      `Prelude.seq` Prelude.rnf messageType
      `Prelude.seq` Prelude.rnf configurationSetName
      `Prelude.seq` Prelude.rnf maxPrice
      `Prelude.seq` Prelude.rnf context
      `Prelude.seq` Prelude.rnf messageBody
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf keyword
      `Prelude.seq` Prelude.rnf destinationCountryParameters
      `Prelude.seq` Prelude.rnf destinationPhoneNumber

instance Data.ToHeaders SendTextMessage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PinpointSMSVoiceV2.SendTextMessage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SendTextMessage where
  toJSON SendTextMessage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("OriginationIdentity" Data..=)
              Prelude.<$> originationIdentity,
            ("TimeToLive" Data..=) Prelude.<$> timeToLive,
            ("MessageType" Data..=) Prelude.<$> messageType,
            ("ConfigurationSetName" Data..=)
              Prelude.<$> configurationSetName,
            ("MaxPrice" Data..=) Prelude.<$> maxPrice,
            ("Context" Data..=) Prelude.<$> context,
            ("MessageBody" Data..=) Prelude.<$> messageBody,
            ("DryRun" Data..=) Prelude.<$> dryRun,
            ("Keyword" Data..=) Prelude.<$> keyword,
            ("DestinationCountryParameters" Data..=)
              Prelude.<$> destinationCountryParameters,
            Prelude.Just
              ( "DestinationPhoneNumber"
                  Data..= destinationPhoneNumber
              )
          ]
      )

instance Data.ToPath SendTextMessage where
  toPath = Prelude.const "/"

instance Data.ToQuery SendTextMessage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSendTextMessageResponse' smart constructor.
data SendTextMessageResponse = SendTextMessageResponse'
  { -- | The unique identifier for the message.
    messageId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendTextMessageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'messageId', 'sendTextMessageResponse_messageId' - The unique identifier for the message.
--
-- 'httpStatus', 'sendTextMessageResponse_httpStatus' - The response's http status code.
newSendTextMessageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SendTextMessageResponse
newSendTextMessageResponse pHttpStatus_ =
  SendTextMessageResponse'
    { messageId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier for the message.
sendTextMessageResponse_messageId :: Lens.Lens' SendTextMessageResponse (Prelude.Maybe Prelude.Text)
sendTextMessageResponse_messageId = Lens.lens (\SendTextMessageResponse' {messageId} -> messageId) (\s@SendTextMessageResponse' {} a -> s {messageId = a} :: SendTextMessageResponse)

-- | The response's http status code.
sendTextMessageResponse_httpStatus :: Lens.Lens' SendTextMessageResponse Prelude.Int
sendTextMessageResponse_httpStatus = Lens.lens (\SendTextMessageResponse' {httpStatus} -> httpStatus) (\s@SendTextMessageResponse' {} a -> s {httpStatus = a} :: SendTextMessageResponse)

instance Prelude.NFData SendTextMessageResponse where
  rnf SendTextMessageResponse' {..} =
    Prelude.rnf messageId
      `Prelude.seq` Prelude.rnf httpStatus
