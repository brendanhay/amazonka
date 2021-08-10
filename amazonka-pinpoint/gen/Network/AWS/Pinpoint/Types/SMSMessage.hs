{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SMSMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SMSMessage where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.MessageType
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the default settings for a one-time SMS message that\'s sent
-- directly to an endpoint.
--
-- /See:/ 'newSMSMessage' smart constructor.
data SMSMessage = SMSMessage'
  { -- | The SMS program name that you provided to AWS Support when you requested
    -- your dedicated number.
    keyword :: Prelude.Maybe Prelude.Text,
    -- | The entity ID or Principal Entity (PE) id received from the regulatory
    -- body for sending SMS in your country.
    entityId :: Prelude.Maybe Prelude.Text,
    -- | The body of the SMS message.
    body :: Prelude.Maybe Prelude.Text,
    -- | This field is reserved for future use.
    mediaUrl :: Prelude.Maybe Prelude.Text,
    -- | The message variables to use in the SMS message. You can override the
    -- default variables with individual address variables.
    substitutions :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The sender ID to display as the sender of the message on a recipient\'s
    -- device. Support for sender IDs varies by country or region.
    senderId :: Prelude.Maybe Prelude.Text,
    -- | The SMS message type. Valid values are TRANSACTIONAL (for messages that
    -- are critical or time-sensitive, such as a one-time passwords) and
    -- PROMOTIONAL (for messsages that aren\'t critical or time-sensitive, such
    -- as marketing messages).
    messageType :: Prelude.Maybe MessageType,
    -- | The template ID received from the regulatory body for sending SMS in
    -- your country.
    templateId :: Prelude.Maybe Prelude.Text,
    -- | The number to send the SMS message from. This value should be one of the
    -- dedicated long or short codes that\'s assigned to your AWS account. If
    -- you don\'t specify a long or short code, Amazon Pinpoint assigns a
    -- random long code to the SMS message and sends the message from that
    -- code.
    originationNumber :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SMSMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyword', 'sMSMessage_keyword' - The SMS program name that you provided to AWS Support when you requested
-- your dedicated number.
--
-- 'entityId', 'sMSMessage_entityId' - The entity ID or Principal Entity (PE) id received from the regulatory
-- body for sending SMS in your country.
--
-- 'body', 'sMSMessage_body' - The body of the SMS message.
--
-- 'mediaUrl', 'sMSMessage_mediaUrl' - This field is reserved for future use.
--
-- 'substitutions', 'sMSMessage_substitutions' - The message variables to use in the SMS message. You can override the
-- default variables with individual address variables.
--
-- 'senderId', 'sMSMessage_senderId' - The sender ID to display as the sender of the message on a recipient\'s
-- device. Support for sender IDs varies by country or region.
--
-- 'messageType', 'sMSMessage_messageType' - The SMS message type. Valid values are TRANSACTIONAL (for messages that
-- are critical or time-sensitive, such as a one-time passwords) and
-- PROMOTIONAL (for messsages that aren\'t critical or time-sensitive, such
-- as marketing messages).
--
-- 'templateId', 'sMSMessage_templateId' - The template ID received from the regulatory body for sending SMS in
-- your country.
--
-- 'originationNumber', 'sMSMessage_originationNumber' - The number to send the SMS message from. This value should be one of the
-- dedicated long or short codes that\'s assigned to your AWS account. If
-- you don\'t specify a long or short code, Amazon Pinpoint assigns a
-- random long code to the SMS message and sends the message from that
-- code.
newSMSMessage ::
  SMSMessage
newSMSMessage =
  SMSMessage'
    { keyword = Prelude.Nothing,
      entityId = Prelude.Nothing,
      body = Prelude.Nothing,
      mediaUrl = Prelude.Nothing,
      substitutions = Prelude.Nothing,
      senderId = Prelude.Nothing,
      messageType = Prelude.Nothing,
      templateId = Prelude.Nothing,
      originationNumber = Prelude.Nothing
    }

-- | The SMS program name that you provided to AWS Support when you requested
-- your dedicated number.
sMSMessage_keyword :: Lens.Lens' SMSMessage (Prelude.Maybe Prelude.Text)
sMSMessage_keyword = Lens.lens (\SMSMessage' {keyword} -> keyword) (\s@SMSMessage' {} a -> s {keyword = a} :: SMSMessage)

-- | The entity ID or Principal Entity (PE) id received from the regulatory
-- body for sending SMS in your country.
sMSMessage_entityId :: Lens.Lens' SMSMessage (Prelude.Maybe Prelude.Text)
sMSMessage_entityId = Lens.lens (\SMSMessage' {entityId} -> entityId) (\s@SMSMessage' {} a -> s {entityId = a} :: SMSMessage)

-- | The body of the SMS message.
sMSMessage_body :: Lens.Lens' SMSMessage (Prelude.Maybe Prelude.Text)
sMSMessage_body = Lens.lens (\SMSMessage' {body} -> body) (\s@SMSMessage' {} a -> s {body = a} :: SMSMessage)

-- | This field is reserved for future use.
sMSMessage_mediaUrl :: Lens.Lens' SMSMessage (Prelude.Maybe Prelude.Text)
sMSMessage_mediaUrl = Lens.lens (\SMSMessage' {mediaUrl} -> mediaUrl) (\s@SMSMessage' {} a -> s {mediaUrl = a} :: SMSMessage)

-- | The message variables to use in the SMS message. You can override the
-- default variables with individual address variables.
sMSMessage_substitutions :: Lens.Lens' SMSMessage (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
sMSMessage_substitutions = Lens.lens (\SMSMessage' {substitutions} -> substitutions) (\s@SMSMessage' {} a -> s {substitutions = a} :: SMSMessage) Prelude.. Lens.mapping Lens._Coerce

-- | The sender ID to display as the sender of the message on a recipient\'s
-- device. Support for sender IDs varies by country or region.
sMSMessage_senderId :: Lens.Lens' SMSMessage (Prelude.Maybe Prelude.Text)
sMSMessage_senderId = Lens.lens (\SMSMessage' {senderId} -> senderId) (\s@SMSMessage' {} a -> s {senderId = a} :: SMSMessage)

-- | The SMS message type. Valid values are TRANSACTIONAL (for messages that
-- are critical or time-sensitive, such as a one-time passwords) and
-- PROMOTIONAL (for messsages that aren\'t critical or time-sensitive, such
-- as marketing messages).
sMSMessage_messageType :: Lens.Lens' SMSMessage (Prelude.Maybe MessageType)
sMSMessage_messageType = Lens.lens (\SMSMessage' {messageType} -> messageType) (\s@SMSMessage' {} a -> s {messageType = a} :: SMSMessage)

-- | The template ID received from the regulatory body for sending SMS in
-- your country.
sMSMessage_templateId :: Lens.Lens' SMSMessage (Prelude.Maybe Prelude.Text)
sMSMessage_templateId = Lens.lens (\SMSMessage' {templateId} -> templateId) (\s@SMSMessage' {} a -> s {templateId = a} :: SMSMessage)

-- | The number to send the SMS message from. This value should be one of the
-- dedicated long or short codes that\'s assigned to your AWS account. If
-- you don\'t specify a long or short code, Amazon Pinpoint assigns a
-- random long code to the SMS message and sends the message from that
-- code.
sMSMessage_originationNumber :: Lens.Lens' SMSMessage (Prelude.Maybe Prelude.Text)
sMSMessage_originationNumber = Lens.lens (\SMSMessage' {originationNumber} -> originationNumber) (\s@SMSMessage' {} a -> s {originationNumber = a} :: SMSMessage)

instance Prelude.Hashable SMSMessage

instance Prelude.NFData SMSMessage

instance Core.ToJSON SMSMessage where
  toJSON SMSMessage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Keyword" Core..=) Prelude.<$> keyword,
            ("EntityId" Core..=) Prelude.<$> entityId,
            ("Body" Core..=) Prelude.<$> body,
            ("MediaUrl" Core..=) Prelude.<$> mediaUrl,
            ("Substitutions" Core..=) Prelude.<$> substitutions,
            ("SenderId" Core..=) Prelude.<$> senderId,
            ("MessageType" Core..=) Prelude.<$> messageType,
            ("TemplateId" Core..=) Prelude.<$> templateId,
            ("OriginationNumber" Core..=)
              Prelude.<$> originationNumber
          ]
      )
