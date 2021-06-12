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
-- Module      : Network.AWS.Pinpoint.Types.JourneySMSMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneySMSMessage where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.MessageType

-- | Specifies the sender ID and message type for an SMS message that\'s sent
-- to participants in a journey.
--
-- /See:/ 'newJourneySMSMessage' smart constructor.
data JourneySMSMessage = JourneySMSMessage'
  { -- | The entity ID or Principal Entity (PE) id received from the regulatory
    -- body for sending SMS in your country.
    entityId :: Core.Maybe Core.Text,
    -- | The sender ID to display as the sender of the message on a recipient\'s
    -- device. Support for sender IDs varies by country or region. For more
    -- information, see
    -- <https://docs.aws.amazon.com/pinpoint/latest/userguide/channels-sms-countries.html Supported Countries and Regions>
    -- in the Amazon Pinpoint User Guide.
    senderId :: Core.Maybe Core.Text,
    -- | The SMS message type. Valid values are TRANSACTIONAL (for messages that
    -- are critical or time-sensitive, such as a one-time passwords) and
    -- PROMOTIONAL (for messsages that aren\'t critical or time-sensitive, such
    -- as marketing messages).
    messageType :: Core.Maybe MessageType,
    -- | The template ID received from the regulatory body for sending SMS in
    -- your country.
    templateId :: Core.Maybe Core.Text,
    -- | The long code to send the SMS message from. This value should be one of
    -- the dedicated long codes that\'s assigned to your AWS account. Although
    -- it isn\'t required, we recommend that you specify the long code using an
    -- E.164 format to ensure prompt and accurate delivery of the message. For
    -- example, +12065550100.
    originationNumber :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'JourneySMSMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entityId', 'journeySMSMessage_entityId' - The entity ID or Principal Entity (PE) id received from the regulatory
-- body for sending SMS in your country.
--
-- 'senderId', 'journeySMSMessage_senderId' - The sender ID to display as the sender of the message on a recipient\'s
-- device. Support for sender IDs varies by country or region. For more
-- information, see
-- <https://docs.aws.amazon.com/pinpoint/latest/userguide/channels-sms-countries.html Supported Countries and Regions>
-- in the Amazon Pinpoint User Guide.
--
-- 'messageType', 'journeySMSMessage_messageType' - The SMS message type. Valid values are TRANSACTIONAL (for messages that
-- are critical or time-sensitive, such as a one-time passwords) and
-- PROMOTIONAL (for messsages that aren\'t critical or time-sensitive, such
-- as marketing messages).
--
-- 'templateId', 'journeySMSMessage_templateId' - The template ID received from the regulatory body for sending SMS in
-- your country.
--
-- 'originationNumber', 'journeySMSMessage_originationNumber' - The long code to send the SMS message from. This value should be one of
-- the dedicated long codes that\'s assigned to your AWS account. Although
-- it isn\'t required, we recommend that you specify the long code using an
-- E.164 format to ensure prompt and accurate delivery of the message. For
-- example, +12065550100.
newJourneySMSMessage ::
  JourneySMSMessage
newJourneySMSMessage =
  JourneySMSMessage'
    { entityId = Core.Nothing,
      senderId = Core.Nothing,
      messageType = Core.Nothing,
      templateId = Core.Nothing,
      originationNumber = Core.Nothing
    }

-- | The entity ID or Principal Entity (PE) id received from the regulatory
-- body for sending SMS in your country.
journeySMSMessage_entityId :: Lens.Lens' JourneySMSMessage (Core.Maybe Core.Text)
journeySMSMessage_entityId = Lens.lens (\JourneySMSMessage' {entityId} -> entityId) (\s@JourneySMSMessage' {} a -> s {entityId = a} :: JourneySMSMessage)

-- | The sender ID to display as the sender of the message on a recipient\'s
-- device. Support for sender IDs varies by country or region. For more
-- information, see
-- <https://docs.aws.amazon.com/pinpoint/latest/userguide/channels-sms-countries.html Supported Countries and Regions>
-- in the Amazon Pinpoint User Guide.
journeySMSMessage_senderId :: Lens.Lens' JourneySMSMessage (Core.Maybe Core.Text)
journeySMSMessage_senderId = Lens.lens (\JourneySMSMessage' {senderId} -> senderId) (\s@JourneySMSMessage' {} a -> s {senderId = a} :: JourneySMSMessage)

-- | The SMS message type. Valid values are TRANSACTIONAL (for messages that
-- are critical or time-sensitive, such as a one-time passwords) and
-- PROMOTIONAL (for messsages that aren\'t critical or time-sensitive, such
-- as marketing messages).
journeySMSMessage_messageType :: Lens.Lens' JourneySMSMessage (Core.Maybe MessageType)
journeySMSMessage_messageType = Lens.lens (\JourneySMSMessage' {messageType} -> messageType) (\s@JourneySMSMessage' {} a -> s {messageType = a} :: JourneySMSMessage)

-- | The template ID received from the regulatory body for sending SMS in
-- your country.
journeySMSMessage_templateId :: Lens.Lens' JourneySMSMessage (Core.Maybe Core.Text)
journeySMSMessage_templateId = Lens.lens (\JourneySMSMessage' {templateId} -> templateId) (\s@JourneySMSMessage' {} a -> s {templateId = a} :: JourneySMSMessage)

-- | The long code to send the SMS message from. This value should be one of
-- the dedicated long codes that\'s assigned to your AWS account. Although
-- it isn\'t required, we recommend that you specify the long code using an
-- E.164 format to ensure prompt and accurate delivery of the message. For
-- example, +12065550100.
journeySMSMessage_originationNumber :: Lens.Lens' JourneySMSMessage (Core.Maybe Core.Text)
journeySMSMessage_originationNumber = Lens.lens (\JourneySMSMessage' {originationNumber} -> originationNumber) (\s@JourneySMSMessage' {} a -> s {originationNumber = a} :: JourneySMSMessage)

instance Core.FromJSON JourneySMSMessage where
  parseJSON =
    Core.withObject
      "JourneySMSMessage"
      ( \x ->
          JourneySMSMessage'
            Core.<$> (x Core..:? "EntityId")
            Core.<*> (x Core..:? "SenderId")
            Core.<*> (x Core..:? "MessageType")
            Core.<*> (x Core..:? "TemplateId")
            Core.<*> (x Core..:? "OriginationNumber")
      )

instance Core.Hashable JourneySMSMessage

instance Core.NFData JourneySMSMessage

instance Core.ToJSON JourneySMSMessage where
  toJSON JourneySMSMessage' {..} =
    Core.object
      ( Core.catMaybes
          [ ("EntityId" Core..=) Core.<$> entityId,
            ("SenderId" Core..=) Core.<$> senderId,
            ("MessageType" Core..=) Core.<$> messageType,
            ("TemplateId" Core..=) Core.<$> templateId,
            ("OriginationNumber" Core..=)
              Core.<$> originationNumber
          ]
      )
