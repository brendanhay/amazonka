{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Pinpoint.Types.CampaignSmsMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CampaignSmsMessage where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.MessageType
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the content and settings for an SMS message that\'s sent to
-- recipients of a campaign.
--
-- /See:/ 'newCampaignSmsMessage' smart constructor.
data CampaignSmsMessage = CampaignSmsMessage'
  { -- | The entity ID or Principal Entity (PE) id received from the regulatory
    -- body for sending SMS in your country.
    entityId :: Prelude.Maybe Prelude.Text,
    -- | The body of the SMS message.
    body :: Prelude.Maybe Prelude.Text,
    -- | The sender ID to display on recipients\' devices when they receive the
    -- SMS message.
    senderId :: Prelude.Maybe Prelude.Text,
    -- | The SMS message type. Valid values are TRANSACTIONAL (for messages that
    -- are critical or time-sensitive, such as a one-time passwords) and
    -- PROMOTIONAL (for messsages that aren\'t critical or time-sensitive, such
    -- as marketing messages).
    messageType :: Prelude.Maybe MessageType,
    -- | The template ID received from the regulatory body for sending SMS in
    -- your country.
    templateId :: Prelude.Maybe Prelude.Text,
    -- | The long code to send the SMS message from. This value should be one of
    -- the dedicated long codes that\'s assigned to your AWS account. Although
    -- it isn\'t required, we recommend that you specify the long code using an
    -- E.164 format to ensure prompt and accurate delivery of the message. For
    -- example, +12065550100.
    originationNumber :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CampaignSmsMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entityId', 'campaignSmsMessage_entityId' - The entity ID or Principal Entity (PE) id received from the regulatory
-- body for sending SMS in your country.
--
-- 'body', 'campaignSmsMessage_body' - The body of the SMS message.
--
-- 'senderId', 'campaignSmsMessage_senderId' - The sender ID to display on recipients\' devices when they receive the
-- SMS message.
--
-- 'messageType', 'campaignSmsMessage_messageType' - The SMS message type. Valid values are TRANSACTIONAL (for messages that
-- are critical or time-sensitive, such as a one-time passwords) and
-- PROMOTIONAL (for messsages that aren\'t critical or time-sensitive, such
-- as marketing messages).
--
-- 'templateId', 'campaignSmsMessage_templateId' - The template ID received from the regulatory body for sending SMS in
-- your country.
--
-- 'originationNumber', 'campaignSmsMessage_originationNumber' - The long code to send the SMS message from. This value should be one of
-- the dedicated long codes that\'s assigned to your AWS account. Although
-- it isn\'t required, we recommend that you specify the long code using an
-- E.164 format to ensure prompt and accurate delivery of the message. For
-- example, +12065550100.
newCampaignSmsMessage ::
  CampaignSmsMessage
newCampaignSmsMessage =
  CampaignSmsMessage'
    { entityId = Prelude.Nothing,
      body = Prelude.Nothing,
      senderId = Prelude.Nothing,
      messageType = Prelude.Nothing,
      templateId = Prelude.Nothing,
      originationNumber = Prelude.Nothing
    }

-- | The entity ID or Principal Entity (PE) id received from the regulatory
-- body for sending SMS in your country.
campaignSmsMessage_entityId :: Lens.Lens' CampaignSmsMessage (Prelude.Maybe Prelude.Text)
campaignSmsMessage_entityId = Lens.lens (\CampaignSmsMessage' {entityId} -> entityId) (\s@CampaignSmsMessage' {} a -> s {entityId = a} :: CampaignSmsMessage)

-- | The body of the SMS message.
campaignSmsMessage_body :: Lens.Lens' CampaignSmsMessage (Prelude.Maybe Prelude.Text)
campaignSmsMessage_body = Lens.lens (\CampaignSmsMessage' {body} -> body) (\s@CampaignSmsMessage' {} a -> s {body = a} :: CampaignSmsMessage)

-- | The sender ID to display on recipients\' devices when they receive the
-- SMS message.
campaignSmsMessage_senderId :: Lens.Lens' CampaignSmsMessage (Prelude.Maybe Prelude.Text)
campaignSmsMessage_senderId = Lens.lens (\CampaignSmsMessage' {senderId} -> senderId) (\s@CampaignSmsMessage' {} a -> s {senderId = a} :: CampaignSmsMessage)

-- | The SMS message type. Valid values are TRANSACTIONAL (for messages that
-- are critical or time-sensitive, such as a one-time passwords) and
-- PROMOTIONAL (for messsages that aren\'t critical or time-sensitive, such
-- as marketing messages).
campaignSmsMessage_messageType :: Lens.Lens' CampaignSmsMessage (Prelude.Maybe MessageType)
campaignSmsMessage_messageType = Lens.lens (\CampaignSmsMessage' {messageType} -> messageType) (\s@CampaignSmsMessage' {} a -> s {messageType = a} :: CampaignSmsMessage)

-- | The template ID received from the regulatory body for sending SMS in
-- your country.
campaignSmsMessage_templateId :: Lens.Lens' CampaignSmsMessage (Prelude.Maybe Prelude.Text)
campaignSmsMessage_templateId = Lens.lens (\CampaignSmsMessage' {templateId} -> templateId) (\s@CampaignSmsMessage' {} a -> s {templateId = a} :: CampaignSmsMessage)

-- | The long code to send the SMS message from. This value should be one of
-- the dedicated long codes that\'s assigned to your AWS account. Although
-- it isn\'t required, we recommend that you specify the long code using an
-- E.164 format to ensure prompt and accurate delivery of the message. For
-- example, +12065550100.
campaignSmsMessage_originationNumber :: Lens.Lens' CampaignSmsMessage (Prelude.Maybe Prelude.Text)
campaignSmsMessage_originationNumber = Lens.lens (\CampaignSmsMessage' {originationNumber} -> originationNumber) (\s@CampaignSmsMessage' {} a -> s {originationNumber = a} :: CampaignSmsMessage)

instance Prelude.FromJSON CampaignSmsMessage where
  parseJSON =
    Prelude.withObject
      "CampaignSmsMessage"
      ( \x ->
          CampaignSmsMessage'
            Prelude.<$> (x Prelude..:? "EntityId")
            Prelude.<*> (x Prelude..:? "Body")
            Prelude.<*> (x Prelude..:? "SenderId")
            Prelude.<*> (x Prelude..:? "MessageType")
            Prelude.<*> (x Prelude..:? "TemplateId")
            Prelude.<*> (x Prelude..:? "OriginationNumber")
      )

instance Prelude.Hashable CampaignSmsMessage

instance Prelude.NFData CampaignSmsMessage

instance Prelude.ToJSON CampaignSmsMessage where
  toJSON CampaignSmsMessage' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("EntityId" Prelude..=) Prelude.<$> entityId,
            ("Body" Prelude..=) Prelude.<$> body,
            ("SenderId" Prelude..=) Prelude.<$> senderId,
            ("MessageType" Prelude..=) Prelude.<$> messageType,
            ("TemplateId" Prelude..=) Prelude.<$> templateId,
            ("OriginationNumber" Prelude..=)
              Prelude.<$> originationNumber
          ]
      )
