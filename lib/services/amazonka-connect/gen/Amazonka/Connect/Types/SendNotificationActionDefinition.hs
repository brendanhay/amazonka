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
-- Module      : Amazonka.Connect.Types.SendNotificationActionDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.SendNotificationActionDefinition where

import Amazonka.Connect.Types.NotificationContentType
import Amazonka.Connect.Types.NotificationDeliveryType
import Amazonka.Connect.Types.NotificationRecipientType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the send notification action.
--
-- /See:/ 'newSendNotificationActionDefinition' smart constructor.
data SendNotificationActionDefinition = SendNotificationActionDefinition'
  { -- | The subject of the email if the delivery method is @EMAIL@. Supports
    -- variable injection. For more information, see
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/contact-lens-variable-injection.html JSONPath reference>
    -- in the /Amazon Connect Administrators Guide/.
    subject :: Prelude.Maybe Prelude.Text,
    -- | Notification delivery method.
    deliveryMethod :: NotificationDeliveryType,
    -- | Notification content. Supports variable injection. For more information,
    -- see
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/contact-lens-variable-injection.html JSONPath reference>
    -- in the /Amazon Connect Administrators Guide/.
    content :: Prelude.Text,
    -- | Content type format.
    contentType :: NotificationContentType,
    -- | Notification recipient.
    recipient :: NotificationRecipientType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendNotificationActionDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subject', 'sendNotificationActionDefinition_subject' - The subject of the email if the delivery method is @EMAIL@. Supports
-- variable injection. For more information, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/contact-lens-variable-injection.html JSONPath reference>
-- in the /Amazon Connect Administrators Guide/.
--
-- 'deliveryMethod', 'sendNotificationActionDefinition_deliveryMethod' - Notification delivery method.
--
-- 'content', 'sendNotificationActionDefinition_content' - Notification content. Supports variable injection. For more information,
-- see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/contact-lens-variable-injection.html JSONPath reference>
-- in the /Amazon Connect Administrators Guide/.
--
-- 'contentType', 'sendNotificationActionDefinition_contentType' - Content type format.
--
-- 'recipient', 'sendNotificationActionDefinition_recipient' - Notification recipient.
newSendNotificationActionDefinition ::
  -- | 'deliveryMethod'
  NotificationDeliveryType ->
  -- | 'content'
  Prelude.Text ->
  -- | 'contentType'
  NotificationContentType ->
  -- | 'recipient'
  NotificationRecipientType ->
  SendNotificationActionDefinition
newSendNotificationActionDefinition
  pDeliveryMethod_
  pContent_
  pContentType_
  pRecipient_ =
    SendNotificationActionDefinition'
      { subject =
          Prelude.Nothing,
        deliveryMethod = pDeliveryMethod_,
        content = pContent_,
        contentType = pContentType_,
        recipient = pRecipient_
      }

-- | The subject of the email if the delivery method is @EMAIL@. Supports
-- variable injection. For more information, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/contact-lens-variable-injection.html JSONPath reference>
-- in the /Amazon Connect Administrators Guide/.
sendNotificationActionDefinition_subject :: Lens.Lens' SendNotificationActionDefinition (Prelude.Maybe Prelude.Text)
sendNotificationActionDefinition_subject = Lens.lens (\SendNotificationActionDefinition' {subject} -> subject) (\s@SendNotificationActionDefinition' {} a -> s {subject = a} :: SendNotificationActionDefinition)

-- | Notification delivery method.
sendNotificationActionDefinition_deliveryMethod :: Lens.Lens' SendNotificationActionDefinition NotificationDeliveryType
sendNotificationActionDefinition_deliveryMethod = Lens.lens (\SendNotificationActionDefinition' {deliveryMethod} -> deliveryMethod) (\s@SendNotificationActionDefinition' {} a -> s {deliveryMethod = a} :: SendNotificationActionDefinition)

-- | Notification content. Supports variable injection. For more information,
-- see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/contact-lens-variable-injection.html JSONPath reference>
-- in the /Amazon Connect Administrators Guide/.
sendNotificationActionDefinition_content :: Lens.Lens' SendNotificationActionDefinition Prelude.Text
sendNotificationActionDefinition_content = Lens.lens (\SendNotificationActionDefinition' {content} -> content) (\s@SendNotificationActionDefinition' {} a -> s {content = a} :: SendNotificationActionDefinition)

-- | Content type format.
sendNotificationActionDefinition_contentType :: Lens.Lens' SendNotificationActionDefinition NotificationContentType
sendNotificationActionDefinition_contentType = Lens.lens (\SendNotificationActionDefinition' {contentType} -> contentType) (\s@SendNotificationActionDefinition' {} a -> s {contentType = a} :: SendNotificationActionDefinition)

-- | Notification recipient.
sendNotificationActionDefinition_recipient :: Lens.Lens' SendNotificationActionDefinition NotificationRecipientType
sendNotificationActionDefinition_recipient = Lens.lens (\SendNotificationActionDefinition' {recipient} -> recipient) (\s@SendNotificationActionDefinition' {} a -> s {recipient = a} :: SendNotificationActionDefinition)

instance
  Data.FromJSON
    SendNotificationActionDefinition
  where
  parseJSON =
    Data.withObject
      "SendNotificationActionDefinition"
      ( \x ->
          SendNotificationActionDefinition'
            Prelude.<$> (x Data..:? "Subject")
            Prelude.<*> (x Data..: "DeliveryMethod")
            Prelude.<*> (x Data..: "Content")
            Prelude.<*> (x Data..: "ContentType")
            Prelude.<*> (x Data..: "Recipient")
      )

instance
  Prelude.Hashable
    SendNotificationActionDefinition
  where
  hashWithSalt
    _salt
    SendNotificationActionDefinition' {..} =
      _salt
        `Prelude.hashWithSalt` subject
        `Prelude.hashWithSalt` deliveryMethod
        `Prelude.hashWithSalt` content
        `Prelude.hashWithSalt` contentType
        `Prelude.hashWithSalt` recipient

instance
  Prelude.NFData
    SendNotificationActionDefinition
  where
  rnf SendNotificationActionDefinition' {..} =
    Prelude.rnf subject `Prelude.seq`
      Prelude.rnf deliveryMethod `Prelude.seq`
        Prelude.rnf content `Prelude.seq`
          Prelude.rnf contentType `Prelude.seq`
            Prelude.rnf recipient

instance Data.ToJSON SendNotificationActionDefinition where
  toJSON SendNotificationActionDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Subject" Data..=) Prelude.<$> subject,
            Prelude.Just
              ("DeliveryMethod" Data..= deliveryMethod),
            Prelude.Just ("Content" Data..= content),
            Prelude.Just ("ContentType" Data..= contentType),
            Prelude.Just ("Recipient" Data..= recipient)
          ]
      )
