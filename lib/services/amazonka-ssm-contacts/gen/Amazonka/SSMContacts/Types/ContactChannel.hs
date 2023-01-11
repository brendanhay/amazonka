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
-- Module      : Amazonka.SSMContacts.Types.ContactChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMContacts.Types.ContactChannel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMContacts.Types.ActivationStatus
import Amazonka.SSMContacts.Types.ChannelType
import Amazonka.SSMContacts.Types.ContactChannelAddress

-- | The method that Incident Manager uses to engage a contact.
--
-- /See:/ 'newContactChannel' smart constructor.
data ContactChannel = ContactChannel'
  { -- | The type of the contact channel. Incident Manager supports three contact
    -- methods:
    --
    -- -   SMS
    --
    -- -   VOICE
    --
    -- -   EMAIL
    type' :: Prelude.Maybe ChannelType,
    -- | The Amazon Resource Name (ARN) of the contact channel.
    contactChannelArn :: Prelude.Text,
    -- | The ARN of the contact that contains the contact channel.
    contactArn :: Prelude.Text,
    -- | The name of the contact channel.
    name :: Prelude.Text,
    -- | The details that Incident Manager uses when trying to engage the contact
    -- channel.
    deliveryAddress :: ContactChannelAddress,
    -- | A Boolean value describing if the contact channel has been activated or
    -- not. If the contact channel isn\'t activated, Incident Manager can\'t
    -- engage the contact through it.
    activationStatus :: ActivationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContactChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'contactChannel_type' - The type of the contact channel. Incident Manager supports three contact
-- methods:
--
-- -   SMS
--
-- -   VOICE
--
-- -   EMAIL
--
-- 'contactChannelArn', 'contactChannel_contactChannelArn' - The Amazon Resource Name (ARN) of the contact channel.
--
-- 'contactArn', 'contactChannel_contactArn' - The ARN of the contact that contains the contact channel.
--
-- 'name', 'contactChannel_name' - The name of the contact channel.
--
-- 'deliveryAddress', 'contactChannel_deliveryAddress' - The details that Incident Manager uses when trying to engage the contact
-- channel.
--
-- 'activationStatus', 'contactChannel_activationStatus' - A Boolean value describing if the contact channel has been activated or
-- not. If the contact channel isn\'t activated, Incident Manager can\'t
-- engage the contact through it.
newContactChannel ::
  -- | 'contactChannelArn'
  Prelude.Text ->
  -- | 'contactArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'deliveryAddress'
  ContactChannelAddress ->
  -- | 'activationStatus'
  ActivationStatus ->
  ContactChannel
newContactChannel
  pContactChannelArn_
  pContactArn_
  pName_
  pDeliveryAddress_
  pActivationStatus_ =
    ContactChannel'
      { type' = Prelude.Nothing,
        contactChannelArn = pContactChannelArn_,
        contactArn = pContactArn_,
        name = pName_,
        deliveryAddress = pDeliveryAddress_,
        activationStatus = pActivationStatus_
      }

-- | The type of the contact channel. Incident Manager supports three contact
-- methods:
--
-- -   SMS
--
-- -   VOICE
--
-- -   EMAIL
contactChannel_type :: Lens.Lens' ContactChannel (Prelude.Maybe ChannelType)
contactChannel_type = Lens.lens (\ContactChannel' {type'} -> type') (\s@ContactChannel' {} a -> s {type' = a} :: ContactChannel)

-- | The Amazon Resource Name (ARN) of the contact channel.
contactChannel_contactChannelArn :: Lens.Lens' ContactChannel Prelude.Text
contactChannel_contactChannelArn = Lens.lens (\ContactChannel' {contactChannelArn} -> contactChannelArn) (\s@ContactChannel' {} a -> s {contactChannelArn = a} :: ContactChannel)

-- | The ARN of the contact that contains the contact channel.
contactChannel_contactArn :: Lens.Lens' ContactChannel Prelude.Text
contactChannel_contactArn = Lens.lens (\ContactChannel' {contactArn} -> contactArn) (\s@ContactChannel' {} a -> s {contactArn = a} :: ContactChannel)

-- | The name of the contact channel.
contactChannel_name :: Lens.Lens' ContactChannel Prelude.Text
contactChannel_name = Lens.lens (\ContactChannel' {name} -> name) (\s@ContactChannel' {} a -> s {name = a} :: ContactChannel)

-- | The details that Incident Manager uses when trying to engage the contact
-- channel.
contactChannel_deliveryAddress :: Lens.Lens' ContactChannel ContactChannelAddress
contactChannel_deliveryAddress = Lens.lens (\ContactChannel' {deliveryAddress} -> deliveryAddress) (\s@ContactChannel' {} a -> s {deliveryAddress = a} :: ContactChannel)

-- | A Boolean value describing if the contact channel has been activated or
-- not. If the contact channel isn\'t activated, Incident Manager can\'t
-- engage the contact through it.
contactChannel_activationStatus :: Lens.Lens' ContactChannel ActivationStatus
contactChannel_activationStatus = Lens.lens (\ContactChannel' {activationStatus} -> activationStatus) (\s@ContactChannel' {} a -> s {activationStatus = a} :: ContactChannel)

instance Data.FromJSON ContactChannel where
  parseJSON =
    Data.withObject
      "ContactChannel"
      ( \x ->
          ContactChannel'
            Prelude.<$> (x Data..:? "Type")
            Prelude.<*> (x Data..: "ContactChannelArn")
            Prelude.<*> (x Data..: "ContactArn")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "DeliveryAddress")
            Prelude.<*> (x Data..: "ActivationStatus")
      )

instance Prelude.Hashable ContactChannel where
  hashWithSalt _salt ContactChannel' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` contactChannelArn
      `Prelude.hashWithSalt` contactArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` deliveryAddress
      `Prelude.hashWithSalt` activationStatus

instance Prelude.NFData ContactChannel where
  rnf ContactChannel' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf contactChannelArn
      `Prelude.seq` Prelude.rnf contactArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf deliveryAddress
      `Prelude.seq` Prelude.rnf activationStatus
