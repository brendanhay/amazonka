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
-- Module      : Amazonka.AlexaBusiness.Types.Contact
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.Contact where

import Amazonka.AlexaBusiness.Types.PhoneNumber
import Amazonka.AlexaBusiness.Types.SipAddress
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A contact with attributes.
--
-- /See:/ 'newContact' smart constructor.
data Contact = Contact'
  { -- | The ARN of the contact.
    contactArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the contact to display on the console.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The first name of the contact, used to call the contact on the device.
    firstName :: Prelude.Maybe Prelude.Text,
    -- | The last name of the contact, used to call the contact on the device.
    lastName :: Prelude.Maybe Prelude.Text,
    -- | The phone number of the contact. The phone number type defaults to WORK.
    -- You can either specify PhoneNumber or PhoneNumbers. We recommend that
    -- you use PhoneNumbers, which lets you specify the phone number type and
    -- multiple numbers.
    phoneNumber :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The list of phone numbers for the contact.
    phoneNumbers :: Prelude.Maybe [PhoneNumber],
    -- | The list of SIP addresses for the contact.
    sipAddresses :: Prelude.Maybe [SipAddress]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Contact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactArn', 'contact_contactArn' - The ARN of the contact.
--
-- 'displayName', 'contact_displayName' - The name of the contact to display on the console.
--
-- 'firstName', 'contact_firstName' - The first name of the contact, used to call the contact on the device.
--
-- 'lastName', 'contact_lastName' - The last name of the contact, used to call the contact on the device.
--
-- 'phoneNumber', 'contact_phoneNumber' - The phone number of the contact. The phone number type defaults to WORK.
-- You can either specify PhoneNumber or PhoneNumbers. We recommend that
-- you use PhoneNumbers, which lets you specify the phone number type and
-- multiple numbers.
--
-- 'phoneNumbers', 'contact_phoneNumbers' - The list of phone numbers for the contact.
--
-- 'sipAddresses', 'contact_sipAddresses' - The list of SIP addresses for the contact.
newContact ::
  Contact
newContact =
  Contact'
    { contactArn = Prelude.Nothing,
      displayName = Prelude.Nothing,
      firstName = Prelude.Nothing,
      lastName = Prelude.Nothing,
      phoneNumber = Prelude.Nothing,
      phoneNumbers = Prelude.Nothing,
      sipAddresses = Prelude.Nothing
    }

-- | The ARN of the contact.
contact_contactArn :: Lens.Lens' Contact (Prelude.Maybe Prelude.Text)
contact_contactArn = Lens.lens (\Contact' {contactArn} -> contactArn) (\s@Contact' {} a -> s {contactArn = a} :: Contact)

-- | The name of the contact to display on the console.
contact_displayName :: Lens.Lens' Contact (Prelude.Maybe Prelude.Text)
contact_displayName = Lens.lens (\Contact' {displayName} -> displayName) (\s@Contact' {} a -> s {displayName = a} :: Contact)

-- | The first name of the contact, used to call the contact on the device.
contact_firstName :: Lens.Lens' Contact (Prelude.Maybe Prelude.Text)
contact_firstName = Lens.lens (\Contact' {firstName} -> firstName) (\s@Contact' {} a -> s {firstName = a} :: Contact)

-- | The last name of the contact, used to call the contact on the device.
contact_lastName :: Lens.Lens' Contact (Prelude.Maybe Prelude.Text)
contact_lastName = Lens.lens (\Contact' {lastName} -> lastName) (\s@Contact' {} a -> s {lastName = a} :: Contact)

-- | The phone number of the contact. The phone number type defaults to WORK.
-- You can either specify PhoneNumber or PhoneNumbers. We recommend that
-- you use PhoneNumbers, which lets you specify the phone number type and
-- multiple numbers.
contact_phoneNumber :: Lens.Lens' Contact (Prelude.Maybe Prelude.Text)
contact_phoneNumber = Lens.lens (\Contact' {phoneNumber} -> phoneNumber) (\s@Contact' {} a -> s {phoneNumber = a} :: Contact) Prelude.. Lens.mapping Data._Sensitive

-- | The list of phone numbers for the contact.
contact_phoneNumbers :: Lens.Lens' Contact (Prelude.Maybe [PhoneNumber])
contact_phoneNumbers = Lens.lens (\Contact' {phoneNumbers} -> phoneNumbers) (\s@Contact' {} a -> s {phoneNumbers = a} :: Contact) Prelude.. Lens.mapping Lens.coerced

-- | The list of SIP addresses for the contact.
contact_sipAddresses :: Lens.Lens' Contact (Prelude.Maybe [SipAddress])
contact_sipAddresses = Lens.lens (\Contact' {sipAddresses} -> sipAddresses) (\s@Contact' {} a -> s {sipAddresses = a} :: Contact) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Contact where
  parseJSON =
    Data.withObject
      "Contact"
      ( \x ->
          Contact'
            Prelude.<$> (x Data..:? "ContactArn")
            Prelude.<*> (x Data..:? "DisplayName")
            Prelude.<*> (x Data..:? "FirstName")
            Prelude.<*> (x Data..:? "LastName")
            Prelude.<*> (x Data..:? "PhoneNumber")
            Prelude.<*> (x Data..:? "PhoneNumbers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SipAddresses" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Contact where
  hashWithSalt _salt Contact' {..} =
    _salt `Prelude.hashWithSalt` contactArn
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` firstName
      `Prelude.hashWithSalt` lastName
      `Prelude.hashWithSalt` phoneNumber
      `Prelude.hashWithSalt` phoneNumbers
      `Prelude.hashWithSalt` sipAddresses

instance Prelude.NFData Contact where
  rnf Contact' {..} =
    Prelude.rnf contactArn
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf firstName
      `Prelude.seq` Prelude.rnf lastName
      `Prelude.seq` Prelude.rnf phoneNumber
      `Prelude.seq` Prelude.rnf phoneNumbers
      `Prelude.seq` Prelude.rnf sipAddresses
