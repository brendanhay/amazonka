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
-- Module      : Network.AWS.AlexaBusiness.Types.Contact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Contact where

import Network.AWS.AlexaBusiness.Types.PhoneNumber
import Network.AWS.AlexaBusiness.Types.SipAddress
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A contact with attributes.
--
-- /See:/ 'newContact' smart constructor.
data Contact = Contact'
  { -- | The phone number of the contact. The phone number type defaults to WORK.
    -- You can either specify PhoneNumber or PhoneNumbers. We recommend that
    -- you use PhoneNumbers, which lets you specify the phone number type and
    -- multiple numbers.
    phoneNumber :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The list of phone numbers for the contact.
    phoneNumbers :: Prelude.Maybe [PhoneNumber],
    -- | The name of the contact to display on the console.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the contact.
    contactArn :: Prelude.Maybe Prelude.Text,
    -- | The first name of the contact, used to call the contact on the device.
    firstName :: Prelude.Maybe Prelude.Text,
    -- | The last name of the contact, used to call the contact on the device.
    lastName :: Prelude.Maybe Prelude.Text,
    -- | The list of SIP addresses for the contact.
    sipAddresses :: Prelude.Maybe [SipAddress]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Contact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumber', 'contact_phoneNumber' - The phone number of the contact. The phone number type defaults to WORK.
-- You can either specify PhoneNumber or PhoneNumbers. We recommend that
-- you use PhoneNumbers, which lets you specify the phone number type and
-- multiple numbers.
--
-- 'phoneNumbers', 'contact_phoneNumbers' - The list of phone numbers for the contact.
--
-- 'displayName', 'contact_displayName' - The name of the contact to display on the console.
--
-- 'contactArn', 'contact_contactArn' - The ARN of the contact.
--
-- 'firstName', 'contact_firstName' - The first name of the contact, used to call the contact on the device.
--
-- 'lastName', 'contact_lastName' - The last name of the contact, used to call the contact on the device.
--
-- 'sipAddresses', 'contact_sipAddresses' - The list of SIP addresses for the contact.
newContact ::
  Contact
newContact =
  Contact'
    { phoneNumber = Prelude.Nothing,
      phoneNumbers = Prelude.Nothing,
      displayName = Prelude.Nothing,
      contactArn = Prelude.Nothing,
      firstName = Prelude.Nothing,
      lastName = Prelude.Nothing,
      sipAddresses = Prelude.Nothing
    }

-- | The phone number of the contact. The phone number type defaults to WORK.
-- You can either specify PhoneNumber or PhoneNumbers. We recommend that
-- you use PhoneNumbers, which lets you specify the phone number type and
-- multiple numbers.
contact_phoneNumber :: Lens.Lens' Contact (Prelude.Maybe Prelude.Text)
contact_phoneNumber = Lens.lens (\Contact' {phoneNumber} -> phoneNumber) (\s@Contact' {} a -> s {phoneNumber = a} :: Contact) Prelude.. Lens.mapping Prelude._Sensitive

-- | The list of phone numbers for the contact.
contact_phoneNumbers :: Lens.Lens' Contact (Prelude.Maybe [PhoneNumber])
contact_phoneNumbers = Lens.lens (\Contact' {phoneNumbers} -> phoneNumbers) (\s@Contact' {} a -> s {phoneNumbers = a} :: Contact) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the contact to display on the console.
contact_displayName :: Lens.Lens' Contact (Prelude.Maybe Prelude.Text)
contact_displayName = Lens.lens (\Contact' {displayName} -> displayName) (\s@Contact' {} a -> s {displayName = a} :: Contact)

-- | The ARN of the contact.
contact_contactArn :: Lens.Lens' Contact (Prelude.Maybe Prelude.Text)
contact_contactArn = Lens.lens (\Contact' {contactArn} -> contactArn) (\s@Contact' {} a -> s {contactArn = a} :: Contact)

-- | The first name of the contact, used to call the contact on the device.
contact_firstName :: Lens.Lens' Contact (Prelude.Maybe Prelude.Text)
contact_firstName = Lens.lens (\Contact' {firstName} -> firstName) (\s@Contact' {} a -> s {firstName = a} :: Contact)

-- | The last name of the contact, used to call the contact on the device.
contact_lastName :: Lens.Lens' Contact (Prelude.Maybe Prelude.Text)
contact_lastName = Lens.lens (\Contact' {lastName} -> lastName) (\s@Contact' {} a -> s {lastName = a} :: Contact)

-- | The list of SIP addresses for the contact.
contact_sipAddresses :: Lens.Lens' Contact (Prelude.Maybe [SipAddress])
contact_sipAddresses = Lens.lens (\Contact' {sipAddresses} -> sipAddresses) (\s@Contact' {} a -> s {sipAddresses = a} :: Contact) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON Contact where
  parseJSON =
    Prelude.withObject
      "Contact"
      ( \x ->
          Contact'
            Prelude.<$> (x Prelude..:? "PhoneNumber")
            Prelude.<*> ( x Prelude..:? "PhoneNumbers"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "DisplayName")
            Prelude.<*> (x Prelude..:? "ContactArn")
            Prelude.<*> (x Prelude..:? "FirstName")
            Prelude.<*> (x Prelude..:? "LastName")
            Prelude.<*> ( x Prelude..:? "SipAddresses"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Contact

instance Prelude.NFData Contact
