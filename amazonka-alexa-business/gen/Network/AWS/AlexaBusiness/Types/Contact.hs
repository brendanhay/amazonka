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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A contact with attributes.
--
-- /See:/ 'newContact' smart constructor.
data Contact = Contact'
  { -- | The phone number of the contact. The phone number type defaults to WORK.
    -- You can either specify PhoneNumber or PhoneNumbers. We recommend that
    -- you use PhoneNumbers, which lets you specify the phone number type and
    -- multiple numbers.
    phoneNumber :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The list of phone numbers for the contact.
    phoneNumbers :: Core.Maybe [PhoneNumber],
    -- | The name of the contact to display on the console.
    displayName :: Core.Maybe Core.Text,
    -- | The ARN of the contact.
    contactArn :: Core.Maybe Core.Text,
    -- | The first name of the contact, used to call the contact on the device.
    firstName :: Core.Maybe Core.Text,
    -- | The last name of the contact, used to call the contact on the device.
    lastName :: Core.Maybe Core.Text,
    -- | The list of SIP addresses for the contact.
    sipAddresses :: Core.Maybe [SipAddress]
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
    { phoneNumber = Core.Nothing,
      phoneNumbers = Core.Nothing,
      displayName = Core.Nothing,
      contactArn = Core.Nothing,
      firstName = Core.Nothing,
      lastName = Core.Nothing,
      sipAddresses = Core.Nothing
    }

-- | The phone number of the contact. The phone number type defaults to WORK.
-- You can either specify PhoneNumber or PhoneNumbers. We recommend that
-- you use PhoneNumbers, which lets you specify the phone number type and
-- multiple numbers.
contact_phoneNumber :: Lens.Lens' Contact (Core.Maybe Core.Text)
contact_phoneNumber = Lens.lens (\Contact' {phoneNumber} -> phoneNumber) (\s@Contact' {} a -> s {phoneNumber = a} :: Contact) Core.. Lens.mapping Core._Sensitive

-- | The list of phone numbers for the contact.
contact_phoneNumbers :: Lens.Lens' Contact (Core.Maybe [PhoneNumber])
contact_phoneNumbers = Lens.lens (\Contact' {phoneNumbers} -> phoneNumbers) (\s@Contact' {} a -> s {phoneNumbers = a} :: Contact) Core.. Lens.mapping Lens._Coerce

-- | The name of the contact to display on the console.
contact_displayName :: Lens.Lens' Contact (Core.Maybe Core.Text)
contact_displayName = Lens.lens (\Contact' {displayName} -> displayName) (\s@Contact' {} a -> s {displayName = a} :: Contact)

-- | The ARN of the contact.
contact_contactArn :: Lens.Lens' Contact (Core.Maybe Core.Text)
contact_contactArn = Lens.lens (\Contact' {contactArn} -> contactArn) (\s@Contact' {} a -> s {contactArn = a} :: Contact)

-- | The first name of the contact, used to call the contact on the device.
contact_firstName :: Lens.Lens' Contact (Core.Maybe Core.Text)
contact_firstName = Lens.lens (\Contact' {firstName} -> firstName) (\s@Contact' {} a -> s {firstName = a} :: Contact)

-- | The last name of the contact, used to call the contact on the device.
contact_lastName :: Lens.Lens' Contact (Core.Maybe Core.Text)
contact_lastName = Lens.lens (\Contact' {lastName} -> lastName) (\s@Contact' {} a -> s {lastName = a} :: Contact)

-- | The list of SIP addresses for the contact.
contact_sipAddresses :: Lens.Lens' Contact (Core.Maybe [SipAddress])
contact_sipAddresses = Lens.lens (\Contact' {sipAddresses} -> sipAddresses) (\s@Contact' {} a -> s {sipAddresses = a} :: Contact) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON Contact where
  parseJSON =
    Core.withObject
      "Contact"
      ( \x ->
          Contact'
            Core.<$> (x Core..:? "PhoneNumber")
            Core.<*> (x Core..:? "PhoneNumbers" Core..!= Core.mempty)
            Core.<*> (x Core..:? "DisplayName")
            Core.<*> (x Core..:? "ContactArn")
            Core.<*> (x Core..:? "FirstName")
            Core.<*> (x Core..:? "LastName")
            Core.<*> (x Core..:? "SipAddresses" Core..!= Core.mempty)
      )

instance Core.Hashable Contact

instance Core.NFData Contact
