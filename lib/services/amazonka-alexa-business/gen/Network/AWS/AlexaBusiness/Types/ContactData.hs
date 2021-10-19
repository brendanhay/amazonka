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
-- Module      : Network.AWS.AlexaBusiness.Types.ContactData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.ContactData where

import Network.AWS.AlexaBusiness.Types.PhoneNumber
import Network.AWS.AlexaBusiness.Types.SipAddress
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information related to a contact.
--
-- /See:/ 'newContactData' smart constructor.
data ContactData = ContactData'
  { -- | The last name of the contact, used to call the contact on the device.
    lastName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the contact.
    contactArn :: Prelude.Maybe Prelude.Text,
    -- | The list of phone numbers for the contact.
    phoneNumbers :: Prelude.Maybe [PhoneNumber],
    -- | The phone number of the contact. The phone number type defaults to WORK.
    -- You can specify PhoneNumber or PhoneNumbers. We recommend that you use
    -- PhoneNumbers, which lets you specify the phone number type and multiple
    -- numbers.
    phoneNumber :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The list of SIP addresses for the contact.
    sipAddresses :: Prelude.Maybe [SipAddress],
    -- | The first name of the contact, used to call the contact on the device.
    firstName :: Prelude.Maybe Prelude.Text,
    -- | The name of the contact to display on the console.
    displayName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContactData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastName', 'contactData_lastName' - The last name of the contact, used to call the contact on the device.
--
-- 'contactArn', 'contactData_contactArn' - The ARN of the contact.
--
-- 'phoneNumbers', 'contactData_phoneNumbers' - The list of phone numbers for the contact.
--
-- 'phoneNumber', 'contactData_phoneNumber' - The phone number of the contact. The phone number type defaults to WORK.
-- You can specify PhoneNumber or PhoneNumbers. We recommend that you use
-- PhoneNumbers, which lets you specify the phone number type and multiple
-- numbers.
--
-- 'sipAddresses', 'contactData_sipAddresses' - The list of SIP addresses for the contact.
--
-- 'firstName', 'contactData_firstName' - The first name of the contact, used to call the contact on the device.
--
-- 'displayName', 'contactData_displayName' - The name of the contact to display on the console.
newContactData ::
  ContactData
newContactData =
  ContactData'
    { lastName = Prelude.Nothing,
      contactArn = Prelude.Nothing,
      phoneNumbers = Prelude.Nothing,
      phoneNumber = Prelude.Nothing,
      sipAddresses = Prelude.Nothing,
      firstName = Prelude.Nothing,
      displayName = Prelude.Nothing
    }

-- | The last name of the contact, used to call the contact on the device.
contactData_lastName :: Lens.Lens' ContactData (Prelude.Maybe Prelude.Text)
contactData_lastName = Lens.lens (\ContactData' {lastName} -> lastName) (\s@ContactData' {} a -> s {lastName = a} :: ContactData)

-- | The ARN of the contact.
contactData_contactArn :: Lens.Lens' ContactData (Prelude.Maybe Prelude.Text)
contactData_contactArn = Lens.lens (\ContactData' {contactArn} -> contactArn) (\s@ContactData' {} a -> s {contactArn = a} :: ContactData)

-- | The list of phone numbers for the contact.
contactData_phoneNumbers :: Lens.Lens' ContactData (Prelude.Maybe [PhoneNumber])
contactData_phoneNumbers = Lens.lens (\ContactData' {phoneNumbers} -> phoneNumbers) (\s@ContactData' {} a -> s {phoneNumbers = a} :: ContactData) Prelude.. Lens.mapping Lens.coerced

-- | The phone number of the contact. The phone number type defaults to WORK.
-- You can specify PhoneNumber or PhoneNumbers. We recommend that you use
-- PhoneNumbers, which lets you specify the phone number type and multiple
-- numbers.
contactData_phoneNumber :: Lens.Lens' ContactData (Prelude.Maybe Prelude.Text)
contactData_phoneNumber = Lens.lens (\ContactData' {phoneNumber} -> phoneNumber) (\s@ContactData' {} a -> s {phoneNumber = a} :: ContactData) Prelude.. Lens.mapping Core._Sensitive

-- | The list of SIP addresses for the contact.
contactData_sipAddresses :: Lens.Lens' ContactData (Prelude.Maybe [SipAddress])
contactData_sipAddresses = Lens.lens (\ContactData' {sipAddresses} -> sipAddresses) (\s@ContactData' {} a -> s {sipAddresses = a} :: ContactData) Prelude.. Lens.mapping Lens.coerced

-- | The first name of the contact, used to call the contact on the device.
contactData_firstName :: Lens.Lens' ContactData (Prelude.Maybe Prelude.Text)
contactData_firstName = Lens.lens (\ContactData' {firstName} -> firstName) (\s@ContactData' {} a -> s {firstName = a} :: ContactData)

-- | The name of the contact to display on the console.
contactData_displayName :: Lens.Lens' ContactData (Prelude.Maybe Prelude.Text)
contactData_displayName = Lens.lens (\ContactData' {displayName} -> displayName) (\s@ContactData' {} a -> s {displayName = a} :: ContactData)

instance Core.FromJSON ContactData where
  parseJSON =
    Core.withObject
      "ContactData"
      ( \x ->
          ContactData'
            Prelude.<$> (x Core..:? "LastName")
            Prelude.<*> (x Core..:? "ContactArn")
            Prelude.<*> (x Core..:? "PhoneNumbers" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "PhoneNumber")
            Prelude.<*> (x Core..:? "SipAddresses" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "FirstName")
            Prelude.<*> (x Core..:? "DisplayName")
      )

instance Prelude.Hashable ContactData

instance Prelude.NFData ContactData
