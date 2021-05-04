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
-- Module      : Network.AWS.AlexaBusiness.Types.ContactData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.ContactData where

import Network.AWS.AlexaBusiness.Types.PhoneNumber
import Network.AWS.AlexaBusiness.Types.SipAddress
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information related to a contact.
--
-- /See:/ 'newContactData' smart constructor.
data ContactData = ContactData'
  { -- | The phone number of the contact. The phone number type defaults to WORK.
    -- You can specify PhoneNumber or PhoneNumbers. We recommend that you use
    -- PhoneNumbers, which lets you specify the phone number type and multiple
    -- numbers.
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
-- Create a value of 'ContactData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumber', 'contactData_phoneNumber' - The phone number of the contact. The phone number type defaults to WORK.
-- You can specify PhoneNumber or PhoneNumbers. We recommend that you use
-- PhoneNumbers, which lets you specify the phone number type and multiple
-- numbers.
--
-- 'phoneNumbers', 'contactData_phoneNumbers' - The list of phone numbers for the contact.
--
-- 'displayName', 'contactData_displayName' - The name of the contact to display on the console.
--
-- 'contactArn', 'contactData_contactArn' - The ARN of the contact.
--
-- 'firstName', 'contactData_firstName' - The first name of the contact, used to call the contact on the device.
--
-- 'lastName', 'contactData_lastName' - The last name of the contact, used to call the contact on the device.
--
-- 'sipAddresses', 'contactData_sipAddresses' - The list of SIP addresses for the contact.
newContactData ::
  ContactData
newContactData =
  ContactData'
    { phoneNumber = Prelude.Nothing,
      phoneNumbers = Prelude.Nothing,
      displayName = Prelude.Nothing,
      contactArn = Prelude.Nothing,
      firstName = Prelude.Nothing,
      lastName = Prelude.Nothing,
      sipAddresses = Prelude.Nothing
    }

-- | The phone number of the contact. The phone number type defaults to WORK.
-- You can specify PhoneNumber or PhoneNumbers. We recommend that you use
-- PhoneNumbers, which lets you specify the phone number type and multiple
-- numbers.
contactData_phoneNumber :: Lens.Lens' ContactData (Prelude.Maybe Prelude.Text)
contactData_phoneNumber = Lens.lens (\ContactData' {phoneNumber} -> phoneNumber) (\s@ContactData' {} a -> s {phoneNumber = a} :: ContactData) Prelude.. Lens.mapping Prelude._Sensitive

-- | The list of phone numbers for the contact.
contactData_phoneNumbers :: Lens.Lens' ContactData (Prelude.Maybe [PhoneNumber])
contactData_phoneNumbers = Lens.lens (\ContactData' {phoneNumbers} -> phoneNumbers) (\s@ContactData' {} a -> s {phoneNumbers = a} :: ContactData) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the contact to display on the console.
contactData_displayName :: Lens.Lens' ContactData (Prelude.Maybe Prelude.Text)
contactData_displayName = Lens.lens (\ContactData' {displayName} -> displayName) (\s@ContactData' {} a -> s {displayName = a} :: ContactData)

-- | The ARN of the contact.
contactData_contactArn :: Lens.Lens' ContactData (Prelude.Maybe Prelude.Text)
contactData_contactArn = Lens.lens (\ContactData' {contactArn} -> contactArn) (\s@ContactData' {} a -> s {contactArn = a} :: ContactData)

-- | The first name of the contact, used to call the contact on the device.
contactData_firstName :: Lens.Lens' ContactData (Prelude.Maybe Prelude.Text)
contactData_firstName = Lens.lens (\ContactData' {firstName} -> firstName) (\s@ContactData' {} a -> s {firstName = a} :: ContactData)

-- | The last name of the contact, used to call the contact on the device.
contactData_lastName :: Lens.Lens' ContactData (Prelude.Maybe Prelude.Text)
contactData_lastName = Lens.lens (\ContactData' {lastName} -> lastName) (\s@ContactData' {} a -> s {lastName = a} :: ContactData)

-- | The list of SIP addresses for the contact.
contactData_sipAddresses :: Lens.Lens' ContactData (Prelude.Maybe [SipAddress])
contactData_sipAddresses = Lens.lens (\ContactData' {sipAddresses} -> sipAddresses) (\s@ContactData' {} a -> s {sipAddresses = a} :: ContactData) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON ContactData where
  parseJSON =
    Prelude.withObject
      "ContactData"
      ( \x ->
          ContactData'
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

instance Prelude.Hashable ContactData

instance Prelude.NFData ContactData
