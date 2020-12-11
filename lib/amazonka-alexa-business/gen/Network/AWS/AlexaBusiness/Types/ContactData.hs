-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.ContactData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.ContactData
  ( ContactData (..),

    -- * Smart constructor
    mkContactData,

    -- * Lenses
    cdLastName,
    cdContactARN,
    cdPhoneNumbers,
    cdPhoneNumber,
    cdSipAddresses,
    cdFirstName,
    cdDisplayName,
  )
where

import Network.AWS.AlexaBusiness.Types.PhoneNumber
import Network.AWS.AlexaBusiness.Types.SipAddress
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information related to a contact.
--
-- /See:/ 'mkContactData' smart constructor.
data ContactData = ContactData'
  { lastName :: Lude.Maybe Lude.Text,
    contactARN :: Lude.Maybe Lude.Text,
    phoneNumbers :: Lude.Maybe [PhoneNumber],
    phoneNumber :: Lude.Maybe (Lude.Sensitive Lude.Text),
    sipAddresses :: Lude.Maybe [SipAddress],
    firstName :: Lude.Maybe Lude.Text,
    displayName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContactData' with the minimum fields required to make a request.
--
-- * 'contactARN' - The ARN of the contact.
-- * 'displayName' - The name of the contact to display on the console.
-- * 'firstName' - The first name of the contact, used to call the contact on the device.
-- * 'lastName' - The last name of the contact, used to call the contact on the device.
-- * 'phoneNumber' - The phone number of the contact. The phone number type defaults to WORK. You can specify PhoneNumber or PhoneNumbers. We recommend that you use PhoneNumbers, which lets you specify the phone number type and multiple numbers.
-- * 'phoneNumbers' - The list of phone numbers for the contact.
-- * 'sipAddresses' - The list of SIP addresses for the contact.
mkContactData ::
  ContactData
mkContactData =
  ContactData'
    { lastName = Lude.Nothing,
      contactARN = Lude.Nothing,
      phoneNumbers = Lude.Nothing,
      phoneNumber = Lude.Nothing,
      sipAddresses = Lude.Nothing,
      firstName = Lude.Nothing,
      displayName = Lude.Nothing
    }

-- | The last name of the contact, used to call the contact on the device.
--
-- /Note:/ Consider using 'lastName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdLastName :: Lens.Lens' ContactData (Lude.Maybe Lude.Text)
cdLastName = Lens.lens (lastName :: ContactData -> Lude.Maybe Lude.Text) (\s a -> s {lastName = a} :: ContactData)
{-# DEPRECATED cdLastName "Use generic-lens or generic-optics with 'lastName' instead." #-}

-- | The ARN of the contact.
--
-- /Note:/ Consider using 'contactARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdContactARN :: Lens.Lens' ContactData (Lude.Maybe Lude.Text)
cdContactARN = Lens.lens (contactARN :: ContactData -> Lude.Maybe Lude.Text) (\s a -> s {contactARN = a} :: ContactData)
{-# DEPRECATED cdContactARN "Use generic-lens or generic-optics with 'contactARN' instead." #-}

-- | The list of phone numbers for the contact.
--
-- /Note:/ Consider using 'phoneNumbers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdPhoneNumbers :: Lens.Lens' ContactData (Lude.Maybe [PhoneNumber])
cdPhoneNumbers = Lens.lens (phoneNumbers :: ContactData -> Lude.Maybe [PhoneNumber]) (\s a -> s {phoneNumbers = a} :: ContactData)
{-# DEPRECATED cdPhoneNumbers "Use generic-lens or generic-optics with 'phoneNumbers' instead." #-}

-- | The phone number of the contact. The phone number type defaults to WORK. You can specify PhoneNumber or PhoneNumbers. We recommend that you use PhoneNumbers, which lets you specify the phone number type and multiple numbers.
--
-- /Note:/ Consider using 'phoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdPhoneNumber :: Lens.Lens' ContactData (Lude.Maybe (Lude.Sensitive Lude.Text))
cdPhoneNumber = Lens.lens (phoneNumber :: ContactData -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {phoneNumber = a} :: ContactData)
{-# DEPRECATED cdPhoneNumber "Use generic-lens or generic-optics with 'phoneNumber' instead." #-}

-- | The list of SIP addresses for the contact.
--
-- /Note:/ Consider using 'sipAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdSipAddresses :: Lens.Lens' ContactData (Lude.Maybe [SipAddress])
cdSipAddresses = Lens.lens (sipAddresses :: ContactData -> Lude.Maybe [SipAddress]) (\s a -> s {sipAddresses = a} :: ContactData)
{-# DEPRECATED cdSipAddresses "Use generic-lens or generic-optics with 'sipAddresses' instead." #-}

-- | The first name of the contact, used to call the contact on the device.
--
-- /Note:/ Consider using 'firstName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdFirstName :: Lens.Lens' ContactData (Lude.Maybe Lude.Text)
cdFirstName = Lens.lens (firstName :: ContactData -> Lude.Maybe Lude.Text) (\s a -> s {firstName = a} :: ContactData)
{-# DEPRECATED cdFirstName "Use generic-lens or generic-optics with 'firstName' instead." #-}

-- | The name of the contact to display on the console.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDisplayName :: Lens.Lens' ContactData (Lude.Maybe Lude.Text)
cdDisplayName = Lens.lens (displayName :: ContactData -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: ContactData)
{-# DEPRECATED cdDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

instance Lude.FromJSON ContactData where
  parseJSON =
    Lude.withObject
      "ContactData"
      ( \x ->
          ContactData'
            Lude.<$> (x Lude..:? "LastName")
            Lude.<*> (x Lude..:? "ContactArn")
            Lude.<*> (x Lude..:? "PhoneNumbers" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "PhoneNumber")
            Lude.<*> (x Lude..:? "SipAddresses" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "FirstName")
            Lude.<*> (x Lude..:? "DisplayName")
      )
