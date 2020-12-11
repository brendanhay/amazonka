-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Contact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Contact
  ( Contact (..),

    -- * Smart constructor
    mkContact,

    -- * Lenses
    cLastName,
    cContactARN,
    cPhoneNumbers,
    cPhoneNumber,
    cSipAddresses,
    cFirstName,
    cDisplayName,
  )
where

import Network.AWS.AlexaBusiness.Types.PhoneNumber
import Network.AWS.AlexaBusiness.Types.SipAddress
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A contact with attributes.
--
-- /See:/ 'mkContact' smart constructor.
data Contact = Contact'
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

-- | Creates a value of 'Contact' with the minimum fields required to make a request.
--
-- * 'contactARN' - The ARN of the contact.
-- * 'displayName' - The name of the contact to display on the console.
-- * 'firstName' - The first name of the contact, used to call the contact on the device.
-- * 'lastName' - The last name of the contact, used to call the contact on the device.
-- * 'phoneNumber' - The phone number of the contact. The phone number type defaults to WORK. You can either specify PhoneNumber or PhoneNumbers. We recommend that you use PhoneNumbers, which lets you specify the phone number type and multiple numbers.
-- * 'phoneNumbers' - The list of phone numbers for the contact.
-- * 'sipAddresses' - The list of SIP addresses for the contact.
mkContact ::
  Contact
mkContact =
  Contact'
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
cLastName :: Lens.Lens' Contact (Lude.Maybe Lude.Text)
cLastName = Lens.lens (lastName :: Contact -> Lude.Maybe Lude.Text) (\s a -> s {lastName = a} :: Contact)
{-# DEPRECATED cLastName "Use generic-lens or generic-optics with 'lastName' instead." #-}

-- | The ARN of the contact.
--
-- /Note:/ Consider using 'contactARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cContactARN :: Lens.Lens' Contact (Lude.Maybe Lude.Text)
cContactARN = Lens.lens (contactARN :: Contact -> Lude.Maybe Lude.Text) (\s a -> s {contactARN = a} :: Contact)
{-# DEPRECATED cContactARN "Use generic-lens or generic-optics with 'contactARN' instead." #-}

-- | The list of phone numbers for the contact.
--
-- /Note:/ Consider using 'phoneNumbers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPhoneNumbers :: Lens.Lens' Contact (Lude.Maybe [PhoneNumber])
cPhoneNumbers = Lens.lens (phoneNumbers :: Contact -> Lude.Maybe [PhoneNumber]) (\s a -> s {phoneNumbers = a} :: Contact)
{-# DEPRECATED cPhoneNumbers "Use generic-lens or generic-optics with 'phoneNumbers' instead." #-}

-- | The phone number of the contact. The phone number type defaults to WORK. You can either specify PhoneNumber or PhoneNumbers. We recommend that you use PhoneNumbers, which lets you specify the phone number type and multiple numbers.
--
-- /Note:/ Consider using 'phoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPhoneNumber :: Lens.Lens' Contact (Lude.Maybe (Lude.Sensitive Lude.Text))
cPhoneNumber = Lens.lens (phoneNumber :: Contact -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {phoneNumber = a} :: Contact)
{-# DEPRECATED cPhoneNumber "Use generic-lens or generic-optics with 'phoneNumber' instead." #-}

-- | The list of SIP addresses for the contact.
--
-- /Note:/ Consider using 'sipAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSipAddresses :: Lens.Lens' Contact (Lude.Maybe [SipAddress])
cSipAddresses = Lens.lens (sipAddresses :: Contact -> Lude.Maybe [SipAddress]) (\s a -> s {sipAddresses = a} :: Contact)
{-# DEPRECATED cSipAddresses "Use generic-lens or generic-optics with 'sipAddresses' instead." #-}

-- | The first name of the contact, used to call the contact on the device.
--
-- /Note:/ Consider using 'firstName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cFirstName :: Lens.Lens' Contact (Lude.Maybe Lude.Text)
cFirstName = Lens.lens (firstName :: Contact -> Lude.Maybe Lude.Text) (\s a -> s {firstName = a} :: Contact)
{-# DEPRECATED cFirstName "Use generic-lens or generic-optics with 'firstName' instead." #-}

-- | The name of the contact to display on the console.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDisplayName :: Lens.Lens' Contact (Lude.Maybe Lude.Text)
cDisplayName = Lens.lens (displayName :: Contact -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: Contact)
{-# DEPRECATED cDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

instance Lude.FromJSON Contact where
  parseJSON =
    Lude.withObject
      "Contact"
      ( \x ->
          Contact'
            Lude.<$> (x Lude..:? "LastName")
            Lude.<*> (x Lude..:? "ContactArn")
            Lude.<*> (x Lude..:? "PhoneNumbers" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "PhoneNumber")
            Lude.<*> (x Lude..:? "SipAddresses" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "FirstName")
            Lude.<*> (x Lude..:? "DisplayName")
      )
