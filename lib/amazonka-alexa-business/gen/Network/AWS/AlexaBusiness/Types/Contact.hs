{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    cContactArn,
    cDisplayName,
    cFirstName,
    cLastName,
    cPhoneNumber,
    cPhoneNumbers,
    cSipAddresses,
  )
where

import qualified Network.AWS.AlexaBusiness.Types.Arn as Types
import qualified Network.AWS.AlexaBusiness.Types.DisplayName as Types
import qualified Network.AWS.AlexaBusiness.Types.FirstName as Types
import qualified Network.AWS.AlexaBusiness.Types.LastName as Types
import qualified Network.AWS.AlexaBusiness.Types.PhoneNumber as Types
import qualified Network.AWS.AlexaBusiness.Types.RawPhoneNumber as Types
import qualified Network.AWS.AlexaBusiness.Types.SipAddress as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A contact with attributes.
--
-- /See:/ 'mkContact' smart constructor.
data Contact = Contact'
  { -- | The ARN of the contact.
    contactArn :: Core.Maybe Types.Arn,
    -- | The name of the contact to display on the console.
    displayName :: Core.Maybe Types.DisplayName,
    -- | The first name of the contact, used to call the contact on the device.
    firstName :: Core.Maybe Types.FirstName,
    -- | The last name of the contact, used to call the contact on the device.
    lastName :: Core.Maybe Types.LastName,
    -- | The phone number of the contact. The phone number type defaults to WORK. You can either specify PhoneNumber or PhoneNumbers. We recommend that you use PhoneNumbers, which lets you specify the phone number type and multiple numbers.
    phoneNumber :: Core.Maybe Types.RawPhoneNumber,
    -- | The list of phone numbers for the contact.
    phoneNumbers :: Core.Maybe [Types.PhoneNumber],
    -- | The list of SIP addresses for the contact.
    sipAddresses :: Core.Maybe [Types.SipAddress]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Contact' value with any optional fields omitted.
mkContact ::
  Contact
mkContact =
  Contact'
    { contactArn = Core.Nothing,
      displayName = Core.Nothing,
      firstName = Core.Nothing,
      lastName = Core.Nothing,
      phoneNumber = Core.Nothing,
      phoneNumbers = Core.Nothing,
      sipAddresses = Core.Nothing
    }

-- | The ARN of the contact.
--
-- /Note:/ Consider using 'contactArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cContactArn :: Lens.Lens' Contact (Core.Maybe Types.Arn)
cContactArn = Lens.field @"contactArn"
{-# DEPRECATED cContactArn "Use generic-lens or generic-optics with 'contactArn' instead." #-}

-- | The name of the contact to display on the console.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDisplayName :: Lens.Lens' Contact (Core.Maybe Types.DisplayName)
cDisplayName = Lens.field @"displayName"
{-# DEPRECATED cDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The first name of the contact, used to call the contact on the device.
--
-- /Note:/ Consider using 'firstName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cFirstName :: Lens.Lens' Contact (Core.Maybe Types.FirstName)
cFirstName = Lens.field @"firstName"
{-# DEPRECATED cFirstName "Use generic-lens or generic-optics with 'firstName' instead." #-}

-- | The last name of the contact, used to call the contact on the device.
--
-- /Note:/ Consider using 'lastName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLastName :: Lens.Lens' Contact (Core.Maybe Types.LastName)
cLastName = Lens.field @"lastName"
{-# DEPRECATED cLastName "Use generic-lens or generic-optics with 'lastName' instead." #-}

-- | The phone number of the contact. The phone number type defaults to WORK. You can either specify PhoneNumber or PhoneNumbers. We recommend that you use PhoneNumbers, which lets you specify the phone number type and multiple numbers.
--
-- /Note:/ Consider using 'phoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPhoneNumber :: Lens.Lens' Contact (Core.Maybe Types.RawPhoneNumber)
cPhoneNumber = Lens.field @"phoneNumber"
{-# DEPRECATED cPhoneNumber "Use generic-lens or generic-optics with 'phoneNumber' instead." #-}

-- | The list of phone numbers for the contact.
--
-- /Note:/ Consider using 'phoneNumbers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPhoneNumbers :: Lens.Lens' Contact (Core.Maybe [Types.PhoneNumber])
cPhoneNumbers = Lens.field @"phoneNumbers"
{-# DEPRECATED cPhoneNumbers "Use generic-lens or generic-optics with 'phoneNumbers' instead." #-}

-- | The list of SIP addresses for the contact.
--
-- /Note:/ Consider using 'sipAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSipAddresses :: Lens.Lens' Contact (Core.Maybe [Types.SipAddress])
cSipAddresses = Lens.field @"sipAddresses"
{-# DEPRECATED cSipAddresses "Use generic-lens or generic-optics with 'sipAddresses' instead." #-}

instance Core.FromJSON Contact where
  parseJSON =
    Core.withObject "Contact" Core.$
      \x ->
        Contact'
          Core.<$> (x Core..:? "ContactArn")
          Core.<*> (x Core..:? "DisplayName")
          Core.<*> (x Core..:? "FirstName")
          Core.<*> (x Core..:? "LastName")
          Core.<*> (x Core..:? "PhoneNumber")
          Core.<*> (x Core..:? "PhoneNumbers")
          Core.<*> (x Core..:? "SipAddresses")
