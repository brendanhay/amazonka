{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    cdContactArn,
    cdDisplayName,
    cdFirstName,
    cdLastName,
    cdPhoneNumber,
    cdPhoneNumbers,
    cdSipAddresses,
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

-- | Information related to a contact.
--
-- /See:/ 'mkContactData' smart constructor.
data ContactData = ContactData'
  { -- | The ARN of the contact.
    contactArn :: Core.Maybe Types.Arn,
    -- | The name of the contact to display on the console.
    displayName :: Core.Maybe Types.DisplayName,
    -- | The first name of the contact, used to call the contact on the device.
    firstName :: Core.Maybe Types.FirstName,
    -- | The last name of the contact, used to call the contact on the device.
    lastName :: Core.Maybe Types.LastName,
    -- | The phone number of the contact. The phone number type defaults to WORK. You can specify PhoneNumber or PhoneNumbers. We recommend that you use PhoneNumbers, which lets you specify the phone number type and multiple numbers.
    phoneNumber :: Core.Maybe Types.RawPhoneNumber,
    -- | The list of phone numbers for the contact.
    phoneNumbers :: Core.Maybe [Types.PhoneNumber],
    -- | The list of SIP addresses for the contact.
    sipAddresses :: Core.Maybe [Types.SipAddress]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ContactData' value with any optional fields omitted.
mkContactData ::
  ContactData
mkContactData =
  ContactData'
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
cdContactArn :: Lens.Lens' ContactData (Core.Maybe Types.Arn)
cdContactArn = Lens.field @"contactArn"
{-# DEPRECATED cdContactArn "Use generic-lens or generic-optics with 'contactArn' instead." #-}

-- | The name of the contact to display on the console.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDisplayName :: Lens.Lens' ContactData (Core.Maybe Types.DisplayName)
cdDisplayName = Lens.field @"displayName"
{-# DEPRECATED cdDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The first name of the contact, used to call the contact on the device.
--
-- /Note:/ Consider using 'firstName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdFirstName :: Lens.Lens' ContactData (Core.Maybe Types.FirstName)
cdFirstName = Lens.field @"firstName"
{-# DEPRECATED cdFirstName "Use generic-lens or generic-optics with 'firstName' instead." #-}

-- | The last name of the contact, used to call the contact on the device.
--
-- /Note:/ Consider using 'lastName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdLastName :: Lens.Lens' ContactData (Core.Maybe Types.LastName)
cdLastName = Lens.field @"lastName"
{-# DEPRECATED cdLastName "Use generic-lens or generic-optics with 'lastName' instead." #-}

-- | The phone number of the contact. The phone number type defaults to WORK. You can specify PhoneNumber or PhoneNumbers. We recommend that you use PhoneNumbers, which lets you specify the phone number type and multiple numbers.
--
-- /Note:/ Consider using 'phoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdPhoneNumber :: Lens.Lens' ContactData (Core.Maybe Types.RawPhoneNumber)
cdPhoneNumber = Lens.field @"phoneNumber"
{-# DEPRECATED cdPhoneNumber "Use generic-lens or generic-optics with 'phoneNumber' instead." #-}

-- | The list of phone numbers for the contact.
--
-- /Note:/ Consider using 'phoneNumbers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdPhoneNumbers :: Lens.Lens' ContactData (Core.Maybe [Types.PhoneNumber])
cdPhoneNumbers = Lens.field @"phoneNumbers"
{-# DEPRECATED cdPhoneNumbers "Use generic-lens or generic-optics with 'phoneNumbers' instead." #-}

-- | The list of SIP addresses for the contact.
--
-- /Note:/ Consider using 'sipAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdSipAddresses :: Lens.Lens' ContactData (Core.Maybe [Types.SipAddress])
cdSipAddresses = Lens.field @"sipAddresses"
{-# DEPRECATED cdSipAddresses "Use generic-lens or generic-optics with 'sipAddresses' instead." #-}

instance Core.FromJSON ContactData where
  parseJSON =
    Core.withObject "ContactData" Core.$
      \x ->
        ContactData'
          Core.<$> (x Core..:? "ContactArn")
          Core.<*> (x Core..:? "DisplayName")
          Core.<*> (x Core..:? "FirstName")
          Core.<*> (x Core..:? "LastName")
          Core.<*> (x Core..:? "PhoneNumber")
          Core.<*> (x Core..:? "PhoneNumbers")
          Core.<*> (x Core..:? "SipAddresses")
