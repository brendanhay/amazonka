{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.EmergencyContact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.EmergencyContact
  ( EmergencyContact (..),

    -- * Smart constructor
    mkEmergencyContact,

    -- * Lenses
    ecEmailAddress,
    ecContactNotes,
    ecPhoneNumber,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Shield.Types.ContactNotes as Types
import qualified Network.AWS.Shield.Types.EmailAddress as Types
import qualified Network.AWS.Shield.Types.PhoneNumber as Types

-- | Contact information that the DRT can use to contact you if you have proactive engagement enabled, for escalations to the DRT and to initiate proactive customer support.
--
-- /See:/ 'mkEmergencyContact' smart constructor.
data EmergencyContact = EmergencyContact'
  { -- | The email address for the contact.
    emailAddress :: Types.EmailAddress,
    -- | Additional notes regarding the contact.
    contactNotes :: Core.Maybe Types.ContactNotes,
    -- | The phone number for the contact.
    phoneNumber :: Core.Maybe Types.PhoneNumber
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EmergencyContact' value with any optional fields omitted.
mkEmergencyContact ::
  -- | 'emailAddress'
  Types.EmailAddress ->
  EmergencyContact
mkEmergencyContact emailAddress =
  EmergencyContact'
    { emailAddress,
      contactNotes = Core.Nothing,
      phoneNumber = Core.Nothing
    }

-- | The email address for the contact.
--
-- /Note:/ Consider using 'emailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecEmailAddress :: Lens.Lens' EmergencyContact Types.EmailAddress
ecEmailAddress = Lens.field @"emailAddress"
{-# DEPRECATED ecEmailAddress "Use generic-lens or generic-optics with 'emailAddress' instead." #-}

-- | Additional notes regarding the contact.
--
-- /Note:/ Consider using 'contactNotes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecContactNotes :: Lens.Lens' EmergencyContact (Core.Maybe Types.ContactNotes)
ecContactNotes = Lens.field @"contactNotes"
{-# DEPRECATED ecContactNotes "Use generic-lens or generic-optics with 'contactNotes' instead." #-}

-- | The phone number for the contact.
--
-- /Note:/ Consider using 'phoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecPhoneNumber :: Lens.Lens' EmergencyContact (Core.Maybe Types.PhoneNumber)
ecPhoneNumber = Lens.field @"phoneNumber"
{-# DEPRECATED ecPhoneNumber "Use generic-lens or generic-optics with 'phoneNumber' instead." #-}

instance Core.FromJSON EmergencyContact where
  toJSON EmergencyContact {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("EmailAddress" Core..= emailAddress),
            ("ContactNotes" Core..=) Core.<$> contactNotes,
            ("PhoneNumber" Core..=) Core.<$> phoneNumber
          ]
      )

instance Core.FromJSON EmergencyContact where
  parseJSON =
    Core.withObject "EmergencyContact" Core.$
      \x ->
        EmergencyContact'
          Core.<$> (x Core..: "EmailAddress")
          Core.<*> (x Core..:? "ContactNotes")
          Core.<*> (x Core..:? "PhoneNumber")
