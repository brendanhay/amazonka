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
    ecPhoneNumber,
    ecEmailAddress,
    ecContactNotes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contact information that the DRT can use to contact you if you have proactive engagement enabled, for escalations to the DRT and to initiate proactive customer support.
--
-- /See:/ 'mkEmergencyContact' smart constructor.
data EmergencyContact = EmergencyContact'
  { -- | The phone number for the contact.
    phoneNumber :: Lude.Maybe Lude.Text,
    -- | The email address for the contact.
    emailAddress :: Lude.Text,
    -- | Additional notes regarding the contact.
    contactNotes :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EmergencyContact' with the minimum fields required to make a request.
--
-- * 'phoneNumber' - The phone number for the contact.
-- * 'emailAddress' - The email address for the contact.
-- * 'contactNotes' - Additional notes regarding the contact.
mkEmergencyContact ::
  -- | 'emailAddress'
  Lude.Text ->
  EmergencyContact
mkEmergencyContact pEmailAddress_ =
  EmergencyContact'
    { phoneNumber = Lude.Nothing,
      emailAddress = pEmailAddress_,
      contactNotes = Lude.Nothing
    }

-- | The phone number for the contact.
--
-- /Note:/ Consider using 'phoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecPhoneNumber :: Lens.Lens' EmergencyContact (Lude.Maybe Lude.Text)
ecPhoneNumber = Lens.lens (phoneNumber :: EmergencyContact -> Lude.Maybe Lude.Text) (\s a -> s {phoneNumber = a} :: EmergencyContact)
{-# DEPRECATED ecPhoneNumber "Use generic-lens or generic-optics with 'phoneNumber' instead." #-}

-- | The email address for the contact.
--
-- /Note:/ Consider using 'emailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecEmailAddress :: Lens.Lens' EmergencyContact Lude.Text
ecEmailAddress = Lens.lens (emailAddress :: EmergencyContact -> Lude.Text) (\s a -> s {emailAddress = a} :: EmergencyContact)
{-# DEPRECATED ecEmailAddress "Use generic-lens or generic-optics with 'emailAddress' instead." #-}

-- | Additional notes regarding the contact.
--
-- /Note:/ Consider using 'contactNotes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecContactNotes :: Lens.Lens' EmergencyContact (Lude.Maybe Lude.Text)
ecContactNotes = Lens.lens (contactNotes :: EmergencyContact -> Lude.Maybe Lude.Text) (\s a -> s {contactNotes = a} :: EmergencyContact)
{-# DEPRECATED ecContactNotes "Use generic-lens or generic-optics with 'contactNotes' instead." #-}

instance Lude.FromJSON EmergencyContact where
  parseJSON =
    Lude.withObject
      "EmergencyContact"
      ( \x ->
          EmergencyContact'
            Lude.<$> (x Lude..:? "PhoneNumber")
            Lude.<*> (x Lude..: "EmailAddress")
            Lude.<*> (x Lude..:? "ContactNotes")
      )

instance Lude.ToJSON EmergencyContact where
  toJSON EmergencyContact' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PhoneNumber" Lude..=) Lude.<$> phoneNumber,
            Lude.Just ("EmailAddress" Lude..= emailAddress),
            ("ContactNotes" Lude..=) Lude.<$> contactNotes
          ]
      )
