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
-- Module      : Network.AWS.Shield.Types.EmergencyContact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.EmergencyContact where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contact information that the DRT can use to contact you if you have
-- proactive engagement enabled, for escalations to the DRT and to initiate
-- proactive customer support.
--
-- /See:/ 'newEmergencyContact' smart constructor.
data EmergencyContact = EmergencyContact'
  { -- | The phone number for the contact.
    phoneNumber :: Core.Maybe Core.Text,
    -- | Additional notes regarding the contact.
    contactNotes :: Core.Maybe Core.Text,
    -- | The email address for the contact.
    emailAddress :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EmergencyContact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumber', 'emergencyContact_phoneNumber' - The phone number for the contact.
--
-- 'contactNotes', 'emergencyContact_contactNotes' - Additional notes regarding the contact.
--
-- 'emailAddress', 'emergencyContact_emailAddress' - The email address for the contact.
newEmergencyContact ::
  -- | 'emailAddress'
  Core.Text ->
  EmergencyContact
newEmergencyContact pEmailAddress_ =
  EmergencyContact'
    { phoneNumber = Core.Nothing,
      contactNotes = Core.Nothing,
      emailAddress = pEmailAddress_
    }

-- | The phone number for the contact.
emergencyContact_phoneNumber :: Lens.Lens' EmergencyContact (Core.Maybe Core.Text)
emergencyContact_phoneNumber = Lens.lens (\EmergencyContact' {phoneNumber} -> phoneNumber) (\s@EmergencyContact' {} a -> s {phoneNumber = a} :: EmergencyContact)

-- | Additional notes regarding the contact.
emergencyContact_contactNotes :: Lens.Lens' EmergencyContact (Core.Maybe Core.Text)
emergencyContact_contactNotes = Lens.lens (\EmergencyContact' {contactNotes} -> contactNotes) (\s@EmergencyContact' {} a -> s {contactNotes = a} :: EmergencyContact)

-- | The email address for the contact.
emergencyContact_emailAddress :: Lens.Lens' EmergencyContact Core.Text
emergencyContact_emailAddress = Lens.lens (\EmergencyContact' {emailAddress} -> emailAddress) (\s@EmergencyContact' {} a -> s {emailAddress = a} :: EmergencyContact)

instance Core.FromJSON EmergencyContact where
  parseJSON =
    Core.withObject
      "EmergencyContact"
      ( \x ->
          EmergencyContact'
            Core.<$> (x Core..:? "PhoneNumber")
            Core.<*> (x Core..:? "ContactNotes")
            Core.<*> (x Core..: "EmailAddress")
      )

instance Core.Hashable EmergencyContact

instance Core.NFData EmergencyContact

instance Core.ToJSON EmergencyContact where
  toJSON EmergencyContact' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PhoneNumber" Core..=) Core.<$> phoneNumber,
            ("ContactNotes" Core..=) Core.<$> contactNotes,
            Core.Just ("EmailAddress" Core..= emailAddress)
          ]
      )
