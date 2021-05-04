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
-- Module      : Network.AWS.Shield.Types.EmergencyContact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.EmergencyContact where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contact information that the DRT can use to contact you if you have
-- proactive engagement enabled, for escalations to the DRT and to initiate
-- proactive customer support.
--
-- /See:/ 'newEmergencyContact' smart constructor.
data EmergencyContact = EmergencyContact'
  { -- | The phone number for the contact.
    phoneNumber :: Prelude.Maybe Prelude.Text,
    -- | Additional notes regarding the contact.
    contactNotes :: Prelude.Maybe Prelude.Text,
    -- | The email address for the contact.
    emailAddress :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  EmergencyContact
newEmergencyContact pEmailAddress_ =
  EmergencyContact'
    { phoneNumber = Prelude.Nothing,
      contactNotes = Prelude.Nothing,
      emailAddress = pEmailAddress_
    }

-- | The phone number for the contact.
emergencyContact_phoneNumber :: Lens.Lens' EmergencyContact (Prelude.Maybe Prelude.Text)
emergencyContact_phoneNumber = Lens.lens (\EmergencyContact' {phoneNumber} -> phoneNumber) (\s@EmergencyContact' {} a -> s {phoneNumber = a} :: EmergencyContact)

-- | Additional notes regarding the contact.
emergencyContact_contactNotes :: Lens.Lens' EmergencyContact (Prelude.Maybe Prelude.Text)
emergencyContact_contactNotes = Lens.lens (\EmergencyContact' {contactNotes} -> contactNotes) (\s@EmergencyContact' {} a -> s {contactNotes = a} :: EmergencyContact)

-- | The email address for the contact.
emergencyContact_emailAddress :: Lens.Lens' EmergencyContact Prelude.Text
emergencyContact_emailAddress = Lens.lens (\EmergencyContact' {emailAddress} -> emailAddress) (\s@EmergencyContact' {} a -> s {emailAddress = a} :: EmergencyContact)

instance Prelude.FromJSON EmergencyContact where
  parseJSON =
    Prelude.withObject
      "EmergencyContact"
      ( \x ->
          EmergencyContact'
            Prelude.<$> (x Prelude..:? "PhoneNumber")
            Prelude.<*> (x Prelude..:? "ContactNotes")
            Prelude.<*> (x Prelude..: "EmailAddress")
      )

instance Prelude.Hashable EmergencyContact

instance Prelude.NFData EmergencyContact

instance Prelude.ToJSON EmergencyContact where
  toJSON EmergencyContact' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("PhoneNumber" Prelude..=) Prelude.<$> phoneNumber,
            ("ContactNotes" Prelude..=) Prelude.<$> contactNotes,
            Prelude.Just
              ("EmailAddress" Prelude..= emailAddress)
          ]
      )
