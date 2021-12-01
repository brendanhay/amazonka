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
-- Module      : Amazonka.Chime.Types.PhoneNumberAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.PhoneNumberAssociation where

import Amazonka.Chime.Types.PhoneNumberAssociationName
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The phone number associations, such as Amazon Chime account ID, Amazon
-- Chime user ID, Amazon Chime Voice Connector ID, or Amazon Chime Voice
-- Connector group ID.
--
-- /See:/ 'newPhoneNumberAssociation' smart constructor.
data PhoneNumberAssociation = PhoneNumberAssociation'
  { -- | Contains the ID for the entity specified in Name.
    value :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of the phone number association, in ISO 8601 format.
    associatedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | Defines the association with an Amazon Chime account ID, user ID, Amazon
    -- Chime Voice Connector ID, or Amazon Chime Voice Connector group ID.
    name :: Prelude.Maybe PhoneNumberAssociationName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PhoneNumberAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'phoneNumberAssociation_value' - Contains the ID for the entity specified in Name.
--
-- 'associatedTimestamp', 'phoneNumberAssociation_associatedTimestamp' - The timestamp of the phone number association, in ISO 8601 format.
--
-- 'name', 'phoneNumberAssociation_name' - Defines the association with an Amazon Chime account ID, user ID, Amazon
-- Chime Voice Connector ID, or Amazon Chime Voice Connector group ID.
newPhoneNumberAssociation ::
  PhoneNumberAssociation
newPhoneNumberAssociation =
  PhoneNumberAssociation'
    { value = Prelude.Nothing,
      associatedTimestamp = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | Contains the ID for the entity specified in Name.
phoneNumberAssociation_value :: Lens.Lens' PhoneNumberAssociation (Prelude.Maybe Prelude.Text)
phoneNumberAssociation_value = Lens.lens (\PhoneNumberAssociation' {value} -> value) (\s@PhoneNumberAssociation' {} a -> s {value = a} :: PhoneNumberAssociation)

-- | The timestamp of the phone number association, in ISO 8601 format.
phoneNumberAssociation_associatedTimestamp :: Lens.Lens' PhoneNumberAssociation (Prelude.Maybe Prelude.UTCTime)
phoneNumberAssociation_associatedTimestamp = Lens.lens (\PhoneNumberAssociation' {associatedTimestamp} -> associatedTimestamp) (\s@PhoneNumberAssociation' {} a -> s {associatedTimestamp = a} :: PhoneNumberAssociation) Prelude.. Lens.mapping Core._Time

-- | Defines the association with an Amazon Chime account ID, user ID, Amazon
-- Chime Voice Connector ID, or Amazon Chime Voice Connector group ID.
phoneNumberAssociation_name :: Lens.Lens' PhoneNumberAssociation (Prelude.Maybe PhoneNumberAssociationName)
phoneNumberAssociation_name = Lens.lens (\PhoneNumberAssociation' {name} -> name) (\s@PhoneNumberAssociation' {} a -> s {name = a} :: PhoneNumberAssociation)

instance Core.FromJSON PhoneNumberAssociation where
  parseJSON =
    Core.withObject
      "PhoneNumberAssociation"
      ( \x ->
          PhoneNumberAssociation'
            Prelude.<$> (x Core..:? "Value")
            Prelude.<*> (x Core..:? "AssociatedTimestamp")
            Prelude.<*> (x Core..:? "Name")
      )

instance Prelude.Hashable PhoneNumberAssociation where
  hashWithSalt salt' PhoneNumberAssociation' {..} =
    salt' `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` associatedTimestamp
      `Prelude.hashWithSalt` value

instance Prelude.NFData PhoneNumberAssociation where
  rnf PhoneNumberAssociation' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf associatedTimestamp
