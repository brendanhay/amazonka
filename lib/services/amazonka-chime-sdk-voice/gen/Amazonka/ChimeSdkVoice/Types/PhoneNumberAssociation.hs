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
-- Module      : Amazonka.ChimeSdkVoice.Types.PhoneNumberAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.PhoneNumberAssociation where

import Amazonka.ChimeSdkVoice.Types.PhoneNumberAssociationName
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newPhoneNumberAssociation' smart constructor.
data PhoneNumberAssociation = PhoneNumberAssociation'
  { associatedTimestamp :: Prelude.Maybe Data.ISO8601,
    name :: Prelude.Maybe PhoneNumberAssociationName,
    value :: Prelude.Maybe Prelude.Text
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
-- 'associatedTimestamp', 'phoneNumberAssociation_associatedTimestamp' - Undocumented member.
--
-- 'name', 'phoneNumberAssociation_name' - Undocumented member.
--
-- 'value', 'phoneNumberAssociation_value' - Undocumented member.
newPhoneNumberAssociation ::
  PhoneNumberAssociation
newPhoneNumberAssociation =
  PhoneNumberAssociation'
    { associatedTimestamp =
        Prelude.Nothing,
      name = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Undocumented member.
phoneNumberAssociation_associatedTimestamp :: Lens.Lens' PhoneNumberAssociation (Prelude.Maybe Prelude.UTCTime)
phoneNumberAssociation_associatedTimestamp = Lens.lens (\PhoneNumberAssociation' {associatedTimestamp} -> associatedTimestamp) (\s@PhoneNumberAssociation' {} a -> s {associatedTimestamp = a} :: PhoneNumberAssociation) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
phoneNumberAssociation_name :: Lens.Lens' PhoneNumberAssociation (Prelude.Maybe PhoneNumberAssociationName)
phoneNumberAssociation_name = Lens.lens (\PhoneNumberAssociation' {name} -> name) (\s@PhoneNumberAssociation' {} a -> s {name = a} :: PhoneNumberAssociation)

-- | Undocumented member.
phoneNumberAssociation_value :: Lens.Lens' PhoneNumberAssociation (Prelude.Maybe Prelude.Text)
phoneNumberAssociation_value = Lens.lens (\PhoneNumberAssociation' {value} -> value) (\s@PhoneNumberAssociation' {} a -> s {value = a} :: PhoneNumberAssociation)

instance Data.FromJSON PhoneNumberAssociation where
  parseJSON =
    Data.withObject
      "PhoneNumberAssociation"
      ( \x ->
          PhoneNumberAssociation'
            Prelude.<$> (x Data..:? "AssociatedTimestamp")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable PhoneNumberAssociation where
  hashWithSalt _salt PhoneNumberAssociation' {..} =
    _salt
      `Prelude.hashWithSalt` associatedTimestamp
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData PhoneNumberAssociation where
  rnf PhoneNumberAssociation' {..} =
    Prelude.rnf associatedTimestamp
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf value
