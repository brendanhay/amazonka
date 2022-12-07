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
-- Module      : Amazonka.IdentityStore.Types.PhoneNumber
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IdentityStore.Types.PhoneNumber where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The phone number associated with the user.
--
-- /See:/ 'newPhoneNumber' smart constructor.
data PhoneNumber = PhoneNumber'
  { -- | A string representing the type of a phone number. For example,
    -- \"Mobile.\"
    type' :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A Boolean value representing whether this is the primary phone number
    -- for the associated resource.
    primary :: Prelude.Maybe (Data.Sensitive Prelude.Bool),
    -- | A string containing a phone number. For example, \"8675309\" or \"+1
    -- (800) 123-4567\".
    value :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PhoneNumber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'phoneNumber_type' - A string representing the type of a phone number. For example,
-- \"Mobile.\"
--
-- 'primary', 'phoneNumber_primary' - A Boolean value representing whether this is the primary phone number
-- for the associated resource.
--
-- 'value', 'phoneNumber_value' - A string containing a phone number. For example, \"8675309\" or \"+1
-- (800) 123-4567\".
newPhoneNumber ::
  PhoneNumber
newPhoneNumber =
  PhoneNumber'
    { type' = Prelude.Nothing,
      primary = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | A string representing the type of a phone number. For example,
-- \"Mobile.\"
phoneNumber_type :: Lens.Lens' PhoneNumber (Prelude.Maybe Prelude.Text)
phoneNumber_type = Lens.lens (\PhoneNumber' {type'} -> type') (\s@PhoneNumber' {} a -> s {type' = a} :: PhoneNumber) Prelude.. Lens.mapping Data._Sensitive

-- | A Boolean value representing whether this is the primary phone number
-- for the associated resource.
phoneNumber_primary :: Lens.Lens' PhoneNumber (Prelude.Maybe Prelude.Bool)
phoneNumber_primary = Lens.lens (\PhoneNumber' {primary} -> primary) (\s@PhoneNumber' {} a -> s {primary = a} :: PhoneNumber) Prelude.. Lens.mapping Data._Sensitive

-- | A string containing a phone number. For example, \"8675309\" or \"+1
-- (800) 123-4567\".
phoneNumber_value :: Lens.Lens' PhoneNumber (Prelude.Maybe Prelude.Text)
phoneNumber_value = Lens.lens (\PhoneNumber' {value} -> value) (\s@PhoneNumber' {} a -> s {value = a} :: PhoneNumber) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON PhoneNumber where
  parseJSON =
    Data.withObject
      "PhoneNumber"
      ( \x ->
          PhoneNumber'
            Prelude.<$> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "Primary")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable PhoneNumber where
  hashWithSalt _salt PhoneNumber' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` primary
      `Prelude.hashWithSalt` value

instance Prelude.NFData PhoneNumber where
  rnf PhoneNumber' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf primary
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON PhoneNumber where
  toJSON PhoneNumber' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Type" Data..=) Prelude.<$> type',
            ("Primary" Data..=) Prelude.<$> primary,
            ("Value" Data..=) Prelude.<$> value
          ]
      )
