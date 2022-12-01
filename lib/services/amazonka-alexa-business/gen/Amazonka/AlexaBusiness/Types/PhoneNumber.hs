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
-- Module      : Amazonka.AlexaBusiness.Types.PhoneNumber
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.PhoneNumber where

import Amazonka.AlexaBusiness.Types.PhoneNumberType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The phone number for the contact containing the raw number and phone
-- number type.
--
-- /See:/ 'newPhoneNumber' smart constructor.
data PhoneNumber = PhoneNumber'
  { -- | The raw value of the phone number.
    number :: Core.Sensitive Prelude.Text,
    -- | The type of the phone number.
    type' :: Core.Sensitive PhoneNumberType
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
-- 'number', 'phoneNumber_number' - The raw value of the phone number.
--
-- 'type'', 'phoneNumber_type' - The type of the phone number.
newPhoneNumber ::
  -- | 'number'
  Prelude.Text ->
  -- | 'type''
  PhoneNumberType ->
  PhoneNumber
newPhoneNumber pNumber_ pType_ =
  PhoneNumber'
    { number =
        Core._Sensitive Lens.# pNumber_,
      type' = Core._Sensitive Lens.# pType_
    }

-- | The raw value of the phone number.
phoneNumber_number :: Lens.Lens' PhoneNumber Prelude.Text
phoneNumber_number = Lens.lens (\PhoneNumber' {number} -> number) (\s@PhoneNumber' {} a -> s {number = a} :: PhoneNumber) Prelude.. Core._Sensitive

-- | The type of the phone number.
phoneNumber_type :: Lens.Lens' PhoneNumber PhoneNumberType
phoneNumber_type = Lens.lens (\PhoneNumber' {type'} -> type') (\s@PhoneNumber' {} a -> s {type' = a} :: PhoneNumber) Prelude.. Core._Sensitive

instance Core.FromJSON PhoneNumber where
  parseJSON =
    Core.withObject
      "PhoneNumber"
      ( \x ->
          PhoneNumber'
            Prelude.<$> (x Core..: "Number") Prelude.<*> (x Core..: "Type")
      )

instance Prelude.Hashable PhoneNumber where
  hashWithSalt _salt PhoneNumber' {..} =
    _salt `Prelude.hashWithSalt` number
      `Prelude.hashWithSalt` type'

instance Prelude.NFData PhoneNumber where
  rnf PhoneNumber' {..} =
    Prelude.rnf number `Prelude.seq` Prelude.rnf type'

instance Core.ToJSON PhoneNumber where
  toJSON PhoneNumber' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Number" Core..= number),
            Prelude.Just ("Type" Core..= type')
          ]
      )
