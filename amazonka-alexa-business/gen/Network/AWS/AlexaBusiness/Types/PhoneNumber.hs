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
-- Module      : Network.AWS.AlexaBusiness.Types.PhoneNumber
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.PhoneNumber where

import Network.AWS.AlexaBusiness.Types.PhoneNumberType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The phone number for the contact containing the raw number and phone
-- number type.
--
-- /See:/ 'newPhoneNumber' smart constructor.
data PhoneNumber = PhoneNumber'
  { -- | The raw value of the phone number.
    number :: Core.Sensitive Core.Text,
    -- | The type of the phone number.
    type' :: Core.Sensitive PhoneNumberType
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Text ->
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
phoneNumber_number :: Lens.Lens' PhoneNumber Core.Text
phoneNumber_number = Lens.lens (\PhoneNumber' {number} -> number) (\s@PhoneNumber' {} a -> s {number = a} :: PhoneNumber) Core.. Core._Sensitive

-- | The type of the phone number.
phoneNumber_type :: Lens.Lens' PhoneNumber PhoneNumberType
phoneNumber_type = Lens.lens (\PhoneNumber' {type'} -> type') (\s@PhoneNumber' {} a -> s {type' = a} :: PhoneNumber) Core.. Core._Sensitive

instance Core.FromJSON PhoneNumber where
  parseJSON =
    Core.withObject
      "PhoneNumber"
      ( \x ->
          PhoneNumber'
            Core.<$> (x Core..: "Number") Core.<*> (x Core..: "Type")
      )

instance Core.Hashable PhoneNumber

instance Core.NFData PhoneNumber

instance Core.ToJSON PhoneNumber where
  toJSON PhoneNumber' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Number" Core..= number),
            Core.Just ("Type" Core..= type')
          ]
      )
