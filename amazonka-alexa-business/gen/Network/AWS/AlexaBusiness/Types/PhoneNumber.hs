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
-- Module      : Network.AWS.AlexaBusiness.Types.PhoneNumber
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.PhoneNumber where

import Network.AWS.AlexaBusiness.Types.PhoneNumberType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The phone number for the contact containing the raw number and phone
-- number type.
--
-- /See:/ 'newPhoneNumber' smart constructor.
data PhoneNumber = PhoneNumber'
  { -- | The raw value of the phone number.
    number :: Prelude.Sensitive Prelude.Text,
    -- | The type of the phone number.
    type' :: Prelude.Sensitive PhoneNumberType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude._Sensitive Lens.# pNumber_,
      type' = Prelude._Sensitive Lens.# pType_
    }

-- | The raw value of the phone number.
phoneNumber_number :: Lens.Lens' PhoneNumber Prelude.Text
phoneNumber_number = Lens.lens (\PhoneNumber' {number} -> number) (\s@PhoneNumber' {} a -> s {number = a} :: PhoneNumber) Prelude.. Prelude._Sensitive

-- | The type of the phone number.
phoneNumber_type :: Lens.Lens' PhoneNumber PhoneNumberType
phoneNumber_type = Lens.lens (\PhoneNumber' {type'} -> type') (\s@PhoneNumber' {} a -> s {type' = a} :: PhoneNumber) Prelude.. Prelude._Sensitive

instance Prelude.FromJSON PhoneNumber where
  parseJSON =
    Prelude.withObject
      "PhoneNumber"
      ( \x ->
          PhoneNumber'
            Prelude.<$> (x Prelude..: "Number")
            Prelude.<*> (x Prelude..: "Type")
      )

instance Prelude.Hashable PhoneNumber

instance Prelude.NFData PhoneNumber

instance Prelude.ToJSON PhoneNumber where
  toJSON PhoneNumber' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Number" Prelude..= number),
            Prelude.Just ("Type" Prelude..= type')
          ]
      )
