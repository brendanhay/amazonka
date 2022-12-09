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
-- Module      : Amazonka.Chime.Types.Address
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.Address where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A validated address.
--
-- /See:/ 'newAddress' smart constructor.
data Address = Address'
  { -- | The city of an address.
    city :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The country of an address.
    country :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | An address suffix location, such as the @S. Unit A@ in
    -- @Central Park S. Unit A@.
    postDirectional :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The postal code of an address.
    postalCode :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The Zip + 4 or postal code + 4 of an address.
    postalCodePlus4 :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | An address prefix location, such as the @N@ in @N. Third St.@.
    preDirectional :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The state of an address.
    state :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The address street, such as @8th Avenue@.
    streetName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The numeric portion of an address.
    streetNumber :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The address suffix, such as the @N@ in @8th Avenue N@.
    streetSuffix :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Address' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'city', 'address_city' - The city of an address.
--
-- 'country', 'address_country' - The country of an address.
--
-- 'postDirectional', 'address_postDirectional' - An address suffix location, such as the @S. Unit A@ in
-- @Central Park S. Unit A@.
--
-- 'postalCode', 'address_postalCode' - The postal code of an address.
--
-- 'postalCodePlus4', 'address_postalCodePlus4' - The Zip + 4 or postal code + 4 of an address.
--
-- 'preDirectional', 'address_preDirectional' - An address prefix location, such as the @N@ in @N. Third St.@.
--
-- 'state', 'address_state' - The state of an address.
--
-- 'streetName', 'address_streetName' - The address street, such as @8th Avenue@.
--
-- 'streetNumber', 'address_streetNumber' - The numeric portion of an address.
--
-- 'streetSuffix', 'address_streetSuffix' - The address suffix, such as the @N@ in @8th Avenue N@.
newAddress ::
  Address
newAddress =
  Address'
    { city = Prelude.Nothing,
      country = Prelude.Nothing,
      postDirectional = Prelude.Nothing,
      postalCode = Prelude.Nothing,
      postalCodePlus4 = Prelude.Nothing,
      preDirectional = Prelude.Nothing,
      state = Prelude.Nothing,
      streetName = Prelude.Nothing,
      streetNumber = Prelude.Nothing,
      streetSuffix = Prelude.Nothing
    }

-- | The city of an address.
address_city :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_city = Lens.lens (\Address' {city} -> city) (\s@Address' {} a -> s {city = a} :: Address) Prelude.. Lens.mapping Data._Sensitive

-- | The country of an address.
address_country :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_country = Lens.lens (\Address' {country} -> country) (\s@Address' {} a -> s {country = a} :: Address) Prelude.. Lens.mapping Data._Sensitive

-- | An address suffix location, such as the @S. Unit A@ in
-- @Central Park S. Unit A@.
address_postDirectional :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_postDirectional = Lens.lens (\Address' {postDirectional} -> postDirectional) (\s@Address' {} a -> s {postDirectional = a} :: Address) Prelude.. Lens.mapping Data._Sensitive

-- | The postal code of an address.
address_postalCode :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_postalCode = Lens.lens (\Address' {postalCode} -> postalCode) (\s@Address' {} a -> s {postalCode = a} :: Address) Prelude.. Lens.mapping Data._Sensitive

-- | The Zip + 4 or postal code + 4 of an address.
address_postalCodePlus4 :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_postalCodePlus4 = Lens.lens (\Address' {postalCodePlus4} -> postalCodePlus4) (\s@Address' {} a -> s {postalCodePlus4 = a} :: Address) Prelude.. Lens.mapping Data._Sensitive

-- | An address prefix location, such as the @N@ in @N. Third St.@.
address_preDirectional :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_preDirectional = Lens.lens (\Address' {preDirectional} -> preDirectional) (\s@Address' {} a -> s {preDirectional = a} :: Address) Prelude.. Lens.mapping Data._Sensitive

-- | The state of an address.
address_state :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_state = Lens.lens (\Address' {state} -> state) (\s@Address' {} a -> s {state = a} :: Address) Prelude.. Lens.mapping Data._Sensitive

-- | The address street, such as @8th Avenue@.
address_streetName :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_streetName = Lens.lens (\Address' {streetName} -> streetName) (\s@Address' {} a -> s {streetName = a} :: Address) Prelude.. Lens.mapping Data._Sensitive

-- | The numeric portion of an address.
address_streetNumber :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_streetNumber = Lens.lens (\Address' {streetNumber} -> streetNumber) (\s@Address' {} a -> s {streetNumber = a} :: Address) Prelude.. Lens.mapping Data._Sensitive

-- | The address suffix, such as the @N@ in @8th Avenue N@.
address_streetSuffix :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_streetSuffix = Lens.lens (\Address' {streetSuffix} -> streetSuffix) (\s@Address' {} a -> s {streetSuffix = a} :: Address) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON Address where
  parseJSON =
    Data.withObject
      "Address"
      ( \x ->
          Address'
            Prelude.<$> (x Data..:? "city")
            Prelude.<*> (x Data..:? "country")
            Prelude.<*> (x Data..:? "postDirectional")
            Prelude.<*> (x Data..:? "postalCode")
            Prelude.<*> (x Data..:? "postalCodePlus4")
            Prelude.<*> (x Data..:? "preDirectional")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "streetName")
            Prelude.<*> (x Data..:? "streetNumber")
            Prelude.<*> (x Data..:? "streetSuffix")
      )

instance Prelude.Hashable Address where
  hashWithSalt _salt Address' {..} =
    _salt `Prelude.hashWithSalt` city
      `Prelude.hashWithSalt` country
      `Prelude.hashWithSalt` postDirectional
      `Prelude.hashWithSalt` postalCode
      `Prelude.hashWithSalt` postalCodePlus4
      `Prelude.hashWithSalt` preDirectional
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` streetName
      `Prelude.hashWithSalt` streetNumber
      `Prelude.hashWithSalt` streetSuffix

instance Prelude.NFData Address where
  rnf Address' {..} =
    Prelude.rnf city
      `Prelude.seq` Prelude.rnf country
      `Prelude.seq` Prelude.rnf postDirectional
      `Prelude.seq` Prelude.rnf postalCode
      `Prelude.seq` Prelude.rnf postalCodePlus4
      `Prelude.seq` Prelude.rnf preDirectional
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf streetName
      `Prelude.seq` Prelude.rnf streetNumber
      `Prelude.seq` Prelude.rnf streetSuffix
