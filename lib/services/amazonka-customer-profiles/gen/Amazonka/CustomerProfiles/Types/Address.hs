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
-- Module      : Amazonka.CustomerProfiles.Types.Address
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.Address where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A generic address associated with the customer that is not mailing,
-- shipping, or billing.
--
-- /See:/ 'newAddress' smart constructor.
data Address = Address'
  { -- | The first line of a customer address.
    address1 :: Prelude.Maybe Prelude.Text,
    -- | The second line of a customer address.
    address2 :: Prelude.Maybe Prelude.Text,
    -- | The third line of a customer address.
    address3 :: Prelude.Maybe Prelude.Text,
    -- | The fourth line of a customer address.
    address4 :: Prelude.Maybe Prelude.Text,
    -- | The city in which a customer lives.
    city :: Prelude.Maybe Prelude.Text,
    -- | The country in which a customer lives.
    country :: Prelude.Maybe Prelude.Text,
    -- | The county in which a customer lives.
    county :: Prelude.Maybe Prelude.Text,
    -- | The postal code of a customer address.
    postalCode :: Prelude.Maybe Prelude.Text,
    -- | The province in which a customer lives.
    province :: Prelude.Maybe Prelude.Text,
    -- | The state in which a customer lives.
    state :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Address' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'address1', 'address_address1' - The first line of a customer address.
--
-- 'address2', 'address_address2' - The second line of a customer address.
--
-- 'address3', 'address_address3' - The third line of a customer address.
--
-- 'address4', 'address_address4' - The fourth line of a customer address.
--
-- 'city', 'address_city' - The city in which a customer lives.
--
-- 'country', 'address_country' - The country in which a customer lives.
--
-- 'county', 'address_county' - The county in which a customer lives.
--
-- 'postalCode', 'address_postalCode' - The postal code of a customer address.
--
-- 'province', 'address_province' - The province in which a customer lives.
--
-- 'state', 'address_state' - The state in which a customer lives.
newAddress ::
  Address
newAddress =
  Address'
    { address1 = Prelude.Nothing,
      address2 = Prelude.Nothing,
      address3 = Prelude.Nothing,
      address4 = Prelude.Nothing,
      city = Prelude.Nothing,
      country = Prelude.Nothing,
      county = Prelude.Nothing,
      postalCode = Prelude.Nothing,
      province = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The first line of a customer address.
address_address1 :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_address1 = Lens.lens (\Address' {address1} -> address1) (\s@Address' {} a -> s {address1 = a} :: Address)

-- | The second line of a customer address.
address_address2 :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_address2 = Lens.lens (\Address' {address2} -> address2) (\s@Address' {} a -> s {address2 = a} :: Address)

-- | The third line of a customer address.
address_address3 :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_address3 = Lens.lens (\Address' {address3} -> address3) (\s@Address' {} a -> s {address3 = a} :: Address)

-- | The fourth line of a customer address.
address_address4 :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_address4 = Lens.lens (\Address' {address4} -> address4) (\s@Address' {} a -> s {address4 = a} :: Address)

-- | The city in which a customer lives.
address_city :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_city = Lens.lens (\Address' {city} -> city) (\s@Address' {} a -> s {city = a} :: Address)

-- | The country in which a customer lives.
address_country :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_country = Lens.lens (\Address' {country} -> country) (\s@Address' {} a -> s {country = a} :: Address)

-- | The county in which a customer lives.
address_county :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_county = Lens.lens (\Address' {county} -> county) (\s@Address' {} a -> s {county = a} :: Address)

-- | The postal code of a customer address.
address_postalCode :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_postalCode = Lens.lens (\Address' {postalCode} -> postalCode) (\s@Address' {} a -> s {postalCode = a} :: Address)

-- | The province in which a customer lives.
address_province :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_province = Lens.lens (\Address' {province} -> province) (\s@Address' {} a -> s {province = a} :: Address)

-- | The state in which a customer lives.
address_state :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_state = Lens.lens (\Address' {state} -> state) (\s@Address' {} a -> s {state = a} :: Address)

instance Data.FromJSON Address where
  parseJSON =
    Data.withObject
      "Address"
      ( \x ->
          Address'
            Prelude.<$> (x Data..:? "Address1")
            Prelude.<*> (x Data..:? "Address2")
            Prelude.<*> (x Data..:? "Address3")
            Prelude.<*> (x Data..:? "Address4")
            Prelude.<*> (x Data..:? "City")
            Prelude.<*> (x Data..:? "Country")
            Prelude.<*> (x Data..:? "County")
            Prelude.<*> (x Data..:? "PostalCode")
            Prelude.<*> (x Data..:? "Province")
            Prelude.<*> (x Data..:? "State")
      )

instance Prelude.Hashable Address where
  hashWithSalt _salt Address' {..} =
    _salt
      `Prelude.hashWithSalt` address1
      `Prelude.hashWithSalt` address2
      `Prelude.hashWithSalt` address3
      `Prelude.hashWithSalt` address4
      `Prelude.hashWithSalt` city
      `Prelude.hashWithSalt` country
      `Prelude.hashWithSalt` county
      `Prelude.hashWithSalt` postalCode
      `Prelude.hashWithSalt` province
      `Prelude.hashWithSalt` state

instance Prelude.NFData Address where
  rnf Address' {..} =
    Prelude.rnf address1 `Prelude.seq`
      Prelude.rnf address2 `Prelude.seq`
        Prelude.rnf address3 `Prelude.seq`
          Prelude.rnf address4 `Prelude.seq`
            Prelude.rnf city `Prelude.seq`
              Prelude.rnf country `Prelude.seq`
                Prelude.rnf county `Prelude.seq`
                  Prelude.rnf postalCode `Prelude.seq`
                    Prelude.rnf province `Prelude.seq`
                      Prelude.rnf state

instance Data.ToJSON Address where
  toJSON Address' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Address1" Data..=) Prelude.<$> address1,
            ("Address2" Data..=) Prelude.<$> address2,
            ("Address3" Data..=) Prelude.<$> address3,
            ("Address4" Data..=) Prelude.<$> address4,
            ("City" Data..=) Prelude.<$> city,
            ("Country" Data..=) Prelude.<$> country,
            ("County" Data..=) Prelude.<$> county,
            ("PostalCode" Data..=) Prelude.<$> postalCode,
            ("Province" Data..=) Prelude.<$> province,
            ("State" Data..=) Prelude.<$> state
          ]
      )
