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
-- Module      : Network.AWS.CustomerProfiles.Types.Address
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CustomerProfiles.Types.Address where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A generic address associated with the customer that is not mailing,
-- shipping, or billing.
--
-- /See:/ 'newAddress' smart constructor.
data Address = Address'
  { -- | The second line of a customer address.
    address2 :: Prelude.Maybe Prelude.Text,
    -- | The state in which a customer lives.
    state :: Prelude.Maybe Prelude.Text,
    -- | The fourth line of a customer address.
    address4 :: Prelude.Maybe Prelude.Text,
    -- | The county in which a customer lives.
    county :: Prelude.Maybe Prelude.Text,
    -- | The third line of a customer address.
    address3 :: Prelude.Maybe Prelude.Text,
    -- | The postal code of a customer address.
    postalCode :: Prelude.Maybe Prelude.Text,
    -- | The country in which a customer lives.
    country :: Prelude.Maybe Prelude.Text,
    -- | The city in which a customer lives.
    city :: Prelude.Maybe Prelude.Text,
    -- | The first line of a customer address.
    address1 :: Prelude.Maybe Prelude.Text,
    -- | The province in which a customer lives.
    province :: Prelude.Maybe Prelude.Text
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
-- 'address2', 'address_address2' - The second line of a customer address.
--
-- 'state', 'address_state' - The state in which a customer lives.
--
-- 'address4', 'address_address4' - The fourth line of a customer address.
--
-- 'county', 'address_county' - The county in which a customer lives.
--
-- 'address3', 'address_address3' - The third line of a customer address.
--
-- 'postalCode', 'address_postalCode' - The postal code of a customer address.
--
-- 'country', 'address_country' - The country in which a customer lives.
--
-- 'city', 'address_city' - The city in which a customer lives.
--
-- 'address1', 'address_address1' - The first line of a customer address.
--
-- 'province', 'address_province' - The province in which a customer lives.
newAddress ::
  Address
newAddress =
  Address'
    { address2 = Prelude.Nothing,
      state = Prelude.Nothing,
      address4 = Prelude.Nothing,
      county = Prelude.Nothing,
      address3 = Prelude.Nothing,
      postalCode = Prelude.Nothing,
      country = Prelude.Nothing,
      city = Prelude.Nothing,
      address1 = Prelude.Nothing,
      province = Prelude.Nothing
    }

-- | The second line of a customer address.
address_address2 :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_address2 = Lens.lens (\Address' {address2} -> address2) (\s@Address' {} a -> s {address2 = a} :: Address)

-- | The state in which a customer lives.
address_state :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_state = Lens.lens (\Address' {state} -> state) (\s@Address' {} a -> s {state = a} :: Address)

-- | The fourth line of a customer address.
address_address4 :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_address4 = Lens.lens (\Address' {address4} -> address4) (\s@Address' {} a -> s {address4 = a} :: Address)

-- | The county in which a customer lives.
address_county :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_county = Lens.lens (\Address' {county} -> county) (\s@Address' {} a -> s {county = a} :: Address)

-- | The third line of a customer address.
address_address3 :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_address3 = Lens.lens (\Address' {address3} -> address3) (\s@Address' {} a -> s {address3 = a} :: Address)

-- | The postal code of a customer address.
address_postalCode :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_postalCode = Lens.lens (\Address' {postalCode} -> postalCode) (\s@Address' {} a -> s {postalCode = a} :: Address)

-- | The country in which a customer lives.
address_country :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_country = Lens.lens (\Address' {country} -> country) (\s@Address' {} a -> s {country = a} :: Address)

-- | The city in which a customer lives.
address_city :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_city = Lens.lens (\Address' {city} -> city) (\s@Address' {} a -> s {city = a} :: Address)

-- | The first line of a customer address.
address_address1 :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_address1 = Lens.lens (\Address' {address1} -> address1) (\s@Address' {} a -> s {address1 = a} :: Address)

-- | The province in which a customer lives.
address_province :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_province = Lens.lens (\Address' {province} -> province) (\s@Address' {} a -> s {province = a} :: Address)

instance Core.FromJSON Address where
  parseJSON =
    Core.withObject
      "Address"
      ( \x ->
          Address'
            Prelude.<$> (x Core..:? "Address2")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "Address4")
            Prelude.<*> (x Core..:? "County")
            Prelude.<*> (x Core..:? "Address3")
            Prelude.<*> (x Core..:? "PostalCode")
            Prelude.<*> (x Core..:? "Country")
            Prelude.<*> (x Core..:? "City")
            Prelude.<*> (x Core..:? "Address1")
            Prelude.<*> (x Core..:? "Province")
      )

instance Prelude.Hashable Address

instance Prelude.NFData Address

instance Core.ToJSON Address where
  toJSON Address' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Address2" Core..=) Prelude.<$> address2,
            ("State" Core..=) Prelude.<$> state,
            ("Address4" Core..=) Prelude.<$> address4,
            ("County" Core..=) Prelude.<$> county,
            ("Address3" Core..=) Prelude.<$> address3,
            ("PostalCode" Core..=) Prelude.<$> postalCode,
            ("Country" Core..=) Prelude.<$> country,
            ("City" Core..=) Prelude.<$> city,
            ("Address1" Core..=) Prelude.<$> address1,
            ("Province" Core..=) Prelude.<$> province
          ]
      )
