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
-- Module      : Amazonka.IdentityStore.Types.Address
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IdentityStore.Types.Address where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The address associated with the specified user.
--
-- /See:/ 'newAddress' smart constructor.
data Address = Address'
  { -- | The country of the address.
    country :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A string containing a formatted version of the address for display.
    formatted :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A string of the address locality.
    locality :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The postal code of the address.
    postalCode :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A Boolean value representing whether this is the primary address for the
    -- associated resource.
    primary :: Prelude.Maybe (Data.Sensitive Prelude.Bool),
    -- | The region of the address.
    region :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The street of the address.
    streetAddress :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A string representing the type of address. For example, \"Home.\"
    type' :: Prelude.Maybe (Data.Sensitive Prelude.Text)
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
-- 'country', 'address_country' - The country of the address.
--
-- 'formatted', 'address_formatted' - A string containing a formatted version of the address for display.
--
-- 'locality', 'address_locality' - A string of the address locality.
--
-- 'postalCode', 'address_postalCode' - The postal code of the address.
--
-- 'primary', 'address_primary' - A Boolean value representing whether this is the primary address for the
-- associated resource.
--
-- 'region', 'address_region' - The region of the address.
--
-- 'streetAddress', 'address_streetAddress' - The street of the address.
--
-- 'type'', 'address_type' - A string representing the type of address. For example, \"Home.\"
newAddress ::
  Address
newAddress =
  Address'
    { country = Prelude.Nothing,
      formatted = Prelude.Nothing,
      locality = Prelude.Nothing,
      postalCode = Prelude.Nothing,
      primary = Prelude.Nothing,
      region = Prelude.Nothing,
      streetAddress = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The country of the address.
address_country :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_country = Lens.lens (\Address' {country} -> country) (\s@Address' {} a -> s {country = a} :: Address) Prelude.. Lens.mapping Data._Sensitive

-- | A string containing a formatted version of the address for display.
address_formatted :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_formatted = Lens.lens (\Address' {formatted} -> formatted) (\s@Address' {} a -> s {formatted = a} :: Address) Prelude.. Lens.mapping Data._Sensitive

-- | A string of the address locality.
address_locality :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_locality = Lens.lens (\Address' {locality} -> locality) (\s@Address' {} a -> s {locality = a} :: Address) Prelude.. Lens.mapping Data._Sensitive

-- | The postal code of the address.
address_postalCode :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_postalCode = Lens.lens (\Address' {postalCode} -> postalCode) (\s@Address' {} a -> s {postalCode = a} :: Address) Prelude.. Lens.mapping Data._Sensitive

-- | A Boolean value representing whether this is the primary address for the
-- associated resource.
address_primary :: Lens.Lens' Address (Prelude.Maybe Prelude.Bool)
address_primary = Lens.lens (\Address' {primary} -> primary) (\s@Address' {} a -> s {primary = a} :: Address) Prelude.. Lens.mapping Data._Sensitive

-- | The region of the address.
address_region :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_region = Lens.lens (\Address' {region} -> region) (\s@Address' {} a -> s {region = a} :: Address) Prelude.. Lens.mapping Data._Sensitive

-- | The street of the address.
address_streetAddress :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_streetAddress = Lens.lens (\Address' {streetAddress} -> streetAddress) (\s@Address' {} a -> s {streetAddress = a} :: Address) Prelude.. Lens.mapping Data._Sensitive

-- | A string representing the type of address. For example, \"Home.\"
address_type :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_type = Lens.lens (\Address' {type'} -> type') (\s@Address' {} a -> s {type' = a} :: Address) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON Address where
  parseJSON =
    Data.withObject
      "Address"
      ( \x ->
          Address'
            Prelude.<$> (x Data..:? "Country")
            Prelude.<*> (x Data..:? "Formatted")
            Prelude.<*> (x Data..:? "Locality")
            Prelude.<*> (x Data..:? "PostalCode")
            Prelude.<*> (x Data..:? "Primary")
            Prelude.<*> (x Data..:? "Region")
            Prelude.<*> (x Data..:? "StreetAddress")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable Address where
  hashWithSalt _salt Address' {..} =
    _salt `Prelude.hashWithSalt` country
      `Prelude.hashWithSalt` formatted
      `Prelude.hashWithSalt` locality
      `Prelude.hashWithSalt` postalCode
      `Prelude.hashWithSalt` primary
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` streetAddress
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Address where
  rnf Address' {..} =
    Prelude.rnf country
      `Prelude.seq` Prelude.rnf formatted
      `Prelude.seq` Prelude.rnf locality
      `Prelude.seq` Prelude.rnf postalCode
      `Prelude.seq` Prelude.rnf primary
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf streetAddress
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON Address where
  toJSON Address' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Country" Data..=) Prelude.<$> country,
            ("Formatted" Data..=) Prelude.<$> formatted,
            ("Locality" Data..=) Prelude.<$> locality,
            ("PostalCode" Data..=) Prelude.<$> postalCode,
            ("Primary" Data..=) Prelude.<$> primary,
            ("Region" Data..=) Prelude.<$> region,
            ("StreetAddress" Data..=) Prelude.<$> streetAddress,
            ("Type" Data..=) Prelude.<$> type'
          ]
      )
