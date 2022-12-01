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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IdentityStore.Types.Address where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The address associated with the specified user.
--
-- /See:/ 'newAddress' smart constructor.
data Address = Address'
  { -- | A string representing the type of address. For example, \"Home.\"
    type' :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The street of the address.
    streetAddress :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | A string containing a formatted version of the address for display.
    formatted :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The postal code of the address.
    postalCode :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The country of the address.
    country :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The region of the address.
    region :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | A string of the address locality.
    locality :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | A Boolean value representing whether this is the primary address for the
    -- associated resource.
    primary :: Prelude.Maybe (Core.Sensitive Prelude.Bool)
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
-- 'type'', 'address_type' - A string representing the type of address. For example, \"Home.\"
--
-- 'streetAddress', 'address_streetAddress' - The street of the address.
--
-- 'formatted', 'address_formatted' - A string containing a formatted version of the address for display.
--
-- 'postalCode', 'address_postalCode' - The postal code of the address.
--
-- 'country', 'address_country' - The country of the address.
--
-- 'region', 'address_region' - The region of the address.
--
-- 'locality', 'address_locality' - A string of the address locality.
--
-- 'primary', 'address_primary' - A Boolean value representing whether this is the primary address for the
-- associated resource.
newAddress ::
  Address
newAddress =
  Address'
    { type' = Prelude.Nothing,
      streetAddress = Prelude.Nothing,
      formatted = Prelude.Nothing,
      postalCode = Prelude.Nothing,
      country = Prelude.Nothing,
      region = Prelude.Nothing,
      locality = Prelude.Nothing,
      primary = Prelude.Nothing
    }

-- | A string representing the type of address. For example, \"Home.\"
address_type :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_type = Lens.lens (\Address' {type'} -> type') (\s@Address' {} a -> s {type' = a} :: Address) Prelude.. Lens.mapping Core._Sensitive

-- | The street of the address.
address_streetAddress :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_streetAddress = Lens.lens (\Address' {streetAddress} -> streetAddress) (\s@Address' {} a -> s {streetAddress = a} :: Address) Prelude.. Lens.mapping Core._Sensitive

-- | A string containing a formatted version of the address for display.
address_formatted :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_formatted = Lens.lens (\Address' {formatted} -> formatted) (\s@Address' {} a -> s {formatted = a} :: Address) Prelude.. Lens.mapping Core._Sensitive

-- | The postal code of the address.
address_postalCode :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_postalCode = Lens.lens (\Address' {postalCode} -> postalCode) (\s@Address' {} a -> s {postalCode = a} :: Address) Prelude.. Lens.mapping Core._Sensitive

-- | The country of the address.
address_country :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_country = Lens.lens (\Address' {country} -> country) (\s@Address' {} a -> s {country = a} :: Address) Prelude.. Lens.mapping Core._Sensitive

-- | The region of the address.
address_region :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_region = Lens.lens (\Address' {region} -> region) (\s@Address' {} a -> s {region = a} :: Address) Prelude.. Lens.mapping Core._Sensitive

-- | A string of the address locality.
address_locality :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_locality = Lens.lens (\Address' {locality} -> locality) (\s@Address' {} a -> s {locality = a} :: Address) Prelude.. Lens.mapping Core._Sensitive

-- | A Boolean value representing whether this is the primary address for the
-- associated resource.
address_primary :: Lens.Lens' Address (Prelude.Maybe Prelude.Bool)
address_primary = Lens.lens (\Address' {primary} -> primary) (\s@Address' {} a -> s {primary = a} :: Address) Prelude.. Lens.mapping Core._Sensitive

instance Core.FromJSON Address where
  parseJSON =
    Core.withObject
      "Address"
      ( \x ->
          Address'
            Prelude.<$> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "StreetAddress")
            Prelude.<*> (x Core..:? "Formatted")
            Prelude.<*> (x Core..:? "PostalCode")
            Prelude.<*> (x Core..:? "Country")
            Prelude.<*> (x Core..:? "Region")
            Prelude.<*> (x Core..:? "Locality")
            Prelude.<*> (x Core..:? "Primary")
      )

instance Prelude.Hashable Address where
  hashWithSalt _salt Address' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` streetAddress
      `Prelude.hashWithSalt` formatted
      `Prelude.hashWithSalt` postalCode
      `Prelude.hashWithSalt` country
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` locality
      `Prelude.hashWithSalt` primary

instance Prelude.NFData Address where
  rnf Address' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf streetAddress
      `Prelude.seq` Prelude.rnf formatted
      `Prelude.seq` Prelude.rnf postalCode
      `Prelude.seq` Prelude.rnf country
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf locality
      `Prelude.seq` Prelude.rnf primary

instance Core.ToJSON Address where
  toJSON Address' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Type" Core..=) Prelude.<$> type',
            ("StreetAddress" Core..=) Prelude.<$> streetAddress,
            ("Formatted" Core..=) Prelude.<$> formatted,
            ("PostalCode" Core..=) Prelude.<$> postalCode,
            ("Country" Core..=) Prelude.<$> country,
            ("Region" Core..=) Prelude.<$> region,
            ("Locality" Core..=) Prelude.<$> locality,
            ("Primary" Core..=) Prelude.<$> primary
          ]
      )
