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
-- Module      : Amazonka.LicenseManager.Types.EntitlementData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.EntitlementData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types.EntitlementDataUnit
import qualified Amazonka.Prelude as Prelude

-- | Data associated with an entitlement resource.
--
-- /See:/ 'newEntitlementData' smart constructor.
data EntitlementData = EntitlementData'
  { -- | Entitlement data value.
    value :: Prelude.Maybe Prelude.Text,
    -- | Entitlement data name.
    name :: Prelude.Text,
    -- | Entitlement data unit.
    unit :: EntitlementDataUnit
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EntitlementData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'entitlementData_value' - Entitlement data value.
--
-- 'name', 'entitlementData_name' - Entitlement data name.
--
-- 'unit', 'entitlementData_unit' - Entitlement data unit.
newEntitlementData ::
  -- | 'name'
  Prelude.Text ->
  -- | 'unit'
  EntitlementDataUnit ->
  EntitlementData
newEntitlementData pName_ pUnit_ =
  EntitlementData'
    { value = Prelude.Nothing,
      name = pName_,
      unit = pUnit_
    }

-- | Entitlement data value.
entitlementData_value :: Lens.Lens' EntitlementData (Prelude.Maybe Prelude.Text)
entitlementData_value = Lens.lens (\EntitlementData' {value} -> value) (\s@EntitlementData' {} a -> s {value = a} :: EntitlementData)

-- | Entitlement data name.
entitlementData_name :: Lens.Lens' EntitlementData Prelude.Text
entitlementData_name = Lens.lens (\EntitlementData' {name} -> name) (\s@EntitlementData' {} a -> s {name = a} :: EntitlementData)

-- | Entitlement data unit.
entitlementData_unit :: Lens.Lens' EntitlementData EntitlementDataUnit
entitlementData_unit = Lens.lens (\EntitlementData' {unit} -> unit) (\s@EntitlementData' {} a -> s {unit = a} :: EntitlementData)

instance Data.FromJSON EntitlementData where
  parseJSON =
    Data.withObject
      "EntitlementData"
      ( \x ->
          EntitlementData'
            Prelude.<$> (x Data..:? "Value")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Unit")
      )

instance Prelude.Hashable EntitlementData where
  hashWithSalt _salt EntitlementData' {..} =
    _salt `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` unit

instance Prelude.NFData EntitlementData where
  rnf EntitlementData' {..} =
    Prelude.rnf value
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf unit

instance Data.ToJSON EntitlementData where
  toJSON EntitlementData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Value" Data..=) Prelude.<$> value,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Unit" Data..= unit)
          ]
      )
