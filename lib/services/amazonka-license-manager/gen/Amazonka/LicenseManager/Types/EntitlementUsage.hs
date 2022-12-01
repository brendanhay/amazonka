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
-- Module      : Amazonka.LicenseManager.Types.EntitlementUsage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.EntitlementUsage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LicenseManager.Types.EntitlementDataUnit
import qualified Amazonka.Prelude as Prelude

-- | Usage associated with an entitlement resource.
--
-- /See:/ 'newEntitlementUsage' smart constructor.
data EntitlementUsage = EntitlementUsage'
  { -- | Maximum entitlement usage count.
    maxCount :: Prelude.Maybe Prelude.Text,
    -- | Entitlement usage name.
    name :: Prelude.Text,
    -- | Resource usage consumed.
    consumedValue :: Prelude.Text,
    -- | Entitlement usage unit.
    unit :: EntitlementDataUnit
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EntitlementUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxCount', 'entitlementUsage_maxCount' - Maximum entitlement usage count.
--
-- 'name', 'entitlementUsage_name' - Entitlement usage name.
--
-- 'consumedValue', 'entitlementUsage_consumedValue' - Resource usage consumed.
--
-- 'unit', 'entitlementUsage_unit' - Entitlement usage unit.
newEntitlementUsage ::
  -- | 'name'
  Prelude.Text ->
  -- | 'consumedValue'
  Prelude.Text ->
  -- | 'unit'
  EntitlementDataUnit ->
  EntitlementUsage
newEntitlementUsage pName_ pConsumedValue_ pUnit_ =
  EntitlementUsage'
    { maxCount = Prelude.Nothing,
      name = pName_,
      consumedValue = pConsumedValue_,
      unit = pUnit_
    }

-- | Maximum entitlement usage count.
entitlementUsage_maxCount :: Lens.Lens' EntitlementUsage (Prelude.Maybe Prelude.Text)
entitlementUsage_maxCount = Lens.lens (\EntitlementUsage' {maxCount} -> maxCount) (\s@EntitlementUsage' {} a -> s {maxCount = a} :: EntitlementUsage)

-- | Entitlement usage name.
entitlementUsage_name :: Lens.Lens' EntitlementUsage Prelude.Text
entitlementUsage_name = Lens.lens (\EntitlementUsage' {name} -> name) (\s@EntitlementUsage' {} a -> s {name = a} :: EntitlementUsage)

-- | Resource usage consumed.
entitlementUsage_consumedValue :: Lens.Lens' EntitlementUsage Prelude.Text
entitlementUsage_consumedValue = Lens.lens (\EntitlementUsage' {consumedValue} -> consumedValue) (\s@EntitlementUsage' {} a -> s {consumedValue = a} :: EntitlementUsage)

-- | Entitlement usage unit.
entitlementUsage_unit :: Lens.Lens' EntitlementUsage EntitlementDataUnit
entitlementUsage_unit = Lens.lens (\EntitlementUsage' {unit} -> unit) (\s@EntitlementUsage' {} a -> s {unit = a} :: EntitlementUsage)

instance Core.FromJSON EntitlementUsage where
  parseJSON =
    Core.withObject
      "EntitlementUsage"
      ( \x ->
          EntitlementUsage'
            Prelude.<$> (x Core..:? "MaxCount")
            Prelude.<*> (x Core..: "Name")
            Prelude.<*> (x Core..: "ConsumedValue")
            Prelude.<*> (x Core..: "Unit")
      )

instance Prelude.Hashable EntitlementUsage where
  hashWithSalt _salt EntitlementUsage' {..} =
    _salt `Prelude.hashWithSalt` maxCount
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` consumedValue
      `Prelude.hashWithSalt` unit

instance Prelude.NFData EntitlementUsage where
  rnf EntitlementUsage' {..} =
    Prelude.rnf maxCount
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf consumedValue
      `Prelude.seq` Prelude.rnf unit
