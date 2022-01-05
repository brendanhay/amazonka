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
-- Module      : Amazonka.LicenseManager.Types.Entitlement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.Entitlement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LicenseManager.Types.EntitlementUnit
import qualified Amazonka.Prelude as Prelude

-- | Describes a resource entitled for use with a license.
--
-- /See:/ 'newEntitlement' smart constructor.
data Entitlement = Entitlement'
  { -- | Maximum entitlement count. Use if the unit is not None.
    maxCount :: Prelude.Maybe Prelude.Integer,
    -- | Entitlement resource. Use only if the unit is None.
    value :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether overages are allowed.
    overage :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether check-ins are allowed.
    allowCheckIn :: Prelude.Maybe Prelude.Bool,
    -- | Entitlement name.
    name :: Prelude.Text,
    -- | Entitlement unit.
    unit :: EntitlementUnit
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Entitlement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxCount', 'entitlement_maxCount' - Maximum entitlement count. Use if the unit is not None.
--
-- 'value', 'entitlement_value' - Entitlement resource. Use only if the unit is None.
--
-- 'overage', 'entitlement_overage' - Indicates whether overages are allowed.
--
-- 'allowCheckIn', 'entitlement_allowCheckIn' - Indicates whether check-ins are allowed.
--
-- 'name', 'entitlement_name' - Entitlement name.
--
-- 'unit', 'entitlement_unit' - Entitlement unit.
newEntitlement ::
  -- | 'name'
  Prelude.Text ->
  -- | 'unit'
  EntitlementUnit ->
  Entitlement
newEntitlement pName_ pUnit_ =
  Entitlement'
    { maxCount = Prelude.Nothing,
      value = Prelude.Nothing,
      overage = Prelude.Nothing,
      allowCheckIn = Prelude.Nothing,
      name = pName_,
      unit = pUnit_
    }

-- | Maximum entitlement count. Use if the unit is not None.
entitlement_maxCount :: Lens.Lens' Entitlement (Prelude.Maybe Prelude.Integer)
entitlement_maxCount = Lens.lens (\Entitlement' {maxCount} -> maxCount) (\s@Entitlement' {} a -> s {maxCount = a} :: Entitlement)

-- | Entitlement resource. Use only if the unit is None.
entitlement_value :: Lens.Lens' Entitlement (Prelude.Maybe Prelude.Text)
entitlement_value = Lens.lens (\Entitlement' {value} -> value) (\s@Entitlement' {} a -> s {value = a} :: Entitlement)

-- | Indicates whether overages are allowed.
entitlement_overage :: Lens.Lens' Entitlement (Prelude.Maybe Prelude.Bool)
entitlement_overage = Lens.lens (\Entitlement' {overage} -> overage) (\s@Entitlement' {} a -> s {overage = a} :: Entitlement)

-- | Indicates whether check-ins are allowed.
entitlement_allowCheckIn :: Lens.Lens' Entitlement (Prelude.Maybe Prelude.Bool)
entitlement_allowCheckIn = Lens.lens (\Entitlement' {allowCheckIn} -> allowCheckIn) (\s@Entitlement' {} a -> s {allowCheckIn = a} :: Entitlement)

-- | Entitlement name.
entitlement_name :: Lens.Lens' Entitlement Prelude.Text
entitlement_name = Lens.lens (\Entitlement' {name} -> name) (\s@Entitlement' {} a -> s {name = a} :: Entitlement)

-- | Entitlement unit.
entitlement_unit :: Lens.Lens' Entitlement EntitlementUnit
entitlement_unit = Lens.lens (\Entitlement' {unit} -> unit) (\s@Entitlement' {} a -> s {unit = a} :: Entitlement)

instance Core.FromJSON Entitlement where
  parseJSON =
    Core.withObject
      "Entitlement"
      ( \x ->
          Entitlement'
            Prelude.<$> (x Core..:? "MaxCount")
            Prelude.<*> (x Core..:? "Value")
            Prelude.<*> (x Core..:? "Overage")
            Prelude.<*> (x Core..:? "AllowCheckIn")
            Prelude.<*> (x Core..: "Name")
            Prelude.<*> (x Core..: "Unit")
      )

instance Prelude.Hashable Entitlement where
  hashWithSalt _salt Entitlement' {..} =
    _salt `Prelude.hashWithSalt` maxCount
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` overage
      `Prelude.hashWithSalt` allowCheckIn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` unit

instance Prelude.NFData Entitlement where
  rnf Entitlement' {..} =
    Prelude.rnf maxCount
      `Prelude.seq` Prelude.rnf value
      `Prelude.seq` Prelude.rnf overage
      `Prelude.seq` Prelude.rnf allowCheckIn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf unit

instance Core.ToJSON Entitlement where
  toJSON Entitlement' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MaxCount" Core..=) Prelude.<$> maxCount,
            ("Value" Core..=) Prelude.<$> value,
            ("Overage" Core..=) Prelude.<$> overage,
            ("AllowCheckIn" Core..=) Prelude.<$> allowCheckIn,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("Unit" Core..= unit)
          ]
      )
