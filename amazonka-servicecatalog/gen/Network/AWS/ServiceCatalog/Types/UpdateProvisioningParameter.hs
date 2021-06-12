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
-- Module      : Network.AWS.ServiceCatalog.Types.UpdateProvisioningParameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.UpdateProvisioningParameter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The parameter key-value pair used to update a provisioned product.
--
-- /See:/ 'newUpdateProvisioningParameter' smart constructor.
data UpdateProvisioningParameter = UpdateProvisioningParameter'
  { -- | The parameter key.
    key :: Core.Maybe Core.Text,
    -- | If set to true, @Value@ is ignored and the previous parameter value is
    -- kept.
    usePreviousValue :: Core.Maybe Core.Bool,
    -- | The parameter value.
    value :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateProvisioningParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'updateProvisioningParameter_key' - The parameter key.
--
-- 'usePreviousValue', 'updateProvisioningParameter_usePreviousValue' - If set to true, @Value@ is ignored and the previous parameter value is
-- kept.
--
-- 'value', 'updateProvisioningParameter_value' - The parameter value.
newUpdateProvisioningParameter ::
  UpdateProvisioningParameter
newUpdateProvisioningParameter =
  UpdateProvisioningParameter'
    { key = Core.Nothing,
      usePreviousValue = Core.Nothing,
      value = Core.Nothing
    }

-- | The parameter key.
updateProvisioningParameter_key :: Lens.Lens' UpdateProvisioningParameter (Core.Maybe Core.Text)
updateProvisioningParameter_key = Lens.lens (\UpdateProvisioningParameter' {key} -> key) (\s@UpdateProvisioningParameter' {} a -> s {key = a} :: UpdateProvisioningParameter)

-- | If set to true, @Value@ is ignored and the previous parameter value is
-- kept.
updateProvisioningParameter_usePreviousValue :: Lens.Lens' UpdateProvisioningParameter (Core.Maybe Core.Bool)
updateProvisioningParameter_usePreviousValue = Lens.lens (\UpdateProvisioningParameter' {usePreviousValue} -> usePreviousValue) (\s@UpdateProvisioningParameter' {} a -> s {usePreviousValue = a} :: UpdateProvisioningParameter)

-- | The parameter value.
updateProvisioningParameter_value :: Lens.Lens' UpdateProvisioningParameter (Core.Maybe Core.Text)
updateProvisioningParameter_value = Lens.lens (\UpdateProvisioningParameter' {value} -> value) (\s@UpdateProvisioningParameter' {} a -> s {value = a} :: UpdateProvisioningParameter)

instance Core.FromJSON UpdateProvisioningParameter where
  parseJSON =
    Core.withObject
      "UpdateProvisioningParameter"
      ( \x ->
          UpdateProvisioningParameter'
            Core.<$> (x Core..:? "Key")
            Core.<*> (x Core..:? "UsePreviousValue")
            Core.<*> (x Core..:? "Value")
      )

instance Core.Hashable UpdateProvisioningParameter

instance Core.NFData UpdateProvisioningParameter

instance Core.ToJSON UpdateProvisioningParameter where
  toJSON UpdateProvisioningParameter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Key" Core..=) Core.<$> key,
            ("UsePreviousValue" Core..=)
              Core.<$> usePreviousValue,
            ("Value" Core..=) Core.<$> value
          ]
      )
