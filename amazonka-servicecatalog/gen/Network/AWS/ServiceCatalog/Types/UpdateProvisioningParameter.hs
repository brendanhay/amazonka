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
-- Module      : Network.AWS.ServiceCatalog.Types.UpdateProvisioningParameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.UpdateProvisioningParameter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The parameter key-value pair used to update a provisioned product.
--
-- /See:/ 'newUpdateProvisioningParameter' smart constructor.
data UpdateProvisioningParameter = UpdateProvisioningParameter'
  { -- | The parameter key.
    key :: Prelude.Maybe Prelude.Text,
    -- | If set to true, @Value@ is ignored and the previous parameter value is
    -- kept.
    usePreviousValue :: Prelude.Maybe Prelude.Bool,
    -- | The parameter value.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { key = Prelude.Nothing,
      usePreviousValue = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The parameter key.
updateProvisioningParameter_key :: Lens.Lens' UpdateProvisioningParameter (Prelude.Maybe Prelude.Text)
updateProvisioningParameter_key = Lens.lens (\UpdateProvisioningParameter' {key} -> key) (\s@UpdateProvisioningParameter' {} a -> s {key = a} :: UpdateProvisioningParameter)

-- | If set to true, @Value@ is ignored and the previous parameter value is
-- kept.
updateProvisioningParameter_usePreviousValue :: Lens.Lens' UpdateProvisioningParameter (Prelude.Maybe Prelude.Bool)
updateProvisioningParameter_usePreviousValue = Lens.lens (\UpdateProvisioningParameter' {usePreviousValue} -> usePreviousValue) (\s@UpdateProvisioningParameter' {} a -> s {usePreviousValue = a} :: UpdateProvisioningParameter)

-- | The parameter value.
updateProvisioningParameter_value :: Lens.Lens' UpdateProvisioningParameter (Prelude.Maybe Prelude.Text)
updateProvisioningParameter_value = Lens.lens (\UpdateProvisioningParameter' {value} -> value) (\s@UpdateProvisioningParameter' {} a -> s {value = a} :: UpdateProvisioningParameter)

instance Prelude.FromJSON UpdateProvisioningParameter where
  parseJSON =
    Prelude.withObject
      "UpdateProvisioningParameter"
      ( \x ->
          UpdateProvisioningParameter'
            Prelude.<$> (x Prelude..:? "Key")
            Prelude.<*> (x Prelude..:? "UsePreviousValue")
            Prelude.<*> (x Prelude..:? "Value")
      )

instance Prelude.Hashable UpdateProvisioningParameter

instance Prelude.NFData UpdateProvisioningParameter

instance Prelude.ToJSON UpdateProvisioningParameter where
  toJSON UpdateProvisioningParameter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Key" Prelude..=) Prelude.<$> key,
            ("UsePreviousValue" Prelude..=)
              Prelude.<$> usePreviousValue,
            ("Value" Prelude..=) Prelude.<$> value
          ]
      )
