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
-- Module      : Amazonka.ServiceCatalog.Types.UpdateProvisioningParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.UpdateProvisioningParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON UpdateProvisioningParameter where
  parseJSON =
    Data.withObject
      "UpdateProvisioningParameter"
      ( \x ->
          UpdateProvisioningParameter'
            Prelude.<$> (x Data..:? "Key")
            Prelude.<*> (x Data..:? "UsePreviousValue")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable UpdateProvisioningParameter where
  hashWithSalt _salt UpdateProvisioningParameter' {..} =
    _salt
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` usePreviousValue
      `Prelude.hashWithSalt` value

instance Prelude.NFData UpdateProvisioningParameter where
  rnf UpdateProvisioningParameter' {..} =
    Prelude.rnf key
      `Prelude.seq` Prelude.rnf usePreviousValue
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON UpdateProvisioningParameter where
  toJSON UpdateProvisioningParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Key" Data..=) Prelude.<$> key,
            ("UsePreviousValue" Data..=)
              Prelude.<$> usePreviousValue,
            ("Value" Data..=) Prelude.<$> value
          ]
      )
