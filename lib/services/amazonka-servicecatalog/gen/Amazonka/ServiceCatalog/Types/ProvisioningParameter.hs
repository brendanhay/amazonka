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
-- Module      : Amazonka.ServiceCatalog.Types.ProvisioningParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ProvisioningParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a parameter used to provision a product.
--
-- /See:/ 'newProvisioningParameter' smart constructor.
data ProvisioningParameter = ProvisioningParameter'
  { -- | The parameter key.
    key :: Prelude.Maybe Prelude.Text,
    -- | The parameter value.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProvisioningParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'provisioningParameter_key' - The parameter key.
--
-- 'value', 'provisioningParameter_value' - The parameter value.
newProvisioningParameter ::
  ProvisioningParameter
newProvisioningParameter =
  ProvisioningParameter'
    { key = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The parameter key.
provisioningParameter_key :: Lens.Lens' ProvisioningParameter (Prelude.Maybe Prelude.Text)
provisioningParameter_key = Lens.lens (\ProvisioningParameter' {key} -> key) (\s@ProvisioningParameter' {} a -> s {key = a} :: ProvisioningParameter)

-- | The parameter value.
provisioningParameter_value :: Lens.Lens' ProvisioningParameter (Prelude.Maybe Prelude.Text)
provisioningParameter_value = Lens.lens (\ProvisioningParameter' {value} -> value) (\s@ProvisioningParameter' {} a -> s {value = a} :: ProvisioningParameter)

instance Prelude.Hashable ProvisioningParameter where
  hashWithSalt _salt ProvisioningParameter' {..} =
    _salt
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData ProvisioningParameter where
  rnf ProvisioningParameter' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value

instance Data.ToJSON ProvisioningParameter where
  toJSON ProvisioningParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Key" Data..=) Prelude.<$> key,
            ("Value" Data..=) Prelude.<$> value
          ]
      )
