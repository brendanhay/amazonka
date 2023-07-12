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
-- Module      : Amazonka.PinpointSmsVoiceV2.Types.ConfigurationSetFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types.ConfigurationSetFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types.ConfigurationSetFilterName
import qualified Amazonka.Prelude as Prelude

-- | The information for configuration sets that meet a specified criteria.
--
-- /See:/ 'newConfigurationSetFilter' smart constructor.
data ConfigurationSetFilter = ConfigurationSetFilter'
  { -- | The name of the attribute to filter on.
    name :: ConfigurationSetFilterName,
    -- | An array values to filter for.
    values :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigurationSetFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'configurationSetFilter_name' - The name of the attribute to filter on.
--
-- 'values', 'configurationSetFilter_values' - An array values to filter for.
newConfigurationSetFilter ::
  -- | 'name'
  ConfigurationSetFilterName ->
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  ConfigurationSetFilter
newConfigurationSetFilter pName_ pValues_ =
  ConfigurationSetFilter'
    { name = pName_,
      values = Lens.coerced Lens.# pValues_
    }

-- | The name of the attribute to filter on.
configurationSetFilter_name :: Lens.Lens' ConfigurationSetFilter ConfigurationSetFilterName
configurationSetFilter_name = Lens.lens (\ConfigurationSetFilter' {name} -> name) (\s@ConfigurationSetFilter' {} a -> s {name = a} :: ConfigurationSetFilter)

-- | An array values to filter for.
configurationSetFilter_values :: Lens.Lens' ConfigurationSetFilter (Prelude.NonEmpty Prelude.Text)
configurationSetFilter_values = Lens.lens (\ConfigurationSetFilter' {values} -> values) (\s@ConfigurationSetFilter' {} a -> s {values = a} :: ConfigurationSetFilter) Prelude.. Lens.coerced

instance Prelude.Hashable ConfigurationSetFilter where
  hashWithSalt _salt ConfigurationSetFilter' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData ConfigurationSetFilter where
  rnf ConfigurationSetFilter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance Data.ToJSON ConfigurationSetFilter where
  toJSON ConfigurationSetFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Values" Data..= values)
          ]
      )
