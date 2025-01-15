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
-- Module      : Amazonka.QuickSight.Types.ComparisonConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ComparisonConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ComparisonFormatConfiguration
import Amazonka.QuickSight.Types.ComparisonMethod

-- | The comparison display configuration of a KPI or gauge chart.
--
-- /See:/ 'newComparisonConfiguration' smart constructor.
data ComparisonConfiguration = ComparisonConfiguration'
  { -- | The format of the comparison.
    comparisonFormat :: Prelude.Maybe ComparisonFormatConfiguration,
    -- | The method of the comparison. Choose from the following options:
    --
    -- -   @DIFFERENCE@
    --
    -- -   @PERCENT_DIFFERENCE@
    --
    -- -   @PERCENT@
    comparisonMethod :: Prelude.Maybe ComparisonMethod
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComparisonConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comparisonFormat', 'comparisonConfiguration_comparisonFormat' - The format of the comparison.
--
-- 'comparisonMethod', 'comparisonConfiguration_comparisonMethod' - The method of the comparison. Choose from the following options:
--
-- -   @DIFFERENCE@
--
-- -   @PERCENT_DIFFERENCE@
--
-- -   @PERCENT@
newComparisonConfiguration ::
  ComparisonConfiguration
newComparisonConfiguration =
  ComparisonConfiguration'
    { comparisonFormat =
        Prelude.Nothing,
      comparisonMethod = Prelude.Nothing
    }

-- | The format of the comparison.
comparisonConfiguration_comparisonFormat :: Lens.Lens' ComparisonConfiguration (Prelude.Maybe ComparisonFormatConfiguration)
comparisonConfiguration_comparisonFormat = Lens.lens (\ComparisonConfiguration' {comparisonFormat} -> comparisonFormat) (\s@ComparisonConfiguration' {} a -> s {comparisonFormat = a} :: ComparisonConfiguration)

-- | The method of the comparison. Choose from the following options:
--
-- -   @DIFFERENCE@
--
-- -   @PERCENT_DIFFERENCE@
--
-- -   @PERCENT@
comparisonConfiguration_comparisonMethod :: Lens.Lens' ComparisonConfiguration (Prelude.Maybe ComparisonMethod)
comparisonConfiguration_comparisonMethod = Lens.lens (\ComparisonConfiguration' {comparisonMethod} -> comparisonMethod) (\s@ComparisonConfiguration' {} a -> s {comparisonMethod = a} :: ComparisonConfiguration)

instance Data.FromJSON ComparisonConfiguration where
  parseJSON =
    Data.withObject
      "ComparisonConfiguration"
      ( \x ->
          ComparisonConfiguration'
            Prelude.<$> (x Data..:? "ComparisonFormat")
            Prelude.<*> (x Data..:? "ComparisonMethod")
      )

instance Prelude.Hashable ComparisonConfiguration where
  hashWithSalt _salt ComparisonConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` comparisonFormat
      `Prelude.hashWithSalt` comparisonMethod

instance Prelude.NFData ComparisonConfiguration where
  rnf ComparisonConfiguration' {..} =
    Prelude.rnf comparisonFormat `Prelude.seq`
      Prelude.rnf comparisonMethod

instance Data.ToJSON ComparisonConfiguration where
  toJSON ComparisonConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ComparisonFormat" Data..=)
              Prelude.<$> comparisonFormat,
            ("ComparisonMethod" Data..=)
              Prelude.<$> comparisonMethod
          ]
      )
