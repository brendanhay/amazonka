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
-- Module      : Amazonka.QuickSight.Types.ComparisonFormatConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ComparisonFormatConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.NumberDisplayFormatConfiguration
import Amazonka.QuickSight.Types.PercentageDisplayFormatConfiguration

-- | The format of the comparison.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newComparisonFormatConfiguration' smart constructor.
data ComparisonFormatConfiguration = ComparisonFormatConfiguration'
  { -- | The number display format.
    numberDisplayFormatConfiguration :: Prelude.Maybe NumberDisplayFormatConfiguration,
    -- | The percentage display format.
    percentageDisplayFormatConfiguration :: Prelude.Maybe PercentageDisplayFormatConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComparisonFormatConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numberDisplayFormatConfiguration', 'comparisonFormatConfiguration_numberDisplayFormatConfiguration' - The number display format.
--
-- 'percentageDisplayFormatConfiguration', 'comparisonFormatConfiguration_percentageDisplayFormatConfiguration' - The percentage display format.
newComparisonFormatConfiguration ::
  ComparisonFormatConfiguration
newComparisonFormatConfiguration =
  ComparisonFormatConfiguration'
    { numberDisplayFormatConfiguration =
        Prelude.Nothing,
      percentageDisplayFormatConfiguration =
        Prelude.Nothing
    }

-- | The number display format.
comparisonFormatConfiguration_numberDisplayFormatConfiguration :: Lens.Lens' ComparisonFormatConfiguration (Prelude.Maybe NumberDisplayFormatConfiguration)
comparisonFormatConfiguration_numberDisplayFormatConfiguration = Lens.lens (\ComparisonFormatConfiguration' {numberDisplayFormatConfiguration} -> numberDisplayFormatConfiguration) (\s@ComparisonFormatConfiguration' {} a -> s {numberDisplayFormatConfiguration = a} :: ComparisonFormatConfiguration)

-- | The percentage display format.
comparisonFormatConfiguration_percentageDisplayFormatConfiguration :: Lens.Lens' ComparisonFormatConfiguration (Prelude.Maybe PercentageDisplayFormatConfiguration)
comparisonFormatConfiguration_percentageDisplayFormatConfiguration = Lens.lens (\ComparisonFormatConfiguration' {percentageDisplayFormatConfiguration} -> percentageDisplayFormatConfiguration) (\s@ComparisonFormatConfiguration' {} a -> s {percentageDisplayFormatConfiguration = a} :: ComparisonFormatConfiguration)

instance Data.FromJSON ComparisonFormatConfiguration where
  parseJSON =
    Data.withObject
      "ComparisonFormatConfiguration"
      ( \x ->
          ComparisonFormatConfiguration'
            Prelude.<$> (x Data..:? "NumberDisplayFormatConfiguration")
            Prelude.<*> (x Data..:? "PercentageDisplayFormatConfiguration")
      )

instance
  Prelude.Hashable
    ComparisonFormatConfiguration
  where
  hashWithSalt _salt ComparisonFormatConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` numberDisplayFormatConfiguration
      `Prelude.hashWithSalt` percentageDisplayFormatConfiguration

instance Prelude.NFData ComparisonFormatConfiguration where
  rnf ComparisonFormatConfiguration' {..} =
    Prelude.rnf numberDisplayFormatConfiguration
      `Prelude.seq` Prelude.rnf percentageDisplayFormatConfiguration

instance Data.ToJSON ComparisonFormatConfiguration where
  toJSON ComparisonFormatConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NumberDisplayFormatConfiguration" Data..=)
              Prelude.<$> numberDisplayFormatConfiguration,
            ("PercentageDisplayFormatConfiguration" Data..=)
              Prelude.<$> percentageDisplayFormatConfiguration
          ]
      )
