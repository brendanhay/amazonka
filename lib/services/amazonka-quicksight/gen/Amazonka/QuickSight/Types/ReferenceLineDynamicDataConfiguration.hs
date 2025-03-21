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
-- Module      : Amazonka.QuickSight.Types.ReferenceLineDynamicDataConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ReferenceLineDynamicDataConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AggregationFunction
import Amazonka.QuickSight.Types.ColumnIdentifier
import Amazonka.QuickSight.Types.NumericalAggregationFunction

-- | The dynamic configuration of the reference line data configuration.
--
-- /See:/ 'newReferenceLineDynamicDataConfiguration' smart constructor.
data ReferenceLineDynamicDataConfiguration = ReferenceLineDynamicDataConfiguration'
  { -- | The column that the dynamic data targets.
    column :: ColumnIdentifier,
    -- | The aggregation function that is used in the dynamic data.
    measureAggregationFunction :: AggregationFunction,
    -- | The calculation that is used in the dynamic data.
    calculation :: NumericalAggregationFunction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReferenceLineDynamicDataConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'column', 'referenceLineDynamicDataConfiguration_column' - The column that the dynamic data targets.
--
-- 'measureAggregationFunction', 'referenceLineDynamicDataConfiguration_measureAggregationFunction' - The aggregation function that is used in the dynamic data.
--
-- 'calculation', 'referenceLineDynamicDataConfiguration_calculation' - The calculation that is used in the dynamic data.
newReferenceLineDynamicDataConfiguration ::
  -- | 'column'
  ColumnIdentifier ->
  -- | 'measureAggregationFunction'
  AggregationFunction ->
  -- | 'calculation'
  NumericalAggregationFunction ->
  ReferenceLineDynamicDataConfiguration
newReferenceLineDynamicDataConfiguration
  pColumn_
  pMeasureAggregationFunction_
  pCalculation_ =
    ReferenceLineDynamicDataConfiguration'
      { column =
          pColumn_,
        measureAggregationFunction =
          pMeasureAggregationFunction_,
        calculation = pCalculation_
      }

-- | The column that the dynamic data targets.
referenceLineDynamicDataConfiguration_column :: Lens.Lens' ReferenceLineDynamicDataConfiguration ColumnIdentifier
referenceLineDynamicDataConfiguration_column = Lens.lens (\ReferenceLineDynamicDataConfiguration' {column} -> column) (\s@ReferenceLineDynamicDataConfiguration' {} a -> s {column = a} :: ReferenceLineDynamicDataConfiguration)

-- | The aggregation function that is used in the dynamic data.
referenceLineDynamicDataConfiguration_measureAggregationFunction :: Lens.Lens' ReferenceLineDynamicDataConfiguration AggregationFunction
referenceLineDynamicDataConfiguration_measureAggregationFunction = Lens.lens (\ReferenceLineDynamicDataConfiguration' {measureAggregationFunction} -> measureAggregationFunction) (\s@ReferenceLineDynamicDataConfiguration' {} a -> s {measureAggregationFunction = a} :: ReferenceLineDynamicDataConfiguration)

-- | The calculation that is used in the dynamic data.
referenceLineDynamicDataConfiguration_calculation :: Lens.Lens' ReferenceLineDynamicDataConfiguration NumericalAggregationFunction
referenceLineDynamicDataConfiguration_calculation = Lens.lens (\ReferenceLineDynamicDataConfiguration' {calculation} -> calculation) (\s@ReferenceLineDynamicDataConfiguration' {} a -> s {calculation = a} :: ReferenceLineDynamicDataConfiguration)

instance
  Data.FromJSON
    ReferenceLineDynamicDataConfiguration
  where
  parseJSON =
    Data.withObject
      "ReferenceLineDynamicDataConfiguration"
      ( \x ->
          ReferenceLineDynamicDataConfiguration'
            Prelude.<$> (x Data..: "Column")
            Prelude.<*> (x Data..: "MeasureAggregationFunction")
            Prelude.<*> (x Data..: "Calculation")
      )

instance
  Prelude.Hashable
    ReferenceLineDynamicDataConfiguration
  where
  hashWithSalt
    _salt
    ReferenceLineDynamicDataConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` column
        `Prelude.hashWithSalt` measureAggregationFunction
        `Prelude.hashWithSalt` calculation

instance
  Prelude.NFData
    ReferenceLineDynamicDataConfiguration
  where
  rnf ReferenceLineDynamicDataConfiguration' {..} =
    Prelude.rnf column `Prelude.seq`
      Prelude.rnf measureAggregationFunction `Prelude.seq`
        Prelude.rnf calculation

instance
  Data.ToJSON
    ReferenceLineDynamicDataConfiguration
  where
  toJSON ReferenceLineDynamicDataConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Column" Data..= column),
            Prelude.Just
              ( "MeasureAggregationFunction"
                  Data..= measureAggregationFunction
              ),
            Prelude.Just ("Calculation" Data..= calculation)
          ]
      )
