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
-- Module      : Amazonka.CostExplorer.Types.TotalImpactFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.TotalImpactFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.NumericOperator
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Filters cost anomalies based on the total impact.
--
-- /See:/ 'newTotalImpactFilter' smart constructor.
data TotalImpactFilter = TotalImpactFilter'
  { -- | The upper bound dollar value that\'s used in the filter.
    endValue :: Prelude.Maybe Prelude.Double,
    -- | The comparing value that\'s used in the filter.
    numericOperator :: NumericOperator,
    -- | The lower bound dollar value that\'s used in the filter.
    startValue :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TotalImpactFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endValue', 'totalImpactFilter_endValue' - The upper bound dollar value that\'s used in the filter.
--
-- 'numericOperator', 'totalImpactFilter_numericOperator' - The comparing value that\'s used in the filter.
--
-- 'startValue', 'totalImpactFilter_startValue' - The lower bound dollar value that\'s used in the filter.
newTotalImpactFilter ::
  -- | 'numericOperator'
  NumericOperator ->
  -- | 'startValue'
  Prelude.Double ->
  TotalImpactFilter
newTotalImpactFilter pNumericOperator_ pStartValue_ =
  TotalImpactFilter'
    { endValue = Prelude.Nothing,
      numericOperator = pNumericOperator_,
      startValue = pStartValue_
    }

-- | The upper bound dollar value that\'s used in the filter.
totalImpactFilter_endValue :: Lens.Lens' TotalImpactFilter (Prelude.Maybe Prelude.Double)
totalImpactFilter_endValue = Lens.lens (\TotalImpactFilter' {endValue} -> endValue) (\s@TotalImpactFilter' {} a -> s {endValue = a} :: TotalImpactFilter)

-- | The comparing value that\'s used in the filter.
totalImpactFilter_numericOperator :: Lens.Lens' TotalImpactFilter NumericOperator
totalImpactFilter_numericOperator = Lens.lens (\TotalImpactFilter' {numericOperator} -> numericOperator) (\s@TotalImpactFilter' {} a -> s {numericOperator = a} :: TotalImpactFilter)

-- | The lower bound dollar value that\'s used in the filter.
totalImpactFilter_startValue :: Lens.Lens' TotalImpactFilter Prelude.Double
totalImpactFilter_startValue = Lens.lens (\TotalImpactFilter' {startValue} -> startValue) (\s@TotalImpactFilter' {} a -> s {startValue = a} :: TotalImpactFilter)

instance Prelude.Hashable TotalImpactFilter where
  hashWithSalt _salt TotalImpactFilter' {..} =
    _salt `Prelude.hashWithSalt` endValue
      `Prelude.hashWithSalt` numericOperator
      `Prelude.hashWithSalt` startValue

instance Prelude.NFData TotalImpactFilter where
  rnf TotalImpactFilter' {..} =
    Prelude.rnf endValue
      `Prelude.seq` Prelude.rnf numericOperator
      `Prelude.seq` Prelude.rnf startValue

instance Data.ToJSON TotalImpactFilter where
  toJSON TotalImpactFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EndValue" Data..=) Prelude.<$> endValue,
            Prelude.Just
              ("NumericOperator" Data..= numericOperator),
            Prelude.Just ("StartValue" Data..= startValue)
          ]
      )
