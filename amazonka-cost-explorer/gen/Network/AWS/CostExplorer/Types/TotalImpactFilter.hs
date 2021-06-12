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
-- Module      : Network.AWS.CostExplorer.Types.TotalImpactFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.TotalImpactFilter where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types.NumericOperator
import qualified Network.AWS.Lens as Lens

-- | Filters cost anomalies based on the total impact.
--
-- /See:/ 'newTotalImpactFilter' smart constructor.
data TotalImpactFilter = TotalImpactFilter'
  { -- | The upper bound dollar value used in the filter.
    endValue :: Core.Maybe Core.Double,
    -- | The comparing value used in the filter.
    numericOperator :: NumericOperator,
    -- | The lower bound dollar value used in the filter.
    startValue :: Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TotalImpactFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endValue', 'totalImpactFilter_endValue' - The upper bound dollar value used in the filter.
--
-- 'numericOperator', 'totalImpactFilter_numericOperator' - The comparing value used in the filter.
--
-- 'startValue', 'totalImpactFilter_startValue' - The lower bound dollar value used in the filter.
newTotalImpactFilter ::
  -- | 'numericOperator'
  NumericOperator ->
  -- | 'startValue'
  Core.Double ->
  TotalImpactFilter
newTotalImpactFilter pNumericOperator_ pStartValue_ =
  TotalImpactFilter'
    { endValue = Core.Nothing,
      numericOperator = pNumericOperator_,
      startValue = pStartValue_
    }

-- | The upper bound dollar value used in the filter.
totalImpactFilter_endValue :: Lens.Lens' TotalImpactFilter (Core.Maybe Core.Double)
totalImpactFilter_endValue = Lens.lens (\TotalImpactFilter' {endValue} -> endValue) (\s@TotalImpactFilter' {} a -> s {endValue = a} :: TotalImpactFilter)

-- | The comparing value used in the filter.
totalImpactFilter_numericOperator :: Lens.Lens' TotalImpactFilter NumericOperator
totalImpactFilter_numericOperator = Lens.lens (\TotalImpactFilter' {numericOperator} -> numericOperator) (\s@TotalImpactFilter' {} a -> s {numericOperator = a} :: TotalImpactFilter)

-- | The lower bound dollar value used in the filter.
totalImpactFilter_startValue :: Lens.Lens' TotalImpactFilter Core.Double
totalImpactFilter_startValue = Lens.lens (\TotalImpactFilter' {startValue} -> startValue) (\s@TotalImpactFilter' {} a -> s {startValue = a} :: TotalImpactFilter)

instance Core.Hashable TotalImpactFilter

instance Core.NFData TotalImpactFilter

instance Core.ToJSON TotalImpactFilter where
  toJSON TotalImpactFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("EndValue" Core..=) Core.<$> endValue,
            Core.Just
              ("NumericOperator" Core..= numericOperator),
            Core.Just ("StartValue" Core..= startValue)
          ]
      )
