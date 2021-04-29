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
-- Module      : Network.AWS.CostExplorer.Types.TotalImpactFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.TotalImpactFilter where

import Network.AWS.CostExplorer.Types.NumericOperator
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Filters cost anomalies based on the total impact.
--
-- /See:/ 'newTotalImpactFilter' smart constructor.
data TotalImpactFilter = TotalImpactFilter'
  { -- | The upper bound dollar value used in the filter.
    endValue :: Prelude.Maybe Prelude.Double,
    -- | The comparing value used in the filter.
    numericOperator :: NumericOperator,
    -- | The lower bound dollar value used in the filter.
    startValue :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Double ->
  TotalImpactFilter
newTotalImpactFilter pNumericOperator_ pStartValue_ =
  TotalImpactFilter'
    { endValue = Prelude.Nothing,
      numericOperator = pNumericOperator_,
      startValue = pStartValue_
    }

-- | The upper bound dollar value used in the filter.
totalImpactFilter_endValue :: Lens.Lens' TotalImpactFilter (Prelude.Maybe Prelude.Double)
totalImpactFilter_endValue = Lens.lens (\TotalImpactFilter' {endValue} -> endValue) (\s@TotalImpactFilter' {} a -> s {endValue = a} :: TotalImpactFilter)

-- | The comparing value used in the filter.
totalImpactFilter_numericOperator :: Lens.Lens' TotalImpactFilter NumericOperator
totalImpactFilter_numericOperator = Lens.lens (\TotalImpactFilter' {numericOperator} -> numericOperator) (\s@TotalImpactFilter' {} a -> s {numericOperator = a} :: TotalImpactFilter)

-- | The lower bound dollar value used in the filter.
totalImpactFilter_startValue :: Lens.Lens' TotalImpactFilter Prelude.Double
totalImpactFilter_startValue = Lens.lens (\TotalImpactFilter' {startValue} -> startValue) (\s@TotalImpactFilter' {} a -> s {startValue = a} :: TotalImpactFilter)

instance Prelude.Hashable TotalImpactFilter

instance Prelude.NFData TotalImpactFilter

instance Prelude.ToJSON TotalImpactFilter where
  toJSON TotalImpactFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("EndValue" Prelude..=) Prelude.<$> endValue,
            Prelude.Just
              ("NumericOperator" Prelude..= numericOperator),
            Prelude.Just ("StartValue" Prelude..= startValue)
          ]
      )
