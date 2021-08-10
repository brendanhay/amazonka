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
-- Module      : Network.AWS.MachineLearning.Types.PerformanceMetrics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.PerformanceMetrics where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Measurements of how well the @MLModel@ performed on known observations.
-- One of the following metrics is returned, based on the type of the
-- @MLModel@:
--
-- -   BinaryAUC: The binary @MLModel@ uses the Area Under the Curve (AUC)
--     technique to measure performance.
--
-- -   RegressionRMSE: The regression @MLModel@ uses the Root Mean Square
--     Error (RMSE) technique to measure performance. RMSE measures the
--     difference between predicted and actual values for a single
--     variable.
--
-- -   MulticlassAvgFScore: The multiclass @MLModel@ uses the F1 score
--     technique to measure performance.
--
-- For more information about performance metrics, please see the
-- <http://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide>.
--
-- /See:/ 'newPerformanceMetrics' smart constructor.
data PerformanceMetrics = PerformanceMetrics'
  { properties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PerformanceMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'properties', 'performanceMetrics_properties' - Undocumented member.
newPerformanceMetrics ::
  PerformanceMetrics
newPerformanceMetrics =
  PerformanceMetrics' {properties = Prelude.Nothing}

-- | Undocumented member.
performanceMetrics_properties :: Lens.Lens' PerformanceMetrics (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
performanceMetrics_properties = Lens.lens (\PerformanceMetrics' {properties} -> properties) (\s@PerformanceMetrics' {} a -> s {properties = a} :: PerformanceMetrics) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON PerformanceMetrics where
  parseJSON =
    Core.withObject
      "PerformanceMetrics"
      ( \x ->
          PerformanceMetrics'
            Prelude.<$> (x Core..:? "Properties" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable PerformanceMetrics

instance Prelude.NFData PerformanceMetrics
