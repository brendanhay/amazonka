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
-- Module      : Network.AWS.Connect.Types.Threshold
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.Threshold where

import Network.AWS.Connect.Types.Comparison
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about the threshold for service level metrics.
--
-- /See:/ 'newThreshold' smart constructor.
data Threshold = Threshold'
  { -- | The threshold value to compare.
    thresholdValue :: Core.Maybe Core.Double,
    -- | The type of comparison. Only \"less than\" (LT) comparisons are
    -- supported.
    comparison :: Core.Maybe Comparison
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Threshold' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thresholdValue', 'threshold_thresholdValue' - The threshold value to compare.
--
-- 'comparison', 'threshold_comparison' - The type of comparison. Only \"less than\" (LT) comparisons are
-- supported.
newThreshold ::
  Threshold
newThreshold =
  Threshold'
    { thresholdValue = Core.Nothing,
      comparison = Core.Nothing
    }

-- | The threshold value to compare.
threshold_thresholdValue :: Lens.Lens' Threshold (Core.Maybe Core.Double)
threshold_thresholdValue = Lens.lens (\Threshold' {thresholdValue} -> thresholdValue) (\s@Threshold' {} a -> s {thresholdValue = a} :: Threshold)

-- | The type of comparison. Only \"less than\" (LT) comparisons are
-- supported.
threshold_comparison :: Lens.Lens' Threshold (Core.Maybe Comparison)
threshold_comparison = Lens.lens (\Threshold' {comparison} -> comparison) (\s@Threshold' {} a -> s {comparison = a} :: Threshold)

instance Core.FromJSON Threshold where
  parseJSON =
    Core.withObject
      "Threshold"
      ( \x ->
          Threshold'
            Core.<$> (x Core..:? "ThresholdValue")
            Core.<*> (x Core..:? "Comparison")
      )

instance Core.Hashable Threshold

instance Core.NFData Threshold

instance Core.ToJSON Threshold where
  toJSON Threshold' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ThresholdValue" Core..=) Core.<$> thresholdValue,
            ("Comparison" Core..=) Core.<$> comparison
          ]
      )
