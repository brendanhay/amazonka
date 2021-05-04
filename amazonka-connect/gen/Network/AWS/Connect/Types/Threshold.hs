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
-- Module      : Network.AWS.Connect.Types.Threshold
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.Threshold where

import Network.AWS.Connect.Types.Comparison
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the threshold for service level metrics.
--
-- /See:/ 'newThreshold' smart constructor.
data Threshold = Threshold'
  { -- | The threshold value to compare.
    thresholdValue :: Prelude.Maybe Prelude.Double,
    -- | The type of comparison. Only \"less than\" (LT) comparisons are
    -- supported.
    comparison :: Prelude.Maybe Comparison
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { thresholdValue = Prelude.Nothing,
      comparison = Prelude.Nothing
    }

-- | The threshold value to compare.
threshold_thresholdValue :: Lens.Lens' Threshold (Prelude.Maybe Prelude.Double)
threshold_thresholdValue = Lens.lens (\Threshold' {thresholdValue} -> thresholdValue) (\s@Threshold' {} a -> s {thresholdValue = a} :: Threshold)

-- | The type of comparison. Only \"less than\" (LT) comparisons are
-- supported.
threshold_comparison :: Lens.Lens' Threshold (Prelude.Maybe Comparison)
threshold_comparison = Lens.lens (\Threshold' {comparison} -> comparison) (\s@Threshold' {} a -> s {comparison = a} :: Threshold)

instance Prelude.FromJSON Threshold where
  parseJSON =
    Prelude.withObject
      "Threshold"
      ( \x ->
          Threshold'
            Prelude.<$> (x Prelude..:? "ThresholdValue")
            Prelude.<*> (x Prelude..:? "Comparison")
      )

instance Prelude.Hashable Threshold

instance Prelude.NFData Threshold

instance Prelude.ToJSON Threshold where
  toJSON Threshold' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ThresholdValue" Prelude..=)
              Prelude.<$> thresholdValue,
            ("Comparison" Prelude..=) Prelude.<$> comparison
          ]
      )
