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
-- Module      : Amazonka.Connect.Types.Threshold
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.Threshold where

import Amazonka.Connect.Types.Comparison
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the threshold for service level metrics.
--
-- /See:/ 'newThreshold' smart constructor.
data Threshold = Threshold'
  { -- | The type of comparison. Only \"less than\" (LT) comparisons are
    -- supported.
    comparison :: Prelude.Maybe Comparison,
    -- | The threshold value to compare.
    thresholdValue :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Threshold' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comparison', 'threshold_comparison' - The type of comparison. Only \"less than\" (LT) comparisons are
-- supported.
--
-- 'thresholdValue', 'threshold_thresholdValue' - The threshold value to compare.
newThreshold ::
  Threshold
newThreshold =
  Threshold'
    { comparison = Prelude.Nothing,
      thresholdValue = Prelude.Nothing
    }

-- | The type of comparison. Only \"less than\" (LT) comparisons are
-- supported.
threshold_comparison :: Lens.Lens' Threshold (Prelude.Maybe Comparison)
threshold_comparison = Lens.lens (\Threshold' {comparison} -> comparison) (\s@Threshold' {} a -> s {comparison = a} :: Threshold)

-- | The threshold value to compare.
threshold_thresholdValue :: Lens.Lens' Threshold (Prelude.Maybe Prelude.Double)
threshold_thresholdValue = Lens.lens (\Threshold' {thresholdValue} -> thresholdValue) (\s@Threshold' {} a -> s {thresholdValue = a} :: Threshold)

instance Data.FromJSON Threshold where
  parseJSON =
    Data.withObject
      "Threshold"
      ( \x ->
          Threshold'
            Prelude.<$> (x Data..:? "Comparison")
            Prelude.<*> (x Data..:? "ThresholdValue")
      )

instance Prelude.Hashable Threshold where
  hashWithSalt _salt Threshold' {..} =
    _salt
      `Prelude.hashWithSalt` comparison
      `Prelude.hashWithSalt` thresholdValue

instance Prelude.NFData Threshold where
  rnf Threshold' {..} =
    Prelude.rnf comparison
      `Prelude.seq` Prelude.rnf thresholdValue

instance Data.ToJSON Threshold where
  toJSON Threshold' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Comparison" Data..=) Prelude.<$> comparison,
            ("ThresholdValue" Data..=)
              Prelude.<$> thresholdValue
          ]
      )
