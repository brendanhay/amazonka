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
-- Module      : Amazonka.Connect.Types.ThresholdV2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.ThresholdV2 where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the threshold for service level metrics.
--
-- /See:/ 'newThresholdV2' smart constructor.
data ThresholdV2 = ThresholdV2'
  { -- | The type of comparison. Only \"less than\" (LT) comparisons are
    -- supported.
    comparison :: Prelude.Maybe Prelude.Text,
    -- | The threshold value to compare.
    thresholdValue :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThresholdV2' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comparison', 'thresholdV2_comparison' - The type of comparison. Only \"less than\" (LT) comparisons are
-- supported.
--
-- 'thresholdValue', 'thresholdV2_thresholdValue' - The threshold value to compare.
newThresholdV2 ::
  ThresholdV2
newThresholdV2 =
  ThresholdV2'
    { comparison = Prelude.Nothing,
      thresholdValue = Prelude.Nothing
    }

-- | The type of comparison. Only \"less than\" (LT) comparisons are
-- supported.
thresholdV2_comparison :: Lens.Lens' ThresholdV2 (Prelude.Maybe Prelude.Text)
thresholdV2_comparison = Lens.lens (\ThresholdV2' {comparison} -> comparison) (\s@ThresholdV2' {} a -> s {comparison = a} :: ThresholdV2)

-- | The threshold value to compare.
thresholdV2_thresholdValue :: Lens.Lens' ThresholdV2 (Prelude.Maybe Prelude.Double)
thresholdV2_thresholdValue = Lens.lens (\ThresholdV2' {thresholdValue} -> thresholdValue) (\s@ThresholdV2' {} a -> s {thresholdValue = a} :: ThresholdV2)

instance Data.FromJSON ThresholdV2 where
  parseJSON =
    Data.withObject
      "ThresholdV2"
      ( \x ->
          ThresholdV2'
            Prelude.<$> (x Data..:? "Comparison")
            Prelude.<*> (x Data..:? "ThresholdValue")
      )

instance Prelude.Hashable ThresholdV2 where
  hashWithSalt _salt ThresholdV2' {..} =
    _salt
      `Prelude.hashWithSalt` comparison
      `Prelude.hashWithSalt` thresholdValue

instance Prelude.NFData ThresholdV2 where
  rnf ThresholdV2' {..} =
    Prelude.rnf comparison
      `Prelude.seq` Prelude.rnf thresholdValue

instance Data.ToJSON ThresholdV2 where
  toJSON ThresholdV2' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Comparison" Data..=) Prelude.<$> comparison,
            ("ThresholdValue" Data..=)
              Prelude.<$> thresholdValue
          ]
      )
