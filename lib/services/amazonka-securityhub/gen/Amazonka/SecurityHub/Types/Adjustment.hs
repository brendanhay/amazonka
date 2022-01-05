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
-- Module      : Amazonka.SecurityHub.Types.Adjustment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.Adjustment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An adjustment to the CVSS metric.
--
-- /See:/ 'newAdjustment' smart constructor.
data Adjustment = Adjustment'
  { -- | The reason for the adjustment.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The metric to adjust.
    metric :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Adjustment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reason', 'adjustment_reason' - The reason for the adjustment.
--
-- 'metric', 'adjustment_metric' - The metric to adjust.
newAdjustment ::
  Adjustment
newAdjustment =
  Adjustment'
    { reason = Prelude.Nothing,
      metric = Prelude.Nothing
    }

-- | The reason for the adjustment.
adjustment_reason :: Lens.Lens' Adjustment (Prelude.Maybe Prelude.Text)
adjustment_reason = Lens.lens (\Adjustment' {reason} -> reason) (\s@Adjustment' {} a -> s {reason = a} :: Adjustment)

-- | The metric to adjust.
adjustment_metric :: Lens.Lens' Adjustment (Prelude.Maybe Prelude.Text)
adjustment_metric = Lens.lens (\Adjustment' {metric} -> metric) (\s@Adjustment' {} a -> s {metric = a} :: Adjustment)

instance Core.FromJSON Adjustment where
  parseJSON =
    Core.withObject
      "Adjustment"
      ( \x ->
          Adjustment'
            Prelude.<$> (x Core..:? "Reason")
            Prelude.<*> (x Core..:? "Metric")
      )

instance Prelude.Hashable Adjustment where
  hashWithSalt _salt Adjustment' {..} =
    _salt `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` metric

instance Prelude.NFData Adjustment where
  rnf Adjustment' {..} =
    Prelude.rnf reason `Prelude.seq` Prelude.rnf metric

instance Core.ToJSON Adjustment where
  toJSON Adjustment' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Reason" Core..=) Prelude.<$> reason,
            ("Metric" Core..=) Prelude.<$> metric
          ]
      )
