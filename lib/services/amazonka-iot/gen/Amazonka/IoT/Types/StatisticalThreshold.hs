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
-- Module      : Amazonka.IoT.Types.StatisticalThreshold
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.StatisticalThreshold where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A statistical ranking (percentile) that indicates a threshold value by
-- which a behavior is determined to be in compliance or in violation of
-- the behavior.
--
-- /See:/ 'newStatisticalThreshold' smart constructor.
data StatisticalThreshold = StatisticalThreshold'
  { -- | The percentile that resolves to a threshold value by which compliance
    -- with a behavior is determined. Metrics are collected over the specified
    -- period (@durationSeconds@) from all reporting devices in your account
    -- and statistical ranks are calculated. Then, the measurements from a
    -- device are collected over the same period. If the accumulated
    -- measurements from the device fall above or below (@comparisonOperator@)
    -- the value associated with the percentile specified, then the device is
    -- considered to be in compliance with the behavior, otherwise a violation
    -- occurs.
    statistic :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StatisticalThreshold' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statistic', 'statisticalThreshold_statistic' - The percentile that resolves to a threshold value by which compliance
-- with a behavior is determined. Metrics are collected over the specified
-- period (@durationSeconds@) from all reporting devices in your account
-- and statistical ranks are calculated. Then, the measurements from a
-- device are collected over the same period. If the accumulated
-- measurements from the device fall above or below (@comparisonOperator@)
-- the value associated with the percentile specified, then the device is
-- considered to be in compliance with the behavior, otherwise a violation
-- occurs.
newStatisticalThreshold ::
  StatisticalThreshold
newStatisticalThreshold =
  StatisticalThreshold' {statistic = Prelude.Nothing}

-- | The percentile that resolves to a threshold value by which compliance
-- with a behavior is determined. Metrics are collected over the specified
-- period (@durationSeconds@) from all reporting devices in your account
-- and statistical ranks are calculated. Then, the measurements from a
-- device are collected over the same period. If the accumulated
-- measurements from the device fall above or below (@comparisonOperator@)
-- the value associated with the percentile specified, then the device is
-- considered to be in compliance with the behavior, otherwise a violation
-- occurs.
statisticalThreshold_statistic :: Lens.Lens' StatisticalThreshold (Prelude.Maybe Prelude.Text)
statisticalThreshold_statistic = Lens.lens (\StatisticalThreshold' {statistic} -> statistic) (\s@StatisticalThreshold' {} a -> s {statistic = a} :: StatisticalThreshold)

instance Data.FromJSON StatisticalThreshold where
  parseJSON =
    Data.withObject
      "StatisticalThreshold"
      ( \x ->
          StatisticalThreshold'
            Prelude.<$> (x Data..:? "statistic")
      )

instance Prelude.Hashable StatisticalThreshold where
  hashWithSalt _salt StatisticalThreshold' {..} =
    _salt `Prelude.hashWithSalt` statistic

instance Prelude.NFData StatisticalThreshold where
  rnf StatisticalThreshold' {..} = Prelude.rnf statistic

instance Data.ToJSON StatisticalThreshold where
  toJSON StatisticalThreshold' {..} =
    Data.object
      ( Prelude.catMaybes
          [("statistic" Data..=) Prelude.<$> statistic]
      )
