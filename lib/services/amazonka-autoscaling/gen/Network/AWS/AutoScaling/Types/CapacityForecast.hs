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
-- Module      : Network.AWS.AutoScaling.Types.CapacityForecast
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.CapacityForecast where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A @GetPredictiveScalingForecast@ call returns the capacity forecast for
-- a predictive scaling policy. This structure includes the data points for
-- that capacity forecast, along with the timestamps of those data points.
--
-- /See:/ 'newCapacityForecast' smart constructor.
data CapacityForecast = CapacityForecast'
  { -- | The time stamps for the data points, in UTC format.
    timestamps :: [Core.ISO8601],
    -- | The values of the data points.
    values :: [Prelude.Double]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CapacityForecast' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timestamps', 'capacityForecast_timestamps' - The time stamps for the data points, in UTC format.
--
-- 'values', 'capacityForecast_values' - The values of the data points.
newCapacityForecast ::
  CapacityForecast
newCapacityForecast =
  CapacityForecast'
    { timestamps = Prelude.mempty,
      values = Prelude.mempty
    }

-- | The time stamps for the data points, in UTC format.
capacityForecast_timestamps :: Lens.Lens' CapacityForecast [Prelude.UTCTime]
capacityForecast_timestamps = Lens.lens (\CapacityForecast' {timestamps} -> timestamps) (\s@CapacityForecast' {} a -> s {timestamps = a} :: CapacityForecast) Prelude.. Lens._Coerce

-- | The values of the data points.
capacityForecast_values :: Lens.Lens' CapacityForecast [Prelude.Double]
capacityForecast_values = Lens.lens (\CapacityForecast' {values} -> values) (\s@CapacityForecast' {} a -> s {values = a} :: CapacityForecast) Prelude.. Lens._Coerce

instance Core.FromXML CapacityForecast where
  parseXML x =
    CapacityForecast'
      Prelude.<$> ( x Core..@? "Timestamps" Core..!@ Prelude.mempty
                      Prelude.>>= Core.parseXMLList "member"
                  )
      Prelude.<*> ( x Core..@? "Values" Core..!@ Prelude.mempty
                      Prelude.>>= Core.parseXMLList "member"
                  )

instance Prelude.Hashable CapacityForecast

instance Prelude.NFData CapacityForecast
