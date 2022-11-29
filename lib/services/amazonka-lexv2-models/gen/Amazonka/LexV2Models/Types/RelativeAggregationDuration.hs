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
-- Module      : Amazonka.LexV2Models.Types.RelativeAggregationDuration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.RelativeAggregationDuration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types.TimeDimension
import qualified Amazonka.Prelude as Prelude

-- | Specifies the time window that utterance statistics are returned for.
-- The time window is always relative to the last time that the that
-- utterances were aggregated. For example, if the
-- @ListAggregatedUtterances@ operation is called at 1600, the time window
-- is set to 1 hour, and the last refresh time was 1530, only utterances
-- made between 1430 and 1530 are returned.
--
-- You can choose the time window that statistics should be returned for.
--
-- -   __Hours__ - You can request utterance statistics for 1, 3, 6, 12, or
--     24 hour time windows. Statistics are refreshed every half hour for 1
--     hour time windows, and hourly for the other time windows.
--
-- -   __Days__ - You can request utterance statistics for 3 days.
--     Statistics are refreshed every 6 hours.
--
-- -   __Weeks__ - You can see statistics for one or two weeks. Statistics
--     are refreshed every 12 hours for one week time windows, and once per
--     day for two week time windows.
--
-- /See:/ 'newRelativeAggregationDuration' smart constructor.
data RelativeAggregationDuration = RelativeAggregationDuration'
  { -- | The type of time period that the @timeValue@ field represents.
    timeDimension :: TimeDimension,
    -- | The period of the time window to gather statistics for. The valid value
    -- depends on the setting of the @timeDimension@ field.
    --
    -- -   @Hours@ - 1\/3\/6\/12\/24
    --
    -- -   @Days@ - 3
    --
    -- -   @Weeks@ - 1\/2
    timeValue :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RelativeAggregationDuration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeDimension', 'relativeAggregationDuration_timeDimension' - The type of time period that the @timeValue@ field represents.
--
-- 'timeValue', 'relativeAggregationDuration_timeValue' - The period of the time window to gather statistics for. The valid value
-- depends on the setting of the @timeDimension@ field.
--
-- -   @Hours@ - 1\/3\/6\/12\/24
--
-- -   @Days@ - 3
--
-- -   @Weeks@ - 1\/2
newRelativeAggregationDuration ::
  -- | 'timeDimension'
  TimeDimension ->
  -- | 'timeValue'
  Prelude.Natural ->
  RelativeAggregationDuration
newRelativeAggregationDuration
  pTimeDimension_
  pTimeValue_ =
    RelativeAggregationDuration'
      { timeDimension =
          pTimeDimension_,
        timeValue = pTimeValue_
      }

-- | The type of time period that the @timeValue@ field represents.
relativeAggregationDuration_timeDimension :: Lens.Lens' RelativeAggregationDuration TimeDimension
relativeAggregationDuration_timeDimension = Lens.lens (\RelativeAggregationDuration' {timeDimension} -> timeDimension) (\s@RelativeAggregationDuration' {} a -> s {timeDimension = a} :: RelativeAggregationDuration)

-- | The period of the time window to gather statistics for. The valid value
-- depends on the setting of the @timeDimension@ field.
--
-- -   @Hours@ - 1\/3\/6\/12\/24
--
-- -   @Days@ - 3
--
-- -   @Weeks@ - 1\/2
relativeAggregationDuration_timeValue :: Lens.Lens' RelativeAggregationDuration Prelude.Natural
relativeAggregationDuration_timeValue = Lens.lens (\RelativeAggregationDuration' {timeValue} -> timeValue) (\s@RelativeAggregationDuration' {} a -> s {timeValue = a} :: RelativeAggregationDuration)

instance Core.FromJSON RelativeAggregationDuration where
  parseJSON =
    Core.withObject
      "RelativeAggregationDuration"
      ( \x ->
          RelativeAggregationDuration'
            Prelude.<$> (x Core..: "timeDimension")
            Prelude.<*> (x Core..: "timeValue")
      )

instance Prelude.Hashable RelativeAggregationDuration where
  hashWithSalt _salt RelativeAggregationDuration' {..} =
    _salt `Prelude.hashWithSalt` timeDimension
      `Prelude.hashWithSalt` timeValue

instance Prelude.NFData RelativeAggregationDuration where
  rnf RelativeAggregationDuration' {..} =
    Prelude.rnf timeDimension
      `Prelude.seq` Prelude.rnf timeValue

instance Core.ToJSON RelativeAggregationDuration where
  toJSON RelativeAggregationDuration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("timeDimension" Core..= timeDimension),
            Prelude.Just ("timeValue" Core..= timeValue)
          ]
      )
