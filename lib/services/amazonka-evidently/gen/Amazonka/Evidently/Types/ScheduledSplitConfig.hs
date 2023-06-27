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
-- Module      : Amazonka.Evidently.Types.ScheduledSplitConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.ScheduledSplitConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types.SegmentOverride
import qualified Amazonka.Prelude as Prelude

-- | This structure defines the traffic allocation percentages among the
-- feature variations during one step of a launch, and the start time of
-- that step.
--
-- /See:/ 'newScheduledSplitConfig' smart constructor.
data ScheduledSplitConfig = ScheduledSplitConfig'
  { -- | Use this parameter to specify different traffic splits for one or more
    -- audience /segments/. A segment is a portion of your audience that share
    -- one or more characteristics. Examples could be Chrome browser users,
    -- users in Europe, or Firefox browser users in Europe who also fit other
    -- criteria that your application collects, such as age.
    --
    -- This parameter is an array of up to six segment override objects. Each
    -- of these objects specifies a segment that you have already created, and
    -- defines the traffic split for that segment.
    segmentOverrides :: Prelude.Maybe [SegmentOverride],
    -- | The traffic allocation percentages among the feature variations during
    -- one step of a launch. This is a set of key-value pairs. The keys are
    -- variation names. The values represent the percentage of traffic to
    -- allocate to that variation during this step.
    --
    -- The values is expressed in thousandths of a percent, so assigning a
    -- weight of 50000 assigns 50% of traffic to that variation.
    --
    -- If the sum of the weights for all the variations in a segment override
    -- does not add up to 100,000, then the remaining traffic that matches this
    -- segment is not assigned by this segment override, and instead moves on
    -- to the next segment override or the default traffic split.
    groupWeights :: Prelude.HashMap Prelude.Text Prelude.Natural,
    -- | The date and time that this step of the launch starts.
    startTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduledSplitConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'segmentOverrides', 'scheduledSplitConfig_segmentOverrides' - Use this parameter to specify different traffic splits for one or more
-- audience /segments/. A segment is a portion of your audience that share
-- one or more characteristics. Examples could be Chrome browser users,
-- users in Europe, or Firefox browser users in Europe who also fit other
-- criteria that your application collects, such as age.
--
-- This parameter is an array of up to six segment override objects. Each
-- of these objects specifies a segment that you have already created, and
-- defines the traffic split for that segment.
--
-- 'groupWeights', 'scheduledSplitConfig_groupWeights' - The traffic allocation percentages among the feature variations during
-- one step of a launch. This is a set of key-value pairs. The keys are
-- variation names. The values represent the percentage of traffic to
-- allocate to that variation during this step.
--
-- The values is expressed in thousandths of a percent, so assigning a
-- weight of 50000 assigns 50% of traffic to that variation.
--
-- If the sum of the weights for all the variations in a segment override
-- does not add up to 100,000, then the remaining traffic that matches this
-- segment is not assigned by this segment override, and instead moves on
-- to the next segment override or the default traffic split.
--
-- 'startTime', 'scheduledSplitConfig_startTime' - The date and time that this step of the launch starts.
newScheduledSplitConfig ::
  -- | 'startTime'
  Prelude.UTCTime ->
  ScheduledSplitConfig
newScheduledSplitConfig pStartTime_ =
  ScheduledSplitConfig'
    { segmentOverrides =
        Prelude.Nothing,
      groupWeights = Prelude.mempty,
      startTime = Data._Time Lens.# pStartTime_
    }

-- | Use this parameter to specify different traffic splits for one or more
-- audience /segments/. A segment is a portion of your audience that share
-- one or more characteristics. Examples could be Chrome browser users,
-- users in Europe, or Firefox browser users in Europe who also fit other
-- criteria that your application collects, such as age.
--
-- This parameter is an array of up to six segment override objects. Each
-- of these objects specifies a segment that you have already created, and
-- defines the traffic split for that segment.
scheduledSplitConfig_segmentOverrides :: Lens.Lens' ScheduledSplitConfig (Prelude.Maybe [SegmentOverride])
scheduledSplitConfig_segmentOverrides = Lens.lens (\ScheduledSplitConfig' {segmentOverrides} -> segmentOverrides) (\s@ScheduledSplitConfig' {} a -> s {segmentOverrides = a} :: ScheduledSplitConfig) Prelude.. Lens.mapping Lens.coerced

-- | The traffic allocation percentages among the feature variations during
-- one step of a launch. This is a set of key-value pairs. The keys are
-- variation names. The values represent the percentage of traffic to
-- allocate to that variation during this step.
--
-- The values is expressed in thousandths of a percent, so assigning a
-- weight of 50000 assigns 50% of traffic to that variation.
--
-- If the sum of the weights for all the variations in a segment override
-- does not add up to 100,000, then the remaining traffic that matches this
-- segment is not assigned by this segment override, and instead moves on
-- to the next segment override or the default traffic split.
scheduledSplitConfig_groupWeights :: Lens.Lens' ScheduledSplitConfig (Prelude.HashMap Prelude.Text Prelude.Natural)
scheduledSplitConfig_groupWeights = Lens.lens (\ScheduledSplitConfig' {groupWeights} -> groupWeights) (\s@ScheduledSplitConfig' {} a -> s {groupWeights = a} :: ScheduledSplitConfig) Prelude.. Lens.coerced

-- | The date and time that this step of the launch starts.
scheduledSplitConfig_startTime :: Lens.Lens' ScheduledSplitConfig Prelude.UTCTime
scheduledSplitConfig_startTime = Lens.lens (\ScheduledSplitConfig' {startTime} -> startTime) (\s@ScheduledSplitConfig' {} a -> s {startTime = a} :: ScheduledSplitConfig) Prelude.. Data._Time

instance Prelude.Hashable ScheduledSplitConfig where
  hashWithSalt _salt ScheduledSplitConfig' {..} =
    _salt
      `Prelude.hashWithSalt` segmentOverrides
      `Prelude.hashWithSalt` groupWeights
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData ScheduledSplitConfig where
  rnf ScheduledSplitConfig' {..} =
    Prelude.rnf segmentOverrides
      `Prelude.seq` Prelude.rnf groupWeights
      `Prelude.seq` Prelude.rnf startTime

instance Data.ToJSON ScheduledSplitConfig where
  toJSON ScheduledSplitConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("segmentOverrides" Data..=)
              Prelude.<$> segmentOverrides,
            Prelude.Just ("groupWeights" Data..= groupWeights),
            Prelude.Just ("startTime" Data..= startTime)
          ]
      )
