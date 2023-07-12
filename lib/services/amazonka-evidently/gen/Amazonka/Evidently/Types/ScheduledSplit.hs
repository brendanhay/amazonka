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
-- Module      : Amazonka.Evidently.Types.ScheduledSplit
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.ScheduledSplit where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types.SegmentOverride
import qualified Amazonka.Prelude as Prelude

-- | This structure defines the traffic allocation percentages among the
-- feature variations during one step of a launch, and the start time of
-- that step.
--
-- /See:/ 'newScheduledSplit' smart constructor.
data ScheduledSplit = ScheduledSplit'
  { -- | The traffic allocation percentages among the feature variations during
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
    groupWeights :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Natural),
    -- | Use this parameter to specify different traffic splits for one or more
    -- audience /segments/. A segment is a portion of your audience that share
    -- one or more characteristics. Examples could be Chrome browser users,
    -- users in Europe, or Firefox browser users in Europe who also fit other
    -- criteria that your application collects, such as age.
    --
    -- This parameter is an array of up to six segment override objects. Each
    -- of these objects specifies a segment that you have already created, and
    -- defines the traffic split for that segment.
    segmentOverrides :: Prelude.Maybe [SegmentOverride],
    -- | The date and time that this step of the launch starts.
    startTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduledSplit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupWeights', 'scheduledSplit_groupWeights' - The traffic allocation percentages among the feature variations during
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
-- 'segmentOverrides', 'scheduledSplit_segmentOverrides' - Use this parameter to specify different traffic splits for one or more
-- audience /segments/. A segment is a portion of your audience that share
-- one or more characteristics. Examples could be Chrome browser users,
-- users in Europe, or Firefox browser users in Europe who also fit other
-- criteria that your application collects, such as age.
--
-- This parameter is an array of up to six segment override objects. Each
-- of these objects specifies a segment that you have already created, and
-- defines the traffic split for that segment.
--
-- 'startTime', 'scheduledSplit_startTime' - The date and time that this step of the launch starts.
newScheduledSplit ::
  -- | 'startTime'
  Prelude.UTCTime ->
  ScheduledSplit
newScheduledSplit pStartTime_ =
  ScheduledSplit'
    { groupWeights = Prelude.Nothing,
      segmentOverrides = Prelude.Nothing,
      startTime = Data._Time Lens.# pStartTime_
    }

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
scheduledSplit_groupWeights :: Lens.Lens' ScheduledSplit (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Natural))
scheduledSplit_groupWeights = Lens.lens (\ScheduledSplit' {groupWeights} -> groupWeights) (\s@ScheduledSplit' {} a -> s {groupWeights = a} :: ScheduledSplit) Prelude.. Lens.mapping Lens.coerced

-- | Use this parameter to specify different traffic splits for one or more
-- audience /segments/. A segment is a portion of your audience that share
-- one or more characteristics. Examples could be Chrome browser users,
-- users in Europe, or Firefox browser users in Europe who also fit other
-- criteria that your application collects, such as age.
--
-- This parameter is an array of up to six segment override objects. Each
-- of these objects specifies a segment that you have already created, and
-- defines the traffic split for that segment.
scheduledSplit_segmentOverrides :: Lens.Lens' ScheduledSplit (Prelude.Maybe [SegmentOverride])
scheduledSplit_segmentOverrides = Lens.lens (\ScheduledSplit' {segmentOverrides} -> segmentOverrides) (\s@ScheduledSplit' {} a -> s {segmentOverrides = a} :: ScheduledSplit) Prelude.. Lens.mapping Lens.coerced

-- | The date and time that this step of the launch starts.
scheduledSplit_startTime :: Lens.Lens' ScheduledSplit Prelude.UTCTime
scheduledSplit_startTime = Lens.lens (\ScheduledSplit' {startTime} -> startTime) (\s@ScheduledSplit' {} a -> s {startTime = a} :: ScheduledSplit) Prelude.. Data._Time

instance Data.FromJSON ScheduledSplit where
  parseJSON =
    Data.withObject
      "ScheduledSplit"
      ( \x ->
          ScheduledSplit'
            Prelude.<$> (x Data..:? "groupWeights" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "segmentOverrides"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "startTime")
      )

instance Prelude.Hashable ScheduledSplit where
  hashWithSalt _salt ScheduledSplit' {..} =
    _salt
      `Prelude.hashWithSalt` groupWeights
      `Prelude.hashWithSalt` segmentOverrides
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData ScheduledSplit where
  rnf ScheduledSplit' {..} =
    Prelude.rnf groupWeights
      `Prelude.seq` Prelude.rnf segmentOverrides
      `Prelude.seq` Prelude.rnf startTime
