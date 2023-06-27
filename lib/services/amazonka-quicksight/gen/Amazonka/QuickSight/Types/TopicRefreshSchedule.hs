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
-- Module      : Amazonka.QuickSight.Types.TopicRefreshSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TopicRefreshSchedule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TopicScheduleType

-- | A structure that represents a topic refresh schedule.
--
-- /See:/ 'newTopicRefreshSchedule' smart constructor.
data TopicRefreshSchedule = TopicRefreshSchedule'
  { -- | The time of day when the refresh should run, for example, Monday-Sunday.
    repeatAt :: Prelude.Maybe Prelude.Text,
    -- | The starting date and time for the refresh schedule.
    startingAt :: Prelude.Maybe Data.POSIX,
    -- | The timezone that you want the refresh schedule to use.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | The type of refresh schedule. Valid values for this structure are
    -- @HOURLY@, @DAILY@, @WEEKLY@, and @MONTHLY@.
    topicScheduleType :: Prelude.Maybe TopicScheduleType,
    -- | A Boolean value that controls whether to schedule is enabled.
    isEnabled :: Prelude.Bool,
    -- | A Boolean value that controls whether to schedule runs at the same
    -- schedule that is specified in SPICE dataset.
    basedOnSpiceSchedule :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TopicRefreshSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repeatAt', 'topicRefreshSchedule_repeatAt' - The time of day when the refresh should run, for example, Monday-Sunday.
--
-- 'startingAt', 'topicRefreshSchedule_startingAt' - The starting date and time for the refresh schedule.
--
-- 'timezone', 'topicRefreshSchedule_timezone' - The timezone that you want the refresh schedule to use.
--
-- 'topicScheduleType', 'topicRefreshSchedule_topicScheduleType' - The type of refresh schedule. Valid values for this structure are
-- @HOURLY@, @DAILY@, @WEEKLY@, and @MONTHLY@.
--
-- 'isEnabled', 'topicRefreshSchedule_isEnabled' - A Boolean value that controls whether to schedule is enabled.
--
-- 'basedOnSpiceSchedule', 'topicRefreshSchedule_basedOnSpiceSchedule' - A Boolean value that controls whether to schedule runs at the same
-- schedule that is specified in SPICE dataset.
newTopicRefreshSchedule ::
  -- | 'isEnabled'
  Prelude.Bool ->
  -- | 'basedOnSpiceSchedule'
  Prelude.Bool ->
  TopicRefreshSchedule
newTopicRefreshSchedule
  pIsEnabled_
  pBasedOnSpiceSchedule_ =
    TopicRefreshSchedule'
      { repeatAt = Prelude.Nothing,
        startingAt = Prelude.Nothing,
        timezone = Prelude.Nothing,
        topicScheduleType = Prelude.Nothing,
        isEnabled = pIsEnabled_,
        basedOnSpiceSchedule = pBasedOnSpiceSchedule_
      }

-- | The time of day when the refresh should run, for example, Monday-Sunday.
topicRefreshSchedule_repeatAt :: Lens.Lens' TopicRefreshSchedule (Prelude.Maybe Prelude.Text)
topicRefreshSchedule_repeatAt = Lens.lens (\TopicRefreshSchedule' {repeatAt} -> repeatAt) (\s@TopicRefreshSchedule' {} a -> s {repeatAt = a} :: TopicRefreshSchedule)

-- | The starting date and time for the refresh schedule.
topicRefreshSchedule_startingAt :: Lens.Lens' TopicRefreshSchedule (Prelude.Maybe Prelude.UTCTime)
topicRefreshSchedule_startingAt = Lens.lens (\TopicRefreshSchedule' {startingAt} -> startingAt) (\s@TopicRefreshSchedule' {} a -> s {startingAt = a} :: TopicRefreshSchedule) Prelude.. Lens.mapping Data._Time

-- | The timezone that you want the refresh schedule to use.
topicRefreshSchedule_timezone :: Lens.Lens' TopicRefreshSchedule (Prelude.Maybe Prelude.Text)
topicRefreshSchedule_timezone = Lens.lens (\TopicRefreshSchedule' {timezone} -> timezone) (\s@TopicRefreshSchedule' {} a -> s {timezone = a} :: TopicRefreshSchedule)

-- | The type of refresh schedule. Valid values for this structure are
-- @HOURLY@, @DAILY@, @WEEKLY@, and @MONTHLY@.
topicRefreshSchedule_topicScheduleType :: Lens.Lens' TopicRefreshSchedule (Prelude.Maybe TopicScheduleType)
topicRefreshSchedule_topicScheduleType = Lens.lens (\TopicRefreshSchedule' {topicScheduleType} -> topicScheduleType) (\s@TopicRefreshSchedule' {} a -> s {topicScheduleType = a} :: TopicRefreshSchedule)

-- | A Boolean value that controls whether to schedule is enabled.
topicRefreshSchedule_isEnabled :: Lens.Lens' TopicRefreshSchedule Prelude.Bool
topicRefreshSchedule_isEnabled = Lens.lens (\TopicRefreshSchedule' {isEnabled} -> isEnabled) (\s@TopicRefreshSchedule' {} a -> s {isEnabled = a} :: TopicRefreshSchedule)

-- | A Boolean value that controls whether to schedule runs at the same
-- schedule that is specified in SPICE dataset.
topicRefreshSchedule_basedOnSpiceSchedule :: Lens.Lens' TopicRefreshSchedule Prelude.Bool
topicRefreshSchedule_basedOnSpiceSchedule = Lens.lens (\TopicRefreshSchedule' {basedOnSpiceSchedule} -> basedOnSpiceSchedule) (\s@TopicRefreshSchedule' {} a -> s {basedOnSpiceSchedule = a} :: TopicRefreshSchedule)

instance Data.FromJSON TopicRefreshSchedule where
  parseJSON =
    Data.withObject
      "TopicRefreshSchedule"
      ( \x ->
          TopicRefreshSchedule'
            Prelude.<$> (x Data..:? "RepeatAt")
            Prelude.<*> (x Data..:? "StartingAt")
            Prelude.<*> (x Data..:? "Timezone")
            Prelude.<*> (x Data..:? "TopicScheduleType")
            Prelude.<*> (x Data..: "IsEnabled")
            Prelude.<*> (x Data..: "BasedOnSpiceSchedule")
      )

instance Prelude.Hashable TopicRefreshSchedule where
  hashWithSalt _salt TopicRefreshSchedule' {..} =
    _salt
      `Prelude.hashWithSalt` repeatAt
      `Prelude.hashWithSalt` startingAt
      `Prelude.hashWithSalt` timezone
      `Prelude.hashWithSalt` topicScheduleType
      `Prelude.hashWithSalt` isEnabled
      `Prelude.hashWithSalt` basedOnSpiceSchedule

instance Prelude.NFData TopicRefreshSchedule where
  rnf TopicRefreshSchedule' {..} =
    Prelude.rnf repeatAt
      `Prelude.seq` Prelude.rnf startingAt
      `Prelude.seq` Prelude.rnf timezone
      `Prelude.seq` Prelude.rnf topicScheduleType
      `Prelude.seq` Prelude.rnf isEnabled
      `Prelude.seq` Prelude.rnf basedOnSpiceSchedule

instance Data.ToJSON TopicRefreshSchedule where
  toJSON TopicRefreshSchedule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RepeatAt" Data..=) Prelude.<$> repeatAt,
            ("StartingAt" Data..=) Prelude.<$> startingAt,
            ("Timezone" Data..=) Prelude.<$> timezone,
            ("TopicScheduleType" Data..=)
              Prelude.<$> topicScheduleType,
            Prelude.Just ("IsEnabled" Data..= isEnabled),
            Prelude.Just
              ( "BasedOnSpiceSchedule"
                  Data..= basedOnSpiceSchedule
              )
          ]
      )
