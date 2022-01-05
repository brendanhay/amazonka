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
-- Module      : Amazonka.MediaTailor.Types.ScheduleEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.ScheduleEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaTailor.Types.ScheduleAdBreak
import Amazonka.MediaTailor.Types.ScheduleEntryType
import qualified Amazonka.Prelude as Prelude

-- | The properties for a schedule.
--
-- /See:/ 'newScheduleEntry' smart constructor.
data ScheduleEntry = ScheduleEntry'
  { -- | The schedule\'s ad break properties.
    scheduleAdBreaks :: Prelude.Maybe [ScheduleAdBreak],
    -- | The approximate duration of this program, in seconds.
    approximateDurationSeconds :: Prelude.Maybe Prelude.Integer,
    -- | The approximate time that the program will start playing.
    approximateStartTime :: Prelude.Maybe Core.POSIX,
    -- | The type of schedule entry.
    --
    -- Valid values: PROGRAM or FILLER_SLATE.
    scheduleEntryType :: Prelude.Maybe ScheduleEntryType,
    -- | The name of the VOD source.
    vodSourceName :: Prelude.Text,
    -- | The name of the channel that uses this schedule.
    channelName :: Prelude.Text,
    -- | The name of the source location.
    sourceLocationName :: Prelude.Text,
    -- | The ARN of the program.
    arn :: Prelude.Text,
    -- | The name of the program.
    programName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduleEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduleAdBreaks', 'scheduleEntry_scheduleAdBreaks' - The schedule\'s ad break properties.
--
-- 'approximateDurationSeconds', 'scheduleEntry_approximateDurationSeconds' - The approximate duration of this program, in seconds.
--
-- 'approximateStartTime', 'scheduleEntry_approximateStartTime' - The approximate time that the program will start playing.
--
-- 'scheduleEntryType', 'scheduleEntry_scheduleEntryType' - The type of schedule entry.
--
-- Valid values: PROGRAM or FILLER_SLATE.
--
-- 'vodSourceName', 'scheduleEntry_vodSourceName' - The name of the VOD source.
--
-- 'channelName', 'scheduleEntry_channelName' - The name of the channel that uses this schedule.
--
-- 'sourceLocationName', 'scheduleEntry_sourceLocationName' - The name of the source location.
--
-- 'arn', 'scheduleEntry_arn' - The ARN of the program.
--
-- 'programName', 'scheduleEntry_programName' - The name of the program.
newScheduleEntry ::
  -- | 'vodSourceName'
  Prelude.Text ->
  -- | 'channelName'
  Prelude.Text ->
  -- | 'sourceLocationName'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'programName'
  Prelude.Text ->
  ScheduleEntry
newScheduleEntry
  pVodSourceName_
  pChannelName_
  pSourceLocationName_
  pArn_
  pProgramName_ =
    ScheduleEntry'
      { scheduleAdBreaks = Prelude.Nothing,
        approximateDurationSeconds = Prelude.Nothing,
        approximateStartTime = Prelude.Nothing,
        scheduleEntryType = Prelude.Nothing,
        vodSourceName = pVodSourceName_,
        channelName = pChannelName_,
        sourceLocationName = pSourceLocationName_,
        arn = pArn_,
        programName = pProgramName_
      }

-- | The schedule\'s ad break properties.
scheduleEntry_scheduleAdBreaks :: Lens.Lens' ScheduleEntry (Prelude.Maybe [ScheduleAdBreak])
scheduleEntry_scheduleAdBreaks = Lens.lens (\ScheduleEntry' {scheduleAdBreaks} -> scheduleAdBreaks) (\s@ScheduleEntry' {} a -> s {scheduleAdBreaks = a} :: ScheduleEntry) Prelude.. Lens.mapping Lens.coerced

-- | The approximate duration of this program, in seconds.
scheduleEntry_approximateDurationSeconds :: Lens.Lens' ScheduleEntry (Prelude.Maybe Prelude.Integer)
scheduleEntry_approximateDurationSeconds = Lens.lens (\ScheduleEntry' {approximateDurationSeconds} -> approximateDurationSeconds) (\s@ScheduleEntry' {} a -> s {approximateDurationSeconds = a} :: ScheduleEntry)

-- | The approximate time that the program will start playing.
scheduleEntry_approximateStartTime :: Lens.Lens' ScheduleEntry (Prelude.Maybe Prelude.UTCTime)
scheduleEntry_approximateStartTime = Lens.lens (\ScheduleEntry' {approximateStartTime} -> approximateStartTime) (\s@ScheduleEntry' {} a -> s {approximateStartTime = a} :: ScheduleEntry) Prelude.. Lens.mapping Core._Time

-- | The type of schedule entry.
--
-- Valid values: PROGRAM or FILLER_SLATE.
scheduleEntry_scheduleEntryType :: Lens.Lens' ScheduleEntry (Prelude.Maybe ScheduleEntryType)
scheduleEntry_scheduleEntryType = Lens.lens (\ScheduleEntry' {scheduleEntryType} -> scheduleEntryType) (\s@ScheduleEntry' {} a -> s {scheduleEntryType = a} :: ScheduleEntry)

-- | The name of the VOD source.
scheduleEntry_vodSourceName :: Lens.Lens' ScheduleEntry Prelude.Text
scheduleEntry_vodSourceName = Lens.lens (\ScheduleEntry' {vodSourceName} -> vodSourceName) (\s@ScheduleEntry' {} a -> s {vodSourceName = a} :: ScheduleEntry)

-- | The name of the channel that uses this schedule.
scheduleEntry_channelName :: Lens.Lens' ScheduleEntry Prelude.Text
scheduleEntry_channelName = Lens.lens (\ScheduleEntry' {channelName} -> channelName) (\s@ScheduleEntry' {} a -> s {channelName = a} :: ScheduleEntry)

-- | The name of the source location.
scheduleEntry_sourceLocationName :: Lens.Lens' ScheduleEntry Prelude.Text
scheduleEntry_sourceLocationName = Lens.lens (\ScheduleEntry' {sourceLocationName} -> sourceLocationName) (\s@ScheduleEntry' {} a -> s {sourceLocationName = a} :: ScheduleEntry)

-- | The ARN of the program.
scheduleEntry_arn :: Lens.Lens' ScheduleEntry Prelude.Text
scheduleEntry_arn = Lens.lens (\ScheduleEntry' {arn} -> arn) (\s@ScheduleEntry' {} a -> s {arn = a} :: ScheduleEntry)

-- | The name of the program.
scheduleEntry_programName :: Lens.Lens' ScheduleEntry Prelude.Text
scheduleEntry_programName = Lens.lens (\ScheduleEntry' {programName} -> programName) (\s@ScheduleEntry' {} a -> s {programName = a} :: ScheduleEntry)

instance Core.FromJSON ScheduleEntry where
  parseJSON =
    Core.withObject
      "ScheduleEntry"
      ( \x ->
          ScheduleEntry'
            Prelude.<$> ( x Core..:? "ScheduleAdBreaks"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ApproximateDurationSeconds")
            Prelude.<*> (x Core..:? "ApproximateStartTime")
            Prelude.<*> (x Core..:? "ScheduleEntryType")
            Prelude.<*> (x Core..: "VodSourceName")
            Prelude.<*> (x Core..: "ChannelName")
            Prelude.<*> (x Core..: "SourceLocationName")
            Prelude.<*> (x Core..: "Arn")
            Prelude.<*> (x Core..: "ProgramName")
      )

instance Prelude.Hashable ScheduleEntry where
  hashWithSalt _salt ScheduleEntry' {..} =
    _salt `Prelude.hashWithSalt` scheduleAdBreaks
      `Prelude.hashWithSalt` approximateDurationSeconds
      `Prelude.hashWithSalt` approximateStartTime
      `Prelude.hashWithSalt` scheduleEntryType
      `Prelude.hashWithSalt` vodSourceName
      `Prelude.hashWithSalt` channelName
      `Prelude.hashWithSalt` sourceLocationName
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` programName

instance Prelude.NFData ScheduleEntry where
  rnf ScheduleEntry' {..} =
    Prelude.rnf scheduleAdBreaks
      `Prelude.seq` Prelude.rnf approximateDurationSeconds
      `Prelude.seq` Prelude.rnf approximateStartTime
      `Prelude.seq` Prelude.rnf scheduleEntryType
      `Prelude.seq` Prelude.rnf vodSourceName
      `Prelude.seq` Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf sourceLocationName
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf programName
