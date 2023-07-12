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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.ScheduleEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types.ScheduleAdBreak
import Amazonka.MediaTailor.Types.ScheduleEntryType
import qualified Amazonka.Prelude as Prelude

-- | The properties for a schedule.
--
-- /See:/ 'newScheduleEntry' smart constructor.
data ScheduleEntry = ScheduleEntry'
  { -- | The approximate duration of this program, in seconds.
    approximateDurationSeconds :: Prelude.Maybe Prelude.Integer,
    -- | The approximate time that the program will start playing.
    approximateStartTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the live source used for the program.
    liveSourceName :: Prelude.Maybe Prelude.Text,
    -- | The schedule\'s ad break properties.
    scheduleAdBreaks :: Prelude.Maybe [ScheduleAdBreak],
    -- | The type of schedule entry.
    scheduleEntryType :: Prelude.Maybe ScheduleEntryType,
    -- | The name of the VOD source.
    vodSourceName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the program.
    arn :: Prelude.Text,
    -- | The name of the channel that uses this schedule.
    channelName :: Prelude.Text,
    -- | The name of the program.
    programName :: Prelude.Text,
    -- | The name of the source location.
    sourceLocationName :: Prelude.Text
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
-- 'approximateDurationSeconds', 'scheduleEntry_approximateDurationSeconds' - The approximate duration of this program, in seconds.
--
-- 'approximateStartTime', 'scheduleEntry_approximateStartTime' - The approximate time that the program will start playing.
--
-- 'liveSourceName', 'scheduleEntry_liveSourceName' - The name of the live source used for the program.
--
-- 'scheduleAdBreaks', 'scheduleEntry_scheduleAdBreaks' - The schedule\'s ad break properties.
--
-- 'scheduleEntryType', 'scheduleEntry_scheduleEntryType' - The type of schedule entry.
--
-- 'vodSourceName', 'scheduleEntry_vodSourceName' - The name of the VOD source.
--
-- 'arn', 'scheduleEntry_arn' - The ARN of the program.
--
-- 'channelName', 'scheduleEntry_channelName' - The name of the channel that uses this schedule.
--
-- 'programName', 'scheduleEntry_programName' - The name of the program.
--
-- 'sourceLocationName', 'scheduleEntry_sourceLocationName' - The name of the source location.
newScheduleEntry ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'channelName'
  Prelude.Text ->
  -- | 'programName'
  Prelude.Text ->
  -- | 'sourceLocationName'
  Prelude.Text ->
  ScheduleEntry
newScheduleEntry
  pArn_
  pChannelName_
  pProgramName_
  pSourceLocationName_ =
    ScheduleEntry'
      { approximateDurationSeconds =
          Prelude.Nothing,
        approximateStartTime = Prelude.Nothing,
        liveSourceName = Prelude.Nothing,
        scheduleAdBreaks = Prelude.Nothing,
        scheduleEntryType = Prelude.Nothing,
        vodSourceName = Prelude.Nothing,
        arn = pArn_,
        channelName = pChannelName_,
        programName = pProgramName_,
        sourceLocationName = pSourceLocationName_
      }

-- | The approximate duration of this program, in seconds.
scheduleEntry_approximateDurationSeconds :: Lens.Lens' ScheduleEntry (Prelude.Maybe Prelude.Integer)
scheduleEntry_approximateDurationSeconds = Lens.lens (\ScheduleEntry' {approximateDurationSeconds} -> approximateDurationSeconds) (\s@ScheduleEntry' {} a -> s {approximateDurationSeconds = a} :: ScheduleEntry)

-- | The approximate time that the program will start playing.
scheduleEntry_approximateStartTime :: Lens.Lens' ScheduleEntry (Prelude.Maybe Prelude.UTCTime)
scheduleEntry_approximateStartTime = Lens.lens (\ScheduleEntry' {approximateStartTime} -> approximateStartTime) (\s@ScheduleEntry' {} a -> s {approximateStartTime = a} :: ScheduleEntry) Prelude.. Lens.mapping Data._Time

-- | The name of the live source used for the program.
scheduleEntry_liveSourceName :: Lens.Lens' ScheduleEntry (Prelude.Maybe Prelude.Text)
scheduleEntry_liveSourceName = Lens.lens (\ScheduleEntry' {liveSourceName} -> liveSourceName) (\s@ScheduleEntry' {} a -> s {liveSourceName = a} :: ScheduleEntry)

-- | The schedule\'s ad break properties.
scheduleEntry_scheduleAdBreaks :: Lens.Lens' ScheduleEntry (Prelude.Maybe [ScheduleAdBreak])
scheduleEntry_scheduleAdBreaks = Lens.lens (\ScheduleEntry' {scheduleAdBreaks} -> scheduleAdBreaks) (\s@ScheduleEntry' {} a -> s {scheduleAdBreaks = a} :: ScheduleEntry) Prelude.. Lens.mapping Lens.coerced

-- | The type of schedule entry.
scheduleEntry_scheduleEntryType :: Lens.Lens' ScheduleEntry (Prelude.Maybe ScheduleEntryType)
scheduleEntry_scheduleEntryType = Lens.lens (\ScheduleEntry' {scheduleEntryType} -> scheduleEntryType) (\s@ScheduleEntry' {} a -> s {scheduleEntryType = a} :: ScheduleEntry)

-- | The name of the VOD source.
scheduleEntry_vodSourceName :: Lens.Lens' ScheduleEntry (Prelude.Maybe Prelude.Text)
scheduleEntry_vodSourceName = Lens.lens (\ScheduleEntry' {vodSourceName} -> vodSourceName) (\s@ScheduleEntry' {} a -> s {vodSourceName = a} :: ScheduleEntry)

-- | The ARN of the program.
scheduleEntry_arn :: Lens.Lens' ScheduleEntry Prelude.Text
scheduleEntry_arn = Lens.lens (\ScheduleEntry' {arn} -> arn) (\s@ScheduleEntry' {} a -> s {arn = a} :: ScheduleEntry)

-- | The name of the channel that uses this schedule.
scheduleEntry_channelName :: Lens.Lens' ScheduleEntry Prelude.Text
scheduleEntry_channelName = Lens.lens (\ScheduleEntry' {channelName} -> channelName) (\s@ScheduleEntry' {} a -> s {channelName = a} :: ScheduleEntry)

-- | The name of the program.
scheduleEntry_programName :: Lens.Lens' ScheduleEntry Prelude.Text
scheduleEntry_programName = Lens.lens (\ScheduleEntry' {programName} -> programName) (\s@ScheduleEntry' {} a -> s {programName = a} :: ScheduleEntry)

-- | The name of the source location.
scheduleEntry_sourceLocationName :: Lens.Lens' ScheduleEntry Prelude.Text
scheduleEntry_sourceLocationName = Lens.lens (\ScheduleEntry' {sourceLocationName} -> sourceLocationName) (\s@ScheduleEntry' {} a -> s {sourceLocationName = a} :: ScheduleEntry)

instance Data.FromJSON ScheduleEntry where
  parseJSON =
    Data.withObject
      "ScheduleEntry"
      ( \x ->
          ScheduleEntry'
            Prelude.<$> (x Data..:? "ApproximateDurationSeconds")
            Prelude.<*> (x Data..:? "ApproximateStartTime")
            Prelude.<*> (x Data..:? "LiveSourceName")
            Prelude.<*> ( x
                            Data..:? "ScheduleAdBreaks"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ScheduleEntryType")
            Prelude.<*> (x Data..:? "VodSourceName")
            Prelude.<*> (x Data..: "Arn")
            Prelude.<*> (x Data..: "ChannelName")
            Prelude.<*> (x Data..: "ProgramName")
            Prelude.<*> (x Data..: "SourceLocationName")
      )

instance Prelude.Hashable ScheduleEntry where
  hashWithSalt _salt ScheduleEntry' {..} =
    _salt
      `Prelude.hashWithSalt` approximateDurationSeconds
      `Prelude.hashWithSalt` approximateStartTime
      `Prelude.hashWithSalt` liveSourceName
      `Prelude.hashWithSalt` scheduleAdBreaks
      `Prelude.hashWithSalt` scheduleEntryType
      `Prelude.hashWithSalt` vodSourceName
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` channelName
      `Prelude.hashWithSalt` programName
      `Prelude.hashWithSalt` sourceLocationName

instance Prelude.NFData ScheduleEntry where
  rnf ScheduleEntry' {..} =
    Prelude.rnf approximateDurationSeconds
      `Prelude.seq` Prelude.rnf approximateStartTime
      `Prelude.seq` Prelude.rnf liveSourceName
      `Prelude.seq` Prelude.rnf scheduleAdBreaks
      `Prelude.seq` Prelude.rnf scheduleEntryType
      `Prelude.seq` Prelude.rnf vodSourceName
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf programName
      `Prelude.seq` Prelude.rnf sourceLocationName
