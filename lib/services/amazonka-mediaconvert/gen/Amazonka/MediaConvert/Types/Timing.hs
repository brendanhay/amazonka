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
-- Module      : Amazonka.MediaConvert.Types.Timing
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Timing where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about when jobs are submitted, started, and finished is
-- specified in Unix epoch format in seconds.
--
-- /See:/ 'newTiming' smart constructor.
data Timing = Timing'
  { -- | The time, in Unix epoch format, that the transcoding job finished
    finishTime :: Prelude.Maybe Data.POSIX,
    -- | The time, in Unix epoch format, that transcoding for the job began.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The time, in Unix epoch format, that you submitted the job.
    submitTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Timing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'finishTime', 'timing_finishTime' - The time, in Unix epoch format, that the transcoding job finished
--
-- 'startTime', 'timing_startTime' - The time, in Unix epoch format, that transcoding for the job began.
--
-- 'submitTime', 'timing_submitTime' - The time, in Unix epoch format, that you submitted the job.
newTiming ::
  Timing
newTiming =
  Timing'
    { finishTime = Prelude.Nothing,
      startTime = Prelude.Nothing,
      submitTime = Prelude.Nothing
    }

-- | The time, in Unix epoch format, that the transcoding job finished
timing_finishTime :: Lens.Lens' Timing (Prelude.Maybe Prelude.UTCTime)
timing_finishTime = Lens.lens (\Timing' {finishTime} -> finishTime) (\s@Timing' {} a -> s {finishTime = a} :: Timing) Prelude.. Lens.mapping Data._Time

-- | The time, in Unix epoch format, that transcoding for the job began.
timing_startTime :: Lens.Lens' Timing (Prelude.Maybe Prelude.UTCTime)
timing_startTime = Lens.lens (\Timing' {startTime} -> startTime) (\s@Timing' {} a -> s {startTime = a} :: Timing) Prelude.. Lens.mapping Data._Time

-- | The time, in Unix epoch format, that you submitted the job.
timing_submitTime :: Lens.Lens' Timing (Prelude.Maybe Prelude.UTCTime)
timing_submitTime = Lens.lens (\Timing' {submitTime} -> submitTime) (\s@Timing' {} a -> s {submitTime = a} :: Timing) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON Timing where
  parseJSON =
    Data.withObject
      "Timing"
      ( \x ->
          Timing'
            Prelude.<$> (x Data..:? "finishTime")
            Prelude.<*> (x Data..:? "startTime")
            Prelude.<*> (x Data..:? "submitTime")
      )

instance Prelude.Hashable Timing where
  hashWithSalt _salt Timing' {..} =
    _salt
      `Prelude.hashWithSalt` finishTime
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` submitTime

instance Prelude.NFData Timing where
  rnf Timing' {..} =
    Prelude.rnf finishTime
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf submitTime
