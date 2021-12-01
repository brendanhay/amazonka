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
-- Module      : Amazonka.ElasticTranscoder.Types.TimeSpan
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticTranscoder.Types.TimeSpan where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Settings that determine when a clip begins and how long it lasts.
--
-- /See:/ 'newTimeSpan' smart constructor.
data TimeSpan = TimeSpan'
  { -- | The place in the input file where you want a clip to start. The format
    -- can be either HH:mm:ss.SSS (maximum value: 23:59:59.999; SSS is
    -- thousandths of a second) or sssss.SSS (maximum value: 86399.999). If you
    -- don\'t specify a value, Elastic Transcoder starts at the beginning of
    -- the input file.
    startTime :: Prelude.Maybe Prelude.Text,
    -- | The duration of the clip. The format can be either HH:mm:ss.SSS (maximum
    -- value: 23:59:59.999; SSS is thousandths of a second) or sssss.SSS
    -- (maximum value: 86399.999). If you don\'t specify a value, Elastic
    -- Transcoder creates an output file from StartTime to the end of the file.
    --
    -- If you specify a value longer than the duration of the input file,
    -- Elastic Transcoder transcodes the file and returns a warning message.
    duration :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeSpan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTime', 'timeSpan_startTime' - The place in the input file where you want a clip to start. The format
-- can be either HH:mm:ss.SSS (maximum value: 23:59:59.999; SSS is
-- thousandths of a second) or sssss.SSS (maximum value: 86399.999). If you
-- don\'t specify a value, Elastic Transcoder starts at the beginning of
-- the input file.
--
-- 'duration', 'timeSpan_duration' - The duration of the clip. The format can be either HH:mm:ss.SSS (maximum
-- value: 23:59:59.999; SSS is thousandths of a second) or sssss.SSS
-- (maximum value: 86399.999). If you don\'t specify a value, Elastic
-- Transcoder creates an output file from StartTime to the end of the file.
--
-- If you specify a value longer than the duration of the input file,
-- Elastic Transcoder transcodes the file and returns a warning message.
newTimeSpan ::
  TimeSpan
newTimeSpan =
  TimeSpan'
    { startTime = Prelude.Nothing,
      duration = Prelude.Nothing
    }

-- | The place in the input file where you want a clip to start. The format
-- can be either HH:mm:ss.SSS (maximum value: 23:59:59.999; SSS is
-- thousandths of a second) or sssss.SSS (maximum value: 86399.999). If you
-- don\'t specify a value, Elastic Transcoder starts at the beginning of
-- the input file.
timeSpan_startTime :: Lens.Lens' TimeSpan (Prelude.Maybe Prelude.Text)
timeSpan_startTime = Lens.lens (\TimeSpan' {startTime} -> startTime) (\s@TimeSpan' {} a -> s {startTime = a} :: TimeSpan)

-- | The duration of the clip. The format can be either HH:mm:ss.SSS (maximum
-- value: 23:59:59.999; SSS is thousandths of a second) or sssss.SSS
-- (maximum value: 86399.999). If you don\'t specify a value, Elastic
-- Transcoder creates an output file from StartTime to the end of the file.
--
-- If you specify a value longer than the duration of the input file,
-- Elastic Transcoder transcodes the file and returns a warning message.
timeSpan_duration :: Lens.Lens' TimeSpan (Prelude.Maybe Prelude.Text)
timeSpan_duration = Lens.lens (\TimeSpan' {duration} -> duration) (\s@TimeSpan' {} a -> s {duration = a} :: TimeSpan)

instance Core.FromJSON TimeSpan where
  parseJSON =
    Core.withObject
      "TimeSpan"
      ( \x ->
          TimeSpan'
            Prelude.<$> (x Core..:? "StartTime")
            Prelude.<*> (x Core..:? "Duration")
      )

instance Prelude.Hashable TimeSpan where
  hashWithSalt salt' TimeSpan' {..} =
    salt' `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData TimeSpan where
  rnf TimeSpan' {..} =
    Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf duration

instance Core.ToJSON TimeSpan where
  toJSON TimeSpan' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("StartTime" Core..=) Prelude.<$> startTime,
            ("Duration" Core..=) Prelude.<$> duration
          ]
      )
