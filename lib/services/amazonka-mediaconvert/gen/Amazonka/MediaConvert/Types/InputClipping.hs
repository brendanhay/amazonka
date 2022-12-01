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
-- Module      : Amazonka.MediaConvert.Types.InputClipping
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.InputClipping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | To transcode only portions of your input, include one input clip for
-- each part of your input that you want in your output. All input clips
-- that you specify will be included in every output of the job. For more
-- information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/assembling-multiple-inputs-and-input-clips.html.
--
-- /See:/ 'newInputClipping' smart constructor.
data InputClipping = InputClipping'
  { -- | Set Start timecode (StartTimecode) to the beginning of the portion of
    -- the input you are clipping. The frame corresponding to the Start
    -- timecode value is included in the clip. Start timecode or End timecode
    -- may be left blank, but not both. Use the format HH:MM:SS:FF or
    -- HH:MM:SS;FF, where HH is the hour, MM is the minute, SS is the second,
    -- and FF is the frame number. When choosing this value, take into account
    -- your setting for Input timecode source. For example, if you have
    -- embedded timecodes that start at 01:00:00:00 and you want your clip to
    -- begin five minutes into the video, use 01:05:00:00.
    startTimecode :: Prelude.Maybe Prelude.Text,
    -- | Set End timecode (EndTimecode) to the end of the portion of the input
    -- you are clipping. The frame corresponding to the End timecode value is
    -- included in the clip. Start timecode or End timecode may be left blank,
    -- but not both. Use the format HH:MM:SS:FF or HH:MM:SS;FF, where HH is the
    -- hour, MM is the minute, SS is the second, and FF is the frame number.
    -- When choosing this value, take into account your setting for timecode
    -- source under input settings (InputTimecodeSource). For example, if you
    -- have embedded timecodes that start at 01:00:00:00 and you want your clip
    -- to end six minutes into the video, use 01:06:00:00.
    endTimecode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputClipping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTimecode', 'inputClipping_startTimecode' - Set Start timecode (StartTimecode) to the beginning of the portion of
-- the input you are clipping. The frame corresponding to the Start
-- timecode value is included in the clip. Start timecode or End timecode
-- may be left blank, but not both. Use the format HH:MM:SS:FF or
-- HH:MM:SS;FF, where HH is the hour, MM is the minute, SS is the second,
-- and FF is the frame number. When choosing this value, take into account
-- your setting for Input timecode source. For example, if you have
-- embedded timecodes that start at 01:00:00:00 and you want your clip to
-- begin five minutes into the video, use 01:05:00:00.
--
-- 'endTimecode', 'inputClipping_endTimecode' - Set End timecode (EndTimecode) to the end of the portion of the input
-- you are clipping. The frame corresponding to the End timecode value is
-- included in the clip. Start timecode or End timecode may be left blank,
-- but not both. Use the format HH:MM:SS:FF or HH:MM:SS;FF, where HH is the
-- hour, MM is the minute, SS is the second, and FF is the frame number.
-- When choosing this value, take into account your setting for timecode
-- source under input settings (InputTimecodeSource). For example, if you
-- have embedded timecodes that start at 01:00:00:00 and you want your clip
-- to end six minutes into the video, use 01:06:00:00.
newInputClipping ::
  InputClipping
newInputClipping =
  InputClipping'
    { startTimecode = Prelude.Nothing,
      endTimecode = Prelude.Nothing
    }

-- | Set Start timecode (StartTimecode) to the beginning of the portion of
-- the input you are clipping. The frame corresponding to the Start
-- timecode value is included in the clip. Start timecode or End timecode
-- may be left blank, but not both. Use the format HH:MM:SS:FF or
-- HH:MM:SS;FF, where HH is the hour, MM is the minute, SS is the second,
-- and FF is the frame number. When choosing this value, take into account
-- your setting for Input timecode source. For example, if you have
-- embedded timecodes that start at 01:00:00:00 and you want your clip to
-- begin five minutes into the video, use 01:05:00:00.
inputClipping_startTimecode :: Lens.Lens' InputClipping (Prelude.Maybe Prelude.Text)
inputClipping_startTimecode = Lens.lens (\InputClipping' {startTimecode} -> startTimecode) (\s@InputClipping' {} a -> s {startTimecode = a} :: InputClipping)

-- | Set End timecode (EndTimecode) to the end of the portion of the input
-- you are clipping. The frame corresponding to the End timecode value is
-- included in the clip. Start timecode or End timecode may be left blank,
-- but not both. Use the format HH:MM:SS:FF or HH:MM:SS;FF, where HH is the
-- hour, MM is the minute, SS is the second, and FF is the frame number.
-- When choosing this value, take into account your setting for timecode
-- source under input settings (InputTimecodeSource). For example, if you
-- have embedded timecodes that start at 01:00:00:00 and you want your clip
-- to end six minutes into the video, use 01:06:00:00.
inputClipping_endTimecode :: Lens.Lens' InputClipping (Prelude.Maybe Prelude.Text)
inputClipping_endTimecode = Lens.lens (\InputClipping' {endTimecode} -> endTimecode) (\s@InputClipping' {} a -> s {endTimecode = a} :: InputClipping)

instance Core.FromJSON InputClipping where
  parseJSON =
    Core.withObject
      "InputClipping"
      ( \x ->
          InputClipping'
            Prelude.<$> (x Core..:? "startTimecode")
            Prelude.<*> (x Core..:? "endTimecode")
      )

instance Prelude.Hashable InputClipping where
  hashWithSalt _salt InputClipping' {..} =
    _salt `Prelude.hashWithSalt` startTimecode
      `Prelude.hashWithSalt` endTimecode

instance Prelude.NFData InputClipping where
  rnf InputClipping' {..} =
    Prelude.rnf startTimecode
      `Prelude.seq` Prelude.rnf endTimecode

instance Core.ToJSON InputClipping where
  toJSON InputClipping' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("startTimecode" Core..=) Prelude.<$> startTimecode,
            ("endTimecode" Core..=) Prelude.<$> endTimecode
          ]
      )
