{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaConvert.Types.InputClipping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.InputClipping where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | To transcode only portions of your input (clips), include one Input
-- clipping (one instance of InputClipping in the JSON job file) for each
-- input clip. All input clips you specify will be included in every output
-- of the job.
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON InputClipping where
  parseJSON =
    Prelude.withObject
      "InputClipping"
      ( \x ->
          InputClipping'
            Prelude.<$> (x Prelude..:? "startTimecode")
            Prelude.<*> (x Prelude..:? "endTimecode")
      )

instance Prelude.Hashable InputClipping

instance Prelude.NFData InputClipping

instance Prelude.ToJSON InputClipping where
  toJSON InputClipping' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("startTimecode" Prelude..=)
              Prelude.<$> startTimecode,
            ("endTimecode" Prelude..=) Prelude.<$> endTimecode
          ]
      )
