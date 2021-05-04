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
-- Module      : Network.AWS.MediaConvert.Types.TimecodeConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.TimecodeConfig where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.TimecodeSource
import qualified Network.AWS.Prelude as Prelude

-- | These settings control how the service handles timecodes throughout the
-- job. These settings don\'t affect input clipping.
--
-- /See:/ 'newTimecodeConfig' smart constructor.
data TimecodeConfig = TimecodeConfig'
  { -- | If you use an editing platform that relies on an anchor timecode, use
    -- Anchor Timecode (Anchor) to specify a timecode that will match the input
    -- video frame to the output video frame. Use 24-hour format with frame
    -- number, (HH:MM:SS:FF) or (HH:MM:SS;FF). This setting ignores frame rate
    -- conversion. System behavior for Anchor Timecode varies depending on your
    -- setting for Source (TimecodeSource). * If Source (TimecodeSource) is set
    -- to Specified Start (SPECIFIEDSTART), the first input frame is the
    -- specified value in Start Timecode (Start). Anchor Timecode (Anchor) and
    -- Start Timecode (Start) are used calculate output timecode. * If Source
    -- (TimecodeSource) is set to Start at 0 (ZEROBASED) the first frame is
    -- 00:00:00:00. * If Source (TimecodeSource) is set to Embedded (EMBEDDED),
    -- the first frame is the timecode value on the first input frame of the
    -- input.
    anchor :: Prelude.Maybe Prelude.Text,
    -- | Use Source (TimecodeSource) to set how timecodes are handled within this
    -- job. To make sure that your video, audio, captions, and markers are
    -- synchronized and that time-based features, such as image inserter, work
    -- correctly, choose the Timecode source option that matches your assets.
    -- All timecodes are in a 24-hour format with frame number (HH:MM:SS:FF). *
    -- Embedded (EMBEDDED) - Use the timecode that is in the input video. If no
    -- embedded timecode is in the source, the service will use Start at 0
    -- (ZEROBASED) instead. * Start at 0 (ZEROBASED) - Set the timecode of the
    -- initial frame to 00:00:00:00. * Specified Start (SPECIFIEDSTART) - Set
    -- the timecode of the initial frame to a value other than zero. You use
    -- Start timecode (Start) to provide this value.
    source :: Prelude.Maybe TimecodeSource,
    -- | Only applies to outputs that support program-date-time stamp. Use
    -- Timestamp offset (TimestampOffset) to overwrite the timecode date
    -- without affecting the time and frame number. Provide the new date as a
    -- string in the format \"yyyy-mm-dd\". To use Time stamp offset, you must
    -- also enable Insert program-date-time (InsertProgramDateTime) in the
    -- output settings. For example, if the date part of your timecodes is
    -- 2002-1-25 and you want to change it to one year later, set Timestamp
    -- offset (TimestampOffset) to 2003-1-25.
    timestampOffset :: Prelude.Maybe Prelude.Text,
    -- | Only use when you set Source (TimecodeSource) to Specified start
    -- (SPECIFIEDSTART). Use Start timecode (Start) to specify the timecode for
    -- the initial frame. Use 24-hour format with frame number, (HH:MM:SS:FF)
    -- or (HH:MM:SS;FF).
    start :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TimecodeConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'anchor', 'timecodeConfig_anchor' - If you use an editing platform that relies on an anchor timecode, use
-- Anchor Timecode (Anchor) to specify a timecode that will match the input
-- video frame to the output video frame. Use 24-hour format with frame
-- number, (HH:MM:SS:FF) or (HH:MM:SS;FF). This setting ignores frame rate
-- conversion. System behavior for Anchor Timecode varies depending on your
-- setting for Source (TimecodeSource). * If Source (TimecodeSource) is set
-- to Specified Start (SPECIFIEDSTART), the first input frame is the
-- specified value in Start Timecode (Start). Anchor Timecode (Anchor) and
-- Start Timecode (Start) are used calculate output timecode. * If Source
-- (TimecodeSource) is set to Start at 0 (ZEROBASED) the first frame is
-- 00:00:00:00. * If Source (TimecodeSource) is set to Embedded (EMBEDDED),
-- the first frame is the timecode value on the first input frame of the
-- input.
--
-- 'source', 'timecodeConfig_source' - Use Source (TimecodeSource) to set how timecodes are handled within this
-- job. To make sure that your video, audio, captions, and markers are
-- synchronized and that time-based features, such as image inserter, work
-- correctly, choose the Timecode source option that matches your assets.
-- All timecodes are in a 24-hour format with frame number (HH:MM:SS:FF). *
-- Embedded (EMBEDDED) - Use the timecode that is in the input video. If no
-- embedded timecode is in the source, the service will use Start at 0
-- (ZEROBASED) instead. * Start at 0 (ZEROBASED) - Set the timecode of the
-- initial frame to 00:00:00:00. * Specified Start (SPECIFIEDSTART) - Set
-- the timecode of the initial frame to a value other than zero. You use
-- Start timecode (Start) to provide this value.
--
-- 'timestampOffset', 'timecodeConfig_timestampOffset' - Only applies to outputs that support program-date-time stamp. Use
-- Timestamp offset (TimestampOffset) to overwrite the timecode date
-- without affecting the time and frame number. Provide the new date as a
-- string in the format \"yyyy-mm-dd\". To use Time stamp offset, you must
-- also enable Insert program-date-time (InsertProgramDateTime) in the
-- output settings. For example, if the date part of your timecodes is
-- 2002-1-25 and you want to change it to one year later, set Timestamp
-- offset (TimestampOffset) to 2003-1-25.
--
-- 'start', 'timecodeConfig_start' - Only use when you set Source (TimecodeSource) to Specified start
-- (SPECIFIEDSTART). Use Start timecode (Start) to specify the timecode for
-- the initial frame. Use 24-hour format with frame number, (HH:MM:SS:FF)
-- or (HH:MM:SS;FF).
newTimecodeConfig ::
  TimecodeConfig
newTimecodeConfig =
  TimecodeConfig'
    { anchor = Prelude.Nothing,
      source = Prelude.Nothing,
      timestampOffset = Prelude.Nothing,
      start = Prelude.Nothing
    }

-- | If you use an editing platform that relies on an anchor timecode, use
-- Anchor Timecode (Anchor) to specify a timecode that will match the input
-- video frame to the output video frame. Use 24-hour format with frame
-- number, (HH:MM:SS:FF) or (HH:MM:SS;FF). This setting ignores frame rate
-- conversion. System behavior for Anchor Timecode varies depending on your
-- setting for Source (TimecodeSource). * If Source (TimecodeSource) is set
-- to Specified Start (SPECIFIEDSTART), the first input frame is the
-- specified value in Start Timecode (Start). Anchor Timecode (Anchor) and
-- Start Timecode (Start) are used calculate output timecode. * If Source
-- (TimecodeSource) is set to Start at 0 (ZEROBASED) the first frame is
-- 00:00:00:00. * If Source (TimecodeSource) is set to Embedded (EMBEDDED),
-- the first frame is the timecode value on the first input frame of the
-- input.
timecodeConfig_anchor :: Lens.Lens' TimecodeConfig (Prelude.Maybe Prelude.Text)
timecodeConfig_anchor = Lens.lens (\TimecodeConfig' {anchor} -> anchor) (\s@TimecodeConfig' {} a -> s {anchor = a} :: TimecodeConfig)

-- | Use Source (TimecodeSource) to set how timecodes are handled within this
-- job. To make sure that your video, audio, captions, and markers are
-- synchronized and that time-based features, such as image inserter, work
-- correctly, choose the Timecode source option that matches your assets.
-- All timecodes are in a 24-hour format with frame number (HH:MM:SS:FF). *
-- Embedded (EMBEDDED) - Use the timecode that is in the input video. If no
-- embedded timecode is in the source, the service will use Start at 0
-- (ZEROBASED) instead. * Start at 0 (ZEROBASED) - Set the timecode of the
-- initial frame to 00:00:00:00. * Specified Start (SPECIFIEDSTART) - Set
-- the timecode of the initial frame to a value other than zero. You use
-- Start timecode (Start) to provide this value.
timecodeConfig_source :: Lens.Lens' TimecodeConfig (Prelude.Maybe TimecodeSource)
timecodeConfig_source = Lens.lens (\TimecodeConfig' {source} -> source) (\s@TimecodeConfig' {} a -> s {source = a} :: TimecodeConfig)

-- | Only applies to outputs that support program-date-time stamp. Use
-- Timestamp offset (TimestampOffset) to overwrite the timecode date
-- without affecting the time and frame number. Provide the new date as a
-- string in the format \"yyyy-mm-dd\". To use Time stamp offset, you must
-- also enable Insert program-date-time (InsertProgramDateTime) in the
-- output settings. For example, if the date part of your timecodes is
-- 2002-1-25 and you want to change it to one year later, set Timestamp
-- offset (TimestampOffset) to 2003-1-25.
timecodeConfig_timestampOffset :: Lens.Lens' TimecodeConfig (Prelude.Maybe Prelude.Text)
timecodeConfig_timestampOffset = Lens.lens (\TimecodeConfig' {timestampOffset} -> timestampOffset) (\s@TimecodeConfig' {} a -> s {timestampOffset = a} :: TimecodeConfig)

-- | Only use when you set Source (TimecodeSource) to Specified start
-- (SPECIFIEDSTART). Use Start timecode (Start) to specify the timecode for
-- the initial frame. Use 24-hour format with frame number, (HH:MM:SS:FF)
-- or (HH:MM:SS;FF).
timecodeConfig_start :: Lens.Lens' TimecodeConfig (Prelude.Maybe Prelude.Text)
timecodeConfig_start = Lens.lens (\TimecodeConfig' {start} -> start) (\s@TimecodeConfig' {} a -> s {start = a} :: TimecodeConfig)

instance Prelude.FromJSON TimecodeConfig where
  parseJSON =
    Prelude.withObject
      "TimecodeConfig"
      ( \x ->
          TimecodeConfig'
            Prelude.<$> (x Prelude..:? "anchor")
            Prelude.<*> (x Prelude..:? "source")
            Prelude.<*> (x Prelude..:? "timestampOffset")
            Prelude.<*> (x Prelude..:? "start")
      )

instance Prelude.Hashable TimecodeConfig

instance Prelude.NFData TimecodeConfig

instance Prelude.ToJSON TimecodeConfig where
  toJSON TimecodeConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("anchor" Prelude..=) Prelude.<$> anchor,
            ("source" Prelude..=) Prelude.<$> source,
            ("timestampOffset" Prelude..=)
              Prelude.<$> timestampOffset,
            ("start" Prelude..=) Prelude.<$> start
          ]
      )
