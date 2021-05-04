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
-- Module      : Network.AWS.Rekognition.Types.SegmentDetection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.SegmentDetection where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.SegmentType
import Network.AWS.Rekognition.Types.ShotSegment
import Network.AWS.Rekognition.Types.TechnicalCueSegment

-- | A technical cue or shot detection segment detected in a video. An array
-- of @SegmentDetection@ objects containing all segments detected in a
-- stored video is returned by GetSegmentDetection.
--
-- /See:/ 'newSegmentDetection' smart constructor.
data SegmentDetection = SegmentDetection'
  { -- | If the segment is a shot detection, contains information about the shot
    -- detection.
    shotSegment :: Prelude.Maybe ShotSegment,
    -- | The end time of the detected segment, in milliseconds, from the start of
    -- the video. This value is rounded down.
    endTimestampMillis :: Prelude.Maybe Prelude.Integer,
    -- | The frame-accurate SMPTE timecode, from the start of a video, for the
    -- start of a detected segment. @StartTimecode@ is in /HH:MM:SS:fr/ format
    -- (and /;fr/ for drop frame-rates).
    startTimecodeSMPTE :: Prelude.Maybe Prelude.Text,
    -- | The duration of the timecode for the detected segment in SMPTE format.
    durationSMPTE :: Prelude.Maybe Prelude.Text,
    -- | If the segment is a technical cue, contains information about the
    -- technical cue.
    technicalCueSegment :: Prelude.Maybe TechnicalCueSegment,
    -- | The type of the segment. Valid values are @TECHNICAL_CUE@ and @SHOT@.
    type' :: Prelude.Maybe SegmentType,
    -- | The duration of the detected segment in milliseconds.
    durationMillis :: Prelude.Maybe Prelude.Natural,
    -- | The frame-accurate SMPTE timecode, from the start of a video, for the
    -- end of a detected segment. @EndTimecode@ is in /HH:MM:SS:fr/ format (and
    -- /;fr/ for drop frame-rates).
    endTimecodeSMPTE :: Prelude.Maybe Prelude.Text,
    -- | The start time of the detected segment in milliseconds from the start of
    -- the video. This value is rounded down. For example, if the actual
    -- timestamp is 100.6667 milliseconds, Amazon Rekognition Video returns a
    -- value of 100 millis.
    startTimestampMillis :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SegmentDetection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shotSegment', 'segmentDetection_shotSegment' - If the segment is a shot detection, contains information about the shot
-- detection.
--
-- 'endTimestampMillis', 'segmentDetection_endTimestampMillis' - The end time of the detected segment, in milliseconds, from the start of
-- the video. This value is rounded down.
--
-- 'startTimecodeSMPTE', 'segmentDetection_startTimecodeSMPTE' - The frame-accurate SMPTE timecode, from the start of a video, for the
-- start of a detected segment. @StartTimecode@ is in /HH:MM:SS:fr/ format
-- (and /;fr/ for drop frame-rates).
--
-- 'durationSMPTE', 'segmentDetection_durationSMPTE' - The duration of the timecode for the detected segment in SMPTE format.
--
-- 'technicalCueSegment', 'segmentDetection_technicalCueSegment' - If the segment is a technical cue, contains information about the
-- technical cue.
--
-- 'type'', 'segmentDetection_type' - The type of the segment. Valid values are @TECHNICAL_CUE@ and @SHOT@.
--
-- 'durationMillis', 'segmentDetection_durationMillis' - The duration of the detected segment in milliseconds.
--
-- 'endTimecodeSMPTE', 'segmentDetection_endTimecodeSMPTE' - The frame-accurate SMPTE timecode, from the start of a video, for the
-- end of a detected segment. @EndTimecode@ is in /HH:MM:SS:fr/ format (and
-- /;fr/ for drop frame-rates).
--
-- 'startTimestampMillis', 'segmentDetection_startTimestampMillis' - The start time of the detected segment in milliseconds from the start of
-- the video. This value is rounded down. For example, if the actual
-- timestamp is 100.6667 milliseconds, Amazon Rekognition Video returns a
-- value of 100 millis.
newSegmentDetection ::
  SegmentDetection
newSegmentDetection =
  SegmentDetection'
    { shotSegment = Prelude.Nothing,
      endTimestampMillis = Prelude.Nothing,
      startTimecodeSMPTE = Prelude.Nothing,
      durationSMPTE = Prelude.Nothing,
      technicalCueSegment = Prelude.Nothing,
      type' = Prelude.Nothing,
      durationMillis = Prelude.Nothing,
      endTimecodeSMPTE = Prelude.Nothing,
      startTimestampMillis = Prelude.Nothing
    }

-- | If the segment is a shot detection, contains information about the shot
-- detection.
segmentDetection_shotSegment :: Lens.Lens' SegmentDetection (Prelude.Maybe ShotSegment)
segmentDetection_shotSegment = Lens.lens (\SegmentDetection' {shotSegment} -> shotSegment) (\s@SegmentDetection' {} a -> s {shotSegment = a} :: SegmentDetection)

-- | The end time of the detected segment, in milliseconds, from the start of
-- the video. This value is rounded down.
segmentDetection_endTimestampMillis :: Lens.Lens' SegmentDetection (Prelude.Maybe Prelude.Integer)
segmentDetection_endTimestampMillis = Lens.lens (\SegmentDetection' {endTimestampMillis} -> endTimestampMillis) (\s@SegmentDetection' {} a -> s {endTimestampMillis = a} :: SegmentDetection)

-- | The frame-accurate SMPTE timecode, from the start of a video, for the
-- start of a detected segment. @StartTimecode@ is in /HH:MM:SS:fr/ format
-- (and /;fr/ for drop frame-rates).
segmentDetection_startTimecodeSMPTE :: Lens.Lens' SegmentDetection (Prelude.Maybe Prelude.Text)
segmentDetection_startTimecodeSMPTE = Lens.lens (\SegmentDetection' {startTimecodeSMPTE} -> startTimecodeSMPTE) (\s@SegmentDetection' {} a -> s {startTimecodeSMPTE = a} :: SegmentDetection)

-- | The duration of the timecode for the detected segment in SMPTE format.
segmentDetection_durationSMPTE :: Lens.Lens' SegmentDetection (Prelude.Maybe Prelude.Text)
segmentDetection_durationSMPTE = Lens.lens (\SegmentDetection' {durationSMPTE} -> durationSMPTE) (\s@SegmentDetection' {} a -> s {durationSMPTE = a} :: SegmentDetection)

-- | If the segment is a technical cue, contains information about the
-- technical cue.
segmentDetection_technicalCueSegment :: Lens.Lens' SegmentDetection (Prelude.Maybe TechnicalCueSegment)
segmentDetection_technicalCueSegment = Lens.lens (\SegmentDetection' {technicalCueSegment} -> technicalCueSegment) (\s@SegmentDetection' {} a -> s {technicalCueSegment = a} :: SegmentDetection)

-- | The type of the segment. Valid values are @TECHNICAL_CUE@ and @SHOT@.
segmentDetection_type :: Lens.Lens' SegmentDetection (Prelude.Maybe SegmentType)
segmentDetection_type = Lens.lens (\SegmentDetection' {type'} -> type') (\s@SegmentDetection' {} a -> s {type' = a} :: SegmentDetection)

-- | The duration of the detected segment in milliseconds.
segmentDetection_durationMillis :: Lens.Lens' SegmentDetection (Prelude.Maybe Prelude.Natural)
segmentDetection_durationMillis = Lens.lens (\SegmentDetection' {durationMillis} -> durationMillis) (\s@SegmentDetection' {} a -> s {durationMillis = a} :: SegmentDetection)

-- | The frame-accurate SMPTE timecode, from the start of a video, for the
-- end of a detected segment. @EndTimecode@ is in /HH:MM:SS:fr/ format (and
-- /;fr/ for drop frame-rates).
segmentDetection_endTimecodeSMPTE :: Lens.Lens' SegmentDetection (Prelude.Maybe Prelude.Text)
segmentDetection_endTimecodeSMPTE = Lens.lens (\SegmentDetection' {endTimecodeSMPTE} -> endTimecodeSMPTE) (\s@SegmentDetection' {} a -> s {endTimecodeSMPTE = a} :: SegmentDetection)

-- | The start time of the detected segment in milliseconds from the start of
-- the video. This value is rounded down. For example, if the actual
-- timestamp is 100.6667 milliseconds, Amazon Rekognition Video returns a
-- value of 100 millis.
segmentDetection_startTimestampMillis :: Lens.Lens' SegmentDetection (Prelude.Maybe Prelude.Integer)
segmentDetection_startTimestampMillis = Lens.lens (\SegmentDetection' {startTimestampMillis} -> startTimestampMillis) (\s@SegmentDetection' {} a -> s {startTimestampMillis = a} :: SegmentDetection)

instance Prelude.FromJSON SegmentDetection where
  parseJSON =
    Prelude.withObject
      "SegmentDetection"
      ( \x ->
          SegmentDetection'
            Prelude.<$> (x Prelude..:? "ShotSegment")
            Prelude.<*> (x Prelude..:? "EndTimestampMillis")
            Prelude.<*> (x Prelude..:? "StartTimecodeSMPTE")
            Prelude.<*> (x Prelude..:? "DurationSMPTE")
            Prelude.<*> (x Prelude..:? "TechnicalCueSegment")
            Prelude.<*> (x Prelude..:? "Type")
            Prelude.<*> (x Prelude..:? "DurationMillis")
            Prelude.<*> (x Prelude..:? "EndTimecodeSMPTE")
            Prelude.<*> (x Prelude..:? "StartTimestampMillis")
      )

instance Prelude.Hashable SegmentDetection

instance Prelude.NFData SegmentDetection
