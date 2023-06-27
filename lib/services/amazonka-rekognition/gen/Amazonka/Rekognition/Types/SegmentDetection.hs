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
-- Module      : Amazonka.Rekognition.Types.SegmentDetection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.SegmentDetection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.SegmentType
import Amazonka.Rekognition.Types.ShotSegment
import Amazonka.Rekognition.Types.TechnicalCueSegment

-- | A technical cue or shot detection segment detected in a video. An array
-- of @SegmentDetection@ objects containing all segments detected in a
-- stored video is returned by GetSegmentDetection.
--
-- /See:/ 'newSegmentDetection' smart constructor.
data SegmentDetection = SegmentDetection'
  { -- | The duration of a video segment, expressed in frames.
    durationFrames :: Prelude.Maybe Prelude.Natural,
    -- | The duration of the detected segment in milliseconds.
    durationMillis :: Prelude.Maybe Prelude.Natural,
    -- | The duration of the timecode for the detected segment in SMPTE format.
    durationSMPTE :: Prelude.Maybe Prelude.Text,
    -- | The frame number at the end of a video segment, using a frame index that
    -- starts with 0.
    endFrameNumber :: Prelude.Maybe Prelude.Natural,
    -- | The frame-accurate SMPTE timecode, from the start of a video, for the
    -- end of a detected segment. @EndTimecode@ is in /HH:MM:SS:fr/ format (and
    -- /;fr/ for drop frame-rates).
    endTimecodeSMPTE :: Prelude.Maybe Prelude.Text,
    -- | The end time of the detected segment, in milliseconds, from the start of
    -- the video. This value is rounded down.
    endTimestampMillis :: Prelude.Maybe Prelude.Integer,
    -- | If the segment is a shot detection, contains information about the shot
    -- detection.
    shotSegment :: Prelude.Maybe ShotSegment,
    -- | The frame number of the start of a video segment, using a frame index
    -- that starts with 0.
    startFrameNumber :: Prelude.Maybe Prelude.Natural,
    -- | The frame-accurate SMPTE timecode, from the start of a video, for the
    -- start of a detected segment. @StartTimecode@ is in /HH:MM:SS:fr/ format
    -- (and /;fr/ for drop frame-rates).
    startTimecodeSMPTE :: Prelude.Maybe Prelude.Text,
    -- | The start time of the detected segment in milliseconds from the start of
    -- the video. This value is rounded down. For example, if the actual
    -- timestamp is 100.6667 milliseconds, Amazon Rekognition Video returns a
    -- value of 100 millis.
    startTimestampMillis :: Prelude.Maybe Prelude.Integer,
    -- | If the segment is a technical cue, contains information about the
    -- technical cue.
    technicalCueSegment :: Prelude.Maybe TechnicalCueSegment,
    -- | The type of the segment. Valid values are @TECHNICAL_CUE@ and @SHOT@.
    type' :: Prelude.Maybe SegmentType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SegmentDetection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'durationFrames', 'segmentDetection_durationFrames' - The duration of a video segment, expressed in frames.
--
-- 'durationMillis', 'segmentDetection_durationMillis' - The duration of the detected segment in milliseconds.
--
-- 'durationSMPTE', 'segmentDetection_durationSMPTE' - The duration of the timecode for the detected segment in SMPTE format.
--
-- 'endFrameNumber', 'segmentDetection_endFrameNumber' - The frame number at the end of a video segment, using a frame index that
-- starts with 0.
--
-- 'endTimecodeSMPTE', 'segmentDetection_endTimecodeSMPTE' - The frame-accurate SMPTE timecode, from the start of a video, for the
-- end of a detected segment. @EndTimecode@ is in /HH:MM:SS:fr/ format (and
-- /;fr/ for drop frame-rates).
--
-- 'endTimestampMillis', 'segmentDetection_endTimestampMillis' - The end time of the detected segment, in milliseconds, from the start of
-- the video. This value is rounded down.
--
-- 'shotSegment', 'segmentDetection_shotSegment' - If the segment is a shot detection, contains information about the shot
-- detection.
--
-- 'startFrameNumber', 'segmentDetection_startFrameNumber' - The frame number of the start of a video segment, using a frame index
-- that starts with 0.
--
-- 'startTimecodeSMPTE', 'segmentDetection_startTimecodeSMPTE' - The frame-accurate SMPTE timecode, from the start of a video, for the
-- start of a detected segment. @StartTimecode@ is in /HH:MM:SS:fr/ format
-- (and /;fr/ for drop frame-rates).
--
-- 'startTimestampMillis', 'segmentDetection_startTimestampMillis' - The start time of the detected segment in milliseconds from the start of
-- the video. This value is rounded down. For example, if the actual
-- timestamp is 100.6667 milliseconds, Amazon Rekognition Video returns a
-- value of 100 millis.
--
-- 'technicalCueSegment', 'segmentDetection_technicalCueSegment' - If the segment is a technical cue, contains information about the
-- technical cue.
--
-- 'type'', 'segmentDetection_type' - The type of the segment. Valid values are @TECHNICAL_CUE@ and @SHOT@.
newSegmentDetection ::
  SegmentDetection
newSegmentDetection =
  SegmentDetection'
    { durationFrames = Prelude.Nothing,
      durationMillis = Prelude.Nothing,
      durationSMPTE = Prelude.Nothing,
      endFrameNumber = Prelude.Nothing,
      endTimecodeSMPTE = Prelude.Nothing,
      endTimestampMillis = Prelude.Nothing,
      shotSegment = Prelude.Nothing,
      startFrameNumber = Prelude.Nothing,
      startTimecodeSMPTE = Prelude.Nothing,
      startTimestampMillis = Prelude.Nothing,
      technicalCueSegment = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The duration of a video segment, expressed in frames.
segmentDetection_durationFrames :: Lens.Lens' SegmentDetection (Prelude.Maybe Prelude.Natural)
segmentDetection_durationFrames = Lens.lens (\SegmentDetection' {durationFrames} -> durationFrames) (\s@SegmentDetection' {} a -> s {durationFrames = a} :: SegmentDetection)

-- | The duration of the detected segment in milliseconds.
segmentDetection_durationMillis :: Lens.Lens' SegmentDetection (Prelude.Maybe Prelude.Natural)
segmentDetection_durationMillis = Lens.lens (\SegmentDetection' {durationMillis} -> durationMillis) (\s@SegmentDetection' {} a -> s {durationMillis = a} :: SegmentDetection)

-- | The duration of the timecode for the detected segment in SMPTE format.
segmentDetection_durationSMPTE :: Lens.Lens' SegmentDetection (Prelude.Maybe Prelude.Text)
segmentDetection_durationSMPTE = Lens.lens (\SegmentDetection' {durationSMPTE} -> durationSMPTE) (\s@SegmentDetection' {} a -> s {durationSMPTE = a} :: SegmentDetection)

-- | The frame number at the end of a video segment, using a frame index that
-- starts with 0.
segmentDetection_endFrameNumber :: Lens.Lens' SegmentDetection (Prelude.Maybe Prelude.Natural)
segmentDetection_endFrameNumber = Lens.lens (\SegmentDetection' {endFrameNumber} -> endFrameNumber) (\s@SegmentDetection' {} a -> s {endFrameNumber = a} :: SegmentDetection)

-- | The frame-accurate SMPTE timecode, from the start of a video, for the
-- end of a detected segment. @EndTimecode@ is in /HH:MM:SS:fr/ format (and
-- /;fr/ for drop frame-rates).
segmentDetection_endTimecodeSMPTE :: Lens.Lens' SegmentDetection (Prelude.Maybe Prelude.Text)
segmentDetection_endTimecodeSMPTE = Lens.lens (\SegmentDetection' {endTimecodeSMPTE} -> endTimecodeSMPTE) (\s@SegmentDetection' {} a -> s {endTimecodeSMPTE = a} :: SegmentDetection)

-- | The end time of the detected segment, in milliseconds, from the start of
-- the video. This value is rounded down.
segmentDetection_endTimestampMillis :: Lens.Lens' SegmentDetection (Prelude.Maybe Prelude.Integer)
segmentDetection_endTimestampMillis = Lens.lens (\SegmentDetection' {endTimestampMillis} -> endTimestampMillis) (\s@SegmentDetection' {} a -> s {endTimestampMillis = a} :: SegmentDetection)

-- | If the segment is a shot detection, contains information about the shot
-- detection.
segmentDetection_shotSegment :: Lens.Lens' SegmentDetection (Prelude.Maybe ShotSegment)
segmentDetection_shotSegment = Lens.lens (\SegmentDetection' {shotSegment} -> shotSegment) (\s@SegmentDetection' {} a -> s {shotSegment = a} :: SegmentDetection)

-- | The frame number of the start of a video segment, using a frame index
-- that starts with 0.
segmentDetection_startFrameNumber :: Lens.Lens' SegmentDetection (Prelude.Maybe Prelude.Natural)
segmentDetection_startFrameNumber = Lens.lens (\SegmentDetection' {startFrameNumber} -> startFrameNumber) (\s@SegmentDetection' {} a -> s {startFrameNumber = a} :: SegmentDetection)

-- | The frame-accurate SMPTE timecode, from the start of a video, for the
-- start of a detected segment. @StartTimecode@ is in /HH:MM:SS:fr/ format
-- (and /;fr/ for drop frame-rates).
segmentDetection_startTimecodeSMPTE :: Lens.Lens' SegmentDetection (Prelude.Maybe Prelude.Text)
segmentDetection_startTimecodeSMPTE = Lens.lens (\SegmentDetection' {startTimecodeSMPTE} -> startTimecodeSMPTE) (\s@SegmentDetection' {} a -> s {startTimecodeSMPTE = a} :: SegmentDetection)

-- | The start time of the detected segment in milliseconds from the start of
-- the video. This value is rounded down. For example, if the actual
-- timestamp is 100.6667 milliseconds, Amazon Rekognition Video returns a
-- value of 100 millis.
segmentDetection_startTimestampMillis :: Lens.Lens' SegmentDetection (Prelude.Maybe Prelude.Integer)
segmentDetection_startTimestampMillis = Lens.lens (\SegmentDetection' {startTimestampMillis} -> startTimestampMillis) (\s@SegmentDetection' {} a -> s {startTimestampMillis = a} :: SegmentDetection)

-- | If the segment is a technical cue, contains information about the
-- technical cue.
segmentDetection_technicalCueSegment :: Lens.Lens' SegmentDetection (Prelude.Maybe TechnicalCueSegment)
segmentDetection_technicalCueSegment = Lens.lens (\SegmentDetection' {technicalCueSegment} -> technicalCueSegment) (\s@SegmentDetection' {} a -> s {technicalCueSegment = a} :: SegmentDetection)

-- | The type of the segment. Valid values are @TECHNICAL_CUE@ and @SHOT@.
segmentDetection_type :: Lens.Lens' SegmentDetection (Prelude.Maybe SegmentType)
segmentDetection_type = Lens.lens (\SegmentDetection' {type'} -> type') (\s@SegmentDetection' {} a -> s {type' = a} :: SegmentDetection)

instance Data.FromJSON SegmentDetection where
  parseJSON =
    Data.withObject
      "SegmentDetection"
      ( \x ->
          SegmentDetection'
            Prelude.<$> (x Data..:? "DurationFrames")
            Prelude.<*> (x Data..:? "DurationMillis")
            Prelude.<*> (x Data..:? "DurationSMPTE")
            Prelude.<*> (x Data..:? "EndFrameNumber")
            Prelude.<*> (x Data..:? "EndTimecodeSMPTE")
            Prelude.<*> (x Data..:? "EndTimestampMillis")
            Prelude.<*> (x Data..:? "ShotSegment")
            Prelude.<*> (x Data..:? "StartFrameNumber")
            Prelude.<*> (x Data..:? "StartTimecodeSMPTE")
            Prelude.<*> (x Data..:? "StartTimestampMillis")
            Prelude.<*> (x Data..:? "TechnicalCueSegment")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable SegmentDetection where
  hashWithSalt _salt SegmentDetection' {..} =
    _salt
      `Prelude.hashWithSalt` durationFrames
      `Prelude.hashWithSalt` durationMillis
      `Prelude.hashWithSalt` durationSMPTE
      `Prelude.hashWithSalt` endFrameNumber
      `Prelude.hashWithSalt` endTimecodeSMPTE
      `Prelude.hashWithSalt` endTimestampMillis
      `Prelude.hashWithSalt` shotSegment
      `Prelude.hashWithSalt` startFrameNumber
      `Prelude.hashWithSalt` startTimecodeSMPTE
      `Prelude.hashWithSalt` startTimestampMillis
      `Prelude.hashWithSalt` technicalCueSegment
      `Prelude.hashWithSalt` type'

instance Prelude.NFData SegmentDetection where
  rnf SegmentDetection' {..} =
    Prelude.rnf durationFrames
      `Prelude.seq` Prelude.rnf durationMillis
      `Prelude.seq` Prelude.rnf durationSMPTE
      `Prelude.seq` Prelude.rnf endFrameNumber
      `Prelude.seq` Prelude.rnf endTimecodeSMPTE
      `Prelude.seq` Prelude.rnf endTimestampMillis
      `Prelude.seq` Prelude.rnf shotSegment
      `Prelude.seq` Prelude.rnf startFrameNumber
      `Prelude.seq` Prelude.rnf startTimecodeSMPTE
      `Prelude.seq` Prelude.rnf startTimestampMillis
      `Prelude.seq` Prelude.rnf technicalCueSegment
      `Prelude.seq` Prelude.rnf type'
