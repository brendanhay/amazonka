{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.SegmentDetection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.SegmentDetection
  ( SegmentDetection (..),

    -- * Smart constructor
    mkSegmentDetection,

    -- * Lenses
    sdTechnicalCueSegment,
    sdDurationSMPTE,
    sdEndTimestampMillis,
    sdStartTimecodeSMPTE,
    sdEndTimecodeSMPTE,
    sdDurationMillis,
    sdStartTimestampMillis,
    sdType,
    sdShotSegment,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.SegmentType
import Network.AWS.Rekognition.Types.ShotSegment
import Network.AWS.Rekognition.Types.TechnicalCueSegment

-- | A technical cue or shot detection segment detected in a video. An array of @SegmentDetection@ objects containing all segments detected in a stored video is returned by 'GetSegmentDetection' .
--
-- /See:/ 'mkSegmentDetection' smart constructor.
data SegmentDetection = SegmentDetection'
  { -- | If the segment is a technical cue, contains information about the technical cue.
    technicalCueSegment :: Lude.Maybe TechnicalCueSegment,
    -- | The duration of the timecode for the detected segment in SMPTE format.
    durationSMPTE :: Lude.Maybe Lude.Text,
    -- | The end time of the detected segment, in milliseconds, from the start of the video. This value is rounded down.
    endTimestampMillis :: Lude.Maybe Lude.Integer,
    -- | The frame-accurate SMPTE timecode, from the start of a video, for the start of a detected segment. @StartTimecode@ is in /HH:MM:SS:fr/ format (and /;fr/ for drop frame-rates).
    startTimecodeSMPTE :: Lude.Maybe Lude.Text,
    -- | The frame-accurate SMPTE timecode, from the start of a video, for the end of a detected segment. @EndTimecode@ is in /HH:MM:SS:fr/ format (and /;fr/ for drop frame-rates).
    endTimecodeSMPTE :: Lude.Maybe Lude.Text,
    -- | The duration of the detected segment in milliseconds.
    durationMillis :: Lude.Maybe Lude.Natural,
    -- | The start time of the detected segment in milliseconds from the start of the video. This value is rounded down. For example, if the actual timestamp is 100.6667 milliseconds, Amazon Rekognition Video returns a value of 100 millis.
    startTimestampMillis :: Lude.Maybe Lude.Integer,
    -- | The type of the segment. Valid values are @TECHNICAL_CUE@ and @SHOT@ .
    type' :: Lude.Maybe SegmentType,
    -- | If the segment is a shot detection, contains information about the shot detection.
    shotSegment :: Lude.Maybe ShotSegment
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SegmentDetection' with the minimum fields required to make a request.
--
-- * 'technicalCueSegment' - If the segment is a technical cue, contains information about the technical cue.
-- * 'durationSMPTE' - The duration of the timecode for the detected segment in SMPTE format.
-- * 'endTimestampMillis' - The end time of the detected segment, in milliseconds, from the start of the video. This value is rounded down.
-- * 'startTimecodeSMPTE' - The frame-accurate SMPTE timecode, from the start of a video, for the start of a detected segment. @StartTimecode@ is in /HH:MM:SS:fr/ format (and /;fr/ for drop frame-rates).
-- * 'endTimecodeSMPTE' - The frame-accurate SMPTE timecode, from the start of a video, for the end of a detected segment. @EndTimecode@ is in /HH:MM:SS:fr/ format (and /;fr/ for drop frame-rates).
-- * 'durationMillis' - The duration of the detected segment in milliseconds.
-- * 'startTimestampMillis' - The start time of the detected segment in milliseconds from the start of the video. This value is rounded down. For example, if the actual timestamp is 100.6667 milliseconds, Amazon Rekognition Video returns a value of 100 millis.
-- * 'type'' - The type of the segment. Valid values are @TECHNICAL_CUE@ and @SHOT@ .
-- * 'shotSegment' - If the segment is a shot detection, contains information about the shot detection.
mkSegmentDetection ::
  SegmentDetection
mkSegmentDetection =
  SegmentDetection'
    { technicalCueSegment = Lude.Nothing,
      durationSMPTE = Lude.Nothing,
      endTimestampMillis = Lude.Nothing,
      startTimecodeSMPTE = Lude.Nothing,
      endTimecodeSMPTE = Lude.Nothing,
      durationMillis = Lude.Nothing,
      startTimestampMillis = Lude.Nothing,
      type' = Lude.Nothing,
      shotSegment = Lude.Nothing
    }

-- | If the segment is a technical cue, contains information about the technical cue.
--
-- /Note:/ Consider using 'technicalCueSegment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdTechnicalCueSegment :: Lens.Lens' SegmentDetection (Lude.Maybe TechnicalCueSegment)
sdTechnicalCueSegment = Lens.lens (technicalCueSegment :: SegmentDetection -> Lude.Maybe TechnicalCueSegment) (\s a -> s {technicalCueSegment = a} :: SegmentDetection)
{-# DEPRECATED sdTechnicalCueSegment "Use generic-lens or generic-optics with 'technicalCueSegment' instead." #-}

-- | The duration of the timecode for the detected segment in SMPTE format.
--
-- /Note:/ Consider using 'durationSMPTE' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdDurationSMPTE :: Lens.Lens' SegmentDetection (Lude.Maybe Lude.Text)
sdDurationSMPTE = Lens.lens (durationSMPTE :: SegmentDetection -> Lude.Maybe Lude.Text) (\s a -> s {durationSMPTE = a} :: SegmentDetection)
{-# DEPRECATED sdDurationSMPTE "Use generic-lens or generic-optics with 'durationSMPTE' instead." #-}

-- | The end time of the detected segment, in milliseconds, from the start of the video. This value is rounded down.
--
-- /Note:/ Consider using 'endTimestampMillis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdEndTimestampMillis :: Lens.Lens' SegmentDetection (Lude.Maybe Lude.Integer)
sdEndTimestampMillis = Lens.lens (endTimestampMillis :: SegmentDetection -> Lude.Maybe Lude.Integer) (\s a -> s {endTimestampMillis = a} :: SegmentDetection)
{-# DEPRECATED sdEndTimestampMillis "Use generic-lens or generic-optics with 'endTimestampMillis' instead." #-}

-- | The frame-accurate SMPTE timecode, from the start of a video, for the start of a detected segment. @StartTimecode@ is in /HH:MM:SS:fr/ format (and /;fr/ for drop frame-rates).
--
-- /Note:/ Consider using 'startTimecodeSMPTE' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdStartTimecodeSMPTE :: Lens.Lens' SegmentDetection (Lude.Maybe Lude.Text)
sdStartTimecodeSMPTE = Lens.lens (startTimecodeSMPTE :: SegmentDetection -> Lude.Maybe Lude.Text) (\s a -> s {startTimecodeSMPTE = a} :: SegmentDetection)
{-# DEPRECATED sdStartTimecodeSMPTE "Use generic-lens or generic-optics with 'startTimecodeSMPTE' instead." #-}

-- | The frame-accurate SMPTE timecode, from the start of a video, for the end of a detected segment. @EndTimecode@ is in /HH:MM:SS:fr/ format (and /;fr/ for drop frame-rates).
--
-- /Note:/ Consider using 'endTimecodeSMPTE' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdEndTimecodeSMPTE :: Lens.Lens' SegmentDetection (Lude.Maybe Lude.Text)
sdEndTimecodeSMPTE = Lens.lens (endTimecodeSMPTE :: SegmentDetection -> Lude.Maybe Lude.Text) (\s a -> s {endTimecodeSMPTE = a} :: SegmentDetection)
{-# DEPRECATED sdEndTimecodeSMPTE "Use generic-lens or generic-optics with 'endTimecodeSMPTE' instead." #-}

-- | The duration of the detected segment in milliseconds.
--
-- /Note:/ Consider using 'durationMillis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdDurationMillis :: Lens.Lens' SegmentDetection (Lude.Maybe Lude.Natural)
sdDurationMillis = Lens.lens (durationMillis :: SegmentDetection -> Lude.Maybe Lude.Natural) (\s a -> s {durationMillis = a} :: SegmentDetection)
{-# DEPRECATED sdDurationMillis "Use generic-lens or generic-optics with 'durationMillis' instead." #-}

-- | The start time of the detected segment in milliseconds from the start of the video. This value is rounded down. For example, if the actual timestamp is 100.6667 milliseconds, Amazon Rekognition Video returns a value of 100 millis.
--
-- /Note:/ Consider using 'startTimestampMillis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdStartTimestampMillis :: Lens.Lens' SegmentDetection (Lude.Maybe Lude.Integer)
sdStartTimestampMillis = Lens.lens (startTimestampMillis :: SegmentDetection -> Lude.Maybe Lude.Integer) (\s a -> s {startTimestampMillis = a} :: SegmentDetection)
{-# DEPRECATED sdStartTimestampMillis "Use generic-lens or generic-optics with 'startTimestampMillis' instead." #-}

-- | The type of the segment. Valid values are @TECHNICAL_CUE@ and @SHOT@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdType :: Lens.Lens' SegmentDetection (Lude.Maybe SegmentType)
sdType = Lens.lens (type' :: SegmentDetection -> Lude.Maybe SegmentType) (\s a -> s {type' = a} :: SegmentDetection)
{-# DEPRECATED sdType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | If the segment is a shot detection, contains information about the shot detection.
--
-- /Note:/ Consider using 'shotSegment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdShotSegment :: Lens.Lens' SegmentDetection (Lude.Maybe ShotSegment)
sdShotSegment = Lens.lens (shotSegment :: SegmentDetection -> Lude.Maybe ShotSegment) (\s a -> s {shotSegment = a} :: SegmentDetection)
{-# DEPRECATED sdShotSegment "Use generic-lens or generic-optics with 'shotSegment' instead." #-}

instance Lude.FromJSON SegmentDetection where
  parseJSON =
    Lude.withObject
      "SegmentDetection"
      ( \x ->
          SegmentDetection'
            Lude.<$> (x Lude..:? "TechnicalCueSegment")
            Lude.<*> (x Lude..:? "DurationSMPTE")
            Lude.<*> (x Lude..:? "EndTimestampMillis")
            Lude.<*> (x Lude..:? "StartTimecodeSMPTE")
            Lude.<*> (x Lude..:? "EndTimecodeSMPTE")
            Lude.<*> (x Lude..:? "DurationMillis")
            Lude.<*> (x Lude..:? "StartTimestampMillis")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "ShotSegment")
      )
