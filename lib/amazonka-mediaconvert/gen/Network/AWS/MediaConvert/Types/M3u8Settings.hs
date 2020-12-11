-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M3u8Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M3u8Settings
  ( M3u8Settings (..),

    -- * Smart constructor
    mkM3u8Settings,

    -- * Lenses
    msPmtPid,
    msVideoPid,
    msProgramNumber,
    msScte35Pid,
    msTransportStreamId,
    msPrivateMetadataPid,
    msAudioDuration,
    msPmtInterval,
    msTimedMetadataPid,
    msAudioFramesPerPes,
    msPcrPid,
    msTimedMetadata,
    msScte35Source,
    msPatInterval,
    msAudioPids,
    msNielsenId3,
    msPcrControl,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.M3u8AudioDuration
import Network.AWS.MediaConvert.Types.M3u8NielsenId3
import Network.AWS.MediaConvert.Types.M3u8PcrControl
import Network.AWS.MediaConvert.Types.M3u8Scte35Source
import Network.AWS.MediaConvert.Types.TimedMetadata
import qualified Network.AWS.Prelude as Lude

-- | Settings for TS segments in HLS
--
-- /See:/ 'mkM3u8Settings' smart constructor.
data M3u8Settings = M3u8Settings'
  { pmtPid ::
      Lude.Maybe Lude.Natural,
    videoPid :: Lude.Maybe Lude.Natural,
    programNumber :: Lude.Maybe Lude.Natural,
    scte35Pid :: Lude.Maybe Lude.Natural,
    transportStreamId :: Lude.Maybe Lude.Natural,
    privateMetadataPid :: Lude.Maybe Lude.Natural,
    audioDuration :: Lude.Maybe M3u8AudioDuration,
    pmtInterval :: Lude.Maybe Lude.Natural,
    timedMetadataPid :: Lude.Maybe Lude.Natural,
    audioFramesPerPes :: Lude.Maybe Lude.Natural,
    pcrPid :: Lude.Maybe Lude.Natural,
    timedMetadata :: Lude.Maybe TimedMetadata,
    scte35Source :: Lude.Maybe M3u8Scte35Source,
    patInterval :: Lude.Maybe Lude.Natural,
    audioPids :: Lude.Maybe [Lude.Natural],
    nielsenId3 :: Lude.Maybe M3u8NielsenId3,
    pcrControl :: Lude.Maybe M3u8PcrControl
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'M3u8Settings' with the minimum fields required to make a request.
--
-- * 'audioDuration' - Specify this setting only when your output will be consumed by a downstream repackaging workflow that is sensitive to very small duration differences between video and audio. For this situation, choose Match video duration (MATCH_VIDEO_DURATION). In all other cases, keep the default value, Default codec duration (DEFAULT_CODEC_DURATION). When you choose Match video duration, MediaConvert pads the output audio streams with silence or trims them to ensure that the total duration of each audio stream is at least as long as the total duration of the video stream. After padding or trimming, the audio stream duration is no more than one frame longer than the video stream. MediaConvert applies audio padding or trimming only to the end of the last segment of the output. For unsegmented outputs, MediaConvert adds padding only to the end of the file. When you keep the default value, any minor discrepancies between audio and video duration will depend on your output audio codec.
-- * 'audioFramesPerPes' - The number of audio frames to insert for each PES packet.
-- * 'audioPids' - Packet Identifier (PID) of the elementary audio stream(s) in the transport stream. Multiple values are accepted, and can be entered in ranges and/or by comma separation.
-- * 'nielsenId3' - If INSERT, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
-- * 'patInterval' - The number of milliseconds between instances of this table in the output transport stream.
-- * 'pcrControl' - When set to PCR_EVERY_PES_PACKET a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This parameter is effective only when the PCR PID is the same as the video or audio elementary stream.
-- * 'pcrPid' - Packet Identifier (PID) of the Program Clock Reference (PCR) in the transport stream. When no value is given, the encoder will assign the same value as the Video PID.
-- * 'pmtInterval' - The number of milliseconds between instances of this table in the output transport stream.
-- * 'pmtPid' - Packet Identifier (PID) for the Program Map Table (PMT) in the transport stream.
-- * 'privateMetadataPid' - Packet Identifier (PID) of the private metadata stream in the transport stream.
-- * 'programNumber' - The value of the program number field in the Program Map Table.
-- * 'scte35Pid' - Packet Identifier (PID) of the SCTE-35 stream in the transport stream.
-- * 'scte35Source' - For SCTE-35 markers from your input-- Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that appear in your input to also appear in this output. Choose None (NONE) if you don't want SCTE-35 markers in this output. For SCTE-35 markers from an ESAM XML document-- Choose None (NONE) if you don't want manifest conditioning. Choose Passthrough (PASSTHROUGH) and choose Ad markers (adMarkers) if you do want manifest conditioning. In both cases, also provide the ESAM XML as a string in the setting Signal processing notification XML (sccXml).
-- * 'timedMetadata' - Applies only to HLS outputs. Use this setting to specify whether the service inserts the ID3 timed metadata from the input in this output.
-- * 'timedMetadataPid' - Packet Identifier (PID) of the timed metadata stream in the transport stream.
-- * 'transportStreamId' - The value of the transport stream ID field in the Program Map Table.
-- * 'videoPid' - Packet Identifier (PID) of the elementary video stream in the transport stream.
mkM3u8Settings ::
  M3u8Settings
mkM3u8Settings =
  M3u8Settings'
    { pmtPid = Lude.Nothing,
      videoPid = Lude.Nothing,
      programNumber = Lude.Nothing,
      scte35Pid = Lude.Nothing,
      transportStreamId = Lude.Nothing,
      privateMetadataPid = Lude.Nothing,
      audioDuration = Lude.Nothing,
      pmtInterval = Lude.Nothing,
      timedMetadataPid = Lude.Nothing,
      audioFramesPerPes = Lude.Nothing,
      pcrPid = Lude.Nothing,
      timedMetadata = Lude.Nothing,
      scte35Source = Lude.Nothing,
      patInterval = Lude.Nothing,
      audioPids = Lude.Nothing,
      nielsenId3 = Lude.Nothing,
      pcrControl = Lude.Nothing
    }

-- | Packet Identifier (PID) for the Program Map Table (PMT) in the transport stream.
--
-- /Note:/ Consider using 'pmtPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msPmtPid :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Natural)
msPmtPid = Lens.lens (pmtPid :: M3u8Settings -> Lude.Maybe Lude.Natural) (\s a -> s {pmtPid = a} :: M3u8Settings)
{-# DEPRECATED msPmtPid "Use generic-lens or generic-optics with 'pmtPid' instead." #-}

-- | Packet Identifier (PID) of the elementary video stream in the transport stream.
--
-- /Note:/ Consider using 'videoPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msVideoPid :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Natural)
msVideoPid = Lens.lens (videoPid :: M3u8Settings -> Lude.Maybe Lude.Natural) (\s a -> s {videoPid = a} :: M3u8Settings)
{-# DEPRECATED msVideoPid "Use generic-lens or generic-optics with 'videoPid' instead." #-}

-- | The value of the program number field in the Program Map Table.
--
-- /Note:/ Consider using 'programNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msProgramNumber :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Natural)
msProgramNumber = Lens.lens (programNumber :: M3u8Settings -> Lude.Maybe Lude.Natural) (\s a -> s {programNumber = a} :: M3u8Settings)
{-# DEPRECATED msProgramNumber "Use generic-lens or generic-optics with 'programNumber' instead." #-}

-- | Packet Identifier (PID) of the SCTE-35 stream in the transport stream.
--
-- /Note:/ Consider using 'scte35Pid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msScte35Pid :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Natural)
msScte35Pid = Lens.lens (scte35Pid :: M3u8Settings -> Lude.Maybe Lude.Natural) (\s a -> s {scte35Pid = a} :: M3u8Settings)
{-# DEPRECATED msScte35Pid "Use generic-lens or generic-optics with 'scte35Pid' instead." #-}

-- | The value of the transport stream ID field in the Program Map Table.
--
-- /Note:/ Consider using 'transportStreamId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msTransportStreamId :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Natural)
msTransportStreamId = Lens.lens (transportStreamId :: M3u8Settings -> Lude.Maybe Lude.Natural) (\s a -> s {transportStreamId = a} :: M3u8Settings)
{-# DEPRECATED msTransportStreamId "Use generic-lens or generic-optics with 'transportStreamId' instead." #-}

-- | Packet Identifier (PID) of the private metadata stream in the transport stream.
--
-- /Note:/ Consider using 'privateMetadataPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msPrivateMetadataPid :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Natural)
msPrivateMetadataPid = Lens.lens (privateMetadataPid :: M3u8Settings -> Lude.Maybe Lude.Natural) (\s a -> s {privateMetadataPid = a} :: M3u8Settings)
{-# DEPRECATED msPrivateMetadataPid "Use generic-lens or generic-optics with 'privateMetadataPid' instead." #-}

-- | Specify this setting only when your output will be consumed by a downstream repackaging workflow that is sensitive to very small duration differences between video and audio. For this situation, choose Match video duration (MATCH_VIDEO_DURATION). In all other cases, keep the default value, Default codec duration (DEFAULT_CODEC_DURATION). When you choose Match video duration, MediaConvert pads the output audio streams with silence or trims them to ensure that the total duration of each audio stream is at least as long as the total duration of the video stream. After padding or trimming, the audio stream duration is no more than one frame longer than the video stream. MediaConvert applies audio padding or trimming only to the end of the last segment of the output. For unsegmented outputs, MediaConvert adds padding only to the end of the file. When you keep the default value, any minor discrepancies between audio and video duration will depend on your output audio codec.
--
-- /Note:/ Consider using 'audioDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msAudioDuration :: Lens.Lens' M3u8Settings (Lude.Maybe M3u8AudioDuration)
msAudioDuration = Lens.lens (audioDuration :: M3u8Settings -> Lude.Maybe M3u8AudioDuration) (\s a -> s {audioDuration = a} :: M3u8Settings)
{-# DEPRECATED msAudioDuration "Use generic-lens or generic-optics with 'audioDuration' instead." #-}

-- | The number of milliseconds between instances of this table in the output transport stream.
--
-- /Note:/ Consider using 'pmtInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msPmtInterval :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Natural)
msPmtInterval = Lens.lens (pmtInterval :: M3u8Settings -> Lude.Maybe Lude.Natural) (\s a -> s {pmtInterval = a} :: M3u8Settings)
{-# DEPRECATED msPmtInterval "Use generic-lens or generic-optics with 'pmtInterval' instead." #-}

-- | Packet Identifier (PID) of the timed metadata stream in the transport stream.
--
-- /Note:/ Consider using 'timedMetadataPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msTimedMetadataPid :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Natural)
msTimedMetadataPid = Lens.lens (timedMetadataPid :: M3u8Settings -> Lude.Maybe Lude.Natural) (\s a -> s {timedMetadataPid = a} :: M3u8Settings)
{-# DEPRECATED msTimedMetadataPid "Use generic-lens or generic-optics with 'timedMetadataPid' instead." #-}

-- | The number of audio frames to insert for each PES packet.
--
-- /Note:/ Consider using 'audioFramesPerPes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msAudioFramesPerPes :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Natural)
msAudioFramesPerPes = Lens.lens (audioFramesPerPes :: M3u8Settings -> Lude.Maybe Lude.Natural) (\s a -> s {audioFramesPerPes = a} :: M3u8Settings)
{-# DEPRECATED msAudioFramesPerPes "Use generic-lens or generic-optics with 'audioFramesPerPes' instead." #-}

-- | Packet Identifier (PID) of the Program Clock Reference (PCR) in the transport stream. When no value is given, the encoder will assign the same value as the Video PID.
--
-- /Note:/ Consider using 'pcrPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msPcrPid :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Natural)
msPcrPid = Lens.lens (pcrPid :: M3u8Settings -> Lude.Maybe Lude.Natural) (\s a -> s {pcrPid = a} :: M3u8Settings)
{-# DEPRECATED msPcrPid "Use generic-lens or generic-optics with 'pcrPid' instead." #-}

-- | Applies only to HLS outputs. Use this setting to specify whether the service inserts the ID3 timed metadata from the input in this output.
--
-- /Note:/ Consider using 'timedMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msTimedMetadata :: Lens.Lens' M3u8Settings (Lude.Maybe TimedMetadata)
msTimedMetadata = Lens.lens (timedMetadata :: M3u8Settings -> Lude.Maybe TimedMetadata) (\s a -> s {timedMetadata = a} :: M3u8Settings)
{-# DEPRECATED msTimedMetadata "Use generic-lens or generic-optics with 'timedMetadata' instead." #-}

-- | For SCTE-35 markers from your input-- Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that appear in your input to also appear in this output. Choose None (NONE) if you don't want SCTE-35 markers in this output. For SCTE-35 markers from an ESAM XML document-- Choose None (NONE) if you don't want manifest conditioning. Choose Passthrough (PASSTHROUGH) and choose Ad markers (adMarkers) if you do want manifest conditioning. In both cases, also provide the ESAM XML as a string in the setting Signal processing notification XML (sccXml).
--
-- /Note:/ Consider using 'scte35Source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msScte35Source :: Lens.Lens' M3u8Settings (Lude.Maybe M3u8Scte35Source)
msScte35Source = Lens.lens (scte35Source :: M3u8Settings -> Lude.Maybe M3u8Scte35Source) (\s a -> s {scte35Source = a} :: M3u8Settings)
{-# DEPRECATED msScte35Source "Use generic-lens or generic-optics with 'scte35Source' instead." #-}

-- | The number of milliseconds between instances of this table in the output transport stream.
--
-- /Note:/ Consider using 'patInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msPatInterval :: Lens.Lens' M3u8Settings (Lude.Maybe Lude.Natural)
msPatInterval = Lens.lens (patInterval :: M3u8Settings -> Lude.Maybe Lude.Natural) (\s a -> s {patInterval = a} :: M3u8Settings)
{-# DEPRECATED msPatInterval "Use generic-lens or generic-optics with 'patInterval' instead." #-}

-- | Packet Identifier (PID) of the elementary audio stream(s) in the transport stream. Multiple values are accepted, and can be entered in ranges and/or by comma separation.
--
-- /Note:/ Consider using 'audioPids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msAudioPids :: Lens.Lens' M3u8Settings (Lude.Maybe [Lude.Natural])
msAudioPids = Lens.lens (audioPids :: M3u8Settings -> Lude.Maybe [Lude.Natural]) (\s a -> s {audioPids = a} :: M3u8Settings)
{-# DEPRECATED msAudioPids "Use generic-lens or generic-optics with 'audioPids' instead." #-}

-- | If INSERT, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
--
-- /Note:/ Consider using 'nielsenId3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msNielsenId3 :: Lens.Lens' M3u8Settings (Lude.Maybe M3u8NielsenId3)
msNielsenId3 = Lens.lens (nielsenId3 :: M3u8Settings -> Lude.Maybe M3u8NielsenId3) (\s a -> s {nielsenId3 = a} :: M3u8Settings)
{-# DEPRECATED msNielsenId3 "Use generic-lens or generic-optics with 'nielsenId3' instead." #-}

-- | When set to PCR_EVERY_PES_PACKET a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This parameter is effective only when the PCR PID is the same as the video or audio elementary stream.
--
-- /Note:/ Consider using 'pcrControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msPcrControl :: Lens.Lens' M3u8Settings (Lude.Maybe M3u8PcrControl)
msPcrControl = Lens.lens (pcrControl :: M3u8Settings -> Lude.Maybe M3u8PcrControl) (\s a -> s {pcrControl = a} :: M3u8Settings)
{-# DEPRECATED msPcrControl "Use generic-lens or generic-optics with 'pcrControl' instead." #-}

instance Lude.FromJSON M3u8Settings where
  parseJSON =
    Lude.withObject
      "M3u8Settings"
      ( \x ->
          M3u8Settings'
            Lude.<$> (x Lude..:? "pmtPid")
            Lude.<*> (x Lude..:? "videoPid")
            Lude.<*> (x Lude..:? "programNumber")
            Lude.<*> (x Lude..:? "scte35Pid")
            Lude.<*> (x Lude..:? "transportStreamId")
            Lude.<*> (x Lude..:? "privateMetadataPid")
            Lude.<*> (x Lude..:? "audioDuration")
            Lude.<*> (x Lude..:? "pmtInterval")
            Lude.<*> (x Lude..:? "timedMetadataPid")
            Lude.<*> (x Lude..:? "audioFramesPerPes")
            Lude.<*> (x Lude..:? "pcrPid")
            Lude.<*> (x Lude..:? "timedMetadata")
            Lude.<*> (x Lude..:? "scte35Source")
            Lude.<*> (x Lude..:? "patInterval")
            Lude.<*> (x Lude..:? "audioPids" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "nielsenId3")
            Lude.<*> (x Lude..:? "pcrControl")
      )

instance Lude.ToJSON M3u8Settings where
  toJSON M3u8Settings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("pmtPid" Lude..=) Lude.<$> pmtPid,
            ("videoPid" Lude..=) Lude.<$> videoPid,
            ("programNumber" Lude..=) Lude.<$> programNumber,
            ("scte35Pid" Lude..=) Lude.<$> scte35Pid,
            ("transportStreamId" Lude..=) Lude.<$> transportStreamId,
            ("privateMetadataPid" Lude..=) Lude.<$> privateMetadataPid,
            ("audioDuration" Lude..=) Lude.<$> audioDuration,
            ("pmtInterval" Lude..=) Lude.<$> pmtInterval,
            ("timedMetadataPid" Lude..=) Lude.<$> timedMetadataPid,
            ("audioFramesPerPes" Lude..=) Lude.<$> audioFramesPerPes,
            ("pcrPid" Lude..=) Lude.<$> pcrPid,
            ("timedMetadata" Lude..=) Lude.<$> timedMetadata,
            ("scte35Source" Lude..=) Lude.<$> scte35Source,
            ("patInterval" Lude..=) Lude.<$> patInterval,
            ("audioPids" Lude..=) Lude.<$> audioPids,
            ("nielsenId3" Lude..=) Lude.<$> nielsenId3,
            ("pcrControl" Lude..=) Lude.<$> pcrControl
          ]
      )
