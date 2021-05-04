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
-- Module      : Network.AWS.MediaConvert.Types.M3u8Settings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M3u8Settings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.M3u8AudioDuration
import Network.AWS.MediaConvert.Types.M3u8NielsenId3
import Network.AWS.MediaConvert.Types.M3u8PcrControl
import Network.AWS.MediaConvert.Types.M3u8Scte35Source
import Network.AWS.MediaConvert.Types.TimedMetadata
import qualified Network.AWS.Prelude as Prelude

-- | Settings for TS segments in HLS
--
-- /See:/ 'newM3u8Settings' smart constructor.
data M3u8Settings = M3u8Settings'
  { -- | Packet Identifier (PID) for the Program Map Table (PMT) in the transport
    -- stream.
    pmtPid :: Prelude.Maybe Prelude.Natural,
    -- | Applies only to HLS outputs. Use this setting to specify whether the
    -- service inserts the ID3 timed metadata from the input in this output.
    timedMetadata :: Prelude.Maybe TimedMetadata,
    -- | Packet Identifier (PID) of the elementary video stream in the transport
    -- stream.
    videoPid :: Prelude.Maybe Prelude.Natural,
    -- | Packet Identifier (PID) of the timed metadata stream in the transport
    -- stream.
    timedMetadataPid :: Prelude.Maybe Prelude.Natural,
    -- | When set to PCR_EVERY_PES_PACKET a Program Clock Reference value is
    -- inserted for every Packetized Elementary Stream (PES) header. This
    -- parameter is effective only when the PCR PID is the same as the video or
    -- audio elementary stream.
    pcrControl :: Prelude.Maybe M3u8PcrControl,
    -- | The number of milliseconds between instances of this table in the output
    -- transport stream.
    pmtInterval :: Prelude.Maybe Prelude.Natural,
    -- | Packet Identifier (PID) of the elementary audio stream(s) in the
    -- transport stream. Multiple values are accepted, and can be entered in
    -- ranges and\/or by comma separation.
    audioPids :: Prelude.Maybe [Prelude.Natural],
    -- | The number of milliseconds between instances of this table in the output
    -- transport stream.
    patInterval :: Prelude.Maybe Prelude.Natural,
    -- | The value of the program number field in the Program Map Table.
    programNumber :: Prelude.Maybe Prelude.Natural,
    -- | Packet Identifier (PID) of the Program Clock Reference (PCR) in the
    -- transport stream. When no value is given, the encoder will assign the
    -- same value as the Video PID.
    pcrPid :: Prelude.Maybe Prelude.Natural,
    -- | The number of audio frames to insert for each PES packet.
    audioFramesPerPes :: Prelude.Maybe Prelude.Natural,
    -- | Specify this setting only when your output will be consumed by a
    -- downstream repackaging workflow that is sensitive to very small duration
    -- differences between video and audio. For this situation, choose Match
    -- video duration (MATCH_VIDEO_DURATION). In all other cases, keep the
    -- default value, Default codec duration (DEFAULT_CODEC_DURATION). When you
    -- choose Match video duration, MediaConvert pads the output audio streams
    -- with silence or trims them to ensure that the total duration of each
    -- audio stream is at least as long as the total duration of the video
    -- stream. After padding or trimming, the audio stream duration is no more
    -- than one frame longer than the video stream. MediaConvert applies audio
    -- padding or trimming only to the end of the last segment of the output.
    -- For unsegmented outputs, MediaConvert adds padding only to the end of
    -- the file. When you keep the default value, any minor discrepancies
    -- between audio and video duration will depend on your output audio codec.
    audioDuration :: Prelude.Maybe M3u8AudioDuration,
    -- | If INSERT, Nielsen inaudible tones for media tracking will be detected
    -- in the input audio and an equivalent ID3 tag will be inserted in the
    -- output.
    nielsenId3 :: Prelude.Maybe M3u8NielsenId3,
    -- | Packet Identifier (PID) of the private metadata stream in the transport
    -- stream.
    privateMetadataPid :: Prelude.Maybe Prelude.Natural,
    -- | For SCTE-35 markers from your input-- Choose Passthrough (PASSTHROUGH)
    -- if you want SCTE-35 markers that appear in your input to also appear in
    -- this output. Choose None (NONE) if you don\'t want SCTE-35 markers in
    -- this output. For SCTE-35 markers from an ESAM XML document-- Choose None
    -- (NONE) if you don\'t want manifest conditioning. Choose Passthrough
    -- (PASSTHROUGH) and choose Ad markers (adMarkers) if you do want manifest
    -- conditioning. In both cases, also provide the ESAM XML as a string in
    -- the setting Signal processing notification XML (sccXml).
    scte35Source :: Prelude.Maybe M3u8Scte35Source,
    -- | The value of the transport stream ID field in the Program Map Table.
    transportStreamId :: Prelude.Maybe Prelude.Natural,
    -- | Packet Identifier (PID) of the SCTE-35 stream in the transport stream.
    scte35Pid :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'M3u8Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pmtPid', 'm3u8Settings_pmtPid' - Packet Identifier (PID) for the Program Map Table (PMT) in the transport
-- stream.
--
-- 'timedMetadata', 'm3u8Settings_timedMetadata' - Applies only to HLS outputs. Use this setting to specify whether the
-- service inserts the ID3 timed metadata from the input in this output.
--
-- 'videoPid', 'm3u8Settings_videoPid' - Packet Identifier (PID) of the elementary video stream in the transport
-- stream.
--
-- 'timedMetadataPid', 'm3u8Settings_timedMetadataPid' - Packet Identifier (PID) of the timed metadata stream in the transport
-- stream.
--
-- 'pcrControl', 'm3u8Settings_pcrControl' - When set to PCR_EVERY_PES_PACKET a Program Clock Reference value is
-- inserted for every Packetized Elementary Stream (PES) header. This
-- parameter is effective only when the PCR PID is the same as the video or
-- audio elementary stream.
--
-- 'pmtInterval', 'm3u8Settings_pmtInterval' - The number of milliseconds between instances of this table in the output
-- transport stream.
--
-- 'audioPids', 'm3u8Settings_audioPids' - Packet Identifier (PID) of the elementary audio stream(s) in the
-- transport stream. Multiple values are accepted, and can be entered in
-- ranges and\/or by comma separation.
--
-- 'patInterval', 'm3u8Settings_patInterval' - The number of milliseconds between instances of this table in the output
-- transport stream.
--
-- 'programNumber', 'm3u8Settings_programNumber' - The value of the program number field in the Program Map Table.
--
-- 'pcrPid', 'm3u8Settings_pcrPid' - Packet Identifier (PID) of the Program Clock Reference (PCR) in the
-- transport stream. When no value is given, the encoder will assign the
-- same value as the Video PID.
--
-- 'audioFramesPerPes', 'm3u8Settings_audioFramesPerPes' - The number of audio frames to insert for each PES packet.
--
-- 'audioDuration', 'm3u8Settings_audioDuration' - Specify this setting only when your output will be consumed by a
-- downstream repackaging workflow that is sensitive to very small duration
-- differences between video and audio. For this situation, choose Match
-- video duration (MATCH_VIDEO_DURATION). In all other cases, keep the
-- default value, Default codec duration (DEFAULT_CODEC_DURATION). When you
-- choose Match video duration, MediaConvert pads the output audio streams
-- with silence or trims them to ensure that the total duration of each
-- audio stream is at least as long as the total duration of the video
-- stream. After padding or trimming, the audio stream duration is no more
-- than one frame longer than the video stream. MediaConvert applies audio
-- padding or trimming only to the end of the last segment of the output.
-- For unsegmented outputs, MediaConvert adds padding only to the end of
-- the file. When you keep the default value, any minor discrepancies
-- between audio and video duration will depend on your output audio codec.
--
-- 'nielsenId3', 'm3u8Settings_nielsenId3' - If INSERT, Nielsen inaudible tones for media tracking will be detected
-- in the input audio and an equivalent ID3 tag will be inserted in the
-- output.
--
-- 'privateMetadataPid', 'm3u8Settings_privateMetadataPid' - Packet Identifier (PID) of the private metadata stream in the transport
-- stream.
--
-- 'scte35Source', 'm3u8Settings_scte35Source' - For SCTE-35 markers from your input-- Choose Passthrough (PASSTHROUGH)
-- if you want SCTE-35 markers that appear in your input to also appear in
-- this output. Choose None (NONE) if you don\'t want SCTE-35 markers in
-- this output. For SCTE-35 markers from an ESAM XML document-- Choose None
-- (NONE) if you don\'t want manifest conditioning. Choose Passthrough
-- (PASSTHROUGH) and choose Ad markers (adMarkers) if you do want manifest
-- conditioning. In both cases, also provide the ESAM XML as a string in
-- the setting Signal processing notification XML (sccXml).
--
-- 'transportStreamId', 'm3u8Settings_transportStreamId' - The value of the transport stream ID field in the Program Map Table.
--
-- 'scte35Pid', 'm3u8Settings_scte35Pid' - Packet Identifier (PID) of the SCTE-35 stream in the transport stream.
newM3u8Settings ::
  M3u8Settings
newM3u8Settings =
  M3u8Settings'
    { pmtPid = Prelude.Nothing,
      timedMetadata = Prelude.Nothing,
      videoPid = Prelude.Nothing,
      timedMetadataPid = Prelude.Nothing,
      pcrControl = Prelude.Nothing,
      pmtInterval = Prelude.Nothing,
      audioPids = Prelude.Nothing,
      patInterval = Prelude.Nothing,
      programNumber = Prelude.Nothing,
      pcrPid = Prelude.Nothing,
      audioFramesPerPes = Prelude.Nothing,
      audioDuration = Prelude.Nothing,
      nielsenId3 = Prelude.Nothing,
      privateMetadataPid = Prelude.Nothing,
      scte35Source = Prelude.Nothing,
      transportStreamId = Prelude.Nothing,
      scte35Pid = Prelude.Nothing
    }

-- | Packet Identifier (PID) for the Program Map Table (PMT) in the transport
-- stream.
m3u8Settings_pmtPid :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Natural)
m3u8Settings_pmtPid = Lens.lens (\M3u8Settings' {pmtPid} -> pmtPid) (\s@M3u8Settings' {} a -> s {pmtPid = a} :: M3u8Settings)

-- | Applies only to HLS outputs. Use this setting to specify whether the
-- service inserts the ID3 timed metadata from the input in this output.
m3u8Settings_timedMetadata :: Lens.Lens' M3u8Settings (Prelude.Maybe TimedMetadata)
m3u8Settings_timedMetadata = Lens.lens (\M3u8Settings' {timedMetadata} -> timedMetadata) (\s@M3u8Settings' {} a -> s {timedMetadata = a} :: M3u8Settings)

-- | Packet Identifier (PID) of the elementary video stream in the transport
-- stream.
m3u8Settings_videoPid :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Natural)
m3u8Settings_videoPid = Lens.lens (\M3u8Settings' {videoPid} -> videoPid) (\s@M3u8Settings' {} a -> s {videoPid = a} :: M3u8Settings)

-- | Packet Identifier (PID) of the timed metadata stream in the transport
-- stream.
m3u8Settings_timedMetadataPid :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Natural)
m3u8Settings_timedMetadataPid = Lens.lens (\M3u8Settings' {timedMetadataPid} -> timedMetadataPid) (\s@M3u8Settings' {} a -> s {timedMetadataPid = a} :: M3u8Settings)

-- | When set to PCR_EVERY_PES_PACKET a Program Clock Reference value is
-- inserted for every Packetized Elementary Stream (PES) header. This
-- parameter is effective only when the PCR PID is the same as the video or
-- audio elementary stream.
m3u8Settings_pcrControl :: Lens.Lens' M3u8Settings (Prelude.Maybe M3u8PcrControl)
m3u8Settings_pcrControl = Lens.lens (\M3u8Settings' {pcrControl} -> pcrControl) (\s@M3u8Settings' {} a -> s {pcrControl = a} :: M3u8Settings)

-- | The number of milliseconds between instances of this table in the output
-- transport stream.
m3u8Settings_pmtInterval :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Natural)
m3u8Settings_pmtInterval = Lens.lens (\M3u8Settings' {pmtInterval} -> pmtInterval) (\s@M3u8Settings' {} a -> s {pmtInterval = a} :: M3u8Settings)

-- | Packet Identifier (PID) of the elementary audio stream(s) in the
-- transport stream. Multiple values are accepted, and can be entered in
-- ranges and\/or by comma separation.
m3u8Settings_audioPids :: Lens.Lens' M3u8Settings (Prelude.Maybe [Prelude.Natural])
m3u8Settings_audioPids = Lens.lens (\M3u8Settings' {audioPids} -> audioPids) (\s@M3u8Settings' {} a -> s {audioPids = a} :: M3u8Settings) Prelude.. Lens.mapping Prelude._Coerce

-- | The number of milliseconds between instances of this table in the output
-- transport stream.
m3u8Settings_patInterval :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Natural)
m3u8Settings_patInterval = Lens.lens (\M3u8Settings' {patInterval} -> patInterval) (\s@M3u8Settings' {} a -> s {patInterval = a} :: M3u8Settings)

-- | The value of the program number field in the Program Map Table.
m3u8Settings_programNumber :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Natural)
m3u8Settings_programNumber = Lens.lens (\M3u8Settings' {programNumber} -> programNumber) (\s@M3u8Settings' {} a -> s {programNumber = a} :: M3u8Settings)

-- | Packet Identifier (PID) of the Program Clock Reference (PCR) in the
-- transport stream. When no value is given, the encoder will assign the
-- same value as the Video PID.
m3u8Settings_pcrPid :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Natural)
m3u8Settings_pcrPid = Lens.lens (\M3u8Settings' {pcrPid} -> pcrPid) (\s@M3u8Settings' {} a -> s {pcrPid = a} :: M3u8Settings)

-- | The number of audio frames to insert for each PES packet.
m3u8Settings_audioFramesPerPes :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Natural)
m3u8Settings_audioFramesPerPes = Lens.lens (\M3u8Settings' {audioFramesPerPes} -> audioFramesPerPes) (\s@M3u8Settings' {} a -> s {audioFramesPerPes = a} :: M3u8Settings)

-- | Specify this setting only when your output will be consumed by a
-- downstream repackaging workflow that is sensitive to very small duration
-- differences between video and audio. For this situation, choose Match
-- video duration (MATCH_VIDEO_DURATION). In all other cases, keep the
-- default value, Default codec duration (DEFAULT_CODEC_DURATION). When you
-- choose Match video duration, MediaConvert pads the output audio streams
-- with silence or trims them to ensure that the total duration of each
-- audio stream is at least as long as the total duration of the video
-- stream. After padding or trimming, the audio stream duration is no more
-- than one frame longer than the video stream. MediaConvert applies audio
-- padding or trimming only to the end of the last segment of the output.
-- For unsegmented outputs, MediaConvert adds padding only to the end of
-- the file. When you keep the default value, any minor discrepancies
-- between audio and video duration will depend on your output audio codec.
m3u8Settings_audioDuration :: Lens.Lens' M3u8Settings (Prelude.Maybe M3u8AudioDuration)
m3u8Settings_audioDuration = Lens.lens (\M3u8Settings' {audioDuration} -> audioDuration) (\s@M3u8Settings' {} a -> s {audioDuration = a} :: M3u8Settings)

-- | If INSERT, Nielsen inaudible tones for media tracking will be detected
-- in the input audio and an equivalent ID3 tag will be inserted in the
-- output.
m3u8Settings_nielsenId3 :: Lens.Lens' M3u8Settings (Prelude.Maybe M3u8NielsenId3)
m3u8Settings_nielsenId3 = Lens.lens (\M3u8Settings' {nielsenId3} -> nielsenId3) (\s@M3u8Settings' {} a -> s {nielsenId3 = a} :: M3u8Settings)

-- | Packet Identifier (PID) of the private metadata stream in the transport
-- stream.
m3u8Settings_privateMetadataPid :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Natural)
m3u8Settings_privateMetadataPid = Lens.lens (\M3u8Settings' {privateMetadataPid} -> privateMetadataPid) (\s@M3u8Settings' {} a -> s {privateMetadataPid = a} :: M3u8Settings)

-- | For SCTE-35 markers from your input-- Choose Passthrough (PASSTHROUGH)
-- if you want SCTE-35 markers that appear in your input to also appear in
-- this output. Choose None (NONE) if you don\'t want SCTE-35 markers in
-- this output. For SCTE-35 markers from an ESAM XML document-- Choose None
-- (NONE) if you don\'t want manifest conditioning. Choose Passthrough
-- (PASSTHROUGH) and choose Ad markers (adMarkers) if you do want manifest
-- conditioning. In both cases, also provide the ESAM XML as a string in
-- the setting Signal processing notification XML (sccXml).
m3u8Settings_scte35Source :: Lens.Lens' M3u8Settings (Prelude.Maybe M3u8Scte35Source)
m3u8Settings_scte35Source = Lens.lens (\M3u8Settings' {scte35Source} -> scte35Source) (\s@M3u8Settings' {} a -> s {scte35Source = a} :: M3u8Settings)

-- | The value of the transport stream ID field in the Program Map Table.
m3u8Settings_transportStreamId :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Natural)
m3u8Settings_transportStreamId = Lens.lens (\M3u8Settings' {transportStreamId} -> transportStreamId) (\s@M3u8Settings' {} a -> s {transportStreamId = a} :: M3u8Settings)

-- | Packet Identifier (PID) of the SCTE-35 stream in the transport stream.
m3u8Settings_scte35Pid :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Natural)
m3u8Settings_scte35Pid = Lens.lens (\M3u8Settings' {scte35Pid} -> scte35Pid) (\s@M3u8Settings' {} a -> s {scte35Pid = a} :: M3u8Settings)

instance Prelude.FromJSON M3u8Settings where
  parseJSON =
    Prelude.withObject
      "M3u8Settings"
      ( \x ->
          M3u8Settings'
            Prelude.<$> (x Prelude..:? "pmtPid")
            Prelude.<*> (x Prelude..:? "timedMetadata")
            Prelude.<*> (x Prelude..:? "videoPid")
            Prelude.<*> (x Prelude..:? "timedMetadataPid")
            Prelude.<*> (x Prelude..:? "pcrControl")
            Prelude.<*> (x Prelude..:? "pmtInterval")
            Prelude.<*> ( x Prelude..:? "audioPids"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "patInterval")
            Prelude.<*> (x Prelude..:? "programNumber")
            Prelude.<*> (x Prelude..:? "pcrPid")
            Prelude.<*> (x Prelude..:? "audioFramesPerPes")
            Prelude.<*> (x Prelude..:? "audioDuration")
            Prelude.<*> (x Prelude..:? "nielsenId3")
            Prelude.<*> (x Prelude..:? "privateMetadataPid")
            Prelude.<*> (x Prelude..:? "scte35Source")
            Prelude.<*> (x Prelude..:? "transportStreamId")
            Prelude.<*> (x Prelude..:? "scte35Pid")
      )

instance Prelude.Hashable M3u8Settings

instance Prelude.NFData M3u8Settings

instance Prelude.ToJSON M3u8Settings where
  toJSON M3u8Settings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("pmtPid" Prelude..=) Prelude.<$> pmtPid,
            ("timedMetadata" Prelude..=)
              Prelude.<$> timedMetadata,
            ("videoPid" Prelude..=) Prelude.<$> videoPid,
            ("timedMetadataPid" Prelude..=)
              Prelude.<$> timedMetadataPid,
            ("pcrControl" Prelude..=) Prelude.<$> pcrControl,
            ("pmtInterval" Prelude..=) Prelude.<$> pmtInterval,
            ("audioPids" Prelude..=) Prelude.<$> audioPids,
            ("patInterval" Prelude..=) Prelude.<$> patInterval,
            ("programNumber" Prelude..=)
              Prelude.<$> programNumber,
            ("pcrPid" Prelude..=) Prelude.<$> pcrPid,
            ("audioFramesPerPes" Prelude..=)
              Prelude.<$> audioFramesPerPes,
            ("audioDuration" Prelude..=)
              Prelude.<$> audioDuration,
            ("nielsenId3" Prelude..=) Prelude.<$> nielsenId3,
            ("privateMetadataPid" Prelude..=)
              Prelude.<$> privateMetadataPid,
            ("scte35Source" Prelude..=) Prelude.<$> scte35Source,
            ("transportStreamId" Prelude..=)
              Prelude.<$> transportStreamId,
            ("scte35Pid" Prelude..=) Prelude.<$> scte35Pid
          ]
      )
