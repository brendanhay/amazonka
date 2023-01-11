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
-- Module      : Amazonka.MediaConvert.Types.M3u8Settings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.M3u8Settings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.M3u8AudioDuration
import Amazonka.MediaConvert.Types.M3u8DataPtsControl
import Amazonka.MediaConvert.Types.M3u8NielsenId3
import Amazonka.MediaConvert.Types.M3u8PcrControl
import Amazonka.MediaConvert.Types.M3u8Scte35Source
import Amazonka.MediaConvert.Types.TimedMetadata
import qualified Amazonka.Prelude as Prelude

-- | These settings relate to the MPEG-2 transport stream (MPEG2-TS)
-- container for the MPEG2-TS segments in your HLS outputs.
--
-- /See:/ 'newM3u8Settings' smart constructor.
data M3u8Settings = M3u8Settings'
  { -- | Specify this setting only when your output will be consumed by a
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
    -- | The number of audio frames to insert for each PES packet.
    audioFramesPerPes :: Prelude.Maybe Prelude.Natural,
    -- | Packet Identifier (PID) of the elementary audio stream(s) in the
    -- transport stream. Multiple values are accepted, and can be entered in
    -- ranges and\/or by comma separation.
    audioPids :: Prelude.Maybe [Prelude.Natural],
    -- | If you select ALIGN_TO_VIDEO, MediaConvert writes captions and data
    -- packets with Presentation Timestamp (PTS) values greater than or equal
    -- to the first video packet PTS (MediaConvert drops captions and data
    -- packets with lesser PTS values). Keep the default value (AUTO) to allow
    -- all PTS values.
    dataPTSControl :: Prelude.Maybe M3u8DataPtsControl,
    -- | Specify the maximum time, in milliseconds, between Program Clock
    -- References (PCRs) inserted into the transport stream.
    maxPcrInterval :: Prelude.Maybe Prelude.Natural,
    -- | If INSERT, Nielsen inaudible tones for media tracking will be detected
    -- in the input audio and an equivalent ID3 tag will be inserted in the
    -- output.
    nielsenId3 :: Prelude.Maybe M3u8NielsenId3,
    -- | The number of milliseconds between instances of this table in the output
    -- transport stream.
    patInterval :: Prelude.Maybe Prelude.Natural,
    -- | When set to PCR_EVERY_PES_PACKET a Program Clock Reference value is
    -- inserted for every Packetized Elementary Stream (PES) header. This
    -- parameter is effective only when the PCR PID is the same as the video or
    -- audio elementary stream.
    pcrControl :: Prelude.Maybe M3u8PcrControl,
    -- | Packet Identifier (PID) of the Program Clock Reference (PCR) in the
    -- transport stream. When no value is given, the encoder will assign the
    -- same value as the Video PID.
    pcrPid :: Prelude.Maybe Prelude.Natural,
    -- | The number of milliseconds between instances of this table in the output
    -- transport stream.
    pmtInterval :: Prelude.Maybe Prelude.Natural,
    -- | Packet Identifier (PID) for the Program Map Table (PMT) in the transport
    -- stream.
    pmtPid :: Prelude.Maybe Prelude.Natural,
    -- | Packet Identifier (PID) of the private metadata stream in the transport
    -- stream.
    privateMetadataPid :: Prelude.Maybe Prelude.Natural,
    -- | The value of the program number field in the Program Map Table.
    programNumber :: Prelude.Maybe Prelude.Natural,
    -- | Packet Identifier (PID) of the SCTE-35 stream in the transport stream.
    scte35Pid :: Prelude.Maybe Prelude.Natural,
    -- | For SCTE-35 markers from your input-- Choose Passthrough (PASSTHROUGH)
    -- if you want SCTE-35 markers that appear in your input to also appear in
    -- this output. Choose None (NONE) if you don\'t want SCTE-35 markers in
    -- this output. For SCTE-35 markers from an ESAM XML document-- Choose None
    -- (NONE) if you don\'t want manifest conditioning. Choose Passthrough
    -- (PASSTHROUGH) and choose Ad markers (adMarkers) if you do want manifest
    -- conditioning. In both cases, also provide the ESAM XML as a string in
    -- the setting Signal processing notification XML (sccXml).
    scte35Source :: Prelude.Maybe M3u8Scte35Source,
    -- | Set ID3 metadata (timedMetadata) to Passthrough (PASSTHROUGH) to include
    -- ID3 metadata in this output. This includes ID3 metadata from the
    -- following features: ID3 timestamp period (timedMetadataId3Period), and
    -- Custom ID3 metadata inserter (timedMetadataInsertion). To exclude this
    -- ID3 metadata in this output: set ID3 metadata to None (NONE) or leave
    -- blank.
    timedMetadata :: Prelude.Maybe TimedMetadata,
    -- | Packet Identifier (PID) of the ID3 metadata stream in the transport
    -- stream.
    timedMetadataPid :: Prelude.Maybe Prelude.Natural,
    -- | The value of the transport stream ID field in the Program Map Table.
    transportStreamId :: Prelude.Maybe Prelude.Natural,
    -- | Packet Identifier (PID) of the elementary video stream in the transport
    -- stream.
    videoPid :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'M3u8Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'audioFramesPerPes', 'm3u8Settings_audioFramesPerPes' - The number of audio frames to insert for each PES packet.
--
-- 'audioPids', 'm3u8Settings_audioPids' - Packet Identifier (PID) of the elementary audio stream(s) in the
-- transport stream. Multiple values are accepted, and can be entered in
-- ranges and\/or by comma separation.
--
-- 'dataPTSControl', 'm3u8Settings_dataPTSControl' - If you select ALIGN_TO_VIDEO, MediaConvert writes captions and data
-- packets with Presentation Timestamp (PTS) values greater than or equal
-- to the first video packet PTS (MediaConvert drops captions and data
-- packets with lesser PTS values). Keep the default value (AUTO) to allow
-- all PTS values.
--
-- 'maxPcrInterval', 'm3u8Settings_maxPcrInterval' - Specify the maximum time, in milliseconds, between Program Clock
-- References (PCRs) inserted into the transport stream.
--
-- 'nielsenId3', 'm3u8Settings_nielsenId3' - If INSERT, Nielsen inaudible tones for media tracking will be detected
-- in the input audio and an equivalent ID3 tag will be inserted in the
-- output.
--
-- 'patInterval', 'm3u8Settings_patInterval' - The number of milliseconds between instances of this table in the output
-- transport stream.
--
-- 'pcrControl', 'm3u8Settings_pcrControl' - When set to PCR_EVERY_PES_PACKET a Program Clock Reference value is
-- inserted for every Packetized Elementary Stream (PES) header. This
-- parameter is effective only when the PCR PID is the same as the video or
-- audio elementary stream.
--
-- 'pcrPid', 'm3u8Settings_pcrPid' - Packet Identifier (PID) of the Program Clock Reference (PCR) in the
-- transport stream. When no value is given, the encoder will assign the
-- same value as the Video PID.
--
-- 'pmtInterval', 'm3u8Settings_pmtInterval' - The number of milliseconds between instances of this table in the output
-- transport stream.
--
-- 'pmtPid', 'm3u8Settings_pmtPid' - Packet Identifier (PID) for the Program Map Table (PMT) in the transport
-- stream.
--
-- 'privateMetadataPid', 'm3u8Settings_privateMetadataPid' - Packet Identifier (PID) of the private metadata stream in the transport
-- stream.
--
-- 'programNumber', 'm3u8Settings_programNumber' - The value of the program number field in the Program Map Table.
--
-- 'scte35Pid', 'm3u8Settings_scte35Pid' - Packet Identifier (PID) of the SCTE-35 stream in the transport stream.
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
-- 'timedMetadata', 'm3u8Settings_timedMetadata' - Set ID3 metadata (timedMetadata) to Passthrough (PASSTHROUGH) to include
-- ID3 metadata in this output. This includes ID3 metadata from the
-- following features: ID3 timestamp period (timedMetadataId3Period), and
-- Custom ID3 metadata inserter (timedMetadataInsertion). To exclude this
-- ID3 metadata in this output: set ID3 metadata to None (NONE) or leave
-- blank.
--
-- 'timedMetadataPid', 'm3u8Settings_timedMetadataPid' - Packet Identifier (PID) of the ID3 metadata stream in the transport
-- stream.
--
-- 'transportStreamId', 'm3u8Settings_transportStreamId' - The value of the transport stream ID field in the Program Map Table.
--
-- 'videoPid', 'm3u8Settings_videoPid' - Packet Identifier (PID) of the elementary video stream in the transport
-- stream.
newM3u8Settings ::
  M3u8Settings
newM3u8Settings =
  M3u8Settings'
    { audioDuration = Prelude.Nothing,
      audioFramesPerPes = Prelude.Nothing,
      audioPids = Prelude.Nothing,
      dataPTSControl = Prelude.Nothing,
      maxPcrInterval = Prelude.Nothing,
      nielsenId3 = Prelude.Nothing,
      patInterval = Prelude.Nothing,
      pcrControl = Prelude.Nothing,
      pcrPid = Prelude.Nothing,
      pmtInterval = Prelude.Nothing,
      pmtPid = Prelude.Nothing,
      privateMetadataPid = Prelude.Nothing,
      programNumber = Prelude.Nothing,
      scte35Pid = Prelude.Nothing,
      scte35Source = Prelude.Nothing,
      timedMetadata = Prelude.Nothing,
      timedMetadataPid = Prelude.Nothing,
      transportStreamId = Prelude.Nothing,
      videoPid = Prelude.Nothing
    }

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

-- | The number of audio frames to insert for each PES packet.
m3u8Settings_audioFramesPerPes :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Natural)
m3u8Settings_audioFramesPerPes = Lens.lens (\M3u8Settings' {audioFramesPerPes} -> audioFramesPerPes) (\s@M3u8Settings' {} a -> s {audioFramesPerPes = a} :: M3u8Settings)

-- | Packet Identifier (PID) of the elementary audio stream(s) in the
-- transport stream. Multiple values are accepted, and can be entered in
-- ranges and\/or by comma separation.
m3u8Settings_audioPids :: Lens.Lens' M3u8Settings (Prelude.Maybe [Prelude.Natural])
m3u8Settings_audioPids = Lens.lens (\M3u8Settings' {audioPids} -> audioPids) (\s@M3u8Settings' {} a -> s {audioPids = a} :: M3u8Settings) Prelude.. Lens.mapping Lens.coerced

-- | If you select ALIGN_TO_VIDEO, MediaConvert writes captions and data
-- packets with Presentation Timestamp (PTS) values greater than or equal
-- to the first video packet PTS (MediaConvert drops captions and data
-- packets with lesser PTS values). Keep the default value (AUTO) to allow
-- all PTS values.
m3u8Settings_dataPTSControl :: Lens.Lens' M3u8Settings (Prelude.Maybe M3u8DataPtsControl)
m3u8Settings_dataPTSControl = Lens.lens (\M3u8Settings' {dataPTSControl} -> dataPTSControl) (\s@M3u8Settings' {} a -> s {dataPTSControl = a} :: M3u8Settings)

-- | Specify the maximum time, in milliseconds, between Program Clock
-- References (PCRs) inserted into the transport stream.
m3u8Settings_maxPcrInterval :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Natural)
m3u8Settings_maxPcrInterval = Lens.lens (\M3u8Settings' {maxPcrInterval} -> maxPcrInterval) (\s@M3u8Settings' {} a -> s {maxPcrInterval = a} :: M3u8Settings)

-- | If INSERT, Nielsen inaudible tones for media tracking will be detected
-- in the input audio and an equivalent ID3 tag will be inserted in the
-- output.
m3u8Settings_nielsenId3 :: Lens.Lens' M3u8Settings (Prelude.Maybe M3u8NielsenId3)
m3u8Settings_nielsenId3 = Lens.lens (\M3u8Settings' {nielsenId3} -> nielsenId3) (\s@M3u8Settings' {} a -> s {nielsenId3 = a} :: M3u8Settings)

-- | The number of milliseconds between instances of this table in the output
-- transport stream.
m3u8Settings_patInterval :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Natural)
m3u8Settings_patInterval = Lens.lens (\M3u8Settings' {patInterval} -> patInterval) (\s@M3u8Settings' {} a -> s {patInterval = a} :: M3u8Settings)

-- | When set to PCR_EVERY_PES_PACKET a Program Clock Reference value is
-- inserted for every Packetized Elementary Stream (PES) header. This
-- parameter is effective only when the PCR PID is the same as the video or
-- audio elementary stream.
m3u8Settings_pcrControl :: Lens.Lens' M3u8Settings (Prelude.Maybe M3u8PcrControl)
m3u8Settings_pcrControl = Lens.lens (\M3u8Settings' {pcrControl} -> pcrControl) (\s@M3u8Settings' {} a -> s {pcrControl = a} :: M3u8Settings)

-- | Packet Identifier (PID) of the Program Clock Reference (PCR) in the
-- transport stream. When no value is given, the encoder will assign the
-- same value as the Video PID.
m3u8Settings_pcrPid :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Natural)
m3u8Settings_pcrPid = Lens.lens (\M3u8Settings' {pcrPid} -> pcrPid) (\s@M3u8Settings' {} a -> s {pcrPid = a} :: M3u8Settings)

-- | The number of milliseconds between instances of this table in the output
-- transport stream.
m3u8Settings_pmtInterval :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Natural)
m3u8Settings_pmtInterval = Lens.lens (\M3u8Settings' {pmtInterval} -> pmtInterval) (\s@M3u8Settings' {} a -> s {pmtInterval = a} :: M3u8Settings)

-- | Packet Identifier (PID) for the Program Map Table (PMT) in the transport
-- stream.
m3u8Settings_pmtPid :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Natural)
m3u8Settings_pmtPid = Lens.lens (\M3u8Settings' {pmtPid} -> pmtPid) (\s@M3u8Settings' {} a -> s {pmtPid = a} :: M3u8Settings)

-- | Packet Identifier (PID) of the private metadata stream in the transport
-- stream.
m3u8Settings_privateMetadataPid :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Natural)
m3u8Settings_privateMetadataPid = Lens.lens (\M3u8Settings' {privateMetadataPid} -> privateMetadataPid) (\s@M3u8Settings' {} a -> s {privateMetadataPid = a} :: M3u8Settings)

-- | The value of the program number field in the Program Map Table.
m3u8Settings_programNumber :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Natural)
m3u8Settings_programNumber = Lens.lens (\M3u8Settings' {programNumber} -> programNumber) (\s@M3u8Settings' {} a -> s {programNumber = a} :: M3u8Settings)

-- | Packet Identifier (PID) of the SCTE-35 stream in the transport stream.
m3u8Settings_scte35Pid :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Natural)
m3u8Settings_scte35Pid = Lens.lens (\M3u8Settings' {scte35Pid} -> scte35Pid) (\s@M3u8Settings' {} a -> s {scte35Pid = a} :: M3u8Settings)

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

-- | Set ID3 metadata (timedMetadata) to Passthrough (PASSTHROUGH) to include
-- ID3 metadata in this output. This includes ID3 metadata from the
-- following features: ID3 timestamp period (timedMetadataId3Period), and
-- Custom ID3 metadata inserter (timedMetadataInsertion). To exclude this
-- ID3 metadata in this output: set ID3 metadata to None (NONE) or leave
-- blank.
m3u8Settings_timedMetadata :: Lens.Lens' M3u8Settings (Prelude.Maybe TimedMetadata)
m3u8Settings_timedMetadata = Lens.lens (\M3u8Settings' {timedMetadata} -> timedMetadata) (\s@M3u8Settings' {} a -> s {timedMetadata = a} :: M3u8Settings)

-- | Packet Identifier (PID) of the ID3 metadata stream in the transport
-- stream.
m3u8Settings_timedMetadataPid :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Natural)
m3u8Settings_timedMetadataPid = Lens.lens (\M3u8Settings' {timedMetadataPid} -> timedMetadataPid) (\s@M3u8Settings' {} a -> s {timedMetadataPid = a} :: M3u8Settings)

-- | The value of the transport stream ID field in the Program Map Table.
m3u8Settings_transportStreamId :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Natural)
m3u8Settings_transportStreamId = Lens.lens (\M3u8Settings' {transportStreamId} -> transportStreamId) (\s@M3u8Settings' {} a -> s {transportStreamId = a} :: M3u8Settings)

-- | Packet Identifier (PID) of the elementary video stream in the transport
-- stream.
m3u8Settings_videoPid :: Lens.Lens' M3u8Settings (Prelude.Maybe Prelude.Natural)
m3u8Settings_videoPid = Lens.lens (\M3u8Settings' {videoPid} -> videoPid) (\s@M3u8Settings' {} a -> s {videoPid = a} :: M3u8Settings)

instance Data.FromJSON M3u8Settings where
  parseJSON =
    Data.withObject
      "M3u8Settings"
      ( \x ->
          M3u8Settings'
            Prelude.<$> (x Data..:? "audioDuration")
            Prelude.<*> (x Data..:? "audioFramesPerPes")
            Prelude.<*> (x Data..:? "audioPids" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "dataPTSControl")
            Prelude.<*> (x Data..:? "maxPcrInterval")
            Prelude.<*> (x Data..:? "nielsenId3")
            Prelude.<*> (x Data..:? "patInterval")
            Prelude.<*> (x Data..:? "pcrControl")
            Prelude.<*> (x Data..:? "pcrPid")
            Prelude.<*> (x Data..:? "pmtInterval")
            Prelude.<*> (x Data..:? "pmtPid")
            Prelude.<*> (x Data..:? "privateMetadataPid")
            Prelude.<*> (x Data..:? "programNumber")
            Prelude.<*> (x Data..:? "scte35Pid")
            Prelude.<*> (x Data..:? "scte35Source")
            Prelude.<*> (x Data..:? "timedMetadata")
            Prelude.<*> (x Data..:? "timedMetadataPid")
            Prelude.<*> (x Data..:? "transportStreamId")
            Prelude.<*> (x Data..:? "videoPid")
      )

instance Prelude.Hashable M3u8Settings where
  hashWithSalt _salt M3u8Settings' {..} =
    _salt `Prelude.hashWithSalt` audioDuration
      `Prelude.hashWithSalt` audioFramesPerPes
      `Prelude.hashWithSalt` audioPids
      `Prelude.hashWithSalt` dataPTSControl
      `Prelude.hashWithSalt` maxPcrInterval
      `Prelude.hashWithSalt` nielsenId3
      `Prelude.hashWithSalt` patInterval
      `Prelude.hashWithSalt` pcrControl
      `Prelude.hashWithSalt` pcrPid
      `Prelude.hashWithSalt` pmtInterval
      `Prelude.hashWithSalt` pmtPid
      `Prelude.hashWithSalt` privateMetadataPid
      `Prelude.hashWithSalt` programNumber
      `Prelude.hashWithSalt` scte35Pid
      `Prelude.hashWithSalt` scte35Source
      `Prelude.hashWithSalt` timedMetadata
      `Prelude.hashWithSalt` timedMetadataPid
      `Prelude.hashWithSalt` transportStreamId
      `Prelude.hashWithSalt` videoPid

instance Prelude.NFData M3u8Settings where
  rnf M3u8Settings' {..} =
    Prelude.rnf audioDuration
      `Prelude.seq` Prelude.rnf audioFramesPerPes
      `Prelude.seq` Prelude.rnf audioPids
      `Prelude.seq` Prelude.rnf dataPTSControl
      `Prelude.seq` Prelude.rnf maxPcrInterval
      `Prelude.seq` Prelude.rnf nielsenId3
      `Prelude.seq` Prelude.rnf patInterval
      `Prelude.seq` Prelude.rnf pcrControl
      `Prelude.seq` Prelude.rnf pcrPid
      `Prelude.seq` Prelude.rnf pmtInterval
      `Prelude.seq` Prelude.rnf pmtPid
      `Prelude.seq` Prelude.rnf privateMetadataPid
      `Prelude.seq` Prelude.rnf programNumber
      `Prelude.seq` Prelude.rnf scte35Pid
      `Prelude.seq` Prelude.rnf scte35Source
      `Prelude.seq` Prelude.rnf timedMetadata
      `Prelude.seq` Prelude.rnf timedMetadataPid
      `Prelude.seq` Prelude.rnf transportStreamId
      `Prelude.seq` Prelude.rnf videoPid

instance Data.ToJSON M3u8Settings where
  toJSON M3u8Settings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("audioDuration" Data..=) Prelude.<$> audioDuration,
            ("audioFramesPerPes" Data..=)
              Prelude.<$> audioFramesPerPes,
            ("audioPids" Data..=) Prelude.<$> audioPids,
            ("dataPTSControl" Data..=)
              Prelude.<$> dataPTSControl,
            ("maxPcrInterval" Data..=)
              Prelude.<$> maxPcrInterval,
            ("nielsenId3" Data..=) Prelude.<$> nielsenId3,
            ("patInterval" Data..=) Prelude.<$> patInterval,
            ("pcrControl" Data..=) Prelude.<$> pcrControl,
            ("pcrPid" Data..=) Prelude.<$> pcrPid,
            ("pmtInterval" Data..=) Prelude.<$> pmtInterval,
            ("pmtPid" Data..=) Prelude.<$> pmtPid,
            ("privateMetadataPid" Data..=)
              Prelude.<$> privateMetadataPid,
            ("programNumber" Data..=) Prelude.<$> programNumber,
            ("scte35Pid" Data..=) Prelude.<$> scte35Pid,
            ("scte35Source" Data..=) Prelude.<$> scte35Source,
            ("timedMetadata" Data..=) Prelude.<$> timedMetadata,
            ("timedMetadataPid" Data..=)
              Prelude.<$> timedMetadataPid,
            ("transportStreamId" Data..=)
              Prelude.<$> transportStreamId,
            ("videoPid" Data..=) Prelude.<$> videoPid
          ]
      )
