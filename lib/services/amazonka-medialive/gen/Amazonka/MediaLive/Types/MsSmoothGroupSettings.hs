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
-- Module      : Amazonka.MediaLive.Types.MsSmoothGroupSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.MsSmoothGroupSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.InputLossActionForMsSmoothOut
import Amazonka.MediaLive.Types.OutputLocationRef
import Amazonka.MediaLive.Types.SmoothGroupAudioOnlyTimecodeControl
import Amazonka.MediaLive.Types.SmoothGroupCertificateMode
import Amazonka.MediaLive.Types.SmoothGroupEventIdMode
import Amazonka.MediaLive.Types.SmoothGroupEventStopBehavior
import Amazonka.MediaLive.Types.SmoothGroupSegmentationMode
import Amazonka.MediaLive.Types.SmoothGroupSparseTrackType
import Amazonka.MediaLive.Types.SmoothGroupStreamManifestBehavior
import Amazonka.MediaLive.Types.SmoothGroupTimestampOffsetMode
import qualified Amazonka.Prelude as Prelude

-- | Ms Smooth Group Settings
--
-- /See:/ 'newMsSmoothGroupSettings' smart constructor.
data MsSmoothGroupSettings = MsSmoothGroupSettings'
  { -- | The ID to include in each message in the sparse track. Ignored if
    -- sparseTrackType is NONE.
    acquisitionPointId :: Prelude.Maybe Prelude.Text,
    -- | If set to passthrough for an audio-only MS Smooth output, the fragment
    -- absolute time will be set to the current timecode. This option does not
    -- write timecodes to the audio elementary stream.
    audioOnlyTimecodeControl :: Prelude.Maybe SmoothGroupAudioOnlyTimecodeControl,
    -- | If set to verifyAuthenticity, verify the https certificate chain to a
    -- trusted Certificate Authority (CA). This will cause https outputs to
    -- self-signed certificates to fail.
    certificateMode :: Prelude.Maybe SmoothGroupCertificateMode,
    -- | Number of seconds to wait before retrying connection to the IIS server
    -- if the connection is lost. Content will be cached during this time and
    -- the cache will be be delivered to the IIS server once the connection is
    -- re-established.
    connectionRetryInterval :: Prelude.Maybe Prelude.Natural,
    -- | MS Smooth event ID to be sent to the IIS server. Should only be
    -- specified if eventIdMode is set to useConfigured.
    eventId :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether or not to send an event ID to the IIS server. If no
    -- event ID is sent and the same Live Event is used without changing the
    -- publishing point, clients might see cached video from the previous run.
    -- Options: - \"useConfigured\" - use the value provided in eventId -
    -- \"useTimestamp\" - generate and send an event ID based on the current
    -- timestamp - \"noEventId\" - do not send an event ID to the IIS server.
    eventIdMode :: Prelude.Maybe SmoothGroupEventIdMode,
    -- | When set to sendEos, send EOS signal to IIS server when stopping the
    -- event
    eventStopBehavior :: Prelude.Maybe SmoothGroupEventStopBehavior,
    -- | Size in seconds of file cache for streaming outputs.
    filecacheDuration :: Prelude.Maybe Prelude.Natural,
    -- | Length of mp4 fragments to generate (in seconds). Fragment length must
    -- be compatible with GOP size and framerate.
    fragmentLength :: Prelude.Maybe Prelude.Natural,
    -- | Parameter that control output group behavior on input loss.
    inputLossAction :: Prelude.Maybe InputLossActionForMsSmoothOut,
    -- | Number of retry attempts.
    numRetries :: Prelude.Maybe Prelude.Natural,
    -- | Number of seconds before initiating a restart due to output failure, due
    -- to exhausting the numRetries on one segment, or exceeding
    -- filecacheDuration.
    restartDelay :: Prelude.Maybe Prelude.Natural,
    -- | useInputSegmentation has been deprecated. The configured segment size is
    -- always used.
    segmentationMode :: Prelude.Maybe SmoothGroupSegmentationMode,
    -- | Number of milliseconds to delay the output from the second pipeline.
    sendDelayMs :: Prelude.Maybe Prelude.Natural,
    -- | Identifies the type of data to place in the sparse track: - SCTE35:
    -- Insert SCTE-35 messages from the source content. With each message,
    -- insert an IDR frame to start a new segment. -
    -- SCTE35_WITHOUT_SEGMENTATION: Insert SCTE-35 messages from the source
    -- content. With each message, insert an IDR frame but don\'t start a new
    -- segment. - NONE: Don\'t generate a sparse track for any outputs in this
    -- output group.
    sparseTrackType :: Prelude.Maybe SmoothGroupSparseTrackType,
    -- | When set to send, send stream manifest so publishing point doesn\'t
    -- start until all streams start.
    streamManifestBehavior :: Prelude.Maybe SmoothGroupStreamManifestBehavior,
    -- | Timestamp offset for the event. Only used if timestampOffsetMode is set
    -- to useConfiguredOffset.
    timestampOffset :: Prelude.Maybe Prelude.Text,
    -- | Type of timestamp date offset to use. - useEventStartDate: Use the date
    -- the event was started as the offset - useConfiguredOffset: Use an
    -- explicitly configured date as the offset
    timestampOffsetMode :: Prelude.Maybe SmoothGroupTimestampOffsetMode,
    -- | Smooth Streaming publish point on an IIS server. Elemental Live acts as
    -- a \"Push\" encoder to IIS.
    destination :: OutputLocationRef
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MsSmoothGroupSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acquisitionPointId', 'msSmoothGroupSettings_acquisitionPointId' - The ID to include in each message in the sparse track. Ignored if
-- sparseTrackType is NONE.
--
-- 'audioOnlyTimecodeControl', 'msSmoothGroupSettings_audioOnlyTimecodeControl' - If set to passthrough for an audio-only MS Smooth output, the fragment
-- absolute time will be set to the current timecode. This option does not
-- write timecodes to the audio elementary stream.
--
-- 'certificateMode', 'msSmoothGroupSettings_certificateMode' - If set to verifyAuthenticity, verify the https certificate chain to a
-- trusted Certificate Authority (CA). This will cause https outputs to
-- self-signed certificates to fail.
--
-- 'connectionRetryInterval', 'msSmoothGroupSettings_connectionRetryInterval' - Number of seconds to wait before retrying connection to the IIS server
-- if the connection is lost. Content will be cached during this time and
-- the cache will be be delivered to the IIS server once the connection is
-- re-established.
--
-- 'eventId', 'msSmoothGroupSettings_eventId' - MS Smooth event ID to be sent to the IIS server. Should only be
-- specified if eventIdMode is set to useConfigured.
--
-- 'eventIdMode', 'msSmoothGroupSettings_eventIdMode' - Specifies whether or not to send an event ID to the IIS server. If no
-- event ID is sent and the same Live Event is used without changing the
-- publishing point, clients might see cached video from the previous run.
-- Options: - \"useConfigured\" - use the value provided in eventId -
-- \"useTimestamp\" - generate and send an event ID based on the current
-- timestamp - \"noEventId\" - do not send an event ID to the IIS server.
--
-- 'eventStopBehavior', 'msSmoothGroupSettings_eventStopBehavior' - When set to sendEos, send EOS signal to IIS server when stopping the
-- event
--
-- 'filecacheDuration', 'msSmoothGroupSettings_filecacheDuration' - Size in seconds of file cache for streaming outputs.
--
-- 'fragmentLength', 'msSmoothGroupSettings_fragmentLength' - Length of mp4 fragments to generate (in seconds). Fragment length must
-- be compatible with GOP size and framerate.
--
-- 'inputLossAction', 'msSmoothGroupSettings_inputLossAction' - Parameter that control output group behavior on input loss.
--
-- 'numRetries', 'msSmoothGroupSettings_numRetries' - Number of retry attempts.
--
-- 'restartDelay', 'msSmoothGroupSettings_restartDelay' - Number of seconds before initiating a restart due to output failure, due
-- to exhausting the numRetries on one segment, or exceeding
-- filecacheDuration.
--
-- 'segmentationMode', 'msSmoothGroupSettings_segmentationMode' - useInputSegmentation has been deprecated. The configured segment size is
-- always used.
--
-- 'sendDelayMs', 'msSmoothGroupSettings_sendDelayMs' - Number of milliseconds to delay the output from the second pipeline.
--
-- 'sparseTrackType', 'msSmoothGroupSettings_sparseTrackType' - Identifies the type of data to place in the sparse track: - SCTE35:
-- Insert SCTE-35 messages from the source content. With each message,
-- insert an IDR frame to start a new segment. -
-- SCTE35_WITHOUT_SEGMENTATION: Insert SCTE-35 messages from the source
-- content. With each message, insert an IDR frame but don\'t start a new
-- segment. - NONE: Don\'t generate a sparse track for any outputs in this
-- output group.
--
-- 'streamManifestBehavior', 'msSmoothGroupSettings_streamManifestBehavior' - When set to send, send stream manifest so publishing point doesn\'t
-- start until all streams start.
--
-- 'timestampOffset', 'msSmoothGroupSettings_timestampOffset' - Timestamp offset for the event. Only used if timestampOffsetMode is set
-- to useConfiguredOffset.
--
-- 'timestampOffsetMode', 'msSmoothGroupSettings_timestampOffsetMode' - Type of timestamp date offset to use. - useEventStartDate: Use the date
-- the event was started as the offset - useConfiguredOffset: Use an
-- explicitly configured date as the offset
--
-- 'destination', 'msSmoothGroupSettings_destination' - Smooth Streaming publish point on an IIS server. Elemental Live acts as
-- a \"Push\" encoder to IIS.
newMsSmoothGroupSettings ::
  -- | 'destination'
  OutputLocationRef ->
  MsSmoothGroupSettings
newMsSmoothGroupSettings pDestination_ =
  MsSmoothGroupSettings'
    { acquisitionPointId =
        Prelude.Nothing,
      audioOnlyTimecodeControl = Prelude.Nothing,
      certificateMode = Prelude.Nothing,
      connectionRetryInterval = Prelude.Nothing,
      eventId = Prelude.Nothing,
      eventIdMode = Prelude.Nothing,
      eventStopBehavior = Prelude.Nothing,
      filecacheDuration = Prelude.Nothing,
      fragmentLength = Prelude.Nothing,
      inputLossAction = Prelude.Nothing,
      numRetries = Prelude.Nothing,
      restartDelay = Prelude.Nothing,
      segmentationMode = Prelude.Nothing,
      sendDelayMs = Prelude.Nothing,
      sparseTrackType = Prelude.Nothing,
      streamManifestBehavior = Prelude.Nothing,
      timestampOffset = Prelude.Nothing,
      timestampOffsetMode = Prelude.Nothing,
      destination = pDestination_
    }

-- | The ID to include in each message in the sparse track. Ignored if
-- sparseTrackType is NONE.
msSmoothGroupSettings_acquisitionPointId :: Lens.Lens' MsSmoothGroupSettings (Prelude.Maybe Prelude.Text)
msSmoothGroupSettings_acquisitionPointId = Lens.lens (\MsSmoothGroupSettings' {acquisitionPointId} -> acquisitionPointId) (\s@MsSmoothGroupSettings' {} a -> s {acquisitionPointId = a} :: MsSmoothGroupSettings)

-- | If set to passthrough for an audio-only MS Smooth output, the fragment
-- absolute time will be set to the current timecode. This option does not
-- write timecodes to the audio elementary stream.
msSmoothGroupSettings_audioOnlyTimecodeControl :: Lens.Lens' MsSmoothGroupSettings (Prelude.Maybe SmoothGroupAudioOnlyTimecodeControl)
msSmoothGroupSettings_audioOnlyTimecodeControl = Lens.lens (\MsSmoothGroupSettings' {audioOnlyTimecodeControl} -> audioOnlyTimecodeControl) (\s@MsSmoothGroupSettings' {} a -> s {audioOnlyTimecodeControl = a} :: MsSmoothGroupSettings)

-- | If set to verifyAuthenticity, verify the https certificate chain to a
-- trusted Certificate Authority (CA). This will cause https outputs to
-- self-signed certificates to fail.
msSmoothGroupSettings_certificateMode :: Lens.Lens' MsSmoothGroupSettings (Prelude.Maybe SmoothGroupCertificateMode)
msSmoothGroupSettings_certificateMode = Lens.lens (\MsSmoothGroupSettings' {certificateMode} -> certificateMode) (\s@MsSmoothGroupSettings' {} a -> s {certificateMode = a} :: MsSmoothGroupSettings)

-- | Number of seconds to wait before retrying connection to the IIS server
-- if the connection is lost. Content will be cached during this time and
-- the cache will be be delivered to the IIS server once the connection is
-- re-established.
msSmoothGroupSettings_connectionRetryInterval :: Lens.Lens' MsSmoothGroupSettings (Prelude.Maybe Prelude.Natural)
msSmoothGroupSettings_connectionRetryInterval = Lens.lens (\MsSmoothGroupSettings' {connectionRetryInterval} -> connectionRetryInterval) (\s@MsSmoothGroupSettings' {} a -> s {connectionRetryInterval = a} :: MsSmoothGroupSettings)

-- | MS Smooth event ID to be sent to the IIS server. Should only be
-- specified if eventIdMode is set to useConfigured.
msSmoothGroupSettings_eventId :: Lens.Lens' MsSmoothGroupSettings (Prelude.Maybe Prelude.Text)
msSmoothGroupSettings_eventId = Lens.lens (\MsSmoothGroupSettings' {eventId} -> eventId) (\s@MsSmoothGroupSettings' {} a -> s {eventId = a} :: MsSmoothGroupSettings)

-- | Specifies whether or not to send an event ID to the IIS server. If no
-- event ID is sent and the same Live Event is used without changing the
-- publishing point, clients might see cached video from the previous run.
-- Options: - \"useConfigured\" - use the value provided in eventId -
-- \"useTimestamp\" - generate and send an event ID based on the current
-- timestamp - \"noEventId\" - do not send an event ID to the IIS server.
msSmoothGroupSettings_eventIdMode :: Lens.Lens' MsSmoothGroupSettings (Prelude.Maybe SmoothGroupEventIdMode)
msSmoothGroupSettings_eventIdMode = Lens.lens (\MsSmoothGroupSettings' {eventIdMode} -> eventIdMode) (\s@MsSmoothGroupSettings' {} a -> s {eventIdMode = a} :: MsSmoothGroupSettings)

-- | When set to sendEos, send EOS signal to IIS server when stopping the
-- event
msSmoothGroupSettings_eventStopBehavior :: Lens.Lens' MsSmoothGroupSettings (Prelude.Maybe SmoothGroupEventStopBehavior)
msSmoothGroupSettings_eventStopBehavior = Lens.lens (\MsSmoothGroupSettings' {eventStopBehavior} -> eventStopBehavior) (\s@MsSmoothGroupSettings' {} a -> s {eventStopBehavior = a} :: MsSmoothGroupSettings)

-- | Size in seconds of file cache for streaming outputs.
msSmoothGroupSettings_filecacheDuration :: Lens.Lens' MsSmoothGroupSettings (Prelude.Maybe Prelude.Natural)
msSmoothGroupSettings_filecacheDuration = Lens.lens (\MsSmoothGroupSettings' {filecacheDuration} -> filecacheDuration) (\s@MsSmoothGroupSettings' {} a -> s {filecacheDuration = a} :: MsSmoothGroupSettings)

-- | Length of mp4 fragments to generate (in seconds). Fragment length must
-- be compatible with GOP size and framerate.
msSmoothGroupSettings_fragmentLength :: Lens.Lens' MsSmoothGroupSettings (Prelude.Maybe Prelude.Natural)
msSmoothGroupSettings_fragmentLength = Lens.lens (\MsSmoothGroupSettings' {fragmentLength} -> fragmentLength) (\s@MsSmoothGroupSettings' {} a -> s {fragmentLength = a} :: MsSmoothGroupSettings)

-- | Parameter that control output group behavior on input loss.
msSmoothGroupSettings_inputLossAction :: Lens.Lens' MsSmoothGroupSettings (Prelude.Maybe InputLossActionForMsSmoothOut)
msSmoothGroupSettings_inputLossAction = Lens.lens (\MsSmoothGroupSettings' {inputLossAction} -> inputLossAction) (\s@MsSmoothGroupSettings' {} a -> s {inputLossAction = a} :: MsSmoothGroupSettings)

-- | Number of retry attempts.
msSmoothGroupSettings_numRetries :: Lens.Lens' MsSmoothGroupSettings (Prelude.Maybe Prelude.Natural)
msSmoothGroupSettings_numRetries = Lens.lens (\MsSmoothGroupSettings' {numRetries} -> numRetries) (\s@MsSmoothGroupSettings' {} a -> s {numRetries = a} :: MsSmoothGroupSettings)

-- | Number of seconds before initiating a restart due to output failure, due
-- to exhausting the numRetries on one segment, or exceeding
-- filecacheDuration.
msSmoothGroupSettings_restartDelay :: Lens.Lens' MsSmoothGroupSettings (Prelude.Maybe Prelude.Natural)
msSmoothGroupSettings_restartDelay = Lens.lens (\MsSmoothGroupSettings' {restartDelay} -> restartDelay) (\s@MsSmoothGroupSettings' {} a -> s {restartDelay = a} :: MsSmoothGroupSettings)

-- | useInputSegmentation has been deprecated. The configured segment size is
-- always used.
msSmoothGroupSettings_segmentationMode :: Lens.Lens' MsSmoothGroupSettings (Prelude.Maybe SmoothGroupSegmentationMode)
msSmoothGroupSettings_segmentationMode = Lens.lens (\MsSmoothGroupSettings' {segmentationMode} -> segmentationMode) (\s@MsSmoothGroupSettings' {} a -> s {segmentationMode = a} :: MsSmoothGroupSettings)

-- | Number of milliseconds to delay the output from the second pipeline.
msSmoothGroupSettings_sendDelayMs :: Lens.Lens' MsSmoothGroupSettings (Prelude.Maybe Prelude.Natural)
msSmoothGroupSettings_sendDelayMs = Lens.lens (\MsSmoothGroupSettings' {sendDelayMs} -> sendDelayMs) (\s@MsSmoothGroupSettings' {} a -> s {sendDelayMs = a} :: MsSmoothGroupSettings)

-- | Identifies the type of data to place in the sparse track: - SCTE35:
-- Insert SCTE-35 messages from the source content. With each message,
-- insert an IDR frame to start a new segment. -
-- SCTE35_WITHOUT_SEGMENTATION: Insert SCTE-35 messages from the source
-- content. With each message, insert an IDR frame but don\'t start a new
-- segment. - NONE: Don\'t generate a sparse track for any outputs in this
-- output group.
msSmoothGroupSettings_sparseTrackType :: Lens.Lens' MsSmoothGroupSettings (Prelude.Maybe SmoothGroupSparseTrackType)
msSmoothGroupSettings_sparseTrackType = Lens.lens (\MsSmoothGroupSettings' {sparseTrackType} -> sparseTrackType) (\s@MsSmoothGroupSettings' {} a -> s {sparseTrackType = a} :: MsSmoothGroupSettings)

-- | When set to send, send stream manifest so publishing point doesn\'t
-- start until all streams start.
msSmoothGroupSettings_streamManifestBehavior :: Lens.Lens' MsSmoothGroupSettings (Prelude.Maybe SmoothGroupStreamManifestBehavior)
msSmoothGroupSettings_streamManifestBehavior = Lens.lens (\MsSmoothGroupSettings' {streamManifestBehavior} -> streamManifestBehavior) (\s@MsSmoothGroupSettings' {} a -> s {streamManifestBehavior = a} :: MsSmoothGroupSettings)

-- | Timestamp offset for the event. Only used if timestampOffsetMode is set
-- to useConfiguredOffset.
msSmoothGroupSettings_timestampOffset :: Lens.Lens' MsSmoothGroupSettings (Prelude.Maybe Prelude.Text)
msSmoothGroupSettings_timestampOffset = Lens.lens (\MsSmoothGroupSettings' {timestampOffset} -> timestampOffset) (\s@MsSmoothGroupSettings' {} a -> s {timestampOffset = a} :: MsSmoothGroupSettings)

-- | Type of timestamp date offset to use. - useEventStartDate: Use the date
-- the event was started as the offset - useConfiguredOffset: Use an
-- explicitly configured date as the offset
msSmoothGroupSettings_timestampOffsetMode :: Lens.Lens' MsSmoothGroupSettings (Prelude.Maybe SmoothGroupTimestampOffsetMode)
msSmoothGroupSettings_timestampOffsetMode = Lens.lens (\MsSmoothGroupSettings' {timestampOffsetMode} -> timestampOffsetMode) (\s@MsSmoothGroupSettings' {} a -> s {timestampOffsetMode = a} :: MsSmoothGroupSettings)

-- | Smooth Streaming publish point on an IIS server. Elemental Live acts as
-- a \"Push\" encoder to IIS.
msSmoothGroupSettings_destination :: Lens.Lens' MsSmoothGroupSettings OutputLocationRef
msSmoothGroupSettings_destination = Lens.lens (\MsSmoothGroupSettings' {destination} -> destination) (\s@MsSmoothGroupSettings' {} a -> s {destination = a} :: MsSmoothGroupSettings)

instance Data.FromJSON MsSmoothGroupSettings where
  parseJSON =
    Data.withObject
      "MsSmoothGroupSettings"
      ( \x ->
          MsSmoothGroupSettings'
            Prelude.<$> (x Data..:? "acquisitionPointId")
            Prelude.<*> (x Data..:? "audioOnlyTimecodeControl")
            Prelude.<*> (x Data..:? "certificateMode")
            Prelude.<*> (x Data..:? "connectionRetryInterval")
            Prelude.<*> (x Data..:? "eventId")
            Prelude.<*> (x Data..:? "eventIdMode")
            Prelude.<*> (x Data..:? "eventStopBehavior")
            Prelude.<*> (x Data..:? "filecacheDuration")
            Prelude.<*> (x Data..:? "fragmentLength")
            Prelude.<*> (x Data..:? "inputLossAction")
            Prelude.<*> (x Data..:? "numRetries")
            Prelude.<*> (x Data..:? "restartDelay")
            Prelude.<*> (x Data..:? "segmentationMode")
            Prelude.<*> (x Data..:? "sendDelayMs")
            Prelude.<*> (x Data..:? "sparseTrackType")
            Prelude.<*> (x Data..:? "streamManifestBehavior")
            Prelude.<*> (x Data..:? "timestampOffset")
            Prelude.<*> (x Data..:? "timestampOffsetMode")
            Prelude.<*> (x Data..: "destination")
      )

instance Prelude.Hashable MsSmoothGroupSettings where
  hashWithSalt _salt MsSmoothGroupSettings' {..} =
    _salt
      `Prelude.hashWithSalt` acquisitionPointId
      `Prelude.hashWithSalt` audioOnlyTimecodeControl
      `Prelude.hashWithSalt` certificateMode
      `Prelude.hashWithSalt` connectionRetryInterval
      `Prelude.hashWithSalt` eventId
      `Prelude.hashWithSalt` eventIdMode
      `Prelude.hashWithSalt` eventStopBehavior
      `Prelude.hashWithSalt` filecacheDuration
      `Prelude.hashWithSalt` fragmentLength
      `Prelude.hashWithSalt` inputLossAction
      `Prelude.hashWithSalt` numRetries
      `Prelude.hashWithSalt` restartDelay
      `Prelude.hashWithSalt` segmentationMode
      `Prelude.hashWithSalt` sendDelayMs
      `Prelude.hashWithSalt` sparseTrackType
      `Prelude.hashWithSalt` streamManifestBehavior
      `Prelude.hashWithSalt` timestampOffset
      `Prelude.hashWithSalt` timestampOffsetMode
      `Prelude.hashWithSalt` destination

instance Prelude.NFData MsSmoothGroupSettings where
  rnf MsSmoothGroupSettings' {..} =
    Prelude.rnf acquisitionPointId `Prelude.seq`
      Prelude.rnf audioOnlyTimecodeControl `Prelude.seq`
        Prelude.rnf certificateMode `Prelude.seq`
          Prelude.rnf connectionRetryInterval `Prelude.seq`
            Prelude.rnf eventId `Prelude.seq`
              Prelude.rnf eventIdMode `Prelude.seq`
                Prelude.rnf eventStopBehavior `Prelude.seq`
                  Prelude.rnf filecacheDuration `Prelude.seq`
                    Prelude.rnf fragmentLength `Prelude.seq`
                      Prelude.rnf inputLossAction `Prelude.seq`
                        Prelude.rnf numRetries `Prelude.seq`
                          Prelude.rnf restartDelay `Prelude.seq`
                            Prelude.rnf segmentationMode `Prelude.seq`
                              Prelude.rnf sendDelayMs `Prelude.seq`
                                Prelude.rnf sparseTrackType `Prelude.seq`
                                  Prelude.rnf streamManifestBehavior `Prelude.seq`
                                    Prelude.rnf timestampOffset `Prelude.seq`
                                      Prelude.rnf timestampOffsetMode `Prelude.seq`
                                        Prelude.rnf destination

instance Data.ToJSON MsSmoothGroupSettings where
  toJSON MsSmoothGroupSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("acquisitionPointId" Data..=)
              Prelude.<$> acquisitionPointId,
            ("audioOnlyTimecodeControl" Data..=)
              Prelude.<$> audioOnlyTimecodeControl,
            ("certificateMode" Data..=)
              Prelude.<$> certificateMode,
            ("connectionRetryInterval" Data..=)
              Prelude.<$> connectionRetryInterval,
            ("eventId" Data..=) Prelude.<$> eventId,
            ("eventIdMode" Data..=) Prelude.<$> eventIdMode,
            ("eventStopBehavior" Data..=)
              Prelude.<$> eventStopBehavior,
            ("filecacheDuration" Data..=)
              Prelude.<$> filecacheDuration,
            ("fragmentLength" Data..=)
              Prelude.<$> fragmentLength,
            ("inputLossAction" Data..=)
              Prelude.<$> inputLossAction,
            ("numRetries" Data..=) Prelude.<$> numRetries,
            ("restartDelay" Data..=) Prelude.<$> restartDelay,
            ("segmentationMode" Data..=)
              Prelude.<$> segmentationMode,
            ("sendDelayMs" Data..=) Prelude.<$> sendDelayMs,
            ("sparseTrackType" Data..=)
              Prelude.<$> sparseTrackType,
            ("streamManifestBehavior" Data..=)
              Prelude.<$> streamManifestBehavior,
            ("timestampOffset" Data..=)
              Prelude.<$> timestampOffset,
            ("timestampOffsetMode" Data..=)
              Prelude.<$> timestampOffsetMode,
            Prelude.Just ("destination" Data..= destination)
          ]
      )
