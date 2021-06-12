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
-- Module      : Network.AWS.MediaLive.Types.MsSmoothGroupSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MsSmoothGroupSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputLossActionForMsSmoothOut
import Network.AWS.MediaLive.Types.OutputLocationRef
import Network.AWS.MediaLive.Types.SmoothGroupAudioOnlyTimecodeControl
import Network.AWS.MediaLive.Types.SmoothGroupCertificateMode
import Network.AWS.MediaLive.Types.SmoothGroupEventIdMode
import Network.AWS.MediaLive.Types.SmoothGroupEventStopBehavior
import Network.AWS.MediaLive.Types.SmoothGroupSegmentationMode
import Network.AWS.MediaLive.Types.SmoothGroupSparseTrackType
import Network.AWS.MediaLive.Types.SmoothGroupStreamManifestBehavior
import Network.AWS.MediaLive.Types.SmoothGroupTimestampOffsetMode

-- | Ms Smooth Group Settings
--
-- /See:/ 'newMsSmoothGroupSettings' smart constructor.
data MsSmoothGroupSettings = MsSmoothGroupSettings'
  { -- | When set to send, send stream manifest so publishing point doesn\'t
    -- start until all streams start.
    streamManifestBehavior :: Core.Maybe SmoothGroupStreamManifestBehavior,
    -- | Size in seconds of file cache for streaming outputs.
    filecacheDuration :: Core.Maybe Core.Natural,
    -- | Length of mp4 fragments to generate (in seconds). Fragment length must
    -- be compatible with GOP size and framerate.
    fragmentLength :: Core.Maybe Core.Natural,
    -- | MS Smooth event ID to be sent to the IIS server. Should only be
    -- specified if eventIdMode is set to useConfigured.
    eventId :: Core.Maybe Core.Text,
    -- | If set to verifyAuthenticity, verify the https certificate chain to a
    -- trusted Certificate Authority (CA). This will cause https outputs to
    -- self-signed certificates to fail.
    certificateMode :: Core.Maybe SmoothGroupCertificateMode,
    -- | Number of retry attempts.
    numRetries :: Core.Maybe Core.Natural,
    -- | The ID to include in each message in the sparse track. Ignored if
    -- sparseTrackType is NONE.
    acquisitionPointId :: Core.Maybe Core.Text,
    -- | If set to passthrough for an audio-only MS Smooth output, the fragment
    -- absolute time will be set to the current timecode. This option does not
    -- write timecodes to the audio elementary stream.
    audioOnlyTimecodeControl :: Core.Maybe SmoothGroupAudioOnlyTimecodeControl,
    -- | useInputSegmentation has been deprecated. The configured segment size is
    -- always used.
    segmentationMode :: Core.Maybe SmoothGroupSegmentationMode,
    -- | Specifies whether or not to send an event ID to the IIS server. If no
    -- event ID is sent and the same Live Event is used without changing the
    -- publishing point, clients might see cached video from the previous run.
    -- Options: - \"useConfigured\" - use the value provided in eventId -
    -- \"useTimestamp\" - generate and send an event ID based on the current
    -- timestamp - \"noEventId\" - do not send an event ID to the IIS server.
    eventIdMode :: Core.Maybe SmoothGroupEventIdMode,
    -- | Number of milliseconds to delay the output from the second pipeline.
    sendDelayMs :: Core.Maybe Core.Natural,
    -- | Number of seconds to wait before retrying connection to the IIS server
    -- if the connection is lost. Content will be cached during this time and
    -- the cache will be be delivered to the IIS server once the connection is
    -- re-established.
    connectionRetryInterval :: Core.Maybe Core.Natural,
    -- | Identifies the type of data to place in the sparse track: - SCTE35:
    -- Insert SCTE-35 messages from the source content. With each message,
    -- insert an IDR frame to start a new segment. -
    -- SCTE35_WITHOUT_SEGMENTATION: Insert SCTE-35 messages from the source
    -- content. With each message, insert an IDR frame but don\'t start a new
    -- segment. - NONE: Don\'t generate a sparse track for any outputs in this
    -- output group.
    sparseTrackType :: Core.Maybe SmoothGroupSparseTrackType,
    -- | Parameter that control output group behavior on input loss.
    inputLossAction :: Core.Maybe InputLossActionForMsSmoothOut,
    -- | Timestamp offset for the event. Only used if timestampOffsetMode is set
    -- to useConfiguredOffset.
    timestampOffset :: Core.Maybe Core.Text,
    -- | When set to sendEos, send EOS signal to IIS server when stopping the
    -- event
    eventStopBehavior :: Core.Maybe SmoothGroupEventStopBehavior,
    -- | Type of timestamp date offset to use. - useEventStartDate: Use the date
    -- the event was started as the offset - useConfiguredOffset: Use an
    -- explicitly configured date as the offset
    timestampOffsetMode :: Core.Maybe SmoothGroupTimestampOffsetMode,
    -- | Number of seconds before initiating a restart due to output failure, due
    -- to exhausting the numRetries on one segment, or exceeding
    -- filecacheDuration.
    restartDelay :: Core.Maybe Core.Natural,
    -- | Smooth Streaming publish point on an IIS server. Elemental Live acts as
    -- a \"Push\" encoder to IIS.
    destination :: OutputLocationRef
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MsSmoothGroupSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamManifestBehavior', 'msSmoothGroupSettings_streamManifestBehavior' - When set to send, send stream manifest so publishing point doesn\'t
-- start until all streams start.
--
-- 'filecacheDuration', 'msSmoothGroupSettings_filecacheDuration' - Size in seconds of file cache for streaming outputs.
--
-- 'fragmentLength', 'msSmoothGroupSettings_fragmentLength' - Length of mp4 fragments to generate (in seconds). Fragment length must
-- be compatible with GOP size and framerate.
--
-- 'eventId', 'msSmoothGroupSettings_eventId' - MS Smooth event ID to be sent to the IIS server. Should only be
-- specified if eventIdMode is set to useConfigured.
--
-- 'certificateMode', 'msSmoothGroupSettings_certificateMode' - If set to verifyAuthenticity, verify the https certificate chain to a
-- trusted Certificate Authority (CA). This will cause https outputs to
-- self-signed certificates to fail.
--
-- 'numRetries', 'msSmoothGroupSettings_numRetries' - Number of retry attempts.
--
-- 'acquisitionPointId', 'msSmoothGroupSettings_acquisitionPointId' - The ID to include in each message in the sparse track. Ignored if
-- sparseTrackType is NONE.
--
-- 'audioOnlyTimecodeControl', 'msSmoothGroupSettings_audioOnlyTimecodeControl' - If set to passthrough for an audio-only MS Smooth output, the fragment
-- absolute time will be set to the current timecode. This option does not
-- write timecodes to the audio elementary stream.
--
-- 'segmentationMode', 'msSmoothGroupSettings_segmentationMode' - useInputSegmentation has been deprecated. The configured segment size is
-- always used.
--
-- 'eventIdMode', 'msSmoothGroupSettings_eventIdMode' - Specifies whether or not to send an event ID to the IIS server. If no
-- event ID is sent and the same Live Event is used without changing the
-- publishing point, clients might see cached video from the previous run.
-- Options: - \"useConfigured\" - use the value provided in eventId -
-- \"useTimestamp\" - generate and send an event ID based on the current
-- timestamp - \"noEventId\" - do not send an event ID to the IIS server.
--
-- 'sendDelayMs', 'msSmoothGroupSettings_sendDelayMs' - Number of milliseconds to delay the output from the second pipeline.
--
-- 'connectionRetryInterval', 'msSmoothGroupSettings_connectionRetryInterval' - Number of seconds to wait before retrying connection to the IIS server
-- if the connection is lost. Content will be cached during this time and
-- the cache will be be delivered to the IIS server once the connection is
-- re-established.
--
-- 'sparseTrackType', 'msSmoothGroupSettings_sparseTrackType' - Identifies the type of data to place in the sparse track: - SCTE35:
-- Insert SCTE-35 messages from the source content. With each message,
-- insert an IDR frame to start a new segment. -
-- SCTE35_WITHOUT_SEGMENTATION: Insert SCTE-35 messages from the source
-- content. With each message, insert an IDR frame but don\'t start a new
-- segment. - NONE: Don\'t generate a sparse track for any outputs in this
-- output group.
--
-- 'inputLossAction', 'msSmoothGroupSettings_inputLossAction' - Parameter that control output group behavior on input loss.
--
-- 'timestampOffset', 'msSmoothGroupSettings_timestampOffset' - Timestamp offset for the event. Only used if timestampOffsetMode is set
-- to useConfiguredOffset.
--
-- 'eventStopBehavior', 'msSmoothGroupSettings_eventStopBehavior' - When set to sendEos, send EOS signal to IIS server when stopping the
-- event
--
-- 'timestampOffsetMode', 'msSmoothGroupSettings_timestampOffsetMode' - Type of timestamp date offset to use. - useEventStartDate: Use the date
-- the event was started as the offset - useConfiguredOffset: Use an
-- explicitly configured date as the offset
--
-- 'restartDelay', 'msSmoothGroupSettings_restartDelay' - Number of seconds before initiating a restart due to output failure, due
-- to exhausting the numRetries on one segment, or exceeding
-- filecacheDuration.
--
-- 'destination', 'msSmoothGroupSettings_destination' - Smooth Streaming publish point on an IIS server. Elemental Live acts as
-- a \"Push\" encoder to IIS.
newMsSmoothGroupSettings ::
  -- | 'destination'
  OutputLocationRef ->
  MsSmoothGroupSettings
newMsSmoothGroupSettings pDestination_ =
  MsSmoothGroupSettings'
    { streamManifestBehavior =
        Core.Nothing,
      filecacheDuration = Core.Nothing,
      fragmentLength = Core.Nothing,
      eventId = Core.Nothing,
      certificateMode = Core.Nothing,
      numRetries = Core.Nothing,
      acquisitionPointId = Core.Nothing,
      audioOnlyTimecodeControl = Core.Nothing,
      segmentationMode = Core.Nothing,
      eventIdMode = Core.Nothing,
      sendDelayMs = Core.Nothing,
      connectionRetryInterval = Core.Nothing,
      sparseTrackType = Core.Nothing,
      inputLossAction = Core.Nothing,
      timestampOffset = Core.Nothing,
      eventStopBehavior = Core.Nothing,
      timestampOffsetMode = Core.Nothing,
      restartDelay = Core.Nothing,
      destination = pDestination_
    }

-- | When set to send, send stream manifest so publishing point doesn\'t
-- start until all streams start.
msSmoothGroupSettings_streamManifestBehavior :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe SmoothGroupStreamManifestBehavior)
msSmoothGroupSettings_streamManifestBehavior = Lens.lens (\MsSmoothGroupSettings' {streamManifestBehavior} -> streamManifestBehavior) (\s@MsSmoothGroupSettings' {} a -> s {streamManifestBehavior = a} :: MsSmoothGroupSettings)

-- | Size in seconds of file cache for streaming outputs.
msSmoothGroupSettings_filecacheDuration :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Core.Natural)
msSmoothGroupSettings_filecacheDuration = Lens.lens (\MsSmoothGroupSettings' {filecacheDuration} -> filecacheDuration) (\s@MsSmoothGroupSettings' {} a -> s {filecacheDuration = a} :: MsSmoothGroupSettings)

-- | Length of mp4 fragments to generate (in seconds). Fragment length must
-- be compatible with GOP size and framerate.
msSmoothGroupSettings_fragmentLength :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Core.Natural)
msSmoothGroupSettings_fragmentLength = Lens.lens (\MsSmoothGroupSettings' {fragmentLength} -> fragmentLength) (\s@MsSmoothGroupSettings' {} a -> s {fragmentLength = a} :: MsSmoothGroupSettings)

-- | MS Smooth event ID to be sent to the IIS server. Should only be
-- specified if eventIdMode is set to useConfigured.
msSmoothGroupSettings_eventId :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Core.Text)
msSmoothGroupSettings_eventId = Lens.lens (\MsSmoothGroupSettings' {eventId} -> eventId) (\s@MsSmoothGroupSettings' {} a -> s {eventId = a} :: MsSmoothGroupSettings)

-- | If set to verifyAuthenticity, verify the https certificate chain to a
-- trusted Certificate Authority (CA). This will cause https outputs to
-- self-signed certificates to fail.
msSmoothGroupSettings_certificateMode :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe SmoothGroupCertificateMode)
msSmoothGroupSettings_certificateMode = Lens.lens (\MsSmoothGroupSettings' {certificateMode} -> certificateMode) (\s@MsSmoothGroupSettings' {} a -> s {certificateMode = a} :: MsSmoothGroupSettings)

-- | Number of retry attempts.
msSmoothGroupSettings_numRetries :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Core.Natural)
msSmoothGroupSettings_numRetries = Lens.lens (\MsSmoothGroupSettings' {numRetries} -> numRetries) (\s@MsSmoothGroupSettings' {} a -> s {numRetries = a} :: MsSmoothGroupSettings)

-- | The ID to include in each message in the sparse track. Ignored if
-- sparseTrackType is NONE.
msSmoothGroupSettings_acquisitionPointId :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Core.Text)
msSmoothGroupSettings_acquisitionPointId = Lens.lens (\MsSmoothGroupSettings' {acquisitionPointId} -> acquisitionPointId) (\s@MsSmoothGroupSettings' {} a -> s {acquisitionPointId = a} :: MsSmoothGroupSettings)

-- | If set to passthrough for an audio-only MS Smooth output, the fragment
-- absolute time will be set to the current timecode. This option does not
-- write timecodes to the audio elementary stream.
msSmoothGroupSettings_audioOnlyTimecodeControl :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe SmoothGroupAudioOnlyTimecodeControl)
msSmoothGroupSettings_audioOnlyTimecodeControl = Lens.lens (\MsSmoothGroupSettings' {audioOnlyTimecodeControl} -> audioOnlyTimecodeControl) (\s@MsSmoothGroupSettings' {} a -> s {audioOnlyTimecodeControl = a} :: MsSmoothGroupSettings)

-- | useInputSegmentation has been deprecated. The configured segment size is
-- always used.
msSmoothGroupSettings_segmentationMode :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe SmoothGroupSegmentationMode)
msSmoothGroupSettings_segmentationMode = Lens.lens (\MsSmoothGroupSettings' {segmentationMode} -> segmentationMode) (\s@MsSmoothGroupSettings' {} a -> s {segmentationMode = a} :: MsSmoothGroupSettings)

-- | Specifies whether or not to send an event ID to the IIS server. If no
-- event ID is sent and the same Live Event is used without changing the
-- publishing point, clients might see cached video from the previous run.
-- Options: - \"useConfigured\" - use the value provided in eventId -
-- \"useTimestamp\" - generate and send an event ID based on the current
-- timestamp - \"noEventId\" - do not send an event ID to the IIS server.
msSmoothGroupSettings_eventIdMode :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe SmoothGroupEventIdMode)
msSmoothGroupSettings_eventIdMode = Lens.lens (\MsSmoothGroupSettings' {eventIdMode} -> eventIdMode) (\s@MsSmoothGroupSettings' {} a -> s {eventIdMode = a} :: MsSmoothGroupSettings)

-- | Number of milliseconds to delay the output from the second pipeline.
msSmoothGroupSettings_sendDelayMs :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Core.Natural)
msSmoothGroupSettings_sendDelayMs = Lens.lens (\MsSmoothGroupSettings' {sendDelayMs} -> sendDelayMs) (\s@MsSmoothGroupSettings' {} a -> s {sendDelayMs = a} :: MsSmoothGroupSettings)

-- | Number of seconds to wait before retrying connection to the IIS server
-- if the connection is lost. Content will be cached during this time and
-- the cache will be be delivered to the IIS server once the connection is
-- re-established.
msSmoothGroupSettings_connectionRetryInterval :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Core.Natural)
msSmoothGroupSettings_connectionRetryInterval = Lens.lens (\MsSmoothGroupSettings' {connectionRetryInterval} -> connectionRetryInterval) (\s@MsSmoothGroupSettings' {} a -> s {connectionRetryInterval = a} :: MsSmoothGroupSettings)

-- | Identifies the type of data to place in the sparse track: - SCTE35:
-- Insert SCTE-35 messages from the source content. With each message,
-- insert an IDR frame to start a new segment. -
-- SCTE35_WITHOUT_SEGMENTATION: Insert SCTE-35 messages from the source
-- content. With each message, insert an IDR frame but don\'t start a new
-- segment. - NONE: Don\'t generate a sparse track for any outputs in this
-- output group.
msSmoothGroupSettings_sparseTrackType :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe SmoothGroupSparseTrackType)
msSmoothGroupSettings_sparseTrackType = Lens.lens (\MsSmoothGroupSettings' {sparseTrackType} -> sparseTrackType) (\s@MsSmoothGroupSettings' {} a -> s {sparseTrackType = a} :: MsSmoothGroupSettings)

-- | Parameter that control output group behavior on input loss.
msSmoothGroupSettings_inputLossAction :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe InputLossActionForMsSmoothOut)
msSmoothGroupSettings_inputLossAction = Lens.lens (\MsSmoothGroupSettings' {inputLossAction} -> inputLossAction) (\s@MsSmoothGroupSettings' {} a -> s {inputLossAction = a} :: MsSmoothGroupSettings)

-- | Timestamp offset for the event. Only used if timestampOffsetMode is set
-- to useConfiguredOffset.
msSmoothGroupSettings_timestampOffset :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Core.Text)
msSmoothGroupSettings_timestampOffset = Lens.lens (\MsSmoothGroupSettings' {timestampOffset} -> timestampOffset) (\s@MsSmoothGroupSettings' {} a -> s {timestampOffset = a} :: MsSmoothGroupSettings)

-- | When set to sendEos, send EOS signal to IIS server when stopping the
-- event
msSmoothGroupSettings_eventStopBehavior :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe SmoothGroupEventStopBehavior)
msSmoothGroupSettings_eventStopBehavior = Lens.lens (\MsSmoothGroupSettings' {eventStopBehavior} -> eventStopBehavior) (\s@MsSmoothGroupSettings' {} a -> s {eventStopBehavior = a} :: MsSmoothGroupSettings)

-- | Type of timestamp date offset to use. - useEventStartDate: Use the date
-- the event was started as the offset - useConfiguredOffset: Use an
-- explicitly configured date as the offset
msSmoothGroupSettings_timestampOffsetMode :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe SmoothGroupTimestampOffsetMode)
msSmoothGroupSettings_timestampOffsetMode = Lens.lens (\MsSmoothGroupSettings' {timestampOffsetMode} -> timestampOffsetMode) (\s@MsSmoothGroupSettings' {} a -> s {timestampOffsetMode = a} :: MsSmoothGroupSettings)

-- | Number of seconds before initiating a restart due to output failure, due
-- to exhausting the numRetries on one segment, or exceeding
-- filecacheDuration.
msSmoothGroupSettings_restartDelay :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Core.Natural)
msSmoothGroupSettings_restartDelay = Lens.lens (\MsSmoothGroupSettings' {restartDelay} -> restartDelay) (\s@MsSmoothGroupSettings' {} a -> s {restartDelay = a} :: MsSmoothGroupSettings)

-- | Smooth Streaming publish point on an IIS server. Elemental Live acts as
-- a \"Push\" encoder to IIS.
msSmoothGroupSettings_destination :: Lens.Lens' MsSmoothGroupSettings OutputLocationRef
msSmoothGroupSettings_destination = Lens.lens (\MsSmoothGroupSettings' {destination} -> destination) (\s@MsSmoothGroupSettings' {} a -> s {destination = a} :: MsSmoothGroupSettings)

instance Core.FromJSON MsSmoothGroupSettings where
  parseJSON =
    Core.withObject
      "MsSmoothGroupSettings"
      ( \x ->
          MsSmoothGroupSettings'
            Core.<$> (x Core..:? "streamManifestBehavior")
            Core.<*> (x Core..:? "filecacheDuration")
            Core.<*> (x Core..:? "fragmentLength")
            Core.<*> (x Core..:? "eventId")
            Core.<*> (x Core..:? "certificateMode")
            Core.<*> (x Core..:? "numRetries")
            Core.<*> (x Core..:? "acquisitionPointId")
            Core.<*> (x Core..:? "audioOnlyTimecodeControl")
            Core.<*> (x Core..:? "segmentationMode")
            Core.<*> (x Core..:? "eventIdMode")
            Core.<*> (x Core..:? "sendDelayMs")
            Core.<*> (x Core..:? "connectionRetryInterval")
            Core.<*> (x Core..:? "sparseTrackType")
            Core.<*> (x Core..:? "inputLossAction")
            Core.<*> (x Core..:? "timestampOffset")
            Core.<*> (x Core..:? "eventStopBehavior")
            Core.<*> (x Core..:? "timestampOffsetMode")
            Core.<*> (x Core..:? "restartDelay")
            Core.<*> (x Core..: "destination")
      )

instance Core.Hashable MsSmoothGroupSettings

instance Core.NFData MsSmoothGroupSettings

instance Core.ToJSON MsSmoothGroupSettings where
  toJSON MsSmoothGroupSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("streamManifestBehavior" Core..=)
              Core.<$> streamManifestBehavior,
            ("filecacheDuration" Core..=)
              Core.<$> filecacheDuration,
            ("fragmentLength" Core..=) Core.<$> fragmentLength,
            ("eventId" Core..=) Core.<$> eventId,
            ("certificateMode" Core..=) Core.<$> certificateMode,
            ("numRetries" Core..=) Core.<$> numRetries,
            ("acquisitionPointId" Core..=)
              Core.<$> acquisitionPointId,
            ("audioOnlyTimecodeControl" Core..=)
              Core.<$> audioOnlyTimecodeControl,
            ("segmentationMode" Core..=)
              Core.<$> segmentationMode,
            ("eventIdMode" Core..=) Core.<$> eventIdMode,
            ("sendDelayMs" Core..=) Core.<$> sendDelayMs,
            ("connectionRetryInterval" Core..=)
              Core.<$> connectionRetryInterval,
            ("sparseTrackType" Core..=) Core.<$> sparseTrackType,
            ("inputLossAction" Core..=) Core.<$> inputLossAction,
            ("timestampOffset" Core..=) Core.<$> timestampOffset,
            ("eventStopBehavior" Core..=)
              Core.<$> eventStopBehavior,
            ("timestampOffsetMode" Core..=)
              Core.<$> timestampOffsetMode,
            ("restartDelay" Core..=) Core.<$> restartDelay,
            Core.Just ("destination" Core..= destination)
          ]
      )
