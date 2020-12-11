-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MsSmoothGroupSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MsSmoothGroupSettings
  ( MsSmoothGroupSettings (..),

    -- * Smart constructor
    mkMsSmoothGroupSettings,

    -- * Lenses
    msgsFragmentLength,
    msgsStreamManifestBehavior,
    msgsSendDelayMs,
    msgsEventStopBehavior,
    msgsTimestampOffsetMode,
    msgsNumRetries,
    msgsAcquisitionPointId,
    msgsInputLossAction,
    msgsTimestampOffset,
    msgsCertificateMode,
    msgsSparseTrackType,
    msgsConnectionRetryInterval,
    msgsFilecacheDuration,
    msgsRestartDelay,
    msgsEventIdMode,
    msgsAudioOnlyTimecodeControl,
    msgsSegmentationMode,
    msgsEventId,
    msgsDestination,
  )
where

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
import qualified Network.AWS.Prelude as Lude

-- | Ms Smooth Group Settings
--
-- /See:/ 'mkMsSmoothGroupSettings' smart constructor.
data MsSmoothGroupSettings = MsSmoothGroupSettings'
  { fragmentLength ::
      Lude.Maybe Lude.Natural,
    streamManifestBehavior ::
      Lude.Maybe SmoothGroupStreamManifestBehavior,
    sendDelayMs :: Lude.Maybe Lude.Natural,
    eventStopBehavior ::
      Lude.Maybe SmoothGroupEventStopBehavior,
    timestampOffsetMode ::
      Lude.Maybe SmoothGroupTimestampOffsetMode,
    numRetries :: Lude.Maybe Lude.Natural,
    acquisitionPointId :: Lude.Maybe Lude.Text,
    inputLossAction ::
      Lude.Maybe InputLossActionForMsSmoothOut,
    timestampOffset :: Lude.Maybe Lude.Text,
    certificateMode ::
      Lude.Maybe SmoothGroupCertificateMode,
    sparseTrackType ::
      Lude.Maybe SmoothGroupSparseTrackType,
    connectionRetryInterval ::
      Lude.Maybe Lude.Natural,
    filecacheDuration :: Lude.Maybe Lude.Natural,
    restartDelay :: Lude.Maybe Lude.Natural,
    eventIdMode ::
      Lude.Maybe SmoothGroupEventIdMode,
    audioOnlyTimecodeControl ::
      Lude.Maybe SmoothGroupAudioOnlyTimecodeControl,
    segmentationMode ::
      Lude.Maybe SmoothGroupSegmentationMode,
    eventId :: Lude.Maybe Lude.Text,
    destination :: OutputLocationRef
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MsSmoothGroupSettings' with the minimum fields required to make a request.
--
-- * 'acquisitionPointId' - The ID to include in each message in the sparse track. Ignored if sparseTrackType is NONE.
-- * 'audioOnlyTimecodeControl' - If set to passthrough for an audio-only MS Smooth output, the fragment absolute time will be set to the current timecode. This option does not write timecodes to the audio elementary stream.
-- * 'certificateMode' - If set to verifyAuthenticity, verify the https certificate chain to a trusted Certificate Authority (CA).  This will cause https outputs to self-signed certificates to fail.
-- * 'connectionRetryInterval' - Number of seconds to wait before retrying connection to the IIS server if the connection is lost. Content will be cached during this time and the cache will be be delivered to the IIS server once the connection is re-established.
-- * 'destination' - Smooth Streaming publish point on an IIS server. Elemental Live acts as a "Push" encoder to IIS.
-- * 'eventId' - MS Smooth event ID to be sent to the IIS server.
--
--
-- Should only be specified if eventIdMode is set to useConfigured.
-- * 'eventIdMode' - Specifies whether or not to send an event ID to the IIS server. If no event ID is sent and the same Live Event is used without changing the publishing point, clients might see cached video from the previous run.
--
--
-- Options:
-- - "useConfigured" - use the value provided in eventId
-- - "useTimestamp" - generate and send an event ID based on the current timestamp
-- - "noEventId" - do not send an event ID to the IIS server.
-- * 'eventStopBehavior' - When set to sendEos, send EOS signal to IIS server when stopping the event
-- * 'filecacheDuration' - Size in seconds of file cache for streaming outputs.
-- * 'fragmentLength' - Length of mp4 fragments to generate (in seconds). Fragment length must be compatible with GOP size and framerate.
-- * 'inputLossAction' - Parameter that control output group behavior on input loss.
-- * 'numRetries' - Number of retry attempts.
-- * 'restartDelay' - Number of seconds before initiating a restart due to output failure, due to exhausting the numRetries on one segment, or exceeding filecacheDuration.
-- * 'segmentationMode' - useInputSegmentation has been deprecated. The configured segment size is always used.
-- * 'sendDelayMs' - Number of milliseconds to delay the output from the second pipeline.
-- * 'sparseTrackType' - Identifies the type of data to place in the sparse track:
--
-- - SCTE35: Insert SCTE-35 messages from the source content. With each message, insert an IDR frame to start a new segment.
-- - SCTE35_WITHOUT_SEGMENTATION: Insert SCTE-35 messages from the source content. With each message, insert an IDR frame but don't start a new segment.
-- - NONE: Don't generate a sparse track for any outputs in this output group.
-- * 'streamManifestBehavior' - When set to send, send stream manifest so publishing point doesn't start until all streams start.
-- * 'timestampOffset' - Timestamp offset for the event.  Only used if timestampOffsetMode is set to useConfiguredOffset.
-- * 'timestampOffsetMode' - Type of timestamp date offset to use.
--
-- - useEventStartDate: Use the date the event was started as the offset
-- - useConfiguredOffset: Use an explicitly configured date as the offset
mkMsSmoothGroupSettings ::
  -- | 'destination'
  OutputLocationRef ->
  MsSmoothGroupSettings
mkMsSmoothGroupSettings pDestination_ =
  MsSmoothGroupSettings'
    { fragmentLength = Lude.Nothing,
      streamManifestBehavior = Lude.Nothing,
      sendDelayMs = Lude.Nothing,
      eventStopBehavior = Lude.Nothing,
      timestampOffsetMode = Lude.Nothing,
      numRetries = Lude.Nothing,
      acquisitionPointId = Lude.Nothing,
      inputLossAction = Lude.Nothing,
      timestampOffset = Lude.Nothing,
      certificateMode = Lude.Nothing,
      sparseTrackType = Lude.Nothing,
      connectionRetryInterval = Lude.Nothing,
      filecacheDuration = Lude.Nothing,
      restartDelay = Lude.Nothing,
      eventIdMode = Lude.Nothing,
      audioOnlyTimecodeControl = Lude.Nothing,
      segmentationMode = Lude.Nothing,
      eventId = Lude.Nothing,
      destination = pDestination_
    }

-- | Length of mp4 fragments to generate (in seconds). Fragment length must be compatible with GOP size and framerate.
--
-- /Note:/ Consider using 'fragmentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsFragmentLength :: Lens.Lens' MsSmoothGroupSettings (Lude.Maybe Lude.Natural)
msgsFragmentLength = Lens.lens (fragmentLength :: MsSmoothGroupSettings -> Lude.Maybe Lude.Natural) (\s a -> s {fragmentLength = a} :: MsSmoothGroupSettings)
{-# DEPRECATED msgsFragmentLength "Use generic-lens or generic-optics with 'fragmentLength' instead." #-}

-- | When set to send, send stream manifest so publishing point doesn't start until all streams start.
--
-- /Note:/ Consider using 'streamManifestBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsStreamManifestBehavior :: Lens.Lens' MsSmoothGroupSettings (Lude.Maybe SmoothGroupStreamManifestBehavior)
msgsStreamManifestBehavior = Lens.lens (streamManifestBehavior :: MsSmoothGroupSettings -> Lude.Maybe SmoothGroupStreamManifestBehavior) (\s a -> s {streamManifestBehavior = a} :: MsSmoothGroupSettings)
{-# DEPRECATED msgsStreamManifestBehavior "Use generic-lens or generic-optics with 'streamManifestBehavior' instead." #-}

-- | Number of milliseconds to delay the output from the second pipeline.
--
-- /Note:/ Consider using 'sendDelayMs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsSendDelayMs :: Lens.Lens' MsSmoothGroupSettings (Lude.Maybe Lude.Natural)
msgsSendDelayMs = Lens.lens (sendDelayMs :: MsSmoothGroupSettings -> Lude.Maybe Lude.Natural) (\s a -> s {sendDelayMs = a} :: MsSmoothGroupSettings)
{-# DEPRECATED msgsSendDelayMs "Use generic-lens or generic-optics with 'sendDelayMs' instead." #-}

-- | When set to sendEos, send EOS signal to IIS server when stopping the event
--
-- /Note:/ Consider using 'eventStopBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsEventStopBehavior :: Lens.Lens' MsSmoothGroupSettings (Lude.Maybe SmoothGroupEventStopBehavior)
msgsEventStopBehavior = Lens.lens (eventStopBehavior :: MsSmoothGroupSettings -> Lude.Maybe SmoothGroupEventStopBehavior) (\s a -> s {eventStopBehavior = a} :: MsSmoothGroupSettings)
{-# DEPRECATED msgsEventStopBehavior "Use generic-lens or generic-optics with 'eventStopBehavior' instead." #-}

-- | Type of timestamp date offset to use.
--
-- - useEventStartDate: Use the date the event was started as the offset
-- - useConfiguredOffset: Use an explicitly configured date as the offset
--
-- /Note:/ Consider using 'timestampOffsetMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsTimestampOffsetMode :: Lens.Lens' MsSmoothGroupSettings (Lude.Maybe SmoothGroupTimestampOffsetMode)
msgsTimestampOffsetMode = Lens.lens (timestampOffsetMode :: MsSmoothGroupSettings -> Lude.Maybe SmoothGroupTimestampOffsetMode) (\s a -> s {timestampOffsetMode = a} :: MsSmoothGroupSettings)
{-# DEPRECATED msgsTimestampOffsetMode "Use generic-lens or generic-optics with 'timestampOffsetMode' instead." #-}

-- | Number of retry attempts.
--
-- /Note:/ Consider using 'numRetries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsNumRetries :: Lens.Lens' MsSmoothGroupSettings (Lude.Maybe Lude.Natural)
msgsNumRetries = Lens.lens (numRetries :: MsSmoothGroupSettings -> Lude.Maybe Lude.Natural) (\s a -> s {numRetries = a} :: MsSmoothGroupSettings)
{-# DEPRECATED msgsNumRetries "Use generic-lens or generic-optics with 'numRetries' instead." #-}

-- | The ID to include in each message in the sparse track. Ignored if sparseTrackType is NONE.
--
-- /Note:/ Consider using 'acquisitionPointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsAcquisitionPointId :: Lens.Lens' MsSmoothGroupSettings (Lude.Maybe Lude.Text)
msgsAcquisitionPointId = Lens.lens (acquisitionPointId :: MsSmoothGroupSettings -> Lude.Maybe Lude.Text) (\s a -> s {acquisitionPointId = a} :: MsSmoothGroupSettings)
{-# DEPRECATED msgsAcquisitionPointId "Use generic-lens or generic-optics with 'acquisitionPointId' instead." #-}

-- | Parameter that control output group behavior on input loss.
--
-- /Note:/ Consider using 'inputLossAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsInputLossAction :: Lens.Lens' MsSmoothGroupSettings (Lude.Maybe InputLossActionForMsSmoothOut)
msgsInputLossAction = Lens.lens (inputLossAction :: MsSmoothGroupSettings -> Lude.Maybe InputLossActionForMsSmoothOut) (\s a -> s {inputLossAction = a} :: MsSmoothGroupSettings)
{-# DEPRECATED msgsInputLossAction "Use generic-lens or generic-optics with 'inputLossAction' instead." #-}

-- | Timestamp offset for the event.  Only used if timestampOffsetMode is set to useConfiguredOffset.
--
-- /Note:/ Consider using 'timestampOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsTimestampOffset :: Lens.Lens' MsSmoothGroupSettings (Lude.Maybe Lude.Text)
msgsTimestampOffset = Lens.lens (timestampOffset :: MsSmoothGroupSettings -> Lude.Maybe Lude.Text) (\s a -> s {timestampOffset = a} :: MsSmoothGroupSettings)
{-# DEPRECATED msgsTimestampOffset "Use generic-lens or generic-optics with 'timestampOffset' instead." #-}

-- | If set to verifyAuthenticity, verify the https certificate chain to a trusted Certificate Authority (CA).  This will cause https outputs to self-signed certificates to fail.
--
-- /Note:/ Consider using 'certificateMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsCertificateMode :: Lens.Lens' MsSmoothGroupSettings (Lude.Maybe SmoothGroupCertificateMode)
msgsCertificateMode = Lens.lens (certificateMode :: MsSmoothGroupSettings -> Lude.Maybe SmoothGroupCertificateMode) (\s a -> s {certificateMode = a} :: MsSmoothGroupSettings)
{-# DEPRECATED msgsCertificateMode "Use generic-lens or generic-optics with 'certificateMode' instead." #-}

-- | Identifies the type of data to place in the sparse track:
--
-- - SCTE35: Insert SCTE-35 messages from the source content. With each message, insert an IDR frame to start a new segment.
-- - SCTE35_WITHOUT_SEGMENTATION: Insert SCTE-35 messages from the source content. With each message, insert an IDR frame but don't start a new segment.
-- - NONE: Don't generate a sparse track for any outputs in this output group.
--
-- /Note:/ Consider using 'sparseTrackType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsSparseTrackType :: Lens.Lens' MsSmoothGroupSettings (Lude.Maybe SmoothGroupSparseTrackType)
msgsSparseTrackType = Lens.lens (sparseTrackType :: MsSmoothGroupSettings -> Lude.Maybe SmoothGroupSparseTrackType) (\s a -> s {sparseTrackType = a} :: MsSmoothGroupSettings)
{-# DEPRECATED msgsSparseTrackType "Use generic-lens or generic-optics with 'sparseTrackType' instead." #-}

-- | Number of seconds to wait before retrying connection to the IIS server if the connection is lost. Content will be cached during this time and the cache will be be delivered to the IIS server once the connection is re-established.
--
-- /Note:/ Consider using 'connectionRetryInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsConnectionRetryInterval :: Lens.Lens' MsSmoothGroupSettings (Lude.Maybe Lude.Natural)
msgsConnectionRetryInterval = Lens.lens (connectionRetryInterval :: MsSmoothGroupSettings -> Lude.Maybe Lude.Natural) (\s a -> s {connectionRetryInterval = a} :: MsSmoothGroupSettings)
{-# DEPRECATED msgsConnectionRetryInterval "Use generic-lens or generic-optics with 'connectionRetryInterval' instead." #-}

-- | Size in seconds of file cache for streaming outputs.
--
-- /Note:/ Consider using 'filecacheDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsFilecacheDuration :: Lens.Lens' MsSmoothGroupSettings (Lude.Maybe Lude.Natural)
msgsFilecacheDuration = Lens.lens (filecacheDuration :: MsSmoothGroupSettings -> Lude.Maybe Lude.Natural) (\s a -> s {filecacheDuration = a} :: MsSmoothGroupSettings)
{-# DEPRECATED msgsFilecacheDuration "Use generic-lens or generic-optics with 'filecacheDuration' instead." #-}

-- | Number of seconds before initiating a restart due to output failure, due to exhausting the numRetries on one segment, or exceeding filecacheDuration.
--
-- /Note:/ Consider using 'restartDelay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsRestartDelay :: Lens.Lens' MsSmoothGroupSettings (Lude.Maybe Lude.Natural)
msgsRestartDelay = Lens.lens (restartDelay :: MsSmoothGroupSettings -> Lude.Maybe Lude.Natural) (\s a -> s {restartDelay = a} :: MsSmoothGroupSettings)
{-# DEPRECATED msgsRestartDelay "Use generic-lens or generic-optics with 'restartDelay' instead." #-}

-- | Specifies whether or not to send an event ID to the IIS server. If no event ID is sent and the same Live Event is used without changing the publishing point, clients might see cached video from the previous run.
--
--
-- Options:
-- - "useConfigured" - use the value provided in eventId
-- - "useTimestamp" - generate and send an event ID based on the current timestamp
-- - "noEventId" - do not send an event ID to the IIS server.
--
-- /Note:/ Consider using 'eventIdMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsEventIdMode :: Lens.Lens' MsSmoothGroupSettings (Lude.Maybe SmoothGroupEventIdMode)
msgsEventIdMode = Lens.lens (eventIdMode :: MsSmoothGroupSettings -> Lude.Maybe SmoothGroupEventIdMode) (\s a -> s {eventIdMode = a} :: MsSmoothGroupSettings)
{-# DEPRECATED msgsEventIdMode "Use generic-lens or generic-optics with 'eventIdMode' instead." #-}

-- | If set to passthrough for an audio-only MS Smooth output, the fragment absolute time will be set to the current timecode. This option does not write timecodes to the audio elementary stream.
--
-- /Note:/ Consider using 'audioOnlyTimecodeControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsAudioOnlyTimecodeControl :: Lens.Lens' MsSmoothGroupSettings (Lude.Maybe SmoothGroupAudioOnlyTimecodeControl)
msgsAudioOnlyTimecodeControl = Lens.lens (audioOnlyTimecodeControl :: MsSmoothGroupSettings -> Lude.Maybe SmoothGroupAudioOnlyTimecodeControl) (\s a -> s {audioOnlyTimecodeControl = a} :: MsSmoothGroupSettings)
{-# DEPRECATED msgsAudioOnlyTimecodeControl "Use generic-lens or generic-optics with 'audioOnlyTimecodeControl' instead." #-}

-- | useInputSegmentation has been deprecated. The configured segment size is always used.
--
-- /Note:/ Consider using 'segmentationMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsSegmentationMode :: Lens.Lens' MsSmoothGroupSettings (Lude.Maybe SmoothGroupSegmentationMode)
msgsSegmentationMode = Lens.lens (segmentationMode :: MsSmoothGroupSettings -> Lude.Maybe SmoothGroupSegmentationMode) (\s a -> s {segmentationMode = a} :: MsSmoothGroupSettings)
{-# DEPRECATED msgsSegmentationMode "Use generic-lens or generic-optics with 'segmentationMode' instead." #-}

-- | MS Smooth event ID to be sent to the IIS server.
--
--
-- Should only be specified if eventIdMode is set to useConfigured.
--
-- /Note:/ Consider using 'eventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsEventId :: Lens.Lens' MsSmoothGroupSettings (Lude.Maybe Lude.Text)
msgsEventId = Lens.lens (eventId :: MsSmoothGroupSettings -> Lude.Maybe Lude.Text) (\s a -> s {eventId = a} :: MsSmoothGroupSettings)
{-# DEPRECATED msgsEventId "Use generic-lens or generic-optics with 'eventId' instead." #-}

-- | Smooth Streaming publish point on an IIS server. Elemental Live acts as a "Push" encoder to IIS.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsDestination :: Lens.Lens' MsSmoothGroupSettings OutputLocationRef
msgsDestination = Lens.lens (destination :: MsSmoothGroupSettings -> OutputLocationRef) (\s a -> s {destination = a} :: MsSmoothGroupSettings)
{-# DEPRECATED msgsDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

instance Lude.FromJSON MsSmoothGroupSettings where
  parseJSON =
    Lude.withObject
      "MsSmoothGroupSettings"
      ( \x ->
          MsSmoothGroupSettings'
            Lude.<$> (x Lude..:? "fragmentLength")
            Lude.<*> (x Lude..:? "streamManifestBehavior")
            Lude.<*> (x Lude..:? "sendDelayMs")
            Lude.<*> (x Lude..:? "eventStopBehavior")
            Lude.<*> (x Lude..:? "timestampOffsetMode")
            Lude.<*> (x Lude..:? "numRetries")
            Lude.<*> (x Lude..:? "acquisitionPointId")
            Lude.<*> (x Lude..:? "inputLossAction")
            Lude.<*> (x Lude..:? "timestampOffset")
            Lude.<*> (x Lude..:? "certificateMode")
            Lude.<*> (x Lude..:? "sparseTrackType")
            Lude.<*> (x Lude..:? "connectionRetryInterval")
            Lude.<*> (x Lude..:? "filecacheDuration")
            Lude.<*> (x Lude..:? "restartDelay")
            Lude.<*> (x Lude..:? "eventIdMode")
            Lude.<*> (x Lude..:? "audioOnlyTimecodeControl")
            Lude.<*> (x Lude..:? "segmentationMode")
            Lude.<*> (x Lude..:? "eventId")
            Lude.<*> (x Lude..: "destination")
      )

instance Lude.ToJSON MsSmoothGroupSettings where
  toJSON MsSmoothGroupSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("fragmentLength" Lude..=) Lude.<$> fragmentLength,
            ("streamManifestBehavior" Lude..=) Lude.<$> streamManifestBehavior,
            ("sendDelayMs" Lude..=) Lude.<$> sendDelayMs,
            ("eventStopBehavior" Lude..=) Lude.<$> eventStopBehavior,
            ("timestampOffsetMode" Lude..=) Lude.<$> timestampOffsetMode,
            ("numRetries" Lude..=) Lude.<$> numRetries,
            ("acquisitionPointId" Lude..=) Lude.<$> acquisitionPointId,
            ("inputLossAction" Lude..=) Lude.<$> inputLossAction,
            ("timestampOffset" Lude..=) Lude.<$> timestampOffset,
            ("certificateMode" Lude..=) Lude.<$> certificateMode,
            ("sparseTrackType" Lude..=) Lude.<$> sparseTrackType,
            ("connectionRetryInterval" Lude..=)
              Lude.<$> connectionRetryInterval,
            ("filecacheDuration" Lude..=) Lude.<$> filecacheDuration,
            ("restartDelay" Lude..=) Lude.<$> restartDelay,
            ("eventIdMode" Lude..=) Lude.<$> eventIdMode,
            ("audioOnlyTimecodeControl" Lude..=)
              Lude.<$> audioOnlyTimecodeControl,
            ("segmentationMode" Lude..=) Lude.<$> segmentationMode,
            ("eventId" Lude..=) Lude.<$> eventId,
            Lude.Just ("destination" Lude..= destination)
          ]
      )
