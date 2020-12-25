{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    msgsDestination,
    msgsAcquisitionPointId,
    msgsAudioOnlyTimecodeControl,
    msgsCertificateMode,
    msgsConnectionRetryInterval,
    msgsEventId,
    msgsEventIdMode,
    msgsEventStopBehavior,
    msgsFilecacheDuration,
    msgsFragmentLength,
    msgsInputLossAction,
    msgsNumRetries,
    msgsRestartDelay,
    msgsSegmentationMode,
    msgsSendDelayMs,
    msgsSparseTrackType,
    msgsStreamManifestBehavior,
    msgsTimestampOffset,
    msgsTimestampOffsetMode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.InputLossActionForMsSmoothOut as Types
import qualified Network.AWS.MediaLive.Types.OutputLocationRef as Types
import qualified Network.AWS.MediaLive.Types.SmoothGroupAudioOnlyTimecodeControl as Types
import qualified Network.AWS.MediaLive.Types.SmoothGroupCertificateMode as Types
import qualified Network.AWS.MediaLive.Types.SmoothGroupEventIdMode as Types
import qualified Network.AWS.MediaLive.Types.SmoothGroupEventStopBehavior as Types
import qualified Network.AWS.MediaLive.Types.SmoothGroupSegmentationMode as Types
import qualified Network.AWS.MediaLive.Types.SmoothGroupSparseTrackType as Types
import qualified Network.AWS.MediaLive.Types.SmoothGroupStreamManifestBehavior as Types
import qualified Network.AWS.MediaLive.Types.SmoothGroupTimestampOffsetMode as Types
import qualified Network.AWS.Prelude as Core

-- | Ms Smooth Group Settings
--
-- /See:/ 'mkMsSmoothGroupSettings' smart constructor.
data MsSmoothGroupSettings = MsSmoothGroupSettings'
  { -- | Smooth Streaming publish point on an IIS server. Elemental Live acts as a "Push" encoder to IIS.
    destination :: Types.OutputLocationRef,
    -- | The ID to include in each message in the sparse track. Ignored if sparseTrackType is NONE.
    acquisitionPointId :: Core.Maybe Core.Text,
    -- | If set to passthrough for an audio-only MS Smooth output, the fragment absolute time will be set to the current timecode. This option does not write timecodes to the audio elementary stream.
    audioOnlyTimecodeControl :: Core.Maybe Types.SmoothGroupAudioOnlyTimecodeControl,
    -- | If set to verifyAuthenticity, verify the https certificate chain to a trusted Certificate Authority (CA).  This will cause https outputs to self-signed certificates to fail.
    certificateMode :: Core.Maybe Types.SmoothGroupCertificateMode,
    -- | Number of seconds to wait before retrying connection to the IIS server if the connection is lost. Content will be cached during this time and the cache will be be delivered to the IIS server once the connection is re-established.
    connectionRetryInterval :: Core.Maybe Core.Natural,
    -- | MS Smooth event ID to be sent to the IIS server.
    --
    --
    -- Should only be specified if eventIdMode is set to useConfigured.
    eventId :: Core.Maybe Core.Text,
    -- | Specifies whether or not to send an event ID to the IIS server. If no event ID is sent and the same Live Event is used without changing the publishing point, clients might see cached video from the previous run.
    --
    --
    -- Options:
    -- - "useConfigured" - use the value provided in eventId
    -- - "useTimestamp" - generate and send an event ID based on the current timestamp
    -- - "noEventId" - do not send an event ID to the IIS server.
    eventIdMode :: Core.Maybe Types.SmoothGroupEventIdMode,
    -- | When set to sendEos, send EOS signal to IIS server when stopping the event
    eventStopBehavior :: Core.Maybe Types.SmoothGroupEventStopBehavior,
    -- | Size in seconds of file cache for streaming outputs.
    filecacheDuration :: Core.Maybe Core.Natural,
    -- | Length of mp4 fragments to generate (in seconds). Fragment length must be compatible with GOP size and framerate.
    fragmentLength :: Core.Maybe Core.Natural,
    -- | Parameter that control output group behavior on input loss.
    inputLossAction :: Core.Maybe Types.InputLossActionForMsSmoothOut,
    -- | Number of retry attempts.
    numRetries :: Core.Maybe Core.Natural,
    -- | Number of seconds before initiating a restart due to output failure, due to exhausting the numRetries on one segment, or exceeding filecacheDuration.
    restartDelay :: Core.Maybe Core.Natural,
    -- | useInputSegmentation has been deprecated. The configured segment size is always used.
    segmentationMode :: Core.Maybe Types.SmoothGroupSegmentationMode,
    -- | Number of milliseconds to delay the output from the second pipeline.
    sendDelayMs :: Core.Maybe Core.Natural,
    -- | Identifies the type of data to place in the sparse track:
    --
    -- - SCTE35: Insert SCTE-35 messages from the source content. With each message, insert an IDR frame to start a new segment.
    -- - SCTE35_WITHOUT_SEGMENTATION: Insert SCTE-35 messages from the source content. With each message, insert an IDR frame but don't start a new segment.
    -- - NONE: Don't generate a sparse track for any outputs in this output group.
    sparseTrackType :: Core.Maybe Types.SmoothGroupSparseTrackType,
    -- | When set to send, send stream manifest so publishing point doesn't start until all streams start.
    streamManifestBehavior :: Core.Maybe Types.SmoothGroupStreamManifestBehavior,
    -- | Timestamp offset for the event.  Only used if timestampOffsetMode is set to useConfiguredOffset.
    timestampOffset :: Core.Maybe Core.Text,
    -- | Type of timestamp date offset to use.
    --
    -- - useEventStartDate: Use the date the event was started as the offset
    -- - useConfiguredOffset: Use an explicitly configured date as the offset
    timestampOffsetMode :: Core.Maybe Types.SmoothGroupTimestampOffsetMode
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MsSmoothGroupSettings' value with any optional fields omitted.
mkMsSmoothGroupSettings ::
  -- | 'destination'
  Types.OutputLocationRef ->
  MsSmoothGroupSettings
mkMsSmoothGroupSettings destination =
  MsSmoothGroupSettings'
    { destination,
      acquisitionPointId = Core.Nothing,
      audioOnlyTimecodeControl = Core.Nothing,
      certificateMode = Core.Nothing,
      connectionRetryInterval = Core.Nothing,
      eventId = Core.Nothing,
      eventIdMode = Core.Nothing,
      eventStopBehavior = Core.Nothing,
      filecacheDuration = Core.Nothing,
      fragmentLength = Core.Nothing,
      inputLossAction = Core.Nothing,
      numRetries = Core.Nothing,
      restartDelay = Core.Nothing,
      segmentationMode = Core.Nothing,
      sendDelayMs = Core.Nothing,
      sparseTrackType = Core.Nothing,
      streamManifestBehavior = Core.Nothing,
      timestampOffset = Core.Nothing,
      timestampOffsetMode = Core.Nothing
    }

-- | Smooth Streaming publish point on an IIS server. Elemental Live acts as a "Push" encoder to IIS.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsDestination :: Lens.Lens' MsSmoothGroupSettings Types.OutputLocationRef
msgsDestination = Lens.field @"destination"
{-# DEPRECATED msgsDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | The ID to include in each message in the sparse track. Ignored if sparseTrackType is NONE.
--
-- /Note:/ Consider using 'acquisitionPointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsAcquisitionPointId :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Core.Text)
msgsAcquisitionPointId = Lens.field @"acquisitionPointId"
{-# DEPRECATED msgsAcquisitionPointId "Use generic-lens or generic-optics with 'acquisitionPointId' instead." #-}

-- | If set to passthrough for an audio-only MS Smooth output, the fragment absolute time will be set to the current timecode. This option does not write timecodes to the audio elementary stream.
--
-- /Note:/ Consider using 'audioOnlyTimecodeControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsAudioOnlyTimecodeControl :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Types.SmoothGroupAudioOnlyTimecodeControl)
msgsAudioOnlyTimecodeControl = Lens.field @"audioOnlyTimecodeControl"
{-# DEPRECATED msgsAudioOnlyTimecodeControl "Use generic-lens or generic-optics with 'audioOnlyTimecodeControl' instead." #-}

-- | If set to verifyAuthenticity, verify the https certificate chain to a trusted Certificate Authority (CA).  This will cause https outputs to self-signed certificates to fail.
--
-- /Note:/ Consider using 'certificateMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsCertificateMode :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Types.SmoothGroupCertificateMode)
msgsCertificateMode = Lens.field @"certificateMode"
{-# DEPRECATED msgsCertificateMode "Use generic-lens or generic-optics with 'certificateMode' instead." #-}

-- | Number of seconds to wait before retrying connection to the IIS server if the connection is lost. Content will be cached during this time and the cache will be be delivered to the IIS server once the connection is re-established.
--
-- /Note:/ Consider using 'connectionRetryInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsConnectionRetryInterval :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Core.Natural)
msgsConnectionRetryInterval = Lens.field @"connectionRetryInterval"
{-# DEPRECATED msgsConnectionRetryInterval "Use generic-lens or generic-optics with 'connectionRetryInterval' instead." #-}

-- | MS Smooth event ID to be sent to the IIS server.
--
--
-- Should only be specified if eventIdMode is set to useConfigured.
--
-- /Note:/ Consider using 'eventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsEventId :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Core.Text)
msgsEventId = Lens.field @"eventId"
{-# DEPRECATED msgsEventId "Use generic-lens or generic-optics with 'eventId' instead." #-}

-- | Specifies whether or not to send an event ID to the IIS server. If no event ID is sent and the same Live Event is used without changing the publishing point, clients might see cached video from the previous run.
--
--
-- Options:
-- - "useConfigured" - use the value provided in eventId
-- - "useTimestamp" - generate and send an event ID based on the current timestamp
-- - "noEventId" - do not send an event ID to the IIS server.
--
-- /Note:/ Consider using 'eventIdMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsEventIdMode :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Types.SmoothGroupEventIdMode)
msgsEventIdMode = Lens.field @"eventIdMode"
{-# DEPRECATED msgsEventIdMode "Use generic-lens or generic-optics with 'eventIdMode' instead." #-}

-- | When set to sendEos, send EOS signal to IIS server when stopping the event
--
-- /Note:/ Consider using 'eventStopBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsEventStopBehavior :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Types.SmoothGroupEventStopBehavior)
msgsEventStopBehavior = Lens.field @"eventStopBehavior"
{-# DEPRECATED msgsEventStopBehavior "Use generic-lens or generic-optics with 'eventStopBehavior' instead." #-}

-- | Size in seconds of file cache for streaming outputs.
--
-- /Note:/ Consider using 'filecacheDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsFilecacheDuration :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Core.Natural)
msgsFilecacheDuration = Lens.field @"filecacheDuration"
{-# DEPRECATED msgsFilecacheDuration "Use generic-lens or generic-optics with 'filecacheDuration' instead." #-}

-- | Length of mp4 fragments to generate (in seconds). Fragment length must be compatible with GOP size and framerate.
--
-- /Note:/ Consider using 'fragmentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsFragmentLength :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Core.Natural)
msgsFragmentLength = Lens.field @"fragmentLength"
{-# DEPRECATED msgsFragmentLength "Use generic-lens or generic-optics with 'fragmentLength' instead." #-}

-- | Parameter that control output group behavior on input loss.
--
-- /Note:/ Consider using 'inputLossAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsInputLossAction :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Types.InputLossActionForMsSmoothOut)
msgsInputLossAction = Lens.field @"inputLossAction"
{-# DEPRECATED msgsInputLossAction "Use generic-lens or generic-optics with 'inputLossAction' instead." #-}

-- | Number of retry attempts.
--
-- /Note:/ Consider using 'numRetries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsNumRetries :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Core.Natural)
msgsNumRetries = Lens.field @"numRetries"
{-# DEPRECATED msgsNumRetries "Use generic-lens or generic-optics with 'numRetries' instead." #-}

-- | Number of seconds before initiating a restart due to output failure, due to exhausting the numRetries on one segment, or exceeding filecacheDuration.
--
-- /Note:/ Consider using 'restartDelay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsRestartDelay :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Core.Natural)
msgsRestartDelay = Lens.field @"restartDelay"
{-# DEPRECATED msgsRestartDelay "Use generic-lens or generic-optics with 'restartDelay' instead." #-}

-- | useInputSegmentation has been deprecated. The configured segment size is always used.
--
-- /Note:/ Consider using 'segmentationMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsSegmentationMode :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Types.SmoothGroupSegmentationMode)
msgsSegmentationMode = Lens.field @"segmentationMode"
{-# DEPRECATED msgsSegmentationMode "Use generic-lens or generic-optics with 'segmentationMode' instead." #-}

-- | Number of milliseconds to delay the output from the second pipeline.
--
-- /Note:/ Consider using 'sendDelayMs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsSendDelayMs :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Core.Natural)
msgsSendDelayMs = Lens.field @"sendDelayMs"
{-# DEPRECATED msgsSendDelayMs "Use generic-lens or generic-optics with 'sendDelayMs' instead." #-}

-- | Identifies the type of data to place in the sparse track:
--
-- - SCTE35: Insert SCTE-35 messages from the source content. With each message, insert an IDR frame to start a new segment.
-- - SCTE35_WITHOUT_SEGMENTATION: Insert SCTE-35 messages from the source content. With each message, insert an IDR frame but don't start a new segment.
-- - NONE: Don't generate a sparse track for any outputs in this output group.
--
-- /Note:/ Consider using 'sparseTrackType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsSparseTrackType :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Types.SmoothGroupSparseTrackType)
msgsSparseTrackType = Lens.field @"sparseTrackType"
{-# DEPRECATED msgsSparseTrackType "Use generic-lens or generic-optics with 'sparseTrackType' instead." #-}

-- | When set to send, send stream manifest so publishing point doesn't start until all streams start.
--
-- /Note:/ Consider using 'streamManifestBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsStreamManifestBehavior :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Types.SmoothGroupStreamManifestBehavior)
msgsStreamManifestBehavior = Lens.field @"streamManifestBehavior"
{-# DEPRECATED msgsStreamManifestBehavior "Use generic-lens or generic-optics with 'streamManifestBehavior' instead." #-}

-- | Timestamp offset for the event.  Only used if timestampOffsetMode is set to useConfiguredOffset.
--
-- /Note:/ Consider using 'timestampOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsTimestampOffset :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Core.Text)
msgsTimestampOffset = Lens.field @"timestampOffset"
{-# DEPRECATED msgsTimestampOffset "Use generic-lens or generic-optics with 'timestampOffset' instead." #-}

-- | Type of timestamp date offset to use.
--
-- - useEventStartDate: Use the date the event was started as the offset
-- - useConfiguredOffset: Use an explicitly configured date as the offset
--
-- /Note:/ Consider using 'timestampOffsetMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsTimestampOffsetMode :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Types.SmoothGroupTimestampOffsetMode)
msgsTimestampOffsetMode = Lens.field @"timestampOffsetMode"
{-# DEPRECATED msgsTimestampOffsetMode "Use generic-lens or generic-optics with 'timestampOffsetMode' instead." #-}

instance Core.FromJSON MsSmoothGroupSettings where
  toJSON MsSmoothGroupSettings {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("destination" Core..= destination),
            ("acquisitionPointId" Core..=) Core.<$> acquisitionPointId,
            ("audioOnlyTimecodeControl" Core..=)
              Core.<$> audioOnlyTimecodeControl,
            ("certificateMode" Core..=) Core.<$> certificateMode,
            ("connectionRetryInterval" Core..=)
              Core.<$> connectionRetryInterval,
            ("eventId" Core..=) Core.<$> eventId,
            ("eventIdMode" Core..=) Core.<$> eventIdMode,
            ("eventStopBehavior" Core..=) Core.<$> eventStopBehavior,
            ("filecacheDuration" Core..=) Core.<$> filecacheDuration,
            ("fragmentLength" Core..=) Core.<$> fragmentLength,
            ("inputLossAction" Core..=) Core.<$> inputLossAction,
            ("numRetries" Core..=) Core.<$> numRetries,
            ("restartDelay" Core..=) Core.<$> restartDelay,
            ("segmentationMode" Core..=) Core.<$> segmentationMode,
            ("sendDelayMs" Core..=) Core.<$> sendDelayMs,
            ("sparseTrackType" Core..=) Core.<$> sparseTrackType,
            ("streamManifestBehavior" Core..=) Core.<$> streamManifestBehavior,
            ("timestampOffset" Core..=) Core.<$> timestampOffset,
            ("timestampOffsetMode" Core..=) Core.<$> timestampOffsetMode
          ]
      )

instance Core.FromJSON MsSmoothGroupSettings where
  parseJSON =
    Core.withObject "MsSmoothGroupSettings" Core.$
      \x ->
        MsSmoothGroupSettings'
          Core.<$> (x Core..: "destination")
          Core.<*> (x Core..:? "acquisitionPointId")
          Core.<*> (x Core..:? "audioOnlyTimecodeControl")
          Core.<*> (x Core..:? "certificateMode")
          Core.<*> (x Core..:? "connectionRetryInterval")
          Core.<*> (x Core..:? "eventId")
          Core.<*> (x Core..:? "eventIdMode")
          Core.<*> (x Core..:? "eventStopBehavior")
          Core.<*> (x Core..:? "filecacheDuration")
          Core.<*> (x Core..:? "fragmentLength")
          Core.<*> (x Core..:? "inputLossAction")
          Core.<*> (x Core..:? "numRetries")
          Core.<*> (x Core..:? "restartDelay")
          Core.<*> (x Core..:? "segmentationMode")
          Core.<*> (x Core..:? "sendDelayMs")
          Core.<*> (x Core..:? "sparseTrackType")
          Core.<*> (x Core..:? "streamManifestBehavior")
          Core.<*> (x Core..:? "timestampOffset")
          Core.<*> (x Core..:? "timestampOffsetMode")
