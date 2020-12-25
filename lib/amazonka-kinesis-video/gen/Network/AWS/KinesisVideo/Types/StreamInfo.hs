{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types.StreamInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideo.Types.StreamInfo
  ( StreamInfo (..),

    -- * Smart constructor
    mkStreamInfo,

    -- * Lenses
    siCreationTime,
    siDataRetentionInHours,
    siDeviceName,
    siKmsKeyId,
    siMediaType,
    siStatus,
    siStreamARN,
    siStreamName,
    siVersion,
  )
where

import qualified Network.AWS.KinesisVideo.Types.DeviceName as Types
import qualified Network.AWS.KinesisVideo.Types.KmsKeyId as Types
import qualified Network.AWS.KinesisVideo.Types.MediaType as Types
import qualified Network.AWS.KinesisVideo.Types.StreamARN as Types
import qualified Network.AWS.KinesisVideo.Types.StreamName as Types
import qualified Network.AWS.KinesisVideo.Types.StreamStatus as Types
import qualified Network.AWS.KinesisVideo.Types.Version as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object describing a Kinesis video stream.
--
-- /See:/ 'mkStreamInfo' smart constructor.
data StreamInfo = StreamInfo'
  { -- | A time stamp that indicates when the stream was created.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | How long the stream retains data, in hours.
    dataRetentionInHours :: Core.Maybe Core.Natural,
    -- | The name of the device that is associated with the stream.
    deviceName :: Core.Maybe Types.DeviceName,
    -- | The ID of the AWS Key Management Service (AWS KMS) key that Kinesis Video Streams uses to encrypt data on the stream.
    kmsKeyId :: Core.Maybe Types.KmsKeyId,
    -- | The @MediaType@ of the stream.
    mediaType :: Core.Maybe Types.MediaType,
    -- | The status of the stream.
    status :: Core.Maybe Types.StreamStatus,
    -- | The Amazon Resource Name (ARN) of the stream.
    streamARN :: Core.Maybe Types.StreamARN,
    -- | The name of the stream.
    streamName :: Core.Maybe Types.StreamName,
    -- | The version of the stream.
    version :: Core.Maybe Types.Version
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StreamInfo' value with any optional fields omitted.
mkStreamInfo ::
  StreamInfo
mkStreamInfo =
  StreamInfo'
    { creationTime = Core.Nothing,
      dataRetentionInHours = Core.Nothing,
      deviceName = Core.Nothing,
      kmsKeyId = Core.Nothing,
      mediaType = Core.Nothing,
      status = Core.Nothing,
      streamARN = Core.Nothing,
      streamName = Core.Nothing,
      version = Core.Nothing
    }

-- | A time stamp that indicates when the stream was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siCreationTime :: Lens.Lens' StreamInfo (Core.Maybe Core.NominalDiffTime)
siCreationTime = Lens.field @"creationTime"
{-# DEPRECATED siCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | How long the stream retains data, in hours.
--
-- /Note:/ Consider using 'dataRetentionInHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siDataRetentionInHours :: Lens.Lens' StreamInfo (Core.Maybe Core.Natural)
siDataRetentionInHours = Lens.field @"dataRetentionInHours"
{-# DEPRECATED siDataRetentionInHours "Use generic-lens or generic-optics with 'dataRetentionInHours' instead." #-}

-- | The name of the device that is associated with the stream.
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siDeviceName :: Lens.Lens' StreamInfo (Core.Maybe Types.DeviceName)
siDeviceName = Lens.field @"deviceName"
{-# DEPRECATED siDeviceName "Use generic-lens or generic-optics with 'deviceName' instead." #-}

-- | The ID of the AWS Key Management Service (AWS KMS) key that Kinesis Video Streams uses to encrypt data on the stream.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siKmsKeyId :: Lens.Lens' StreamInfo (Core.Maybe Types.KmsKeyId)
siKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED siKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The @MediaType@ of the stream.
--
-- /Note:/ Consider using 'mediaType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siMediaType :: Lens.Lens' StreamInfo (Core.Maybe Types.MediaType)
siMediaType = Lens.field @"mediaType"
{-# DEPRECATED siMediaType "Use generic-lens or generic-optics with 'mediaType' instead." #-}

-- | The status of the stream.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siStatus :: Lens.Lens' StreamInfo (Core.Maybe Types.StreamStatus)
siStatus = Lens.field @"status"
{-# DEPRECATED siStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The Amazon Resource Name (ARN) of the stream.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siStreamARN :: Lens.Lens' StreamInfo (Core.Maybe Types.StreamARN)
siStreamARN = Lens.field @"streamARN"
{-# DEPRECATED siStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | The name of the stream.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siStreamName :: Lens.Lens' StreamInfo (Core.Maybe Types.StreamName)
siStreamName = Lens.field @"streamName"
{-# DEPRECATED siStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

-- | The version of the stream.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siVersion :: Lens.Lens' StreamInfo (Core.Maybe Types.Version)
siVersion = Lens.field @"version"
{-# DEPRECATED siVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON StreamInfo where
  parseJSON =
    Core.withObject "StreamInfo" Core.$
      \x ->
        StreamInfo'
          Core.<$> (x Core..:? "CreationTime")
          Core.<*> (x Core..:? "DataRetentionInHours")
          Core.<*> (x Core..:? "DeviceName")
          Core.<*> (x Core..:? "KmsKeyId")
          Core.<*> (x Core..:? "MediaType")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "StreamARN")
          Core.<*> (x Core..:? "StreamName")
          Core.<*> (x Core..:? "Version")
