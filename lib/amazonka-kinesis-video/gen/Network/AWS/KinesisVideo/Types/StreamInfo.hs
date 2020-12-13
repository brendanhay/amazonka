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
    siStatus,
    siMediaType,
    siDataRetentionInHours,
    siStreamARN,
    siKMSKeyId,
    siDeviceName,
    siVersion,
    siStreamName,
  )
where

import Network.AWS.KinesisVideo.Types.StreamStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object describing a Kinesis video stream.
--
-- /See:/ 'mkStreamInfo' smart constructor.
data StreamInfo = StreamInfo'
  { -- | A time stamp that indicates when the stream was created.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | The status of the stream.
    status :: Lude.Maybe StreamStatus,
    -- | The @MediaType@ of the stream.
    mediaType :: Lude.Maybe Lude.Text,
    -- | How long the stream retains data, in hours.
    dataRetentionInHours :: Lude.Maybe Lude.Natural,
    -- | The Amazon Resource Name (ARN) of the stream.
    streamARN :: Lude.Maybe Lude.Text,
    -- | The ID of the AWS Key Management Service (AWS KMS) key that Kinesis Video Streams uses to encrypt data on the stream.
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | The name of the device that is associated with the stream.
    deviceName :: Lude.Maybe Lude.Text,
    -- | The version of the stream.
    version :: Lude.Maybe Lude.Text,
    -- | The name of the stream.
    streamName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StreamInfo' with the minimum fields required to make a request.
--
-- * 'creationTime' - A time stamp that indicates when the stream was created.
-- * 'status' - The status of the stream.
-- * 'mediaType' - The @MediaType@ of the stream.
-- * 'dataRetentionInHours' - How long the stream retains data, in hours.
-- * 'streamARN' - The Amazon Resource Name (ARN) of the stream.
-- * 'kmsKeyId' - The ID of the AWS Key Management Service (AWS KMS) key that Kinesis Video Streams uses to encrypt data on the stream.
-- * 'deviceName' - The name of the device that is associated with the stream.
-- * 'version' - The version of the stream.
-- * 'streamName' - The name of the stream.
mkStreamInfo ::
  StreamInfo
mkStreamInfo =
  StreamInfo'
    { creationTime = Lude.Nothing,
      status = Lude.Nothing,
      mediaType = Lude.Nothing,
      dataRetentionInHours = Lude.Nothing,
      streamARN = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      deviceName = Lude.Nothing,
      version = Lude.Nothing,
      streamName = Lude.Nothing
    }

-- | A time stamp that indicates when the stream was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siCreationTime :: Lens.Lens' StreamInfo (Lude.Maybe Lude.Timestamp)
siCreationTime = Lens.lens (creationTime :: StreamInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: StreamInfo)
{-# DEPRECATED siCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The status of the stream.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siStatus :: Lens.Lens' StreamInfo (Lude.Maybe StreamStatus)
siStatus = Lens.lens (status :: StreamInfo -> Lude.Maybe StreamStatus) (\s a -> s {status = a} :: StreamInfo)
{-# DEPRECATED siStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The @MediaType@ of the stream.
--
-- /Note:/ Consider using 'mediaType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siMediaType :: Lens.Lens' StreamInfo (Lude.Maybe Lude.Text)
siMediaType = Lens.lens (mediaType :: StreamInfo -> Lude.Maybe Lude.Text) (\s a -> s {mediaType = a} :: StreamInfo)
{-# DEPRECATED siMediaType "Use generic-lens or generic-optics with 'mediaType' instead." #-}

-- | How long the stream retains data, in hours.
--
-- /Note:/ Consider using 'dataRetentionInHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siDataRetentionInHours :: Lens.Lens' StreamInfo (Lude.Maybe Lude.Natural)
siDataRetentionInHours = Lens.lens (dataRetentionInHours :: StreamInfo -> Lude.Maybe Lude.Natural) (\s a -> s {dataRetentionInHours = a} :: StreamInfo)
{-# DEPRECATED siDataRetentionInHours "Use generic-lens or generic-optics with 'dataRetentionInHours' instead." #-}

-- | The Amazon Resource Name (ARN) of the stream.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siStreamARN :: Lens.Lens' StreamInfo (Lude.Maybe Lude.Text)
siStreamARN = Lens.lens (streamARN :: StreamInfo -> Lude.Maybe Lude.Text) (\s a -> s {streamARN = a} :: StreamInfo)
{-# DEPRECATED siStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | The ID of the AWS Key Management Service (AWS KMS) key that Kinesis Video Streams uses to encrypt data on the stream.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siKMSKeyId :: Lens.Lens' StreamInfo (Lude.Maybe Lude.Text)
siKMSKeyId = Lens.lens (kmsKeyId :: StreamInfo -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: StreamInfo)
{-# DEPRECATED siKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The name of the device that is associated with the stream.
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siDeviceName :: Lens.Lens' StreamInfo (Lude.Maybe Lude.Text)
siDeviceName = Lens.lens (deviceName :: StreamInfo -> Lude.Maybe Lude.Text) (\s a -> s {deviceName = a} :: StreamInfo)
{-# DEPRECATED siDeviceName "Use generic-lens or generic-optics with 'deviceName' instead." #-}

-- | The version of the stream.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siVersion :: Lens.Lens' StreamInfo (Lude.Maybe Lude.Text)
siVersion = Lens.lens (version :: StreamInfo -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: StreamInfo)
{-# DEPRECATED siVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The name of the stream.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siStreamName :: Lens.Lens' StreamInfo (Lude.Maybe Lude.Text)
siStreamName = Lens.lens (streamName :: StreamInfo -> Lude.Maybe Lude.Text) (\s a -> s {streamName = a} :: StreamInfo)
{-# DEPRECATED siStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Lude.FromJSON StreamInfo where
  parseJSON =
    Lude.withObject
      "StreamInfo"
      ( \x ->
          StreamInfo'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "MediaType")
            Lude.<*> (x Lude..:? "DataRetentionInHours")
            Lude.<*> (x Lude..:? "StreamARN")
            Lude.<*> (x Lude..:? "KmsKeyId")
            Lude.<*> (x Lude..:? "DeviceName")
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..:? "StreamName")
      )
