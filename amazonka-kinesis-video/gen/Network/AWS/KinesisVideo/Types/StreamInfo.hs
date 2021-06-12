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
-- Module      : Network.AWS.KinesisVideo.Types.StreamInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideo.Types.StreamInfo where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisVideo.Types.StreamStatus
import qualified Network.AWS.Lens as Lens

-- | An object describing a Kinesis video stream.
--
-- /See:/ 'newStreamInfo' smart constructor.
data StreamInfo = StreamInfo'
  { -- | The status of the stream.
    status :: Core.Maybe StreamStatus,
    -- | A time stamp that indicates when the stream was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | How long the stream retains data, in hours.
    dataRetentionInHours :: Core.Maybe Core.Natural,
    -- | The version of the stream.
    version :: Core.Maybe Core.Text,
    -- | The ID of the AWS Key Management Service (AWS KMS) key that Kinesis
    -- Video Streams uses to encrypt data on the stream.
    kmsKeyId :: Core.Maybe Core.Text,
    -- | The name of the device that is associated with the stream.
    deviceName :: Core.Maybe Core.Text,
    -- | The @MediaType@ of the stream.
    mediaType :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the stream.
    streamARN :: Core.Maybe Core.Text,
    -- | The name of the stream.
    streamName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StreamInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'streamInfo_status' - The status of the stream.
--
-- 'creationTime', 'streamInfo_creationTime' - A time stamp that indicates when the stream was created.
--
-- 'dataRetentionInHours', 'streamInfo_dataRetentionInHours' - How long the stream retains data, in hours.
--
-- 'version', 'streamInfo_version' - The version of the stream.
--
-- 'kmsKeyId', 'streamInfo_kmsKeyId' - The ID of the AWS Key Management Service (AWS KMS) key that Kinesis
-- Video Streams uses to encrypt data on the stream.
--
-- 'deviceName', 'streamInfo_deviceName' - The name of the device that is associated with the stream.
--
-- 'mediaType', 'streamInfo_mediaType' - The @MediaType@ of the stream.
--
-- 'streamARN', 'streamInfo_streamARN' - The Amazon Resource Name (ARN) of the stream.
--
-- 'streamName', 'streamInfo_streamName' - The name of the stream.
newStreamInfo ::
  StreamInfo
newStreamInfo =
  StreamInfo'
    { status = Core.Nothing,
      creationTime = Core.Nothing,
      dataRetentionInHours = Core.Nothing,
      version = Core.Nothing,
      kmsKeyId = Core.Nothing,
      deviceName = Core.Nothing,
      mediaType = Core.Nothing,
      streamARN = Core.Nothing,
      streamName = Core.Nothing
    }

-- | The status of the stream.
streamInfo_status :: Lens.Lens' StreamInfo (Core.Maybe StreamStatus)
streamInfo_status = Lens.lens (\StreamInfo' {status} -> status) (\s@StreamInfo' {} a -> s {status = a} :: StreamInfo)

-- | A time stamp that indicates when the stream was created.
streamInfo_creationTime :: Lens.Lens' StreamInfo (Core.Maybe Core.UTCTime)
streamInfo_creationTime = Lens.lens (\StreamInfo' {creationTime} -> creationTime) (\s@StreamInfo' {} a -> s {creationTime = a} :: StreamInfo) Core.. Lens.mapping Core._Time

-- | How long the stream retains data, in hours.
streamInfo_dataRetentionInHours :: Lens.Lens' StreamInfo (Core.Maybe Core.Natural)
streamInfo_dataRetentionInHours = Lens.lens (\StreamInfo' {dataRetentionInHours} -> dataRetentionInHours) (\s@StreamInfo' {} a -> s {dataRetentionInHours = a} :: StreamInfo)

-- | The version of the stream.
streamInfo_version :: Lens.Lens' StreamInfo (Core.Maybe Core.Text)
streamInfo_version = Lens.lens (\StreamInfo' {version} -> version) (\s@StreamInfo' {} a -> s {version = a} :: StreamInfo)

-- | The ID of the AWS Key Management Service (AWS KMS) key that Kinesis
-- Video Streams uses to encrypt data on the stream.
streamInfo_kmsKeyId :: Lens.Lens' StreamInfo (Core.Maybe Core.Text)
streamInfo_kmsKeyId = Lens.lens (\StreamInfo' {kmsKeyId} -> kmsKeyId) (\s@StreamInfo' {} a -> s {kmsKeyId = a} :: StreamInfo)

-- | The name of the device that is associated with the stream.
streamInfo_deviceName :: Lens.Lens' StreamInfo (Core.Maybe Core.Text)
streamInfo_deviceName = Lens.lens (\StreamInfo' {deviceName} -> deviceName) (\s@StreamInfo' {} a -> s {deviceName = a} :: StreamInfo)

-- | The @MediaType@ of the stream.
streamInfo_mediaType :: Lens.Lens' StreamInfo (Core.Maybe Core.Text)
streamInfo_mediaType = Lens.lens (\StreamInfo' {mediaType} -> mediaType) (\s@StreamInfo' {} a -> s {mediaType = a} :: StreamInfo)

-- | The Amazon Resource Name (ARN) of the stream.
streamInfo_streamARN :: Lens.Lens' StreamInfo (Core.Maybe Core.Text)
streamInfo_streamARN = Lens.lens (\StreamInfo' {streamARN} -> streamARN) (\s@StreamInfo' {} a -> s {streamARN = a} :: StreamInfo)

-- | The name of the stream.
streamInfo_streamName :: Lens.Lens' StreamInfo (Core.Maybe Core.Text)
streamInfo_streamName = Lens.lens (\StreamInfo' {streamName} -> streamName) (\s@StreamInfo' {} a -> s {streamName = a} :: StreamInfo)

instance Core.FromJSON StreamInfo where
  parseJSON =
    Core.withObject
      "StreamInfo"
      ( \x ->
          StreamInfo'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "DataRetentionInHours")
            Core.<*> (x Core..:? "Version")
            Core.<*> (x Core..:? "KmsKeyId")
            Core.<*> (x Core..:? "DeviceName")
            Core.<*> (x Core..:? "MediaType")
            Core.<*> (x Core..:? "StreamARN")
            Core.<*> (x Core..:? "StreamName")
      )

instance Core.Hashable StreamInfo

instance Core.NFData StreamInfo
