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
import qualified Network.AWS.Prelude as Prelude

-- | An object describing a Kinesis video stream.
--
-- /See:/ 'newStreamInfo' smart constructor.
data StreamInfo = StreamInfo'
  { -- | The status of the stream.
    status :: Prelude.Maybe StreamStatus,
    -- | A time stamp that indicates when the stream was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | How long the stream retains data, in hours.
    dataRetentionInHours :: Prelude.Maybe Prelude.Natural,
    -- | The version of the stream.
    version :: Prelude.Maybe Prelude.Text,
    -- | The ID of the AWS Key Management Service (AWS KMS) key that Kinesis
    -- Video Streams uses to encrypt data on the stream.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The name of the device that is associated with the stream.
    deviceName :: Prelude.Maybe Prelude.Text,
    -- | The @MediaType@ of the stream.
    mediaType :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the stream.
    streamARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the stream.
    streamName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { status = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      dataRetentionInHours = Prelude.Nothing,
      version = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      deviceName = Prelude.Nothing,
      mediaType = Prelude.Nothing,
      streamARN = Prelude.Nothing,
      streamName = Prelude.Nothing
    }

-- | The status of the stream.
streamInfo_status :: Lens.Lens' StreamInfo (Prelude.Maybe StreamStatus)
streamInfo_status = Lens.lens (\StreamInfo' {status} -> status) (\s@StreamInfo' {} a -> s {status = a} :: StreamInfo)

-- | A time stamp that indicates when the stream was created.
streamInfo_creationTime :: Lens.Lens' StreamInfo (Prelude.Maybe Prelude.UTCTime)
streamInfo_creationTime = Lens.lens (\StreamInfo' {creationTime} -> creationTime) (\s@StreamInfo' {} a -> s {creationTime = a} :: StreamInfo) Prelude.. Lens.mapping Core._Time

-- | How long the stream retains data, in hours.
streamInfo_dataRetentionInHours :: Lens.Lens' StreamInfo (Prelude.Maybe Prelude.Natural)
streamInfo_dataRetentionInHours = Lens.lens (\StreamInfo' {dataRetentionInHours} -> dataRetentionInHours) (\s@StreamInfo' {} a -> s {dataRetentionInHours = a} :: StreamInfo)

-- | The version of the stream.
streamInfo_version :: Lens.Lens' StreamInfo (Prelude.Maybe Prelude.Text)
streamInfo_version = Lens.lens (\StreamInfo' {version} -> version) (\s@StreamInfo' {} a -> s {version = a} :: StreamInfo)

-- | The ID of the AWS Key Management Service (AWS KMS) key that Kinesis
-- Video Streams uses to encrypt data on the stream.
streamInfo_kmsKeyId :: Lens.Lens' StreamInfo (Prelude.Maybe Prelude.Text)
streamInfo_kmsKeyId = Lens.lens (\StreamInfo' {kmsKeyId} -> kmsKeyId) (\s@StreamInfo' {} a -> s {kmsKeyId = a} :: StreamInfo)

-- | The name of the device that is associated with the stream.
streamInfo_deviceName :: Lens.Lens' StreamInfo (Prelude.Maybe Prelude.Text)
streamInfo_deviceName = Lens.lens (\StreamInfo' {deviceName} -> deviceName) (\s@StreamInfo' {} a -> s {deviceName = a} :: StreamInfo)

-- | The @MediaType@ of the stream.
streamInfo_mediaType :: Lens.Lens' StreamInfo (Prelude.Maybe Prelude.Text)
streamInfo_mediaType = Lens.lens (\StreamInfo' {mediaType} -> mediaType) (\s@StreamInfo' {} a -> s {mediaType = a} :: StreamInfo)

-- | The Amazon Resource Name (ARN) of the stream.
streamInfo_streamARN :: Lens.Lens' StreamInfo (Prelude.Maybe Prelude.Text)
streamInfo_streamARN = Lens.lens (\StreamInfo' {streamARN} -> streamARN) (\s@StreamInfo' {} a -> s {streamARN = a} :: StreamInfo)

-- | The name of the stream.
streamInfo_streamName :: Lens.Lens' StreamInfo (Prelude.Maybe Prelude.Text)
streamInfo_streamName = Lens.lens (\StreamInfo' {streamName} -> streamName) (\s@StreamInfo' {} a -> s {streamName = a} :: StreamInfo)

instance Core.FromJSON StreamInfo where
  parseJSON =
    Core.withObject
      "StreamInfo"
      ( \x ->
          StreamInfo'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "DataRetentionInHours")
            Prelude.<*> (x Core..:? "Version")
            Prelude.<*> (x Core..:? "KmsKeyId")
            Prelude.<*> (x Core..:? "DeviceName")
            Prelude.<*> (x Core..:? "MediaType")
            Prelude.<*> (x Core..:? "StreamARN")
            Prelude.<*> (x Core..:? "StreamName")
      )

instance Prelude.Hashable StreamInfo

instance Prelude.NFData StreamInfo
