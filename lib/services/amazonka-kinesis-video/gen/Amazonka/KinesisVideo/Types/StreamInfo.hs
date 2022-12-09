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
-- Module      : Amazonka.KinesisVideo.Types.StreamInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideo.Types.StreamInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideo.Types.StreamStatus
import qualified Amazonka.Prelude as Prelude

-- | An object describing a Kinesis video stream.
--
-- /See:/ 'newStreamInfo' smart constructor.
data StreamInfo = StreamInfo'
  { -- | A time stamp that indicates when the stream was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | How long the stream retains data, in hours.
    dataRetentionInHours :: Prelude.Maybe Prelude.Natural,
    -- | The name of the device that is associated with the stream.
    deviceName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Key Management Service (KMS) key that Kinesis Video
    -- Streams uses to encrypt data on the stream.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The @MediaType@ of the stream.
    mediaType :: Prelude.Maybe Prelude.Text,
    -- | The status of the stream.
    status :: Prelude.Maybe StreamStatus,
    -- | The Amazon Resource Name (ARN) of the stream.
    streamARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the stream.
    streamName :: Prelude.Maybe Prelude.Text,
    -- | The version of the stream.
    version :: Prelude.Maybe Prelude.Text
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
-- 'creationTime', 'streamInfo_creationTime' - A time stamp that indicates when the stream was created.
--
-- 'dataRetentionInHours', 'streamInfo_dataRetentionInHours' - How long the stream retains data, in hours.
--
-- 'deviceName', 'streamInfo_deviceName' - The name of the device that is associated with the stream.
--
-- 'kmsKeyId', 'streamInfo_kmsKeyId' - The ID of the Key Management Service (KMS) key that Kinesis Video
-- Streams uses to encrypt data on the stream.
--
-- 'mediaType', 'streamInfo_mediaType' - The @MediaType@ of the stream.
--
-- 'status', 'streamInfo_status' - The status of the stream.
--
-- 'streamARN', 'streamInfo_streamARN' - The Amazon Resource Name (ARN) of the stream.
--
-- 'streamName', 'streamInfo_streamName' - The name of the stream.
--
-- 'version', 'streamInfo_version' - The version of the stream.
newStreamInfo ::
  StreamInfo
newStreamInfo =
  StreamInfo'
    { creationTime = Prelude.Nothing,
      dataRetentionInHours = Prelude.Nothing,
      deviceName = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      mediaType = Prelude.Nothing,
      status = Prelude.Nothing,
      streamARN = Prelude.Nothing,
      streamName = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | A time stamp that indicates when the stream was created.
streamInfo_creationTime :: Lens.Lens' StreamInfo (Prelude.Maybe Prelude.UTCTime)
streamInfo_creationTime = Lens.lens (\StreamInfo' {creationTime} -> creationTime) (\s@StreamInfo' {} a -> s {creationTime = a} :: StreamInfo) Prelude.. Lens.mapping Data._Time

-- | How long the stream retains data, in hours.
streamInfo_dataRetentionInHours :: Lens.Lens' StreamInfo (Prelude.Maybe Prelude.Natural)
streamInfo_dataRetentionInHours = Lens.lens (\StreamInfo' {dataRetentionInHours} -> dataRetentionInHours) (\s@StreamInfo' {} a -> s {dataRetentionInHours = a} :: StreamInfo)

-- | The name of the device that is associated with the stream.
streamInfo_deviceName :: Lens.Lens' StreamInfo (Prelude.Maybe Prelude.Text)
streamInfo_deviceName = Lens.lens (\StreamInfo' {deviceName} -> deviceName) (\s@StreamInfo' {} a -> s {deviceName = a} :: StreamInfo)

-- | The ID of the Key Management Service (KMS) key that Kinesis Video
-- Streams uses to encrypt data on the stream.
streamInfo_kmsKeyId :: Lens.Lens' StreamInfo (Prelude.Maybe Prelude.Text)
streamInfo_kmsKeyId = Lens.lens (\StreamInfo' {kmsKeyId} -> kmsKeyId) (\s@StreamInfo' {} a -> s {kmsKeyId = a} :: StreamInfo)

-- | The @MediaType@ of the stream.
streamInfo_mediaType :: Lens.Lens' StreamInfo (Prelude.Maybe Prelude.Text)
streamInfo_mediaType = Lens.lens (\StreamInfo' {mediaType} -> mediaType) (\s@StreamInfo' {} a -> s {mediaType = a} :: StreamInfo)

-- | The status of the stream.
streamInfo_status :: Lens.Lens' StreamInfo (Prelude.Maybe StreamStatus)
streamInfo_status = Lens.lens (\StreamInfo' {status} -> status) (\s@StreamInfo' {} a -> s {status = a} :: StreamInfo)

-- | The Amazon Resource Name (ARN) of the stream.
streamInfo_streamARN :: Lens.Lens' StreamInfo (Prelude.Maybe Prelude.Text)
streamInfo_streamARN = Lens.lens (\StreamInfo' {streamARN} -> streamARN) (\s@StreamInfo' {} a -> s {streamARN = a} :: StreamInfo)

-- | The name of the stream.
streamInfo_streamName :: Lens.Lens' StreamInfo (Prelude.Maybe Prelude.Text)
streamInfo_streamName = Lens.lens (\StreamInfo' {streamName} -> streamName) (\s@StreamInfo' {} a -> s {streamName = a} :: StreamInfo)

-- | The version of the stream.
streamInfo_version :: Lens.Lens' StreamInfo (Prelude.Maybe Prelude.Text)
streamInfo_version = Lens.lens (\StreamInfo' {version} -> version) (\s@StreamInfo' {} a -> s {version = a} :: StreamInfo)

instance Data.FromJSON StreamInfo where
  parseJSON =
    Data.withObject
      "StreamInfo"
      ( \x ->
          StreamInfo'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "DataRetentionInHours")
            Prelude.<*> (x Data..:? "DeviceName")
            Prelude.<*> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..:? "MediaType")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StreamARN")
            Prelude.<*> (x Data..:? "StreamName")
            Prelude.<*> (x Data..:? "Version")
      )

instance Prelude.Hashable StreamInfo where
  hashWithSalt _salt StreamInfo' {..} =
    _salt `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` dataRetentionInHours
      `Prelude.hashWithSalt` deviceName
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` mediaType
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` streamARN
      `Prelude.hashWithSalt` streamName
      `Prelude.hashWithSalt` version

instance Prelude.NFData StreamInfo where
  rnf StreamInfo' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf dataRetentionInHours
      `Prelude.seq` Prelude.rnf deviceName
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf mediaType
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf streamARN
      `Prelude.seq` Prelude.rnf streamName
      `Prelude.seq` Prelude.rnf version
