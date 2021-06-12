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
-- Module      : Network.AWS.Connect.Types.InstanceStorageConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.InstanceStorageConfig where

import Network.AWS.Connect.Types.KinesisFirehoseConfig
import Network.AWS.Connect.Types.KinesisStreamConfig
import Network.AWS.Connect.Types.KinesisVideoStreamConfig
import Network.AWS.Connect.Types.S3Config
import Network.AWS.Connect.Types.StorageType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The storage configuration for the instance.
--
-- /See:/ 'newInstanceStorageConfig' smart constructor.
data InstanceStorageConfig = InstanceStorageConfig'
  { -- | The configuration of the Kinesis data stream.
    kinesisStreamConfig :: Core.Maybe KinesisStreamConfig,
    -- | The configuration of the Kinesis Firehose delivery stream.
    kinesisFirehoseConfig :: Core.Maybe KinesisFirehoseConfig,
    -- | The configuration of the Kinesis video stream.
    kinesisVideoStreamConfig :: Core.Maybe KinesisVideoStreamConfig,
    -- | The existing association identifier that uniquely identifies the
    -- resource type and storage config for the given instance ID.
    associationId :: Core.Maybe Core.Text,
    -- | The S3 bucket configuration.
    s3Config :: Core.Maybe S3Config,
    -- | A valid storage type.
    storageType :: StorageType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceStorageConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kinesisStreamConfig', 'instanceStorageConfig_kinesisStreamConfig' - The configuration of the Kinesis data stream.
--
-- 'kinesisFirehoseConfig', 'instanceStorageConfig_kinesisFirehoseConfig' - The configuration of the Kinesis Firehose delivery stream.
--
-- 'kinesisVideoStreamConfig', 'instanceStorageConfig_kinesisVideoStreamConfig' - The configuration of the Kinesis video stream.
--
-- 'associationId', 'instanceStorageConfig_associationId' - The existing association identifier that uniquely identifies the
-- resource type and storage config for the given instance ID.
--
-- 's3Config', 'instanceStorageConfig_s3Config' - The S3 bucket configuration.
--
-- 'storageType', 'instanceStorageConfig_storageType' - A valid storage type.
newInstanceStorageConfig ::
  -- | 'storageType'
  StorageType ->
  InstanceStorageConfig
newInstanceStorageConfig pStorageType_ =
  InstanceStorageConfig'
    { kinesisStreamConfig =
        Core.Nothing,
      kinesisFirehoseConfig = Core.Nothing,
      kinesisVideoStreamConfig = Core.Nothing,
      associationId = Core.Nothing,
      s3Config = Core.Nothing,
      storageType = pStorageType_
    }

-- | The configuration of the Kinesis data stream.
instanceStorageConfig_kinesisStreamConfig :: Lens.Lens' InstanceStorageConfig (Core.Maybe KinesisStreamConfig)
instanceStorageConfig_kinesisStreamConfig = Lens.lens (\InstanceStorageConfig' {kinesisStreamConfig} -> kinesisStreamConfig) (\s@InstanceStorageConfig' {} a -> s {kinesisStreamConfig = a} :: InstanceStorageConfig)

-- | The configuration of the Kinesis Firehose delivery stream.
instanceStorageConfig_kinesisFirehoseConfig :: Lens.Lens' InstanceStorageConfig (Core.Maybe KinesisFirehoseConfig)
instanceStorageConfig_kinesisFirehoseConfig = Lens.lens (\InstanceStorageConfig' {kinesisFirehoseConfig} -> kinesisFirehoseConfig) (\s@InstanceStorageConfig' {} a -> s {kinesisFirehoseConfig = a} :: InstanceStorageConfig)

-- | The configuration of the Kinesis video stream.
instanceStorageConfig_kinesisVideoStreamConfig :: Lens.Lens' InstanceStorageConfig (Core.Maybe KinesisVideoStreamConfig)
instanceStorageConfig_kinesisVideoStreamConfig = Lens.lens (\InstanceStorageConfig' {kinesisVideoStreamConfig} -> kinesisVideoStreamConfig) (\s@InstanceStorageConfig' {} a -> s {kinesisVideoStreamConfig = a} :: InstanceStorageConfig)

-- | The existing association identifier that uniquely identifies the
-- resource type and storage config for the given instance ID.
instanceStorageConfig_associationId :: Lens.Lens' InstanceStorageConfig (Core.Maybe Core.Text)
instanceStorageConfig_associationId = Lens.lens (\InstanceStorageConfig' {associationId} -> associationId) (\s@InstanceStorageConfig' {} a -> s {associationId = a} :: InstanceStorageConfig)

-- | The S3 bucket configuration.
instanceStorageConfig_s3Config :: Lens.Lens' InstanceStorageConfig (Core.Maybe S3Config)
instanceStorageConfig_s3Config = Lens.lens (\InstanceStorageConfig' {s3Config} -> s3Config) (\s@InstanceStorageConfig' {} a -> s {s3Config = a} :: InstanceStorageConfig)

-- | A valid storage type.
instanceStorageConfig_storageType :: Lens.Lens' InstanceStorageConfig StorageType
instanceStorageConfig_storageType = Lens.lens (\InstanceStorageConfig' {storageType} -> storageType) (\s@InstanceStorageConfig' {} a -> s {storageType = a} :: InstanceStorageConfig)

instance Core.FromJSON InstanceStorageConfig where
  parseJSON =
    Core.withObject
      "InstanceStorageConfig"
      ( \x ->
          InstanceStorageConfig'
            Core.<$> (x Core..:? "KinesisStreamConfig")
            Core.<*> (x Core..:? "KinesisFirehoseConfig")
            Core.<*> (x Core..:? "KinesisVideoStreamConfig")
            Core.<*> (x Core..:? "AssociationId")
            Core.<*> (x Core..:? "S3Config")
            Core.<*> (x Core..: "StorageType")
      )

instance Core.Hashable InstanceStorageConfig

instance Core.NFData InstanceStorageConfig

instance Core.ToJSON InstanceStorageConfig where
  toJSON InstanceStorageConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("KinesisStreamConfig" Core..=)
              Core.<$> kinesisStreamConfig,
            ("KinesisFirehoseConfig" Core..=)
              Core.<$> kinesisFirehoseConfig,
            ("KinesisVideoStreamConfig" Core..=)
              Core.<$> kinesisVideoStreamConfig,
            ("AssociationId" Core..=) Core.<$> associationId,
            ("S3Config" Core..=) Core.<$> s3Config,
            Core.Just ("StorageType" Core..= storageType)
          ]
      )
