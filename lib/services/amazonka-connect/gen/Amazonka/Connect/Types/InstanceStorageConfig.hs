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
-- Module      : Amazonka.Connect.Types.InstanceStorageConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.InstanceStorageConfig where

import Amazonka.Connect.Types.KinesisFirehoseConfig
import Amazonka.Connect.Types.KinesisStreamConfig
import Amazonka.Connect.Types.KinesisVideoStreamConfig
import Amazonka.Connect.Types.S3Config
import Amazonka.Connect.Types.StorageType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The storage configuration for the instance.
--
-- /See:/ 'newInstanceStorageConfig' smart constructor.
data InstanceStorageConfig = InstanceStorageConfig'
  { -- | The configuration of the Kinesis video stream.
    kinesisVideoStreamConfig :: Prelude.Maybe KinesisVideoStreamConfig,
    -- | The configuration of the Kinesis Firehose delivery stream.
    kinesisFirehoseConfig :: Prelude.Maybe KinesisFirehoseConfig,
    -- | The S3 bucket configuration.
    s3Config :: Prelude.Maybe S3Config,
    -- | The existing association identifier that uniquely identifies the
    -- resource type and storage config for the given instance ID.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The configuration of the Kinesis data stream.
    kinesisStreamConfig :: Prelude.Maybe KinesisStreamConfig,
    -- | A valid storage type.
    storageType :: StorageType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceStorageConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kinesisVideoStreamConfig', 'instanceStorageConfig_kinesisVideoStreamConfig' - The configuration of the Kinesis video stream.
--
-- 'kinesisFirehoseConfig', 'instanceStorageConfig_kinesisFirehoseConfig' - The configuration of the Kinesis Firehose delivery stream.
--
-- 's3Config', 'instanceStorageConfig_s3Config' - The S3 bucket configuration.
--
-- 'associationId', 'instanceStorageConfig_associationId' - The existing association identifier that uniquely identifies the
-- resource type and storage config for the given instance ID.
--
-- 'kinesisStreamConfig', 'instanceStorageConfig_kinesisStreamConfig' - The configuration of the Kinesis data stream.
--
-- 'storageType', 'instanceStorageConfig_storageType' - A valid storage type.
newInstanceStorageConfig ::
  -- | 'storageType'
  StorageType ->
  InstanceStorageConfig
newInstanceStorageConfig pStorageType_ =
  InstanceStorageConfig'
    { kinesisVideoStreamConfig =
        Prelude.Nothing,
      kinesisFirehoseConfig = Prelude.Nothing,
      s3Config = Prelude.Nothing,
      associationId = Prelude.Nothing,
      kinesisStreamConfig = Prelude.Nothing,
      storageType = pStorageType_
    }

-- | The configuration of the Kinesis video stream.
instanceStorageConfig_kinesisVideoStreamConfig :: Lens.Lens' InstanceStorageConfig (Prelude.Maybe KinesisVideoStreamConfig)
instanceStorageConfig_kinesisVideoStreamConfig = Lens.lens (\InstanceStorageConfig' {kinesisVideoStreamConfig} -> kinesisVideoStreamConfig) (\s@InstanceStorageConfig' {} a -> s {kinesisVideoStreamConfig = a} :: InstanceStorageConfig)

-- | The configuration of the Kinesis Firehose delivery stream.
instanceStorageConfig_kinesisFirehoseConfig :: Lens.Lens' InstanceStorageConfig (Prelude.Maybe KinesisFirehoseConfig)
instanceStorageConfig_kinesisFirehoseConfig = Lens.lens (\InstanceStorageConfig' {kinesisFirehoseConfig} -> kinesisFirehoseConfig) (\s@InstanceStorageConfig' {} a -> s {kinesisFirehoseConfig = a} :: InstanceStorageConfig)

-- | The S3 bucket configuration.
instanceStorageConfig_s3Config :: Lens.Lens' InstanceStorageConfig (Prelude.Maybe S3Config)
instanceStorageConfig_s3Config = Lens.lens (\InstanceStorageConfig' {s3Config} -> s3Config) (\s@InstanceStorageConfig' {} a -> s {s3Config = a} :: InstanceStorageConfig)

-- | The existing association identifier that uniquely identifies the
-- resource type and storage config for the given instance ID.
instanceStorageConfig_associationId :: Lens.Lens' InstanceStorageConfig (Prelude.Maybe Prelude.Text)
instanceStorageConfig_associationId = Lens.lens (\InstanceStorageConfig' {associationId} -> associationId) (\s@InstanceStorageConfig' {} a -> s {associationId = a} :: InstanceStorageConfig)

-- | The configuration of the Kinesis data stream.
instanceStorageConfig_kinesisStreamConfig :: Lens.Lens' InstanceStorageConfig (Prelude.Maybe KinesisStreamConfig)
instanceStorageConfig_kinesisStreamConfig = Lens.lens (\InstanceStorageConfig' {kinesisStreamConfig} -> kinesisStreamConfig) (\s@InstanceStorageConfig' {} a -> s {kinesisStreamConfig = a} :: InstanceStorageConfig)

-- | A valid storage type.
instanceStorageConfig_storageType :: Lens.Lens' InstanceStorageConfig StorageType
instanceStorageConfig_storageType = Lens.lens (\InstanceStorageConfig' {storageType} -> storageType) (\s@InstanceStorageConfig' {} a -> s {storageType = a} :: InstanceStorageConfig)

instance Core.FromJSON InstanceStorageConfig where
  parseJSON =
    Core.withObject
      "InstanceStorageConfig"
      ( \x ->
          InstanceStorageConfig'
            Prelude.<$> (x Core..:? "KinesisVideoStreamConfig")
            Prelude.<*> (x Core..:? "KinesisFirehoseConfig")
            Prelude.<*> (x Core..:? "S3Config")
            Prelude.<*> (x Core..:? "AssociationId")
            Prelude.<*> (x Core..:? "KinesisStreamConfig")
            Prelude.<*> (x Core..: "StorageType")
      )

instance Prelude.Hashable InstanceStorageConfig where
  hashWithSalt _salt InstanceStorageConfig' {..} =
    _salt
      `Prelude.hashWithSalt` kinesisVideoStreamConfig
      `Prelude.hashWithSalt` kinesisFirehoseConfig
      `Prelude.hashWithSalt` s3Config
      `Prelude.hashWithSalt` associationId
      `Prelude.hashWithSalt` kinesisStreamConfig
      `Prelude.hashWithSalt` storageType

instance Prelude.NFData InstanceStorageConfig where
  rnf InstanceStorageConfig' {..} =
    Prelude.rnf kinesisVideoStreamConfig
      `Prelude.seq` Prelude.rnf kinesisFirehoseConfig
      `Prelude.seq` Prelude.rnf s3Config
      `Prelude.seq` Prelude.rnf associationId
      `Prelude.seq` Prelude.rnf kinesisStreamConfig
      `Prelude.seq` Prelude.rnf storageType

instance Core.ToJSON InstanceStorageConfig where
  toJSON InstanceStorageConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("KinesisVideoStreamConfig" Core..=)
              Prelude.<$> kinesisVideoStreamConfig,
            ("KinesisFirehoseConfig" Core..=)
              Prelude.<$> kinesisFirehoseConfig,
            ("S3Config" Core..=) Prelude.<$> s3Config,
            ("AssociationId" Core..=) Prelude.<$> associationId,
            ("KinesisStreamConfig" Core..=)
              Prelude.<$> kinesisStreamConfig,
            Prelude.Just ("StorageType" Core..= storageType)
          ]
      )
