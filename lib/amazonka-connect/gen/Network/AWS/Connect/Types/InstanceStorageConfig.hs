{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.InstanceStorageConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.InstanceStorageConfig
  ( InstanceStorageConfig (..),

    -- * Smart constructor
    mkInstanceStorageConfig,

    -- * Lenses
    iscAssociationId,
    iscKinesisStreamConfig,
    iscKinesisVideoStreamConfig,
    iscS3Config,
    iscKinesisFirehoseConfig,
    iscStorageType,
  )
where

import Network.AWS.Connect.Types.KinesisFirehoseConfig
import Network.AWS.Connect.Types.KinesisStreamConfig
import Network.AWS.Connect.Types.KinesisVideoStreamConfig
import Network.AWS.Connect.Types.S3Config
import Network.AWS.Connect.Types.StorageType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The storage configuration for the instance.
--
-- /See:/ 'mkInstanceStorageConfig' smart constructor.
data InstanceStorageConfig = InstanceStorageConfig'
  { -- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
    associationId :: Lude.Maybe Lude.Text,
    -- | The configuration of the Kinesis data stream.
    kinesisStreamConfig :: Lude.Maybe KinesisStreamConfig,
    -- | The configuration of the Kinesis video stream.
    kinesisVideoStreamConfig :: Lude.Maybe KinesisVideoStreamConfig,
    -- | The S3 configuration.
    s3Config :: Lude.Maybe S3Config,
    -- | The configuration of the Kinesis Firehose delivery stream.
    kinesisFirehoseConfig :: Lude.Maybe KinesisFirehoseConfig,
    -- | A valid storage type.
    storageType :: StorageType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceStorageConfig' with the minimum fields required to make a request.
--
-- * 'associationId' - The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
-- * 'kinesisStreamConfig' - The configuration of the Kinesis data stream.
-- * 'kinesisVideoStreamConfig' - The configuration of the Kinesis video stream.
-- * 's3Config' - The S3 configuration.
-- * 'kinesisFirehoseConfig' - The configuration of the Kinesis Firehose delivery stream.
-- * 'storageType' - A valid storage type.
mkInstanceStorageConfig ::
  -- | 'storageType'
  StorageType ->
  InstanceStorageConfig
mkInstanceStorageConfig pStorageType_ =
  InstanceStorageConfig'
    { associationId = Lude.Nothing,
      kinesisStreamConfig = Lude.Nothing,
      kinesisVideoStreamConfig = Lude.Nothing,
      s3Config = Lude.Nothing,
      kinesisFirehoseConfig = Lude.Nothing,
      storageType = pStorageType_
    }

-- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscAssociationId :: Lens.Lens' InstanceStorageConfig (Lude.Maybe Lude.Text)
iscAssociationId = Lens.lens (associationId :: InstanceStorageConfig -> Lude.Maybe Lude.Text) (\s a -> s {associationId = a} :: InstanceStorageConfig)
{-# DEPRECATED iscAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The configuration of the Kinesis data stream.
--
-- /Note:/ Consider using 'kinesisStreamConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscKinesisStreamConfig :: Lens.Lens' InstanceStorageConfig (Lude.Maybe KinesisStreamConfig)
iscKinesisStreamConfig = Lens.lens (kinesisStreamConfig :: InstanceStorageConfig -> Lude.Maybe KinesisStreamConfig) (\s a -> s {kinesisStreamConfig = a} :: InstanceStorageConfig)
{-# DEPRECATED iscKinesisStreamConfig "Use generic-lens or generic-optics with 'kinesisStreamConfig' instead." #-}

-- | The configuration of the Kinesis video stream.
--
-- /Note:/ Consider using 'kinesisVideoStreamConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscKinesisVideoStreamConfig :: Lens.Lens' InstanceStorageConfig (Lude.Maybe KinesisVideoStreamConfig)
iscKinesisVideoStreamConfig = Lens.lens (kinesisVideoStreamConfig :: InstanceStorageConfig -> Lude.Maybe KinesisVideoStreamConfig) (\s a -> s {kinesisVideoStreamConfig = a} :: InstanceStorageConfig)
{-# DEPRECATED iscKinesisVideoStreamConfig "Use generic-lens or generic-optics with 'kinesisVideoStreamConfig' instead." #-}

-- | The S3 configuration.
--
-- /Note:/ Consider using 's3Config' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscS3Config :: Lens.Lens' InstanceStorageConfig (Lude.Maybe S3Config)
iscS3Config = Lens.lens (s3Config :: InstanceStorageConfig -> Lude.Maybe S3Config) (\s a -> s {s3Config = a} :: InstanceStorageConfig)
{-# DEPRECATED iscS3Config "Use generic-lens or generic-optics with 's3Config' instead." #-}

-- | The configuration of the Kinesis Firehose delivery stream.
--
-- /Note:/ Consider using 'kinesisFirehoseConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscKinesisFirehoseConfig :: Lens.Lens' InstanceStorageConfig (Lude.Maybe KinesisFirehoseConfig)
iscKinesisFirehoseConfig = Lens.lens (kinesisFirehoseConfig :: InstanceStorageConfig -> Lude.Maybe KinesisFirehoseConfig) (\s a -> s {kinesisFirehoseConfig = a} :: InstanceStorageConfig)
{-# DEPRECATED iscKinesisFirehoseConfig "Use generic-lens or generic-optics with 'kinesisFirehoseConfig' instead." #-}

-- | A valid storage type.
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscStorageType :: Lens.Lens' InstanceStorageConfig StorageType
iscStorageType = Lens.lens (storageType :: InstanceStorageConfig -> StorageType) (\s a -> s {storageType = a} :: InstanceStorageConfig)
{-# DEPRECATED iscStorageType "Use generic-lens or generic-optics with 'storageType' instead." #-}

instance Lude.FromJSON InstanceStorageConfig where
  parseJSON =
    Lude.withObject
      "InstanceStorageConfig"
      ( \x ->
          InstanceStorageConfig'
            Lude.<$> (x Lude..:? "AssociationId")
            Lude.<*> (x Lude..:? "KinesisStreamConfig")
            Lude.<*> (x Lude..:? "KinesisVideoStreamConfig")
            Lude.<*> (x Lude..:? "S3Config")
            Lude.<*> (x Lude..:? "KinesisFirehoseConfig")
            Lude.<*> (x Lude..: "StorageType")
      )

instance Lude.ToJSON InstanceStorageConfig where
  toJSON InstanceStorageConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AssociationId" Lude..=) Lude.<$> associationId,
            ("KinesisStreamConfig" Lude..=) Lude.<$> kinesisStreamConfig,
            ("KinesisVideoStreamConfig" Lude..=)
              Lude.<$> kinesisVideoStreamConfig,
            ("S3Config" Lude..=) Lude.<$> s3Config,
            ("KinesisFirehoseConfig" Lude..=) Lude.<$> kinesisFirehoseConfig,
            Lude.Just ("StorageType" Lude..= storageType)
          ]
      )
