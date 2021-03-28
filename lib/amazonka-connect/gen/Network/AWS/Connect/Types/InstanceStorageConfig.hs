{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.InstanceStorageConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.InstanceStorageConfig
  ( InstanceStorageConfig (..)
  -- * Smart constructor
  , mkInstanceStorageConfig
  -- * Lenses
  , iscStorageType
  , iscAssociationId
  , iscKinesisFirehoseConfig
  , iscKinesisStreamConfig
  , iscKinesisVideoStreamConfig
  , iscS3Config
  ) where

import qualified Network.AWS.Connect.Types.AssociationId as Types
import qualified Network.AWS.Connect.Types.KinesisFirehoseConfig as Types
import qualified Network.AWS.Connect.Types.KinesisStreamConfig as Types
import qualified Network.AWS.Connect.Types.KinesisVideoStreamConfig as Types
import qualified Network.AWS.Connect.Types.S3Config as Types
import qualified Network.AWS.Connect.Types.StorageType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The storage configuration for the instance.
--
-- /See:/ 'mkInstanceStorageConfig' smart constructor.
data InstanceStorageConfig = InstanceStorageConfig'
  { storageType :: Types.StorageType
    -- ^ A valid storage type.
  , associationId :: Core.Maybe Types.AssociationId
    -- ^ The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
  , kinesisFirehoseConfig :: Core.Maybe Types.KinesisFirehoseConfig
    -- ^ The configuration of the Kinesis Firehose delivery stream.
  , kinesisStreamConfig :: Core.Maybe Types.KinesisStreamConfig
    -- ^ The configuration of the Kinesis data stream.
  , kinesisVideoStreamConfig :: Core.Maybe Types.KinesisVideoStreamConfig
    -- ^ The configuration of the Kinesis video stream.
  , s3Config :: Core.Maybe Types.S3Config
    -- ^ The S3 configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceStorageConfig' value with any optional fields omitted.
mkInstanceStorageConfig
    :: Types.StorageType -- ^ 'storageType'
    -> InstanceStorageConfig
mkInstanceStorageConfig storageType
  = InstanceStorageConfig'{storageType, associationId = Core.Nothing,
                           kinesisFirehoseConfig = Core.Nothing,
                           kinesisStreamConfig = Core.Nothing,
                           kinesisVideoStreamConfig = Core.Nothing, s3Config = Core.Nothing}

-- | A valid storage type.
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscStorageType :: Lens.Lens' InstanceStorageConfig Types.StorageType
iscStorageType = Lens.field @"storageType"
{-# INLINEABLE iscStorageType #-}
{-# DEPRECATED storageType "Use generic-lens or generic-optics with 'storageType' instead"  #-}

-- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscAssociationId :: Lens.Lens' InstanceStorageConfig (Core.Maybe Types.AssociationId)
iscAssociationId = Lens.field @"associationId"
{-# INLINEABLE iscAssociationId #-}
{-# DEPRECATED associationId "Use generic-lens or generic-optics with 'associationId' instead"  #-}

-- | The configuration of the Kinesis Firehose delivery stream.
--
-- /Note:/ Consider using 'kinesisFirehoseConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscKinesisFirehoseConfig :: Lens.Lens' InstanceStorageConfig (Core.Maybe Types.KinesisFirehoseConfig)
iscKinesisFirehoseConfig = Lens.field @"kinesisFirehoseConfig"
{-# INLINEABLE iscKinesisFirehoseConfig #-}
{-# DEPRECATED kinesisFirehoseConfig "Use generic-lens or generic-optics with 'kinesisFirehoseConfig' instead"  #-}

-- | The configuration of the Kinesis data stream.
--
-- /Note:/ Consider using 'kinesisStreamConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscKinesisStreamConfig :: Lens.Lens' InstanceStorageConfig (Core.Maybe Types.KinesisStreamConfig)
iscKinesisStreamConfig = Lens.field @"kinesisStreamConfig"
{-# INLINEABLE iscKinesisStreamConfig #-}
{-# DEPRECATED kinesisStreamConfig "Use generic-lens or generic-optics with 'kinesisStreamConfig' instead"  #-}

-- | The configuration of the Kinesis video stream.
--
-- /Note:/ Consider using 'kinesisVideoStreamConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscKinesisVideoStreamConfig :: Lens.Lens' InstanceStorageConfig (Core.Maybe Types.KinesisVideoStreamConfig)
iscKinesisVideoStreamConfig = Lens.field @"kinesisVideoStreamConfig"
{-# INLINEABLE iscKinesisVideoStreamConfig #-}
{-# DEPRECATED kinesisVideoStreamConfig "Use generic-lens or generic-optics with 'kinesisVideoStreamConfig' instead"  #-}

-- | The S3 configuration.
--
-- /Note:/ Consider using 's3Config' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscS3Config :: Lens.Lens' InstanceStorageConfig (Core.Maybe Types.S3Config)
iscS3Config = Lens.field @"s3Config"
{-# INLINEABLE iscS3Config #-}
{-# DEPRECATED s3Config "Use generic-lens or generic-optics with 's3Config' instead"  #-}

instance Core.FromJSON InstanceStorageConfig where
        toJSON InstanceStorageConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("StorageType" Core..= storageType),
                  ("AssociationId" Core..=) Core.<$> associationId,
                  ("KinesisFirehoseConfig" Core..=) Core.<$> kinesisFirehoseConfig,
                  ("KinesisStreamConfig" Core..=) Core.<$> kinesisStreamConfig,
                  ("KinesisVideoStreamConfig" Core..=) Core.<$>
                    kinesisVideoStreamConfig,
                  ("S3Config" Core..=) Core.<$> s3Config])

instance Core.FromJSON InstanceStorageConfig where
        parseJSON
          = Core.withObject "InstanceStorageConfig" Core.$
              \ x ->
                InstanceStorageConfig' Core.<$>
                  (x Core..: "StorageType") Core.<*> x Core..:? "AssociationId"
                    Core.<*> x Core..:? "KinesisFirehoseConfig"
                    Core.<*> x Core..:? "KinesisStreamConfig"
                    Core.<*> x Core..:? "KinesisVideoStreamConfig"
                    Core.<*> x Core..:? "S3Config"
