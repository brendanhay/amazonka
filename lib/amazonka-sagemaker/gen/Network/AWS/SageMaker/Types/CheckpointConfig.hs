{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CheckpointConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.CheckpointConfig
  ( CheckpointConfig (..)
  -- * Smart constructor
  , mkCheckpointConfig
  -- * Lenses
  , ccS3Uri
  , ccLocalPath
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.LocalPath as Types
import qualified Network.AWS.SageMaker.Types.S3Uri as Types

-- | Contains information about the output location for managed spot training checkpoint data. 
--
-- /See:/ 'mkCheckpointConfig' smart constructor.
data CheckpointConfig = CheckpointConfig'
  { s3Uri :: Types.S3Uri
    -- ^ Identifies the S3 path where you want Amazon SageMaker to store checkpoints. For example, @s3://bucket-name/key-name-prefix@ .
  , localPath :: Core.Maybe Types.LocalPath
    -- ^ (Optional) The local directory where checkpoints are written. The default directory is @/opt/ml/checkpoints/@ . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CheckpointConfig' value with any optional fields omitted.
mkCheckpointConfig
    :: Types.S3Uri -- ^ 's3Uri'
    -> CheckpointConfig
mkCheckpointConfig s3Uri
  = CheckpointConfig'{s3Uri, localPath = Core.Nothing}

-- | Identifies the S3 path where you want Amazon SageMaker to store checkpoints. For example, @s3://bucket-name/key-name-prefix@ .
--
-- /Note:/ Consider using 's3Uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccS3Uri :: Lens.Lens' CheckpointConfig Types.S3Uri
ccS3Uri = Lens.field @"s3Uri"
{-# INLINEABLE ccS3Uri #-}
{-# DEPRECATED s3Uri "Use generic-lens or generic-optics with 's3Uri' instead"  #-}

-- | (Optional) The local directory where checkpoints are written. The default directory is @/opt/ml/checkpoints/@ . 
--
-- /Note:/ Consider using 'localPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccLocalPath :: Lens.Lens' CheckpointConfig (Core.Maybe Types.LocalPath)
ccLocalPath = Lens.field @"localPath"
{-# INLINEABLE ccLocalPath #-}
{-# DEPRECATED localPath "Use generic-lens or generic-optics with 'localPath' instead"  #-}

instance Core.FromJSON CheckpointConfig where
        toJSON CheckpointConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("S3Uri" Core..= s3Uri),
                  ("LocalPath" Core..=) Core.<$> localPath])

instance Core.FromJSON CheckpointConfig where
        parseJSON
          = Core.withObject "CheckpointConfig" Core.$
              \ x ->
                CheckpointConfig' Core.<$>
                  (x Core..: "S3Uri") Core.<*> x Core..:? "LocalPath"
