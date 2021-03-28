{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLOutputDataConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.AutoMLOutputDataConfig
  ( AutoMLOutputDataConfig (..)
  -- * Smart constructor
  , mkAutoMLOutputDataConfig
  -- * Lenses
  , amlodcS3OutputPath
  , amlodcKmsKeyId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.KmsKeyId as Types
import qualified Network.AWS.SageMaker.Types.S3OutputPath as Types

-- | The output data configuration.
--
-- /See:/ 'mkAutoMLOutputDataConfig' smart constructor.
data AutoMLOutputDataConfig = AutoMLOutputDataConfig'
  { s3OutputPath :: Types.S3OutputPath
    -- ^ The Amazon S3 output path. Must be 128 characters or less.
  , kmsKeyId :: Core.Maybe Types.KmsKeyId
    -- ^ The AWS KMS encryption key ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AutoMLOutputDataConfig' value with any optional fields omitted.
mkAutoMLOutputDataConfig
    :: Types.S3OutputPath -- ^ 's3OutputPath'
    -> AutoMLOutputDataConfig
mkAutoMLOutputDataConfig s3OutputPath
  = AutoMLOutputDataConfig'{s3OutputPath, kmsKeyId = Core.Nothing}

-- | The Amazon S3 output path. Must be 128 characters or less.
--
-- /Note:/ Consider using 's3OutputPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlodcS3OutputPath :: Lens.Lens' AutoMLOutputDataConfig Types.S3OutputPath
amlodcS3OutputPath = Lens.field @"s3OutputPath"
{-# INLINEABLE amlodcS3OutputPath #-}
{-# DEPRECATED s3OutputPath "Use generic-lens or generic-optics with 's3OutputPath' instead"  #-}

-- | The AWS KMS encryption key ID.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlodcKmsKeyId :: Lens.Lens' AutoMLOutputDataConfig (Core.Maybe Types.KmsKeyId)
amlodcKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE amlodcKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

instance Core.FromJSON AutoMLOutputDataConfig where
        toJSON AutoMLOutputDataConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("S3OutputPath" Core..= s3OutputPath),
                  ("KmsKeyId" Core..=) Core.<$> kmsKeyId])

instance Core.FromJSON AutoMLOutputDataConfig where
        parseJSON
          = Core.withObject "AutoMLOutputDataConfig" Core.$
              \ x ->
                AutoMLOutputDataConfig' Core.<$>
                  (x Core..: "S3OutputPath") Core.<*> x Core..:? "KmsKeyId"
