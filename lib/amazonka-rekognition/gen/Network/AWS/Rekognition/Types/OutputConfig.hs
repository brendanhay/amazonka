{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.OutputConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.OutputConfig
  ( OutputConfig (..),

    -- * Smart constructor
    mkOutputConfig,

    -- * Lenses
    ocS3Bucket,
    ocS3KeyPrefix,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.S3Bucket as Types
import qualified Network.AWS.Rekognition.Types.S3KeyPrefix as Types

-- | The S3 bucket and folder location where training output is placed.
--
-- /See:/ 'mkOutputConfig' smart constructor.
data OutputConfig = OutputConfig'
  { -- | The S3 bucket where training output is placed.
    s3Bucket :: Core.Maybe Types.S3Bucket,
    -- | The prefix applied to the training output files.
    s3KeyPrefix :: Core.Maybe Types.S3KeyPrefix
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OutputConfig' value with any optional fields omitted.
mkOutputConfig ::
  OutputConfig
mkOutputConfig =
  OutputConfig'
    { s3Bucket = Core.Nothing,
      s3KeyPrefix = Core.Nothing
    }

-- | The S3 bucket where training output is placed.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocS3Bucket :: Lens.Lens' OutputConfig (Core.Maybe Types.S3Bucket)
ocS3Bucket = Lens.field @"s3Bucket"
{-# DEPRECATED ocS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

-- | The prefix applied to the training output files.
--
-- /Note:/ Consider using 's3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocS3KeyPrefix :: Lens.Lens' OutputConfig (Core.Maybe Types.S3KeyPrefix)
ocS3KeyPrefix = Lens.field @"s3KeyPrefix"
{-# DEPRECATED ocS3KeyPrefix "Use generic-lens or generic-optics with 's3KeyPrefix' instead." #-}

instance Core.FromJSON OutputConfig where
  toJSON OutputConfig {..} =
    Core.object
      ( Core.catMaybes
          [ ("S3Bucket" Core..=) Core.<$> s3Bucket,
            ("S3KeyPrefix" Core..=) Core.<$> s3KeyPrefix
          ]
      )

instance Core.FromJSON OutputConfig where
  parseJSON =
    Core.withObject "OutputConfig" Core.$
      \x ->
        OutputConfig'
          Core.<$> (x Core..:? "S3Bucket") Core.<*> (x Core..:? "S3KeyPrefix")
