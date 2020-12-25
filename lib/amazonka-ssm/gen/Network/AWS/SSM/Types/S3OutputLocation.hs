{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.S3OutputLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.S3OutputLocation
  ( S3OutputLocation (..),

    -- * Smart constructor
    mkS3OutputLocation,

    -- * Lenses
    solOutputS3BucketName,
    solOutputS3KeyPrefix,
    solOutputS3Region,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.OutputS3BucketName as Types
import qualified Network.AWS.SSM.Types.OutputS3Region as Types
import qualified Network.AWS.SSM.Types.S3KeyPrefix as Types

-- | An S3 bucket where you want to store the results of this request.
--
-- /See:/ 'mkS3OutputLocation' smart constructor.
data S3OutputLocation = S3OutputLocation'
  { -- | The name of the S3 bucket.
    outputS3BucketName :: Core.Maybe Types.OutputS3BucketName,
    -- | The S3 bucket subfolder.
    outputS3KeyPrefix :: Core.Maybe Types.S3KeyPrefix,
    -- | (Deprecated) You can no longer specify this parameter. The system ignores it. Instead, Systems Manager automatically determines the Region of the S3 bucket.
    outputS3Region :: Core.Maybe Types.OutputS3Region
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3OutputLocation' value with any optional fields omitted.
mkS3OutputLocation ::
  S3OutputLocation
mkS3OutputLocation =
  S3OutputLocation'
    { outputS3BucketName = Core.Nothing,
      outputS3KeyPrefix = Core.Nothing,
      outputS3Region = Core.Nothing
    }

-- | The name of the S3 bucket.
--
-- /Note:/ Consider using 'outputS3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
solOutputS3BucketName :: Lens.Lens' S3OutputLocation (Core.Maybe Types.OutputS3BucketName)
solOutputS3BucketName = Lens.field @"outputS3BucketName"
{-# DEPRECATED solOutputS3BucketName "Use generic-lens or generic-optics with 'outputS3BucketName' instead." #-}

-- | The S3 bucket subfolder.
--
-- /Note:/ Consider using 'outputS3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
solOutputS3KeyPrefix :: Lens.Lens' S3OutputLocation (Core.Maybe Types.S3KeyPrefix)
solOutputS3KeyPrefix = Lens.field @"outputS3KeyPrefix"
{-# DEPRECATED solOutputS3KeyPrefix "Use generic-lens or generic-optics with 'outputS3KeyPrefix' instead." #-}

-- | (Deprecated) You can no longer specify this parameter. The system ignores it. Instead, Systems Manager automatically determines the Region of the S3 bucket.
--
-- /Note:/ Consider using 'outputS3Region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
solOutputS3Region :: Lens.Lens' S3OutputLocation (Core.Maybe Types.OutputS3Region)
solOutputS3Region = Lens.field @"outputS3Region"
{-# DEPRECATED solOutputS3Region "Use generic-lens or generic-optics with 'outputS3Region' instead." #-}

instance Core.FromJSON S3OutputLocation where
  toJSON S3OutputLocation {..} =
    Core.object
      ( Core.catMaybes
          [ ("OutputS3BucketName" Core..=) Core.<$> outputS3BucketName,
            ("OutputS3KeyPrefix" Core..=) Core.<$> outputS3KeyPrefix,
            ("OutputS3Region" Core..=) Core.<$> outputS3Region
          ]
      )

instance Core.FromJSON S3OutputLocation where
  parseJSON =
    Core.withObject "S3OutputLocation" Core.$
      \x ->
        S3OutputLocation'
          Core.<$> (x Core..:? "OutputS3BucketName")
          Core.<*> (x Core..:? "OutputS3KeyPrefix")
          Core.<*> (x Core..:? "OutputS3Region")
