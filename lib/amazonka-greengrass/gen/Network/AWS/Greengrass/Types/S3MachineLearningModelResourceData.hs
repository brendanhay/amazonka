{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.S3MachineLearningModelResourceData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.S3MachineLearningModelResourceData
  ( S3MachineLearningModelResourceData (..),

    -- * Smart constructor
    mkS3MachineLearningModelResourceData,

    -- * Lenses
    smlmrdDestinationPath,
    smlmrdOwnerSetting,
    smlmrdS3Uri,
  )
where

import qualified Network.AWS.Greengrass.Types.ResourceDownloadOwnerSetting as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Attributes that define an Amazon S3 machine learning resource.
--
-- /See:/ 'mkS3MachineLearningModelResourceData' smart constructor.
data S3MachineLearningModelResourceData = S3MachineLearningModelResourceData'
  { -- | The absolute local path of the resource inside the Lambda environment.
    destinationPath :: Core.Maybe Core.Text,
    ownerSetting :: Core.Maybe Types.ResourceDownloadOwnerSetting,
    -- | The URI of the source model in an S3 bucket. The model package must be in tar.gz or .zip format.
    s3Uri :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3MachineLearningModelResourceData' value with any optional fields omitted.
mkS3MachineLearningModelResourceData ::
  S3MachineLearningModelResourceData
mkS3MachineLearningModelResourceData =
  S3MachineLearningModelResourceData'
    { destinationPath =
        Core.Nothing,
      ownerSetting = Core.Nothing,
      s3Uri = Core.Nothing
    }

-- | The absolute local path of the resource inside the Lambda environment.
--
-- /Note:/ Consider using 'destinationPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smlmrdDestinationPath :: Lens.Lens' S3MachineLearningModelResourceData (Core.Maybe Core.Text)
smlmrdDestinationPath = Lens.field @"destinationPath"
{-# DEPRECATED smlmrdDestinationPath "Use generic-lens or generic-optics with 'destinationPath' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'ownerSetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smlmrdOwnerSetting :: Lens.Lens' S3MachineLearningModelResourceData (Core.Maybe Types.ResourceDownloadOwnerSetting)
smlmrdOwnerSetting = Lens.field @"ownerSetting"
{-# DEPRECATED smlmrdOwnerSetting "Use generic-lens or generic-optics with 'ownerSetting' instead." #-}

-- | The URI of the source model in an S3 bucket. The model package must be in tar.gz or .zip format.
--
-- /Note:/ Consider using 's3Uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smlmrdS3Uri :: Lens.Lens' S3MachineLearningModelResourceData (Core.Maybe Core.Text)
smlmrdS3Uri = Lens.field @"s3Uri"
{-# DEPRECATED smlmrdS3Uri "Use generic-lens or generic-optics with 's3Uri' instead." #-}

instance Core.FromJSON S3MachineLearningModelResourceData where
  toJSON S3MachineLearningModelResourceData {..} =
    Core.object
      ( Core.catMaybes
          [ ("DestinationPath" Core..=) Core.<$> destinationPath,
            ("OwnerSetting" Core..=) Core.<$> ownerSetting,
            ("S3Uri" Core..=) Core.<$> s3Uri
          ]
      )

instance Core.FromJSON S3MachineLearningModelResourceData where
  parseJSON =
    Core.withObject "S3MachineLearningModelResourceData" Core.$
      \x ->
        S3MachineLearningModelResourceData'
          Core.<$> (x Core..:? "DestinationPath")
          Core.<*> (x Core..:? "OwnerSetting")
          Core.<*> (x Core..:? "S3Uri")
