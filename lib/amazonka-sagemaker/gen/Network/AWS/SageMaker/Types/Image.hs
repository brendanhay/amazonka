{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.Image
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.Image
  ( Image (..)
  -- * Smart constructor
  , mkImage
  -- * Lenses
  , iCreationTime
  , iImageArn
  , iImageName
  , iImageStatus
  , iLastModifiedTime
  , iDescription
  , iDisplayName
  , iFailureReason
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.Description as Types
import qualified Network.AWS.SageMaker.Types.DisplayName as Types
import qualified Network.AWS.SageMaker.Types.FailureReason as Types
import qualified Network.AWS.SageMaker.Types.ImageArn as Types
import qualified Network.AWS.SageMaker.Types.ImageName as Types
import qualified Network.AWS.SageMaker.Types.ImageStatus as Types

-- | A SageMaker image. A SageMaker image represents a set of container images that are derived from a common base container image. Each of these container images is represented by a SageMaker @ImageVersion@ .
--
-- /See:/ 'mkImage' smart constructor.
data Image = Image'
  { creationTime :: Core.NominalDiffTime
    -- ^ When the image was created.
  , imageArn :: Types.ImageArn
    -- ^ The Amazon Resource Name (ARN) of the image.
  , imageName :: Types.ImageName
    -- ^ The name of the image.
  , imageStatus :: Types.ImageStatus
    -- ^ The status of the image.
  , lastModifiedTime :: Core.NominalDiffTime
    -- ^ When the image was last modified.
  , description :: Core.Maybe Types.Description
    -- ^ The description of the image.
  , displayName :: Core.Maybe Types.DisplayName
    -- ^ The name of the image as displayed.
  , failureReason :: Core.Maybe Types.FailureReason
    -- ^ When a create, update, or delete operation fails, the reason for the failure.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Image' value with any optional fields omitted.
mkImage
    :: Core.NominalDiffTime -- ^ 'creationTime'
    -> Types.ImageArn -- ^ 'imageArn'
    -> Types.ImageName -- ^ 'imageName'
    -> Types.ImageStatus -- ^ 'imageStatus'
    -> Core.NominalDiffTime -- ^ 'lastModifiedTime'
    -> Image
mkImage creationTime imageArn imageName imageStatus
  lastModifiedTime
  = Image'{creationTime, imageArn, imageName, imageStatus,
           lastModifiedTime, description = Core.Nothing,
           displayName = Core.Nothing, failureReason = Core.Nothing}

-- | When the image was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCreationTime :: Lens.Lens' Image Core.NominalDiffTime
iCreationTime = Lens.field @"creationTime"
{-# INLINEABLE iCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The Amazon Resource Name (ARN) of the image.
--
-- /Note:/ Consider using 'imageArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iImageArn :: Lens.Lens' Image Types.ImageArn
iImageArn = Lens.field @"imageArn"
{-# INLINEABLE iImageArn #-}
{-# DEPRECATED imageArn "Use generic-lens or generic-optics with 'imageArn' instead"  #-}

-- | The name of the image.
--
-- /Note:/ Consider using 'imageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iImageName :: Lens.Lens' Image Types.ImageName
iImageName = Lens.field @"imageName"
{-# INLINEABLE iImageName #-}
{-# DEPRECATED imageName "Use generic-lens or generic-optics with 'imageName' instead"  #-}

-- | The status of the image.
--
-- /Note:/ Consider using 'imageStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iImageStatus :: Lens.Lens' Image Types.ImageStatus
iImageStatus = Lens.field @"imageStatus"
{-# INLINEABLE iImageStatus #-}
{-# DEPRECATED imageStatus "Use generic-lens or generic-optics with 'imageStatus' instead"  #-}

-- | When the image was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iLastModifiedTime :: Lens.Lens' Image Core.NominalDiffTime
iLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE iLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

-- | The description of the image.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iDescription :: Lens.Lens' Image (Core.Maybe Types.Description)
iDescription = Lens.field @"description"
{-# INLINEABLE iDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The name of the image as displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iDisplayName :: Lens.Lens' Image (Core.Maybe Types.DisplayName)
iDisplayName = Lens.field @"displayName"
{-# INLINEABLE iDisplayName #-}
{-# DEPRECATED displayName "Use generic-lens or generic-optics with 'displayName' instead"  #-}

-- | When a create, update, or delete operation fails, the reason for the failure.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iFailureReason :: Lens.Lens' Image (Core.Maybe Types.FailureReason)
iFailureReason = Lens.field @"failureReason"
{-# INLINEABLE iFailureReason #-}
{-# DEPRECATED failureReason "Use generic-lens or generic-optics with 'failureReason' instead"  #-}

instance Core.FromJSON Image where
        parseJSON
          = Core.withObject "Image" Core.$
              \ x ->
                Image' Core.<$>
                  (x Core..: "CreationTime") Core.<*> x Core..: "ImageArn" Core.<*>
                    x Core..: "ImageName"
                    Core.<*> x Core..: "ImageStatus"
                    Core.<*> x Core..: "LastModifiedTime"
                    Core.<*> x Core..:? "Description"
                    Core.<*> x Core..:? "DisplayName"
                    Core.<*> x Core..:? "FailureReason"
