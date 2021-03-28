{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.Image
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.Image
  ( Image (..)
  -- * Smart constructor
  , mkImage
  -- * Lenses
  , iName
  , iApplications
  , iAppstreamAgentVersion
  , iArn
  , iBaseImageArn
  , iCreatedTime
  , iDescription
  , iDisplayName
  , iImageBuilderName
  , iImageBuilderSupported
  , iImagePermissions
  , iPlatform
  , iPublicBaseImageReleasedDate
  , iState
  , iStateChangeReason
  , iVisibility
  ) where

import qualified Network.AWS.AppStream.Types.Application as Types
import qualified Network.AWS.AppStream.Types.AppstreamAgentVersion as Types
import qualified Network.AWS.AppStream.Types.Arn as Types
import qualified Network.AWS.AppStream.Types.BaseImageArn as Types
import qualified Network.AWS.AppStream.Types.ImagePermissions as Types
import qualified Network.AWS.AppStream.Types.ImageState as Types
import qualified Network.AWS.AppStream.Types.ImageStateChangeReason as Types
import qualified Network.AWS.AppStream.Types.PlatformType as Types
import qualified Network.AWS.AppStream.Types.VisibilityType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an image.
--
-- /See:/ 'mkImage' smart constructor.
data Image = Image'
  { name :: Core.Text
    -- ^ The name of the image.
  , applications :: Core.Maybe [Types.Application]
    -- ^ The applications associated with the image.
  , appstreamAgentVersion :: Core.Maybe Types.AppstreamAgentVersion
    -- ^ The version of the AppStream 2.0 agent to use for instances that are launched from this image. 
  , arn :: Core.Maybe Types.Arn
    -- ^ The ARN of the image.
  , baseImageArn :: Core.Maybe Types.BaseImageArn
    -- ^ The ARN of the image from which this image was created.
  , createdTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time the image was created.
  , description :: Core.Maybe Core.Text
    -- ^ The description to display.
  , displayName :: Core.Maybe Core.Text
    -- ^ The image name to display.
  , imageBuilderName :: Core.Maybe Core.Text
    -- ^ The name of the image builder that was used to create the private image. If the image is shared, this value is null.
  , imageBuilderSupported :: Core.Maybe Core.Bool
    -- ^ Indicates whether an image builder can be launched from this image.
  , imagePermissions :: Core.Maybe Types.ImagePermissions
    -- ^ The permissions to provide to the destination AWS account for the specified image.
  , platform :: Core.Maybe Types.PlatformType
    -- ^ The operating system platform of the image.
  , publicBaseImageReleasedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The release date of the public base image. For private images, this date is the release date of the base image from which the image was created.
  , state :: Core.Maybe Types.ImageState
    -- ^ The image starts in the @PENDING@ state. If image creation succeeds, the state is @AVAILABLE@ . If image creation fails, the state is @FAILED@ .
  , stateChangeReason :: Core.Maybe Types.ImageStateChangeReason
    -- ^ The reason why the last state change occurred.
  , visibility :: Core.Maybe Types.VisibilityType
    -- ^ Indicates whether the image is public or private.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Image' value with any optional fields omitted.
mkImage
    :: Core.Text -- ^ 'name'
    -> Image
mkImage name
  = Image'{name, applications = Core.Nothing,
           appstreamAgentVersion = Core.Nothing, arn = Core.Nothing,
           baseImageArn = Core.Nothing, createdTime = Core.Nothing,
           description = Core.Nothing, displayName = Core.Nothing,
           imageBuilderName = Core.Nothing,
           imageBuilderSupported = Core.Nothing,
           imagePermissions = Core.Nothing, platform = Core.Nothing,
           publicBaseImageReleasedDate = Core.Nothing, state = Core.Nothing,
           stateChangeReason = Core.Nothing, visibility = Core.Nothing}

-- | The name of the image.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iName :: Lens.Lens' Image Core.Text
iName = Lens.field @"name"
{-# INLINEABLE iName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The applications associated with the image.
--
-- /Note:/ Consider using 'applications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iApplications :: Lens.Lens' Image (Core.Maybe [Types.Application])
iApplications = Lens.field @"applications"
{-# INLINEABLE iApplications #-}
{-# DEPRECATED applications "Use generic-lens or generic-optics with 'applications' instead"  #-}

-- | The version of the AppStream 2.0 agent to use for instances that are launched from this image. 
--
-- /Note:/ Consider using 'appstreamAgentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iAppstreamAgentVersion :: Lens.Lens' Image (Core.Maybe Types.AppstreamAgentVersion)
iAppstreamAgentVersion = Lens.field @"appstreamAgentVersion"
{-# INLINEABLE iAppstreamAgentVersion #-}
{-# DEPRECATED appstreamAgentVersion "Use generic-lens or generic-optics with 'appstreamAgentVersion' instead"  #-}

-- | The ARN of the image.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iArn :: Lens.Lens' Image (Core.Maybe Types.Arn)
iArn = Lens.field @"arn"
{-# INLINEABLE iArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The ARN of the image from which this image was created.
--
-- /Note:/ Consider using 'baseImageArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iBaseImageArn :: Lens.Lens' Image (Core.Maybe Types.BaseImageArn)
iBaseImageArn = Lens.field @"baseImageArn"
{-# INLINEABLE iBaseImageArn #-}
{-# DEPRECATED baseImageArn "Use generic-lens or generic-optics with 'baseImageArn' instead"  #-}

-- | The time the image was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCreatedTime :: Lens.Lens' Image (Core.Maybe Core.NominalDiffTime)
iCreatedTime = Lens.field @"createdTime"
{-# INLINEABLE iCreatedTime #-}
{-# DEPRECATED createdTime "Use generic-lens or generic-optics with 'createdTime' instead"  #-}

-- | The description to display.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iDescription :: Lens.Lens' Image (Core.Maybe Core.Text)
iDescription = Lens.field @"description"
{-# INLINEABLE iDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The image name to display.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iDisplayName :: Lens.Lens' Image (Core.Maybe Core.Text)
iDisplayName = Lens.field @"displayName"
{-# INLINEABLE iDisplayName #-}
{-# DEPRECATED displayName "Use generic-lens or generic-optics with 'displayName' instead"  #-}

-- | The name of the image builder that was used to create the private image. If the image is shared, this value is null.
--
-- /Note:/ Consider using 'imageBuilderName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iImageBuilderName :: Lens.Lens' Image (Core.Maybe Core.Text)
iImageBuilderName = Lens.field @"imageBuilderName"
{-# INLINEABLE iImageBuilderName #-}
{-# DEPRECATED imageBuilderName "Use generic-lens or generic-optics with 'imageBuilderName' instead"  #-}

-- | Indicates whether an image builder can be launched from this image.
--
-- /Note:/ Consider using 'imageBuilderSupported' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iImageBuilderSupported :: Lens.Lens' Image (Core.Maybe Core.Bool)
iImageBuilderSupported = Lens.field @"imageBuilderSupported"
{-# INLINEABLE iImageBuilderSupported #-}
{-# DEPRECATED imageBuilderSupported "Use generic-lens or generic-optics with 'imageBuilderSupported' instead"  #-}

-- | The permissions to provide to the destination AWS account for the specified image.
--
-- /Note:/ Consider using 'imagePermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iImagePermissions :: Lens.Lens' Image (Core.Maybe Types.ImagePermissions)
iImagePermissions = Lens.field @"imagePermissions"
{-# INLINEABLE iImagePermissions #-}
{-# DEPRECATED imagePermissions "Use generic-lens or generic-optics with 'imagePermissions' instead"  #-}

-- | The operating system platform of the image.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPlatform :: Lens.Lens' Image (Core.Maybe Types.PlatformType)
iPlatform = Lens.field @"platform"
{-# INLINEABLE iPlatform #-}
{-# DEPRECATED platform "Use generic-lens or generic-optics with 'platform' instead"  #-}

-- | The release date of the public base image. For private images, this date is the release date of the base image from which the image was created.
--
-- /Note:/ Consider using 'publicBaseImageReleasedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPublicBaseImageReleasedDate :: Lens.Lens' Image (Core.Maybe Core.NominalDiffTime)
iPublicBaseImageReleasedDate = Lens.field @"publicBaseImageReleasedDate"
{-# INLINEABLE iPublicBaseImageReleasedDate #-}
{-# DEPRECATED publicBaseImageReleasedDate "Use generic-lens or generic-optics with 'publicBaseImageReleasedDate' instead"  #-}

-- | The image starts in the @PENDING@ state. If image creation succeeds, the state is @AVAILABLE@ . If image creation fails, the state is @FAILED@ .
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iState :: Lens.Lens' Image (Core.Maybe Types.ImageState)
iState = Lens.field @"state"
{-# INLINEABLE iState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The reason why the last state change occurred.
--
-- /Note:/ Consider using 'stateChangeReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iStateChangeReason :: Lens.Lens' Image (Core.Maybe Types.ImageStateChangeReason)
iStateChangeReason = Lens.field @"stateChangeReason"
{-# INLINEABLE iStateChangeReason #-}
{-# DEPRECATED stateChangeReason "Use generic-lens or generic-optics with 'stateChangeReason' instead"  #-}

-- | Indicates whether the image is public or private.
--
-- /Note:/ Consider using 'visibility' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iVisibility :: Lens.Lens' Image (Core.Maybe Types.VisibilityType)
iVisibility = Lens.field @"visibility"
{-# INLINEABLE iVisibility #-}
{-# DEPRECATED visibility "Use generic-lens or generic-optics with 'visibility' instead"  #-}

instance Core.FromJSON Image where
        parseJSON
          = Core.withObject "Image" Core.$
              \ x ->
                Image' Core.<$>
                  (x Core..: "Name") Core.<*> x Core..:? "Applications" Core.<*>
                    x Core..:? "AppstreamAgentVersion"
                    Core.<*> x Core..:? "Arn"
                    Core.<*> x Core..:? "BaseImageArn"
                    Core.<*> x Core..:? "CreatedTime"
                    Core.<*> x Core..:? "Description"
                    Core.<*> x Core..:? "DisplayName"
                    Core.<*> x Core..:? "ImageBuilderName"
                    Core.<*> x Core..:? "ImageBuilderSupported"
                    Core.<*> x Core..:? "ImagePermissions"
                    Core.<*> x Core..:? "Platform"
                    Core.<*> x Core..:? "PublicBaseImageReleasedDate"
                    Core.<*> x Core..:? "State"
                    Core.<*> x Core..:? "StateChangeReason"
                    Core.<*> x Core..:? "Visibility"
