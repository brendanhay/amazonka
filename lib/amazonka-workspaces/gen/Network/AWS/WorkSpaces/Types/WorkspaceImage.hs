{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspaceImage
  ( WorkspaceImage (..),

    -- * Smart constructor
    mkWorkspaceImage,

    -- * Lenses
    wiCreated,
    wiDescription,
    wiErrorCode,
    wiErrorMessage,
    wiImageId,
    wiName,
    wiOperatingSystem,
    wiOwnerAccountId,
    wiRequiredTenancy,
    wiState,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkSpaces.Types.AwsAccount as Types
import qualified Network.AWS.WorkSpaces.Types.ErrorMessage as Types
import qualified Network.AWS.WorkSpaces.Types.OperatingSystem as Types
import qualified Network.AWS.WorkSpaces.Types.WorkspaceImageDescription as Types
import qualified Network.AWS.WorkSpaces.Types.WorkspaceImageErrorCode as Types
import qualified Network.AWS.WorkSpaces.Types.WorkspaceImageId as Types
import qualified Network.AWS.WorkSpaces.Types.WorkspaceImageName as Types
import qualified Network.AWS.WorkSpaces.Types.WorkspaceImageRequiredTenancy as Types
import qualified Network.AWS.WorkSpaces.Types.WorkspaceImageState as Types

-- | Describes a WorkSpace image.
--
-- /See:/ 'mkWorkspaceImage' smart constructor.
data WorkspaceImage = WorkspaceImage'
  { -- | The date when the image was created. If the image has been shared, the AWS account that the image has been shared with sees the original creation date of the image.
    created :: Core.Maybe Core.NominalDiffTime,
    -- | The description of the image.
    description :: Core.Maybe Types.WorkspaceImageDescription,
    -- | The error code that is returned for the image.
    errorCode :: Core.Maybe Types.WorkspaceImageErrorCode,
    -- | The text of the error message that is returned for the image.
    errorMessage :: Core.Maybe Types.ErrorMessage,
    -- | The identifier of the image.
    imageId :: Core.Maybe Types.WorkspaceImageId,
    -- | The name of the image.
    name :: Core.Maybe Types.WorkspaceImageName,
    -- | The operating system that the image is running.
    operatingSystem :: Core.Maybe Types.OperatingSystem,
    -- | The identifier of the AWS account that owns the image.
    ownerAccountId :: Core.Maybe Types.AwsAccount,
    -- | Specifies whether the image is running on dedicated hardware. When Bring Your Own License (BYOL) is enabled, this value is set to @DEDICATED@ . For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Images> .
    requiredTenancy :: Core.Maybe Types.WorkspaceImageRequiredTenancy,
    -- | The status of the image.
    state :: Core.Maybe Types.WorkspaceImageState
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'WorkspaceImage' value with any optional fields omitted.
mkWorkspaceImage ::
  WorkspaceImage
mkWorkspaceImage =
  WorkspaceImage'
    { created = Core.Nothing,
      description = Core.Nothing,
      errorCode = Core.Nothing,
      errorMessage = Core.Nothing,
      imageId = Core.Nothing,
      name = Core.Nothing,
      operatingSystem = Core.Nothing,
      ownerAccountId = Core.Nothing,
      requiredTenancy = Core.Nothing,
      state = Core.Nothing
    }

-- | The date when the image was created. If the image has been shared, the AWS account that the image has been shared with sees the original creation date of the image.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wiCreated :: Lens.Lens' WorkspaceImage (Core.Maybe Core.NominalDiffTime)
wiCreated = Lens.field @"created"
{-# DEPRECATED wiCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | The description of the image.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wiDescription :: Lens.Lens' WorkspaceImage (Core.Maybe Types.WorkspaceImageDescription)
wiDescription = Lens.field @"description"
{-# DEPRECATED wiDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The error code that is returned for the image.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wiErrorCode :: Lens.Lens' WorkspaceImage (Core.Maybe Types.WorkspaceImageErrorCode)
wiErrorCode = Lens.field @"errorCode"
{-# DEPRECATED wiErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The text of the error message that is returned for the image.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wiErrorMessage :: Lens.Lens' WorkspaceImage (Core.Maybe Types.ErrorMessage)
wiErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED wiErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The identifier of the image.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wiImageId :: Lens.Lens' WorkspaceImage (Core.Maybe Types.WorkspaceImageId)
wiImageId = Lens.field @"imageId"
{-# DEPRECATED wiImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The name of the image.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wiName :: Lens.Lens' WorkspaceImage (Core.Maybe Types.WorkspaceImageName)
wiName = Lens.field @"name"
{-# DEPRECATED wiName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The operating system that the image is running.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wiOperatingSystem :: Lens.Lens' WorkspaceImage (Core.Maybe Types.OperatingSystem)
wiOperatingSystem = Lens.field @"operatingSystem"
{-# DEPRECATED wiOperatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead." #-}

-- | The identifier of the AWS account that owns the image.
--
-- /Note:/ Consider using 'ownerAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wiOwnerAccountId :: Lens.Lens' WorkspaceImage (Core.Maybe Types.AwsAccount)
wiOwnerAccountId = Lens.field @"ownerAccountId"
{-# DEPRECATED wiOwnerAccountId "Use generic-lens or generic-optics with 'ownerAccountId' instead." #-}

-- | Specifies whether the image is running on dedicated hardware. When Bring Your Own License (BYOL) is enabled, this value is set to @DEDICATED@ . For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Images> .
--
-- /Note:/ Consider using 'requiredTenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wiRequiredTenancy :: Lens.Lens' WorkspaceImage (Core.Maybe Types.WorkspaceImageRequiredTenancy)
wiRequiredTenancy = Lens.field @"requiredTenancy"
{-# DEPRECATED wiRequiredTenancy "Use generic-lens or generic-optics with 'requiredTenancy' instead." #-}

-- | The status of the image.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wiState :: Lens.Lens' WorkspaceImage (Core.Maybe Types.WorkspaceImageState)
wiState = Lens.field @"state"
{-# DEPRECATED wiState "Use generic-lens or generic-optics with 'state' instead." #-}

instance Core.FromJSON WorkspaceImage where
  parseJSON =
    Core.withObject "WorkspaceImage" Core.$
      \x ->
        WorkspaceImage'
          Core.<$> (x Core..:? "Created")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "ErrorCode")
          Core.<*> (x Core..:? "ErrorMessage")
          Core.<*> (x Core..:? "ImageId")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "OperatingSystem")
          Core.<*> (x Core..:? "OwnerAccountId")
          Core.<*> (x Core..:? "RequiredTenancy")
          Core.<*> (x Core..:? "State")
