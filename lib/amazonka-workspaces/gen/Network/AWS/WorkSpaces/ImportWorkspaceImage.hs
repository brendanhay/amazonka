{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.ImportWorkspaceImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports the specified Windows 10 Bring Your Own License (BYOL) image into Amazon WorkSpaces. The image must be an already licensed Amazon EC2 image that is in your AWS account, and you must own the image. For more information about creating BYOL images, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Licenses> .
module Network.AWS.WorkSpaces.ImportWorkspaceImage
  ( -- * Creating a request
    ImportWorkspaceImage (..),
    mkImportWorkspaceImage,

    -- ** Request lenses
    iwiEc2ImageId,
    iwiIngestionProcess,
    iwiImageName,
    iwiImageDescription,
    iwiApplications,
    iwiTags,

    -- * Destructuring the response
    ImportWorkspaceImageResponse (..),
    mkImportWorkspaceImageResponse,

    -- ** Response lenses
    iwirrsImageId,
    iwirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkImportWorkspaceImage' smart constructor.
data ImportWorkspaceImage = ImportWorkspaceImage'
  { -- | The identifier of the EC2 image.
    ec2ImageId :: Types.Ec2ImageId,
    -- | The ingestion process to be used when importing the image. For non-GPU-enabled bundles (bundles other than Graphics or GraphicsPro), specify @BYOL_REGULAR@ .
    ingestionProcess :: Types.WorkspaceImageIngestionProcess,
    -- | The name of the WorkSpace image.
    imageName :: Types.WorkspaceImageName,
    -- | The description of the WorkSpace image.
    imageDescription :: Types.WorkspaceImageDescription,
    -- | If specified, the version of Microsoft Office to subscribe to. Valid only for Windows 10 BYOL images. For more information about subscribing to Office for BYOL images, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Licenses> .
    applications :: Core.Maybe (Core.NonEmpty Types.Application),
    -- | The tags. Each WorkSpaces resource can have a maximum of 50 tags.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportWorkspaceImage' value with any optional fields omitted.
mkImportWorkspaceImage ::
  -- | 'ec2ImageId'
  Types.Ec2ImageId ->
  -- | 'ingestionProcess'
  Types.WorkspaceImageIngestionProcess ->
  -- | 'imageName'
  Types.WorkspaceImageName ->
  -- | 'imageDescription'
  Types.WorkspaceImageDescription ->
  ImportWorkspaceImage
mkImportWorkspaceImage
  ec2ImageId
  ingestionProcess
  imageName
  imageDescription =
    ImportWorkspaceImage'
      { ec2ImageId,
        ingestionProcess,
        imageName,
        imageDescription,
        applications = Core.Nothing,
        tags = Core.Nothing
      }

-- | The identifier of the EC2 image.
--
-- /Note:/ Consider using 'ec2ImageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iwiEc2ImageId :: Lens.Lens' ImportWorkspaceImage Types.Ec2ImageId
iwiEc2ImageId = Lens.field @"ec2ImageId"
{-# DEPRECATED iwiEc2ImageId "Use generic-lens or generic-optics with 'ec2ImageId' instead." #-}

-- | The ingestion process to be used when importing the image. For non-GPU-enabled bundles (bundles other than Graphics or GraphicsPro), specify @BYOL_REGULAR@ .
--
-- /Note:/ Consider using 'ingestionProcess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iwiIngestionProcess :: Lens.Lens' ImportWorkspaceImage Types.WorkspaceImageIngestionProcess
iwiIngestionProcess = Lens.field @"ingestionProcess"
{-# DEPRECATED iwiIngestionProcess "Use generic-lens or generic-optics with 'ingestionProcess' instead." #-}

-- | The name of the WorkSpace image.
--
-- /Note:/ Consider using 'imageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iwiImageName :: Lens.Lens' ImportWorkspaceImage Types.WorkspaceImageName
iwiImageName = Lens.field @"imageName"
{-# DEPRECATED iwiImageName "Use generic-lens or generic-optics with 'imageName' instead." #-}

-- | The description of the WorkSpace image.
--
-- /Note:/ Consider using 'imageDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iwiImageDescription :: Lens.Lens' ImportWorkspaceImage Types.WorkspaceImageDescription
iwiImageDescription = Lens.field @"imageDescription"
{-# DEPRECATED iwiImageDescription "Use generic-lens or generic-optics with 'imageDescription' instead." #-}

-- | If specified, the version of Microsoft Office to subscribe to. Valid only for Windows 10 BYOL images. For more information about subscribing to Office for BYOL images, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Licenses> .
--
-- /Note:/ Consider using 'applications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iwiApplications :: Lens.Lens' ImportWorkspaceImage (Core.Maybe (Core.NonEmpty Types.Application))
iwiApplications = Lens.field @"applications"
{-# DEPRECATED iwiApplications "Use generic-lens or generic-optics with 'applications' instead." #-}

-- | The tags. Each WorkSpaces resource can have a maximum of 50 tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iwiTags :: Lens.Lens' ImportWorkspaceImage (Core.Maybe [Types.Tag])
iwiTags = Lens.field @"tags"
{-# DEPRECATED iwiTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON ImportWorkspaceImage where
  toJSON ImportWorkspaceImage {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Ec2ImageId" Core..= ec2ImageId),
            Core.Just ("IngestionProcess" Core..= ingestionProcess),
            Core.Just ("ImageName" Core..= imageName),
            Core.Just ("ImageDescription" Core..= imageDescription),
            ("Applications" Core..=) Core.<$> applications,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest ImportWorkspaceImage where
  type Rs ImportWorkspaceImage = ImportWorkspaceImageResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "WorkspacesService.ImportWorkspaceImage")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportWorkspaceImageResponse'
            Core.<$> (x Core..:? "ImageId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkImportWorkspaceImageResponse' smart constructor.
data ImportWorkspaceImageResponse = ImportWorkspaceImageResponse'
  { -- | The identifier of the WorkSpace image.
    imageId :: Core.Maybe Types.WorkspaceImageId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportWorkspaceImageResponse' value with any optional fields omitted.
mkImportWorkspaceImageResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ImportWorkspaceImageResponse
mkImportWorkspaceImageResponse responseStatus =
  ImportWorkspaceImageResponse'
    { imageId = Core.Nothing,
      responseStatus
    }

-- | The identifier of the WorkSpace image.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iwirrsImageId :: Lens.Lens' ImportWorkspaceImageResponse (Core.Maybe Types.WorkspaceImageId)
iwirrsImageId = Lens.field @"imageId"
{-# DEPRECATED iwirrsImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iwirrsResponseStatus :: Lens.Lens' ImportWorkspaceImageResponse Core.Int
iwirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED iwirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
