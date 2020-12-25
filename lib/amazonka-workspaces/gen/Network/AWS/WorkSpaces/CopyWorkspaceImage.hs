{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.CopyWorkspaceImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified image from the specified Region to the current Region. For more information about copying images, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/copy-custom-image.html Copy a Custom WorkSpaces Image> .
--
-- /Important:/ Before copying a shared image, be sure to verify that it has been shared from the correct AWS account. To determine if an image has been shared and to see the AWS account ID that owns an image, use the <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaceImages.html DescribeWorkSpaceImages> and <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaceImagePermissions.html DescribeWorkspaceImagePermissions> API operations.
module Network.AWS.WorkSpaces.CopyWorkspaceImage
  ( -- * Creating a request
    CopyWorkspaceImage (..),
    mkCopyWorkspaceImage,

    -- ** Request lenses
    cwiName,
    cwiSourceImageId,
    cwiSourceRegion,
    cwiDescription,
    cwiTags,

    -- * Destructuring the response
    CopyWorkspaceImageResponse (..),
    mkCopyWorkspaceImageResponse,

    -- ** Response lenses
    cwirrsImageId,
    cwirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkCopyWorkspaceImage' smart constructor.
data CopyWorkspaceImage = CopyWorkspaceImage'
  { -- | The name of the image.
    name :: Types.WorkspaceImageName,
    -- | The identifier of the source image.
    sourceImageId :: Types.WorkspaceImageId,
    -- | The identifier of the source Region.
    sourceRegion :: Types.SourceRegion,
    -- | A description of the image.
    description :: Core.Maybe Types.Description,
    -- | The tags for the image.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CopyWorkspaceImage' value with any optional fields omitted.
mkCopyWorkspaceImage ::
  -- | 'name'
  Types.WorkspaceImageName ->
  -- | 'sourceImageId'
  Types.WorkspaceImageId ->
  -- | 'sourceRegion'
  Types.SourceRegion ->
  CopyWorkspaceImage
mkCopyWorkspaceImage name sourceImageId sourceRegion =
  CopyWorkspaceImage'
    { name,
      sourceImageId,
      sourceRegion,
      description = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name of the image.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwiName :: Lens.Lens' CopyWorkspaceImage Types.WorkspaceImageName
cwiName = Lens.field @"name"
{-# DEPRECATED cwiName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier of the source image.
--
-- /Note:/ Consider using 'sourceImageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwiSourceImageId :: Lens.Lens' CopyWorkspaceImage Types.WorkspaceImageId
cwiSourceImageId = Lens.field @"sourceImageId"
{-# DEPRECATED cwiSourceImageId "Use generic-lens or generic-optics with 'sourceImageId' instead." #-}

-- | The identifier of the source Region.
--
-- /Note:/ Consider using 'sourceRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwiSourceRegion :: Lens.Lens' CopyWorkspaceImage Types.SourceRegion
cwiSourceRegion = Lens.field @"sourceRegion"
{-# DEPRECATED cwiSourceRegion "Use generic-lens or generic-optics with 'sourceRegion' instead." #-}

-- | A description of the image.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwiDescription :: Lens.Lens' CopyWorkspaceImage (Core.Maybe Types.Description)
cwiDescription = Lens.field @"description"
{-# DEPRECATED cwiDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The tags for the image.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwiTags :: Lens.Lens' CopyWorkspaceImage (Core.Maybe [Types.Tag])
cwiTags = Lens.field @"tags"
{-# DEPRECATED cwiTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CopyWorkspaceImage where
  toJSON CopyWorkspaceImage {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("SourceImageId" Core..= sourceImageId),
            Core.Just ("SourceRegion" Core..= sourceRegion),
            ("Description" Core..=) Core.<$> description,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CopyWorkspaceImage where
  type Rs CopyWorkspaceImage = CopyWorkspaceImageResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "WorkspacesService.CopyWorkspaceImage")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CopyWorkspaceImageResponse'
            Core.<$> (x Core..:? "ImageId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCopyWorkspaceImageResponse' smart constructor.
data CopyWorkspaceImageResponse = CopyWorkspaceImageResponse'
  { -- | The identifier of the image.
    imageId :: Core.Maybe Types.WorkspaceImageId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CopyWorkspaceImageResponse' value with any optional fields omitted.
mkCopyWorkspaceImageResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CopyWorkspaceImageResponse
mkCopyWorkspaceImageResponse responseStatus =
  CopyWorkspaceImageResponse'
    { imageId = Core.Nothing,
      responseStatus
    }

-- | The identifier of the image.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwirrsImageId :: Lens.Lens' CopyWorkspaceImageResponse (Core.Maybe Types.WorkspaceImageId)
cwirrsImageId = Lens.field @"imageId"
{-# DEPRECATED cwirrsImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwirrsResponseStatus :: Lens.Lens' CopyWorkspaceImageResponse Core.Int
cwirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cwirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
