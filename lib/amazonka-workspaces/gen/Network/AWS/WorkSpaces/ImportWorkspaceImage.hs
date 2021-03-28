{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ImportWorkspaceImage (..)
    , mkImportWorkspaceImage
    -- ** Request lenses
    , iwiEc2ImageId
    , iwiIngestionProcess
    , iwiImageName
    , iwiImageDescription
    , iwiApplications
    , iwiTags

    -- * Destructuring the response
    , ImportWorkspaceImageResponse (..)
    , mkImportWorkspaceImageResponse
    -- ** Response lenses
    , iwirrsImageId
    , iwirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkImportWorkspaceImage' smart constructor.
data ImportWorkspaceImage = ImportWorkspaceImage'
  { ec2ImageId :: Types.Ec2ImageId
    -- ^ The identifier of the EC2 image.
  , ingestionProcess :: Types.WorkspaceImageIngestionProcess
    -- ^ The ingestion process to be used when importing the image. For non-GPU-enabled bundles (bundles other than Graphics or GraphicsPro), specify @BYOL_REGULAR@ .
  , imageName :: Types.WorkspaceImageName
    -- ^ The name of the WorkSpace image.
  , imageDescription :: Types.WorkspaceImageDescription
    -- ^ The description of the WorkSpace image.
  , applications :: Core.Maybe (Core.NonEmpty Types.Application)
    -- ^ If specified, the version of Microsoft Office to subscribe to. Valid only for Windows 10 BYOL images. For more information about subscribing to Office for BYOL images, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Licenses> .
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags. Each WorkSpaces resource can have a maximum of 50 tags.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportWorkspaceImage' value with any optional fields omitted.
mkImportWorkspaceImage
    :: Types.Ec2ImageId -- ^ 'ec2ImageId'
    -> Types.WorkspaceImageIngestionProcess -- ^ 'ingestionProcess'
    -> Types.WorkspaceImageName -- ^ 'imageName'
    -> Types.WorkspaceImageDescription -- ^ 'imageDescription'
    -> ImportWorkspaceImage
mkImportWorkspaceImage ec2ImageId ingestionProcess imageName
  imageDescription
  = ImportWorkspaceImage'{ec2ImageId, ingestionProcess, imageName,
                          imageDescription, applications = Core.Nothing, tags = Core.Nothing}

-- | The identifier of the EC2 image.
--
-- /Note:/ Consider using 'ec2ImageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iwiEc2ImageId :: Lens.Lens' ImportWorkspaceImage Types.Ec2ImageId
iwiEc2ImageId = Lens.field @"ec2ImageId"
{-# INLINEABLE iwiEc2ImageId #-}
{-# DEPRECATED ec2ImageId "Use generic-lens or generic-optics with 'ec2ImageId' instead"  #-}

-- | The ingestion process to be used when importing the image. For non-GPU-enabled bundles (bundles other than Graphics or GraphicsPro), specify @BYOL_REGULAR@ .
--
-- /Note:/ Consider using 'ingestionProcess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iwiIngestionProcess :: Lens.Lens' ImportWorkspaceImage Types.WorkspaceImageIngestionProcess
iwiIngestionProcess = Lens.field @"ingestionProcess"
{-# INLINEABLE iwiIngestionProcess #-}
{-# DEPRECATED ingestionProcess "Use generic-lens or generic-optics with 'ingestionProcess' instead"  #-}

-- | The name of the WorkSpace image.
--
-- /Note:/ Consider using 'imageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iwiImageName :: Lens.Lens' ImportWorkspaceImage Types.WorkspaceImageName
iwiImageName = Lens.field @"imageName"
{-# INLINEABLE iwiImageName #-}
{-# DEPRECATED imageName "Use generic-lens or generic-optics with 'imageName' instead"  #-}

-- | The description of the WorkSpace image.
--
-- /Note:/ Consider using 'imageDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iwiImageDescription :: Lens.Lens' ImportWorkspaceImage Types.WorkspaceImageDescription
iwiImageDescription = Lens.field @"imageDescription"
{-# INLINEABLE iwiImageDescription #-}
{-# DEPRECATED imageDescription "Use generic-lens or generic-optics with 'imageDescription' instead"  #-}

-- | If specified, the version of Microsoft Office to subscribe to. Valid only for Windows 10 BYOL images. For more information about subscribing to Office for BYOL images, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Licenses> .
--
-- /Note:/ Consider using 'applications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iwiApplications :: Lens.Lens' ImportWorkspaceImage (Core.Maybe (Core.NonEmpty Types.Application))
iwiApplications = Lens.field @"applications"
{-# INLINEABLE iwiApplications #-}
{-# DEPRECATED applications "Use generic-lens or generic-optics with 'applications' instead"  #-}

-- | The tags. Each WorkSpaces resource can have a maximum of 50 tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iwiTags :: Lens.Lens' ImportWorkspaceImage (Core.Maybe [Types.Tag])
iwiTags = Lens.field @"tags"
{-# INLINEABLE iwiTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery ImportWorkspaceImage where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ImportWorkspaceImage where
        toHeaders ImportWorkspaceImage{..}
          = Core.pure
              ("X-Amz-Target", "WorkspacesService.ImportWorkspaceImage")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ImportWorkspaceImage where
        toJSON ImportWorkspaceImage{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Ec2ImageId" Core..= ec2ImageId),
                  Core.Just ("IngestionProcess" Core..= ingestionProcess),
                  Core.Just ("ImageName" Core..= imageName),
                  Core.Just ("ImageDescription" Core..= imageDescription),
                  ("Applications" Core..=) Core.<$> applications,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest ImportWorkspaceImage where
        type Rs ImportWorkspaceImage = ImportWorkspaceImageResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ImportWorkspaceImageResponse' Core.<$>
                   (x Core..:? "ImageId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkImportWorkspaceImageResponse' smart constructor.
data ImportWorkspaceImageResponse = ImportWorkspaceImageResponse'
  { imageId :: Core.Maybe Types.WorkspaceImageId
    -- ^ The identifier of the WorkSpace image.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportWorkspaceImageResponse' value with any optional fields omitted.
mkImportWorkspaceImageResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ImportWorkspaceImageResponse
mkImportWorkspaceImageResponse responseStatus
  = ImportWorkspaceImageResponse'{imageId = Core.Nothing,
                                  responseStatus}

-- | The identifier of the WorkSpace image.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iwirrsImageId :: Lens.Lens' ImportWorkspaceImageResponse (Core.Maybe Types.WorkspaceImageId)
iwirrsImageId = Lens.field @"imageId"
{-# INLINEABLE iwirrsImageId #-}
{-# DEPRECATED imageId "Use generic-lens or generic-optics with 'imageId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iwirrsResponseStatus :: Lens.Lens' ImportWorkspaceImageResponse Core.Int
iwirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE iwirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
