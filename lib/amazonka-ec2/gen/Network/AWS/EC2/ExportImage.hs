{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ExportImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports an Amazon Machine Image (AMI) to a VM file. For more information, see <https://docs.aws.amazon.com/vm-import/latest/userguide/vmexport_image.html Exporting a VM Directory from an Amazon Machine Image (AMI)> in the /VM Import\/Export User Guide/ .
module Network.AWS.EC2.ExportImage
  ( -- * Creating a request
    ExportImage (..),
    mkExportImage,

    -- ** Request lenses
    eiDiskImageFormat,
    eiImageId,
    eiS3ExportLocation,
    eiClientToken,
    eiDescription,
    eiDryRun,
    eiRoleName,
    eiTagSpecifications,

    -- * Destructuring the response
    ExportImageResponse (..),
    mkExportImageResponse,

    -- ** Response lenses
    eirrsDescription,
    eirrsDiskImageFormat,
    eirrsExportImageTaskId,
    eirrsImageId,
    eirrsProgress,
    eirrsRoleName,
    eirrsS3ExportLocation,
    eirrsStatus,
    eirrsStatusMessage,
    eirrsTags,
    eirrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkExportImage' smart constructor.
data ExportImage = ExportImage'
  { -- | The disk image format.
    diskImageFormat :: Types.DiskImageFormat,
    -- | The ID of the image.
    imageId :: Types.ImageId,
    -- | Information about the destination Amazon S3 bucket. The bucket must exist and grant WRITE and READ_ACP permissions to the AWS account vm-import-export@amazon.com.
    s3ExportLocation :: Types.ExportTaskS3LocationRequest,
    -- | Token to enable idempotency for export image requests.
    clientToken :: Core.Maybe Types.ClientToken,
    -- | A description of the image being exported. The maximum length is 255 characters.
    description :: Core.Maybe Types.Description,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The name of the role that grants VM Import/Export permission to export images to your Amazon S3 bucket. If this parameter is not specified, the default role is named 'vmimport'.
    roleName :: Core.Maybe Types.RoleName,
    -- | The tags to apply to the image being exported.
    tagSpecifications :: Core.Maybe [Types.TagSpecification]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExportImage' value with any optional fields omitted.
mkExportImage ::
  -- | 'diskImageFormat'
  Types.DiskImageFormat ->
  -- | 'imageId'
  Types.ImageId ->
  -- | 's3ExportLocation'
  Types.ExportTaskS3LocationRequest ->
  ExportImage
mkExportImage diskImageFormat imageId s3ExportLocation =
  ExportImage'
    { diskImageFormat,
      imageId,
      s3ExportLocation,
      clientToken = Core.Nothing,
      description = Core.Nothing,
      dryRun = Core.Nothing,
      roleName = Core.Nothing,
      tagSpecifications = Core.Nothing
    }

-- | The disk image format.
--
-- /Note:/ Consider using 'diskImageFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiDiskImageFormat :: Lens.Lens' ExportImage Types.DiskImageFormat
eiDiskImageFormat = Lens.field @"diskImageFormat"
{-# DEPRECATED eiDiskImageFormat "Use generic-lens or generic-optics with 'diskImageFormat' instead." #-}

-- | The ID of the image.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiImageId :: Lens.Lens' ExportImage Types.ImageId
eiImageId = Lens.field @"imageId"
{-# DEPRECATED eiImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | Information about the destination Amazon S3 bucket. The bucket must exist and grant WRITE and READ_ACP permissions to the AWS account vm-import-export@amazon.com.
--
-- /Note:/ Consider using 's3ExportLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiS3ExportLocation :: Lens.Lens' ExportImage Types.ExportTaskS3LocationRequest
eiS3ExportLocation = Lens.field @"s3ExportLocation"
{-# DEPRECATED eiS3ExportLocation "Use generic-lens or generic-optics with 's3ExportLocation' instead." #-}

-- | Token to enable idempotency for export image requests.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiClientToken :: Lens.Lens' ExportImage (Core.Maybe Types.ClientToken)
eiClientToken = Lens.field @"clientToken"
{-# DEPRECATED eiClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | A description of the image being exported. The maximum length is 255 characters.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiDescription :: Lens.Lens' ExportImage (Core.Maybe Types.Description)
eiDescription = Lens.field @"description"
{-# DEPRECATED eiDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiDryRun :: Lens.Lens' ExportImage (Core.Maybe Core.Bool)
eiDryRun = Lens.field @"dryRun"
{-# DEPRECATED eiDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The name of the role that grants VM Import/Export permission to export images to your Amazon S3 bucket. If this parameter is not specified, the default role is named 'vmimport'.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiRoleName :: Lens.Lens' ExportImage (Core.Maybe Types.RoleName)
eiRoleName = Lens.field @"roleName"
{-# DEPRECATED eiRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | The tags to apply to the image being exported.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiTagSpecifications :: Lens.Lens' ExportImage (Core.Maybe [Types.TagSpecification])
eiTagSpecifications = Lens.field @"tagSpecifications"
{-# DEPRECATED eiTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

instance Core.AWSRequest ExportImage where
  type Rs ExportImage = ExportImageResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ExportImage")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DiskImageFormat" diskImageFormat)
                Core.<> (Core.toQueryValue "ImageId" imageId)
                Core.<> (Core.toQueryValue "S3ExportLocation" s3ExportLocation)
                Core.<> (Core.toQueryValue "ClientToken" Core.<$> clientToken)
                Core.<> (Core.toQueryValue "Description" Core.<$> description)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "RoleName" Core.<$> roleName)
                Core.<> (Core.toQueryList "TagSpecification" Core.<$> tagSpecifications)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ExportImageResponse'
            Core.<$> (x Core..@? "description")
            Core.<*> (x Core..@? "diskImageFormat")
            Core.<*> (x Core..@? "exportImageTaskId")
            Core.<*> (x Core..@? "imageId")
            Core.<*> (x Core..@? "progress")
            Core.<*> (x Core..@? "roleName")
            Core.<*> (x Core..@? "s3ExportLocation")
            Core.<*> (x Core..@? "status")
            Core.<*> (x Core..@? "statusMessage")
            Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkExportImageResponse' smart constructor.
data ExportImageResponse = ExportImageResponse'
  { -- | A description of the image being exported.
    description :: Core.Maybe Types.Description,
    -- | The disk image format for the exported image.
    diskImageFormat :: Core.Maybe Types.DiskImageFormat,
    -- | The ID of the export image task.
    exportImageTaskId :: Core.Maybe Types.ExportImageTaskId,
    -- | The ID of the image.
    imageId :: Core.Maybe Types.ImageId,
    -- | The percent complete of the export image task.
    progress :: Core.Maybe Types.Progress,
    -- | The name of the role that grants VM Import/Export permission to export images to your Amazon S3 bucket.
    roleName :: Core.Maybe Types.RoleName,
    -- | Information about the destination Amazon S3 bucket.
    s3ExportLocation :: Core.Maybe Types.ExportTaskS3Location,
    -- | The status of the export image task. The possible values are @active@ , @completed@ , @deleting@ , and @deleted@ .
    status :: Core.Maybe Types.String,
    -- | The status message for the export image task.
    statusMessage :: Core.Maybe Types.StatusMessage,
    -- | Any tags assigned to the image being exported.
    tags :: Core.Maybe [Types.Tag],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExportImageResponse' value with any optional fields omitted.
mkExportImageResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ExportImageResponse
mkExportImageResponse responseStatus =
  ExportImageResponse'
    { description = Core.Nothing,
      diskImageFormat = Core.Nothing,
      exportImageTaskId = Core.Nothing,
      imageId = Core.Nothing,
      progress = Core.Nothing,
      roleName = Core.Nothing,
      s3ExportLocation = Core.Nothing,
      status = Core.Nothing,
      statusMessage = Core.Nothing,
      tags = Core.Nothing,
      responseStatus
    }

-- | A description of the image being exported.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirrsDescription :: Lens.Lens' ExportImageResponse (Core.Maybe Types.Description)
eirrsDescription = Lens.field @"description"
{-# DEPRECATED eirrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The disk image format for the exported image.
--
-- /Note:/ Consider using 'diskImageFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirrsDiskImageFormat :: Lens.Lens' ExportImageResponse (Core.Maybe Types.DiskImageFormat)
eirrsDiskImageFormat = Lens.field @"diskImageFormat"
{-# DEPRECATED eirrsDiskImageFormat "Use generic-lens or generic-optics with 'diskImageFormat' instead." #-}

-- | The ID of the export image task.
--
-- /Note:/ Consider using 'exportImageTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirrsExportImageTaskId :: Lens.Lens' ExportImageResponse (Core.Maybe Types.ExportImageTaskId)
eirrsExportImageTaskId = Lens.field @"exportImageTaskId"
{-# DEPRECATED eirrsExportImageTaskId "Use generic-lens or generic-optics with 'exportImageTaskId' instead." #-}

-- | The ID of the image.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirrsImageId :: Lens.Lens' ExportImageResponse (Core.Maybe Types.ImageId)
eirrsImageId = Lens.field @"imageId"
{-# DEPRECATED eirrsImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The percent complete of the export image task.
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirrsProgress :: Lens.Lens' ExportImageResponse (Core.Maybe Types.Progress)
eirrsProgress = Lens.field @"progress"
{-# DEPRECATED eirrsProgress "Use generic-lens or generic-optics with 'progress' instead." #-}

-- | The name of the role that grants VM Import/Export permission to export images to your Amazon S3 bucket.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirrsRoleName :: Lens.Lens' ExportImageResponse (Core.Maybe Types.RoleName)
eirrsRoleName = Lens.field @"roleName"
{-# DEPRECATED eirrsRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | Information about the destination Amazon S3 bucket.
--
-- /Note:/ Consider using 's3ExportLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirrsS3ExportLocation :: Lens.Lens' ExportImageResponse (Core.Maybe Types.ExportTaskS3Location)
eirrsS3ExportLocation = Lens.field @"s3ExportLocation"
{-# DEPRECATED eirrsS3ExportLocation "Use generic-lens or generic-optics with 's3ExportLocation' instead." #-}

-- | The status of the export image task. The possible values are @active@ , @completed@ , @deleting@ , and @deleted@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirrsStatus :: Lens.Lens' ExportImageResponse (Core.Maybe Types.String)
eirrsStatus = Lens.field @"status"
{-# DEPRECATED eirrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The status message for the export image task.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirrsStatusMessage :: Lens.Lens' ExportImageResponse (Core.Maybe Types.StatusMessage)
eirrsStatusMessage = Lens.field @"statusMessage"
{-# DEPRECATED eirrsStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | Any tags assigned to the image being exported.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirrsTags :: Lens.Lens' ExportImageResponse (Core.Maybe [Types.Tag])
eirrsTags = Lens.field @"tags"
{-# DEPRECATED eirrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirrsResponseStatus :: Lens.Lens' ExportImageResponse Core.Int
eirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED eirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
