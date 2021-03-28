{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ExportImage (..)
    , mkExportImage
    -- ** Request lenses
    , eiDiskImageFormat
    , eiImageId
    , eiS3ExportLocation
    , eiClientToken
    , eiDescription
    , eiDryRun
    , eiRoleName
    , eiTagSpecifications

    -- * Destructuring the response
    , ExportImageResponse (..)
    , mkExportImageResponse
    -- ** Response lenses
    , eirrsDescription
    , eirrsDiskImageFormat
    , eirrsExportImageTaskId
    , eirrsImageId
    , eirrsProgress
    , eirrsRoleName
    , eirrsS3ExportLocation
    , eirrsStatus
    , eirrsStatusMessage
    , eirrsTags
    , eirrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkExportImage' smart constructor.
data ExportImage = ExportImage'
  { diskImageFormat :: Types.DiskImageFormat
    -- ^ The disk image format.
  , imageId :: Types.ImageId
    -- ^ The ID of the image.
  , s3ExportLocation :: Types.ExportTaskS3LocationRequest
    -- ^ Information about the destination Amazon S3 bucket. The bucket must exist and grant WRITE and READ_ACP permissions to the AWS account vm-import-export@amazon.com.
  , clientToken :: Core.Maybe Core.Text
    -- ^ Token to enable idempotency for export image requests.
  , description :: Core.Maybe Core.Text
    -- ^ A description of the image being exported. The maximum length is 255 characters.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , roleName :: Core.Maybe Core.Text
    -- ^ The name of the role that grants VM Import/Export permission to export images to your Amazon S3 bucket. If this parameter is not specified, the default role is named 'vmimport'.
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to apply to the image being exported.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExportImage' value with any optional fields omitted.
mkExportImage
    :: Types.DiskImageFormat -- ^ 'diskImageFormat'
    -> Types.ImageId -- ^ 'imageId'
    -> Types.ExportTaskS3LocationRequest -- ^ 's3ExportLocation'
    -> ExportImage
mkExportImage diskImageFormat imageId s3ExportLocation
  = ExportImage'{diskImageFormat, imageId, s3ExportLocation,
                 clientToken = Core.Nothing, description = Core.Nothing,
                 dryRun = Core.Nothing, roleName = Core.Nothing,
                 tagSpecifications = Core.Nothing}

-- | The disk image format.
--
-- /Note:/ Consider using 'diskImageFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiDiskImageFormat :: Lens.Lens' ExportImage Types.DiskImageFormat
eiDiskImageFormat = Lens.field @"diskImageFormat"
{-# INLINEABLE eiDiskImageFormat #-}
{-# DEPRECATED diskImageFormat "Use generic-lens or generic-optics with 'diskImageFormat' instead"  #-}

-- | The ID of the image.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiImageId :: Lens.Lens' ExportImage Types.ImageId
eiImageId = Lens.field @"imageId"
{-# INLINEABLE eiImageId #-}
{-# DEPRECATED imageId "Use generic-lens or generic-optics with 'imageId' instead"  #-}

-- | Information about the destination Amazon S3 bucket. The bucket must exist and grant WRITE and READ_ACP permissions to the AWS account vm-import-export@amazon.com.
--
-- /Note:/ Consider using 's3ExportLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiS3ExportLocation :: Lens.Lens' ExportImage Types.ExportTaskS3LocationRequest
eiS3ExportLocation = Lens.field @"s3ExportLocation"
{-# INLINEABLE eiS3ExportLocation #-}
{-# DEPRECATED s3ExportLocation "Use generic-lens or generic-optics with 's3ExportLocation' instead"  #-}

-- | Token to enable idempotency for export image requests.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiClientToken :: Lens.Lens' ExportImage (Core.Maybe Core.Text)
eiClientToken = Lens.field @"clientToken"
{-# INLINEABLE eiClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | A description of the image being exported. The maximum length is 255 characters.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiDescription :: Lens.Lens' ExportImage (Core.Maybe Core.Text)
eiDescription = Lens.field @"description"
{-# INLINEABLE eiDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiDryRun :: Lens.Lens' ExportImage (Core.Maybe Core.Bool)
eiDryRun = Lens.field @"dryRun"
{-# INLINEABLE eiDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The name of the role that grants VM Import/Export permission to export images to your Amazon S3 bucket. If this parameter is not specified, the default role is named 'vmimport'.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiRoleName :: Lens.Lens' ExportImage (Core.Maybe Core.Text)
eiRoleName = Lens.field @"roleName"
{-# INLINEABLE eiRoleName #-}
{-# DEPRECATED roleName "Use generic-lens or generic-optics with 'roleName' instead"  #-}

-- | The tags to apply to the image being exported.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiTagSpecifications :: Lens.Lens' ExportImage (Core.Maybe [Types.TagSpecification])
eiTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE eiTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

instance Core.ToQuery ExportImage where
        toQuery ExportImage{..}
          = Core.toQueryPair "Action" ("ExportImage" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "DiskImageFormat" diskImageFormat
              Core.<> Core.toQueryPair "ImageId" imageId
              Core.<> Core.toQueryPair "S3ExportLocation" s3ExportLocation
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientToken") clientToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "RoleName") roleName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications

instance Core.ToHeaders ExportImage where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ExportImage where
        type Rs ExportImage = ExportImageResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ExportImageResponse' Core.<$>
                   (x Core..@? "description") Core.<*> x Core..@? "diskImageFormat"
                     Core.<*> x Core..@? "exportImageTaskId"
                     Core.<*> x Core..@? "imageId"
                     Core.<*> x Core..@? "progress"
                     Core.<*> x Core..@? "roleName"
                     Core.<*> x Core..@? "s3ExportLocation"
                     Core.<*> x Core..@? "status"
                     Core.<*> x Core..@? "statusMessage"
                     Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkExportImageResponse' smart constructor.
data ExportImageResponse = ExportImageResponse'
  { description :: Core.Maybe Core.Text
    -- ^ A description of the image being exported.
  , diskImageFormat :: Core.Maybe Types.DiskImageFormat
    -- ^ The disk image format for the exported image.
  , exportImageTaskId :: Core.Maybe Core.Text
    -- ^ The ID of the export image task.
  , imageId :: Core.Maybe Core.Text
    -- ^ The ID of the image.
  , progress :: Core.Maybe Core.Text
    -- ^ The percent complete of the export image task.
  , roleName :: Core.Maybe Core.Text
    -- ^ The name of the role that grants VM Import/Export permission to export images to your Amazon S3 bucket.
  , s3ExportLocation :: Core.Maybe Types.ExportTaskS3Location
    -- ^ Information about the destination Amazon S3 bucket.
  , status :: Core.Maybe Core.Text
    -- ^ The status of the export image task. The possible values are @active@ , @completed@ , @deleting@ , and @deleted@ .
  , statusMessage :: Core.Maybe Core.Text
    -- ^ The status message for the export image task.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Any tags assigned to the image being exported.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExportImageResponse' value with any optional fields omitted.
mkExportImageResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ExportImageResponse
mkExportImageResponse responseStatus
  = ExportImageResponse'{description = Core.Nothing,
                         diskImageFormat = Core.Nothing, exportImageTaskId = Core.Nothing,
                         imageId = Core.Nothing, progress = Core.Nothing,
                         roleName = Core.Nothing, s3ExportLocation = Core.Nothing,
                         status = Core.Nothing, statusMessage = Core.Nothing,
                         tags = Core.Nothing, responseStatus}

-- | A description of the image being exported.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirrsDescription :: Lens.Lens' ExportImageResponse (Core.Maybe Core.Text)
eirrsDescription = Lens.field @"description"
{-# INLINEABLE eirrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The disk image format for the exported image.
--
-- /Note:/ Consider using 'diskImageFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirrsDiskImageFormat :: Lens.Lens' ExportImageResponse (Core.Maybe Types.DiskImageFormat)
eirrsDiskImageFormat = Lens.field @"diskImageFormat"
{-# INLINEABLE eirrsDiskImageFormat #-}
{-# DEPRECATED diskImageFormat "Use generic-lens or generic-optics with 'diskImageFormat' instead"  #-}

-- | The ID of the export image task.
--
-- /Note:/ Consider using 'exportImageTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirrsExportImageTaskId :: Lens.Lens' ExportImageResponse (Core.Maybe Core.Text)
eirrsExportImageTaskId = Lens.field @"exportImageTaskId"
{-# INLINEABLE eirrsExportImageTaskId #-}
{-# DEPRECATED exportImageTaskId "Use generic-lens or generic-optics with 'exportImageTaskId' instead"  #-}

-- | The ID of the image.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirrsImageId :: Lens.Lens' ExportImageResponse (Core.Maybe Core.Text)
eirrsImageId = Lens.field @"imageId"
{-# INLINEABLE eirrsImageId #-}
{-# DEPRECATED imageId "Use generic-lens or generic-optics with 'imageId' instead"  #-}

-- | The percent complete of the export image task.
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirrsProgress :: Lens.Lens' ExportImageResponse (Core.Maybe Core.Text)
eirrsProgress = Lens.field @"progress"
{-# INLINEABLE eirrsProgress #-}
{-# DEPRECATED progress "Use generic-lens or generic-optics with 'progress' instead"  #-}

-- | The name of the role that grants VM Import/Export permission to export images to your Amazon S3 bucket.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirrsRoleName :: Lens.Lens' ExportImageResponse (Core.Maybe Core.Text)
eirrsRoleName = Lens.field @"roleName"
{-# INLINEABLE eirrsRoleName #-}
{-# DEPRECATED roleName "Use generic-lens or generic-optics with 'roleName' instead"  #-}

-- | Information about the destination Amazon S3 bucket.
--
-- /Note:/ Consider using 's3ExportLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirrsS3ExportLocation :: Lens.Lens' ExportImageResponse (Core.Maybe Types.ExportTaskS3Location)
eirrsS3ExportLocation = Lens.field @"s3ExportLocation"
{-# INLINEABLE eirrsS3ExportLocation #-}
{-# DEPRECATED s3ExportLocation "Use generic-lens or generic-optics with 's3ExportLocation' instead"  #-}

-- | The status of the export image task. The possible values are @active@ , @completed@ , @deleting@ , and @deleted@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirrsStatus :: Lens.Lens' ExportImageResponse (Core.Maybe Core.Text)
eirrsStatus = Lens.field @"status"
{-# INLINEABLE eirrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The status message for the export image task.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirrsStatusMessage :: Lens.Lens' ExportImageResponse (Core.Maybe Core.Text)
eirrsStatusMessage = Lens.field @"statusMessage"
{-# INLINEABLE eirrsStatusMessage #-}
{-# DEPRECATED statusMessage "Use generic-lens or generic-optics with 'statusMessage' instead"  #-}

-- | Any tags assigned to the image being exported.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirrsTags :: Lens.Lens' ExportImageResponse (Core.Maybe [Types.Tag])
eirrsTags = Lens.field @"tags"
{-# INLINEABLE eirrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirrsResponseStatus :: Lens.Lens' ExportImageResponse Core.Int
eirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE eirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
