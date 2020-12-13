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
    eiClientToken,
    eiRoleName,
    eiTagSpecifications,
    eiImageId,
    eiDescription,
    eiDryRun,
    eiS3ExportLocation,
    eiDiskImageFormat,

    -- * Destructuring the response
    ExportImageResponse (..),
    mkExportImageResponse,

    -- ** Response lenses
    eirsStatus,
    eirsProgress,
    eirsExportImageTaskId,
    eirsRoleName,
    eirsStatusMessage,
    eirsImageId,
    eirsDescription,
    eirsTags,
    eirsS3ExportLocation,
    eirsDiskImageFormat,
    eirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkExportImage' smart constructor.
data ExportImage = ExportImage'
  { -- | Token to enable idempotency for export image requests.
    clientToken :: Lude.Maybe Lude.Text,
    -- | The name of the role that grants VM Import/Export permission to export images to your Amazon S3 bucket. If this parameter is not specified, the default role is named 'vmimport'.
    roleName :: Lude.Maybe Lude.Text,
    -- | The tags to apply to the image being exported.
    tagSpecifications :: Lude.Maybe [TagSpecification],
    -- | The ID of the image.
    imageId :: Lude.Text,
    -- | A description of the image being exported. The maximum length is 255 characters.
    description :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | Information about the destination Amazon S3 bucket. The bucket must exist and grant WRITE and READ_ACP permissions to the AWS account vm-import-export@amazon.com.
    s3ExportLocation :: ExportTaskS3LocationRequest,
    -- | The disk image format.
    diskImageFormat :: DiskImageFormat
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportImage' with the minimum fields required to make a request.
--
-- * 'clientToken' - Token to enable idempotency for export image requests.
-- * 'roleName' - The name of the role that grants VM Import/Export permission to export images to your Amazon S3 bucket. If this parameter is not specified, the default role is named 'vmimport'.
-- * 'tagSpecifications' - The tags to apply to the image being exported.
-- * 'imageId' - The ID of the image.
-- * 'description' - A description of the image being exported. The maximum length is 255 characters.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 's3ExportLocation' - Information about the destination Amazon S3 bucket. The bucket must exist and grant WRITE and READ_ACP permissions to the AWS account vm-import-export@amazon.com.
-- * 'diskImageFormat' - The disk image format.
mkExportImage ::
  -- | 'imageId'
  Lude.Text ->
  -- | 's3ExportLocation'
  ExportTaskS3LocationRequest ->
  -- | 'diskImageFormat'
  DiskImageFormat ->
  ExportImage
mkExportImage pImageId_ pS3ExportLocation_ pDiskImageFormat_ =
  ExportImage'
    { clientToken = Lude.Nothing,
      roleName = Lude.Nothing,
      tagSpecifications = Lude.Nothing,
      imageId = pImageId_,
      description = Lude.Nothing,
      dryRun = Lude.Nothing,
      s3ExportLocation = pS3ExportLocation_,
      diskImageFormat = pDiskImageFormat_
    }

-- | Token to enable idempotency for export image requests.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiClientToken :: Lens.Lens' ExportImage (Lude.Maybe Lude.Text)
eiClientToken = Lens.lens (clientToken :: ExportImage -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: ExportImage)
{-# DEPRECATED eiClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The name of the role that grants VM Import/Export permission to export images to your Amazon S3 bucket. If this parameter is not specified, the default role is named 'vmimport'.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiRoleName :: Lens.Lens' ExportImage (Lude.Maybe Lude.Text)
eiRoleName = Lens.lens (roleName :: ExportImage -> Lude.Maybe Lude.Text) (\s a -> s {roleName = a} :: ExportImage)
{-# DEPRECATED eiRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | The tags to apply to the image being exported.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiTagSpecifications :: Lens.Lens' ExportImage (Lude.Maybe [TagSpecification])
eiTagSpecifications = Lens.lens (tagSpecifications :: ExportImage -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: ExportImage)
{-# DEPRECATED eiTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | The ID of the image.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiImageId :: Lens.Lens' ExportImage Lude.Text
eiImageId = Lens.lens (imageId :: ExportImage -> Lude.Text) (\s a -> s {imageId = a} :: ExportImage)
{-# DEPRECATED eiImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | A description of the image being exported. The maximum length is 255 characters.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiDescription :: Lens.Lens' ExportImage (Lude.Maybe Lude.Text)
eiDescription = Lens.lens (description :: ExportImage -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ExportImage)
{-# DEPRECATED eiDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiDryRun :: Lens.Lens' ExportImage (Lude.Maybe Lude.Bool)
eiDryRun = Lens.lens (dryRun :: ExportImage -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ExportImage)
{-# DEPRECATED eiDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | Information about the destination Amazon S3 bucket. The bucket must exist and grant WRITE and READ_ACP permissions to the AWS account vm-import-export@amazon.com.
--
-- /Note:/ Consider using 's3ExportLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiS3ExportLocation :: Lens.Lens' ExportImage ExportTaskS3LocationRequest
eiS3ExportLocation = Lens.lens (s3ExportLocation :: ExportImage -> ExportTaskS3LocationRequest) (\s a -> s {s3ExportLocation = a} :: ExportImage)
{-# DEPRECATED eiS3ExportLocation "Use generic-lens or generic-optics with 's3ExportLocation' instead." #-}

-- | The disk image format.
--
-- /Note:/ Consider using 'diskImageFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiDiskImageFormat :: Lens.Lens' ExportImage DiskImageFormat
eiDiskImageFormat = Lens.lens (diskImageFormat :: ExportImage -> DiskImageFormat) (\s a -> s {diskImageFormat = a} :: ExportImage)
{-# DEPRECATED eiDiskImageFormat "Use generic-lens or generic-optics with 'diskImageFormat' instead." #-}

instance Lude.AWSRequest ExportImage where
  type Rs ExportImage = ExportImageResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ExportImageResponse'
            Lude.<$> (x Lude..@? "status")
            Lude.<*> (x Lude..@? "progress")
            Lude.<*> (x Lude..@? "exportImageTaskId")
            Lude.<*> (x Lude..@? "roleName")
            Lude.<*> (x Lude..@? "statusMessage")
            Lude.<*> (x Lude..@? "imageId")
            Lude.<*> (x Lude..@? "description")
            Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "s3ExportLocation")
            Lude.<*> (x Lude..@? "diskImageFormat")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ExportImage where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ExportImage where
  toPath = Lude.const "/"

instance Lude.ToQuery ExportImage where
  toQuery ExportImage' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ExportImage" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "ClientToken" Lude.=: clientToken,
        "RoleName" Lude.=: roleName,
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "ImageId" Lude.=: imageId,
        "Description" Lude.=: description,
        "DryRun" Lude.=: dryRun,
        "S3ExportLocation" Lude.=: s3ExportLocation,
        "DiskImageFormat" Lude.=: diskImageFormat
      ]

-- | /See:/ 'mkExportImageResponse' smart constructor.
data ExportImageResponse = ExportImageResponse'
  { -- | The status of the export image task. The possible values are @active@ , @completed@ , @deleting@ , and @deleted@ .
    status :: Lude.Maybe Lude.Text,
    -- | The percent complete of the export image task.
    progress :: Lude.Maybe Lude.Text,
    -- | The ID of the export image task.
    exportImageTaskId :: Lude.Maybe Lude.Text,
    -- | The name of the role that grants VM Import/Export permission to export images to your Amazon S3 bucket.
    roleName :: Lude.Maybe Lude.Text,
    -- | The status message for the export image task.
    statusMessage :: Lude.Maybe Lude.Text,
    -- | The ID of the image.
    imageId :: Lude.Maybe Lude.Text,
    -- | A description of the image being exported.
    description :: Lude.Maybe Lude.Text,
    -- | Any tags assigned to the image being exported.
    tags :: Lude.Maybe [Tag],
    -- | Information about the destination Amazon S3 bucket.
    s3ExportLocation :: Lude.Maybe ExportTaskS3Location,
    -- | The disk image format for the exported image.
    diskImageFormat :: Lude.Maybe DiskImageFormat,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportImageResponse' with the minimum fields required to make a request.
--
-- * 'status' - The status of the export image task. The possible values are @active@ , @completed@ , @deleting@ , and @deleted@ .
-- * 'progress' - The percent complete of the export image task.
-- * 'exportImageTaskId' - The ID of the export image task.
-- * 'roleName' - The name of the role that grants VM Import/Export permission to export images to your Amazon S3 bucket.
-- * 'statusMessage' - The status message for the export image task.
-- * 'imageId' - The ID of the image.
-- * 'description' - A description of the image being exported.
-- * 'tags' - Any tags assigned to the image being exported.
-- * 's3ExportLocation' - Information about the destination Amazon S3 bucket.
-- * 'diskImageFormat' - The disk image format for the exported image.
-- * 'responseStatus' - The response status code.
mkExportImageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ExportImageResponse
mkExportImageResponse pResponseStatus_ =
  ExportImageResponse'
    { status = Lude.Nothing,
      progress = Lude.Nothing,
      exportImageTaskId = Lude.Nothing,
      roleName = Lude.Nothing,
      statusMessage = Lude.Nothing,
      imageId = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      s3ExportLocation = Lude.Nothing,
      diskImageFormat = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the export image task. The possible values are @active@ , @completed@ , @deleting@ , and @deleted@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirsStatus :: Lens.Lens' ExportImageResponse (Lude.Maybe Lude.Text)
eirsStatus = Lens.lens (status :: ExportImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: ExportImageResponse)
{-# DEPRECATED eirsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The percent complete of the export image task.
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirsProgress :: Lens.Lens' ExportImageResponse (Lude.Maybe Lude.Text)
eirsProgress = Lens.lens (progress :: ExportImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {progress = a} :: ExportImageResponse)
{-# DEPRECATED eirsProgress "Use generic-lens or generic-optics with 'progress' instead." #-}

-- | The ID of the export image task.
--
-- /Note:/ Consider using 'exportImageTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirsExportImageTaskId :: Lens.Lens' ExportImageResponse (Lude.Maybe Lude.Text)
eirsExportImageTaskId = Lens.lens (exportImageTaskId :: ExportImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {exportImageTaskId = a} :: ExportImageResponse)
{-# DEPRECATED eirsExportImageTaskId "Use generic-lens or generic-optics with 'exportImageTaskId' instead." #-}

-- | The name of the role that grants VM Import/Export permission to export images to your Amazon S3 bucket.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirsRoleName :: Lens.Lens' ExportImageResponse (Lude.Maybe Lude.Text)
eirsRoleName = Lens.lens (roleName :: ExportImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {roleName = a} :: ExportImageResponse)
{-# DEPRECATED eirsRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | The status message for the export image task.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirsStatusMessage :: Lens.Lens' ExportImageResponse (Lude.Maybe Lude.Text)
eirsStatusMessage = Lens.lens (statusMessage :: ExportImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: ExportImageResponse)
{-# DEPRECATED eirsStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | The ID of the image.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirsImageId :: Lens.Lens' ExportImageResponse (Lude.Maybe Lude.Text)
eirsImageId = Lens.lens (imageId :: ExportImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {imageId = a} :: ExportImageResponse)
{-# DEPRECATED eirsImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | A description of the image being exported.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirsDescription :: Lens.Lens' ExportImageResponse (Lude.Maybe Lude.Text)
eirsDescription = Lens.lens (description :: ExportImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ExportImageResponse)
{-# DEPRECATED eirsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Any tags assigned to the image being exported.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirsTags :: Lens.Lens' ExportImageResponse (Lude.Maybe [Tag])
eirsTags = Lens.lens (tags :: ExportImageResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ExportImageResponse)
{-# DEPRECATED eirsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Information about the destination Amazon S3 bucket.
--
-- /Note:/ Consider using 's3ExportLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirsS3ExportLocation :: Lens.Lens' ExportImageResponse (Lude.Maybe ExportTaskS3Location)
eirsS3ExportLocation = Lens.lens (s3ExportLocation :: ExportImageResponse -> Lude.Maybe ExportTaskS3Location) (\s a -> s {s3ExportLocation = a} :: ExportImageResponse)
{-# DEPRECATED eirsS3ExportLocation "Use generic-lens or generic-optics with 's3ExportLocation' instead." #-}

-- | The disk image format for the exported image.
--
-- /Note:/ Consider using 'diskImageFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirsDiskImageFormat :: Lens.Lens' ExportImageResponse (Lude.Maybe DiskImageFormat)
eirsDiskImageFormat = Lens.lens (diskImageFormat :: ExportImageResponse -> Lude.Maybe DiskImageFormat) (\s a -> s {diskImageFormat = a} :: ExportImageResponse)
{-# DEPRECATED eirsDiskImageFormat "Use generic-lens or generic-optics with 'diskImageFormat' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirsResponseStatus :: Lens.Lens' ExportImageResponse Lude.Int
eirsResponseStatus = Lens.lens (responseStatus :: ExportImageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ExportImageResponse)
{-# DEPRECATED eirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
