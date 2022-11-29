{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.ExportImage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports an Amazon Machine Image (AMI) to a VM file. For more
-- information, see
-- <https://docs.aws.amazon.com/vm-import/latest/userguide/vmexport_image.html Exporting a VM directly from an Amazon Machine Image (AMI)>
-- in the /VM Import\/Export User Guide/.
module Amazonka.EC2.ExportImage
  ( -- * Creating a Request
    ExportImage (..),
    newExportImage,

    -- * Request Lenses
    exportImage_roleName,
    exportImage_clientToken,
    exportImage_description,
    exportImage_dryRun,
    exportImage_tagSpecifications,
    exportImage_diskImageFormat,
    exportImage_imageId,
    exportImage_s3ExportLocation,

    -- * Destructuring the Response
    ExportImageResponse (..),
    newExportImageResponse,

    -- * Response Lenses
    exportImageResponse_tags,
    exportImageResponse_progress,
    exportImageResponse_roleName,
    exportImageResponse_exportImageTaskId,
    exportImageResponse_s3ExportLocation,
    exportImageResponse_diskImageFormat,
    exportImageResponse_status,
    exportImageResponse_description,
    exportImageResponse_statusMessage,
    exportImageResponse_imageId,
    exportImageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newExportImage' smart constructor.
data ExportImage = ExportImage'
  { -- | The name of the role that grants VM Import\/Export permission to export
    -- images to your Amazon S3 bucket. If this parameter is not specified, the
    -- default role is named \'vmimport\'.
    roleName :: Prelude.Maybe Prelude.Text,
    -- | Token to enable idempotency for export image requests.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description of the image being exported. The maximum length is 255
    -- characters.
    description :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The tags to apply to the export image task during creation.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The disk image format.
    diskImageFormat :: DiskImageFormat,
    -- | The ID of the image.
    imageId :: Prelude.Text,
    -- | The Amazon S3 bucket for the destination image. The destination bucket
    -- must exist.
    s3ExportLocation :: ExportTaskS3LocationRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleName', 'exportImage_roleName' - The name of the role that grants VM Import\/Export permission to export
-- images to your Amazon S3 bucket. If this parameter is not specified, the
-- default role is named \'vmimport\'.
--
-- 'clientToken', 'exportImage_clientToken' - Token to enable idempotency for export image requests.
--
-- 'description', 'exportImage_description' - A description of the image being exported. The maximum length is 255
-- characters.
--
-- 'dryRun', 'exportImage_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'tagSpecifications', 'exportImage_tagSpecifications' - The tags to apply to the export image task during creation.
--
-- 'diskImageFormat', 'exportImage_diskImageFormat' - The disk image format.
--
-- 'imageId', 'exportImage_imageId' - The ID of the image.
--
-- 's3ExportLocation', 'exportImage_s3ExportLocation' - The Amazon S3 bucket for the destination image. The destination bucket
-- must exist.
newExportImage ::
  -- | 'diskImageFormat'
  DiskImageFormat ->
  -- | 'imageId'
  Prelude.Text ->
  -- | 's3ExportLocation'
  ExportTaskS3LocationRequest ->
  ExportImage
newExportImage
  pDiskImageFormat_
  pImageId_
  pS3ExportLocation_ =
    ExportImage'
      { roleName = Prelude.Nothing,
        clientToken = Prelude.Nothing,
        description = Prelude.Nothing,
        dryRun = Prelude.Nothing,
        tagSpecifications = Prelude.Nothing,
        diskImageFormat = pDiskImageFormat_,
        imageId = pImageId_,
        s3ExportLocation = pS3ExportLocation_
      }

-- | The name of the role that grants VM Import\/Export permission to export
-- images to your Amazon S3 bucket. If this parameter is not specified, the
-- default role is named \'vmimport\'.
exportImage_roleName :: Lens.Lens' ExportImage (Prelude.Maybe Prelude.Text)
exportImage_roleName = Lens.lens (\ExportImage' {roleName} -> roleName) (\s@ExportImage' {} a -> s {roleName = a} :: ExportImage)

-- | Token to enable idempotency for export image requests.
exportImage_clientToken :: Lens.Lens' ExportImage (Prelude.Maybe Prelude.Text)
exportImage_clientToken = Lens.lens (\ExportImage' {clientToken} -> clientToken) (\s@ExportImage' {} a -> s {clientToken = a} :: ExportImage)

-- | A description of the image being exported. The maximum length is 255
-- characters.
exportImage_description :: Lens.Lens' ExportImage (Prelude.Maybe Prelude.Text)
exportImage_description = Lens.lens (\ExportImage' {description} -> description) (\s@ExportImage' {} a -> s {description = a} :: ExportImage)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
exportImage_dryRun :: Lens.Lens' ExportImage (Prelude.Maybe Prelude.Bool)
exportImage_dryRun = Lens.lens (\ExportImage' {dryRun} -> dryRun) (\s@ExportImage' {} a -> s {dryRun = a} :: ExportImage)

-- | The tags to apply to the export image task during creation.
exportImage_tagSpecifications :: Lens.Lens' ExportImage (Prelude.Maybe [TagSpecification])
exportImage_tagSpecifications = Lens.lens (\ExportImage' {tagSpecifications} -> tagSpecifications) (\s@ExportImage' {} a -> s {tagSpecifications = a} :: ExportImage) Prelude.. Lens.mapping Lens.coerced

-- | The disk image format.
exportImage_diskImageFormat :: Lens.Lens' ExportImage DiskImageFormat
exportImage_diskImageFormat = Lens.lens (\ExportImage' {diskImageFormat} -> diskImageFormat) (\s@ExportImage' {} a -> s {diskImageFormat = a} :: ExportImage)

-- | The ID of the image.
exportImage_imageId :: Lens.Lens' ExportImage Prelude.Text
exportImage_imageId = Lens.lens (\ExportImage' {imageId} -> imageId) (\s@ExportImage' {} a -> s {imageId = a} :: ExportImage)

-- | The Amazon S3 bucket for the destination image. The destination bucket
-- must exist.
exportImage_s3ExportLocation :: Lens.Lens' ExportImage ExportTaskS3LocationRequest
exportImage_s3ExportLocation = Lens.lens (\ExportImage' {s3ExportLocation} -> s3ExportLocation) (\s@ExportImage' {} a -> s {s3ExportLocation = a} :: ExportImage)

instance Core.AWSRequest ExportImage where
  type AWSResponse ExportImage = ExportImageResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ExportImageResponse'
            Prelude.<$> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (x Core..@? "progress")
            Prelude.<*> (x Core..@? "roleName")
            Prelude.<*> (x Core..@? "exportImageTaskId")
            Prelude.<*> (x Core..@? "s3ExportLocation")
            Prelude.<*> (x Core..@? "diskImageFormat")
            Prelude.<*> (x Core..@? "status")
            Prelude.<*> (x Core..@? "description")
            Prelude.<*> (x Core..@? "statusMessage")
            Prelude.<*> (x Core..@? "imageId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ExportImage where
  hashWithSalt _salt ExportImage' {..} =
    _salt `Prelude.hashWithSalt` roleName
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` diskImageFormat
      `Prelude.hashWithSalt` imageId
      `Prelude.hashWithSalt` s3ExportLocation

instance Prelude.NFData ExportImage where
  rnf ExportImage' {..} =
    Prelude.rnf roleName
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf diskImageFormat
      `Prelude.seq` Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf s3ExportLocation

instance Core.ToHeaders ExportImage where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ExportImage where
  toPath = Prelude.const "/"

instance Core.ToQuery ExportImage where
  toQuery ExportImage' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ExportImage" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "RoleName" Core.=: roleName,
        "ClientToken" Core.=: clientToken,
        "Description" Core.=: description,
        "DryRun" Core.=: dryRun,
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "DiskImageFormat" Core.=: diskImageFormat,
        "ImageId" Core.=: imageId,
        "S3ExportLocation" Core.=: s3ExportLocation
      ]

-- | /See:/ 'newExportImageResponse' smart constructor.
data ExportImageResponse = ExportImageResponse'
  { -- | Any tags assigned to the export image task.
    tags :: Prelude.Maybe [Tag],
    -- | The percent complete of the export image task.
    progress :: Prelude.Maybe Prelude.Text,
    -- | The name of the role that grants VM Import\/Export permission to export
    -- images to your Amazon S3 bucket.
    roleName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the export image task.
    exportImageTaskId :: Prelude.Maybe Prelude.Text,
    -- | Information about the destination Amazon S3 bucket.
    s3ExportLocation :: Prelude.Maybe ExportTaskS3Location,
    -- | The disk image format for the exported image.
    diskImageFormat :: Prelude.Maybe DiskImageFormat,
    -- | The status of the export image task. The possible values are @active@,
    -- @completed@, @deleting@, and @deleted@.
    status :: Prelude.Maybe Prelude.Text,
    -- | A description of the image being exported.
    description :: Prelude.Maybe Prelude.Text,
    -- | The status message for the export image task.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The ID of the image.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'exportImageResponse_tags' - Any tags assigned to the export image task.
--
-- 'progress', 'exportImageResponse_progress' - The percent complete of the export image task.
--
-- 'roleName', 'exportImageResponse_roleName' - The name of the role that grants VM Import\/Export permission to export
-- images to your Amazon S3 bucket.
--
-- 'exportImageTaskId', 'exportImageResponse_exportImageTaskId' - The ID of the export image task.
--
-- 's3ExportLocation', 'exportImageResponse_s3ExportLocation' - Information about the destination Amazon S3 bucket.
--
-- 'diskImageFormat', 'exportImageResponse_diskImageFormat' - The disk image format for the exported image.
--
-- 'status', 'exportImageResponse_status' - The status of the export image task. The possible values are @active@,
-- @completed@, @deleting@, and @deleted@.
--
-- 'description', 'exportImageResponse_description' - A description of the image being exported.
--
-- 'statusMessage', 'exportImageResponse_statusMessage' - The status message for the export image task.
--
-- 'imageId', 'exportImageResponse_imageId' - The ID of the image.
--
-- 'httpStatus', 'exportImageResponse_httpStatus' - The response's http status code.
newExportImageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ExportImageResponse
newExportImageResponse pHttpStatus_ =
  ExportImageResponse'
    { tags = Prelude.Nothing,
      progress = Prelude.Nothing,
      roleName = Prelude.Nothing,
      exportImageTaskId = Prelude.Nothing,
      s3ExportLocation = Prelude.Nothing,
      diskImageFormat = Prelude.Nothing,
      status = Prelude.Nothing,
      description = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      imageId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Any tags assigned to the export image task.
exportImageResponse_tags :: Lens.Lens' ExportImageResponse (Prelude.Maybe [Tag])
exportImageResponse_tags = Lens.lens (\ExportImageResponse' {tags} -> tags) (\s@ExportImageResponse' {} a -> s {tags = a} :: ExportImageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The percent complete of the export image task.
exportImageResponse_progress :: Lens.Lens' ExportImageResponse (Prelude.Maybe Prelude.Text)
exportImageResponse_progress = Lens.lens (\ExportImageResponse' {progress} -> progress) (\s@ExportImageResponse' {} a -> s {progress = a} :: ExportImageResponse)

-- | The name of the role that grants VM Import\/Export permission to export
-- images to your Amazon S3 bucket.
exportImageResponse_roleName :: Lens.Lens' ExportImageResponse (Prelude.Maybe Prelude.Text)
exportImageResponse_roleName = Lens.lens (\ExportImageResponse' {roleName} -> roleName) (\s@ExportImageResponse' {} a -> s {roleName = a} :: ExportImageResponse)

-- | The ID of the export image task.
exportImageResponse_exportImageTaskId :: Lens.Lens' ExportImageResponse (Prelude.Maybe Prelude.Text)
exportImageResponse_exportImageTaskId = Lens.lens (\ExportImageResponse' {exportImageTaskId} -> exportImageTaskId) (\s@ExportImageResponse' {} a -> s {exportImageTaskId = a} :: ExportImageResponse)

-- | Information about the destination Amazon S3 bucket.
exportImageResponse_s3ExportLocation :: Lens.Lens' ExportImageResponse (Prelude.Maybe ExportTaskS3Location)
exportImageResponse_s3ExportLocation = Lens.lens (\ExportImageResponse' {s3ExportLocation} -> s3ExportLocation) (\s@ExportImageResponse' {} a -> s {s3ExportLocation = a} :: ExportImageResponse)

-- | The disk image format for the exported image.
exportImageResponse_diskImageFormat :: Lens.Lens' ExportImageResponse (Prelude.Maybe DiskImageFormat)
exportImageResponse_diskImageFormat = Lens.lens (\ExportImageResponse' {diskImageFormat} -> diskImageFormat) (\s@ExportImageResponse' {} a -> s {diskImageFormat = a} :: ExportImageResponse)

-- | The status of the export image task. The possible values are @active@,
-- @completed@, @deleting@, and @deleted@.
exportImageResponse_status :: Lens.Lens' ExportImageResponse (Prelude.Maybe Prelude.Text)
exportImageResponse_status = Lens.lens (\ExportImageResponse' {status} -> status) (\s@ExportImageResponse' {} a -> s {status = a} :: ExportImageResponse)

-- | A description of the image being exported.
exportImageResponse_description :: Lens.Lens' ExportImageResponse (Prelude.Maybe Prelude.Text)
exportImageResponse_description = Lens.lens (\ExportImageResponse' {description} -> description) (\s@ExportImageResponse' {} a -> s {description = a} :: ExportImageResponse)

-- | The status message for the export image task.
exportImageResponse_statusMessage :: Lens.Lens' ExportImageResponse (Prelude.Maybe Prelude.Text)
exportImageResponse_statusMessage = Lens.lens (\ExportImageResponse' {statusMessage} -> statusMessage) (\s@ExportImageResponse' {} a -> s {statusMessage = a} :: ExportImageResponse)

-- | The ID of the image.
exportImageResponse_imageId :: Lens.Lens' ExportImageResponse (Prelude.Maybe Prelude.Text)
exportImageResponse_imageId = Lens.lens (\ExportImageResponse' {imageId} -> imageId) (\s@ExportImageResponse' {} a -> s {imageId = a} :: ExportImageResponse)

-- | The response's http status code.
exportImageResponse_httpStatus :: Lens.Lens' ExportImageResponse Prelude.Int
exportImageResponse_httpStatus = Lens.lens (\ExportImageResponse' {httpStatus} -> httpStatus) (\s@ExportImageResponse' {} a -> s {httpStatus = a} :: ExportImageResponse)

instance Prelude.NFData ExportImageResponse where
  rnf ExportImageResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf progress
      `Prelude.seq` Prelude.rnf roleName
      `Prelude.seq` Prelude.rnf exportImageTaskId
      `Prelude.seq` Prelude.rnf s3ExportLocation
      `Prelude.seq` Prelude.rnf diskImageFormat
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf httpStatus
