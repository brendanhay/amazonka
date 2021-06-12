{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ExportImageTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ExportImageTask where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ExportTaskS3Location
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Describes an export image task.
--
-- /See:/ 'newExportImageTask' smart constructor.
data ExportImageTask = ExportImageTask'
  { -- | The status message for the export image task.
    statusMessage :: Core.Maybe Core.Text,
    -- | The status of the export image task. The possible values are @active@,
    -- @completed@, @deleting@, and @deleted@.
    status :: Core.Maybe Core.Text,
    -- | The ID of the image.
    imageId :: Core.Maybe Core.Text,
    -- | Any tags assigned to the export image task.
    tags :: Core.Maybe [Tag],
    -- | Information about the destination Amazon S3 bucket.
    s3ExportLocation :: Core.Maybe ExportTaskS3Location,
    -- | A description of the image being exported.
    description :: Core.Maybe Core.Text,
    -- | The ID of the export image task.
    exportImageTaskId :: Core.Maybe Core.Text,
    -- | The percent complete of the export image task.
    progress :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ExportImageTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'exportImageTask_statusMessage' - The status message for the export image task.
--
-- 'status', 'exportImageTask_status' - The status of the export image task. The possible values are @active@,
-- @completed@, @deleting@, and @deleted@.
--
-- 'imageId', 'exportImageTask_imageId' - The ID of the image.
--
-- 'tags', 'exportImageTask_tags' - Any tags assigned to the export image task.
--
-- 's3ExportLocation', 'exportImageTask_s3ExportLocation' - Information about the destination Amazon S3 bucket.
--
-- 'description', 'exportImageTask_description' - A description of the image being exported.
--
-- 'exportImageTaskId', 'exportImageTask_exportImageTaskId' - The ID of the export image task.
--
-- 'progress', 'exportImageTask_progress' - The percent complete of the export image task.
newExportImageTask ::
  ExportImageTask
newExportImageTask =
  ExportImageTask'
    { statusMessage = Core.Nothing,
      status = Core.Nothing,
      imageId = Core.Nothing,
      tags = Core.Nothing,
      s3ExportLocation = Core.Nothing,
      description = Core.Nothing,
      exportImageTaskId = Core.Nothing,
      progress = Core.Nothing
    }

-- | The status message for the export image task.
exportImageTask_statusMessage :: Lens.Lens' ExportImageTask (Core.Maybe Core.Text)
exportImageTask_statusMessage = Lens.lens (\ExportImageTask' {statusMessage} -> statusMessage) (\s@ExportImageTask' {} a -> s {statusMessage = a} :: ExportImageTask)

-- | The status of the export image task. The possible values are @active@,
-- @completed@, @deleting@, and @deleted@.
exportImageTask_status :: Lens.Lens' ExportImageTask (Core.Maybe Core.Text)
exportImageTask_status = Lens.lens (\ExportImageTask' {status} -> status) (\s@ExportImageTask' {} a -> s {status = a} :: ExportImageTask)

-- | The ID of the image.
exportImageTask_imageId :: Lens.Lens' ExportImageTask (Core.Maybe Core.Text)
exportImageTask_imageId = Lens.lens (\ExportImageTask' {imageId} -> imageId) (\s@ExportImageTask' {} a -> s {imageId = a} :: ExportImageTask)

-- | Any tags assigned to the export image task.
exportImageTask_tags :: Lens.Lens' ExportImageTask (Core.Maybe [Tag])
exportImageTask_tags = Lens.lens (\ExportImageTask' {tags} -> tags) (\s@ExportImageTask' {} a -> s {tags = a} :: ExportImageTask) Core.. Lens.mapping Lens._Coerce

-- | Information about the destination Amazon S3 bucket.
exportImageTask_s3ExportLocation :: Lens.Lens' ExportImageTask (Core.Maybe ExportTaskS3Location)
exportImageTask_s3ExportLocation = Lens.lens (\ExportImageTask' {s3ExportLocation} -> s3ExportLocation) (\s@ExportImageTask' {} a -> s {s3ExportLocation = a} :: ExportImageTask)

-- | A description of the image being exported.
exportImageTask_description :: Lens.Lens' ExportImageTask (Core.Maybe Core.Text)
exportImageTask_description = Lens.lens (\ExportImageTask' {description} -> description) (\s@ExportImageTask' {} a -> s {description = a} :: ExportImageTask)

-- | The ID of the export image task.
exportImageTask_exportImageTaskId :: Lens.Lens' ExportImageTask (Core.Maybe Core.Text)
exportImageTask_exportImageTaskId = Lens.lens (\ExportImageTask' {exportImageTaskId} -> exportImageTaskId) (\s@ExportImageTask' {} a -> s {exportImageTaskId = a} :: ExportImageTask)

-- | The percent complete of the export image task.
exportImageTask_progress :: Lens.Lens' ExportImageTask (Core.Maybe Core.Text)
exportImageTask_progress = Lens.lens (\ExportImageTask' {progress} -> progress) (\s@ExportImageTask' {} a -> s {progress = a} :: ExportImageTask)

instance Core.FromXML ExportImageTask where
  parseXML x =
    ExportImageTask'
      Core.<$> (x Core..@? "statusMessage")
      Core.<*> (x Core..@? "status")
      Core.<*> (x Core..@? "imageId")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "s3ExportLocation")
      Core.<*> (x Core..@? "description")
      Core.<*> (x Core..@? "exportImageTaskId")
      Core.<*> (x Core..@? "progress")

instance Core.Hashable ExportImageTask

instance Core.NFData ExportImageTask
