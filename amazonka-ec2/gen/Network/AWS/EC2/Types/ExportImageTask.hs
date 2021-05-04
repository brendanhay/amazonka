{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ExportTaskS3Location
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an export image task.
--
-- /See:/ 'newExportImageTask' smart constructor.
data ExportImageTask = ExportImageTask'
  { -- | The status message for the export image task.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The status of the export image task. The possible values are @active@,
    -- @completed@, @deleting@, and @deleted@.
    status :: Prelude.Maybe Prelude.Text,
    -- | The ID of the image.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | Any tags assigned to the export image task.
    tags :: Prelude.Maybe [Tag],
    -- | Information about the destination Amazon S3 bucket.
    s3ExportLocation :: Prelude.Maybe ExportTaskS3Location,
    -- | A description of the image being exported.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the export image task.
    exportImageTaskId :: Prelude.Maybe Prelude.Text,
    -- | The percent complete of the export image task.
    progress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { statusMessage = Prelude.Nothing,
      status = Prelude.Nothing,
      imageId = Prelude.Nothing,
      tags = Prelude.Nothing,
      s3ExportLocation = Prelude.Nothing,
      description = Prelude.Nothing,
      exportImageTaskId = Prelude.Nothing,
      progress = Prelude.Nothing
    }

-- | The status message for the export image task.
exportImageTask_statusMessage :: Lens.Lens' ExportImageTask (Prelude.Maybe Prelude.Text)
exportImageTask_statusMessage = Lens.lens (\ExportImageTask' {statusMessage} -> statusMessage) (\s@ExportImageTask' {} a -> s {statusMessage = a} :: ExportImageTask)

-- | The status of the export image task. The possible values are @active@,
-- @completed@, @deleting@, and @deleted@.
exportImageTask_status :: Lens.Lens' ExportImageTask (Prelude.Maybe Prelude.Text)
exportImageTask_status = Lens.lens (\ExportImageTask' {status} -> status) (\s@ExportImageTask' {} a -> s {status = a} :: ExportImageTask)

-- | The ID of the image.
exportImageTask_imageId :: Lens.Lens' ExportImageTask (Prelude.Maybe Prelude.Text)
exportImageTask_imageId = Lens.lens (\ExportImageTask' {imageId} -> imageId) (\s@ExportImageTask' {} a -> s {imageId = a} :: ExportImageTask)

-- | Any tags assigned to the export image task.
exportImageTask_tags :: Lens.Lens' ExportImageTask (Prelude.Maybe [Tag])
exportImageTask_tags = Lens.lens (\ExportImageTask' {tags} -> tags) (\s@ExportImageTask' {} a -> s {tags = a} :: ExportImageTask) Prelude.. Lens.mapping Prelude._Coerce

-- | Information about the destination Amazon S3 bucket.
exportImageTask_s3ExportLocation :: Lens.Lens' ExportImageTask (Prelude.Maybe ExportTaskS3Location)
exportImageTask_s3ExportLocation = Lens.lens (\ExportImageTask' {s3ExportLocation} -> s3ExportLocation) (\s@ExportImageTask' {} a -> s {s3ExportLocation = a} :: ExportImageTask)

-- | A description of the image being exported.
exportImageTask_description :: Lens.Lens' ExportImageTask (Prelude.Maybe Prelude.Text)
exportImageTask_description = Lens.lens (\ExportImageTask' {description} -> description) (\s@ExportImageTask' {} a -> s {description = a} :: ExportImageTask)

-- | The ID of the export image task.
exportImageTask_exportImageTaskId :: Lens.Lens' ExportImageTask (Prelude.Maybe Prelude.Text)
exportImageTask_exportImageTaskId = Lens.lens (\ExportImageTask' {exportImageTaskId} -> exportImageTaskId) (\s@ExportImageTask' {} a -> s {exportImageTaskId = a} :: ExportImageTask)

-- | The percent complete of the export image task.
exportImageTask_progress :: Lens.Lens' ExportImageTask (Prelude.Maybe Prelude.Text)
exportImageTask_progress = Lens.lens (\ExportImageTask' {progress} -> progress) (\s@ExportImageTask' {} a -> s {progress = a} :: ExportImageTask)

instance Prelude.FromXML ExportImageTask where
  parseXML x =
    ExportImageTask'
      Prelude.<$> (x Prelude..@? "statusMessage")
      Prelude.<*> (x Prelude..@? "status")
      Prelude.<*> (x Prelude..@? "imageId")
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "s3ExportLocation")
      Prelude.<*> (x Prelude..@? "description")
      Prelude.<*> (x Prelude..@? "exportImageTaskId")
      Prelude.<*> (x Prelude..@? "progress")

instance Prelude.Hashable ExportImageTask

instance Prelude.NFData ExportImageTask
