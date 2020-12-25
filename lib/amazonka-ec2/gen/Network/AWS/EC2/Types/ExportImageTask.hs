{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ExportImageTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ExportImageTask
  ( ExportImageTask (..),

    -- * Smart constructor
    mkExportImageTask,

    -- * Lenses
    eitDescription,
    eitExportImageTaskId,
    eitImageId,
    eitProgress,
    eitS3ExportLocation,
    eitStatus,
    eitStatusMessage,
    eitTags,
  )
where

import qualified Network.AWS.EC2.Types.ExportTaskS3Location as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an export image task.
--
-- /See:/ 'mkExportImageTask' smart constructor.
data ExportImageTask = ExportImageTask'
  { -- | A description of the image being exported.
    description :: Core.Maybe Types.String,
    -- | The ID of the export image task.
    exportImageTaskId :: Core.Maybe Types.String,
    -- | The ID of the image.
    imageId :: Core.Maybe Types.String,
    -- | The percent complete of the export image task.
    progress :: Core.Maybe Types.String,
    -- | Information about the destination Amazon S3 bucket.
    s3ExportLocation :: Core.Maybe Types.ExportTaskS3Location,
    -- | The status of the export image task. The possible values are @active@ , @completed@ , @deleting@ , and @deleted@ .
    status :: Core.Maybe Types.String,
    -- | The status message for the export image task.
    statusMessage :: Core.Maybe Types.String,
    -- | Any tags assigned to the image being exported.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExportImageTask' value with any optional fields omitted.
mkExportImageTask ::
  ExportImageTask
mkExportImageTask =
  ExportImageTask'
    { description = Core.Nothing,
      exportImageTaskId = Core.Nothing,
      imageId = Core.Nothing,
      progress = Core.Nothing,
      s3ExportLocation = Core.Nothing,
      status = Core.Nothing,
      statusMessage = Core.Nothing,
      tags = Core.Nothing
    }

-- | A description of the image being exported.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eitDescription :: Lens.Lens' ExportImageTask (Core.Maybe Types.String)
eitDescription = Lens.field @"description"
{-# DEPRECATED eitDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ID of the export image task.
--
-- /Note:/ Consider using 'exportImageTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eitExportImageTaskId :: Lens.Lens' ExportImageTask (Core.Maybe Types.String)
eitExportImageTaskId = Lens.field @"exportImageTaskId"
{-# DEPRECATED eitExportImageTaskId "Use generic-lens or generic-optics with 'exportImageTaskId' instead." #-}

-- | The ID of the image.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eitImageId :: Lens.Lens' ExportImageTask (Core.Maybe Types.String)
eitImageId = Lens.field @"imageId"
{-# DEPRECATED eitImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The percent complete of the export image task.
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eitProgress :: Lens.Lens' ExportImageTask (Core.Maybe Types.String)
eitProgress = Lens.field @"progress"
{-# DEPRECATED eitProgress "Use generic-lens or generic-optics with 'progress' instead." #-}

-- | Information about the destination Amazon S3 bucket.
--
-- /Note:/ Consider using 's3ExportLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eitS3ExportLocation :: Lens.Lens' ExportImageTask (Core.Maybe Types.ExportTaskS3Location)
eitS3ExportLocation = Lens.field @"s3ExportLocation"
{-# DEPRECATED eitS3ExportLocation "Use generic-lens or generic-optics with 's3ExportLocation' instead." #-}

-- | The status of the export image task. The possible values are @active@ , @completed@ , @deleting@ , and @deleted@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eitStatus :: Lens.Lens' ExportImageTask (Core.Maybe Types.String)
eitStatus = Lens.field @"status"
{-# DEPRECATED eitStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The status message for the export image task.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eitStatusMessage :: Lens.Lens' ExportImageTask (Core.Maybe Types.String)
eitStatusMessage = Lens.field @"statusMessage"
{-# DEPRECATED eitStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | Any tags assigned to the image being exported.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eitTags :: Lens.Lens' ExportImageTask (Core.Maybe [Types.Tag])
eitTags = Lens.field @"tags"
{-# DEPRECATED eitTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromXML ExportImageTask where
  parseXML x =
    ExportImageTask'
      Core.<$> (x Core..@? "description")
      Core.<*> (x Core..@? "exportImageTaskId")
      Core.<*> (x Core..@? "imageId")
      Core.<*> (x Core..@? "progress")
      Core.<*> (x Core..@? "s3ExportLocation")
      Core.<*> (x Core..@? "status")
      Core.<*> (x Core..@? "statusMessage")
      Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
