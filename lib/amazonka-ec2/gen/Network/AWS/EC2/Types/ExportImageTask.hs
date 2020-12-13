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
    eitStatus,
    eitProgress,
    eitExportImageTaskId,
    eitStatusMessage,
    eitImageId,
    eitDescription,
    eitTags,
    eitS3ExportLocation,
  )
where

import Network.AWS.EC2.Types.ExportTaskS3Location
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an export image task.
--
-- /See:/ 'mkExportImageTask' smart constructor.
data ExportImageTask = ExportImageTask'
  { -- | The status of the export image task. The possible values are @active@ , @completed@ , @deleting@ , and @deleted@ .
    status :: Lude.Maybe Lude.Text,
    -- | The percent complete of the export image task.
    progress :: Lude.Maybe Lude.Text,
    -- | The ID of the export image task.
    exportImageTaskId :: Lude.Maybe Lude.Text,
    -- | The status message for the export image task.
    statusMessage :: Lude.Maybe Lude.Text,
    -- | The ID of the image.
    imageId :: Lude.Maybe Lude.Text,
    -- | A description of the image being exported.
    description :: Lude.Maybe Lude.Text,
    -- | Any tags assigned to the image being exported.
    tags :: Lude.Maybe [Tag],
    -- | Information about the destination Amazon S3 bucket.
    s3ExportLocation :: Lude.Maybe ExportTaskS3Location
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportImageTask' with the minimum fields required to make a request.
--
-- * 'status' - The status of the export image task. The possible values are @active@ , @completed@ , @deleting@ , and @deleted@ .
-- * 'progress' - The percent complete of the export image task.
-- * 'exportImageTaskId' - The ID of the export image task.
-- * 'statusMessage' - The status message for the export image task.
-- * 'imageId' - The ID of the image.
-- * 'description' - A description of the image being exported.
-- * 'tags' - Any tags assigned to the image being exported.
-- * 's3ExportLocation' - Information about the destination Amazon S3 bucket.
mkExportImageTask ::
  ExportImageTask
mkExportImageTask =
  ExportImageTask'
    { status = Lude.Nothing,
      progress = Lude.Nothing,
      exportImageTaskId = Lude.Nothing,
      statusMessage = Lude.Nothing,
      imageId = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      s3ExportLocation = Lude.Nothing
    }

-- | The status of the export image task. The possible values are @active@ , @completed@ , @deleting@ , and @deleted@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eitStatus :: Lens.Lens' ExportImageTask (Lude.Maybe Lude.Text)
eitStatus = Lens.lens (status :: ExportImageTask -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: ExportImageTask)
{-# DEPRECATED eitStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The percent complete of the export image task.
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eitProgress :: Lens.Lens' ExportImageTask (Lude.Maybe Lude.Text)
eitProgress = Lens.lens (progress :: ExportImageTask -> Lude.Maybe Lude.Text) (\s a -> s {progress = a} :: ExportImageTask)
{-# DEPRECATED eitProgress "Use generic-lens or generic-optics with 'progress' instead." #-}

-- | The ID of the export image task.
--
-- /Note:/ Consider using 'exportImageTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eitExportImageTaskId :: Lens.Lens' ExportImageTask (Lude.Maybe Lude.Text)
eitExportImageTaskId = Lens.lens (exportImageTaskId :: ExportImageTask -> Lude.Maybe Lude.Text) (\s a -> s {exportImageTaskId = a} :: ExportImageTask)
{-# DEPRECATED eitExportImageTaskId "Use generic-lens or generic-optics with 'exportImageTaskId' instead." #-}

-- | The status message for the export image task.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eitStatusMessage :: Lens.Lens' ExportImageTask (Lude.Maybe Lude.Text)
eitStatusMessage = Lens.lens (statusMessage :: ExportImageTask -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: ExportImageTask)
{-# DEPRECATED eitStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | The ID of the image.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eitImageId :: Lens.Lens' ExportImageTask (Lude.Maybe Lude.Text)
eitImageId = Lens.lens (imageId :: ExportImageTask -> Lude.Maybe Lude.Text) (\s a -> s {imageId = a} :: ExportImageTask)
{-# DEPRECATED eitImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | A description of the image being exported.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eitDescription :: Lens.Lens' ExportImageTask (Lude.Maybe Lude.Text)
eitDescription = Lens.lens (description :: ExportImageTask -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ExportImageTask)
{-# DEPRECATED eitDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Any tags assigned to the image being exported.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eitTags :: Lens.Lens' ExportImageTask (Lude.Maybe [Tag])
eitTags = Lens.lens (tags :: ExportImageTask -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ExportImageTask)
{-# DEPRECATED eitTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Information about the destination Amazon S3 bucket.
--
-- /Note:/ Consider using 's3ExportLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eitS3ExportLocation :: Lens.Lens' ExportImageTask (Lude.Maybe ExportTaskS3Location)
eitS3ExportLocation = Lens.lens (s3ExportLocation :: ExportImageTask -> Lude.Maybe ExportTaskS3Location) (\s a -> s {s3ExportLocation = a} :: ExportImageTask)
{-# DEPRECATED eitS3ExportLocation "Use generic-lens or generic-optics with 's3ExportLocation' instead." #-}

instance Lude.FromXML ExportImageTask where
  parseXML x =
    ExportImageTask'
      Lude.<$> (x Lude..@? "status")
      Lude.<*> (x Lude..@? "progress")
      Lude.<*> (x Lude..@? "exportImageTaskId")
      Lude.<*> (x Lude..@? "statusMessage")
      Lude.<*> (x Lude..@? "imageId")
      Lude.<*> (x Lude..@? "description")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "s3ExportLocation")
