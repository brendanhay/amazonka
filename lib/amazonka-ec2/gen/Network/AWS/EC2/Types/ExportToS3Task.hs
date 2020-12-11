-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ExportToS3Task
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ExportToS3Task
  ( ExportToS3Task (..),

    -- * Smart constructor
    mkExportToS3Task,

    -- * Lenses
    etstS3Key,
    etstContainerFormat,
    etstS3Bucket,
    etstDiskImageFormat,
  )
where

import Network.AWS.EC2.Types.ContainerFormat
import Network.AWS.EC2.Types.DiskImageFormat
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the format and location for an instance export task.
--
-- /See:/ 'mkExportToS3Task' smart constructor.
data ExportToS3Task = ExportToS3Task'
  { s3Key ::
      Lude.Maybe Lude.Text,
    containerFormat :: Lude.Maybe ContainerFormat,
    s3Bucket :: Lude.Maybe Lude.Text,
    diskImageFormat :: Lude.Maybe DiskImageFormat
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportToS3Task' with the minimum fields required to make a request.
--
-- * 'containerFormat' - The container format used to combine disk images with metadata (such as OVF). If absent, only the disk image is exported.
-- * 'diskImageFormat' - The format for the exported image.
-- * 's3Bucket' - The Amazon S3 bucket for the destination image. The destination bucket must exist and grant WRITE and READ_ACP permissions to the AWS account @vm-import-export@amazon.com@ .
-- * 's3Key' - The encryption key for your S3 bucket.
mkExportToS3Task ::
  ExportToS3Task
mkExportToS3Task =
  ExportToS3Task'
    { s3Key = Lude.Nothing,
      containerFormat = Lude.Nothing,
      s3Bucket = Lude.Nothing,
      diskImageFormat = Lude.Nothing
    }

-- | The encryption key for your S3 bucket.
--
-- /Note:/ Consider using 's3Key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etstS3Key :: Lens.Lens' ExportToS3Task (Lude.Maybe Lude.Text)
etstS3Key = Lens.lens (s3Key :: ExportToS3Task -> Lude.Maybe Lude.Text) (\s a -> s {s3Key = a} :: ExportToS3Task)
{-# DEPRECATED etstS3Key "Use generic-lens or generic-optics with 's3Key' instead." #-}

-- | The container format used to combine disk images with metadata (such as OVF). If absent, only the disk image is exported.
--
-- /Note:/ Consider using 'containerFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etstContainerFormat :: Lens.Lens' ExportToS3Task (Lude.Maybe ContainerFormat)
etstContainerFormat = Lens.lens (containerFormat :: ExportToS3Task -> Lude.Maybe ContainerFormat) (\s a -> s {containerFormat = a} :: ExportToS3Task)
{-# DEPRECATED etstContainerFormat "Use generic-lens or generic-optics with 'containerFormat' instead." #-}

-- | The Amazon S3 bucket for the destination image. The destination bucket must exist and grant WRITE and READ_ACP permissions to the AWS account @vm-import-export@amazon.com@ .
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etstS3Bucket :: Lens.Lens' ExportToS3Task (Lude.Maybe Lude.Text)
etstS3Bucket = Lens.lens (s3Bucket :: ExportToS3Task -> Lude.Maybe Lude.Text) (\s a -> s {s3Bucket = a} :: ExportToS3Task)
{-# DEPRECATED etstS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

-- | The format for the exported image.
--
-- /Note:/ Consider using 'diskImageFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etstDiskImageFormat :: Lens.Lens' ExportToS3Task (Lude.Maybe DiskImageFormat)
etstDiskImageFormat = Lens.lens (diskImageFormat :: ExportToS3Task -> Lude.Maybe DiskImageFormat) (\s a -> s {diskImageFormat = a} :: ExportToS3Task)
{-# DEPRECATED etstDiskImageFormat "Use generic-lens or generic-optics with 'diskImageFormat' instead." #-}

instance Lude.FromXML ExportToS3Task where
  parseXML x =
    ExportToS3Task'
      Lude.<$> (x Lude..@? "s3Key")
      Lude.<*> (x Lude..@? "containerFormat")
      Lude.<*> (x Lude..@? "s3Bucket")
      Lude.<*> (x Lude..@? "diskImageFormat")
