{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ExportToS3TaskSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ExportToS3TaskSpecification
  ( ExportToS3TaskSpecification (..),

    -- * Smart constructor
    mkExportToS3TaskSpecification,

    -- * Lenses
    etstsContainerFormat,
    etstsS3Prefix,
    etstsS3Bucket,
    etstsDiskImageFormat,
  )
where

import Network.AWS.EC2.Types.ContainerFormat
import Network.AWS.EC2.Types.DiskImageFormat
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an instance export task.
--
-- /See:/ 'mkExportToS3TaskSpecification' smart constructor.
data ExportToS3TaskSpecification = ExportToS3TaskSpecification'
  { -- | The container format used to combine disk images with metadata (such as OVF). If absent, only the disk image is exported.
    containerFormat :: Lude.Maybe ContainerFormat,
    -- | The image is written to a single object in the Amazon S3 bucket at the S3 key s3prefix + exportTaskId + '.' + diskImageFormat.
    s3Prefix :: Lude.Maybe Lude.Text,
    -- | The Amazon S3 bucket for the destination image. The destination bucket must exist and grant WRITE and READ_ACP permissions to the AWS account @vm-import-export@amazon.com@ .
    s3Bucket :: Lude.Maybe Lude.Text,
    -- | The format for the exported image.
    diskImageFormat :: Lude.Maybe DiskImageFormat
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportToS3TaskSpecification' with the minimum fields required to make a request.
--
-- * 'containerFormat' - The container format used to combine disk images with metadata (such as OVF). If absent, only the disk image is exported.
-- * 's3Prefix' - The image is written to a single object in the Amazon S3 bucket at the S3 key s3prefix + exportTaskId + '.' + diskImageFormat.
-- * 's3Bucket' - The Amazon S3 bucket for the destination image. The destination bucket must exist and grant WRITE and READ_ACP permissions to the AWS account @vm-import-export@amazon.com@ .
-- * 'diskImageFormat' - The format for the exported image.
mkExportToS3TaskSpecification ::
  ExportToS3TaskSpecification
mkExportToS3TaskSpecification =
  ExportToS3TaskSpecification'
    { containerFormat = Lude.Nothing,
      s3Prefix = Lude.Nothing,
      s3Bucket = Lude.Nothing,
      diskImageFormat = Lude.Nothing
    }

-- | The container format used to combine disk images with metadata (such as OVF). If absent, only the disk image is exported.
--
-- /Note:/ Consider using 'containerFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etstsContainerFormat :: Lens.Lens' ExportToS3TaskSpecification (Lude.Maybe ContainerFormat)
etstsContainerFormat = Lens.lens (containerFormat :: ExportToS3TaskSpecification -> Lude.Maybe ContainerFormat) (\s a -> s {containerFormat = a} :: ExportToS3TaskSpecification)
{-# DEPRECATED etstsContainerFormat "Use generic-lens or generic-optics with 'containerFormat' instead." #-}

-- | The image is written to a single object in the Amazon S3 bucket at the S3 key s3prefix + exportTaskId + '.' + diskImageFormat.
--
-- /Note:/ Consider using 's3Prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etstsS3Prefix :: Lens.Lens' ExportToS3TaskSpecification (Lude.Maybe Lude.Text)
etstsS3Prefix = Lens.lens (s3Prefix :: ExportToS3TaskSpecification -> Lude.Maybe Lude.Text) (\s a -> s {s3Prefix = a} :: ExportToS3TaskSpecification)
{-# DEPRECATED etstsS3Prefix "Use generic-lens or generic-optics with 's3Prefix' instead." #-}

-- | The Amazon S3 bucket for the destination image. The destination bucket must exist and grant WRITE and READ_ACP permissions to the AWS account @vm-import-export@amazon.com@ .
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etstsS3Bucket :: Lens.Lens' ExportToS3TaskSpecification (Lude.Maybe Lude.Text)
etstsS3Bucket = Lens.lens (s3Bucket :: ExportToS3TaskSpecification -> Lude.Maybe Lude.Text) (\s a -> s {s3Bucket = a} :: ExportToS3TaskSpecification)
{-# DEPRECATED etstsS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

-- | The format for the exported image.
--
-- /Note:/ Consider using 'diskImageFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etstsDiskImageFormat :: Lens.Lens' ExportToS3TaskSpecification (Lude.Maybe DiskImageFormat)
etstsDiskImageFormat = Lens.lens (diskImageFormat :: ExportToS3TaskSpecification -> Lude.Maybe DiskImageFormat) (\s a -> s {diskImageFormat = a} :: ExportToS3TaskSpecification)
{-# DEPRECATED etstsDiskImageFormat "Use generic-lens or generic-optics with 'diskImageFormat' instead." #-}

instance Lude.ToQuery ExportToS3TaskSpecification where
  toQuery ExportToS3TaskSpecification' {..} =
    Lude.mconcat
      [ "ContainerFormat" Lude.=: containerFormat,
        "S3Prefix" Lude.=: s3Prefix,
        "S3Bucket" Lude.=: s3Bucket,
        "DiskImageFormat" Lude.=: diskImageFormat
      ]
