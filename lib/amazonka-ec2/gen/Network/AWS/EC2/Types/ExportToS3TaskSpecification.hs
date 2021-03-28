{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ExportToS3TaskSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ExportToS3TaskSpecification
  ( ExportToS3TaskSpecification (..)
  -- * Smart constructor
  , mkExportToS3TaskSpecification
  -- * Lenses
  , etstsContainerFormat
  , etstsDiskImageFormat
  , etstsS3Bucket
  , etstsS3Prefix
  ) where

import qualified Network.AWS.EC2.Types.ContainerFormat as Types
import qualified Network.AWS.EC2.Types.DiskImageFormat as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an instance export task.
--
-- /See:/ 'mkExportToS3TaskSpecification' smart constructor.
data ExportToS3TaskSpecification = ExportToS3TaskSpecification'
  { containerFormat :: Core.Maybe Types.ContainerFormat
    -- ^ The container format used to combine disk images with metadata (such as OVF). If absent, only the disk image is exported.
  , diskImageFormat :: Core.Maybe Types.DiskImageFormat
    -- ^ The format for the exported image.
  , s3Bucket :: Core.Maybe Core.Text
    -- ^ The Amazon S3 bucket for the destination image. The destination bucket must exist and grant WRITE and READ_ACP permissions to the AWS account @vm-import-export@amazon.com@ .
  , s3Prefix :: Core.Maybe Core.Text
    -- ^ The image is written to a single object in the Amazon S3 bucket at the S3 key s3prefix + exportTaskId + '.' + diskImageFormat.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExportToS3TaskSpecification' value with any optional fields omitted.
mkExportToS3TaskSpecification
    :: ExportToS3TaskSpecification
mkExportToS3TaskSpecification
  = ExportToS3TaskSpecification'{containerFormat = Core.Nothing,
                                 diskImageFormat = Core.Nothing, s3Bucket = Core.Nothing,
                                 s3Prefix = Core.Nothing}

-- | The container format used to combine disk images with metadata (such as OVF). If absent, only the disk image is exported.
--
-- /Note:/ Consider using 'containerFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etstsContainerFormat :: Lens.Lens' ExportToS3TaskSpecification (Core.Maybe Types.ContainerFormat)
etstsContainerFormat = Lens.field @"containerFormat"
{-# INLINEABLE etstsContainerFormat #-}
{-# DEPRECATED containerFormat "Use generic-lens or generic-optics with 'containerFormat' instead"  #-}

-- | The format for the exported image.
--
-- /Note:/ Consider using 'diskImageFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etstsDiskImageFormat :: Lens.Lens' ExportToS3TaskSpecification (Core.Maybe Types.DiskImageFormat)
etstsDiskImageFormat = Lens.field @"diskImageFormat"
{-# INLINEABLE etstsDiskImageFormat #-}
{-# DEPRECATED diskImageFormat "Use generic-lens or generic-optics with 'diskImageFormat' instead"  #-}

-- | The Amazon S3 bucket for the destination image. The destination bucket must exist and grant WRITE and READ_ACP permissions to the AWS account @vm-import-export@amazon.com@ .
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etstsS3Bucket :: Lens.Lens' ExportToS3TaskSpecification (Core.Maybe Core.Text)
etstsS3Bucket = Lens.field @"s3Bucket"
{-# INLINEABLE etstsS3Bucket #-}
{-# DEPRECATED s3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead"  #-}

-- | The image is written to a single object in the Amazon S3 bucket at the S3 key s3prefix + exportTaskId + '.' + diskImageFormat.
--
-- /Note:/ Consider using 's3Prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etstsS3Prefix :: Lens.Lens' ExportToS3TaskSpecification (Core.Maybe Core.Text)
etstsS3Prefix = Lens.field @"s3Prefix"
{-# INLINEABLE etstsS3Prefix #-}
{-# DEPRECATED s3Prefix "Use generic-lens or generic-optics with 's3Prefix' instead"  #-}

instance Core.ToQuery ExportToS3TaskSpecification where
        toQuery ExportToS3TaskSpecification{..}
          = Core.maybe Core.mempty (Core.toQueryPair "ContainerFormat")
              containerFormat
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DiskImageFormat")
                diskImageFormat
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "S3Bucket") s3Bucket
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "S3Prefix") s3Prefix
