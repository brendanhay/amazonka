{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ExportTaskS3LocationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ExportTaskS3LocationRequest
  ( ExportTaskS3LocationRequest (..),

    -- * Smart constructor
    mkExportTaskS3LocationRequest,

    -- * Lenses
    etslrS3Bucket,
    etslrS3Prefix,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the destination for an export image task.
--
-- /See:/ 'mkExportTaskS3LocationRequest' smart constructor.
data ExportTaskS3LocationRequest = ExportTaskS3LocationRequest'
  { -- | The destination Amazon S3 bucket.
    s3Bucket :: Types.String,
    -- | The prefix (logical hierarchy) in the bucket.
    s3Prefix :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExportTaskS3LocationRequest' value with any optional fields omitted.
mkExportTaskS3LocationRequest ::
  -- | 's3Bucket'
  Types.String ->
  ExportTaskS3LocationRequest
mkExportTaskS3LocationRequest s3Bucket =
  ExportTaskS3LocationRequest' {s3Bucket, s3Prefix = Core.Nothing}

-- | The destination Amazon S3 bucket.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etslrS3Bucket :: Lens.Lens' ExportTaskS3LocationRequest Types.String
etslrS3Bucket = Lens.field @"s3Bucket"
{-# DEPRECATED etslrS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

-- | The prefix (logical hierarchy) in the bucket.
--
-- /Note:/ Consider using 's3Prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etslrS3Prefix :: Lens.Lens' ExportTaskS3LocationRequest (Core.Maybe Types.String)
etslrS3Prefix = Lens.field @"s3Prefix"
{-# DEPRECATED etslrS3Prefix "Use generic-lens or generic-optics with 's3Prefix' instead." #-}
