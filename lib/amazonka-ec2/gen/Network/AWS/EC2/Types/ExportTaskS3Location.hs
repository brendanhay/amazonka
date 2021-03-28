{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ExportTaskS3Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ExportTaskS3Location
  ( ExportTaskS3Location (..)
  -- * Smart constructor
  , mkExportTaskS3Location
  -- * Lenses
  , etslS3Bucket
  , etslS3Prefix
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the destination for an export image task.
--
-- /See:/ 'mkExportTaskS3Location' smart constructor.
data ExportTaskS3Location = ExportTaskS3Location'
  { s3Bucket :: Core.Maybe Core.Text
    -- ^ The destination Amazon S3 bucket.
  , s3Prefix :: Core.Maybe Core.Text
    -- ^ The prefix (logical hierarchy) in the bucket.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExportTaskS3Location' value with any optional fields omitted.
mkExportTaskS3Location
    :: ExportTaskS3Location
mkExportTaskS3Location
  = ExportTaskS3Location'{s3Bucket = Core.Nothing,
                          s3Prefix = Core.Nothing}

-- | The destination Amazon S3 bucket.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etslS3Bucket :: Lens.Lens' ExportTaskS3Location (Core.Maybe Core.Text)
etslS3Bucket = Lens.field @"s3Bucket"
{-# INLINEABLE etslS3Bucket #-}
{-# DEPRECATED s3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead"  #-}

-- | The prefix (logical hierarchy) in the bucket.
--
-- /Note:/ Consider using 's3Prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etslS3Prefix :: Lens.Lens' ExportTaskS3Location (Core.Maybe Core.Text)
etslS3Prefix = Lens.field @"s3Prefix"
{-# INLINEABLE etslS3Prefix #-}
{-# DEPRECATED s3Prefix "Use generic-lens or generic-optics with 's3Prefix' instead"  #-}

instance Core.FromXML ExportTaskS3Location where
        parseXML x
          = ExportTaskS3Location' Core.<$>
              (x Core..@? "s3Bucket") Core.<*> x Core..@? "s3Prefix"
