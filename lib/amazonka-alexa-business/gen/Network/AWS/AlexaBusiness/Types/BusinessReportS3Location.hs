{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.BusinessReportS3Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.BusinessReportS3Location
  ( BusinessReportS3Location (..),

    -- * Smart constructor
    mkBusinessReportS3Location,

    -- * Lenses
    brslBucketName,
    brslPath,
  )
where

import qualified Network.AWS.AlexaBusiness.Types.BucketName as Types
import qualified Network.AWS.AlexaBusiness.Types.Path as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The S3 location of the output reports.
--
-- /See:/ 'mkBusinessReportS3Location' smart constructor.
data BusinessReportS3Location = BusinessReportS3Location'
  { -- | The S3 bucket name of the output reports.
    bucketName :: Core.Maybe Types.BucketName,
    -- | The path of the business report.
    path :: Core.Maybe Types.Path
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BusinessReportS3Location' value with any optional fields omitted.
mkBusinessReportS3Location ::
  BusinessReportS3Location
mkBusinessReportS3Location =
  BusinessReportS3Location'
    { bucketName = Core.Nothing,
      path = Core.Nothing
    }

-- | The S3 bucket name of the output reports.
--
-- /Note:/ Consider using 'bucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brslBucketName :: Lens.Lens' BusinessReportS3Location (Core.Maybe Types.BucketName)
brslBucketName = Lens.field @"bucketName"
{-# DEPRECATED brslBucketName "Use generic-lens or generic-optics with 'bucketName' instead." #-}

-- | The path of the business report.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brslPath :: Lens.Lens' BusinessReportS3Location (Core.Maybe Types.Path)
brslPath = Lens.field @"path"
{-# DEPRECATED brslPath "Use generic-lens or generic-optics with 'path' instead." #-}

instance Core.FromJSON BusinessReportS3Location where
  parseJSON =
    Core.withObject "BusinessReportS3Location" Core.$
      \x ->
        BusinessReportS3Location'
          Core.<$> (x Core..:? "BucketName") Core.<*> (x Core..:? "Path")
