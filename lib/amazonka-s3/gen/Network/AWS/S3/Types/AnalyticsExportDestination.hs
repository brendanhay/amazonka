{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.AnalyticsExportDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.AnalyticsExportDestination
  ( AnalyticsExportDestination (..),

    -- * Smart constructor
    mkAnalyticsExportDestination,

    -- * Lenses
    aedS3BucketDestination,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.AnalyticsS3BucketDestination as Types

-- | Where to publish the analytics results.
--
-- /See:/ 'mkAnalyticsExportDestination' smart constructor.
newtype AnalyticsExportDestination = AnalyticsExportDestination'
  { -- | A destination signifying output to an S3 bucket.
    s3BucketDestination :: Types.AnalyticsS3BucketDestination
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AnalyticsExportDestination' value with any optional fields omitted.
mkAnalyticsExportDestination ::
  -- | 's3BucketDestination'
  Types.AnalyticsS3BucketDestination ->
  AnalyticsExportDestination
mkAnalyticsExportDestination s3BucketDestination =
  AnalyticsExportDestination' {s3BucketDestination}

-- | A destination signifying output to an S3 bucket.
--
-- /Note:/ Consider using 's3BucketDestination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aedS3BucketDestination :: Lens.Lens' AnalyticsExportDestination Types.AnalyticsS3BucketDestination
aedS3BucketDestination = Lens.field @"s3BucketDestination"
{-# DEPRECATED aedS3BucketDestination "Use generic-lens or generic-optics with 's3BucketDestination' instead." #-}

instance Core.ToXML AnalyticsExportDestination where
  toXML AnalyticsExportDestination {..} =
    Core.toXMLNode "S3BucketDestination" s3BucketDestination

instance Core.FromXML AnalyticsExportDestination where
  parseXML x =
    AnalyticsExportDestination'
      Core.<$> (x Core..@ "S3BucketDestination")
