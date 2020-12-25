{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringStatisticsResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringStatisticsResource
  ( MonitoringStatisticsResource (..),

    -- * Smart constructor
    mkMonitoringStatisticsResource,

    -- * Lenses
    msrS3Uri,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.S3Uri as Types

-- | The statistics resource for a monitoring job.
--
-- /See:/ 'mkMonitoringStatisticsResource' smart constructor.
newtype MonitoringStatisticsResource = MonitoringStatisticsResource'
  { -- | The Amazon S3 URI for the statistics resource.
    s3Uri :: Core.Maybe Types.S3Uri
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'MonitoringStatisticsResource' value with any optional fields omitted.
mkMonitoringStatisticsResource ::
  MonitoringStatisticsResource
mkMonitoringStatisticsResource =
  MonitoringStatisticsResource' {s3Uri = Core.Nothing}

-- | The Amazon S3 URI for the statistics resource.
--
-- /Note:/ Consider using 's3Uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msrS3Uri :: Lens.Lens' MonitoringStatisticsResource (Core.Maybe Types.S3Uri)
msrS3Uri = Lens.field @"s3Uri"
{-# DEPRECATED msrS3Uri "Use generic-lens or generic-optics with 's3Uri' instead." #-}

instance Core.FromJSON MonitoringStatisticsResource where
  toJSON MonitoringStatisticsResource {..} =
    Core.object (Core.catMaybes [("S3Uri" Core..=) Core.<$> s3Uri])

instance Core.FromJSON MonitoringStatisticsResource where
  parseJSON =
    Core.withObject "MonitoringStatisticsResource" Core.$
      \x -> MonitoringStatisticsResource' Core.<$> (x Core..:? "S3Uri")
