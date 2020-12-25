{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringConstraintsResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringConstraintsResource
  ( MonitoringConstraintsResource (..),

    -- * Smart constructor
    mkMonitoringConstraintsResource,

    -- * Lenses
    mcrS3Uri,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.S3Uri as Types

-- | The constraints resource for a monitoring job.
--
-- /See:/ 'mkMonitoringConstraintsResource' smart constructor.
newtype MonitoringConstraintsResource = MonitoringConstraintsResource'
  { -- | The Amazon S3 URI for the constraints resource.
    s3Uri :: Core.Maybe Types.S3Uri
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'MonitoringConstraintsResource' value with any optional fields omitted.
mkMonitoringConstraintsResource ::
  MonitoringConstraintsResource
mkMonitoringConstraintsResource =
  MonitoringConstraintsResource' {s3Uri = Core.Nothing}

-- | The Amazon S3 URI for the constraints resource.
--
-- /Note:/ Consider using 's3Uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcrS3Uri :: Lens.Lens' MonitoringConstraintsResource (Core.Maybe Types.S3Uri)
mcrS3Uri = Lens.field @"s3Uri"
{-# DEPRECATED mcrS3Uri "Use generic-lens or generic-optics with 's3Uri' instead." #-}

instance Core.FromJSON MonitoringConstraintsResource where
  toJSON MonitoringConstraintsResource {..} =
    Core.object (Core.catMaybes [("S3Uri" Core..=) Core.<$> s3Uri])

instance Core.FromJSON MonitoringConstraintsResource where
  parseJSON =
    Core.withObject "MonitoringConstraintsResource" Core.$
      \x -> MonitoringConstraintsResource' Core.<$> (x Core..:? "S3Uri")
