{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.SystemStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.SystemStatus
  ( SystemStatus (..),

    -- * Smart constructor
    mkSystemStatus,

    -- * Lenses
    ssCPUUtilization,
    ssLoadAverage,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types.CPUUtilization as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | CPU utilization and load average metrics for an Amazon EC2 instance.
--
-- /See:/ 'mkSystemStatus' smart constructor.
data SystemStatus = SystemStatus'
  { -- | CPU utilization metrics for the instance.
    cPUUtilization :: Core.Maybe Types.CPUUtilization,
    -- | Load average in the last 1-minute, 5-minute, and 15-minute periods. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-metrics.html#health-enhanced-metrics-os Operating System Metrics> .
    loadAverage :: Core.Maybe [Core.Double]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SystemStatus' value with any optional fields omitted.
mkSystemStatus ::
  SystemStatus
mkSystemStatus =
  SystemStatus'
    { cPUUtilization = Core.Nothing,
      loadAverage = Core.Nothing
    }

-- | CPU utilization metrics for the instance.
--
-- /Note:/ Consider using 'cPUUtilization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssCPUUtilization :: Lens.Lens' SystemStatus (Core.Maybe Types.CPUUtilization)
ssCPUUtilization = Lens.field @"cPUUtilization"
{-# DEPRECATED ssCPUUtilization "Use generic-lens or generic-optics with 'cPUUtilization' instead." #-}

-- | Load average in the last 1-minute, 5-minute, and 15-minute periods. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-metrics.html#health-enhanced-metrics-os Operating System Metrics> .
--
-- /Note:/ Consider using 'loadAverage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssLoadAverage :: Lens.Lens' SystemStatus (Core.Maybe [Core.Double])
ssLoadAverage = Lens.field @"loadAverage"
{-# DEPRECATED ssLoadAverage "Use generic-lens or generic-optics with 'loadAverage' instead." #-}

instance Core.FromXML SystemStatus where
  parseXML x =
    SystemStatus'
      Core.<$> (x Core..@? "CPUUtilization")
      Core.<*> (x Core..@? "LoadAverage" Core..<@> Core.parseXMLList "member")
