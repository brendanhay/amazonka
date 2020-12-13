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

import Network.AWS.ElasticBeanstalk.Types.CPUUtilization
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | CPU utilization and load average metrics for an Amazon EC2 instance.
--
-- /See:/ 'mkSystemStatus' smart constructor.
data SystemStatus = SystemStatus'
  { -- | CPU utilization metrics for the instance.
    cpuUtilization :: Lude.Maybe CPUUtilization,
    -- | Load average in the last 1-minute, 5-minute, and 15-minute periods. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-metrics.html#health-enhanced-metrics-os Operating System Metrics> .
    loadAverage :: Lude.Maybe [Lude.Double]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SystemStatus' with the minimum fields required to make a request.
--
-- * 'cpuUtilization' - CPU utilization metrics for the instance.
-- * 'loadAverage' - Load average in the last 1-minute, 5-minute, and 15-minute periods. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-metrics.html#health-enhanced-metrics-os Operating System Metrics> .
mkSystemStatus ::
  SystemStatus
mkSystemStatus =
  SystemStatus'
    { cpuUtilization = Lude.Nothing,
      loadAverage = Lude.Nothing
    }

-- | CPU utilization metrics for the instance.
--
-- /Note:/ Consider using 'cpuUtilization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssCPUUtilization :: Lens.Lens' SystemStatus (Lude.Maybe CPUUtilization)
ssCPUUtilization = Lens.lens (cpuUtilization :: SystemStatus -> Lude.Maybe CPUUtilization) (\s a -> s {cpuUtilization = a} :: SystemStatus)
{-# DEPRECATED ssCPUUtilization "Use generic-lens or generic-optics with 'cpuUtilization' instead." #-}

-- | Load average in the last 1-minute, 5-minute, and 15-minute periods. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-metrics.html#health-enhanced-metrics-os Operating System Metrics> .
--
-- /Note:/ Consider using 'loadAverage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssLoadAverage :: Lens.Lens' SystemStatus (Lude.Maybe [Lude.Double])
ssLoadAverage = Lens.lens (loadAverage :: SystemStatus -> Lude.Maybe [Lude.Double]) (\s a -> s {loadAverage = a} :: SystemStatus)
{-# DEPRECATED ssLoadAverage "Use generic-lens or generic-optics with 'loadAverage' instead." #-}

instance Lude.FromXML SystemStatus where
  parseXML x =
    SystemStatus'
      Lude.<$> (x Lude..@? "CPUUtilization")
      Lude.<*> ( x Lude..@? "LoadAverage" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
