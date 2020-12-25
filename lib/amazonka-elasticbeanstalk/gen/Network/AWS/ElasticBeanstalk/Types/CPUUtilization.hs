{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.CPUUtilization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.CPUUtilization
  ( CPUUtilization (..),

    -- * Smart constructor
    mkCPUUtilization,

    -- * Lenses
    cpuuIOWait,
    cpuuIRQ,
    cpuuIdle,
    cpuuNice,
    cpuuPrivileged,
    cpuuSoftIRQ,
    cpuuSystem,
    cpuuUser,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | CPU utilization metrics for an instance.
--
-- /See:/ 'mkCPUUtilization' smart constructor.
data CPUUtilization = CPUUtilization'
  { -- | Available on Linux environments only.
    --
    -- Percentage of time that the CPU has spent in the @I/O Wait@ state over the last 10 seconds.
    iOWait :: Core.Maybe Core.Double,
    -- | Available on Linux environments only.
    --
    -- Percentage of time that the CPU has spent in the @IRQ@ state over the last 10 seconds.
    irq :: Core.Maybe Core.Double,
    -- | Percentage of time that the CPU has spent in the @Idle@ state over the last 10 seconds.
    idle :: Core.Maybe Core.Double,
    -- | Available on Linux environments only.
    --
    -- Percentage of time that the CPU has spent in the @Nice@ state over the last 10 seconds.
    nice :: Core.Maybe Core.Double,
    -- | Available on Windows environments only.
    --
    -- Percentage of time that the CPU has spent in the @Privileged@ state over the last 10 seconds.
    privileged :: Core.Maybe Core.Double,
    -- | Available on Linux environments only.
    --
    -- Percentage of time that the CPU has spent in the @SoftIRQ@ state over the last 10 seconds.
    softIRQ :: Core.Maybe Core.Double,
    -- | Available on Linux environments only.
    --
    -- Percentage of time that the CPU has spent in the @System@ state over the last 10 seconds.
    system :: Core.Maybe Core.Double,
    -- | Percentage of time that the CPU has spent in the @User@ state over the last 10 seconds.
    user :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CPUUtilization' value with any optional fields omitted.
mkCPUUtilization ::
  CPUUtilization
mkCPUUtilization =
  CPUUtilization'
    { iOWait = Core.Nothing,
      irq = Core.Nothing,
      idle = Core.Nothing,
      nice = Core.Nothing,
      privileged = Core.Nothing,
      softIRQ = Core.Nothing,
      system = Core.Nothing,
      user = Core.Nothing
    }

-- | Available on Linux environments only.
--
-- Percentage of time that the CPU has spent in the @I/O Wait@ state over the last 10 seconds.
--
-- /Note:/ Consider using 'iOWait' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpuuIOWait :: Lens.Lens' CPUUtilization (Core.Maybe Core.Double)
cpuuIOWait = Lens.field @"iOWait"
{-# DEPRECATED cpuuIOWait "Use generic-lens or generic-optics with 'iOWait' instead." #-}

-- | Available on Linux environments only.
--
-- Percentage of time that the CPU has spent in the @IRQ@ state over the last 10 seconds.
--
-- /Note:/ Consider using 'irq' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpuuIRQ :: Lens.Lens' CPUUtilization (Core.Maybe Core.Double)
cpuuIRQ = Lens.field @"irq"
{-# DEPRECATED cpuuIRQ "Use generic-lens or generic-optics with 'irq' instead." #-}

-- | Percentage of time that the CPU has spent in the @Idle@ state over the last 10 seconds.
--
-- /Note:/ Consider using 'idle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpuuIdle :: Lens.Lens' CPUUtilization (Core.Maybe Core.Double)
cpuuIdle = Lens.field @"idle"
{-# DEPRECATED cpuuIdle "Use generic-lens or generic-optics with 'idle' instead." #-}

-- | Available on Linux environments only.
--
-- Percentage of time that the CPU has spent in the @Nice@ state over the last 10 seconds.
--
-- /Note:/ Consider using 'nice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpuuNice :: Lens.Lens' CPUUtilization (Core.Maybe Core.Double)
cpuuNice = Lens.field @"nice"
{-# DEPRECATED cpuuNice "Use generic-lens or generic-optics with 'nice' instead." #-}

-- | Available on Windows environments only.
--
-- Percentage of time that the CPU has spent in the @Privileged@ state over the last 10 seconds.
--
-- /Note:/ Consider using 'privileged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpuuPrivileged :: Lens.Lens' CPUUtilization (Core.Maybe Core.Double)
cpuuPrivileged = Lens.field @"privileged"
{-# DEPRECATED cpuuPrivileged "Use generic-lens or generic-optics with 'privileged' instead." #-}

-- | Available on Linux environments only.
--
-- Percentage of time that the CPU has spent in the @SoftIRQ@ state over the last 10 seconds.
--
-- /Note:/ Consider using 'softIRQ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpuuSoftIRQ :: Lens.Lens' CPUUtilization (Core.Maybe Core.Double)
cpuuSoftIRQ = Lens.field @"softIRQ"
{-# DEPRECATED cpuuSoftIRQ "Use generic-lens or generic-optics with 'softIRQ' instead." #-}

-- | Available on Linux environments only.
--
-- Percentage of time that the CPU has spent in the @System@ state over the last 10 seconds.
--
-- /Note:/ Consider using 'system' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpuuSystem :: Lens.Lens' CPUUtilization (Core.Maybe Core.Double)
cpuuSystem = Lens.field @"system"
{-# DEPRECATED cpuuSystem "Use generic-lens or generic-optics with 'system' instead." #-}

-- | Percentage of time that the CPU has spent in the @User@ state over the last 10 seconds.
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpuuUser :: Lens.Lens' CPUUtilization (Core.Maybe Core.Double)
cpuuUser = Lens.field @"user"
{-# DEPRECATED cpuuUser "Use generic-lens or generic-optics with 'user' instead." #-}

instance Core.FromXML CPUUtilization where
  parseXML x =
    CPUUtilization'
      Core.<$> (x Core..@? "IOWait")
      Core.<*> (x Core..@? "IRQ")
      Core.<*> (x Core..@? "Idle")
      Core.<*> (x Core..@? "Nice")
      Core.<*> (x Core..@? "Privileged")
      Core.<*> (x Core..@? "SoftIRQ")
      Core.<*> (x Core..@? "System")
      Core.<*> (x Core..@? "User")
