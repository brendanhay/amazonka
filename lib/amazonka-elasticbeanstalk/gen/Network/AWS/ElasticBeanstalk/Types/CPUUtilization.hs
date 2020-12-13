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
    cuSoftIRQ,
    cuIdle,
    cuIRQ,
    cuSystem,
    cuPrivileged,
    cuUser,
    cuIOWait,
    cuNice,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | CPU utilization metrics for an instance.
--
-- /See:/ 'mkCPUUtilization' smart constructor.
data CPUUtilization = CPUUtilization'
  { -- | Available on Linux environments only.
    --
    -- Percentage of time that the CPU has spent in the @SoftIRQ@ state over the last 10 seconds.
    softIRQ :: Lude.Maybe Lude.Double,
    -- | Percentage of time that the CPU has spent in the @Idle@ state over the last 10 seconds.
    idle :: Lude.Maybe Lude.Double,
    -- | Available on Linux environments only.
    --
    -- Percentage of time that the CPU has spent in the @IRQ@ state over the last 10 seconds.
    iRQ :: Lude.Maybe Lude.Double,
    -- | Available on Linux environments only.
    --
    -- Percentage of time that the CPU has spent in the @System@ state over the last 10 seconds.
    system :: Lude.Maybe Lude.Double,
    -- | Available on Windows environments only.
    --
    -- Percentage of time that the CPU has spent in the @Privileged@ state over the last 10 seconds.
    privileged :: Lude.Maybe Lude.Double,
    -- | Percentage of time that the CPU has spent in the @User@ state over the last 10 seconds.
    user :: Lude.Maybe Lude.Double,
    -- | Available on Linux environments only.
    --
    -- Percentage of time that the CPU has spent in the @I/O Wait@ state over the last 10 seconds.
    iOWait :: Lude.Maybe Lude.Double,
    -- | Available on Linux environments only.
    --
    -- Percentage of time that the CPU has spent in the @Nice@ state over the last 10 seconds.
    nice :: Lude.Maybe Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CPUUtilization' with the minimum fields required to make a request.
--
-- * 'softIRQ' - Available on Linux environments only.
--
-- Percentage of time that the CPU has spent in the @SoftIRQ@ state over the last 10 seconds.
-- * 'idle' - Percentage of time that the CPU has spent in the @Idle@ state over the last 10 seconds.
-- * 'iRQ' - Available on Linux environments only.
--
-- Percentage of time that the CPU has spent in the @IRQ@ state over the last 10 seconds.
-- * 'system' - Available on Linux environments only.
--
-- Percentage of time that the CPU has spent in the @System@ state over the last 10 seconds.
-- * 'privileged' - Available on Windows environments only.
--
-- Percentage of time that the CPU has spent in the @Privileged@ state over the last 10 seconds.
-- * 'user' - Percentage of time that the CPU has spent in the @User@ state over the last 10 seconds.
-- * 'iOWait' - Available on Linux environments only.
--
-- Percentage of time that the CPU has spent in the @I/O Wait@ state over the last 10 seconds.
-- * 'nice' - Available on Linux environments only.
--
-- Percentage of time that the CPU has spent in the @Nice@ state over the last 10 seconds.
mkCPUUtilization ::
  CPUUtilization
mkCPUUtilization =
  CPUUtilization'
    { softIRQ = Lude.Nothing,
      idle = Lude.Nothing,
      iRQ = Lude.Nothing,
      system = Lude.Nothing,
      privileged = Lude.Nothing,
      user = Lude.Nothing,
      iOWait = Lude.Nothing,
      nice = Lude.Nothing
    }

-- | Available on Linux environments only.
--
-- Percentage of time that the CPU has spent in the @SoftIRQ@ state over the last 10 seconds.
--
-- /Note:/ Consider using 'softIRQ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuSoftIRQ :: Lens.Lens' CPUUtilization (Lude.Maybe Lude.Double)
cuSoftIRQ = Lens.lens (softIRQ :: CPUUtilization -> Lude.Maybe Lude.Double) (\s a -> s {softIRQ = a} :: CPUUtilization)
{-# DEPRECATED cuSoftIRQ "Use generic-lens or generic-optics with 'softIRQ' instead." #-}

-- | Percentage of time that the CPU has spent in the @Idle@ state over the last 10 seconds.
--
-- /Note:/ Consider using 'idle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuIdle :: Lens.Lens' CPUUtilization (Lude.Maybe Lude.Double)
cuIdle = Lens.lens (idle :: CPUUtilization -> Lude.Maybe Lude.Double) (\s a -> s {idle = a} :: CPUUtilization)
{-# DEPRECATED cuIdle "Use generic-lens or generic-optics with 'idle' instead." #-}

-- | Available on Linux environments only.
--
-- Percentage of time that the CPU has spent in the @IRQ@ state over the last 10 seconds.
--
-- /Note:/ Consider using 'iRQ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuIRQ :: Lens.Lens' CPUUtilization (Lude.Maybe Lude.Double)
cuIRQ = Lens.lens (iRQ :: CPUUtilization -> Lude.Maybe Lude.Double) (\s a -> s {iRQ = a} :: CPUUtilization)
{-# DEPRECATED cuIRQ "Use generic-lens or generic-optics with 'iRQ' instead." #-}

-- | Available on Linux environments only.
--
-- Percentage of time that the CPU has spent in the @System@ state over the last 10 seconds.
--
-- /Note:/ Consider using 'system' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuSystem :: Lens.Lens' CPUUtilization (Lude.Maybe Lude.Double)
cuSystem = Lens.lens (system :: CPUUtilization -> Lude.Maybe Lude.Double) (\s a -> s {system = a} :: CPUUtilization)
{-# DEPRECATED cuSystem "Use generic-lens or generic-optics with 'system' instead." #-}

-- | Available on Windows environments only.
--
-- Percentage of time that the CPU has spent in the @Privileged@ state over the last 10 seconds.
--
-- /Note:/ Consider using 'privileged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuPrivileged :: Lens.Lens' CPUUtilization (Lude.Maybe Lude.Double)
cuPrivileged = Lens.lens (privileged :: CPUUtilization -> Lude.Maybe Lude.Double) (\s a -> s {privileged = a} :: CPUUtilization)
{-# DEPRECATED cuPrivileged "Use generic-lens or generic-optics with 'privileged' instead." #-}

-- | Percentage of time that the CPU has spent in the @User@ state over the last 10 seconds.
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuUser :: Lens.Lens' CPUUtilization (Lude.Maybe Lude.Double)
cuUser = Lens.lens (user :: CPUUtilization -> Lude.Maybe Lude.Double) (\s a -> s {user = a} :: CPUUtilization)
{-# DEPRECATED cuUser "Use generic-lens or generic-optics with 'user' instead." #-}

-- | Available on Linux environments only.
--
-- Percentage of time that the CPU has spent in the @I/O Wait@ state over the last 10 seconds.
--
-- /Note:/ Consider using 'iOWait' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuIOWait :: Lens.Lens' CPUUtilization (Lude.Maybe Lude.Double)
cuIOWait = Lens.lens (iOWait :: CPUUtilization -> Lude.Maybe Lude.Double) (\s a -> s {iOWait = a} :: CPUUtilization)
{-# DEPRECATED cuIOWait "Use generic-lens or generic-optics with 'iOWait' instead." #-}

-- | Available on Linux environments only.
--
-- Percentage of time that the CPU has spent in the @Nice@ state over the last 10 seconds.
--
-- /Note:/ Consider using 'nice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuNice :: Lens.Lens' CPUUtilization (Lude.Maybe Lude.Double)
cuNice = Lens.lens (nice :: CPUUtilization -> Lude.Maybe Lude.Double) (\s a -> s {nice = a} :: CPUUtilization)
{-# DEPRECATED cuNice "Use generic-lens or generic-optics with 'nice' instead." #-}

instance Lude.FromXML CPUUtilization where
  parseXML x =
    CPUUtilization'
      Lude.<$> (x Lude..@? "SoftIRQ")
      Lude.<*> (x Lude..@? "Idle")
      Lude.<*> (x Lude..@? "IRQ")
      Lude.<*> (x Lude..@? "System")
      Lude.<*> (x Lude..@? "Privileged")
      Lude.<*> (x Lude..@? "User")
      Lude.<*> (x Lude..@? "IOWait")
      Lude.<*> (x Lude..@? "Nice")
