{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.CPU
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.CPU
  ( CPU (..),

    -- * Smart constructor
    mkCPU,

    -- * Lenses
    cpuFrequency,
    cpuClock,
    cpuArchitecture,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the amount of CPU that an app is using on a physical device. Does not represent system-wide CPU usage.
--
-- /See:/ 'mkCPU' smart constructor.
data CPU = CPU'
  { -- | The CPU's frequency.
    frequency :: Lude.Maybe Lude.Text,
    -- | The clock speed of the device's CPU, expressed in hertz (Hz). For example, a 1.2 GHz CPU is expressed as 1200000000.
    clock :: Lude.Maybe Lude.Double,
    -- | The CPU's architecture (for example, x86 or ARM).
    architecture :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CPU' with the minimum fields required to make a request.
--
-- * 'frequency' - The CPU's frequency.
-- * 'clock' - The clock speed of the device's CPU, expressed in hertz (Hz). For example, a 1.2 GHz CPU is expressed as 1200000000.
-- * 'architecture' - The CPU's architecture (for example, x86 or ARM).
mkCPU ::
  CPU
mkCPU =
  CPU'
    { frequency = Lude.Nothing,
      clock = Lude.Nothing,
      architecture = Lude.Nothing
    }

-- | The CPU's frequency.
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpuFrequency :: Lens.Lens' CPU (Lude.Maybe Lude.Text)
cpuFrequency = Lens.lens (frequency :: CPU -> Lude.Maybe Lude.Text) (\s a -> s {frequency = a} :: CPU)
{-# DEPRECATED cpuFrequency "Use generic-lens or generic-optics with 'frequency' instead." #-}

-- | The clock speed of the device's CPU, expressed in hertz (Hz). For example, a 1.2 GHz CPU is expressed as 1200000000.
--
-- /Note:/ Consider using 'clock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpuClock :: Lens.Lens' CPU (Lude.Maybe Lude.Double)
cpuClock = Lens.lens (clock :: CPU -> Lude.Maybe Lude.Double) (\s a -> s {clock = a} :: CPU)
{-# DEPRECATED cpuClock "Use generic-lens or generic-optics with 'clock' instead." #-}

-- | The CPU's architecture (for example, x86 or ARM).
--
-- /Note:/ Consider using 'architecture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpuArchitecture :: Lens.Lens' CPU (Lude.Maybe Lude.Text)
cpuArchitecture = Lens.lens (architecture :: CPU -> Lude.Maybe Lude.Text) (\s a -> s {architecture = a} :: CPU)
{-# DEPRECATED cpuArchitecture "Use generic-lens or generic-optics with 'architecture' instead." #-}

instance Lude.FromJSON CPU where
  parseJSON =
    Lude.withObject
      "CPU"
      ( \x ->
          CPU'
            Lude.<$> (x Lude..:? "frequency")
            Lude.<*> (x Lude..:? "clock")
            Lude.<*> (x Lude..:? "architecture")
      )
