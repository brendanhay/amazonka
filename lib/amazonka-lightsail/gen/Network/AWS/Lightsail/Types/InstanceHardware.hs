{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceHardware
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstanceHardware
  ( InstanceHardware (..),

    -- * Smart constructor
    mkInstanceHardware,

    -- * Lenses
    ihCpuCount,
    ihDisks,
    ihRamSizeInGb,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.Disk
import qualified Network.AWS.Prelude as Lude

-- | Describes the hardware for the instance.
--
-- /See:/ 'mkInstanceHardware' smart constructor.
data InstanceHardware = InstanceHardware'
  { -- | The number of vCPUs the instance has.
    cpuCount :: Lude.Maybe Lude.Int,
    -- | The disks attached to the instance.
    disks :: Lude.Maybe [Disk],
    -- | The amount of RAM in GB on the instance (e.g., @1.0@ ).
    ramSizeInGb :: Lude.Maybe Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceHardware' with the minimum fields required to make a request.
--
-- * 'cpuCount' - The number of vCPUs the instance has.
-- * 'disks' - The disks attached to the instance.
-- * 'ramSizeInGb' - The amount of RAM in GB on the instance (e.g., @1.0@ ).
mkInstanceHardware ::
  InstanceHardware
mkInstanceHardware =
  InstanceHardware'
    { cpuCount = Lude.Nothing,
      disks = Lude.Nothing,
      ramSizeInGb = Lude.Nothing
    }

-- | The number of vCPUs the instance has.
--
-- /Note:/ Consider using 'cpuCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ihCpuCount :: Lens.Lens' InstanceHardware (Lude.Maybe Lude.Int)
ihCpuCount = Lens.lens (cpuCount :: InstanceHardware -> Lude.Maybe Lude.Int) (\s a -> s {cpuCount = a} :: InstanceHardware)
{-# DEPRECATED ihCpuCount "Use generic-lens or generic-optics with 'cpuCount' instead." #-}

-- | The disks attached to the instance.
--
-- /Note:/ Consider using 'disks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ihDisks :: Lens.Lens' InstanceHardware (Lude.Maybe [Disk])
ihDisks = Lens.lens (disks :: InstanceHardware -> Lude.Maybe [Disk]) (\s a -> s {disks = a} :: InstanceHardware)
{-# DEPRECATED ihDisks "Use generic-lens or generic-optics with 'disks' instead." #-}

-- | The amount of RAM in GB on the instance (e.g., @1.0@ ).
--
-- /Note:/ Consider using 'ramSizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ihRamSizeInGb :: Lens.Lens' InstanceHardware (Lude.Maybe Lude.Double)
ihRamSizeInGb = Lens.lens (ramSizeInGb :: InstanceHardware -> Lude.Maybe Lude.Double) (\s a -> s {ramSizeInGb = a} :: InstanceHardware)
{-# DEPRECATED ihRamSizeInGb "Use generic-lens or generic-optics with 'ramSizeInGb' instead." #-}

instance Lude.FromJSON InstanceHardware where
  parseJSON =
    Lude.withObject
      "InstanceHardware"
      ( \x ->
          InstanceHardware'
            Lude.<$> (x Lude..:? "cpuCount")
            Lude.<*> (x Lude..:? "disks" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ramSizeInGb")
      )
