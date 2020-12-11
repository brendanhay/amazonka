-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabaseHardware
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RelationalDatabaseHardware
  ( RelationalDatabaseHardware (..),

    -- * Smart constructor
    mkRelationalDatabaseHardware,

    -- * Lenses
    rdhCpuCount,
    rdhDiskSizeInGb,
    rdhRamSizeInGb,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the hardware of a database.
--
-- /See:/ 'mkRelationalDatabaseHardware' smart constructor.
data RelationalDatabaseHardware = RelationalDatabaseHardware'
  { cpuCount ::
      Lude.Maybe Lude.Int,
    diskSizeInGb :: Lude.Maybe Lude.Int,
    ramSizeInGb :: Lude.Maybe Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RelationalDatabaseHardware' with the minimum fields required to make a request.
--
-- * 'cpuCount' - The number of vCPUs for the database.
-- * 'diskSizeInGb' - The size of the disk for the database.
-- * 'ramSizeInGb' - The amount of RAM in GB for the database.
mkRelationalDatabaseHardware ::
  RelationalDatabaseHardware
mkRelationalDatabaseHardware =
  RelationalDatabaseHardware'
    { cpuCount = Lude.Nothing,
      diskSizeInGb = Lude.Nothing,
      ramSizeInGb = Lude.Nothing
    }

-- | The number of vCPUs for the database.
--
-- /Note:/ Consider using 'cpuCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdhCpuCount :: Lens.Lens' RelationalDatabaseHardware (Lude.Maybe Lude.Int)
rdhCpuCount = Lens.lens (cpuCount :: RelationalDatabaseHardware -> Lude.Maybe Lude.Int) (\s a -> s {cpuCount = a} :: RelationalDatabaseHardware)
{-# DEPRECATED rdhCpuCount "Use generic-lens or generic-optics with 'cpuCount' instead." #-}

-- | The size of the disk for the database.
--
-- /Note:/ Consider using 'diskSizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdhDiskSizeInGb :: Lens.Lens' RelationalDatabaseHardware (Lude.Maybe Lude.Int)
rdhDiskSizeInGb = Lens.lens (diskSizeInGb :: RelationalDatabaseHardware -> Lude.Maybe Lude.Int) (\s a -> s {diskSizeInGb = a} :: RelationalDatabaseHardware)
{-# DEPRECATED rdhDiskSizeInGb "Use generic-lens or generic-optics with 'diskSizeInGb' instead." #-}

-- | The amount of RAM in GB for the database.
--
-- /Note:/ Consider using 'ramSizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdhRamSizeInGb :: Lens.Lens' RelationalDatabaseHardware (Lude.Maybe Lude.Double)
rdhRamSizeInGb = Lens.lens (ramSizeInGb :: RelationalDatabaseHardware -> Lude.Maybe Lude.Double) (\s a -> s {ramSizeInGb = a} :: RelationalDatabaseHardware)
{-# DEPRECATED rdhRamSizeInGb "Use generic-lens or generic-optics with 'ramSizeInGb' instead." #-}

instance Lude.FromJSON RelationalDatabaseHardware where
  parseJSON =
    Lude.withObject
      "RelationalDatabaseHardware"
      ( \x ->
          RelationalDatabaseHardware'
            Lude.<$> (x Lude..:? "cpuCount")
            Lude.<*> (x Lude..:? "diskSizeInGb")
            Lude.<*> (x Lude..:? "ramSizeInGb")
      )
