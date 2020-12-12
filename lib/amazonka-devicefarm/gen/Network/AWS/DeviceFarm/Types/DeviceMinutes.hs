{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.DeviceMinutes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.DeviceMinutes
  ( DeviceMinutes (..),

    -- * Smart constructor
    mkDeviceMinutes,

    -- * Lenses
    dmMetered,
    dmTotal,
    dmUnmetered,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the total (metered or unmetered) minutes used by the resource to run tests. Contains the sum of minutes consumed by all children.
--
-- /See:/ 'mkDeviceMinutes' smart constructor.
data DeviceMinutes = DeviceMinutes'
  { metered ::
      Lude.Maybe Lude.Double,
    total :: Lude.Maybe Lude.Double,
    unmetered :: Lude.Maybe Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeviceMinutes' with the minimum fields required to make a request.
--
-- * 'metered' - When specified, represents only the sum of metered minutes used by the resource to run tests.
-- * 'total' - When specified, represents the total minutes used by the resource to run tests.
-- * 'unmetered' - When specified, represents only the sum of unmetered minutes used by the resource to run tests.
mkDeviceMinutes ::
  DeviceMinutes
mkDeviceMinutes =
  DeviceMinutes'
    { metered = Lude.Nothing,
      total = Lude.Nothing,
      unmetered = Lude.Nothing
    }

-- | When specified, represents only the sum of metered minutes used by the resource to run tests.
--
-- /Note:/ Consider using 'metered' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmMetered :: Lens.Lens' DeviceMinutes (Lude.Maybe Lude.Double)
dmMetered = Lens.lens (metered :: DeviceMinutes -> Lude.Maybe Lude.Double) (\s a -> s {metered = a} :: DeviceMinutes)
{-# DEPRECATED dmMetered "Use generic-lens or generic-optics with 'metered' instead." #-}

-- | When specified, represents the total minutes used by the resource to run tests.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmTotal :: Lens.Lens' DeviceMinutes (Lude.Maybe Lude.Double)
dmTotal = Lens.lens (total :: DeviceMinutes -> Lude.Maybe Lude.Double) (\s a -> s {total = a} :: DeviceMinutes)
{-# DEPRECATED dmTotal "Use generic-lens or generic-optics with 'total' instead." #-}

-- | When specified, represents only the sum of unmetered minutes used by the resource to run tests.
--
-- /Note:/ Consider using 'unmetered' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmUnmetered :: Lens.Lens' DeviceMinutes (Lude.Maybe Lude.Double)
dmUnmetered = Lens.lens (unmetered :: DeviceMinutes -> Lude.Maybe Lude.Double) (\s a -> s {unmetered = a} :: DeviceMinutes)
{-# DEPRECATED dmUnmetered "Use generic-lens or generic-optics with 'unmetered' instead." #-}

instance Lude.FromJSON DeviceMinutes where
  parseJSON =
    Lude.withObject
      "DeviceMinutes"
      ( \x ->
          DeviceMinutes'
            Lude.<$> (x Lude..:? "metered")
            Lude.<*> (x Lude..:? "total")
            Lude.<*> (x Lude..:? "unmetered")
      )
