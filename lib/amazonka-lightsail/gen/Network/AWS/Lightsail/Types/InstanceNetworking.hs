{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceNetworking
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstanceNetworking
  ( InstanceNetworking (..),

    -- * Smart constructor
    mkInstanceNetworking,

    -- * Lenses
    inMonthlyTransfer,
    inPorts,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.InstancePortInfo
import Network.AWS.Lightsail.Types.MonthlyTransfer
import qualified Network.AWS.Prelude as Lude

-- | Describes monthly data transfer rates and port information for an instance.
--
-- /See:/ 'mkInstanceNetworking' smart constructor.
data InstanceNetworking = InstanceNetworking'
  { monthlyTransfer ::
      Lude.Maybe MonthlyTransfer,
    ports :: Lude.Maybe [InstancePortInfo]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceNetworking' with the minimum fields required to make a request.
--
-- * 'monthlyTransfer' - The amount of data in GB allocated for monthly data transfers.
-- * 'ports' - An array of key-value pairs containing information about the ports on the instance.
mkInstanceNetworking ::
  InstanceNetworking
mkInstanceNetworking =
  InstanceNetworking'
    { monthlyTransfer = Lude.Nothing,
      ports = Lude.Nothing
    }

-- | The amount of data in GB allocated for monthly data transfers.
--
-- /Note:/ Consider using 'monthlyTransfer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inMonthlyTransfer :: Lens.Lens' InstanceNetworking (Lude.Maybe MonthlyTransfer)
inMonthlyTransfer = Lens.lens (monthlyTransfer :: InstanceNetworking -> Lude.Maybe MonthlyTransfer) (\s a -> s {monthlyTransfer = a} :: InstanceNetworking)
{-# DEPRECATED inMonthlyTransfer "Use generic-lens or generic-optics with 'monthlyTransfer' instead." #-}

-- | An array of key-value pairs containing information about the ports on the instance.
--
-- /Note:/ Consider using 'ports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inPorts :: Lens.Lens' InstanceNetworking (Lude.Maybe [InstancePortInfo])
inPorts = Lens.lens (ports :: InstanceNetworking -> Lude.Maybe [InstancePortInfo]) (\s a -> s {ports = a} :: InstanceNetworking)
{-# DEPRECATED inPorts "Use generic-lens or generic-optics with 'ports' instead." #-}

instance Lude.FromJSON InstanceNetworking where
  parseJSON =
    Lude.withObject
      "InstanceNetworking"
      ( \x ->
          InstanceNetworking'
            Lude.<$> (x Lude..:? "monthlyTransfer")
            Lude.<*> (x Lude..:? "ports" Lude..!= Lude.mempty)
      )
