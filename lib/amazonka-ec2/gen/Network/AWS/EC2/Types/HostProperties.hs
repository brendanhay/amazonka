{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.HostProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.HostProperties
  ( HostProperties (..),

    -- * Smart constructor
    mkHostProperties,

    -- * Lenses
    hpInstanceFamily,
    hpInstanceType,
    hpTotalVCPUs,
    hpCores,
    hpSockets,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the properties of a Dedicated Host.
--
-- /See:/ 'mkHostProperties' smart constructor.
data HostProperties = HostProperties'
  { -- | The instance family supported by the Dedicated Host. For example, @m5@ .
    instanceFamily :: Lude.Maybe Lude.Text,
    -- | The instance type supported by the Dedicated Host. For example, @m5.large@ . If the host supports multiple instance types, no __instanceType__ is returned.
    instanceType :: Lude.Maybe Lude.Text,
    -- | The total number of vCPUs on the Dedicated Host.
    totalVCPUs :: Lude.Maybe Lude.Int,
    -- | The number of cores on the Dedicated Host.
    cores :: Lude.Maybe Lude.Int,
    -- | The number of sockets on the Dedicated Host.
    sockets :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HostProperties' with the minimum fields required to make a request.
--
-- * 'instanceFamily' - The instance family supported by the Dedicated Host. For example, @m5@ .
-- * 'instanceType' - The instance type supported by the Dedicated Host. For example, @m5.large@ . If the host supports multiple instance types, no __instanceType__ is returned.
-- * 'totalVCPUs' - The total number of vCPUs on the Dedicated Host.
-- * 'cores' - The number of cores on the Dedicated Host.
-- * 'sockets' - The number of sockets on the Dedicated Host.
mkHostProperties ::
  HostProperties
mkHostProperties =
  HostProperties'
    { instanceFamily = Lude.Nothing,
      instanceType = Lude.Nothing,
      totalVCPUs = Lude.Nothing,
      cores = Lude.Nothing,
      sockets = Lude.Nothing
    }

-- | The instance family supported by the Dedicated Host. For example, @m5@ .
--
-- /Note:/ Consider using 'instanceFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpInstanceFamily :: Lens.Lens' HostProperties (Lude.Maybe Lude.Text)
hpInstanceFamily = Lens.lens (instanceFamily :: HostProperties -> Lude.Maybe Lude.Text) (\s a -> s {instanceFamily = a} :: HostProperties)
{-# DEPRECATED hpInstanceFamily "Use generic-lens or generic-optics with 'instanceFamily' instead." #-}

-- | The instance type supported by the Dedicated Host. For example, @m5.large@ . If the host supports multiple instance types, no __instanceType__ is returned.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpInstanceType :: Lens.Lens' HostProperties (Lude.Maybe Lude.Text)
hpInstanceType = Lens.lens (instanceType :: HostProperties -> Lude.Maybe Lude.Text) (\s a -> s {instanceType = a} :: HostProperties)
{-# DEPRECATED hpInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The total number of vCPUs on the Dedicated Host.
--
-- /Note:/ Consider using 'totalVCPUs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpTotalVCPUs :: Lens.Lens' HostProperties (Lude.Maybe Lude.Int)
hpTotalVCPUs = Lens.lens (totalVCPUs :: HostProperties -> Lude.Maybe Lude.Int) (\s a -> s {totalVCPUs = a} :: HostProperties)
{-# DEPRECATED hpTotalVCPUs "Use generic-lens or generic-optics with 'totalVCPUs' instead." #-}

-- | The number of cores on the Dedicated Host.
--
-- /Note:/ Consider using 'cores' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpCores :: Lens.Lens' HostProperties (Lude.Maybe Lude.Int)
hpCores = Lens.lens (cores :: HostProperties -> Lude.Maybe Lude.Int) (\s a -> s {cores = a} :: HostProperties)
{-# DEPRECATED hpCores "Use generic-lens or generic-optics with 'cores' instead." #-}

-- | The number of sockets on the Dedicated Host.
--
-- /Note:/ Consider using 'sockets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpSockets :: Lens.Lens' HostProperties (Lude.Maybe Lude.Int)
hpSockets = Lens.lens (sockets :: HostProperties -> Lude.Maybe Lude.Int) (\s a -> s {sockets = a} :: HostProperties)
{-# DEPRECATED hpSockets "Use generic-lens or generic-optics with 'sockets' instead." #-}

instance Lude.FromXML HostProperties where
  parseXML x =
    HostProperties'
      Lude.<$> (x Lude..@? "instanceFamily")
      Lude.<*> (x Lude..@? "instanceType")
      Lude.<*> (x Lude..@? "totalVCpus")
      Lude.<*> (x Lude..@? "cores")
      Lude.<*> (x Lude..@? "sockets")
