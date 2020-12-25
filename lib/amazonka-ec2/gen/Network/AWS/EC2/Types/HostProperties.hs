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
    hpCores,
    hpInstanceFamily,
    hpInstanceType,
    hpSockets,
    hpTotalVCpus,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the properties of a Dedicated Host.
--
-- /See:/ 'mkHostProperties' smart constructor.
data HostProperties = HostProperties'
  { -- | The number of cores on the Dedicated Host.
    cores :: Core.Maybe Core.Int,
    -- | The instance family supported by the Dedicated Host. For example, @m5@ .
    instanceFamily :: Core.Maybe Types.String,
    -- | The instance type supported by the Dedicated Host. For example, @m5.large@ . If the host supports multiple instance types, no __instanceType__ is returned.
    instanceType :: Core.Maybe Types.String,
    -- | The number of sockets on the Dedicated Host.
    sockets :: Core.Maybe Core.Int,
    -- | The total number of vCPUs on the Dedicated Host.
    totalVCpus :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HostProperties' value with any optional fields omitted.
mkHostProperties ::
  HostProperties
mkHostProperties =
  HostProperties'
    { cores = Core.Nothing,
      instanceFamily = Core.Nothing,
      instanceType = Core.Nothing,
      sockets = Core.Nothing,
      totalVCpus = Core.Nothing
    }

-- | The number of cores on the Dedicated Host.
--
-- /Note:/ Consider using 'cores' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpCores :: Lens.Lens' HostProperties (Core.Maybe Core.Int)
hpCores = Lens.field @"cores"
{-# DEPRECATED hpCores "Use generic-lens or generic-optics with 'cores' instead." #-}

-- | The instance family supported by the Dedicated Host. For example, @m5@ .
--
-- /Note:/ Consider using 'instanceFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpInstanceFamily :: Lens.Lens' HostProperties (Core.Maybe Types.String)
hpInstanceFamily = Lens.field @"instanceFamily"
{-# DEPRECATED hpInstanceFamily "Use generic-lens or generic-optics with 'instanceFamily' instead." #-}

-- | The instance type supported by the Dedicated Host. For example, @m5.large@ . If the host supports multiple instance types, no __instanceType__ is returned.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpInstanceType :: Lens.Lens' HostProperties (Core.Maybe Types.String)
hpInstanceType = Lens.field @"instanceType"
{-# DEPRECATED hpInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The number of sockets on the Dedicated Host.
--
-- /Note:/ Consider using 'sockets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpSockets :: Lens.Lens' HostProperties (Core.Maybe Core.Int)
hpSockets = Lens.field @"sockets"
{-# DEPRECATED hpSockets "Use generic-lens or generic-optics with 'sockets' instead." #-}

-- | The total number of vCPUs on the Dedicated Host.
--
-- /Note:/ Consider using 'totalVCpus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpTotalVCpus :: Lens.Lens' HostProperties (Core.Maybe Core.Int)
hpTotalVCpus = Lens.field @"totalVCpus"
{-# DEPRECATED hpTotalVCpus "Use generic-lens or generic-optics with 'totalVCpus' instead." #-}

instance Core.FromXML HostProperties where
  parseXML x =
    HostProperties'
      Core.<$> (x Core..@? "cores")
      Core.<*> (x Core..@? "instanceFamily")
      Core.<*> (x Core..@? "instanceType")
      Core.<*> (x Core..@? "sockets")
      Core.<*> (x Core..@? "totalVCpus")
