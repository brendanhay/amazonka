{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstancesIpv6Address
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ScheduledInstancesIpv6Address
  ( ScheduledInstancesIpv6Address (..)
  -- * Smart constructor
  , mkScheduledInstancesIpv6Address
  -- * Lenses
  , siiaIpv6Address
  ) where

import qualified Network.AWS.EC2.Types.Ipv6Address as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an IPv6 address.
--
-- /See:/ 'mkScheduledInstancesIpv6Address' smart constructor.
newtype ScheduledInstancesIpv6Address = ScheduledInstancesIpv6Address'
  { ipv6Address :: Core.Maybe Types.Ipv6Address
    -- ^ The IPv6 address.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ScheduledInstancesIpv6Address' value with any optional fields omitted.
mkScheduledInstancesIpv6Address
    :: ScheduledInstancesIpv6Address
mkScheduledInstancesIpv6Address
  = ScheduledInstancesIpv6Address'{ipv6Address = Core.Nothing}

-- | The IPv6 address.
--
-- /Note:/ Consider using 'ipv6Address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siiaIpv6Address :: Lens.Lens' ScheduledInstancesIpv6Address (Core.Maybe Types.Ipv6Address)
siiaIpv6Address = Lens.field @"ipv6Address"
{-# INLINEABLE siiaIpv6Address #-}
{-# DEPRECATED ipv6Address "Use generic-lens or generic-optics with 'ipv6Address' instead"  #-}

instance Core.ToQuery ScheduledInstancesIpv6Address where
        toQuery ScheduledInstancesIpv6Address{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Ipv6Address")
              ipv6Address
