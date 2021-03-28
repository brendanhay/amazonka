{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceIpv6Address
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.InstanceIpv6Address
  ( InstanceIpv6Address (..)
  -- * Smart constructor
  , mkInstanceIpv6Address
  -- * Lenses
  , iiaIpv6Address
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an IPv6 address.
--
-- /See:/ 'mkInstanceIpv6Address' smart constructor.
newtype InstanceIpv6Address = InstanceIpv6Address'
  { ipv6Address :: Core.Maybe Core.Text
    -- ^ The IPv6 address.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceIpv6Address' value with any optional fields omitted.
mkInstanceIpv6Address
    :: InstanceIpv6Address
mkInstanceIpv6Address
  = InstanceIpv6Address'{ipv6Address = Core.Nothing}

-- | The IPv6 address.
--
-- /Note:/ Consider using 'ipv6Address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiaIpv6Address :: Lens.Lens' InstanceIpv6Address (Core.Maybe Core.Text)
iiaIpv6Address = Lens.field @"ipv6Address"
{-# INLINEABLE iiaIpv6Address #-}
{-# DEPRECATED ipv6Address "Use generic-lens or generic-optics with 'ipv6Address' instead"  #-}

instance Core.ToQuery InstanceIpv6Address where
        toQuery InstanceIpv6Address{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Ipv6Address")
              ipv6Address

instance Core.FromXML InstanceIpv6Address where
        parseXML x
          = InstanceIpv6Address' Core.<$> (x Core..@? "ipv6Address")
