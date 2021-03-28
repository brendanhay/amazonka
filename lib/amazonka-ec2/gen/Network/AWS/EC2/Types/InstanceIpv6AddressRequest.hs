{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceIpv6AddressRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.InstanceIpv6AddressRequest
  ( InstanceIpv6AddressRequest (..)
  -- * Smart constructor
  , mkInstanceIpv6AddressRequest
  -- * Lenses
  , iiarIpv6Address
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an IPv6 address.
--
-- /See:/ 'mkInstanceIpv6AddressRequest' smart constructor.
newtype InstanceIpv6AddressRequest = InstanceIpv6AddressRequest'
  { ipv6Address :: Core.Maybe Core.Text
    -- ^ The IPv6 address.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceIpv6AddressRequest' value with any optional fields omitted.
mkInstanceIpv6AddressRequest
    :: InstanceIpv6AddressRequest
mkInstanceIpv6AddressRequest
  = InstanceIpv6AddressRequest'{ipv6Address = Core.Nothing}

-- | The IPv6 address.
--
-- /Note:/ Consider using 'ipv6Address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiarIpv6Address :: Lens.Lens' InstanceIpv6AddressRequest (Core.Maybe Core.Text)
iiarIpv6Address = Lens.field @"ipv6Address"
{-# INLINEABLE iiarIpv6Address #-}
{-# DEPRECATED ipv6Address "Use generic-lens or generic-optics with 'ipv6Address' instead"  #-}

instance Core.ToQuery InstanceIpv6AddressRequest where
        toQuery InstanceIpv6AddressRequest{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Ipv6Address")
              ipv6Address
