{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PrivateIpAddressSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.PrivateIpAddressSpecification
  ( PrivateIpAddressSpecification (..)
  -- * Smart constructor
  , mkPrivateIpAddressSpecification
  -- * Lenses
  , piasPrimary
  , piasPrivateIpAddress
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a secondary private IPv4 address for a network interface.
--
-- /See:/ 'mkPrivateIpAddressSpecification' smart constructor.
data PrivateIpAddressSpecification = PrivateIpAddressSpecification'
  { primary :: Core.Maybe Core.Bool
    -- ^ Indicates whether the private IPv4 address is the primary private IPv4 address. Only one IPv4 address can be designated as primary.
  , privateIpAddress :: Core.Maybe Core.Text
    -- ^ The private IPv4 addresses.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PrivateIpAddressSpecification' value with any optional fields omitted.
mkPrivateIpAddressSpecification
    :: PrivateIpAddressSpecification
mkPrivateIpAddressSpecification
  = PrivateIpAddressSpecification'{primary = Core.Nothing,
                                   privateIpAddress = Core.Nothing}

-- | Indicates whether the private IPv4 address is the primary private IPv4 address. Only one IPv4 address can be designated as primary.
--
-- /Note:/ Consider using 'primary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piasPrimary :: Lens.Lens' PrivateIpAddressSpecification (Core.Maybe Core.Bool)
piasPrimary = Lens.field @"primary"
{-# INLINEABLE piasPrimary #-}
{-# DEPRECATED primary "Use generic-lens or generic-optics with 'primary' instead"  #-}

-- | The private IPv4 addresses.
--
-- /Note:/ Consider using 'privateIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piasPrivateIpAddress :: Lens.Lens' PrivateIpAddressSpecification (Core.Maybe Core.Text)
piasPrivateIpAddress = Lens.field @"privateIpAddress"
{-# INLINEABLE piasPrivateIpAddress #-}
{-# DEPRECATED privateIpAddress "Use generic-lens or generic-optics with 'privateIpAddress' instead"  #-}

instance Core.ToQuery PrivateIpAddressSpecification where
        toQuery PrivateIpAddressSpecification{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Primary") primary
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PrivateIpAddress")
                privateIpAddress

instance Core.FromXML PrivateIpAddressSpecification where
        parseXML x
          = PrivateIpAddressSpecification' Core.<$>
              (x Core..@? "primary") Core.<*> x Core..@? "privateIpAddress"
