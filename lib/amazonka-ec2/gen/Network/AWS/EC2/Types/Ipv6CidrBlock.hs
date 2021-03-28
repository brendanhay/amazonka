{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Ipv6CidrBlock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.Ipv6CidrBlock
  ( Ipv6CidrBlock (..)
  -- * Smart constructor
  , mkIpv6CidrBlock
  -- * Lenses
  , icbIpv6CidrBlock
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an IPv6 CIDR block.
--
-- /See:/ 'mkIpv6CidrBlock' smart constructor.
newtype Ipv6CidrBlock = Ipv6CidrBlock'
  { ipv6CidrBlock :: Core.Maybe Core.Text
    -- ^ The IPv6 CIDR block.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Ipv6CidrBlock' value with any optional fields omitted.
mkIpv6CidrBlock
    :: Ipv6CidrBlock
mkIpv6CidrBlock = Ipv6CidrBlock'{ipv6CidrBlock = Core.Nothing}

-- | The IPv6 CIDR block.
--
-- /Note:/ Consider using 'ipv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icbIpv6CidrBlock :: Lens.Lens' Ipv6CidrBlock (Core.Maybe Core.Text)
icbIpv6CidrBlock = Lens.field @"ipv6CidrBlock"
{-# INLINEABLE icbIpv6CidrBlock #-}
{-# DEPRECATED ipv6CidrBlock "Use generic-lens or generic-optics with 'ipv6CidrBlock' instead"  #-}

instance Core.FromXML Ipv6CidrBlock where
        parseXML x = Ipv6CidrBlock' Core.<$> (x Core..@? "ipv6CidrBlock")
