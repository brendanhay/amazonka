{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.RouteFilterPrefix
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.RouteFilterPrefix
  ( RouteFilterPrefix (..),

    -- * Smart constructor
    mkRouteFilterPrefix,

    -- * Lenses
    rfpCidr,
  )
where

import qualified Network.AWS.DirectConnect.Types.CIDR as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a route filter prefix that a customer can advertise through Border Gateway Protocol (BGP) over a public virtual interface.
--
-- /See:/ 'mkRouteFilterPrefix' smart constructor.
newtype RouteFilterPrefix = RouteFilterPrefix'
  { -- | The CIDR block for the advertised route. Separate multiple routes using commas. An IPv6 CIDR must use /64 or shorter.
    cidr :: Core.Maybe Types.CIDR
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RouteFilterPrefix' value with any optional fields omitted.
mkRouteFilterPrefix ::
  RouteFilterPrefix
mkRouteFilterPrefix = RouteFilterPrefix' {cidr = Core.Nothing}

-- | The CIDR block for the advertised route. Separate multiple routes using commas. An IPv6 CIDR must use /64 or shorter.
--
-- /Note:/ Consider using 'cidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfpCidr :: Lens.Lens' RouteFilterPrefix (Core.Maybe Types.CIDR)
rfpCidr = Lens.field @"cidr"
{-# DEPRECATED rfpCidr "Use generic-lens or generic-optics with 'cidr' instead." #-}

instance Core.FromJSON RouteFilterPrefix where
  toJSON RouteFilterPrefix {..} =
    Core.object (Core.catMaybes [("cidr" Core..=) Core.<$> cidr])

instance Core.FromJSON RouteFilterPrefix where
  parseJSON =
    Core.withObject "RouteFilterPrefix" Core.$
      \x -> RouteFilterPrefix' Core.<$> (x Core..:? "cidr")
