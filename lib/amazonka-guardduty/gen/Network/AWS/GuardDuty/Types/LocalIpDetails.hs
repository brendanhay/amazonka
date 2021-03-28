{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.LocalIpDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.LocalIpDetails
  ( LocalIpDetails (..)
  -- * Smart constructor
  , mkLocalIpDetails
  -- * Lenses
  , lidIpAddressV4
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the local IP address of the connection.
--
-- /See:/ 'mkLocalIpDetails' smart constructor.
newtype LocalIpDetails = LocalIpDetails'
  { ipAddressV4 :: Core.Maybe Core.Text
    -- ^ The IPv4 local address of the connection.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'LocalIpDetails' value with any optional fields omitted.
mkLocalIpDetails
    :: LocalIpDetails
mkLocalIpDetails = LocalIpDetails'{ipAddressV4 = Core.Nothing}

-- | The IPv4 local address of the connection.
--
-- /Note:/ Consider using 'ipAddressV4' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lidIpAddressV4 :: Lens.Lens' LocalIpDetails (Core.Maybe Core.Text)
lidIpAddressV4 = Lens.field @"ipAddressV4"
{-# INLINEABLE lidIpAddressV4 #-}
{-# DEPRECATED ipAddressV4 "Use generic-lens or generic-optics with 'ipAddressV4' instead"  #-}

instance Core.FromJSON LocalIpDetails where
        parseJSON
          = Core.withObject "LocalIpDetails" Core.$
              \ x -> LocalIpDetails' Core.<$> (x Core..:? "ipAddressV4")
