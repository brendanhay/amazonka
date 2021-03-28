{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.CustomerNodeEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.CustomerNodeEndpoint
  ( CustomerNodeEndpoint (..)
  -- * Smart constructor
  , mkCustomerNodeEndpoint
  -- * Lenses
  , cneAddress
  , cnePort
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The endpoint from which data should be migrated.
--
-- /See:/ 'mkCustomerNodeEndpoint' smart constructor.
data CustomerNodeEndpoint = CustomerNodeEndpoint'
  { address :: Core.Maybe Core.Text
    -- ^ The address of the node endpoint
  , port :: Core.Maybe Core.Int
    -- ^ The port of the node endpoint
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CustomerNodeEndpoint' value with any optional fields omitted.
mkCustomerNodeEndpoint
    :: CustomerNodeEndpoint
mkCustomerNodeEndpoint
  = CustomerNodeEndpoint'{address = Core.Nothing,
                          port = Core.Nothing}

-- | The address of the node endpoint
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cneAddress :: Lens.Lens' CustomerNodeEndpoint (Core.Maybe Core.Text)
cneAddress = Lens.field @"address"
{-# INLINEABLE cneAddress #-}
{-# DEPRECATED address "Use generic-lens or generic-optics with 'address' instead"  #-}

-- | The port of the node endpoint
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnePort :: Lens.Lens' CustomerNodeEndpoint (Core.Maybe Core.Int)
cnePort = Lens.field @"port"
{-# INLINEABLE cnePort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

instance Core.ToQuery CustomerNodeEndpoint where
        toQuery CustomerNodeEndpoint{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Address") address
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Port") port
