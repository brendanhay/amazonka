{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.Endpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.Endpoint
  ( Endpoint (..),

    -- * Smart constructor
    mkEndpoint,

    -- * Lenses
    eAddress,
    ePort,
  )
where

import qualified Network.AWS.ElastiCache.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the information required for client programs to connect to a cache node.
--
-- /See:/ 'mkEndpoint' smart constructor.
data Endpoint = Endpoint'
  { -- | The DNS hostname of the cache node.
    address :: Core.Maybe Types.String,
    -- | The port number that the cache engine is listening on.
    port :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Endpoint' value with any optional fields omitted.
mkEndpoint ::
  Endpoint
mkEndpoint = Endpoint' {address = Core.Nothing, port = Core.Nothing}

-- | The DNS hostname of the cache node.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAddress :: Lens.Lens' Endpoint (Core.Maybe Types.String)
eAddress = Lens.field @"address"
{-# DEPRECATED eAddress "Use generic-lens or generic-optics with 'address' instead." #-}

-- | The port number that the cache engine is listening on.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ePort :: Lens.Lens' Endpoint (Core.Maybe Core.Int)
ePort = Lens.field @"port"
{-# DEPRECATED ePort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Core.FromXML Endpoint where
  parseXML x =
    Endpoint'
      Core.<$> (x Core..@? "Address") Core.<*> (x Core..@? "Port")
