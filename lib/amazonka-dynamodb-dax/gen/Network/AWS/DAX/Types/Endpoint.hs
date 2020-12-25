{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.Endpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.Endpoint
  ( Endpoint (..),

    -- * Smart constructor
    mkEndpoint,

    -- * Lenses
    eAddress,
    ePort,
  )
where

import qualified Network.AWS.DAX.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the information required for client programs to connect to the configuration endpoint for a DAX cluster, or to an individual node within the cluster.
--
-- /See:/ 'mkEndpoint' smart constructor.
data Endpoint = Endpoint'
  { -- | The DNS hostname of the endpoint.
    address :: Core.Maybe Types.String,
    -- | The port number that applications should use to connect to the endpoint.
    port :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Endpoint' value with any optional fields omitted.
mkEndpoint ::
  Endpoint
mkEndpoint = Endpoint' {address = Core.Nothing, port = Core.Nothing}

-- | The DNS hostname of the endpoint.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAddress :: Lens.Lens' Endpoint (Core.Maybe Types.String)
eAddress = Lens.field @"address"
{-# DEPRECATED eAddress "Use generic-lens or generic-optics with 'address' instead." #-}

-- | The port number that applications should use to connect to the endpoint.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ePort :: Lens.Lens' Endpoint (Core.Maybe Core.Int)
ePort = Lens.field @"port"
{-# DEPRECATED ePort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Core.FromJSON Endpoint where
  parseJSON =
    Core.withObject "Endpoint" Core.$
      \x ->
        Endpoint'
          Core.<$> (x Core..:? "Address") Core.<*> (x Core..:? "Port")
