{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.ConnectionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELB.Types.ConnectionSettings
  ( ConnectionSettings (..)
  -- * Smart constructor
  , mkConnectionSettings
  -- * Lenses
  , csIdleTimeout
  ) where

import qualified Network.AWS.ELB.Internal as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the @ConnectionSettings@ attribute.
--
-- /See:/ 'mkConnectionSettings' smart constructor.
newtype ConnectionSettings = ConnectionSettings'
  { idleTimeout :: Core.Natural
    -- ^ The time, in seconds, that the connection is allowed to be idle (no data has been sent over the connection) before it is closed by the load balancer.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ConnectionSettings' value with any optional fields omitted.
mkConnectionSettings
    :: Core.Natural -- ^ 'idleTimeout'
    -> ConnectionSettings
mkConnectionSettings idleTimeout = ConnectionSettings'{idleTimeout}

-- | The time, in seconds, that the connection is allowed to be idle (no data has been sent over the connection) before it is closed by the load balancer.
--
-- /Note:/ Consider using 'idleTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csIdleTimeout :: Lens.Lens' ConnectionSettings Core.Natural
csIdleTimeout = Lens.field @"idleTimeout"
{-# INLINEABLE csIdleTimeout #-}
{-# DEPRECATED idleTimeout "Use generic-lens or generic-optics with 'idleTimeout' instead"  #-}

instance Core.ToQuery ConnectionSettings where
        toQuery ConnectionSettings{..}
          = Core.toQueryPair "IdleTimeout" idleTimeout

instance Core.FromXML ConnectionSettings where
        parseXML x = ConnectionSettings' Core.<$> (x Core..@ "IdleTimeout")
