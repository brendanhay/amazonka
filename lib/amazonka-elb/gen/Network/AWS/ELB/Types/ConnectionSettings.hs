{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.ConnectionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.ConnectionSettings
  ( ConnectionSettings (..),

    -- * Smart constructor
    mkConnectionSettings,

    -- * Lenses
    csIdleTimeout,
  )
where

import qualified Network.AWS.ELB.Internal as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the @ConnectionSettings@ attribute.
--
-- /See:/ 'mkConnectionSettings' smart constructor.
newtype ConnectionSettings = ConnectionSettings'
  { -- | The time, in seconds, that the connection is allowed to be idle (no data has been sent over the connection) before it is closed by the load balancer.
    idleTimeout :: Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ConnectionSettings' value with any optional fields omitted.
mkConnectionSettings ::
  -- | 'idleTimeout'
  Core.Natural ->
  ConnectionSettings
mkConnectionSettings idleTimeout = ConnectionSettings' {idleTimeout}

-- | The time, in seconds, that the connection is allowed to be idle (no data has been sent over the connection) before it is closed by the load balancer.
--
-- /Note:/ Consider using 'idleTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csIdleTimeout :: Lens.Lens' ConnectionSettings Core.Natural
csIdleTimeout = Lens.field @"idleTimeout"
{-# DEPRECATED csIdleTimeout "Use generic-lens or generic-optics with 'idleTimeout' instead." #-}

instance Core.FromXML ConnectionSettings where
  parseXML x = ConnectionSettings' Core.<$> (x Core..@ "IdleTimeout")
