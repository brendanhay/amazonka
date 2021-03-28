{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.Connections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.Connections
  ( Connections (..)
  -- * Smart constructor
  , mkConnections
  -- * Lenses
  , cConnections
  ) where

import qualified Network.AWS.DirectConnect.Types.Connection as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /See:/ 'mkConnections' smart constructor.
newtype Connections = Connections'
  { connections :: Core.Maybe [Types.Connection]
    -- ^ The connections.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype Core.NFData

-- | Creates a 'Connections' value with any optional fields omitted.
mkConnections
    :: Connections
mkConnections = Connections'{connections = Core.Nothing}

-- | The connections.
--
-- /Note:/ Consider using 'connections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cConnections :: Lens.Lens' Connections (Core.Maybe [Types.Connection])
cConnections = Lens.field @"connections"
{-# INLINEABLE cConnections #-}
{-# DEPRECATED connections "Use generic-lens or generic-optics with 'connections' instead"  #-}

instance Core.FromJSON Connections where
        parseJSON
          = Core.withObject "Connections" Core.$
              \ x -> Connections' Core.<$> (x Core..:? "connections")
