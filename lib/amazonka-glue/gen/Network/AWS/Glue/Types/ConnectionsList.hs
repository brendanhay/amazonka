{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ConnectionsList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.ConnectionsList
  ( ConnectionsList (..)
  -- * Smart constructor
  , mkConnectionsList
  -- * Lenses
  , clConnections
  ) where

import qualified Network.AWS.Glue.Types.GenericString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the connections used by a job.
--
-- /See:/ 'mkConnectionsList' smart constructor.
newtype ConnectionsList = ConnectionsList'
  { connections :: Core.Maybe [Types.GenericString]
    -- ^ A list of connections used by the job.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ConnectionsList' value with any optional fields omitted.
mkConnectionsList
    :: ConnectionsList
mkConnectionsList = ConnectionsList'{connections = Core.Nothing}

-- | A list of connections used by the job.
--
-- /Note:/ Consider using 'connections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clConnections :: Lens.Lens' ConnectionsList (Core.Maybe [Types.GenericString])
clConnections = Lens.field @"connections"
{-# INLINEABLE clConnections #-}
{-# DEPRECATED connections "Use generic-lens or generic-optics with 'connections' instead"  #-}

instance Core.FromJSON ConnectionsList where
        toJSON ConnectionsList{..}
          = Core.object
              (Core.catMaybes [("Connections" Core..=) Core.<$> connections])

instance Core.FromJSON ConnectionsList where
        parseJSON
          = Core.withObject "ConnectionsList" Core.$
              \ x -> ConnectionsList' Core.<$> (x Core..:? "Connections")
