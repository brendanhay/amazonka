{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Edge
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Edge
  ( Edge (..),

    -- * Smart constructor
    mkEdge,

    -- * Lenses
    eDestinationId,
    eSourceId,
  )
where

import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An edge represents a directed connection between two AWS Glue components that are part of the workflow the edge belongs to.
--
-- /See:/ 'mkEdge' smart constructor.
data Edge = Edge'
  { -- | The unique of the node within the workflow where the edge ends.
    destinationId :: Core.Maybe Types.NameString,
    -- | The unique of the node within the workflow where the edge starts.
    sourceId :: Core.Maybe Types.NameString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Edge' value with any optional fields omitted.
mkEdge ::
  Edge
mkEdge =
  Edge' {destinationId = Core.Nothing, sourceId = Core.Nothing}

-- | The unique of the node within the workflow where the edge ends.
--
-- /Note:/ Consider using 'destinationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDestinationId :: Lens.Lens' Edge (Core.Maybe Types.NameString)
eDestinationId = Lens.field @"destinationId"
{-# DEPRECATED eDestinationId "Use generic-lens or generic-optics with 'destinationId' instead." #-}

-- | The unique of the node within the workflow where the edge starts.
--
-- /Note:/ Consider using 'sourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSourceId :: Lens.Lens' Edge (Core.Maybe Types.NameString)
eSourceId = Lens.field @"sourceId"
{-# DEPRECATED eSourceId "Use generic-lens or generic-optics with 'sourceId' instead." #-}

instance Core.FromJSON Edge where
  parseJSON =
    Core.withObject "Edge" Core.$
      \x ->
        Edge'
          Core.<$> (x Core..:? "DestinationId") Core.<*> (x Core..:? "SourceId")
