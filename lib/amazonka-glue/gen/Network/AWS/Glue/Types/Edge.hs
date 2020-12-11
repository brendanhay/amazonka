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
    eSourceId,
    eDestinationId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An edge represents a directed connection between two AWS Glue components that are part of the workflow the edge belongs to.
--
-- /See:/ 'mkEdge' smart constructor.
data Edge = Edge'
  { sourceId :: Lude.Maybe Lude.Text,
    destinationId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Edge' with the minimum fields required to make a request.
--
-- * 'destinationId' - The unique of the node within the workflow where the edge ends.
-- * 'sourceId' - The unique of the node within the workflow where the edge starts.
mkEdge ::
  Edge
mkEdge =
  Edge' {sourceId = Lude.Nothing, destinationId = Lude.Nothing}

-- | The unique of the node within the workflow where the edge starts.
--
-- /Note:/ Consider using 'sourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSourceId :: Lens.Lens' Edge (Lude.Maybe Lude.Text)
eSourceId = Lens.lens (sourceId :: Edge -> Lude.Maybe Lude.Text) (\s a -> s {sourceId = a} :: Edge)
{-# DEPRECATED eSourceId "Use generic-lens or generic-optics with 'sourceId' instead." #-}

-- | The unique of the node within the workflow where the edge ends.
--
-- /Note:/ Consider using 'destinationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDestinationId :: Lens.Lens' Edge (Lude.Maybe Lude.Text)
eDestinationId = Lens.lens (destinationId :: Edge -> Lude.Maybe Lude.Text) (\s a -> s {destinationId = a} :: Edge)
{-# DEPRECATED eDestinationId "Use generic-lens or generic-optics with 'destinationId' instead." #-}

instance Lude.FromJSON Edge where
  parseJSON =
    Lude.withObject
      "Edge"
      ( \x ->
          Edge'
            Lude.<$> (x Lude..:? "SourceId") Lude.<*> (x Lude..:? "DestinationId")
      )
