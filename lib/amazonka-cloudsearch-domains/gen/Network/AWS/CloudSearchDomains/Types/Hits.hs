{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.Hits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.Hits
  ( Hits (..),

    -- * Smart constructor
    mkHits,

    -- * Lenses
    hCursor,
    hFound,
    hHit,
    hStart,
  )
where

import qualified Network.AWS.CloudSearchDomains.Types.Hit as Types
import qualified Network.AWS.CloudSearchDomains.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The collection of documents that match the search request.
--
-- /See:/ 'mkHits' smart constructor.
data Hits = Hits'
  { -- | A cursor that can be used to retrieve the next set of matching documents when you want to page through a large result set.
    cursor :: Core.Maybe Types.String,
    -- | The total number of documents that match the search request.
    found :: Core.Maybe Core.Integer,
    -- | A document that matches the search request.
    hit :: Core.Maybe [Types.Hit],
    -- | The index of the first matching document.
    start :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Hits' value with any optional fields omitted.
mkHits ::
  Hits
mkHits =
  Hits'
    { cursor = Core.Nothing,
      found = Core.Nothing,
      hit = Core.Nothing,
      start = Core.Nothing
    }

-- | A cursor that can be used to retrieve the next set of matching documents when you want to page through a large result set.
--
-- /Note:/ Consider using 'cursor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hCursor :: Lens.Lens' Hits (Core.Maybe Types.String)
hCursor = Lens.field @"cursor"
{-# DEPRECATED hCursor "Use generic-lens or generic-optics with 'cursor' instead." #-}

-- | The total number of documents that match the search request.
--
-- /Note:/ Consider using 'found' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hFound :: Lens.Lens' Hits (Core.Maybe Core.Integer)
hFound = Lens.field @"found"
{-# DEPRECATED hFound "Use generic-lens or generic-optics with 'found' instead." #-}

-- | A document that matches the search request.
--
-- /Note:/ Consider using 'hit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hHit :: Lens.Lens' Hits (Core.Maybe [Types.Hit])
hHit = Lens.field @"hit"
{-# DEPRECATED hHit "Use generic-lens or generic-optics with 'hit' instead." #-}

-- | The index of the first matching document.
--
-- /Note:/ Consider using 'start' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hStart :: Lens.Lens' Hits (Core.Maybe Core.Integer)
hStart = Lens.field @"start"
{-# DEPRECATED hStart "Use generic-lens or generic-optics with 'start' instead." #-}

instance Core.FromJSON Hits where
  parseJSON =
    Core.withObject "Hits" Core.$
      \x ->
        Hits'
          Core.<$> (x Core..:? "cursor")
          Core.<*> (x Core..:? "found")
          Core.<*> (x Core..:? "hit")
          Core.<*> (x Core..:? "start")
