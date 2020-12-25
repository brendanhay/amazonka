{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.Hit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.Hit
  ( Hit (..),

    -- * Smart constructor
    mkHit,

    -- * Lenses
    hExprs,
    hFields,
    hHighlights,
    hId,
  )
where

import qualified Network.AWS.CloudSearchDomains.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a document that matches the search request.
--
-- /See:/ 'mkHit' smart constructor.
data Hit = Hit'
  { -- | The expressions returned from a document that matches the search request.
    exprs :: Core.Maybe (Core.HashMap Types.String Types.String),
    -- | The fields returned from a document that matches the search request.
    fields :: Core.Maybe (Core.HashMap Types.String [Types.String]),
    -- | The highlights returned from a document that matches the search request.
    highlights :: Core.Maybe (Core.HashMap Types.String Types.String),
    -- | The document ID of a document that matches the search request.
    id :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Hit' value with any optional fields omitted.
mkHit ::
  Hit
mkHit =
  Hit'
    { exprs = Core.Nothing,
      fields = Core.Nothing,
      highlights = Core.Nothing,
      id = Core.Nothing
    }

-- | The expressions returned from a document that matches the search request.
--
-- /Note:/ Consider using 'exprs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hExprs :: Lens.Lens' Hit (Core.Maybe (Core.HashMap Types.String Types.String))
hExprs = Lens.field @"exprs"
{-# DEPRECATED hExprs "Use generic-lens or generic-optics with 'exprs' instead." #-}

-- | The fields returned from a document that matches the search request.
--
-- /Note:/ Consider using 'fields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hFields :: Lens.Lens' Hit (Core.Maybe (Core.HashMap Types.String [Types.String]))
hFields = Lens.field @"fields"
{-# DEPRECATED hFields "Use generic-lens or generic-optics with 'fields' instead." #-}

-- | The highlights returned from a document that matches the search request.
--
-- /Note:/ Consider using 'highlights' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hHighlights :: Lens.Lens' Hit (Core.Maybe (Core.HashMap Types.String Types.String))
hHighlights = Lens.field @"highlights"
{-# DEPRECATED hHighlights "Use generic-lens or generic-optics with 'highlights' instead." #-}

-- | The document ID of a document that matches the search request.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hId :: Lens.Lens' Hit (Core.Maybe Types.String)
hId = Lens.field @"id"
{-# DEPRECATED hId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.FromJSON Hit where
  parseJSON =
    Core.withObject "Hit" Core.$
      \x ->
        Hit'
          Core.<$> (x Core..:? "exprs")
          Core.<*> (x Core..:? "fields")
          Core.<*> (x Core..:? "highlights")
          Core.<*> (x Core..:? "id")
