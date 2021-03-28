{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.Hit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearchDomains.Types.Hit
  ( Hit (..)
  -- * Smart constructor
  , mkHit
  -- * Lenses
  , hExprs
  , hFields
  , hHighlights
  , hId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a document that matches the search request.
--
-- /See:/ 'mkHit' smart constructor.
data Hit = Hit'
  { exprs :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The expressions returned from a document that matches the search request.
  , fields :: Core.Maybe (Core.HashMap Core.Text [Core.Text])
    -- ^ The fields returned from a document that matches the search request.
  , highlights :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The highlights returned from a document that matches the search request.
  , id :: Core.Maybe Core.Text
    -- ^ The document ID of a document that matches the search request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Hit' value with any optional fields omitted.
mkHit
    :: Hit
mkHit
  = Hit'{exprs = Core.Nothing, fields = Core.Nothing,
         highlights = Core.Nothing, id = Core.Nothing}

-- | The expressions returned from a document that matches the search request.
--
-- /Note:/ Consider using 'exprs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hExprs :: Lens.Lens' Hit (Core.Maybe (Core.HashMap Core.Text Core.Text))
hExprs = Lens.field @"exprs"
{-# INLINEABLE hExprs #-}
{-# DEPRECATED exprs "Use generic-lens or generic-optics with 'exprs' instead"  #-}

-- | The fields returned from a document that matches the search request.
--
-- /Note:/ Consider using 'fields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hFields :: Lens.Lens' Hit (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
hFields = Lens.field @"fields"
{-# INLINEABLE hFields #-}
{-# DEPRECATED fields "Use generic-lens or generic-optics with 'fields' instead"  #-}

-- | The highlights returned from a document that matches the search request.
--
-- /Note:/ Consider using 'highlights' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hHighlights :: Lens.Lens' Hit (Core.Maybe (Core.HashMap Core.Text Core.Text))
hHighlights = Lens.field @"highlights"
{-# INLINEABLE hHighlights #-}
{-# DEPRECATED highlights "Use generic-lens or generic-optics with 'highlights' instead"  #-}

-- | The document ID of a document that matches the search request.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hId :: Lens.Lens' Hit (Core.Maybe Core.Text)
hId = Lens.field @"id"
{-# INLINEABLE hId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.FromJSON Hit where
        parseJSON
          = Core.withObject "Hit" Core.$
              \ x ->
                Hit' Core.<$>
                  (x Core..:? "exprs") Core.<*> x Core..:? "fields" Core.<*>
                    x Core..:? "highlights"
                    Core.<*> x Core..:? "id"
