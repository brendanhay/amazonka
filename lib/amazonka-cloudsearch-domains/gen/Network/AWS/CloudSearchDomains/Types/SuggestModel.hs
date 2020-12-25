{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.SuggestModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.SuggestModel
  ( SuggestModel (..),

    -- * Smart constructor
    mkSuggestModel,

    -- * Lenses
    smFound,
    smQuery,
    smSuggestions,
  )
where

import qualified Network.AWS.CloudSearchDomains.Types.String as Types
import qualified Network.AWS.CloudSearchDomains.Types.SuggestionMatch as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Container for the suggestion information returned in a @SuggestResponse@ .
--
-- /See:/ 'mkSuggestModel' smart constructor.
data SuggestModel = SuggestModel'
  { -- | The number of documents that were found to match the query string.
    found :: Core.Maybe Core.Integer,
    -- | The query string specified in the suggest request.
    query :: Core.Maybe Types.String,
    -- | The documents that match the query string.
    suggestions :: Core.Maybe [Types.SuggestionMatch]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SuggestModel' value with any optional fields omitted.
mkSuggestModel ::
  SuggestModel
mkSuggestModel =
  SuggestModel'
    { found = Core.Nothing,
      query = Core.Nothing,
      suggestions = Core.Nothing
    }

-- | The number of documents that were found to match the query string.
--
-- /Note:/ Consider using 'found' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smFound :: Lens.Lens' SuggestModel (Core.Maybe Core.Integer)
smFound = Lens.field @"found"
{-# DEPRECATED smFound "Use generic-lens or generic-optics with 'found' instead." #-}

-- | The query string specified in the suggest request.
--
-- /Note:/ Consider using 'query' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smQuery :: Lens.Lens' SuggestModel (Core.Maybe Types.String)
smQuery = Lens.field @"query"
{-# DEPRECATED smQuery "Use generic-lens or generic-optics with 'query' instead." #-}

-- | The documents that match the query string.
--
-- /Note:/ Consider using 'suggestions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smSuggestions :: Lens.Lens' SuggestModel (Core.Maybe [Types.SuggestionMatch])
smSuggestions = Lens.field @"suggestions"
{-# DEPRECATED smSuggestions "Use generic-lens or generic-optics with 'suggestions' instead." #-}

instance Core.FromJSON SuggestModel where
  parseJSON =
    Core.withObject "SuggestModel" Core.$
      \x ->
        SuggestModel'
          Core.<$> (x Core..:? "found")
          Core.<*> (x Core..:? "query")
          Core.<*> (x Core..:? "suggestions")
