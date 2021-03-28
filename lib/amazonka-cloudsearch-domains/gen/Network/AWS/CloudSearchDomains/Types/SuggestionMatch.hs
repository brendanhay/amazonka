{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.SuggestionMatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearchDomains.Types.SuggestionMatch
  ( SuggestionMatch (..)
  -- * Smart constructor
  , mkSuggestionMatch
  -- * Lenses
  , smId
  , smScore
  , smSuggestion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An autocomplete suggestion that matches the query string specified in a @SuggestRequest@ . 
--
-- /See:/ 'mkSuggestionMatch' smart constructor.
data SuggestionMatch = SuggestionMatch'
  { id :: Core.Maybe Core.Text
    -- ^ The document ID of the suggested document.
  , score :: Core.Maybe Core.Integer
    -- ^ The relevance score of a suggested match.
  , suggestion :: Core.Maybe Core.Text
    -- ^ The string that matches the query string specified in the @SuggestRequest@ . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SuggestionMatch' value with any optional fields omitted.
mkSuggestionMatch
    :: SuggestionMatch
mkSuggestionMatch
  = SuggestionMatch'{id = Core.Nothing, score = Core.Nothing,
                     suggestion = Core.Nothing}

-- | The document ID of the suggested document.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smId :: Lens.Lens' SuggestionMatch (Core.Maybe Core.Text)
smId = Lens.field @"id"
{-# INLINEABLE smId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The relevance score of a suggested match.
--
-- /Note:/ Consider using 'score' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smScore :: Lens.Lens' SuggestionMatch (Core.Maybe Core.Integer)
smScore = Lens.field @"score"
{-# INLINEABLE smScore #-}
{-# DEPRECATED score "Use generic-lens or generic-optics with 'score' instead"  #-}

-- | The string that matches the query string specified in the @SuggestRequest@ . 
--
-- /Note:/ Consider using 'suggestion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smSuggestion :: Lens.Lens' SuggestionMatch (Core.Maybe Core.Text)
smSuggestion = Lens.field @"suggestion"
{-# INLINEABLE smSuggestion #-}
{-# DEPRECATED suggestion "Use generic-lens or generic-optics with 'suggestion' instead"  #-}

instance Core.FromJSON SuggestionMatch where
        parseJSON
          = Core.withObject "SuggestionMatch" Core.$
              \ x ->
                SuggestionMatch' Core.<$>
                  (x Core..:? "id") Core.<*> x Core..:? "score" Core.<*>
                    x Core..:? "suggestion"
