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
    smSuggestions,
    smQuery,
  )
where

import Network.AWS.CloudSearchDomains.Types.SuggestionMatch
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Container for the suggestion information returned in a @SuggestResponse@ .
--
-- /See:/ 'mkSuggestModel' smart constructor.
data SuggestModel = SuggestModel'
  { found :: Lude.Maybe Lude.Integer,
    suggestions :: Lude.Maybe [SuggestionMatch],
    query :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SuggestModel' with the minimum fields required to make a request.
--
-- * 'found' - The number of documents that were found to match the query string.
-- * 'query' - The query string specified in the suggest request.
-- * 'suggestions' - The documents that match the query string.
mkSuggestModel ::
  SuggestModel
mkSuggestModel =
  SuggestModel'
    { found = Lude.Nothing,
      suggestions = Lude.Nothing,
      query = Lude.Nothing
    }

-- | The number of documents that were found to match the query string.
--
-- /Note:/ Consider using 'found' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smFound :: Lens.Lens' SuggestModel (Lude.Maybe Lude.Integer)
smFound = Lens.lens (found :: SuggestModel -> Lude.Maybe Lude.Integer) (\s a -> s {found = a} :: SuggestModel)
{-# DEPRECATED smFound "Use generic-lens or generic-optics with 'found' instead." #-}

-- | The documents that match the query string.
--
-- /Note:/ Consider using 'suggestions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smSuggestions :: Lens.Lens' SuggestModel (Lude.Maybe [SuggestionMatch])
smSuggestions = Lens.lens (suggestions :: SuggestModel -> Lude.Maybe [SuggestionMatch]) (\s a -> s {suggestions = a} :: SuggestModel)
{-# DEPRECATED smSuggestions "Use generic-lens or generic-optics with 'suggestions' instead." #-}

-- | The query string specified in the suggest request.
--
-- /Note:/ Consider using 'query' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smQuery :: Lens.Lens' SuggestModel (Lude.Maybe Lude.Text)
smQuery = Lens.lens (query :: SuggestModel -> Lude.Maybe Lude.Text) (\s a -> s {query = a} :: SuggestModel)
{-# DEPRECATED smQuery "Use generic-lens or generic-optics with 'query' instead." #-}

instance Lude.FromJSON SuggestModel where
  parseJSON =
    Lude.withObject
      "SuggestModel"
      ( \x ->
          SuggestModel'
            Lude.<$> (x Lude..:? "found")
            Lude.<*> (x Lude..:? "suggestions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "query")
      )
