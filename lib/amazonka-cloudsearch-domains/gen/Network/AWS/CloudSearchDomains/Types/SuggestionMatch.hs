-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.SuggestionMatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.SuggestionMatch
  ( SuggestionMatch (..),

    -- * Smart constructor
    mkSuggestionMatch,

    -- * Lenses
    smSuggestion,
    smScore,
    smId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An autocomplete suggestion that matches the query string specified in a @SuggestRequest@ .
--
-- /See:/ 'mkSuggestionMatch' smart constructor.
data SuggestionMatch = SuggestionMatch'
  { suggestion ::
      Lude.Maybe Lude.Text,
    score :: Lude.Maybe Lude.Integer,
    id :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SuggestionMatch' with the minimum fields required to make a request.
--
-- * 'id' - The document ID of the suggested document.
-- * 'score' - The relevance score of a suggested match.
-- * 'suggestion' - The string that matches the query string specified in the @SuggestRequest@ .
mkSuggestionMatch ::
  SuggestionMatch
mkSuggestionMatch =
  SuggestionMatch'
    { suggestion = Lude.Nothing,
      score = Lude.Nothing,
      id = Lude.Nothing
    }

-- | The string that matches the query string specified in the @SuggestRequest@ .
--
-- /Note:/ Consider using 'suggestion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smSuggestion :: Lens.Lens' SuggestionMatch (Lude.Maybe Lude.Text)
smSuggestion = Lens.lens (suggestion :: SuggestionMatch -> Lude.Maybe Lude.Text) (\s a -> s {suggestion = a} :: SuggestionMatch)
{-# DEPRECATED smSuggestion "Use generic-lens or generic-optics with 'suggestion' instead." #-}

-- | The relevance score of a suggested match.
--
-- /Note:/ Consider using 'score' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smScore :: Lens.Lens' SuggestionMatch (Lude.Maybe Lude.Integer)
smScore = Lens.lens (score :: SuggestionMatch -> Lude.Maybe Lude.Integer) (\s a -> s {score = a} :: SuggestionMatch)
{-# DEPRECATED smScore "Use generic-lens or generic-optics with 'score' instead." #-}

-- | The document ID of the suggested document.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smId :: Lens.Lens' SuggestionMatch (Lude.Maybe Lude.Text)
smId = Lens.lens (id :: SuggestionMatch -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: SuggestionMatch)
{-# DEPRECATED smId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON SuggestionMatch where
  parseJSON =
    Lude.withObject
      "SuggestionMatch"
      ( \x ->
          SuggestionMatch'
            Lude.<$> (x Lude..:? "suggestion")
            Lude.<*> (x Lude..:? "score")
            Lude.<*> (x Lude..:? "id")
      )
