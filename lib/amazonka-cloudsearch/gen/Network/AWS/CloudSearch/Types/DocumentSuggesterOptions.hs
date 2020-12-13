{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.DocumentSuggesterOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.DocumentSuggesterOptions
  ( DocumentSuggesterOptions (..),

    -- * Smart constructor
    mkDocumentSuggesterOptions,

    -- * Lenses
    dsoSourceField,
    dsoSortExpression,
    dsoFuzzyMatching,
  )
where

import Network.AWS.CloudSearch.Types.SuggesterFuzzyMatching
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Options for a search suggester.
--
-- /See:/ 'mkDocumentSuggesterOptions' smart constructor.
data DocumentSuggesterOptions = DocumentSuggesterOptions'
  { -- | The name of the index field you want to use for suggestions.
    sourceField :: Lude.Text,
    -- | An expression that computes a score for each suggestion to control how they are sorted. The scores are rounded to the nearest integer, with a floor of 0 and a ceiling of 2^31-1. A document's relevance score is not computed for suggestions, so sort expressions cannot reference the @_score@ value. To sort suggestions using a numeric field or existing expression, simply specify the name of the field or expression. If no expression is configured for the suggester, the suggestions are sorted with the closest matches listed first.
    sortExpression :: Lude.Maybe Lude.Text,
    -- | The level of fuzziness allowed when suggesting matches for a string: @none@ , @low@ , or @high@ . With none, the specified string is treated as an exact prefix. With low, suggestions must differ from the specified string by no more than one character. With high, suggestions can differ by up to two characters. The default is none.
    fuzzyMatching :: Lude.Maybe SuggesterFuzzyMatching
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DocumentSuggesterOptions' with the minimum fields required to make a request.
--
-- * 'sourceField' - The name of the index field you want to use for suggestions.
-- * 'sortExpression' - An expression that computes a score for each suggestion to control how they are sorted. The scores are rounded to the nearest integer, with a floor of 0 and a ceiling of 2^31-1. A document's relevance score is not computed for suggestions, so sort expressions cannot reference the @_score@ value. To sort suggestions using a numeric field or existing expression, simply specify the name of the field or expression. If no expression is configured for the suggester, the suggestions are sorted with the closest matches listed first.
-- * 'fuzzyMatching' - The level of fuzziness allowed when suggesting matches for a string: @none@ , @low@ , or @high@ . With none, the specified string is treated as an exact prefix. With low, suggestions must differ from the specified string by no more than one character. With high, suggestions can differ by up to two characters. The default is none.
mkDocumentSuggesterOptions ::
  -- | 'sourceField'
  Lude.Text ->
  DocumentSuggesterOptions
mkDocumentSuggesterOptions pSourceField_ =
  DocumentSuggesterOptions'
    { sourceField = pSourceField_,
      sortExpression = Lude.Nothing,
      fuzzyMatching = Lude.Nothing
    }

-- | The name of the index field you want to use for suggestions.
--
-- /Note:/ Consider using 'sourceField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsoSourceField :: Lens.Lens' DocumentSuggesterOptions Lude.Text
dsoSourceField = Lens.lens (sourceField :: DocumentSuggesterOptions -> Lude.Text) (\s a -> s {sourceField = a} :: DocumentSuggesterOptions)
{-# DEPRECATED dsoSourceField "Use generic-lens or generic-optics with 'sourceField' instead." #-}

-- | An expression that computes a score for each suggestion to control how they are sorted. The scores are rounded to the nearest integer, with a floor of 0 and a ceiling of 2^31-1. A document's relevance score is not computed for suggestions, so sort expressions cannot reference the @_score@ value. To sort suggestions using a numeric field or existing expression, simply specify the name of the field or expression. If no expression is configured for the suggester, the suggestions are sorted with the closest matches listed first.
--
-- /Note:/ Consider using 'sortExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsoSortExpression :: Lens.Lens' DocumentSuggesterOptions (Lude.Maybe Lude.Text)
dsoSortExpression = Lens.lens (sortExpression :: DocumentSuggesterOptions -> Lude.Maybe Lude.Text) (\s a -> s {sortExpression = a} :: DocumentSuggesterOptions)
{-# DEPRECATED dsoSortExpression "Use generic-lens or generic-optics with 'sortExpression' instead." #-}

-- | The level of fuzziness allowed when suggesting matches for a string: @none@ , @low@ , or @high@ . With none, the specified string is treated as an exact prefix. With low, suggestions must differ from the specified string by no more than one character. With high, suggestions can differ by up to two characters. The default is none.
--
-- /Note:/ Consider using 'fuzzyMatching' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsoFuzzyMatching :: Lens.Lens' DocumentSuggesterOptions (Lude.Maybe SuggesterFuzzyMatching)
dsoFuzzyMatching = Lens.lens (fuzzyMatching :: DocumentSuggesterOptions -> Lude.Maybe SuggesterFuzzyMatching) (\s a -> s {fuzzyMatching = a} :: DocumentSuggesterOptions)
{-# DEPRECATED dsoFuzzyMatching "Use generic-lens or generic-optics with 'fuzzyMatching' instead." #-}

instance Lude.FromXML DocumentSuggesterOptions where
  parseXML x =
    DocumentSuggesterOptions'
      Lude.<$> (x Lude..@ "SourceField")
      Lude.<*> (x Lude..@? "SortExpression")
      Lude.<*> (x Lude..@? "FuzzyMatching")

instance Lude.ToQuery DocumentSuggesterOptions where
  toQuery DocumentSuggesterOptions' {..} =
    Lude.mconcat
      [ "SourceField" Lude.=: sourceField,
        "SortExpression" Lude.=: sortExpression,
        "FuzzyMatching" Lude.=: fuzzyMatching
      ]
