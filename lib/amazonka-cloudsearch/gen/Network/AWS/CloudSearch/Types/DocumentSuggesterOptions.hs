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
    dsoFuzzyMatching,
    dsoSortExpression,
  )
where

import qualified Network.AWS.CloudSearch.Types.SortExpression as Types
import qualified Network.AWS.CloudSearch.Types.SourceField as Types
import qualified Network.AWS.CloudSearch.Types.SuggesterFuzzyMatching as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Options for a search suggester.
--
-- /See:/ 'mkDocumentSuggesterOptions' smart constructor.
data DocumentSuggesterOptions = DocumentSuggesterOptions'
  { -- | The name of the index field you want to use for suggestions.
    sourceField :: Types.SourceField,
    -- | The level of fuzziness allowed when suggesting matches for a string: @none@ , @low@ , or @high@ . With none, the specified string is treated as an exact prefix. With low, suggestions must differ from the specified string by no more than one character. With high, suggestions can differ by up to two characters. The default is none.
    fuzzyMatching :: Core.Maybe Types.SuggesterFuzzyMatching,
    -- | An expression that computes a score for each suggestion to control how they are sorted. The scores are rounded to the nearest integer, with a floor of 0 and a ceiling of 2^31-1. A document's relevance score is not computed for suggestions, so sort expressions cannot reference the @_score@ value. To sort suggestions using a numeric field or existing expression, simply specify the name of the field or expression. If no expression is configured for the suggester, the suggestions are sorted with the closest matches listed first.
    sortExpression :: Core.Maybe Types.SortExpression
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DocumentSuggesterOptions' value with any optional fields omitted.
mkDocumentSuggesterOptions ::
  -- | 'sourceField'
  Types.SourceField ->
  DocumentSuggesterOptions
mkDocumentSuggesterOptions sourceField =
  DocumentSuggesterOptions'
    { sourceField,
      fuzzyMatching = Core.Nothing,
      sortExpression = Core.Nothing
    }

-- | The name of the index field you want to use for suggestions.
--
-- /Note:/ Consider using 'sourceField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsoSourceField :: Lens.Lens' DocumentSuggesterOptions Types.SourceField
dsoSourceField = Lens.field @"sourceField"
{-# DEPRECATED dsoSourceField "Use generic-lens or generic-optics with 'sourceField' instead." #-}

-- | The level of fuzziness allowed when suggesting matches for a string: @none@ , @low@ , or @high@ . With none, the specified string is treated as an exact prefix. With low, suggestions must differ from the specified string by no more than one character. With high, suggestions can differ by up to two characters. The default is none.
--
-- /Note:/ Consider using 'fuzzyMatching' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsoFuzzyMatching :: Lens.Lens' DocumentSuggesterOptions (Core.Maybe Types.SuggesterFuzzyMatching)
dsoFuzzyMatching = Lens.field @"fuzzyMatching"
{-# DEPRECATED dsoFuzzyMatching "Use generic-lens or generic-optics with 'fuzzyMatching' instead." #-}

-- | An expression that computes a score for each suggestion to control how they are sorted. The scores are rounded to the nearest integer, with a floor of 0 and a ceiling of 2^31-1. A document's relevance score is not computed for suggestions, so sort expressions cannot reference the @_score@ value. To sort suggestions using a numeric field or existing expression, simply specify the name of the field or expression. If no expression is configured for the suggester, the suggestions are sorted with the closest matches listed first.
--
-- /Note:/ Consider using 'sortExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsoSortExpression :: Lens.Lens' DocumentSuggesterOptions (Core.Maybe Types.SortExpression)
dsoSortExpression = Lens.field @"sortExpression"
{-# DEPRECATED dsoSortExpression "Use generic-lens or generic-optics with 'sortExpression' instead." #-}

instance Core.FromXML DocumentSuggesterOptions where
  parseXML x =
    DocumentSuggesterOptions'
      Core.<$> (x Core..@ "SourceField")
      Core.<*> (x Core..@? "FuzzyMatching")
      Core.<*> (x Core..@? "SortExpression")
