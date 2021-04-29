{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.DocumentSuggesterOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.DocumentSuggesterOptions where

import Network.AWS.CloudSearch.Types.SuggesterFuzzyMatching
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Options for a search suggester.
--
-- /See:/ 'newDocumentSuggesterOptions' smart constructor.
data DocumentSuggesterOptions = DocumentSuggesterOptions'
  { -- | The level of fuzziness allowed when suggesting matches for a string:
    -- @none@, @low@, or @high@. With none, the specified string is treated as
    -- an exact prefix. With low, suggestions must differ from the specified
    -- string by no more than one character. With high, suggestions can differ
    -- by up to two characters. The default is none.
    fuzzyMatching :: Prelude.Maybe SuggesterFuzzyMatching,
    -- | An expression that computes a score for each suggestion to control how
    -- they are sorted. The scores are rounded to the nearest integer, with a
    -- floor of 0 and a ceiling of 2^31-1. A document\'s relevance score is not
    -- computed for suggestions, so sort expressions cannot reference the
    -- @_score@ value. To sort suggestions using a numeric field or existing
    -- expression, simply specify the name of the field or expression. If no
    -- expression is configured for the suggester, the suggestions are sorted
    -- with the closest matches listed first.
    sortExpression :: Prelude.Maybe Prelude.Text,
    -- | The name of the index field you want to use for suggestions.
    sourceField :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DocumentSuggesterOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fuzzyMatching', 'documentSuggesterOptions_fuzzyMatching' - The level of fuzziness allowed when suggesting matches for a string:
-- @none@, @low@, or @high@. With none, the specified string is treated as
-- an exact prefix. With low, suggestions must differ from the specified
-- string by no more than one character. With high, suggestions can differ
-- by up to two characters. The default is none.
--
-- 'sortExpression', 'documentSuggesterOptions_sortExpression' - An expression that computes a score for each suggestion to control how
-- they are sorted. The scores are rounded to the nearest integer, with a
-- floor of 0 and a ceiling of 2^31-1. A document\'s relevance score is not
-- computed for suggestions, so sort expressions cannot reference the
-- @_score@ value. To sort suggestions using a numeric field or existing
-- expression, simply specify the name of the field or expression. If no
-- expression is configured for the suggester, the suggestions are sorted
-- with the closest matches listed first.
--
-- 'sourceField', 'documentSuggesterOptions_sourceField' - The name of the index field you want to use for suggestions.
newDocumentSuggesterOptions ::
  -- | 'sourceField'
  Prelude.Text ->
  DocumentSuggesterOptions
newDocumentSuggesterOptions pSourceField_ =
  DocumentSuggesterOptions'
    { fuzzyMatching =
        Prelude.Nothing,
      sortExpression = Prelude.Nothing,
      sourceField = pSourceField_
    }

-- | The level of fuzziness allowed when suggesting matches for a string:
-- @none@, @low@, or @high@. With none, the specified string is treated as
-- an exact prefix. With low, suggestions must differ from the specified
-- string by no more than one character. With high, suggestions can differ
-- by up to two characters. The default is none.
documentSuggesterOptions_fuzzyMatching :: Lens.Lens' DocumentSuggesterOptions (Prelude.Maybe SuggesterFuzzyMatching)
documentSuggesterOptions_fuzzyMatching = Lens.lens (\DocumentSuggesterOptions' {fuzzyMatching} -> fuzzyMatching) (\s@DocumentSuggesterOptions' {} a -> s {fuzzyMatching = a} :: DocumentSuggesterOptions)

-- | An expression that computes a score for each suggestion to control how
-- they are sorted. The scores are rounded to the nearest integer, with a
-- floor of 0 and a ceiling of 2^31-1. A document\'s relevance score is not
-- computed for suggestions, so sort expressions cannot reference the
-- @_score@ value. To sort suggestions using a numeric field or existing
-- expression, simply specify the name of the field or expression. If no
-- expression is configured for the suggester, the suggestions are sorted
-- with the closest matches listed first.
documentSuggesterOptions_sortExpression :: Lens.Lens' DocumentSuggesterOptions (Prelude.Maybe Prelude.Text)
documentSuggesterOptions_sortExpression = Lens.lens (\DocumentSuggesterOptions' {sortExpression} -> sortExpression) (\s@DocumentSuggesterOptions' {} a -> s {sortExpression = a} :: DocumentSuggesterOptions)

-- | The name of the index field you want to use for suggestions.
documentSuggesterOptions_sourceField :: Lens.Lens' DocumentSuggesterOptions Prelude.Text
documentSuggesterOptions_sourceField = Lens.lens (\DocumentSuggesterOptions' {sourceField} -> sourceField) (\s@DocumentSuggesterOptions' {} a -> s {sourceField = a} :: DocumentSuggesterOptions)

instance Prelude.FromXML DocumentSuggesterOptions where
  parseXML x =
    DocumentSuggesterOptions'
      Prelude.<$> (x Prelude..@? "FuzzyMatching")
      Prelude.<*> (x Prelude..@? "SortExpression")
      Prelude.<*> (x Prelude..@ "SourceField")

instance Prelude.Hashable DocumentSuggesterOptions

instance Prelude.NFData DocumentSuggesterOptions

instance Prelude.ToQuery DocumentSuggesterOptions where
  toQuery DocumentSuggesterOptions' {..} =
    Prelude.mconcat
      [ "FuzzyMatching" Prelude.=: fuzzyMatching,
        "SortExpression" Prelude.=: sortExpression,
        "SourceField" Prelude.=: sourceField
      ]
