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
-- Module      : Amazonka.CloudSearch.Types.DocumentSuggesterOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearch.Types.DocumentSuggesterOptions where

import Amazonka.CloudSearch.Types.SuggesterFuzzyMatching
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromXML DocumentSuggesterOptions where
  parseXML x =
    DocumentSuggesterOptions'
      Prelude.<$> (x Data..@? "FuzzyMatching")
      Prelude.<*> (x Data..@? "SortExpression")
      Prelude.<*> (x Data..@ "SourceField")

instance Prelude.Hashable DocumentSuggesterOptions where
  hashWithSalt _salt DocumentSuggesterOptions' {..} =
    _salt
      `Prelude.hashWithSalt` fuzzyMatching
      `Prelude.hashWithSalt` sortExpression
      `Prelude.hashWithSalt` sourceField

instance Prelude.NFData DocumentSuggesterOptions where
  rnf DocumentSuggesterOptions' {..} =
    Prelude.rnf fuzzyMatching
      `Prelude.seq` Prelude.rnf sortExpression
      `Prelude.seq` Prelude.rnf sourceField

instance Data.ToQuery DocumentSuggesterOptions where
  toQuery DocumentSuggesterOptions' {..} =
    Prelude.mconcat
      [ "FuzzyMatching" Data.=: fuzzyMatching,
        "SortExpression" Data.=: sortExpression,
        "SourceField" Data.=: sourceField
      ]
