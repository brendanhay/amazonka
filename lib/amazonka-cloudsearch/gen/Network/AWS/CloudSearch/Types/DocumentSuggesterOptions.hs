{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.DocumentSuggesterOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.DocumentSuggesterOptions where

import Network.AWS.CloudSearch.Types.SuggesterFuzzyMatching
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Options for a search suggester.
--
--
--
-- /See:/ 'documentSuggesterOptions' smart constructor.
data DocumentSuggesterOptions = DocumentSuggesterOptions'
  { _dsoSortExpression ::
      !(Maybe Text),
    _dsoFuzzyMatching ::
      !(Maybe SuggesterFuzzyMatching),
    _dsoSourceField :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DocumentSuggesterOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsoSortExpression' - An expression that computes a score for each suggestion to control how they are sorted. The scores are rounded to the nearest integer, with a floor of 0 and a ceiling of 2^31-1. A document's relevance score is not computed for suggestions, so sort expressions cannot reference the @_score@ value. To sort suggestions using a numeric field or existing expression, simply specify the name of the field or expression. If no expression is configured for the suggester, the suggestions are sorted with the closest matches listed first.
--
-- * 'dsoFuzzyMatching' - The level of fuzziness allowed when suggesting matches for a string: @none@ , @low@ , or @high@ . With none, the specified string is treated as an exact prefix. With low, suggestions must differ from the specified string by no more than one character. With high, suggestions can differ by up to two characters. The default is none.
--
-- * 'dsoSourceField' - The name of the index field you want to use for suggestions.
documentSuggesterOptions ::
  -- | 'dsoSourceField'
  Text ->
  DocumentSuggesterOptions
documentSuggesterOptions pSourceField_ =
  DocumentSuggesterOptions'
    { _dsoSortExpression = Nothing,
      _dsoFuzzyMatching = Nothing,
      _dsoSourceField = pSourceField_
    }

-- | An expression that computes a score for each suggestion to control how they are sorted. The scores are rounded to the nearest integer, with a floor of 0 and a ceiling of 2^31-1. A document's relevance score is not computed for suggestions, so sort expressions cannot reference the @_score@ value. To sort suggestions using a numeric field or existing expression, simply specify the name of the field or expression. If no expression is configured for the suggester, the suggestions are sorted with the closest matches listed first.
dsoSortExpression :: Lens' DocumentSuggesterOptions (Maybe Text)
dsoSortExpression = lens _dsoSortExpression (\s a -> s {_dsoSortExpression = a})

-- | The level of fuzziness allowed when suggesting matches for a string: @none@ , @low@ , or @high@ . With none, the specified string is treated as an exact prefix. With low, suggestions must differ from the specified string by no more than one character. With high, suggestions can differ by up to two characters. The default is none.
dsoFuzzyMatching :: Lens' DocumentSuggesterOptions (Maybe SuggesterFuzzyMatching)
dsoFuzzyMatching = lens _dsoFuzzyMatching (\s a -> s {_dsoFuzzyMatching = a})

-- | The name of the index field you want to use for suggestions.
dsoSourceField :: Lens' DocumentSuggesterOptions Text
dsoSourceField = lens _dsoSourceField (\s a -> s {_dsoSourceField = a})

instance FromXML DocumentSuggesterOptions where
  parseXML x =
    DocumentSuggesterOptions'
      <$> (x .@? "SortExpression")
      <*> (x .@? "FuzzyMatching")
      <*> (x .@ "SourceField")

instance Hashable DocumentSuggesterOptions

instance NFData DocumentSuggesterOptions

instance ToQuery DocumentSuggesterOptions where
  toQuery DocumentSuggesterOptions' {..} =
    mconcat
      [ "SortExpression" =: _dsoSortExpression,
        "FuzzyMatching" =: _dsoFuzzyMatching,
        "SourceField" =: _dsoSourceField
      ]
