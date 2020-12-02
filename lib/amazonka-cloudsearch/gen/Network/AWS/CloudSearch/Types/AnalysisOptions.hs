{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.AnalysisOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.AnalysisOptions where

import Network.AWS.CloudSearch.Types.AlgorithmicStemming
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Synonyms, stopwords, and stemming options for an analysis scheme. Includes tokenization dictionary for Japanese.
--
--
--
-- /See:/ 'analysisOptions' smart constructor.
data AnalysisOptions = AnalysisOptions'
  { _aoAlgorithmicStemming ::
      !(Maybe AlgorithmicStemming),
    _aoStopwords :: !(Maybe Text),
    _aoJapaneseTokenizationDictionary :: !(Maybe Text),
    _aoSynonyms :: !(Maybe Text),
    _aoStemmingDictionary :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AnalysisOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aoAlgorithmicStemming' - The level of algorithmic stemming to perform: @none@ , @minimal@ , @light@ , or @full@ . The available levels vary depending on the language. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/text-processing.html#text-processing-settings Language Specific Text Processing Settings> in the /Amazon CloudSearch Developer Guide/
--
-- * 'aoStopwords' - A JSON array of terms to ignore during indexing and searching. For example, @["a", "an", "the", "of"]@ . The stopwords dictionary must explicitly list each word you want to ignore. Wildcards and regular expressions are not supported.
--
-- * 'aoJapaneseTokenizationDictionary' - A JSON array that contains a collection of terms, tokens, readings and part of speech for Japanese Tokenizaiton. The Japanese tokenization dictionary enables you to override the default tokenization for selected terms. This is only valid for Japanese language fields.
--
-- * 'aoSynonyms' - A JSON object that defines synonym groups and aliases. A synonym group is an array of arrays, where each sub-array is a group of terms where each term in the group is considered a synonym of every other term in the group. The aliases value is an object that contains a collection of string:value pairs where the string specifies a term and the array of values specifies each of the aliases for that term. An alias is considered a synonym of the specified term, but the term is not considered a synonym of the alias. For more information about specifying synonyms, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-analysis-schemes.html#synonyms Synonyms> in the /Amazon CloudSearch Developer Guide/ .
--
-- * 'aoStemmingDictionary' - A JSON object that contains a collection of string:value pairs that each map a term to its stem. For example, @{"term1": "stem1", "term2": "stem2", "term3": "stem3"}@ . The stemming dictionary is applied in addition to any algorithmic stemming. This enables you to override the results of the algorithmic stemming to correct specific cases of overstemming or understemming. The maximum size of a stemming dictionary is 500 KB.
analysisOptions ::
  AnalysisOptions
analysisOptions =
  AnalysisOptions'
    { _aoAlgorithmicStemming = Nothing,
      _aoStopwords = Nothing,
      _aoJapaneseTokenizationDictionary = Nothing,
      _aoSynonyms = Nothing,
      _aoStemmingDictionary = Nothing
    }

-- | The level of algorithmic stemming to perform: @none@ , @minimal@ , @light@ , or @full@ . The available levels vary depending on the language. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/text-processing.html#text-processing-settings Language Specific Text Processing Settings> in the /Amazon CloudSearch Developer Guide/
aoAlgorithmicStemming :: Lens' AnalysisOptions (Maybe AlgorithmicStemming)
aoAlgorithmicStemming = lens _aoAlgorithmicStemming (\s a -> s {_aoAlgorithmicStemming = a})

-- | A JSON array of terms to ignore during indexing and searching. For example, @["a", "an", "the", "of"]@ . The stopwords dictionary must explicitly list each word you want to ignore. Wildcards and regular expressions are not supported.
aoStopwords :: Lens' AnalysisOptions (Maybe Text)
aoStopwords = lens _aoStopwords (\s a -> s {_aoStopwords = a})

-- | A JSON array that contains a collection of terms, tokens, readings and part of speech for Japanese Tokenizaiton. The Japanese tokenization dictionary enables you to override the default tokenization for selected terms. This is only valid for Japanese language fields.
aoJapaneseTokenizationDictionary :: Lens' AnalysisOptions (Maybe Text)
aoJapaneseTokenizationDictionary = lens _aoJapaneseTokenizationDictionary (\s a -> s {_aoJapaneseTokenizationDictionary = a})

-- | A JSON object that defines synonym groups and aliases. A synonym group is an array of arrays, where each sub-array is a group of terms where each term in the group is considered a synonym of every other term in the group. The aliases value is an object that contains a collection of string:value pairs where the string specifies a term and the array of values specifies each of the aliases for that term. An alias is considered a synonym of the specified term, but the term is not considered a synonym of the alias. For more information about specifying synonyms, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-analysis-schemes.html#synonyms Synonyms> in the /Amazon CloudSearch Developer Guide/ .
aoSynonyms :: Lens' AnalysisOptions (Maybe Text)
aoSynonyms = lens _aoSynonyms (\s a -> s {_aoSynonyms = a})

-- | A JSON object that contains a collection of string:value pairs that each map a term to its stem. For example, @{"term1": "stem1", "term2": "stem2", "term3": "stem3"}@ . The stemming dictionary is applied in addition to any algorithmic stemming. This enables you to override the results of the algorithmic stemming to correct specific cases of overstemming or understemming. The maximum size of a stemming dictionary is 500 KB.
aoStemmingDictionary :: Lens' AnalysisOptions (Maybe Text)
aoStemmingDictionary = lens _aoStemmingDictionary (\s a -> s {_aoStemmingDictionary = a})

instance FromXML AnalysisOptions where
  parseXML x =
    AnalysisOptions'
      <$> (x .@? "AlgorithmicStemming")
      <*> (x .@? "Stopwords")
      <*> (x .@? "JapaneseTokenizationDictionary")
      <*> (x .@? "Synonyms")
      <*> (x .@? "StemmingDictionary")

instance Hashable AnalysisOptions

instance NFData AnalysisOptions

instance ToQuery AnalysisOptions where
  toQuery AnalysisOptions' {..} =
    mconcat
      [ "AlgorithmicStemming" =: _aoAlgorithmicStemming,
        "Stopwords" =: _aoStopwords,
        "JapaneseTokenizationDictionary"
          =: _aoJapaneseTokenizationDictionary,
        "Synonyms" =: _aoSynonyms,
        "StemmingDictionary" =: _aoStemmingDictionary
      ]
