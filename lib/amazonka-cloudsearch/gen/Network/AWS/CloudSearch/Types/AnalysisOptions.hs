-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.AnalysisOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.AnalysisOptions
  ( AnalysisOptions (..),

    -- * Smart constructor
    mkAnalysisOptions,

    -- * Lenses
    aoAlgorithmicStemming,
    aoStopwords,
    aoJapaneseTokenizationDictionary,
    aoSynonyms,
    aoStemmingDictionary,
  )
where

import Network.AWS.CloudSearch.Types.AlgorithmicStemming
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Synonyms, stopwords, and stemming options for an analysis scheme. Includes tokenization dictionary for Japanese.
--
-- /See:/ 'mkAnalysisOptions' smart constructor.
data AnalysisOptions = AnalysisOptions'
  { algorithmicStemming ::
      Lude.Maybe AlgorithmicStemming,
    stopwords :: Lude.Maybe Lude.Text,
    japaneseTokenizationDictionary :: Lude.Maybe Lude.Text,
    synonyms :: Lude.Maybe Lude.Text,
    stemmingDictionary :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AnalysisOptions' with the minimum fields required to make a request.
--
-- * 'algorithmicStemming' - The level of algorithmic stemming to perform: @none@ , @minimal@ , @light@ , or @full@ . The available levels vary depending on the language. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/text-processing.html#text-processing-settings Language Specific Text Processing Settings> in the /Amazon CloudSearch Developer Guide/
-- * 'japaneseTokenizationDictionary' - A JSON array that contains a collection of terms, tokens, readings and part of speech for Japanese Tokenizaiton. The Japanese tokenization dictionary enables you to override the default tokenization for selected terms. This is only valid for Japanese language fields.
-- * 'stemmingDictionary' - A JSON object that contains a collection of string:value pairs that each map a term to its stem. For example, @{"term1": "stem1", "term2": "stem2", "term3": "stem3"}@ . The stemming dictionary is applied in addition to any algorithmic stemming. This enables you to override the results of the algorithmic stemming to correct specific cases of overstemming or understemming. The maximum size of a stemming dictionary is 500 KB.
-- * 'stopwords' - A JSON array of terms to ignore during indexing and searching. For example, @["a", "an", "the", "of"]@ . The stopwords dictionary must explicitly list each word you want to ignore. Wildcards and regular expressions are not supported.
-- * 'synonyms' - A JSON object that defines synonym groups and aliases. A synonym group is an array of arrays, where each sub-array is a group of terms where each term in the group is considered a synonym of every other term in the group. The aliases value is an object that contains a collection of string:value pairs where the string specifies a term and the array of values specifies each of the aliases for that term. An alias is considered a synonym of the specified term, but the term is not considered a synonym of the alias. For more information about specifying synonyms, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-analysis-schemes.html#synonyms Synonyms> in the /Amazon CloudSearch Developer Guide/ .
mkAnalysisOptions ::
  AnalysisOptions
mkAnalysisOptions =
  AnalysisOptions'
    { algorithmicStemming = Lude.Nothing,
      stopwords = Lude.Nothing,
      japaneseTokenizationDictionary = Lude.Nothing,
      synonyms = Lude.Nothing,
      stemmingDictionary = Lude.Nothing
    }

-- | The level of algorithmic stemming to perform: @none@ , @minimal@ , @light@ , or @full@ . The available levels vary depending on the language. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/text-processing.html#text-processing-settings Language Specific Text Processing Settings> in the /Amazon CloudSearch Developer Guide/
--
-- /Note:/ Consider using 'algorithmicStemming' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoAlgorithmicStemming :: Lens.Lens' AnalysisOptions (Lude.Maybe AlgorithmicStemming)
aoAlgorithmicStemming = Lens.lens (algorithmicStemming :: AnalysisOptions -> Lude.Maybe AlgorithmicStemming) (\s a -> s {algorithmicStemming = a} :: AnalysisOptions)
{-# DEPRECATED aoAlgorithmicStemming "Use generic-lens or generic-optics with 'algorithmicStemming' instead." #-}

-- | A JSON array of terms to ignore during indexing and searching. For example, @["a", "an", "the", "of"]@ . The stopwords dictionary must explicitly list each word you want to ignore. Wildcards and regular expressions are not supported.
--
-- /Note:/ Consider using 'stopwords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoStopwords :: Lens.Lens' AnalysisOptions (Lude.Maybe Lude.Text)
aoStopwords = Lens.lens (stopwords :: AnalysisOptions -> Lude.Maybe Lude.Text) (\s a -> s {stopwords = a} :: AnalysisOptions)
{-# DEPRECATED aoStopwords "Use generic-lens or generic-optics with 'stopwords' instead." #-}

-- | A JSON array that contains a collection of terms, tokens, readings and part of speech for Japanese Tokenizaiton. The Japanese tokenization dictionary enables you to override the default tokenization for selected terms. This is only valid for Japanese language fields.
--
-- /Note:/ Consider using 'japaneseTokenizationDictionary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoJapaneseTokenizationDictionary :: Lens.Lens' AnalysisOptions (Lude.Maybe Lude.Text)
aoJapaneseTokenizationDictionary = Lens.lens (japaneseTokenizationDictionary :: AnalysisOptions -> Lude.Maybe Lude.Text) (\s a -> s {japaneseTokenizationDictionary = a} :: AnalysisOptions)
{-# DEPRECATED aoJapaneseTokenizationDictionary "Use generic-lens or generic-optics with 'japaneseTokenizationDictionary' instead." #-}

-- | A JSON object that defines synonym groups and aliases. A synonym group is an array of arrays, where each sub-array is a group of terms where each term in the group is considered a synonym of every other term in the group. The aliases value is an object that contains a collection of string:value pairs where the string specifies a term and the array of values specifies each of the aliases for that term. An alias is considered a synonym of the specified term, but the term is not considered a synonym of the alias. For more information about specifying synonyms, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-analysis-schemes.html#synonyms Synonyms> in the /Amazon CloudSearch Developer Guide/ .
--
-- /Note:/ Consider using 'synonyms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoSynonyms :: Lens.Lens' AnalysisOptions (Lude.Maybe Lude.Text)
aoSynonyms = Lens.lens (synonyms :: AnalysisOptions -> Lude.Maybe Lude.Text) (\s a -> s {synonyms = a} :: AnalysisOptions)
{-# DEPRECATED aoSynonyms "Use generic-lens or generic-optics with 'synonyms' instead." #-}

-- | A JSON object that contains a collection of string:value pairs that each map a term to its stem. For example, @{"term1": "stem1", "term2": "stem2", "term3": "stem3"}@ . The stemming dictionary is applied in addition to any algorithmic stemming. This enables you to override the results of the algorithmic stemming to correct specific cases of overstemming or understemming. The maximum size of a stemming dictionary is 500 KB.
--
-- /Note:/ Consider using 'stemmingDictionary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoStemmingDictionary :: Lens.Lens' AnalysisOptions (Lude.Maybe Lude.Text)
aoStemmingDictionary = Lens.lens (stemmingDictionary :: AnalysisOptions -> Lude.Maybe Lude.Text) (\s a -> s {stemmingDictionary = a} :: AnalysisOptions)
{-# DEPRECATED aoStemmingDictionary "Use generic-lens or generic-optics with 'stemmingDictionary' instead." #-}

instance Lude.FromXML AnalysisOptions where
  parseXML x =
    AnalysisOptions'
      Lude.<$> (x Lude..@? "AlgorithmicStemming")
      Lude.<*> (x Lude..@? "Stopwords")
      Lude.<*> (x Lude..@? "JapaneseTokenizationDictionary")
      Lude.<*> (x Lude..@? "Synonyms")
      Lude.<*> (x Lude..@? "StemmingDictionary")

instance Lude.ToQuery AnalysisOptions where
  toQuery AnalysisOptions' {..} =
    Lude.mconcat
      [ "AlgorithmicStemming" Lude.=: algorithmicStemming,
        "Stopwords" Lude.=: stopwords,
        "JapaneseTokenizationDictionary"
          Lude.=: japaneseTokenizationDictionary,
        "Synonyms" Lude.=: synonyms,
        "StemmingDictionary" Lude.=: stemmingDictionary
      ]
