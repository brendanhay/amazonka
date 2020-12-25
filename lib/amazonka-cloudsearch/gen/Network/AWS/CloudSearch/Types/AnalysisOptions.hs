{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    aoJapaneseTokenizationDictionary,
    aoStemmingDictionary,
    aoStopwords,
    aoSynonyms,
  )
where

import qualified Network.AWS.CloudSearch.Types.AlgorithmicStemming as Types
import qualified Network.AWS.CloudSearch.Types.JapaneseTokenizationDictionary as Types
import qualified Network.AWS.CloudSearch.Types.StemmingDictionary as Types
import qualified Network.AWS.CloudSearch.Types.Stopwords as Types
import qualified Network.AWS.CloudSearch.Types.Synonyms as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Synonyms, stopwords, and stemming options for an analysis scheme. Includes tokenization dictionary for Japanese.
--
-- /See:/ 'mkAnalysisOptions' smart constructor.
data AnalysisOptions = AnalysisOptions'
  { -- | The level of algorithmic stemming to perform: @none@ , @minimal@ , @light@ , or @full@ . The available levels vary depending on the language. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/text-processing.html#text-processing-settings Language Specific Text Processing Settings> in the /Amazon CloudSearch Developer Guide/
    algorithmicStemming :: Core.Maybe Types.AlgorithmicStemming,
    -- | A JSON array that contains a collection of terms, tokens, readings and part of speech for Japanese Tokenizaiton. The Japanese tokenization dictionary enables you to override the default tokenization for selected terms. This is only valid for Japanese language fields.
    japaneseTokenizationDictionary :: Core.Maybe Types.JapaneseTokenizationDictionary,
    -- | A JSON object that contains a collection of string:value pairs that each map a term to its stem. For example, @{"term1": "stem1", "term2": "stem2", "term3": "stem3"}@ . The stemming dictionary is applied in addition to any algorithmic stemming. This enables you to override the results of the algorithmic stemming to correct specific cases of overstemming or understemming. The maximum size of a stemming dictionary is 500 KB.
    stemmingDictionary :: Core.Maybe Types.StemmingDictionary,
    -- | A JSON array of terms to ignore during indexing and searching. For example, @["a", "an", "the", "of"]@ . The stopwords dictionary must explicitly list each word you want to ignore. Wildcards and regular expressions are not supported.
    stopwords :: Core.Maybe Types.Stopwords,
    -- | A JSON object that defines synonym groups and aliases. A synonym group is an array of arrays, where each sub-array is a group of terms where each term in the group is considered a synonym of every other term in the group. The aliases value is an object that contains a collection of string:value pairs where the string specifies a term and the array of values specifies each of the aliases for that term. An alias is considered a synonym of the specified term, but the term is not considered a synonym of the alias. For more information about specifying synonyms, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-analysis-schemes.html#synonyms Synonyms> in the /Amazon CloudSearch Developer Guide/ .
    synonyms :: Core.Maybe Types.Synonyms
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AnalysisOptions' value with any optional fields omitted.
mkAnalysisOptions ::
  AnalysisOptions
mkAnalysisOptions =
  AnalysisOptions'
    { algorithmicStemming = Core.Nothing,
      japaneseTokenizationDictionary = Core.Nothing,
      stemmingDictionary = Core.Nothing,
      stopwords = Core.Nothing,
      synonyms = Core.Nothing
    }

-- | The level of algorithmic stemming to perform: @none@ , @minimal@ , @light@ , or @full@ . The available levels vary depending on the language. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/text-processing.html#text-processing-settings Language Specific Text Processing Settings> in the /Amazon CloudSearch Developer Guide/
--
-- /Note:/ Consider using 'algorithmicStemming' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoAlgorithmicStemming :: Lens.Lens' AnalysisOptions (Core.Maybe Types.AlgorithmicStemming)
aoAlgorithmicStemming = Lens.field @"algorithmicStemming"
{-# DEPRECATED aoAlgorithmicStemming "Use generic-lens or generic-optics with 'algorithmicStemming' instead." #-}

-- | A JSON array that contains a collection of terms, tokens, readings and part of speech for Japanese Tokenizaiton. The Japanese tokenization dictionary enables you to override the default tokenization for selected terms. This is only valid for Japanese language fields.
--
-- /Note:/ Consider using 'japaneseTokenizationDictionary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoJapaneseTokenizationDictionary :: Lens.Lens' AnalysisOptions (Core.Maybe Types.JapaneseTokenizationDictionary)
aoJapaneseTokenizationDictionary = Lens.field @"japaneseTokenizationDictionary"
{-# DEPRECATED aoJapaneseTokenizationDictionary "Use generic-lens or generic-optics with 'japaneseTokenizationDictionary' instead." #-}

-- | A JSON object that contains a collection of string:value pairs that each map a term to its stem. For example, @{"term1": "stem1", "term2": "stem2", "term3": "stem3"}@ . The stemming dictionary is applied in addition to any algorithmic stemming. This enables you to override the results of the algorithmic stemming to correct specific cases of overstemming or understemming. The maximum size of a stemming dictionary is 500 KB.
--
-- /Note:/ Consider using 'stemmingDictionary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoStemmingDictionary :: Lens.Lens' AnalysisOptions (Core.Maybe Types.StemmingDictionary)
aoStemmingDictionary = Lens.field @"stemmingDictionary"
{-# DEPRECATED aoStemmingDictionary "Use generic-lens or generic-optics with 'stemmingDictionary' instead." #-}

-- | A JSON array of terms to ignore during indexing and searching. For example, @["a", "an", "the", "of"]@ . The stopwords dictionary must explicitly list each word you want to ignore. Wildcards and regular expressions are not supported.
--
-- /Note:/ Consider using 'stopwords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoStopwords :: Lens.Lens' AnalysisOptions (Core.Maybe Types.Stopwords)
aoStopwords = Lens.field @"stopwords"
{-# DEPRECATED aoStopwords "Use generic-lens or generic-optics with 'stopwords' instead." #-}

-- | A JSON object that defines synonym groups and aliases. A synonym group is an array of arrays, where each sub-array is a group of terms where each term in the group is considered a synonym of every other term in the group. The aliases value is an object that contains a collection of string:value pairs where the string specifies a term and the array of values specifies each of the aliases for that term. An alias is considered a synonym of the specified term, but the term is not considered a synonym of the alias. For more information about specifying synonyms, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-analysis-schemes.html#synonyms Synonyms> in the /Amazon CloudSearch Developer Guide/ .
--
-- /Note:/ Consider using 'synonyms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoSynonyms :: Lens.Lens' AnalysisOptions (Core.Maybe Types.Synonyms)
aoSynonyms = Lens.field @"synonyms"
{-# DEPRECATED aoSynonyms "Use generic-lens or generic-optics with 'synonyms' instead." #-}

instance Core.FromXML AnalysisOptions where
  parseXML x =
    AnalysisOptions'
      Core.<$> (x Core..@? "AlgorithmicStemming")
      Core.<*> (x Core..@? "JapaneseTokenizationDictionary")
      Core.<*> (x Core..@? "StemmingDictionary")
      Core.<*> (x Core..@? "Stopwords")
      Core.<*> (x Core..@? "Synonyms")
