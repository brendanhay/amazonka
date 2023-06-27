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
-- Module      : Amazonka.CloudSearch.Types.AnalysisOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearch.Types.AnalysisOptions where

import Amazonka.CloudSearch.Types.AlgorithmicStemming
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Synonyms, stopwords, and stemming options for an analysis scheme.
-- Includes tokenization dictionary for Japanese.
--
-- /See:/ 'newAnalysisOptions' smart constructor.
data AnalysisOptions = AnalysisOptions'
  { -- | The level of algorithmic stemming to perform: @none@, @minimal@,
    -- @light@, or @full@. The available levels vary depending on the language.
    -- For more information, see
    -- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/text-processing.html#text-processing-settings Language Specific Text Processing Settings>
    -- in the /Amazon CloudSearch Developer Guide/
    algorithmicStemming :: Prelude.Maybe AlgorithmicStemming,
    -- | A JSON array that contains a collection of terms, tokens, readings and
    -- part of speech for Japanese Tokenizaiton. The Japanese tokenization
    -- dictionary enables you to override the default tokenization for selected
    -- terms. This is only valid for Japanese language fields.
    japaneseTokenizationDictionary :: Prelude.Maybe Prelude.Text,
    -- | A JSON object that contains a collection of string:value pairs that each
    -- map a term to its stem. For example,
    -- @{\"term1\": \"stem1\", \"term2\": \"stem2\", \"term3\": \"stem3\"}@.
    -- The stemming dictionary is applied in addition to any algorithmic
    -- stemming. This enables you to override the results of the algorithmic
    -- stemming to correct specific cases of overstemming or understemming. The
    -- maximum size of a stemming dictionary is 500 KB.
    stemmingDictionary :: Prelude.Maybe Prelude.Text,
    -- | A JSON array of terms to ignore during indexing and searching. For
    -- example, @[\"a\", \"an\", \"the\", \"of\"]@. The stopwords dictionary
    -- must explicitly list each word you want to ignore. Wildcards and regular
    -- expressions are not supported.
    stopwords :: Prelude.Maybe Prelude.Text,
    -- | A JSON object that defines synonym groups and aliases. A synonym group
    -- is an array of arrays, where each sub-array is a group of terms where
    -- each term in the group is considered a synonym of every other term in
    -- the group. The aliases value is an object that contains a collection of
    -- string:value pairs where the string specifies a term and the array of
    -- values specifies each of the aliases for that term. An alias is
    -- considered a synonym of the specified term, but the term is not
    -- considered a synonym of the alias. For more information about specifying
    -- synonyms, see
    -- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-analysis-schemes.html#synonyms Synonyms>
    -- in the /Amazon CloudSearch Developer Guide/.
    synonyms :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalysisOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'algorithmicStemming', 'analysisOptions_algorithmicStemming' - The level of algorithmic stemming to perform: @none@, @minimal@,
-- @light@, or @full@. The available levels vary depending on the language.
-- For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/text-processing.html#text-processing-settings Language Specific Text Processing Settings>
-- in the /Amazon CloudSearch Developer Guide/
--
-- 'japaneseTokenizationDictionary', 'analysisOptions_japaneseTokenizationDictionary' - A JSON array that contains a collection of terms, tokens, readings and
-- part of speech for Japanese Tokenizaiton. The Japanese tokenization
-- dictionary enables you to override the default tokenization for selected
-- terms. This is only valid for Japanese language fields.
--
-- 'stemmingDictionary', 'analysisOptions_stemmingDictionary' - A JSON object that contains a collection of string:value pairs that each
-- map a term to its stem. For example,
-- @{\"term1\": \"stem1\", \"term2\": \"stem2\", \"term3\": \"stem3\"}@.
-- The stemming dictionary is applied in addition to any algorithmic
-- stemming. This enables you to override the results of the algorithmic
-- stemming to correct specific cases of overstemming or understemming. The
-- maximum size of a stemming dictionary is 500 KB.
--
-- 'stopwords', 'analysisOptions_stopwords' - A JSON array of terms to ignore during indexing and searching. For
-- example, @[\"a\", \"an\", \"the\", \"of\"]@. The stopwords dictionary
-- must explicitly list each word you want to ignore. Wildcards and regular
-- expressions are not supported.
--
-- 'synonyms', 'analysisOptions_synonyms' - A JSON object that defines synonym groups and aliases. A synonym group
-- is an array of arrays, where each sub-array is a group of terms where
-- each term in the group is considered a synonym of every other term in
-- the group. The aliases value is an object that contains a collection of
-- string:value pairs where the string specifies a term and the array of
-- values specifies each of the aliases for that term. An alias is
-- considered a synonym of the specified term, but the term is not
-- considered a synonym of the alias. For more information about specifying
-- synonyms, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-analysis-schemes.html#synonyms Synonyms>
-- in the /Amazon CloudSearch Developer Guide/.
newAnalysisOptions ::
  AnalysisOptions
newAnalysisOptions =
  AnalysisOptions'
    { algorithmicStemming =
        Prelude.Nothing,
      japaneseTokenizationDictionary = Prelude.Nothing,
      stemmingDictionary = Prelude.Nothing,
      stopwords = Prelude.Nothing,
      synonyms = Prelude.Nothing
    }

-- | The level of algorithmic stemming to perform: @none@, @minimal@,
-- @light@, or @full@. The available levels vary depending on the language.
-- For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/text-processing.html#text-processing-settings Language Specific Text Processing Settings>
-- in the /Amazon CloudSearch Developer Guide/
analysisOptions_algorithmicStemming :: Lens.Lens' AnalysisOptions (Prelude.Maybe AlgorithmicStemming)
analysisOptions_algorithmicStemming = Lens.lens (\AnalysisOptions' {algorithmicStemming} -> algorithmicStemming) (\s@AnalysisOptions' {} a -> s {algorithmicStemming = a} :: AnalysisOptions)

-- | A JSON array that contains a collection of terms, tokens, readings and
-- part of speech for Japanese Tokenizaiton. The Japanese tokenization
-- dictionary enables you to override the default tokenization for selected
-- terms. This is only valid for Japanese language fields.
analysisOptions_japaneseTokenizationDictionary :: Lens.Lens' AnalysisOptions (Prelude.Maybe Prelude.Text)
analysisOptions_japaneseTokenizationDictionary = Lens.lens (\AnalysisOptions' {japaneseTokenizationDictionary} -> japaneseTokenizationDictionary) (\s@AnalysisOptions' {} a -> s {japaneseTokenizationDictionary = a} :: AnalysisOptions)

-- | A JSON object that contains a collection of string:value pairs that each
-- map a term to its stem. For example,
-- @{\"term1\": \"stem1\", \"term2\": \"stem2\", \"term3\": \"stem3\"}@.
-- The stemming dictionary is applied in addition to any algorithmic
-- stemming. This enables you to override the results of the algorithmic
-- stemming to correct specific cases of overstemming or understemming. The
-- maximum size of a stemming dictionary is 500 KB.
analysisOptions_stemmingDictionary :: Lens.Lens' AnalysisOptions (Prelude.Maybe Prelude.Text)
analysisOptions_stemmingDictionary = Lens.lens (\AnalysisOptions' {stemmingDictionary} -> stemmingDictionary) (\s@AnalysisOptions' {} a -> s {stemmingDictionary = a} :: AnalysisOptions)

-- | A JSON array of terms to ignore during indexing and searching. For
-- example, @[\"a\", \"an\", \"the\", \"of\"]@. The stopwords dictionary
-- must explicitly list each word you want to ignore. Wildcards and regular
-- expressions are not supported.
analysisOptions_stopwords :: Lens.Lens' AnalysisOptions (Prelude.Maybe Prelude.Text)
analysisOptions_stopwords = Lens.lens (\AnalysisOptions' {stopwords} -> stopwords) (\s@AnalysisOptions' {} a -> s {stopwords = a} :: AnalysisOptions)

-- | A JSON object that defines synonym groups and aliases. A synonym group
-- is an array of arrays, where each sub-array is a group of terms where
-- each term in the group is considered a synonym of every other term in
-- the group. The aliases value is an object that contains a collection of
-- string:value pairs where the string specifies a term and the array of
-- values specifies each of the aliases for that term. An alias is
-- considered a synonym of the specified term, but the term is not
-- considered a synonym of the alias. For more information about specifying
-- synonyms, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-analysis-schemes.html#synonyms Synonyms>
-- in the /Amazon CloudSearch Developer Guide/.
analysisOptions_synonyms :: Lens.Lens' AnalysisOptions (Prelude.Maybe Prelude.Text)
analysisOptions_synonyms = Lens.lens (\AnalysisOptions' {synonyms} -> synonyms) (\s@AnalysisOptions' {} a -> s {synonyms = a} :: AnalysisOptions)

instance Data.FromXML AnalysisOptions where
  parseXML x =
    AnalysisOptions'
      Prelude.<$> (x Data..@? "AlgorithmicStemming")
      Prelude.<*> (x Data..@? "JapaneseTokenizationDictionary")
      Prelude.<*> (x Data..@? "StemmingDictionary")
      Prelude.<*> (x Data..@? "Stopwords")
      Prelude.<*> (x Data..@? "Synonyms")

instance Prelude.Hashable AnalysisOptions where
  hashWithSalt _salt AnalysisOptions' {..} =
    _salt
      `Prelude.hashWithSalt` algorithmicStemming
      `Prelude.hashWithSalt` japaneseTokenizationDictionary
      `Prelude.hashWithSalt` stemmingDictionary
      `Prelude.hashWithSalt` stopwords
      `Prelude.hashWithSalt` synonyms

instance Prelude.NFData AnalysisOptions where
  rnf AnalysisOptions' {..} =
    Prelude.rnf algorithmicStemming
      `Prelude.seq` Prelude.rnf japaneseTokenizationDictionary
      `Prelude.seq` Prelude.rnf stemmingDictionary
      `Prelude.seq` Prelude.rnf stopwords
      `Prelude.seq` Prelude.rnf synonyms

instance Data.ToQuery AnalysisOptions where
  toQuery AnalysisOptions' {..} =
    Prelude.mconcat
      [ "AlgorithmicStemming" Data.=: algorithmicStemming,
        "JapaneseTokenizationDictionary"
          Data.=: japaneseTokenizationDictionary,
        "StemmingDictionary" Data.=: stemmingDictionary,
        "Stopwords" Data.=: stopwords,
        "Synonyms" Data.=: synonyms
      ]
