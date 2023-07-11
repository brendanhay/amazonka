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
-- Module      : Amazonka.CloudSearch.Types.AnalysisScheme
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearch.Types.AnalysisScheme where

import Amazonka.CloudSearch.Types.AnalysisOptions
import Amazonka.CloudSearch.Types.AnalysisSchemeLanguage
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration information for an analysis scheme. Each analysis scheme
-- has a unique name and specifies the language of the text to be
-- processed. The following options can be configured for an analysis
-- scheme: @Synonyms@, @Stopwords@, @StemmingDictionary@,
-- @JapaneseTokenizationDictionary@ and @AlgorithmicStemming@.
--
-- /See:/ 'newAnalysisScheme' smart constructor.
data AnalysisScheme = AnalysisScheme'
  { analysisOptions :: Prelude.Maybe AnalysisOptions,
    analysisSchemeName :: Prelude.Text,
    analysisSchemeLanguage :: AnalysisSchemeLanguage
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalysisScheme' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analysisOptions', 'analysisScheme_analysisOptions' - Undocumented member.
--
-- 'analysisSchemeName', 'analysisScheme_analysisSchemeName' - Undocumented member.
--
-- 'analysisSchemeLanguage', 'analysisScheme_analysisSchemeLanguage' - Undocumented member.
newAnalysisScheme ::
  -- | 'analysisSchemeName'
  Prelude.Text ->
  -- | 'analysisSchemeLanguage'
  AnalysisSchemeLanguage ->
  AnalysisScheme
newAnalysisScheme
  pAnalysisSchemeName_
  pAnalysisSchemeLanguage_ =
    AnalysisScheme'
      { analysisOptions = Prelude.Nothing,
        analysisSchemeName = pAnalysisSchemeName_,
        analysisSchemeLanguage = pAnalysisSchemeLanguage_
      }

-- | Undocumented member.
analysisScheme_analysisOptions :: Lens.Lens' AnalysisScheme (Prelude.Maybe AnalysisOptions)
analysisScheme_analysisOptions = Lens.lens (\AnalysisScheme' {analysisOptions} -> analysisOptions) (\s@AnalysisScheme' {} a -> s {analysisOptions = a} :: AnalysisScheme)

-- | Undocumented member.
analysisScheme_analysisSchemeName :: Lens.Lens' AnalysisScheme Prelude.Text
analysisScheme_analysisSchemeName = Lens.lens (\AnalysisScheme' {analysisSchemeName} -> analysisSchemeName) (\s@AnalysisScheme' {} a -> s {analysisSchemeName = a} :: AnalysisScheme)

-- | Undocumented member.
analysisScheme_analysisSchemeLanguage :: Lens.Lens' AnalysisScheme AnalysisSchemeLanguage
analysisScheme_analysisSchemeLanguage = Lens.lens (\AnalysisScheme' {analysisSchemeLanguage} -> analysisSchemeLanguage) (\s@AnalysisScheme' {} a -> s {analysisSchemeLanguage = a} :: AnalysisScheme)

instance Data.FromXML AnalysisScheme where
  parseXML x =
    AnalysisScheme'
      Prelude.<$> (x Data..@? "AnalysisOptions")
      Prelude.<*> (x Data..@ "AnalysisSchemeName")
      Prelude.<*> (x Data..@ "AnalysisSchemeLanguage")

instance Prelude.Hashable AnalysisScheme where
  hashWithSalt _salt AnalysisScheme' {..} =
    _salt
      `Prelude.hashWithSalt` analysisOptions
      `Prelude.hashWithSalt` analysisSchemeName
      `Prelude.hashWithSalt` analysisSchemeLanguage

instance Prelude.NFData AnalysisScheme where
  rnf AnalysisScheme' {..} =
    Prelude.rnf analysisOptions
      `Prelude.seq` Prelude.rnf analysisSchemeName
      `Prelude.seq` Prelude.rnf analysisSchemeLanguage

instance Data.ToQuery AnalysisScheme where
  toQuery AnalysisScheme' {..} =
    Prelude.mconcat
      [ "AnalysisOptions" Data.=: analysisOptions,
        "AnalysisSchemeName" Data.=: analysisSchemeName,
        "AnalysisSchemeLanguage"
          Data.=: analysisSchemeLanguage
      ]
