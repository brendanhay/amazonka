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
-- Module      : Network.AWS.CloudSearch.Types.AnalysisScheme
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.AnalysisScheme where

import Network.AWS.CloudSearch.Types.AnalysisOptions
import Network.AWS.CloudSearch.Types.AnalysisSchemeLanguage
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Configuration information for an analysis scheme. Each analysis scheme
-- has a unique name and specifies the language of the text to be
-- processed. The following options can be configured for an analysis
-- scheme: @Synonyms@, @Stopwords@, @StemmingDictionary@,
-- @JapaneseTokenizationDictionary@ and @AlgorithmicStemming@.
--
-- /See:/ 'newAnalysisScheme' smart constructor.
data AnalysisScheme = AnalysisScheme'
  { analysisOptions :: Core.Maybe AnalysisOptions,
    analysisSchemeName :: Core.Text,
    analysisSchemeLanguage :: AnalysisSchemeLanguage
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'analysisSchemeLanguage'
  AnalysisSchemeLanguage ->
  AnalysisScheme
newAnalysisScheme
  pAnalysisSchemeName_
  pAnalysisSchemeLanguage_ =
    AnalysisScheme'
      { analysisOptions = Core.Nothing,
        analysisSchemeName = pAnalysisSchemeName_,
        analysisSchemeLanguage = pAnalysisSchemeLanguage_
      }

-- | Undocumented member.
analysisScheme_analysisOptions :: Lens.Lens' AnalysisScheme (Core.Maybe AnalysisOptions)
analysisScheme_analysisOptions = Lens.lens (\AnalysisScheme' {analysisOptions} -> analysisOptions) (\s@AnalysisScheme' {} a -> s {analysisOptions = a} :: AnalysisScheme)

-- | Undocumented member.
analysisScheme_analysisSchemeName :: Lens.Lens' AnalysisScheme Core.Text
analysisScheme_analysisSchemeName = Lens.lens (\AnalysisScheme' {analysisSchemeName} -> analysisSchemeName) (\s@AnalysisScheme' {} a -> s {analysisSchemeName = a} :: AnalysisScheme)

-- | Undocumented member.
analysisScheme_analysisSchemeLanguage :: Lens.Lens' AnalysisScheme AnalysisSchemeLanguage
analysisScheme_analysisSchemeLanguage = Lens.lens (\AnalysisScheme' {analysisSchemeLanguage} -> analysisSchemeLanguage) (\s@AnalysisScheme' {} a -> s {analysisSchemeLanguage = a} :: AnalysisScheme)

instance Core.FromXML AnalysisScheme where
  parseXML x =
    AnalysisScheme'
      Core.<$> (x Core..@? "AnalysisOptions")
      Core.<*> (x Core..@ "AnalysisSchemeName")
      Core.<*> (x Core..@ "AnalysisSchemeLanguage")

instance Core.Hashable AnalysisScheme

instance Core.NFData AnalysisScheme

instance Core.ToQuery AnalysisScheme where
  toQuery AnalysisScheme' {..} =
    Core.mconcat
      [ "AnalysisOptions" Core.=: analysisOptions,
        "AnalysisSchemeName" Core.=: analysisSchemeName,
        "AnalysisSchemeLanguage"
          Core.=: analysisSchemeLanguage
      ]
