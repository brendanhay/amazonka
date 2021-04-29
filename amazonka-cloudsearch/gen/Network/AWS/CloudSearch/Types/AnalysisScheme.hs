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
-- Module      : Network.AWS.CloudSearch.Types.AnalysisScheme
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.AnalysisScheme where

import Network.AWS.CloudSearch.Types.AnalysisOptions
import Network.AWS.CloudSearch.Types.AnalysisSchemeLanguage
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromXML AnalysisScheme where
  parseXML x =
    AnalysisScheme'
      Prelude.<$> (x Prelude..@? "AnalysisOptions")
      Prelude.<*> (x Prelude..@ "AnalysisSchemeName")
      Prelude.<*> (x Prelude..@ "AnalysisSchemeLanguage")

instance Prelude.Hashable AnalysisScheme

instance Prelude.NFData AnalysisScheme

instance Prelude.ToQuery AnalysisScheme where
  toQuery AnalysisScheme' {..} =
    Prelude.mconcat
      [ "AnalysisOptions" Prelude.=: analysisOptions,
        "AnalysisSchemeName" Prelude.=: analysisSchemeName,
        "AnalysisSchemeLanguage"
          Prelude.=: analysisSchemeLanguage
      ]
