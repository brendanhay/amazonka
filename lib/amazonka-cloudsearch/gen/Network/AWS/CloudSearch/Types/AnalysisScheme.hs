{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.AnalysisScheme
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.AnalysisScheme
  ( AnalysisScheme (..),

    -- * Smart constructor
    mkAnalysisScheme,

    -- * Lenses
    asAnalysisOptions,
    asAnalysisSchemeName,
    asAnalysisSchemeLanguage,
  )
where

import Network.AWS.CloudSearch.Types.AnalysisOptions
import Network.AWS.CloudSearch.Types.AnalysisSchemeLanguage
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration information for an analysis scheme. Each analysis scheme has a unique name and specifies the language of the text to be processed. The following options can be configured for an analysis scheme: @Synonyms@ , @Stopwords@ , @StemmingDictionary@ , @JapaneseTokenizationDictionary@ and @AlgorithmicStemming@ .
--
-- /See:/ 'mkAnalysisScheme' smart constructor.
data AnalysisScheme = AnalysisScheme'
  { analysisOptions ::
      Lude.Maybe AnalysisOptions,
    analysisSchemeName :: Lude.Text,
    analysisSchemeLanguage :: AnalysisSchemeLanguage
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AnalysisScheme' with the minimum fields required to make a request.
--
-- * 'analysisOptions' - Undocumented field.
-- * 'analysisSchemeLanguage' - Undocumented field.
-- * 'analysisSchemeName' - Undocumented field.
mkAnalysisScheme ::
  -- | 'analysisSchemeName'
  Lude.Text ->
  -- | 'analysisSchemeLanguage'
  AnalysisSchemeLanguage ->
  AnalysisScheme
mkAnalysisScheme pAnalysisSchemeName_ pAnalysisSchemeLanguage_ =
  AnalysisScheme'
    { analysisOptions = Lude.Nothing,
      analysisSchemeName = pAnalysisSchemeName_,
      analysisSchemeLanguage = pAnalysisSchemeLanguage_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'analysisOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asAnalysisOptions :: Lens.Lens' AnalysisScheme (Lude.Maybe AnalysisOptions)
asAnalysisOptions = Lens.lens (analysisOptions :: AnalysisScheme -> Lude.Maybe AnalysisOptions) (\s a -> s {analysisOptions = a} :: AnalysisScheme)
{-# DEPRECATED asAnalysisOptions "Use generic-lens or generic-optics with 'analysisOptions' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'analysisSchemeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asAnalysisSchemeName :: Lens.Lens' AnalysisScheme Lude.Text
asAnalysisSchemeName = Lens.lens (analysisSchemeName :: AnalysisScheme -> Lude.Text) (\s a -> s {analysisSchemeName = a} :: AnalysisScheme)
{-# DEPRECATED asAnalysisSchemeName "Use generic-lens or generic-optics with 'analysisSchemeName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'analysisSchemeLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asAnalysisSchemeLanguage :: Lens.Lens' AnalysisScheme AnalysisSchemeLanguage
asAnalysisSchemeLanguage = Lens.lens (analysisSchemeLanguage :: AnalysisScheme -> AnalysisSchemeLanguage) (\s a -> s {analysisSchemeLanguage = a} :: AnalysisScheme)
{-# DEPRECATED asAnalysisSchemeLanguage "Use generic-lens or generic-optics with 'analysisSchemeLanguage' instead." #-}

instance Lude.FromXML AnalysisScheme where
  parseXML x =
    AnalysisScheme'
      Lude.<$> (x Lude..@? "AnalysisOptions")
      Lude.<*> (x Lude..@ "AnalysisSchemeName")
      Lude.<*> (x Lude..@ "AnalysisSchemeLanguage")

instance Lude.ToQuery AnalysisScheme where
  toQuery AnalysisScheme' {..} =
    Lude.mconcat
      [ "AnalysisOptions" Lude.=: analysisOptions,
        "AnalysisSchemeName" Lude.=: analysisSchemeName,
        "AnalysisSchemeLanguage" Lude.=: analysisSchemeLanguage
      ]
