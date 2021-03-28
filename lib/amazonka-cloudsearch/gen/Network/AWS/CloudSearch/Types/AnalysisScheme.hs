{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.AnalysisScheme
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearch.Types.AnalysisScheme
  ( AnalysisScheme (..)
  -- * Smart constructor
  , mkAnalysisScheme
  -- * Lenses
  , asAnalysisSchemeName
  , asAnalysisSchemeLanguage
  , asAnalysisOptions
  ) where

import qualified Network.AWS.CloudSearch.Types.AnalysisOptions as Types
import qualified Network.AWS.CloudSearch.Types.AnalysisSchemeLanguage as Types
import qualified Network.AWS.CloudSearch.Types.AnalysisSchemeName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration information for an analysis scheme. Each analysis scheme has a unique name and specifies the language of the text to be processed. The following options can be configured for an analysis scheme: @Synonyms@ , @Stopwords@ , @StemmingDictionary@ , @JapaneseTokenizationDictionary@ and @AlgorithmicStemming@ .
--
-- /See:/ 'mkAnalysisScheme' smart constructor.
data AnalysisScheme = AnalysisScheme'
  { analysisSchemeName :: Types.AnalysisSchemeName
  , analysisSchemeLanguage :: Types.AnalysisSchemeLanguage
  , analysisOptions :: Core.Maybe Types.AnalysisOptions
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AnalysisScheme' value with any optional fields omitted.
mkAnalysisScheme
    :: Types.AnalysisSchemeName -- ^ 'analysisSchemeName'
    -> Types.AnalysisSchemeLanguage -- ^ 'analysisSchemeLanguage'
    -> AnalysisScheme
mkAnalysisScheme analysisSchemeName analysisSchemeLanguage
  = AnalysisScheme'{analysisSchemeName, analysisSchemeLanguage,
                    analysisOptions = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'analysisSchemeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asAnalysisSchemeName :: Lens.Lens' AnalysisScheme Types.AnalysisSchemeName
asAnalysisSchemeName = Lens.field @"analysisSchemeName"
{-# INLINEABLE asAnalysisSchemeName #-}
{-# DEPRECATED analysisSchemeName "Use generic-lens or generic-optics with 'analysisSchemeName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'analysisSchemeLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asAnalysisSchemeLanguage :: Lens.Lens' AnalysisScheme Types.AnalysisSchemeLanguage
asAnalysisSchemeLanguage = Lens.field @"analysisSchemeLanguage"
{-# INLINEABLE asAnalysisSchemeLanguage #-}
{-# DEPRECATED analysisSchemeLanguage "Use generic-lens or generic-optics with 'analysisSchemeLanguage' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'analysisOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asAnalysisOptions :: Lens.Lens' AnalysisScheme (Core.Maybe Types.AnalysisOptions)
asAnalysisOptions = Lens.field @"analysisOptions"
{-# INLINEABLE asAnalysisOptions #-}
{-# DEPRECATED analysisOptions "Use generic-lens or generic-optics with 'analysisOptions' instead"  #-}

instance Core.ToQuery AnalysisScheme where
        toQuery AnalysisScheme{..}
          = Core.toQueryPair "AnalysisSchemeName" analysisSchemeName Core.<>
              Core.toQueryPair "AnalysisSchemeLanguage" analysisSchemeLanguage
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AnalysisOptions")
                analysisOptions

instance Core.FromXML AnalysisScheme where
        parseXML x
          = AnalysisScheme' Core.<$>
              (x Core..@ "AnalysisSchemeName") Core.<*>
                x Core..@ "AnalysisSchemeLanguage"
                Core.<*> x Core..@? "AnalysisOptions"
