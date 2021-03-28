{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.VocabularyFilterInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Transcribe.Types.VocabularyFilterInfo
  ( VocabularyFilterInfo (..)
  -- * Smart constructor
  , mkVocabularyFilterInfo
  -- * Lenses
  , vfiLanguageCode
  , vfiLastModifiedTime
  , vfiVocabularyFilterName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Transcribe.Types.LanguageCode as Types
import qualified Network.AWS.Transcribe.Types.VocabularyFilterName as Types

-- | Provides information about a vocabulary filter.
--
-- /See:/ 'mkVocabularyFilterInfo' smart constructor.
data VocabularyFilterInfo = VocabularyFilterInfo'
  { languageCode :: Core.Maybe Types.LanguageCode
    -- ^ The language code of the words in the vocabulary filter.
  , lastModifiedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time that the vocabulary was last updated.
  , vocabularyFilterName :: Core.Maybe Types.VocabularyFilterName
    -- ^ The name of the vocabulary filter. The name must be unique in the account that holds the filter.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'VocabularyFilterInfo' value with any optional fields omitted.
mkVocabularyFilterInfo
    :: VocabularyFilterInfo
mkVocabularyFilterInfo
  = VocabularyFilterInfo'{languageCode = Core.Nothing,
                          lastModifiedTime = Core.Nothing,
                          vocabularyFilterName = Core.Nothing}

-- | The language code of the words in the vocabulary filter.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vfiLanguageCode :: Lens.Lens' VocabularyFilterInfo (Core.Maybe Types.LanguageCode)
vfiLanguageCode = Lens.field @"languageCode"
{-# INLINEABLE vfiLanguageCode #-}
{-# DEPRECATED languageCode "Use generic-lens or generic-optics with 'languageCode' instead"  #-}

-- | The date and time that the vocabulary was last updated.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vfiLastModifiedTime :: Lens.Lens' VocabularyFilterInfo (Core.Maybe Core.NominalDiffTime)
vfiLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE vfiLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

-- | The name of the vocabulary filter. The name must be unique in the account that holds the filter.
--
-- /Note:/ Consider using 'vocabularyFilterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vfiVocabularyFilterName :: Lens.Lens' VocabularyFilterInfo (Core.Maybe Types.VocabularyFilterName)
vfiVocabularyFilterName = Lens.field @"vocabularyFilterName"
{-# INLINEABLE vfiVocabularyFilterName #-}
{-# DEPRECATED vocabularyFilterName "Use generic-lens or generic-optics with 'vocabularyFilterName' instead"  #-}

instance Core.FromJSON VocabularyFilterInfo where
        parseJSON
          = Core.withObject "VocabularyFilterInfo" Core.$
              \ x ->
                VocabularyFilterInfo' Core.<$>
                  (x Core..:? "LanguageCode") Core.<*> x Core..:? "LastModifiedTime"
                    Core.<*> x Core..:? "VocabularyFilterName"
