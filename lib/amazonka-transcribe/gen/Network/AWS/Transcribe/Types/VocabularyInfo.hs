{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.VocabularyInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.VocabularyInfo
  ( VocabularyInfo (..),

    -- * Smart constructor
    mkVocabularyInfo,

    -- * Lenses
    viLanguageCode,
    viLastModifiedTime,
    viVocabularyName,
    viVocabularyState,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Transcribe.Types.LanguageCode as Types
import qualified Network.AWS.Transcribe.Types.VocabularyName as Types
import qualified Network.AWS.Transcribe.Types.VocabularyState as Types

-- | Provides information about a custom vocabulary.
--
-- /See:/ 'mkVocabularyInfo' smart constructor.
data VocabularyInfo = VocabularyInfo'
  { -- | The language code of the vocabulary entries.
    languageCode :: Core.Maybe Types.LanguageCode,
    -- | The date and time that the vocabulary was last modified.
    lastModifiedTime :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the vocabulary.
    vocabularyName :: Core.Maybe Types.VocabularyName,
    -- | The processing state of the vocabulary. If the state is @READY@ you can use the vocabulary in a @StartTranscriptionJob@ request.
    vocabularyState :: Core.Maybe Types.VocabularyState
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'VocabularyInfo' value with any optional fields omitted.
mkVocabularyInfo ::
  VocabularyInfo
mkVocabularyInfo =
  VocabularyInfo'
    { languageCode = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      vocabularyName = Core.Nothing,
      vocabularyState = Core.Nothing
    }

-- | The language code of the vocabulary entries.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viLanguageCode :: Lens.Lens' VocabularyInfo (Core.Maybe Types.LanguageCode)
viLanguageCode = Lens.field @"languageCode"
{-# DEPRECATED viLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The date and time that the vocabulary was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viLastModifiedTime :: Lens.Lens' VocabularyInfo (Core.Maybe Core.NominalDiffTime)
viLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED viLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The name of the vocabulary.
--
-- /Note:/ Consider using 'vocabularyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viVocabularyName :: Lens.Lens' VocabularyInfo (Core.Maybe Types.VocabularyName)
viVocabularyName = Lens.field @"vocabularyName"
{-# DEPRECATED viVocabularyName "Use generic-lens or generic-optics with 'vocabularyName' instead." #-}

-- | The processing state of the vocabulary. If the state is @READY@ you can use the vocabulary in a @StartTranscriptionJob@ request.
--
-- /Note:/ Consider using 'vocabularyState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viVocabularyState :: Lens.Lens' VocabularyInfo (Core.Maybe Types.VocabularyState)
viVocabularyState = Lens.field @"vocabularyState"
{-# DEPRECATED viVocabularyState "Use generic-lens or generic-optics with 'vocabularyState' instead." #-}

instance Core.FromJSON VocabularyInfo where
  parseJSON =
    Core.withObject "VocabularyInfo" Core.$
      \x ->
        VocabularyInfo'
          Core.<$> (x Core..:? "LanguageCode")
          Core.<*> (x Core..:? "LastModifiedTime")
          Core.<*> (x Core..:? "VocabularyName")
          Core.<*> (x Core..:? "VocabularyState")
