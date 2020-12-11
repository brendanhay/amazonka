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
    viVocabularyName,
    viLastModifiedTime,
    viVocabularyState,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Transcribe.Types.LanguageCode
import Network.AWS.Transcribe.Types.VocabularyState

-- | Provides information about a custom vocabulary.
--
-- /See:/ 'mkVocabularyInfo' smart constructor.
data VocabularyInfo = VocabularyInfo'
  { languageCode ::
      Lude.Maybe LanguageCode,
    vocabularyName :: Lude.Maybe Lude.Text,
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    vocabularyState :: Lude.Maybe VocabularyState
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VocabularyInfo' with the minimum fields required to make a request.
--
-- * 'languageCode' - The language code of the vocabulary entries.
-- * 'lastModifiedTime' - The date and time that the vocabulary was last modified.
-- * 'vocabularyName' - The name of the vocabulary.
-- * 'vocabularyState' - The processing state of the vocabulary. If the state is @READY@ you can use the vocabulary in a @StartTranscriptionJob@ request.
mkVocabularyInfo ::
  VocabularyInfo
mkVocabularyInfo =
  VocabularyInfo'
    { languageCode = Lude.Nothing,
      vocabularyName = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      vocabularyState = Lude.Nothing
    }

-- | The language code of the vocabulary entries.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viLanguageCode :: Lens.Lens' VocabularyInfo (Lude.Maybe LanguageCode)
viLanguageCode = Lens.lens (languageCode :: VocabularyInfo -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: VocabularyInfo)
{-# DEPRECATED viLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The name of the vocabulary.
--
-- /Note:/ Consider using 'vocabularyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viVocabularyName :: Lens.Lens' VocabularyInfo (Lude.Maybe Lude.Text)
viVocabularyName = Lens.lens (vocabularyName :: VocabularyInfo -> Lude.Maybe Lude.Text) (\s a -> s {vocabularyName = a} :: VocabularyInfo)
{-# DEPRECATED viVocabularyName "Use generic-lens or generic-optics with 'vocabularyName' instead." #-}

-- | The date and time that the vocabulary was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viLastModifiedTime :: Lens.Lens' VocabularyInfo (Lude.Maybe Lude.Timestamp)
viLastModifiedTime = Lens.lens (lastModifiedTime :: VocabularyInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: VocabularyInfo)
{-# DEPRECATED viLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The processing state of the vocabulary. If the state is @READY@ you can use the vocabulary in a @StartTranscriptionJob@ request.
--
-- /Note:/ Consider using 'vocabularyState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viVocabularyState :: Lens.Lens' VocabularyInfo (Lude.Maybe VocabularyState)
viVocabularyState = Lens.lens (vocabularyState :: VocabularyInfo -> Lude.Maybe VocabularyState) (\s a -> s {vocabularyState = a} :: VocabularyInfo)
{-# DEPRECATED viVocabularyState "Use generic-lens or generic-optics with 'vocabularyState' instead." #-}

instance Lude.FromJSON VocabularyInfo where
  parseJSON =
    Lude.withObject
      "VocabularyInfo"
      ( \x ->
          VocabularyInfo'
            Lude.<$> (x Lude..:? "LanguageCode")
            Lude.<*> (x Lude..:? "VocabularyName")
            Lude.<*> (x Lude..:? "LastModifiedTime")
            Lude.<*> (x Lude..:? "VocabularyState")
      )
