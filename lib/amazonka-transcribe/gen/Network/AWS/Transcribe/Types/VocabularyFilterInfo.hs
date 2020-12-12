{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.VocabularyFilterInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.VocabularyFilterInfo
  ( VocabularyFilterInfo (..),

    -- * Smart constructor
    mkVocabularyFilterInfo,

    -- * Lenses
    vfiLanguageCode,
    vfiLastModifiedTime,
    vfiVocabularyFilterName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Transcribe.Types.LanguageCode

-- | Provides information about a vocabulary filter.
--
-- /See:/ 'mkVocabularyFilterInfo' smart constructor.
data VocabularyFilterInfo = VocabularyFilterInfo'
  { languageCode ::
      Lude.Maybe LanguageCode,
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    vocabularyFilterName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VocabularyFilterInfo' with the minimum fields required to make a request.
--
-- * 'languageCode' - The language code of the words in the vocabulary filter.
-- * 'lastModifiedTime' - The date and time that the vocabulary was last updated.
-- * 'vocabularyFilterName' - The name of the vocabulary filter. The name must be unique in the account that holds the filter.
mkVocabularyFilterInfo ::
  VocabularyFilterInfo
mkVocabularyFilterInfo =
  VocabularyFilterInfo'
    { languageCode = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      vocabularyFilterName = Lude.Nothing
    }

-- | The language code of the words in the vocabulary filter.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vfiLanguageCode :: Lens.Lens' VocabularyFilterInfo (Lude.Maybe LanguageCode)
vfiLanguageCode = Lens.lens (languageCode :: VocabularyFilterInfo -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: VocabularyFilterInfo)
{-# DEPRECATED vfiLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The date and time that the vocabulary was last updated.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vfiLastModifiedTime :: Lens.Lens' VocabularyFilterInfo (Lude.Maybe Lude.Timestamp)
vfiLastModifiedTime = Lens.lens (lastModifiedTime :: VocabularyFilterInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: VocabularyFilterInfo)
{-# DEPRECATED vfiLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The name of the vocabulary filter. The name must be unique in the account that holds the filter.
--
-- /Note:/ Consider using 'vocabularyFilterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vfiVocabularyFilterName :: Lens.Lens' VocabularyFilterInfo (Lude.Maybe Lude.Text)
vfiVocabularyFilterName = Lens.lens (vocabularyFilterName :: VocabularyFilterInfo -> Lude.Maybe Lude.Text) (\s a -> s {vocabularyFilterName = a} :: VocabularyFilterInfo)
{-# DEPRECATED vfiVocabularyFilterName "Use generic-lens or generic-optics with 'vocabularyFilterName' instead." #-}

instance Lude.FromJSON VocabularyFilterInfo where
  parseJSON =
    Lude.withObject
      "VocabularyFilterInfo"
      ( \x ->
          VocabularyFilterInfo'
            Lude.<$> (x Lude..:? "LanguageCode")
            Lude.<*> (x Lude..:? "LastModifiedTime")
            Lude.<*> (x Lude..:? "VocabularyFilterName")
      )
