{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.UpdateVocabularyFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a vocabulary filter with a new list of filtered words.
module Network.AWS.Transcribe.UpdateVocabularyFilter
  ( -- * Creating a request
    UpdateVocabularyFilter (..),
    mkUpdateVocabularyFilter,

    -- ** Request lenses
    uvfVocabularyFilterFileURI,
    uvfVocabularyFilterName,
    uvfWords,

    -- * Destructuring the response
    UpdateVocabularyFilterResponse (..),
    mkUpdateVocabularyFilterResponse,

    -- ** Response lenses
    uvfrsLanguageCode,
    uvfrsLastModifiedTime,
    uvfrsVocabularyFilterName,
    uvfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Transcribe.Types

-- | /See:/ 'mkUpdateVocabularyFilter' smart constructor.
data UpdateVocabularyFilter = UpdateVocabularyFilter'
  { -- | The Amazon S3 location of a text file used as input to create the vocabulary filter. Only use characters from the character set defined for custom vocabularies. For a list of character sets, see <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies> .
    --
    -- The specified file must be less than 50 KB of UTF-8 characters.
    -- If you provide the location of a list of words in the @VocabularyFilterFileUri@ parameter, you can't use the @Words@ parameter.
    vocabularyFilterFileURI :: Lude.Maybe Lude.Text,
    -- | The name of the vocabulary filter to update. If you try to update a vocabulary filter with the same name as another vocabulary filter, you get a @ConflictException@ error.
    vocabularyFilterName :: Lude.Text,
    -- | The words to use in the vocabulary filter. Only use characters from the character set defined for custom vocabularies. For a list of character sets, see <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies> .
    --
    -- If you provide a list of words in the @Words@ parameter, you can't use the @VocabularyFilterFileUri@ parameter.
    words :: Lude.Maybe (Lude.NonEmpty Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateVocabularyFilter' with the minimum fields required to make a request.
--
-- * 'vocabularyFilterFileURI' - The Amazon S3 location of a text file used as input to create the vocabulary filter. Only use characters from the character set defined for custom vocabularies. For a list of character sets, see <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies> .
--
-- The specified file must be less than 50 KB of UTF-8 characters.
-- If you provide the location of a list of words in the @VocabularyFilterFileUri@ parameter, you can't use the @Words@ parameter.
-- * 'vocabularyFilterName' - The name of the vocabulary filter to update. If you try to update a vocabulary filter with the same name as another vocabulary filter, you get a @ConflictException@ error.
-- * 'words' - The words to use in the vocabulary filter. Only use characters from the character set defined for custom vocabularies. For a list of character sets, see <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies> .
--
-- If you provide a list of words in the @Words@ parameter, you can't use the @VocabularyFilterFileUri@ parameter.
mkUpdateVocabularyFilter ::
  -- | 'vocabularyFilterName'
  Lude.Text ->
  UpdateVocabularyFilter
mkUpdateVocabularyFilter pVocabularyFilterName_ =
  UpdateVocabularyFilter'
    { vocabularyFilterFileURI = Lude.Nothing,
      vocabularyFilterName = pVocabularyFilterName_,
      words = Lude.Nothing
    }

-- | The Amazon S3 location of a text file used as input to create the vocabulary filter. Only use characters from the character set defined for custom vocabularies. For a list of character sets, see <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies> .
--
-- The specified file must be less than 50 KB of UTF-8 characters.
-- If you provide the location of a list of words in the @VocabularyFilterFileUri@ parameter, you can't use the @Words@ parameter.
--
-- /Note:/ Consider using 'vocabularyFilterFileURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvfVocabularyFilterFileURI :: Lens.Lens' UpdateVocabularyFilter (Lude.Maybe Lude.Text)
uvfVocabularyFilterFileURI = Lens.lens (vocabularyFilterFileURI :: UpdateVocabularyFilter -> Lude.Maybe Lude.Text) (\s a -> s {vocabularyFilterFileURI = a} :: UpdateVocabularyFilter)
{-# DEPRECATED uvfVocabularyFilterFileURI "Use generic-lens or generic-optics with 'vocabularyFilterFileURI' instead." #-}

-- | The name of the vocabulary filter to update. If you try to update a vocabulary filter with the same name as another vocabulary filter, you get a @ConflictException@ error.
--
-- /Note:/ Consider using 'vocabularyFilterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvfVocabularyFilterName :: Lens.Lens' UpdateVocabularyFilter Lude.Text
uvfVocabularyFilterName = Lens.lens (vocabularyFilterName :: UpdateVocabularyFilter -> Lude.Text) (\s a -> s {vocabularyFilterName = a} :: UpdateVocabularyFilter)
{-# DEPRECATED uvfVocabularyFilterName "Use generic-lens or generic-optics with 'vocabularyFilterName' instead." #-}

-- | The words to use in the vocabulary filter. Only use characters from the character set defined for custom vocabularies. For a list of character sets, see <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies> .
--
-- If you provide a list of words in the @Words@ parameter, you can't use the @VocabularyFilterFileUri@ parameter.
--
-- /Note:/ Consider using 'words' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvfWords :: Lens.Lens' UpdateVocabularyFilter (Lude.Maybe (Lude.NonEmpty Lude.Text))
uvfWords = Lens.lens (words :: UpdateVocabularyFilter -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {words = a} :: UpdateVocabularyFilter)
{-# DEPRECATED uvfWords "Use generic-lens or generic-optics with 'words' instead." #-}

instance Lude.AWSRequest UpdateVocabularyFilter where
  type Rs UpdateVocabularyFilter = UpdateVocabularyFilterResponse
  request = Req.postJSON transcribeService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateVocabularyFilterResponse'
            Lude.<$> (x Lude..?> "LanguageCode")
            Lude.<*> (x Lude..?> "LastModifiedTime")
            Lude.<*> (x Lude..?> "VocabularyFilterName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateVocabularyFilter where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Transcribe.UpdateVocabularyFilter" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateVocabularyFilter where
  toJSON UpdateVocabularyFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("VocabularyFilterFileUri" Lude..=)
              Lude.<$> vocabularyFilterFileURI,
            Lude.Just ("VocabularyFilterName" Lude..= vocabularyFilterName),
            ("Words" Lude..=) Lude.<$> words
          ]
      )

instance Lude.ToPath UpdateVocabularyFilter where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateVocabularyFilter where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateVocabularyFilterResponse' smart constructor.
data UpdateVocabularyFilterResponse = UpdateVocabularyFilterResponse'
  { -- | The language code of the words in the vocabulary filter.
    languageCode :: Lude.Maybe LanguageCode,
    -- | The date and time that the vocabulary filter was updated.
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    -- | The name of the updated vocabulary filter.
    vocabularyFilterName :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateVocabularyFilterResponse' with the minimum fields required to make a request.
--
-- * 'languageCode' - The language code of the words in the vocabulary filter.
-- * 'lastModifiedTime' - The date and time that the vocabulary filter was updated.
-- * 'vocabularyFilterName' - The name of the updated vocabulary filter.
-- * 'responseStatus' - The response status code.
mkUpdateVocabularyFilterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateVocabularyFilterResponse
mkUpdateVocabularyFilterResponse pResponseStatus_ =
  UpdateVocabularyFilterResponse'
    { languageCode = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      vocabularyFilterName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The language code of the words in the vocabulary filter.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvfrsLanguageCode :: Lens.Lens' UpdateVocabularyFilterResponse (Lude.Maybe LanguageCode)
uvfrsLanguageCode = Lens.lens (languageCode :: UpdateVocabularyFilterResponse -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: UpdateVocabularyFilterResponse)
{-# DEPRECATED uvfrsLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The date and time that the vocabulary filter was updated.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvfrsLastModifiedTime :: Lens.Lens' UpdateVocabularyFilterResponse (Lude.Maybe Lude.Timestamp)
uvfrsLastModifiedTime = Lens.lens (lastModifiedTime :: UpdateVocabularyFilterResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: UpdateVocabularyFilterResponse)
{-# DEPRECATED uvfrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The name of the updated vocabulary filter.
--
-- /Note:/ Consider using 'vocabularyFilterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvfrsVocabularyFilterName :: Lens.Lens' UpdateVocabularyFilterResponse (Lude.Maybe Lude.Text)
uvfrsVocabularyFilterName = Lens.lens (vocabularyFilterName :: UpdateVocabularyFilterResponse -> Lude.Maybe Lude.Text) (\s a -> s {vocabularyFilterName = a} :: UpdateVocabularyFilterResponse)
{-# DEPRECATED uvfrsVocabularyFilterName "Use generic-lens or generic-optics with 'vocabularyFilterName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvfrsResponseStatus :: Lens.Lens' UpdateVocabularyFilterResponse Lude.Int
uvfrsResponseStatus = Lens.lens (responseStatus :: UpdateVocabularyFilterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateVocabularyFilterResponse)
{-# DEPRECATED uvfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
