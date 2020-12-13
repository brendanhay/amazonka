{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.CreateVocabularyFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new vocabulary filter that you can use to filter words, such as profane words, from the output of a transcription job.
module Network.AWS.Transcribe.CreateVocabularyFilter
  ( -- * Creating a request
    CreateVocabularyFilter (..),
    mkCreateVocabularyFilter,

    -- ** Request lenses
    cvfLanguageCode,
    cvfVocabularyFilterFileURI,
    cvfVocabularyFilterName,
    cvfWords,

    -- * Destructuring the response
    CreateVocabularyFilterResponse (..),
    mkCreateVocabularyFilterResponse,

    -- ** Response lenses
    cvfrsLanguageCode,
    cvfrsLastModifiedTime,
    cvfrsVocabularyFilterName,
    cvfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Transcribe.Types

-- | /See:/ 'mkCreateVocabularyFilter' smart constructor.
data CreateVocabularyFilter = CreateVocabularyFilter'
  { -- | The language code of the words in the vocabulary filter. All words in the filter must be in the same language. The vocabulary filter can only be used with transcription jobs in the specified language.
    languageCode :: LanguageCode,
    -- | The Amazon S3 location of a text file used as input to create the vocabulary filter. Only use characters from the character set defined for custom vocabularies. For a list of character sets, see <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies> .
    --
    -- The specified file must be less than 50 KB of UTF-8 characters.
    -- If you provide the location of a list of words in the @VocabularyFilterFileUri@ parameter, you can't use the @Words@ parameter.
    vocabularyFilterFileURI :: Lude.Maybe Lude.Text,
    -- | The vocabulary filter name. The name must be unique within the account that contains it. If you try to create a vocabulary filter with the same name as another vocabulary filter, you get a @ConflictException@ error.
    vocabularyFilterName :: Lude.Text,
    -- | The words to use in the vocabulary filter. Only use characters from the character set defined for custom vocabularies. For a list of character sets, see <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies> .
    --
    -- If you provide a list of words in the @Words@ parameter, you can't use the @VocabularyFilterFileUri@ parameter.
    words :: Lude.Maybe (Lude.NonEmpty Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateVocabularyFilter' with the minimum fields required to make a request.
--
-- * 'languageCode' - The language code of the words in the vocabulary filter. All words in the filter must be in the same language. The vocabulary filter can only be used with transcription jobs in the specified language.
-- * 'vocabularyFilterFileURI' - The Amazon S3 location of a text file used as input to create the vocabulary filter. Only use characters from the character set defined for custom vocabularies. For a list of character sets, see <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies> .
--
-- The specified file must be less than 50 KB of UTF-8 characters.
-- If you provide the location of a list of words in the @VocabularyFilterFileUri@ parameter, you can't use the @Words@ parameter.
-- * 'vocabularyFilterName' - The vocabulary filter name. The name must be unique within the account that contains it. If you try to create a vocabulary filter with the same name as another vocabulary filter, you get a @ConflictException@ error.
-- * 'words' - The words to use in the vocabulary filter. Only use characters from the character set defined for custom vocabularies. For a list of character sets, see <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies> .
--
-- If you provide a list of words in the @Words@ parameter, you can't use the @VocabularyFilterFileUri@ parameter.
mkCreateVocabularyFilter ::
  -- | 'languageCode'
  LanguageCode ->
  -- | 'vocabularyFilterName'
  Lude.Text ->
  CreateVocabularyFilter
mkCreateVocabularyFilter pLanguageCode_ pVocabularyFilterName_ =
  CreateVocabularyFilter'
    { languageCode = pLanguageCode_,
      vocabularyFilterFileURI = Lude.Nothing,
      vocabularyFilterName = pVocabularyFilterName_,
      words = Lude.Nothing
    }

-- | The language code of the words in the vocabulary filter. All words in the filter must be in the same language. The vocabulary filter can only be used with transcription jobs in the specified language.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvfLanguageCode :: Lens.Lens' CreateVocabularyFilter LanguageCode
cvfLanguageCode = Lens.lens (languageCode :: CreateVocabularyFilter -> LanguageCode) (\s a -> s {languageCode = a} :: CreateVocabularyFilter)
{-# DEPRECATED cvfLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The Amazon S3 location of a text file used as input to create the vocabulary filter. Only use characters from the character set defined for custom vocabularies. For a list of character sets, see <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies> .
--
-- The specified file must be less than 50 KB of UTF-8 characters.
-- If you provide the location of a list of words in the @VocabularyFilterFileUri@ parameter, you can't use the @Words@ parameter.
--
-- /Note:/ Consider using 'vocabularyFilterFileURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvfVocabularyFilterFileURI :: Lens.Lens' CreateVocabularyFilter (Lude.Maybe Lude.Text)
cvfVocabularyFilterFileURI = Lens.lens (vocabularyFilterFileURI :: CreateVocabularyFilter -> Lude.Maybe Lude.Text) (\s a -> s {vocabularyFilterFileURI = a} :: CreateVocabularyFilter)
{-# DEPRECATED cvfVocabularyFilterFileURI "Use generic-lens or generic-optics with 'vocabularyFilterFileURI' instead." #-}

-- | The vocabulary filter name. The name must be unique within the account that contains it. If you try to create a vocabulary filter with the same name as another vocabulary filter, you get a @ConflictException@ error.
--
-- /Note:/ Consider using 'vocabularyFilterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvfVocabularyFilterName :: Lens.Lens' CreateVocabularyFilter Lude.Text
cvfVocabularyFilterName = Lens.lens (vocabularyFilterName :: CreateVocabularyFilter -> Lude.Text) (\s a -> s {vocabularyFilterName = a} :: CreateVocabularyFilter)
{-# DEPRECATED cvfVocabularyFilterName "Use generic-lens or generic-optics with 'vocabularyFilterName' instead." #-}

-- | The words to use in the vocabulary filter. Only use characters from the character set defined for custom vocabularies. For a list of character sets, see <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies> .
--
-- If you provide a list of words in the @Words@ parameter, you can't use the @VocabularyFilterFileUri@ parameter.
--
-- /Note:/ Consider using 'words' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvfWords :: Lens.Lens' CreateVocabularyFilter (Lude.Maybe (Lude.NonEmpty Lude.Text))
cvfWords = Lens.lens (words :: CreateVocabularyFilter -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {words = a} :: CreateVocabularyFilter)
{-# DEPRECATED cvfWords "Use generic-lens or generic-optics with 'words' instead." #-}

instance Lude.AWSRequest CreateVocabularyFilter where
  type Rs CreateVocabularyFilter = CreateVocabularyFilterResponse
  request = Req.postJSON transcribeService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateVocabularyFilterResponse'
            Lude.<$> (x Lude..?> "LanguageCode")
            Lude.<*> (x Lude..?> "LastModifiedTime")
            Lude.<*> (x Lude..?> "VocabularyFilterName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateVocabularyFilter where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Transcribe.CreateVocabularyFilter" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateVocabularyFilter where
  toJSON CreateVocabularyFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("LanguageCode" Lude..= languageCode),
            ("VocabularyFilterFileUri" Lude..=)
              Lude.<$> vocabularyFilterFileURI,
            Lude.Just ("VocabularyFilterName" Lude..= vocabularyFilterName),
            ("Words" Lude..=) Lude.<$> words
          ]
      )

instance Lude.ToPath CreateVocabularyFilter where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateVocabularyFilter where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateVocabularyFilterResponse' smart constructor.
data CreateVocabularyFilterResponse = CreateVocabularyFilterResponse'
  { -- | The language code of the words in the collection.
    languageCode :: Lude.Maybe LanguageCode,
    -- | The date and time that the vocabulary filter was modified.
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    -- | The name of the vocabulary filter.
    vocabularyFilterName :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateVocabularyFilterResponse' with the minimum fields required to make a request.
--
-- * 'languageCode' - The language code of the words in the collection.
-- * 'lastModifiedTime' - The date and time that the vocabulary filter was modified.
-- * 'vocabularyFilterName' - The name of the vocabulary filter.
-- * 'responseStatus' - The response status code.
mkCreateVocabularyFilterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateVocabularyFilterResponse
mkCreateVocabularyFilterResponse pResponseStatus_ =
  CreateVocabularyFilterResponse'
    { languageCode = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      vocabularyFilterName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The language code of the words in the collection.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvfrsLanguageCode :: Lens.Lens' CreateVocabularyFilterResponse (Lude.Maybe LanguageCode)
cvfrsLanguageCode = Lens.lens (languageCode :: CreateVocabularyFilterResponse -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: CreateVocabularyFilterResponse)
{-# DEPRECATED cvfrsLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The date and time that the vocabulary filter was modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvfrsLastModifiedTime :: Lens.Lens' CreateVocabularyFilterResponse (Lude.Maybe Lude.Timestamp)
cvfrsLastModifiedTime = Lens.lens (lastModifiedTime :: CreateVocabularyFilterResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: CreateVocabularyFilterResponse)
{-# DEPRECATED cvfrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The name of the vocabulary filter.
--
-- /Note:/ Consider using 'vocabularyFilterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvfrsVocabularyFilterName :: Lens.Lens' CreateVocabularyFilterResponse (Lude.Maybe Lude.Text)
cvfrsVocabularyFilterName = Lens.lens (vocabularyFilterName :: CreateVocabularyFilterResponse -> Lude.Maybe Lude.Text) (\s a -> s {vocabularyFilterName = a} :: CreateVocabularyFilterResponse)
{-# DEPRECATED cvfrsVocabularyFilterName "Use generic-lens or generic-optics with 'vocabularyFilterName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvfrsResponseStatus :: Lens.Lens' CreateVocabularyFilterResponse Lude.Int
cvfrsResponseStatus = Lens.lens (responseStatus :: CreateVocabularyFilterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateVocabularyFilterResponse)
{-# DEPRECATED cvfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
