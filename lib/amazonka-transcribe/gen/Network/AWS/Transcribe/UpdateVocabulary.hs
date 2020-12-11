{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.UpdateVocabulary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing vocabulary with new values. The @UpdateVocabulary@ operation overwrites all of the existing information with the values that you provide in the request.
module Network.AWS.Transcribe.UpdateVocabulary
  ( -- * Creating a request
    UpdateVocabulary (..),
    mkUpdateVocabulary,

    -- ** Request lenses
    uvVocabularyFileURI,
    uvPhrases,
    uvVocabularyName,
    uvLanguageCode,

    -- * Destructuring the response
    UpdateVocabularyResponse (..),
    mkUpdateVocabularyResponse,

    -- ** Response lenses
    uvrsLanguageCode,
    uvrsVocabularyName,
    uvrsLastModifiedTime,
    uvrsVocabularyState,
    uvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Transcribe.Types

-- | /See:/ 'mkUpdateVocabulary' smart constructor.
data UpdateVocabulary = UpdateVocabulary'
  { vocabularyFileURI ::
      Lude.Maybe Lude.Text,
    phrases :: Lude.Maybe [Lude.Text],
    vocabularyName :: Lude.Text,
    languageCode :: LanguageCode
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateVocabulary' with the minimum fields required to make a request.
--
-- * 'languageCode' - The language code of the vocabulary entries.
-- * 'phrases' - An array of strings containing the vocabulary entries.
-- * 'vocabularyFileURI' - The S3 location of the text file that contains the definition of the custom vocabulary. The URI must be in the same region as the API endpoint that you are calling. The general form is
--
-- For example:
-- For more information about S3 object names, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys> in the /Amazon S3 Developer Guide/ .
-- For more information about custom vocabularies, see <http://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary Custom Vocabularies> .
-- * 'vocabularyName' - The name of the vocabulary to update. The name is case sensitive. If you try to update a vocabulary with the same name as a previous vocabulary you will receive a @ConflictException@ error.
mkUpdateVocabulary ::
  -- | 'vocabularyName'
  Lude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  UpdateVocabulary
mkUpdateVocabulary pVocabularyName_ pLanguageCode_ =
  UpdateVocabulary'
    { vocabularyFileURI = Lude.Nothing,
      phrases = Lude.Nothing,
      vocabularyName = pVocabularyName_,
      languageCode = pLanguageCode_
    }

-- | The S3 location of the text file that contains the definition of the custom vocabulary. The URI must be in the same region as the API endpoint that you are calling. The general form is
--
-- For example:
-- For more information about S3 object names, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys> in the /Amazon S3 Developer Guide/ .
-- For more information about custom vocabularies, see <http://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary Custom Vocabularies> .
--
-- /Note:/ Consider using 'vocabularyFileURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvVocabularyFileURI :: Lens.Lens' UpdateVocabulary (Lude.Maybe Lude.Text)
uvVocabularyFileURI = Lens.lens (vocabularyFileURI :: UpdateVocabulary -> Lude.Maybe Lude.Text) (\s a -> s {vocabularyFileURI = a} :: UpdateVocabulary)
{-# DEPRECATED uvVocabularyFileURI "Use generic-lens or generic-optics with 'vocabularyFileURI' instead." #-}

-- | An array of strings containing the vocabulary entries.
--
-- /Note:/ Consider using 'phrases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvPhrases :: Lens.Lens' UpdateVocabulary (Lude.Maybe [Lude.Text])
uvPhrases = Lens.lens (phrases :: UpdateVocabulary -> Lude.Maybe [Lude.Text]) (\s a -> s {phrases = a} :: UpdateVocabulary)
{-# DEPRECATED uvPhrases "Use generic-lens or generic-optics with 'phrases' instead." #-}

-- | The name of the vocabulary to update. The name is case sensitive. If you try to update a vocabulary with the same name as a previous vocabulary you will receive a @ConflictException@ error.
--
-- /Note:/ Consider using 'vocabularyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvVocabularyName :: Lens.Lens' UpdateVocabulary Lude.Text
uvVocabularyName = Lens.lens (vocabularyName :: UpdateVocabulary -> Lude.Text) (\s a -> s {vocabularyName = a} :: UpdateVocabulary)
{-# DEPRECATED uvVocabularyName "Use generic-lens or generic-optics with 'vocabularyName' instead." #-}

-- | The language code of the vocabulary entries.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvLanguageCode :: Lens.Lens' UpdateVocabulary LanguageCode
uvLanguageCode = Lens.lens (languageCode :: UpdateVocabulary -> LanguageCode) (\s a -> s {languageCode = a} :: UpdateVocabulary)
{-# DEPRECATED uvLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

instance Lude.AWSRequest UpdateVocabulary where
  type Rs UpdateVocabulary = UpdateVocabularyResponse
  request = Req.postJSON transcribeService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateVocabularyResponse'
            Lude.<$> (x Lude..?> "LanguageCode")
            Lude.<*> (x Lude..?> "VocabularyName")
            Lude.<*> (x Lude..?> "LastModifiedTime")
            Lude.<*> (x Lude..?> "VocabularyState")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateVocabulary where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Transcribe.UpdateVocabulary" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateVocabulary where
  toJSON UpdateVocabulary' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("VocabularyFileUri" Lude..=) Lude.<$> vocabularyFileURI,
            ("Phrases" Lude..=) Lude.<$> phrases,
            Lude.Just ("VocabularyName" Lude..= vocabularyName),
            Lude.Just ("LanguageCode" Lude..= languageCode)
          ]
      )

instance Lude.ToPath UpdateVocabulary where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateVocabulary where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateVocabularyResponse' smart constructor.
data UpdateVocabularyResponse = UpdateVocabularyResponse'
  { languageCode ::
      Lude.Maybe LanguageCode,
    vocabularyName :: Lude.Maybe Lude.Text,
    lastModifiedTime ::
      Lude.Maybe Lude.Timestamp,
    vocabularyState ::
      Lude.Maybe VocabularyState,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateVocabularyResponse' with the minimum fields required to make a request.
--
-- * 'languageCode' - The language code of the vocabulary entries.
-- * 'lastModifiedTime' - The date and time that the vocabulary was updated.
-- * 'responseStatus' - The response status code.
-- * 'vocabularyName' - The name of the vocabulary that was updated.
-- * 'vocabularyState' - The processing state of the vocabulary. When the @VocabularyState@ field contains @READY@ the vocabulary is ready to be used in a @StartTranscriptionJob@ request.
mkUpdateVocabularyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateVocabularyResponse
mkUpdateVocabularyResponse pResponseStatus_ =
  UpdateVocabularyResponse'
    { languageCode = Lude.Nothing,
      vocabularyName = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      vocabularyState = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The language code of the vocabulary entries.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvrsLanguageCode :: Lens.Lens' UpdateVocabularyResponse (Lude.Maybe LanguageCode)
uvrsLanguageCode = Lens.lens (languageCode :: UpdateVocabularyResponse -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: UpdateVocabularyResponse)
{-# DEPRECATED uvrsLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The name of the vocabulary that was updated.
--
-- /Note:/ Consider using 'vocabularyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvrsVocabularyName :: Lens.Lens' UpdateVocabularyResponse (Lude.Maybe Lude.Text)
uvrsVocabularyName = Lens.lens (vocabularyName :: UpdateVocabularyResponse -> Lude.Maybe Lude.Text) (\s a -> s {vocabularyName = a} :: UpdateVocabularyResponse)
{-# DEPRECATED uvrsVocabularyName "Use generic-lens or generic-optics with 'vocabularyName' instead." #-}

-- | The date and time that the vocabulary was updated.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvrsLastModifiedTime :: Lens.Lens' UpdateVocabularyResponse (Lude.Maybe Lude.Timestamp)
uvrsLastModifiedTime = Lens.lens (lastModifiedTime :: UpdateVocabularyResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: UpdateVocabularyResponse)
{-# DEPRECATED uvrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The processing state of the vocabulary. When the @VocabularyState@ field contains @READY@ the vocabulary is ready to be used in a @StartTranscriptionJob@ request.
--
-- /Note:/ Consider using 'vocabularyState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvrsVocabularyState :: Lens.Lens' UpdateVocabularyResponse (Lude.Maybe VocabularyState)
uvrsVocabularyState = Lens.lens (vocabularyState :: UpdateVocabularyResponse -> Lude.Maybe VocabularyState) (\s a -> s {vocabularyState = a} :: UpdateVocabularyResponse)
{-# DEPRECATED uvrsVocabularyState "Use generic-lens or generic-optics with 'vocabularyState' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvrsResponseStatus :: Lens.Lens' UpdateVocabularyResponse Lude.Int
uvrsResponseStatus = Lens.lens (responseStatus :: UpdateVocabularyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateVocabularyResponse)
{-# DEPRECATED uvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
