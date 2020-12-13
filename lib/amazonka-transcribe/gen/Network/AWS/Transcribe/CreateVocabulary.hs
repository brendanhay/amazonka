{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.CreateVocabulary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new custom vocabulary that you can use to change the way Amazon Transcribe handles transcription of an audio file.
module Network.AWS.Transcribe.CreateVocabulary
  ( -- * Creating a request
    CreateVocabulary (..),
    mkCreateVocabulary,

    -- ** Request lenses
    cvLanguageCode,
    cvVocabularyFileURI,
    cvVocabularyName,
    cvPhrases,

    -- * Destructuring the response
    CreateVocabularyResponse (..),
    mkCreateVocabularyResponse,

    -- ** Response lenses
    cvrsFailureReason,
    cvrsLanguageCode,
    cvrsVocabularyName,
    cvrsLastModifiedTime,
    cvrsVocabularyState,
    cvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Transcribe.Types

-- | /See:/ 'mkCreateVocabulary' smart constructor.
data CreateVocabulary = CreateVocabulary'
  { -- | The language code of the vocabulary entries.
    languageCode :: LanguageCode,
    -- | The S3 location of the text file that contains the definition of the custom vocabulary. The URI must be in the same region as the API endpoint that you are calling. The general form is
    --
    -- For more information about S3 object names, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys> in the /Amazon S3 Developer Guide/ .
    -- For more information about custom vocabularies, see <http://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary Custom Vocabularies> .
    vocabularyFileURI :: Lude.Maybe Lude.Text,
    -- | The name of the vocabulary. The name must be unique within an AWS account. The name is case sensitive. If you try to create a vocabulary with the same name as a previous vocabulary you will receive a @ConflictException@ error.
    vocabularyName :: Lude.Text,
    -- | An array of strings that contains the vocabulary entries.
    phrases :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateVocabulary' with the minimum fields required to make a request.
--
-- * 'languageCode' - The language code of the vocabulary entries.
-- * 'vocabularyFileURI' - The S3 location of the text file that contains the definition of the custom vocabulary. The URI must be in the same region as the API endpoint that you are calling. The general form is
--
-- For more information about S3 object names, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys> in the /Amazon S3 Developer Guide/ .
-- For more information about custom vocabularies, see <http://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary Custom Vocabularies> .
-- * 'vocabularyName' - The name of the vocabulary. The name must be unique within an AWS account. The name is case sensitive. If you try to create a vocabulary with the same name as a previous vocabulary you will receive a @ConflictException@ error.
-- * 'phrases' - An array of strings that contains the vocabulary entries.
mkCreateVocabulary ::
  -- | 'languageCode'
  LanguageCode ->
  -- | 'vocabularyName'
  Lude.Text ->
  CreateVocabulary
mkCreateVocabulary pLanguageCode_ pVocabularyName_ =
  CreateVocabulary'
    { languageCode = pLanguageCode_,
      vocabularyFileURI = Lude.Nothing,
      vocabularyName = pVocabularyName_,
      phrases = Lude.Nothing
    }

-- | The language code of the vocabulary entries.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvLanguageCode :: Lens.Lens' CreateVocabulary LanguageCode
cvLanguageCode = Lens.lens (languageCode :: CreateVocabulary -> LanguageCode) (\s a -> s {languageCode = a} :: CreateVocabulary)
{-# DEPRECATED cvLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The S3 location of the text file that contains the definition of the custom vocabulary. The URI must be in the same region as the API endpoint that you are calling. The general form is
--
-- For more information about S3 object names, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys> in the /Amazon S3 Developer Guide/ .
-- For more information about custom vocabularies, see <http://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary Custom Vocabularies> .
--
-- /Note:/ Consider using 'vocabularyFileURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvVocabularyFileURI :: Lens.Lens' CreateVocabulary (Lude.Maybe Lude.Text)
cvVocabularyFileURI = Lens.lens (vocabularyFileURI :: CreateVocabulary -> Lude.Maybe Lude.Text) (\s a -> s {vocabularyFileURI = a} :: CreateVocabulary)
{-# DEPRECATED cvVocabularyFileURI "Use generic-lens or generic-optics with 'vocabularyFileURI' instead." #-}

-- | The name of the vocabulary. The name must be unique within an AWS account. The name is case sensitive. If you try to create a vocabulary with the same name as a previous vocabulary you will receive a @ConflictException@ error.
--
-- /Note:/ Consider using 'vocabularyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvVocabularyName :: Lens.Lens' CreateVocabulary Lude.Text
cvVocabularyName = Lens.lens (vocabularyName :: CreateVocabulary -> Lude.Text) (\s a -> s {vocabularyName = a} :: CreateVocabulary)
{-# DEPRECATED cvVocabularyName "Use generic-lens or generic-optics with 'vocabularyName' instead." #-}

-- | An array of strings that contains the vocabulary entries.
--
-- /Note:/ Consider using 'phrases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvPhrases :: Lens.Lens' CreateVocabulary (Lude.Maybe [Lude.Text])
cvPhrases = Lens.lens (phrases :: CreateVocabulary -> Lude.Maybe [Lude.Text]) (\s a -> s {phrases = a} :: CreateVocabulary)
{-# DEPRECATED cvPhrases "Use generic-lens or generic-optics with 'phrases' instead." #-}

instance Lude.AWSRequest CreateVocabulary where
  type Rs CreateVocabulary = CreateVocabularyResponse
  request = Req.postJSON transcribeService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateVocabularyResponse'
            Lude.<$> (x Lude..?> "FailureReason")
            Lude.<*> (x Lude..?> "LanguageCode")
            Lude.<*> (x Lude..?> "VocabularyName")
            Lude.<*> (x Lude..?> "LastModifiedTime")
            Lude.<*> (x Lude..?> "VocabularyState")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateVocabulary where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Transcribe.CreateVocabulary" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateVocabulary where
  toJSON CreateVocabulary' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("LanguageCode" Lude..= languageCode),
            ("VocabularyFileUri" Lude..=) Lude.<$> vocabularyFileURI,
            Lude.Just ("VocabularyName" Lude..= vocabularyName),
            ("Phrases" Lude..=) Lude.<$> phrases
          ]
      )

instance Lude.ToPath CreateVocabulary where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateVocabulary where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateVocabularyResponse' smart constructor.
data CreateVocabularyResponse = CreateVocabularyResponse'
  { -- | If the @VocabularyState@ field is @FAILED@ , this field contains information about why the job failed.
    failureReason :: Lude.Maybe Lude.Text,
    -- | The language code of the vocabulary entries.
    languageCode :: Lude.Maybe LanguageCode,
    -- | The name of the vocabulary.
    vocabularyName :: Lude.Maybe Lude.Text,
    -- | The date and time that the vocabulary was created.
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    -- | The processing state of the vocabulary. When the @VocabularyState@ field contains @READY@ the vocabulary is ready to be used in a @StartTranscriptionJob@ request.
    vocabularyState :: Lude.Maybe VocabularyState,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateVocabularyResponse' with the minimum fields required to make a request.
--
-- * 'failureReason' - If the @VocabularyState@ field is @FAILED@ , this field contains information about why the job failed.
-- * 'languageCode' - The language code of the vocabulary entries.
-- * 'vocabularyName' - The name of the vocabulary.
-- * 'lastModifiedTime' - The date and time that the vocabulary was created.
-- * 'vocabularyState' - The processing state of the vocabulary. When the @VocabularyState@ field contains @READY@ the vocabulary is ready to be used in a @StartTranscriptionJob@ request.
-- * 'responseStatus' - The response status code.
mkCreateVocabularyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateVocabularyResponse
mkCreateVocabularyResponse pResponseStatus_ =
  CreateVocabularyResponse'
    { failureReason = Lude.Nothing,
      languageCode = Lude.Nothing,
      vocabularyName = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      vocabularyState = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the @VocabularyState@ field is @FAILED@ , this field contains information about why the job failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrsFailureReason :: Lens.Lens' CreateVocabularyResponse (Lude.Maybe Lude.Text)
cvrsFailureReason = Lens.lens (failureReason :: CreateVocabularyResponse -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: CreateVocabularyResponse)
{-# DEPRECATED cvrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The language code of the vocabulary entries.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrsLanguageCode :: Lens.Lens' CreateVocabularyResponse (Lude.Maybe LanguageCode)
cvrsLanguageCode = Lens.lens (languageCode :: CreateVocabularyResponse -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: CreateVocabularyResponse)
{-# DEPRECATED cvrsLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The name of the vocabulary.
--
-- /Note:/ Consider using 'vocabularyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrsVocabularyName :: Lens.Lens' CreateVocabularyResponse (Lude.Maybe Lude.Text)
cvrsVocabularyName = Lens.lens (vocabularyName :: CreateVocabularyResponse -> Lude.Maybe Lude.Text) (\s a -> s {vocabularyName = a} :: CreateVocabularyResponse)
{-# DEPRECATED cvrsVocabularyName "Use generic-lens or generic-optics with 'vocabularyName' instead." #-}

-- | The date and time that the vocabulary was created.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrsLastModifiedTime :: Lens.Lens' CreateVocabularyResponse (Lude.Maybe Lude.Timestamp)
cvrsLastModifiedTime = Lens.lens (lastModifiedTime :: CreateVocabularyResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: CreateVocabularyResponse)
{-# DEPRECATED cvrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The processing state of the vocabulary. When the @VocabularyState@ field contains @READY@ the vocabulary is ready to be used in a @StartTranscriptionJob@ request.
--
-- /Note:/ Consider using 'vocabularyState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrsVocabularyState :: Lens.Lens' CreateVocabularyResponse (Lude.Maybe VocabularyState)
cvrsVocabularyState = Lens.lens (vocabularyState :: CreateVocabularyResponse -> Lude.Maybe VocabularyState) (\s a -> s {vocabularyState = a} :: CreateVocabularyResponse)
{-# DEPRECATED cvrsVocabularyState "Use generic-lens or generic-optics with 'vocabularyState' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrsResponseStatus :: Lens.Lens' CreateVocabularyResponse Lude.Int
cvrsResponseStatus = Lens.lens (responseStatus :: CreateVocabularyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateVocabularyResponse)
{-# DEPRECATED cvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
