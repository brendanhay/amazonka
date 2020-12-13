{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.CreateMedicalVocabulary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new custom vocabulary that you can use to change how Amazon Transcribe Medical transcribes your audio file.
module Network.AWS.Transcribe.CreateMedicalVocabulary
  ( -- * Creating a request
    CreateMedicalVocabulary (..),
    mkCreateMedicalVocabulary,

    -- ** Request lenses
    cmvLanguageCode,
    cmvVocabularyFileURI,
    cmvVocabularyName,

    -- * Destructuring the response
    CreateMedicalVocabularyResponse (..),
    mkCreateMedicalVocabularyResponse,

    -- ** Response lenses
    cmvrsFailureReason,
    cmvrsLanguageCode,
    cmvrsVocabularyName,
    cmvrsLastModifiedTime,
    cmvrsVocabularyState,
    cmvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Transcribe.Types

-- | /See:/ 'mkCreateMedicalVocabulary' smart constructor.
data CreateMedicalVocabulary = CreateMedicalVocabulary'
  { -- | The language code for the language used for the entries in your custom vocabulary. The language code of your custom vocabulary must match the language code of your transcription job. US English (en-US) is the only language code available for Amazon Transcribe Medical.
    languageCode :: LanguageCode,
    -- | The location in Amazon S3 of the text file you use to define your custom vocabulary. The URI must be in the same AWS Region as the resource that you're calling. Enter information about your @VocabularyFileUri@ in the following format:
    --
    -- @https://s3.<aws-region>.amazonaws.com/<bucket-name>/<keyprefix>/<objectkey> @
    -- The following is an example URI for a vocabulary file that is stored in Amazon S3:
    -- @https://s3.us-east-1.amazonaws.com/AWSDOC-EXAMPLE-BUCKET/vocab.txt@
    -- For more information about Amazon S3 object names, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys> in the /Amazon S3 Developer Guide/ .
    -- For more information about custom vocabularies, see <http://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary-med Medical Custom Vocabularies> .
    vocabularyFileURI :: Lude.Text,
    -- | The name of the custom vocabulary. This case-sensitive name must be unique within an AWS account. If you try to create a vocabulary with the same name as a previous vocabulary, you get a @ConflictException@ error.
    vocabularyName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateMedicalVocabulary' with the minimum fields required to make a request.
--
-- * 'languageCode' - The language code for the language used for the entries in your custom vocabulary. The language code of your custom vocabulary must match the language code of your transcription job. US English (en-US) is the only language code available for Amazon Transcribe Medical.
-- * 'vocabularyFileURI' - The location in Amazon S3 of the text file you use to define your custom vocabulary. The URI must be in the same AWS Region as the resource that you're calling. Enter information about your @VocabularyFileUri@ in the following format:
--
-- @https://s3.<aws-region>.amazonaws.com/<bucket-name>/<keyprefix>/<objectkey> @
-- The following is an example URI for a vocabulary file that is stored in Amazon S3:
-- @https://s3.us-east-1.amazonaws.com/AWSDOC-EXAMPLE-BUCKET/vocab.txt@
-- For more information about Amazon S3 object names, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys> in the /Amazon S3 Developer Guide/ .
-- For more information about custom vocabularies, see <http://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary-med Medical Custom Vocabularies> .
-- * 'vocabularyName' - The name of the custom vocabulary. This case-sensitive name must be unique within an AWS account. If you try to create a vocabulary with the same name as a previous vocabulary, you get a @ConflictException@ error.
mkCreateMedicalVocabulary ::
  -- | 'languageCode'
  LanguageCode ->
  -- | 'vocabularyFileURI'
  Lude.Text ->
  -- | 'vocabularyName'
  Lude.Text ->
  CreateMedicalVocabulary
mkCreateMedicalVocabulary
  pLanguageCode_
  pVocabularyFileURI_
  pVocabularyName_ =
    CreateMedicalVocabulary'
      { languageCode = pLanguageCode_,
        vocabularyFileURI = pVocabularyFileURI_,
        vocabularyName = pVocabularyName_
      }

-- | The language code for the language used for the entries in your custom vocabulary. The language code of your custom vocabulary must match the language code of your transcription job. US English (en-US) is the only language code available for Amazon Transcribe Medical.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmvLanguageCode :: Lens.Lens' CreateMedicalVocabulary LanguageCode
cmvLanguageCode = Lens.lens (languageCode :: CreateMedicalVocabulary -> LanguageCode) (\s a -> s {languageCode = a} :: CreateMedicalVocabulary)
{-# DEPRECATED cmvLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The location in Amazon S3 of the text file you use to define your custom vocabulary. The URI must be in the same AWS Region as the resource that you're calling. Enter information about your @VocabularyFileUri@ in the following format:
--
-- @https://s3.<aws-region>.amazonaws.com/<bucket-name>/<keyprefix>/<objectkey> @
-- The following is an example URI for a vocabulary file that is stored in Amazon S3:
-- @https://s3.us-east-1.amazonaws.com/AWSDOC-EXAMPLE-BUCKET/vocab.txt@
-- For more information about Amazon S3 object names, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys> in the /Amazon S3 Developer Guide/ .
-- For more information about custom vocabularies, see <http://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary-med Medical Custom Vocabularies> .
--
-- /Note:/ Consider using 'vocabularyFileURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmvVocabularyFileURI :: Lens.Lens' CreateMedicalVocabulary Lude.Text
cmvVocabularyFileURI = Lens.lens (vocabularyFileURI :: CreateMedicalVocabulary -> Lude.Text) (\s a -> s {vocabularyFileURI = a} :: CreateMedicalVocabulary)
{-# DEPRECATED cmvVocabularyFileURI "Use generic-lens or generic-optics with 'vocabularyFileURI' instead." #-}

-- | The name of the custom vocabulary. This case-sensitive name must be unique within an AWS account. If you try to create a vocabulary with the same name as a previous vocabulary, you get a @ConflictException@ error.
--
-- /Note:/ Consider using 'vocabularyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmvVocabularyName :: Lens.Lens' CreateMedicalVocabulary Lude.Text
cmvVocabularyName = Lens.lens (vocabularyName :: CreateMedicalVocabulary -> Lude.Text) (\s a -> s {vocabularyName = a} :: CreateMedicalVocabulary)
{-# DEPRECATED cmvVocabularyName "Use generic-lens or generic-optics with 'vocabularyName' instead." #-}

instance Lude.AWSRequest CreateMedicalVocabulary where
  type Rs CreateMedicalVocabulary = CreateMedicalVocabularyResponse
  request = Req.postJSON transcribeService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateMedicalVocabularyResponse'
            Lude.<$> (x Lude..?> "FailureReason")
            Lude.<*> (x Lude..?> "LanguageCode")
            Lude.<*> (x Lude..?> "VocabularyName")
            Lude.<*> (x Lude..?> "LastModifiedTime")
            Lude.<*> (x Lude..?> "VocabularyState")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateMedicalVocabulary where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Transcribe.CreateMedicalVocabulary" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateMedicalVocabulary where
  toJSON CreateMedicalVocabulary' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("LanguageCode" Lude..= languageCode),
            Lude.Just ("VocabularyFileUri" Lude..= vocabularyFileURI),
            Lude.Just ("VocabularyName" Lude..= vocabularyName)
          ]
      )

instance Lude.ToPath CreateMedicalVocabulary where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateMedicalVocabulary where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateMedicalVocabularyResponse' smart constructor.
data CreateMedicalVocabularyResponse = CreateMedicalVocabularyResponse'
  { -- | If the @VocabularyState@ field is @FAILED@ , this field contains information about why the job failed.
    failureReason :: Lude.Maybe Lude.Text,
    -- | The language code for the entries in your custom vocabulary. US English (en-US) is the only valid language code for Amazon Transcribe Medical.
    languageCode :: Lude.Maybe LanguageCode,
    -- | The name of the vocabulary. The name must be unique within an AWS account and is case sensitive.
    vocabularyName :: Lude.Maybe Lude.Text,
    -- | The date and time that you created the vocabulary.
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    -- | The processing state of your custom vocabulary in Amazon Transcribe Medical. If the state is @READY@ , you can use the vocabulary in a @StartMedicalTranscriptionJob@ request.
    vocabularyState :: Lude.Maybe VocabularyState,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateMedicalVocabularyResponse' with the minimum fields required to make a request.
--
-- * 'failureReason' - If the @VocabularyState@ field is @FAILED@ , this field contains information about why the job failed.
-- * 'languageCode' - The language code for the entries in your custom vocabulary. US English (en-US) is the only valid language code for Amazon Transcribe Medical.
-- * 'vocabularyName' - The name of the vocabulary. The name must be unique within an AWS account and is case sensitive.
-- * 'lastModifiedTime' - The date and time that you created the vocabulary.
-- * 'vocabularyState' - The processing state of your custom vocabulary in Amazon Transcribe Medical. If the state is @READY@ , you can use the vocabulary in a @StartMedicalTranscriptionJob@ request.
-- * 'responseStatus' - The response status code.
mkCreateMedicalVocabularyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateMedicalVocabularyResponse
mkCreateMedicalVocabularyResponse pResponseStatus_ =
  CreateMedicalVocabularyResponse'
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
cmvrsFailureReason :: Lens.Lens' CreateMedicalVocabularyResponse (Lude.Maybe Lude.Text)
cmvrsFailureReason = Lens.lens (failureReason :: CreateMedicalVocabularyResponse -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: CreateMedicalVocabularyResponse)
{-# DEPRECATED cmvrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The language code for the entries in your custom vocabulary. US English (en-US) is the only valid language code for Amazon Transcribe Medical.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmvrsLanguageCode :: Lens.Lens' CreateMedicalVocabularyResponse (Lude.Maybe LanguageCode)
cmvrsLanguageCode = Lens.lens (languageCode :: CreateMedicalVocabularyResponse -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: CreateMedicalVocabularyResponse)
{-# DEPRECATED cmvrsLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The name of the vocabulary. The name must be unique within an AWS account and is case sensitive.
--
-- /Note:/ Consider using 'vocabularyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmvrsVocabularyName :: Lens.Lens' CreateMedicalVocabularyResponse (Lude.Maybe Lude.Text)
cmvrsVocabularyName = Lens.lens (vocabularyName :: CreateMedicalVocabularyResponse -> Lude.Maybe Lude.Text) (\s a -> s {vocabularyName = a} :: CreateMedicalVocabularyResponse)
{-# DEPRECATED cmvrsVocabularyName "Use generic-lens or generic-optics with 'vocabularyName' instead." #-}

-- | The date and time that you created the vocabulary.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmvrsLastModifiedTime :: Lens.Lens' CreateMedicalVocabularyResponse (Lude.Maybe Lude.Timestamp)
cmvrsLastModifiedTime = Lens.lens (lastModifiedTime :: CreateMedicalVocabularyResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: CreateMedicalVocabularyResponse)
{-# DEPRECATED cmvrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The processing state of your custom vocabulary in Amazon Transcribe Medical. If the state is @READY@ , you can use the vocabulary in a @StartMedicalTranscriptionJob@ request.
--
-- /Note:/ Consider using 'vocabularyState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmvrsVocabularyState :: Lens.Lens' CreateMedicalVocabularyResponse (Lude.Maybe VocabularyState)
cmvrsVocabularyState = Lens.lens (vocabularyState :: CreateMedicalVocabularyResponse -> Lude.Maybe VocabularyState) (\s a -> s {vocabularyState = a} :: CreateMedicalVocabularyResponse)
{-# DEPRECATED cmvrsVocabularyState "Use generic-lens or generic-optics with 'vocabularyState' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmvrsResponseStatus :: Lens.Lens' CreateMedicalVocabularyResponse Lude.Int
cmvrsResponseStatus = Lens.lens (responseStatus :: CreateMedicalVocabularyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateMedicalVocabularyResponse)
{-# DEPRECATED cmvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
