{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.UpdateMedicalVocabulary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a vocabulary with new values that you provide in a different text file from the one you used to create the vocabulary. The @UpdateMedicalVocabulary@ operation overwrites all of the existing information with the values that you provide in the request.
module Network.AWS.Transcribe.UpdateMedicalVocabulary
  ( -- * Creating a request
    UpdateMedicalVocabulary (..),
    mkUpdateMedicalVocabulary,

    -- ** Request lenses
    umvVocabularyFileURI,
    umvVocabularyName,
    umvLanguageCode,

    -- * Destructuring the response
    UpdateMedicalVocabularyResponse (..),
    mkUpdateMedicalVocabularyResponse,

    -- ** Response lenses
    umvrsLanguageCode,
    umvrsVocabularyName,
    umvrsLastModifiedTime,
    umvrsVocabularyState,
    umvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Transcribe.Types

-- | /See:/ 'mkUpdateMedicalVocabulary' smart constructor.
data UpdateMedicalVocabulary = UpdateMedicalVocabulary'
  { vocabularyFileURI ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'UpdateMedicalVocabulary' with the minimum fields required to make a request.
--
-- * 'languageCode' - The language code of the language used for the entries in the updated vocabulary. US English (en-US) is the only valid language code in Amazon Transcribe Medical.
-- * 'vocabularyFileURI' - The location in Amazon S3 of the text file that contains the you use for your custom vocabulary. The URI must be in the same AWS Region as the resource that you are calling. The following is the format for a URI:
--
-- @https://s3.<aws-region>.amazonaws.com/<bucket-name>/<keyprefix>/<objectkey> @
-- For example:
-- @https://s3.us-east-1.amazonaws.com/AWSDOC-EXAMPLE-BUCKET/vocab.txt@
-- For more information about Amazon S3 object names, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys> in the /Amazon S3 Developer Guide/ .
-- For more information about custom vocabularies in Amazon Transcribe Medical, see <http://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary Medical Custom Vocabularies> .
-- * 'vocabularyName' - The name of the vocabulary to update. The name is case sensitive. If you try to update a vocabulary with the same name as a vocabulary you've already made, you get a @ConflictException@ error.
mkUpdateMedicalVocabulary ::
  -- | 'vocabularyName'
  Lude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  UpdateMedicalVocabulary
mkUpdateMedicalVocabulary pVocabularyName_ pLanguageCode_ =
  UpdateMedicalVocabulary'
    { vocabularyFileURI = Lude.Nothing,
      vocabularyName = pVocabularyName_,
      languageCode = pLanguageCode_
    }

-- | The location in Amazon S3 of the text file that contains the you use for your custom vocabulary. The URI must be in the same AWS Region as the resource that you are calling. The following is the format for a URI:
--
-- @https://s3.<aws-region>.amazonaws.com/<bucket-name>/<keyprefix>/<objectkey> @
-- For example:
-- @https://s3.us-east-1.amazonaws.com/AWSDOC-EXAMPLE-BUCKET/vocab.txt@
-- For more information about Amazon S3 object names, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys> in the /Amazon S3 Developer Guide/ .
-- For more information about custom vocabularies in Amazon Transcribe Medical, see <http://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary Medical Custom Vocabularies> .
--
-- /Note:/ Consider using 'vocabularyFileURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umvVocabularyFileURI :: Lens.Lens' UpdateMedicalVocabulary (Lude.Maybe Lude.Text)
umvVocabularyFileURI = Lens.lens (vocabularyFileURI :: UpdateMedicalVocabulary -> Lude.Maybe Lude.Text) (\s a -> s {vocabularyFileURI = a} :: UpdateMedicalVocabulary)
{-# DEPRECATED umvVocabularyFileURI "Use generic-lens or generic-optics with 'vocabularyFileURI' instead." #-}

-- | The name of the vocabulary to update. The name is case sensitive. If you try to update a vocabulary with the same name as a vocabulary you've already made, you get a @ConflictException@ error.
--
-- /Note:/ Consider using 'vocabularyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umvVocabularyName :: Lens.Lens' UpdateMedicalVocabulary Lude.Text
umvVocabularyName = Lens.lens (vocabularyName :: UpdateMedicalVocabulary -> Lude.Text) (\s a -> s {vocabularyName = a} :: UpdateMedicalVocabulary)
{-# DEPRECATED umvVocabularyName "Use generic-lens or generic-optics with 'vocabularyName' instead." #-}

-- | The language code of the language used for the entries in the updated vocabulary. US English (en-US) is the only valid language code in Amazon Transcribe Medical.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umvLanguageCode :: Lens.Lens' UpdateMedicalVocabulary LanguageCode
umvLanguageCode = Lens.lens (languageCode :: UpdateMedicalVocabulary -> LanguageCode) (\s a -> s {languageCode = a} :: UpdateMedicalVocabulary)
{-# DEPRECATED umvLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

instance Lude.AWSRequest UpdateMedicalVocabulary where
  type Rs UpdateMedicalVocabulary = UpdateMedicalVocabularyResponse
  request = Req.postJSON transcribeService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateMedicalVocabularyResponse'
            Lude.<$> (x Lude..?> "LanguageCode")
            Lude.<*> (x Lude..?> "VocabularyName")
            Lude.<*> (x Lude..?> "LastModifiedTime")
            Lude.<*> (x Lude..?> "VocabularyState")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateMedicalVocabulary where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Transcribe.UpdateMedicalVocabulary" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateMedicalVocabulary where
  toJSON UpdateMedicalVocabulary' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("VocabularyFileUri" Lude..=) Lude.<$> vocabularyFileURI,
            Lude.Just ("VocabularyName" Lude..= vocabularyName),
            Lude.Just ("LanguageCode" Lude..= languageCode)
          ]
      )

instance Lude.ToPath UpdateMedicalVocabulary where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateMedicalVocabulary where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateMedicalVocabularyResponse' smart constructor.
data UpdateMedicalVocabularyResponse = UpdateMedicalVocabularyResponse'
  { languageCode ::
      Lude.Maybe LanguageCode,
    vocabularyName ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'UpdateMedicalVocabularyResponse' with the minimum fields required to make a request.
--
-- * 'languageCode' - The language code for the language of the text file used to update the custom vocabulary. US English (en-US) is the only language supported in Amazon Transcribe Medical.
-- * 'lastModifiedTime' - The date and time that the vocabulary was updated.
-- * 'responseStatus' - The response status code.
-- * 'vocabularyName' - The name of the updated vocabulary.
-- * 'vocabularyState' - The processing state of the update to the vocabulary. When the @VocabularyState@ field is @READY@ , the vocabulary is ready to be used in a @StartMedicalTranscriptionJob@ request.
mkUpdateMedicalVocabularyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateMedicalVocabularyResponse
mkUpdateMedicalVocabularyResponse pResponseStatus_ =
  UpdateMedicalVocabularyResponse'
    { languageCode = Lude.Nothing,
      vocabularyName = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      vocabularyState = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The language code for the language of the text file used to update the custom vocabulary. US English (en-US) is the only language supported in Amazon Transcribe Medical.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umvrsLanguageCode :: Lens.Lens' UpdateMedicalVocabularyResponse (Lude.Maybe LanguageCode)
umvrsLanguageCode = Lens.lens (languageCode :: UpdateMedicalVocabularyResponse -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: UpdateMedicalVocabularyResponse)
{-# DEPRECATED umvrsLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The name of the updated vocabulary.
--
-- /Note:/ Consider using 'vocabularyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umvrsVocabularyName :: Lens.Lens' UpdateMedicalVocabularyResponse (Lude.Maybe Lude.Text)
umvrsVocabularyName = Lens.lens (vocabularyName :: UpdateMedicalVocabularyResponse -> Lude.Maybe Lude.Text) (\s a -> s {vocabularyName = a} :: UpdateMedicalVocabularyResponse)
{-# DEPRECATED umvrsVocabularyName "Use generic-lens or generic-optics with 'vocabularyName' instead." #-}

-- | The date and time that the vocabulary was updated.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umvrsLastModifiedTime :: Lens.Lens' UpdateMedicalVocabularyResponse (Lude.Maybe Lude.Timestamp)
umvrsLastModifiedTime = Lens.lens (lastModifiedTime :: UpdateMedicalVocabularyResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: UpdateMedicalVocabularyResponse)
{-# DEPRECATED umvrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The processing state of the update to the vocabulary. When the @VocabularyState@ field is @READY@ , the vocabulary is ready to be used in a @StartMedicalTranscriptionJob@ request.
--
-- /Note:/ Consider using 'vocabularyState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umvrsVocabularyState :: Lens.Lens' UpdateMedicalVocabularyResponse (Lude.Maybe VocabularyState)
umvrsVocabularyState = Lens.lens (vocabularyState :: UpdateMedicalVocabularyResponse -> Lude.Maybe VocabularyState) (\s a -> s {vocabularyState = a} :: UpdateMedicalVocabularyResponse)
{-# DEPRECATED umvrsVocabularyState "Use generic-lens or generic-optics with 'vocabularyState' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umvrsResponseStatus :: Lens.Lens' UpdateMedicalVocabularyResponse Lude.Int
umvrsResponseStatus = Lens.lens (responseStatus :: UpdateMedicalVocabularyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateMedicalVocabularyResponse)
{-# DEPRECATED umvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
