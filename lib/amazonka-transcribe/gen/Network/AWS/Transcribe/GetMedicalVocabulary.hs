{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.GetMedicalVocabulary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a medical vocabulary.
module Network.AWS.Transcribe.GetMedicalVocabulary
  ( -- * Creating a request
    GetMedicalVocabulary (..),
    mkGetMedicalVocabulary,

    -- ** Request lenses
    gmvVocabularyName,

    -- * Destructuring the response
    GetMedicalVocabularyResponse (..),
    mkGetMedicalVocabularyResponse,

    -- ** Response lenses
    gmvrsFailureReason,
    gmvrsLanguageCode,
    gmvrsDownloadURI,
    gmvrsVocabularyName,
    gmvrsLastModifiedTime,
    gmvrsVocabularyState,
    gmvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Transcribe.Types

-- | /See:/ 'mkGetMedicalVocabulary' smart constructor.
newtype GetMedicalVocabulary = GetMedicalVocabulary'
  { vocabularyName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMedicalVocabulary' with the minimum fields required to make a request.
--
-- * 'vocabularyName' - The name of the vocabulary that you want information about. The value is case sensitive.
mkGetMedicalVocabulary ::
  -- | 'vocabularyName'
  Lude.Text ->
  GetMedicalVocabulary
mkGetMedicalVocabulary pVocabularyName_ =
  GetMedicalVocabulary' {vocabularyName = pVocabularyName_}

-- | The name of the vocabulary that you want information about. The value is case sensitive.
--
-- /Note:/ Consider using 'vocabularyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmvVocabularyName :: Lens.Lens' GetMedicalVocabulary Lude.Text
gmvVocabularyName = Lens.lens (vocabularyName :: GetMedicalVocabulary -> Lude.Text) (\s a -> s {vocabularyName = a} :: GetMedicalVocabulary)
{-# DEPRECATED gmvVocabularyName "Use generic-lens or generic-optics with 'vocabularyName' instead." #-}

instance Lude.AWSRequest GetMedicalVocabulary where
  type Rs GetMedicalVocabulary = GetMedicalVocabularyResponse
  request = Req.postJSON transcribeService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetMedicalVocabularyResponse'
            Lude.<$> (x Lude..?> "FailureReason")
            Lude.<*> (x Lude..?> "LanguageCode")
            Lude.<*> (x Lude..?> "DownloadUri")
            Lude.<*> (x Lude..?> "VocabularyName")
            Lude.<*> (x Lude..?> "LastModifiedTime")
            Lude.<*> (x Lude..?> "VocabularyState")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetMedicalVocabulary where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Transcribe.GetMedicalVocabulary" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetMedicalVocabulary where
  toJSON GetMedicalVocabulary' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("VocabularyName" Lude..= vocabularyName)]
      )

instance Lude.ToPath GetMedicalVocabulary where
  toPath = Lude.const "/"

instance Lude.ToQuery GetMedicalVocabulary where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetMedicalVocabularyResponse' smart constructor.
data GetMedicalVocabularyResponse = GetMedicalVocabularyResponse'
  { failureReason ::
      Lude.Maybe Lude.Text,
    languageCode ::
      Lude.Maybe LanguageCode,
    downloadURI ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetMedicalVocabularyResponse' with the minimum fields required to make a request.
--
-- * 'downloadURI' - The location in Amazon S3 where the vocabulary is stored. Use this URI to get the contents of the vocabulary. You can download your vocabulary from the URI for a limited time.
-- * 'failureReason' - If the @VocabularyState@ is @FAILED@ , this field contains information about why the job failed.
-- * 'languageCode' - The valid language code for your vocabulary entries.
-- * 'lastModifiedTime' - The date and time that the vocabulary was last modified with a text file different from the one that was previously used.
-- * 'responseStatus' - The response status code.
-- * 'vocabularyName' - The name of the vocabulary returned by Amazon Transcribe Medical.
-- * 'vocabularyState' - The processing state of the vocabulary. If the @VocabularyState@ is @READY@ then you can use it in the @StartMedicalTranscriptionJob@ operation.
mkGetMedicalVocabularyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetMedicalVocabularyResponse
mkGetMedicalVocabularyResponse pResponseStatus_ =
  GetMedicalVocabularyResponse'
    { failureReason = Lude.Nothing,
      languageCode = Lude.Nothing,
      downloadURI = Lude.Nothing,
      vocabularyName = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      vocabularyState = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the @VocabularyState@ is @FAILED@ , this field contains information about why the job failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmvrsFailureReason :: Lens.Lens' GetMedicalVocabularyResponse (Lude.Maybe Lude.Text)
gmvrsFailureReason = Lens.lens (failureReason :: GetMedicalVocabularyResponse -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: GetMedicalVocabularyResponse)
{-# DEPRECATED gmvrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The valid language code for your vocabulary entries.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmvrsLanguageCode :: Lens.Lens' GetMedicalVocabularyResponse (Lude.Maybe LanguageCode)
gmvrsLanguageCode = Lens.lens (languageCode :: GetMedicalVocabularyResponse -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: GetMedicalVocabularyResponse)
{-# DEPRECATED gmvrsLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The location in Amazon S3 where the vocabulary is stored. Use this URI to get the contents of the vocabulary. You can download your vocabulary from the URI for a limited time.
--
-- /Note:/ Consider using 'downloadURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmvrsDownloadURI :: Lens.Lens' GetMedicalVocabularyResponse (Lude.Maybe Lude.Text)
gmvrsDownloadURI = Lens.lens (downloadURI :: GetMedicalVocabularyResponse -> Lude.Maybe Lude.Text) (\s a -> s {downloadURI = a} :: GetMedicalVocabularyResponse)
{-# DEPRECATED gmvrsDownloadURI "Use generic-lens or generic-optics with 'downloadURI' instead." #-}

-- | The name of the vocabulary returned by Amazon Transcribe Medical.
--
-- /Note:/ Consider using 'vocabularyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmvrsVocabularyName :: Lens.Lens' GetMedicalVocabularyResponse (Lude.Maybe Lude.Text)
gmvrsVocabularyName = Lens.lens (vocabularyName :: GetMedicalVocabularyResponse -> Lude.Maybe Lude.Text) (\s a -> s {vocabularyName = a} :: GetMedicalVocabularyResponse)
{-# DEPRECATED gmvrsVocabularyName "Use generic-lens or generic-optics with 'vocabularyName' instead." #-}

-- | The date and time that the vocabulary was last modified with a text file different from the one that was previously used.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmvrsLastModifiedTime :: Lens.Lens' GetMedicalVocabularyResponse (Lude.Maybe Lude.Timestamp)
gmvrsLastModifiedTime = Lens.lens (lastModifiedTime :: GetMedicalVocabularyResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: GetMedicalVocabularyResponse)
{-# DEPRECATED gmvrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The processing state of the vocabulary. If the @VocabularyState@ is @READY@ then you can use it in the @StartMedicalTranscriptionJob@ operation.
--
-- /Note:/ Consider using 'vocabularyState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmvrsVocabularyState :: Lens.Lens' GetMedicalVocabularyResponse (Lude.Maybe VocabularyState)
gmvrsVocabularyState = Lens.lens (vocabularyState :: GetMedicalVocabularyResponse -> Lude.Maybe VocabularyState) (\s a -> s {vocabularyState = a} :: GetMedicalVocabularyResponse)
{-# DEPRECATED gmvrsVocabularyState "Use generic-lens or generic-optics with 'vocabularyState' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmvrsResponseStatus :: Lens.Lens' GetMedicalVocabularyResponse Lude.Int
gmvrsResponseStatus = Lens.lens (responseStatus :: GetMedicalVocabularyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetMedicalVocabularyResponse)
{-# DEPRECATED gmvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
