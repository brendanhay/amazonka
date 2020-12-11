{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.GetVocabulary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a vocabulary.
module Network.AWS.Transcribe.GetVocabulary
  ( -- * Creating a request
    GetVocabulary (..),
    mkGetVocabulary,

    -- ** Request lenses
    gvVocabularyName,

    -- * Destructuring the response
    GetVocabularyResponse (..),
    mkGetVocabularyResponse,

    -- ** Response lenses
    gvrsFailureReason,
    gvrsLanguageCode,
    gvrsDownloadURI,
    gvrsVocabularyName,
    gvrsLastModifiedTime,
    gvrsVocabularyState,
    gvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Transcribe.Types

-- | /See:/ 'mkGetVocabulary' smart constructor.
newtype GetVocabulary = GetVocabulary' {vocabularyName :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetVocabulary' with the minimum fields required to make a request.
--
-- * 'vocabularyName' - The name of the vocabulary to return information about. The name is case sensitive.
mkGetVocabulary ::
  -- | 'vocabularyName'
  Lude.Text ->
  GetVocabulary
mkGetVocabulary pVocabularyName_ =
  GetVocabulary' {vocabularyName = pVocabularyName_}

-- | The name of the vocabulary to return information about. The name is case sensitive.
--
-- /Note:/ Consider using 'vocabularyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvVocabularyName :: Lens.Lens' GetVocabulary Lude.Text
gvVocabularyName = Lens.lens (vocabularyName :: GetVocabulary -> Lude.Text) (\s a -> s {vocabularyName = a} :: GetVocabulary)
{-# DEPRECATED gvVocabularyName "Use generic-lens or generic-optics with 'vocabularyName' instead." #-}

instance Lude.AWSRequest GetVocabulary where
  type Rs GetVocabulary = GetVocabularyResponse
  request = Req.postJSON transcribeService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetVocabularyResponse'
            Lude.<$> (x Lude..?> "FailureReason")
            Lude.<*> (x Lude..?> "LanguageCode")
            Lude.<*> (x Lude..?> "DownloadUri")
            Lude.<*> (x Lude..?> "VocabularyName")
            Lude.<*> (x Lude..?> "LastModifiedTime")
            Lude.<*> (x Lude..?> "VocabularyState")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetVocabulary where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Transcribe.GetVocabulary" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetVocabulary where
  toJSON GetVocabulary' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("VocabularyName" Lude..= vocabularyName)]
      )

instance Lude.ToPath GetVocabulary where
  toPath = Lude.const "/"

instance Lude.ToQuery GetVocabulary where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetVocabularyResponse' smart constructor.
data GetVocabularyResponse = GetVocabularyResponse'
  { failureReason ::
      Lude.Maybe Lude.Text,
    languageCode :: Lude.Maybe LanguageCode,
    downloadURI :: Lude.Maybe Lude.Text,
    vocabularyName :: Lude.Maybe Lude.Text,
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    vocabularyState :: Lude.Maybe VocabularyState,
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

-- | Creates a value of 'GetVocabularyResponse' with the minimum fields required to make a request.
--
-- * 'downloadURI' - The S3 location where the vocabulary is stored. Use this URI to get the contents of the vocabulary. The URI is available for a limited time.
-- * 'failureReason' - If the @VocabularyState@ field is @FAILED@ , this field contains information about why the job failed.
-- * 'languageCode' - The language code of the vocabulary entries.
-- * 'lastModifiedTime' - The date and time that the vocabulary was last modified.
-- * 'responseStatus' - The response status code.
-- * 'vocabularyName' - The name of the vocabulary to return.
-- * 'vocabularyState' - The processing state of the vocabulary.
mkGetVocabularyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetVocabularyResponse
mkGetVocabularyResponse pResponseStatus_ =
  GetVocabularyResponse'
    { failureReason = Lude.Nothing,
      languageCode = Lude.Nothing,
      downloadURI = Lude.Nothing,
      vocabularyName = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      vocabularyState = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the @VocabularyState@ field is @FAILED@ , this field contains information about why the job failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvrsFailureReason :: Lens.Lens' GetVocabularyResponse (Lude.Maybe Lude.Text)
gvrsFailureReason = Lens.lens (failureReason :: GetVocabularyResponse -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: GetVocabularyResponse)
{-# DEPRECATED gvrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The language code of the vocabulary entries.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvrsLanguageCode :: Lens.Lens' GetVocabularyResponse (Lude.Maybe LanguageCode)
gvrsLanguageCode = Lens.lens (languageCode :: GetVocabularyResponse -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: GetVocabularyResponse)
{-# DEPRECATED gvrsLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The S3 location where the vocabulary is stored. Use this URI to get the contents of the vocabulary. The URI is available for a limited time.
--
-- /Note:/ Consider using 'downloadURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvrsDownloadURI :: Lens.Lens' GetVocabularyResponse (Lude.Maybe Lude.Text)
gvrsDownloadURI = Lens.lens (downloadURI :: GetVocabularyResponse -> Lude.Maybe Lude.Text) (\s a -> s {downloadURI = a} :: GetVocabularyResponse)
{-# DEPRECATED gvrsDownloadURI "Use generic-lens or generic-optics with 'downloadURI' instead." #-}

-- | The name of the vocabulary to return.
--
-- /Note:/ Consider using 'vocabularyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvrsVocabularyName :: Lens.Lens' GetVocabularyResponse (Lude.Maybe Lude.Text)
gvrsVocabularyName = Lens.lens (vocabularyName :: GetVocabularyResponse -> Lude.Maybe Lude.Text) (\s a -> s {vocabularyName = a} :: GetVocabularyResponse)
{-# DEPRECATED gvrsVocabularyName "Use generic-lens or generic-optics with 'vocabularyName' instead." #-}

-- | The date and time that the vocabulary was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvrsLastModifiedTime :: Lens.Lens' GetVocabularyResponse (Lude.Maybe Lude.Timestamp)
gvrsLastModifiedTime = Lens.lens (lastModifiedTime :: GetVocabularyResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: GetVocabularyResponse)
{-# DEPRECATED gvrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The processing state of the vocabulary.
--
-- /Note:/ Consider using 'vocabularyState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvrsVocabularyState :: Lens.Lens' GetVocabularyResponse (Lude.Maybe VocabularyState)
gvrsVocabularyState = Lens.lens (vocabularyState :: GetVocabularyResponse -> Lude.Maybe VocabularyState) (\s a -> s {vocabularyState = a} :: GetVocabularyResponse)
{-# DEPRECATED gvrsVocabularyState "Use generic-lens or generic-optics with 'vocabularyState' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvrsResponseStatus :: Lens.Lens' GetVocabularyResponse Lude.Int
gvrsResponseStatus = Lens.lens (responseStatus :: GetVocabularyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetVocabularyResponse)
{-# DEPRECATED gvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
