{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.GetVocabularyFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a vocabulary filter.
module Network.AWS.Transcribe.GetVocabularyFilter
  ( -- * Creating a request
    GetVocabularyFilter (..),
    mkGetVocabularyFilter,

    -- ** Request lenses
    gvfVocabularyFilterName,

    -- * Destructuring the response
    GetVocabularyFilterResponse (..),
    mkGetVocabularyFilterResponse,

    -- ** Response lenses
    gvfrsLanguageCode,
    gvfrsDownloadURI,
    gvfrsLastModifiedTime,
    gvfrsVocabularyFilterName,
    gvfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Transcribe.Types

-- | /See:/ 'mkGetVocabularyFilter' smart constructor.
newtype GetVocabularyFilter = GetVocabularyFilter'
  { -- | The name of the vocabulary filter for which to return information.
    vocabularyFilterName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetVocabularyFilter' with the minimum fields required to make a request.
--
-- * 'vocabularyFilterName' - The name of the vocabulary filter for which to return information.
mkGetVocabularyFilter ::
  -- | 'vocabularyFilterName'
  Lude.Text ->
  GetVocabularyFilter
mkGetVocabularyFilter pVocabularyFilterName_ =
  GetVocabularyFilter'
    { vocabularyFilterName =
        pVocabularyFilterName_
    }

-- | The name of the vocabulary filter for which to return information.
--
-- /Note:/ Consider using 'vocabularyFilterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvfVocabularyFilterName :: Lens.Lens' GetVocabularyFilter Lude.Text
gvfVocabularyFilterName = Lens.lens (vocabularyFilterName :: GetVocabularyFilter -> Lude.Text) (\s a -> s {vocabularyFilterName = a} :: GetVocabularyFilter)
{-# DEPRECATED gvfVocabularyFilterName "Use generic-lens or generic-optics with 'vocabularyFilterName' instead." #-}

instance Lude.AWSRequest GetVocabularyFilter where
  type Rs GetVocabularyFilter = GetVocabularyFilterResponse
  request = Req.postJSON transcribeService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetVocabularyFilterResponse'
            Lude.<$> (x Lude..?> "LanguageCode")
            Lude.<*> (x Lude..?> "DownloadUri")
            Lude.<*> (x Lude..?> "LastModifiedTime")
            Lude.<*> (x Lude..?> "VocabularyFilterName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetVocabularyFilter where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Transcribe.GetVocabularyFilter" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetVocabularyFilter where
  toJSON GetVocabularyFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("VocabularyFilterName" Lude..= vocabularyFilterName)]
      )

instance Lude.ToPath GetVocabularyFilter where
  toPath = Lude.const "/"

instance Lude.ToQuery GetVocabularyFilter where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetVocabularyFilterResponse' smart constructor.
data GetVocabularyFilterResponse = GetVocabularyFilterResponse'
  { -- | The language code of the words in the vocabulary filter.
    languageCode :: Lude.Maybe LanguageCode,
    -- | The URI of the list of words in the vocabulary filter. You can use this URI to get the list of words.
    downloadURI :: Lude.Maybe Lude.Text,
    -- | The date and time that the contents of the vocabulary filter were updated.
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    -- | The name of the vocabulary filter.
    vocabularyFilterName :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetVocabularyFilterResponse' with the minimum fields required to make a request.
--
-- * 'languageCode' - The language code of the words in the vocabulary filter.
-- * 'downloadURI' - The URI of the list of words in the vocabulary filter. You can use this URI to get the list of words.
-- * 'lastModifiedTime' - The date and time that the contents of the vocabulary filter were updated.
-- * 'vocabularyFilterName' - The name of the vocabulary filter.
-- * 'responseStatus' - The response status code.
mkGetVocabularyFilterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetVocabularyFilterResponse
mkGetVocabularyFilterResponse pResponseStatus_ =
  GetVocabularyFilterResponse'
    { languageCode = Lude.Nothing,
      downloadURI = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      vocabularyFilterName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The language code of the words in the vocabulary filter.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvfrsLanguageCode :: Lens.Lens' GetVocabularyFilterResponse (Lude.Maybe LanguageCode)
gvfrsLanguageCode = Lens.lens (languageCode :: GetVocabularyFilterResponse -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: GetVocabularyFilterResponse)
{-# DEPRECATED gvfrsLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The URI of the list of words in the vocabulary filter. You can use this URI to get the list of words.
--
-- /Note:/ Consider using 'downloadURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvfrsDownloadURI :: Lens.Lens' GetVocabularyFilterResponse (Lude.Maybe Lude.Text)
gvfrsDownloadURI = Lens.lens (downloadURI :: GetVocabularyFilterResponse -> Lude.Maybe Lude.Text) (\s a -> s {downloadURI = a} :: GetVocabularyFilterResponse)
{-# DEPRECATED gvfrsDownloadURI "Use generic-lens or generic-optics with 'downloadURI' instead." #-}

-- | The date and time that the contents of the vocabulary filter were updated.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvfrsLastModifiedTime :: Lens.Lens' GetVocabularyFilterResponse (Lude.Maybe Lude.Timestamp)
gvfrsLastModifiedTime = Lens.lens (lastModifiedTime :: GetVocabularyFilterResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: GetVocabularyFilterResponse)
{-# DEPRECATED gvfrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The name of the vocabulary filter.
--
-- /Note:/ Consider using 'vocabularyFilterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvfrsVocabularyFilterName :: Lens.Lens' GetVocabularyFilterResponse (Lude.Maybe Lude.Text)
gvfrsVocabularyFilterName = Lens.lens (vocabularyFilterName :: GetVocabularyFilterResponse -> Lude.Maybe Lude.Text) (\s a -> s {vocabularyFilterName = a} :: GetVocabularyFilterResponse)
{-# DEPRECATED gvfrsVocabularyFilterName "Use generic-lens or generic-optics with 'vocabularyFilterName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvfrsResponseStatus :: Lens.Lens' GetVocabularyFilterResponse Lude.Int
gvfrsResponseStatus = Lens.lens (responseStatus :: GetVocabularyFilterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetVocabularyFilterResponse)
{-# DEPRECATED gvfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
