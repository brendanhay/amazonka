{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    gvrrsDownloadUri,
    gvrrsFailureReason,
    gvrrsLanguageCode,
    gvrrsLastModifiedTime,
    gvrrsVocabularyName,
    gvrrsVocabularyState,
    gvrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Transcribe.Types as Types

-- | /See:/ 'mkGetVocabulary' smart constructor.
newtype GetVocabulary = GetVocabulary'
  { -- | The name of the vocabulary to return information about. The name is case sensitive.
    vocabularyName :: Types.VocabularyName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetVocabulary' value with any optional fields omitted.
mkGetVocabulary ::
  -- | 'vocabularyName'
  Types.VocabularyName ->
  GetVocabulary
mkGetVocabulary vocabularyName = GetVocabulary' {vocabularyName}

-- | The name of the vocabulary to return information about. The name is case sensitive.
--
-- /Note:/ Consider using 'vocabularyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvVocabularyName :: Lens.Lens' GetVocabulary Types.VocabularyName
gvVocabularyName = Lens.field @"vocabularyName"
{-# DEPRECATED gvVocabularyName "Use generic-lens or generic-optics with 'vocabularyName' instead." #-}

instance Core.FromJSON GetVocabulary where
  toJSON GetVocabulary {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("VocabularyName" Core..= vocabularyName)]
      )

instance Core.AWSRequest GetVocabulary where
  type Rs GetVocabulary = GetVocabularyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Transcribe.GetVocabulary")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetVocabularyResponse'
            Core.<$> (x Core..:? "DownloadUri")
            Core.<*> (x Core..:? "FailureReason")
            Core.<*> (x Core..:? "LanguageCode")
            Core.<*> (x Core..:? "LastModifiedTime")
            Core.<*> (x Core..:? "VocabularyName")
            Core.<*> (x Core..:? "VocabularyState")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetVocabularyResponse' smart constructor.
data GetVocabularyResponse = GetVocabularyResponse'
  { -- | The S3 location where the vocabulary is stored. Use this URI to get the contents of the vocabulary. The URI is available for a limited time.
    downloadUri :: Core.Maybe Types.DownloadUri,
    -- | If the @VocabularyState@ field is @FAILED@ , this field contains information about why the job failed.
    failureReason :: Core.Maybe Types.FailureReason,
    -- | The language code of the vocabulary entries.
    languageCode :: Core.Maybe Types.LanguageCode,
    -- | The date and time that the vocabulary was last modified.
    lastModifiedTime :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the vocabulary to return.
    vocabularyName :: Core.Maybe Types.VocabularyName,
    -- | The processing state of the vocabulary.
    vocabularyState :: Core.Maybe Types.VocabularyState,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetVocabularyResponse' value with any optional fields omitted.
mkGetVocabularyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetVocabularyResponse
mkGetVocabularyResponse responseStatus =
  GetVocabularyResponse'
    { downloadUri = Core.Nothing,
      failureReason = Core.Nothing,
      languageCode = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      vocabularyName = Core.Nothing,
      vocabularyState = Core.Nothing,
      responseStatus
    }

-- | The S3 location where the vocabulary is stored. Use this URI to get the contents of the vocabulary. The URI is available for a limited time.
--
-- /Note:/ Consider using 'downloadUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvrrsDownloadUri :: Lens.Lens' GetVocabularyResponse (Core.Maybe Types.DownloadUri)
gvrrsDownloadUri = Lens.field @"downloadUri"
{-# DEPRECATED gvrrsDownloadUri "Use generic-lens or generic-optics with 'downloadUri' instead." #-}

-- | If the @VocabularyState@ field is @FAILED@ , this field contains information about why the job failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvrrsFailureReason :: Lens.Lens' GetVocabularyResponse (Core.Maybe Types.FailureReason)
gvrrsFailureReason = Lens.field @"failureReason"
{-# DEPRECATED gvrrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The language code of the vocabulary entries.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvrrsLanguageCode :: Lens.Lens' GetVocabularyResponse (Core.Maybe Types.LanguageCode)
gvrrsLanguageCode = Lens.field @"languageCode"
{-# DEPRECATED gvrrsLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The date and time that the vocabulary was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvrrsLastModifiedTime :: Lens.Lens' GetVocabularyResponse (Core.Maybe Core.NominalDiffTime)
gvrrsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED gvrrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The name of the vocabulary to return.
--
-- /Note:/ Consider using 'vocabularyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvrrsVocabularyName :: Lens.Lens' GetVocabularyResponse (Core.Maybe Types.VocabularyName)
gvrrsVocabularyName = Lens.field @"vocabularyName"
{-# DEPRECATED gvrrsVocabularyName "Use generic-lens or generic-optics with 'vocabularyName' instead." #-}

-- | The processing state of the vocabulary.
--
-- /Note:/ Consider using 'vocabularyState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvrrsVocabularyState :: Lens.Lens' GetVocabularyResponse (Core.Maybe Types.VocabularyState)
gvrrsVocabularyState = Lens.field @"vocabularyState"
{-# DEPRECATED gvrrsVocabularyState "Use generic-lens or generic-optics with 'vocabularyState' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvrrsResponseStatus :: Lens.Lens' GetVocabularyResponse Core.Int
gvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
