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
    uvfVocabularyFilterName,
    uvfVocabularyFilterFileUri,
    uvfWords,

    -- * Destructuring the response
    UpdateVocabularyFilterResponse (..),
    mkUpdateVocabularyFilterResponse,

    -- ** Response lenses
    uvfrrsLanguageCode,
    uvfrrsLastModifiedTime,
    uvfrrsVocabularyFilterName,
    uvfrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Transcribe.Types as Types

-- | /See:/ 'mkUpdateVocabularyFilter' smart constructor.
data UpdateVocabularyFilter = UpdateVocabularyFilter'
  { -- | The name of the vocabulary filter to update. If you try to update a vocabulary filter with the same name as another vocabulary filter, you get a @ConflictException@ error.
    vocabularyFilterName :: Types.VocabularyFilterName,
    -- | The Amazon S3 location of a text file used as input to create the vocabulary filter. Only use characters from the character set defined for custom vocabularies. For a list of character sets, see <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies> .
    --
    -- The specified file must be less than 50 KB of UTF-8 characters.
    -- If you provide the location of a list of words in the @VocabularyFilterFileUri@ parameter, you can't use the @Words@ parameter.
    vocabularyFilterFileUri :: Core.Maybe Types.Uri,
    -- | The words to use in the vocabulary filter. Only use characters from the character set defined for custom vocabularies. For a list of character sets, see <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies> .
    --
    -- If you provide a list of words in the @Words@ parameter, you can't use the @VocabularyFilterFileUri@ parameter.
    words :: Core.Maybe (Core.NonEmpty Types.Word)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateVocabularyFilter' value with any optional fields omitted.
mkUpdateVocabularyFilter ::
  -- | 'vocabularyFilterName'
  Types.VocabularyFilterName ->
  UpdateVocabularyFilter
mkUpdateVocabularyFilter vocabularyFilterName =
  UpdateVocabularyFilter'
    { vocabularyFilterName,
      vocabularyFilterFileUri = Core.Nothing,
      words = Core.Nothing
    }

-- | The name of the vocabulary filter to update. If you try to update a vocabulary filter with the same name as another vocabulary filter, you get a @ConflictException@ error.
--
-- /Note:/ Consider using 'vocabularyFilterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvfVocabularyFilterName :: Lens.Lens' UpdateVocabularyFilter Types.VocabularyFilterName
uvfVocabularyFilterName = Lens.field @"vocabularyFilterName"
{-# DEPRECATED uvfVocabularyFilterName "Use generic-lens or generic-optics with 'vocabularyFilterName' instead." #-}

-- | The Amazon S3 location of a text file used as input to create the vocabulary filter. Only use characters from the character set defined for custom vocabularies. For a list of character sets, see <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies> .
--
-- The specified file must be less than 50 KB of UTF-8 characters.
-- If you provide the location of a list of words in the @VocabularyFilterFileUri@ parameter, you can't use the @Words@ parameter.
--
-- /Note:/ Consider using 'vocabularyFilterFileUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvfVocabularyFilterFileUri :: Lens.Lens' UpdateVocabularyFilter (Core.Maybe Types.Uri)
uvfVocabularyFilterFileUri = Lens.field @"vocabularyFilterFileUri"
{-# DEPRECATED uvfVocabularyFilterFileUri "Use generic-lens or generic-optics with 'vocabularyFilterFileUri' instead." #-}

-- | The words to use in the vocabulary filter. Only use characters from the character set defined for custom vocabularies. For a list of character sets, see <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies> .
--
-- If you provide a list of words in the @Words@ parameter, you can't use the @VocabularyFilterFileUri@ parameter.
--
-- /Note:/ Consider using 'words' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvfWords :: Lens.Lens' UpdateVocabularyFilter (Core.Maybe (Core.NonEmpty Types.Word))
uvfWords = Lens.field @"words"
{-# DEPRECATED uvfWords "Use generic-lens or generic-optics with 'words' instead." #-}

instance Core.FromJSON UpdateVocabularyFilter where
  toJSON UpdateVocabularyFilter {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("VocabularyFilterName" Core..= vocabularyFilterName),
            ("VocabularyFilterFileUri" Core..=)
              Core.<$> vocabularyFilterFileUri,
            ("Words" Core..=) Core.<$> words
          ]
      )

instance Core.AWSRequest UpdateVocabularyFilter where
  type Rs UpdateVocabularyFilter = UpdateVocabularyFilterResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Transcribe.UpdateVocabularyFilter")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateVocabularyFilterResponse'
            Core.<$> (x Core..:? "LanguageCode")
            Core.<*> (x Core..:? "LastModifiedTime")
            Core.<*> (x Core..:? "VocabularyFilterName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateVocabularyFilterResponse' smart constructor.
data UpdateVocabularyFilterResponse = UpdateVocabularyFilterResponse'
  { -- | The language code of the words in the vocabulary filter.
    languageCode :: Core.Maybe Types.LanguageCode,
    -- | The date and time that the vocabulary filter was updated.
    lastModifiedTime :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the updated vocabulary filter.
    vocabularyFilterName :: Core.Maybe Types.VocabularyFilterName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateVocabularyFilterResponse' value with any optional fields omitted.
mkUpdateVocabularyFilterResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateVocabularyFilterResponse
mkUpdateVocabularyFilterResponse responseStatus =
  UpdateVocabularyFilterResponse'
    { languageCode = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      vocabularyFilterName = Core.Nothing,
      responseStatus
    }

-- | The language code of the words in the vocabulary filter.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvfrrsLanguageCode :: Lens.Lens' UpdateVocabularyFilterResponse (Core.Maybe Types.LanguageCode)
uvfrrsLanguageCode = Lens.field @"languageCode"
{-# DEPRECATED uvfrrsLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The date and time that the vocabulary filter was updated.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvfrrsLastModifiedTime :: Lens.Lens' UpdateVocabularyFilterResponse (Core.Maybe Core.NominalDiffTime)
uvfrrsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED uvfrrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The name of the updated vocabulary filter.
--
-- /Note:/ Consider using 'vocabularyFilterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvfrrsVocabularyFilterName :: Lens.Lens' UpdateVocabularyFilterResponse (Core.Maybe Types.VocabularyFilterName)
uvfrrsVocabularyFilterName = Lens.field @"vocabularyFilterName"
{-# DEPRECATED uvfrrsVocabularyFilterName "Use generic-lens or generic-optics with 'vocabularyFilterName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvfrrsResponseStatus :: Lens.Lens' UpdateVocabularyFilterResponse Core.Int
uvfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uvfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
