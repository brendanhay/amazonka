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
    cvfVocabularyFilterName,
    cvfLanguageCode,
    cvfVocabularyFilterFileUri,
    cvfWords,

    -- * Destructuring the response
    CreateVocabularyFilterResponse (..),
    mkCreateVocabularyFilterResponse,

    -- ** Response lenses
    cvfrrsLanguageCode,
    cvfrrsLastModifiedTime,
    cvfrrsVocabularyFilterName,
    cvfrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Transcribe.Types as Types

-- | /See:/ 'mkCreateVocabularyFilter' smart constructor.
data CreateVocabularyFilter = CreateVocabularyFilter'
  { -- | The vocabulary filter name. The name must be unique within the account that contains it. If you try to create a vocabulary filter with the same name as another vocabulary filter, you get a @ConflictException@ error.
    vocabularyFilterName :: Types.VocabularyFilterName,
    -- | The language code of the words in the vocabulary filter. All words in the filter must be in the same language. The vocabulary filter can only be used with transcription jobs in the specified language.
    languageCode :: Types.LanguageCode,
    -- | The Amazon S3 location of a text file used as input to create the vocabulary filter. Only use characters from the character set defined for custom vocabularies. For a list of character sets, see <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies> .
    --
    -- The specified file must be less than 50 KB of UTF-8 characters.
    -- If you provide the location of a list of words in the @VocabularyFilterFileUri@ parameter, you can't use the @Words@ parameter.
    vocabularyFilterFileUri :: Core.Maybe Types.VocabularyFilterFileUri,
    -- | The words to use in the vocabulary filter. Only use characters from the character set defined for custom vocabularies. For a list of character sets, see <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies> .
    --
    -- If you provide a list of words in the @Words@ parameter, you can't use the @VocabularyFilterFileUri@ parameter.
    words :: Core.Maybe (Core.NonEmpty Types.Word)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateVocabularyFilter' value with any optional fields omitted.
mkCreateVocabularyFilter ::
  -- | 'vocabularyFilterName'
  Types.VocabularyFilterName ->
  -- | 'languageCode'
  Types.LanguageCode ->
  CreateVocabularyFilter
mkCreateVocabularyFilter vocabularyFilterName languageCode =
  CreateVocabularyFilter'
    { vocabularyFilterName,
      languageCode,
      vocabularyFilterFileUri = Core.Nothing,
      words = Core.Nothing
    }

-- | The vocabulary filter name. The name must be unique within the account that contains it. If you try to create a vocabulary filter with the same name as another vocabulary filter, you get a @ConflictException@ error.
--
-- /Note:/ Consider using 'vocabularyFilterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvfVocabularyFilterName :: Lens.Lens' CreateVocabularyFilter Types.VocabularyFilterName
cvfVocabularyFilterName = Lens.field @"vocabularyFilterName"
{-# DEPRECATED cvfVocabularyFilterName "Use generic-lens or generic-optics with 'vocabularyFilterName' instead." #-}

-- | The language code of the words in the vocabulary filter. All words in the filter must be in the same language. The vocabulary filter can only be used with transcription jobs in the specified language.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvfLanguageCode :: Lens.Lens' CreateVocabularyFilter Types.LanguageCode
cvfLanguageCode = Lens.field @"languageCode"
{-# DEPRECATED cvfLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The Amazon S3 location of a text file used as input to create the vocabulary filter. Only use characters from the character set defined for custom vocabularies. For a list of character sets, see <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies> .
--
-- The specified file must be less than 50 KB of UTF-8 characters.
-- If you provide the location of a list of words in the @VocabularyFilterFileUri@ parameter, you can't use the @Words@ parameter.
--
-- /Note:/ Consider using 'vocabularyFilterFileUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvfVocabularyFilterFileUri :: Lens.Lens' CreateVocabularyFilter (Core.Maybe Types.VocabularyFilterFileUri)
cvfVocabularyFilterFileUri = Lens.field @"vocabularyFilterFileUri"
{-# DEPRECATED cvfVocabularyFilterFileUri "Use generic-lens or generic-optics with 'vocabularyFilterFileUri' instead." #-}

-- | The words to use in the vocabulary filter. Only use characters from the character set defined for custom vocabularies. For a list of character sets, see <https://docs.aws.amazon.com/transcribe/latest/dg/how-vocabulary.html#charsets Character Sets for Custom Vocabularies> .
--
-- If you provide a list of words in the @Words@ parameter, you can't use the @VocabularyFilterFileUri@ parameter.
--
-- /Note:/ Consider using 'words' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvfWords :: Lens.Lens' CreateVocabularyFilter (Core.Maybe (Core.NonEmpty Types.Word))
cvfWords = Lens.field @"words"
{-# DEPRECATED cvfWords "Use generic-lens or generic-optics with 'words' instead." #-}

instance Core.FromJSON CreateVocabularyFilter where
  toJSON CreateVocabularyFilter {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("VocabularyFilterName" Core..= vocabularyFilterName),
            Core.Just ("LanguageCode" Core..= languageCode),
            ("VocabularyFilterFileUri" Core..=)
              Core.<$> vocabularyFilterFileUri,
            ("Words" Core..=) Core.<$> words
          ]
      )

instance Core.AWSRequest CreateVocabularyFilter where
  type Rs CreateVocabularyFilter = CreateVocabularyFilterResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Transcribe.CreateVocabularyFilter")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateVocabularyFilterResponse'
            Core.<$> (x Core..:? "LanguageCode")
            Core.<*> (x Core..:? "LastModifiedTime")
            Core.<*> (x Core..:? "VocabularyFilterName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateVocabularyFilterResponse' smart constructor.
data CreateVocabularyFilterResponse = CreateVocabularyFilterResponse'
  { -- | The language code of the words in the collection.
    languageCode :: Core.Maybe Types.LanguageCode,
    -- | The date and time that the vocabulary filter was modified.
    lastModifiedTime :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the vocabulary filter.
    vocabularyFilterName :: Core.Maybe Types.VocabularyFilterName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateVocabularyFilterResponse' value with any optional fields omitted.
mkCreateVocabularyFilterResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateVocabularyFilterResponse
mkCreateVocabularyFilterResponse responseStatus =
  CreateVocabularyFilterResponse'
    { languageCode = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      vocabularyFilterName = Core.Nothing,
      responseStatus
    }

-- | The language code of the words in the collection.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvfrrsLanguageCode :: Lens.Lens' CreateVocabularyFilterResponse (Core.Maybe Types.LanguageCode)
cvfrrsLanguageCode = Lens.field @"languageCode"
{-# DEPRECATED cvfrrsLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The date and time that the vocabulary filter was modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvfrrsLastModifiedTime :: Lens.Lens' CreateVocabularyFilterResponse (Core.Maybe Core.NominalDiffTime)
cvfrrsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED cvfrrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The name of the vocabulary filter.
--
-- /Note:/ Consider using 'vocabularyFilterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvfrrsVocabularyFilterName :: Lens.Lens' CreateVocabularyFilterResponse (Core.Maybe Types.VocabularyFilterName)
cvfrrsVocabularyFilterName = Lens.field @"vocabularyFilterName"
{-# DEPRECATED cvfrrsVocabularyFilterName "Use generic-lens or generic-optics with 'vocabularyFilterName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvfrrsResponseStatus :: Lens.Lens' CreateVocabularyFilterResponse Core.Int
cvfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cvfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
