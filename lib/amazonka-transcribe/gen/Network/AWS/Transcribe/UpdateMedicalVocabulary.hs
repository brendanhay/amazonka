{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    umvVocabularyName,
    umvLanguageCode,
    umvVocabularyFileUri,

    -- * Destructuring the response
    UpdateMedicalVocabularyResponse (..),
    mkUpdateMedicalVocabularyResponse,

    -- ** Response lenses
    umvrrsLanguageCode,
    umvrrsLastModifiedTime,
    umvrrsVocabularyName,
    umvrrsVocabularyState,
    umvrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Transcribe.Types as Types

-- | /See:/ 'mkUpdateMedicalVocabulary' smart constructor.
data UpdateMedicalVocabulary = UpdateMedicalVocabulary'
  { -- | The name of the vocabulary to update. The name is case sensitive. If you try to update a vocabulary with the same name as a vocabulary you've already made, you get a @ConflictException@ error.
    vocabularyName :: Types.VocabularyName,
    -- | The language code of the language used for the entries in the updated vocabulary. US English (en-US) is the only valid language code in Amazon Transcribe Medical.
    languageCode :: Types.LanguageCode,
    -- | The location in Amazon S3 of the text file that contains the you use for your custom vocabulary. The URI must be in the same AWS Region as the resource that you are calling. The following is the format for a URI:
    --
    -- @https://s3.<aws-region>.amazonaws.com/<bucket-name>/<keyprefix>/<objectkey> @
    -- For example:
    -- @https://s3.us-east-1.amazonaws.com/AWSDOC-EXAMPLE-BUCKET/vocab.txt@
    -- For more information about Amazon S3 object names, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys> in the /Amazon S3 Developer Guide/ .
    -- For more information about custom vocabularies in Amazon Transcribe Medical, see <http://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary Medical Custom Vocabularies> .
    vocabularyFileUri :: Core.Maybe Types.Uri
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMedicalVocabulary' value with any optional fields omitted.
mkUpdateMedicalVocabulary ::
  -- | 'vocabularyName'
  Types.VocabularyName ->
  -- | 'languageCode'
  Types.LanguageCode ->
  UpdateMedicalVocabulary
mkUpdateMedicalVocabulary vocabularyName languageCode =
  UpdateMedicalVocabulary'
    { vocabularyName,
      languageCode,
      vocabularyFileUri = Core.Nothing
    }

-- | The name of the vocabulary to update. The name is case sensitive. If you try to update a vocabulary with the same name as a vocabulary you've already made, you get a @ConflictException@ error.
--
-- /Note:/ Consider using 'vocabularyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umvVocabularyName :: Lens.Lens' UpdateMedicalVocabulary Types.VocabularyName
umvVocabularyName = Lens.field @"vocabularyName"
{-# DEPRECATED umvVocabularyName "Use generic-lens or generic-optics with 'vocabularyName' instead." #-}

-- | The language code of the language used for the entries in the updated vocabulary. US English (en-US) is the only valid language code in Amazon Transcribe Medical.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umvLanguageCode :: Lens.Lens' UpdateMedicalVocabulary Types.LanguageCode
umvLanguageCode = Lens.field @"languageCode"
{-# DEPRECATED umvLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The location in Amazon S3 of the text file that contains the you use for your custom vocabulary. The URI must be in the same AWS Region as the resource that you are calling. The following is the format for a URI:
--
-- @https://s3.<aws-region>.amazonaws.com/<bucket-name>/<keyprefix>/<objectkey> @
-- For example:
-- @https://s3.us-east-1.amazonaws.com/AWSDOC-EXAMPLE-BUCKET/vocab.txt@
-- For more information about Amazon S3 object names, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys> in the /Amazon S3 Developer Guide/ .
-- For more information about custom vocabularies in Amazon Transcribe Medical, see <http://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary Medical Custom Vocabularies> .
--
-- /Note:/ Consider using 'vocabularyFileUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umvVocabularyFileUri :: Lens.Lens' UpdateMedicalVocabulary (Core.Maybe Types.Uri)
umvVocabularyFileUri = Lens.field @"vocabularyFileUri"
{-# DEPRECATED umvVocabularyFileUri "Use generic-lens or generic-optics with 'vocabularyFileUri' instead." #-}

instance Core.FromJSON UpdateMedicalVocabulary where
  toJSON UpdateMedicalVocabulary {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("VocabularyName" Core..= vocabularyName),
            Core.Just ("LanguageCode" Core..= languageCode),
            ("VocabularyFileUri" Core..=) Core.<$> vocabularyFileUri
          ]
      )

instance Core.AWSRequest UpdateMedicalVocabulary where
  type Rs UpdateMedicalVocabulary = UpdateMedicalVocabularyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Transcribe.UpdateMedicalVocabulary")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMedicalVocabularyResponse'
            Core.<$> (x Core..:? "LanguageCode")
            Core.<*> (x Core..:? "LastModifiedTime")
            Core.<*> (x Core..:? "VocabularyName")
            Core.<*> (x Core..:? "VocabularyState")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateMedicalVocabularyResponse' smart constructor.
data UpdateMedicalVocabularyResponse = UpdateMedicalVocabularyResponse'
  { -- | The language code for the language of the text file used to update the custom vocabulary. US English (en-US) is the only language supported in Amazon Transcribe Medical.
    languageCode :: Core.Maybe Types.LanguageCode,
    -- | The date and time that the vocabulary was updated.
    lastModifiedTime :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the updated vocabulary.
    vocabularyName :: Core.Maybe Types.VocabularyName,
    -- | The processing state of the update to the vocabulary. When the @VocabularyState@ field is @READY@ , the vocabulary is ready to be used in a @StartMedicalTranscriptionJob@ request.
    vocabularyState :: Core.Maybe Types.VocabularyState,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateMedicalVocabularyResponse' value with any optional fields omitted.
mkUpdateMedicalVocabularyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateMedicalVocabularyResponse
mkUpdateMedicalVocabularyResponse responseStatus =
  UpdateMedicalVocabularyResponse'
    { languageCode = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      vocabularyName = Core.Nothing,
      vocabularyState = Core.Nothing,
      responseStatus
    }

-- | The language code for the language of the text file used to update the custom vocabulary. US English (en-US) is the only language supported in Amazon Transcribe Medical.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umvrrsLanguageCode :: Lens.Lens' UpdateMedicalVocabularyResponse (Core.Maybe Types.LanguageCode)
umvrrsLanguageCode = Lens.field @"languageCode"
{-# DEPRECATED umvrrsLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The date and time that the vocabulary was updated.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umvrrsLastModifiedTime :: Lens.Lens' UpdateMedicalVocabularyResponse (Core.Maybe Core.NominalDiffTime)
umvrrsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED umvrrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The name of the updated vocabulary.
--
-- /Note:/ Consider using 'vocabularyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umvrrsVocabularyName :: Lens.Lens' UpdateMedicalVocabularyResponse (Core.Maybe Types.VocabularyName)
umvrrsVocabularyName = Lens.field @"vocabularyName"
{-# DEPRECATED umvrrsVocabularyName "Use generic-lens or generic-optics with 'vocabularyName' instead." #-}

-- | The processing state of the update to the vocabulary. When the @VocabularyState@ field is @READY@ , the vocabulary is ready to be used in a @StartMedicalTranscriptionJob@ request.
--
-- /Note:/ Consider using 'vocabularyState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umvrrsVocabularyState :: Lens.Lens' UpdateMedicalVocabularyResponse (Core.Maybe Types.VocabularyState)
umvrrsVocabularyState = Lens.field @"vocabularyState"
{-# DEPRECATED umvrrsVocabularyState "Use generic-lens or generic-optics with 'vocabularyState' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umvrrsResponseStatus :: Lens.Lens' UpdateMedicalVocabularyResponse Core.Int
umvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED umvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
