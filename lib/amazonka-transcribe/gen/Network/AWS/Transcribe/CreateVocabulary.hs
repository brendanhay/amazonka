{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateVocabulary (..)
    , mkCreateVocabulary
    -- ** Request lenses
    , cvVocabularyName
    , cvLanguageCode
    , cvPhrases
    , cvVocabularyFileUri

    -- * Destructuring the response
    , CreateVocabularyResponse (..)
    , mkCreateVocabularyResponse
    -- ** Response lenses
    , cvrrsFailureReason
    , cvrrsLanguageCode
    , cvrrsLastModifiedTime
    , cvrrsVocabularyName
    , cvrrsVocabularyState
    , cvrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Transcribe.Types as Types

-- | /See:/ 'mkCreateVocabulary' smart constructor.
data CreateVocabulary = CreateVocabulary'
  { vocabularyName :: Types.VocabularyName
    -- ^ The name of the vocabulary. The name must be unique within an AWS account. The name is case sensitive. If you try to create a vocabulary with the same name as a previous vocabulary you will receive a @ConflictException@ error.
  , languageCode :: Types.LanguageCode
    -- ^ The language code of the vocabulary entries.
  , phrases :: Core.Maybe [Types.Phrase]
    -- ^ An array of strings that contains the vocabulary entries. 
  , vocabularyFileUri :: Core.Maybe Types.Uri
    -- ^ The S3 location of the text file that contains the definition of the custom vocabulary. The URI must be in the same region as the API endpoint that you are calling. The general form is 
--
-- For more information about S3 object names, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys> in the /Amazon S3 Developer Guide/ .
-- For more information about custom vocabularies, see <http://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary Custom Vocabularies> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateVocabulary' value with any optional fields omitted.
mkCreateVocabulary
    :: Types.VocabularyName -- ^ 'vocabularyName'
    -> Types.LanguageCode -- ^ 'languageCode'
    -> CreateVocabulary
mkCreateVocabulary vocabularyName languageCode
  = CreateVocabulary'{vocabularyName, languageCode,
                      phrases = Core.Nothing, vocabularyFileUri = Core.Nothing}

-- | The name of the vocabulary. The name must be unique within an AWS account. The name is case sensitive. If you try to create a vocabulary with the same name as a previous vocabulary you will receive a @ConflictException@ error.
--
-- /Note:/ Consider using 'vocabularyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvVocabularyName :: Lens.Lens' CreateVocabulary Types.VocabularyName
cvVocabularyName = Lens.field @"vocabularyName"
{-# INLINEABLE cvVocabularyName #-}
{-# DEPRECATED vocabularyName "Use generic-lens or generic-optics with 'vocabularyName' instead"  #-}

-- | The language code of the vocabulary entries.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvLanguageCode :: Lens.Lens' CreateVocabulary Types.LanguageCode
cvLanguageCode = Lens.field @"languageCode"
{-# INLINEABLE cvLanguageCode #-}
{-# DEPRECATED languageCode "Use generic-lens or generic-optics with 'languageCode' instead"  #-}

-- | An array of strings that contains the vocabulary entries. 
--
-- /Note:/ Consider using 'phrases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvPhrases :: Lens.Lens' CreateVocabulary (Core.Maybe [Types.Phrase])
cvPhrases = Lens.field @"phrases"
{-# INLINEABLE cvPhrases #-}
{-# DEPRECATED phrases "Use generic-lens or generic-optics with 'phrases' instead"  #-}

-- | The S3 location of the text file that contains the definition of the custom vocabulary. The URI must be in the same region as the API endpoint that you are calling. The general form is 
--
-- For more information about S3 object names, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys> in the /Amazon S3 Developer Guide/ .
-- For more information about custom vocabularies, see <http://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary Custom Vocabularies> .
--
-- /Note:/ Consider using 'vocabularyFileUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvVocabularyFileUri :: Lens.Lens' CreateVocabulary (Core.Maybe Types.Uri)
cvVocabularyFileUri = Lens.field @"vocabularyFileUri"
{-# INLINEABLE cvVocabularyFileUri #-}
{-# DEPRECATED vocabularyFileUri "Use generic-lens or generic-optics with 'vocabularyFileUri' instead"  #-}

instance Core.ToQuery CreateVocabulary where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateVocabulary where
        toHeaders CreateVocabulary{..}
          = Core.pure ("X-Amz-Target", "Transcribe.CreateVocabulary") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateVocabulary where
        toJSON CreateVocabulary{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("VocabularyName" Core..= vocabularyName),
                  Core.Just ("LanguageCode" Core..= languageCode),
                  ("Phrases" Core..=) Core.<$> phrases,
                  ("VocabularyFileUri" Core..=) Core.<$> vocabularyFileUri])

instance Core.AWSRequest CreateVocabulary where
        type Rs CreateVocabulary = CreateVocabularyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateVocabularyResponse' Core.<$>
                   (x Core..:? "FailureReason") Core.<*> x Core..:? "LanguageCode"
                     Core.<*> x Core..:? "LastModifiedTime"
                     Core.<*> x Core..:? "VocabularyName"
                     Core.<*> x Core..:? "VocabularyState"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateVocabularyResponse' smart constructor.
data CreateVocabularyResponse = CreateVocabularyResponse'
  { failureReason :: Core.Maybe Types.FailureReason
    -- ^ If the @VocabularyState@ field is @FAILED@ , this field contains information about why the job failed.
  , languageCode :: Core.Maybe Types.LanguageCode
    -- ^ The language code of the vocabulary entries.
  , lastModifiedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time that the vocabulary was created.
  , vocabularyName :: Core.Maybe Types.VocabularyName
    -- ^ The name of the vocabulary.
  , vocabularyState :: Core.Maybe Types.VocabularyState
    -- ^ The processing state of the vocabulary. When the @VocabularyState@ field contains @READY@ the vocabulary is ready to be used in a @StartTranscriptionJob@ request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateVocabularyResponse' value with any optional fields omitted.
mkCreateVocabularyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateVocabularyResponse
mkCreateVocabularyResponse responseStatus
  = CreateVocabularyResponse'{failureReason = Core.Nothing,
                              languageCode = Core.Nothing, lastModifiedTime = Core.Nothing,
                              vocabularyName = Core.Nothing, vocabularyState = Core.Nothing,
                              responseStatus}

-- | If the @VocabularyState@ field is @FAILED@ , this field contains information about why the job failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrrsFailureReason :: Lens.Lens' CreateVocabularyResponse (Core.Maybe Types.FailureReason)
cvrrsFailureReason = Lens.field @"failureReason"
{-# INLINEABLE cvrrsFailureReason #-}
{-# DEPRECATED failureReason "Use generic-lens or generic-optics with 'failureReason' instead"  #-}

-- | The language code of the vocabulary entries.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrrsLanguageCode :: Lens.Lens' CreateVocabularyResponse (Core.Maybe Types.LanguageCode)
cvrrsLanguageCode = Lens.field @"languageCode"
{-# INLINEABLE cvrrsLanguageCode #-}
{-# DEPRECATED languageCode "Use generic-lens or generic-optics with 'languageCode' instead"  #-}

-- | The date and time that the vocabulary was created.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrrsLastModifiedTime :: Lens.Lens' CreateVocabularyResponse (Core.Maybe Core.NominalDiffTime)
cvrrsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE cvrrsLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

-- | The name of the vocabulary.
--
-- /Note:/ Consider using 'vocabularyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrrsVocabularyName :: Lens.Lens' CreateVocabularyResponse (Core.Maybe Types.VocabularyName)
cvrrsVocabularyName = Lens.field @"vocabularyName"
{-# INLINEABLE cvrrsVocabularyName #-}
{-# DEPRECATED vocabularyName "Use generic-lens or generic-optics with 'vocabularyName' instead"  #-}

-- | The processing state of the vocabulary. When the @VocabularyState@ field contains @READY@ the vocabulary is ready to be used in a @StartTranscriptionJob@ request.
--
-- /Note:/ Consider using 'vocabularyState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrrsVocabularyState :: Lens.Lens' CreateVocabularyResponse (Core.Maybe Types.VocabularyState)
cvrrsVocabularyState = Lens.field @"vocabularyState"
{-# INLINEABLE cvrrsVocabularyState #-}
{-# DEPRECATED vocabularyState "Use generic-lens or generic-optics with 'vocabularyState' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrrsResponseStatus :: Lens.Lens' CreateVocabularyResponse Core.Int
cvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
