{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.StartSpeechSynthesisTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the creation of an asynchronous synthesis task, by starting a new @SpeechSynthesisTask@ . This operation requires all the standard information needed for speech synthesis, plus the name of an Amazon S3 bucket for the service to store the output of the synthesis task and two optional parameters (OutputS3KeyPrefix and SnsTopicArn). Once the synthesis task is created, this operation will return a SpeechSynthesisTask object, which will include an identifier of this task as well as the current status.
module Network.AWS.Polly.StartSpeechSynthesisTask
    (
    -- * Creating a request
      StartSpeechSynthesisTask (..)
    , mkStartSpeechSynthesisTask
    -- ** Request lenses
    , ssstOutputFormat
    , ssstOutputS3BucketName
    , ssstText
    , ssstVoiceId
    , ssstEngine
    , ssstLanguageCode
    , ssstLexiconNames
    , ssstOutputS3KeyPrefix
    , ssstSampleRate
    , ssstSnsTopicArn
    , ssstSpeechMarkTypes
    , ssstTextType

    -- * Destructuring the response
    , StartSpeechSynthesisTaskResponse (..)
    , mkStartSpeechSynthesisTaskResponse
    -- ** Response lenses
    , ssstrrsSynthesisTask
    , ssstrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Polly.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartSpeechSynthesisTask' smart constructor.
data StartSpeechSynthesisTask = StartSpeechSynthesisTask'
  { outputFormat :: Types.OutputFormat
    -- ^ The format in which the returned output will be encoded. For audio stream, this will be mp3, ogg_vorbis, or pcm. For speech marks, this will be json. 
  , outputS3BucketName :: Types.OutputS3BucketName
    -- ^ Amazon S3 bucket name to which the output file will be saved.
  , text :: Types.Text
    -- ^ The input text to synthesize. If you specify ssml as the TextType, follow the SSML format for the input text. 
  , voiceId :: Types.VoiceId
    -- ^ Voice ID to use for the synthesis. 
  , engine :: Core.Maybe Types.Engine
    -- ^ Specifies the engine (@standard@ or @neural@ ) for Amazon Polly to use when processing input text for speech synthesis. Using a voice that is not supported for the engine selected will result in an error.
  , languageCode :: Core.Maybe Types.LanguageCode
    -- ^ Optional language code for the Speech Synthesis request. This is only necessary if using a bilingual voice, such as Aditi, which can be used for either Indian English (en-IN) or Hindi (hi-IN). 
--
-- If a bilingual voice is used and no language code is specified, Amazon Polly will use the default language of the bilingual voice. The default language for any voice is the one returned by the <https://docs.aws.amazon.com/polly/latest/dg/API_DescribeVoices.html DescribeVoices> operation for the @LanguageCode@ parameter. For example, if no language code is specified, Aditi will use Indian English rather than Hindi.
  , lexiconNames :: Core.Maybe [Types.LexiconName]
    -- ^ List of one or more pronunciation lexicon names you want the service to apply during synthesis. Lexicons are applied only if the language of the lexicon is the same as the language of the voice. 
  , outputS3KeyPrefix :: Core.Maybe Types.OutputS3KeyPrefix
    -- ^ The Amazon S3 key prefix for the output speech file.
  , sampleRate :: Core.Maybe Types.SampleRate
    -- ^ The audio frequency specified in Hz.
--
-- The valid values for mp3 and ogg_vorbis are "8000", "16000", "22050", and "24000". The default value for standard voices is "22050". The default value for neural voices is "24000".
-- Valid values for pcm are "8000" and "16000" The default value is "16000". 
  , snsTopicArn :: Core.Maybe Types.SnsTopicArn
    -- ^ ARN for the SNS topic optionally used for providing status notification for a speech synthesis task.
  , speechMarkTypes :: Core.Maybe [Types.SpeechMarkType]
    -- ^ The type of speech marks returned for the input text.
  , textType :: Core.Maybe Types.TextType
    -- ^ Specifies whether the input text is plain text or SSML. The default value is plain text. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartSpeechSynthesisTask' value with any optional fields omitted.
mkStartSpeechSynthesisTask
    :: Types.OutputFormat -- ^ 'outputFormat'
    -> Types.OutputS3BucketName -- ^ 'outputS3BucketName'
    -> Types.Text -- ^ 'text'
    -> Types.VoiceId -- ^ 'voiceId'
    -> StartSpeechSynthesisTask
mkStartSpeechSynthesisTask outputFormat outputS3BucketName text
  voiceId
  = StartSpeechSynthesisTask'{outputFormat, outputS3BucketName, text,
                              voiceId, engine = Core.Nothing, languageCode = Core.Nothing,
                              lexiconNames = Core.Nothing, outputS3KeyPrefix = Core.Nothing,
                              sampleRate = Core.Nothing, snsTopicArn = Core.Nothing,
                              speechMarkTypes = Core.Nothing, textType = Core.Nothing}

-- | The format in which the returned output will be encoded. For audio stream, this will be mp3, ogg_vorbis, or pcm. For speech marks, this will be json. 
--
-- /Note:/ Consider using 'outputFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssstOutputFormat :: Lens.Lens' StartSpeechSynthesisTask Types.OutputFormat
ssstOutputFormat = Lens.field @"outputFormat"
{-# INLINEABLE ssstOutputFormat #-}
{-# DEPRECATED outputFormat "Use generic-lens or generic-optics with 'outputFormat' instead"  #-}

-- | Amazon S3 bucket name to which the output file will be saved.
--
-- /Note:/ Consider using 'outputS3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssstOutputS3BucketName :: Lens.Lens' StartSpeechSynthesisTask Types.OutputS3BucketName
ssstOutputS3BucketName = Lens.field @"outputS3BucketName"
{-# INLINEABLE ssstOutputS3BucketName #-}
{-# DEPRECATED outputS3BucketName "Use generic-lens or generic-optics with 'outputS3BucketName' instead"  #-}

-- | The input text to synthesize. If you specify ssml as the TextType, follow the SSML format for the input text. 
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssstText :: Lens.Lens' StartSpeechSynthesisTask Types.Text
ssstText = Lens.field @"text"
{-# INLINEABLE ssstText #-}
{-# DEPRECATED text "Use generic-lens or generic-optics with 'text' instead"  #-}

-- | Voice ID to use for the synthesis. 
--
-- /Note:/ Consider using 'voiceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssstVoiceId :: Lens.Lens' StartSpeechSynthesisTask Types.VoiceId
ssstVoiceId = Lens.field @"voiceId"
{-# INLINEABLE ssstVoiceId #-}
{-# DEPRECATED voiceId "Use generic-lens or generic-optics with 'voiceId' instead"  #-}

-- | Specifies the engine (@standard@ or @neural@ ) for Amazon Polly to use when processing input text for speech synthesis. Using a voice that is not supported for the engine selected will result in an error.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssstEngine :: Lens.Lens' StartSpeechSynthesisTask (Core.Maybe Types.Engine)
ssstEngine = Lens.field @"engine"
{-# INLINEABLE ssstEngine #-}
{-# DEPRECATED engine "Use generic-lens or generic-optics with 'engine' instead"  #-}

-- | Optional language code for the Speech Synthesis request. This is only necessary if using a bilingual voice, such as Aditi, which can be used for either Indian English (en-IN) or Hindi (hi-IN). 
--
-- If a bilingual voice is used and no language code is specified, Amazon Polly will use the default language of the bilingual voice. The default language for any voice is the one returned by the <https://docs.aws.amazon.com/polly/latest/dg/API_DescribeVoices.html DescribeVoices> operation for the @LanguageCode@ parameter. For example, if no language code is specified, Aditi will use Indian English rather than Hindi.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssstLanguageCode :: Lens.Lens' StartSpeechSynthesisTask (Core.Maybe Types.LanguageCode)
ssstLanguageCode = Lens.field @"languageCode"
{-# INLINEABLE ssstLanguageCode #-}
{-# DEPRECATED languageCode "Use generic-lens or generic-optics with 'languageCode' instead"  #-}

-- | List of one or more pronunciation lexicon names you want the service to apply during synthesis. Lexicons are applied only if the language of the lexicon is the same as the language of the voice. 
--
-- /Note:/ Consider using 'lexiconNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssstLexiconNames :: Lens.Lens' StartSpeechSynthesisTask (Core.Maybe [Types.LexiconName])
ssstLexiconNames = Lens.field @"lexiconNames"
{-# INLINEABLE ssstLexiconNames #-}
{-# DEPRECATED lexiconNames "Use generic-lens or generic-optics with 'lexiconNames' instead"  #-}

-- | The Amazon S3 key prefix for the output speech file.
--
-- /Note:/ Consider using 'outputS3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssstOutputS3KeyPrefix :: Lens.Lens' StartSpeechSynthesisTask (Core.Maybe Types.OutputS3KeyPrefix)
ssstOutputS3KeyPrefix = Lens.field @"outputS3KeyPrefix"
{-# INLINEABLE ssstOutputS3KeyPrefix #-}
{-# DEPRECATED outputS3KeyPrefix "Use generic-lens or generic-optics with 'outputS3KeyPrefix' instead"  #-}

-- | The audio frequency specified in Hz.
--
-- The valid values for mp3 and ogg_vorbis are "8000", "16000", "22050", and "24000". The default value for standard voices is "22050". The default value for neural voices is "24000".
-- Valid values for pcm are "8000" and "16000" The default value is "16000". 
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssstSampleRate :: Lens.Lens' StartSpeechSynthesisTask (Core.Maybe Types.SampleRate)
ssstSampleRate = Lens.field @"sampleRate"
{-# INLINEABLE ssstSampleRate #-}
{-# DEPRECATED sampleRate "Use generic-lens or generic-optics with 'sampleRate' instead"  #-}

-- | ARN for the SNS topic optionally used for providing status notification for a speech synthesis task.
--
-- /Note:/ Consider using 'snsTopicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssstSnsTopicArn :: Lens.Lens' StartSpeechSynthesisTask (Core.Maybe Types.SnsTopicArn)
ssstSnsTopicArn = Lens.field @"snsTopicArn"
{-# INLINEABLE ssstSnsTopicArn #-}
{-# DEPRECATED snsTopicArn "Use generic-lens or generic-optics with 'snsTopicArn' instead"  #-}

-- | The type of speech marks returned for the input text.
--
-- /Note:/ Consider using 'speechMarkTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssstSpeechMarkTypes :: Lens.Lens' StartSpeechSynthesisTask (Core.Maybe [Types.SpeechMarkType])
ssstSpeechMarkTypes = Lens.field @"speechMarkTypes"
{-# INLINEABLE ssstSpeechMarkTypes #-}
{-# DEPRECATED speechMarkTypes "Use generic-lens or generic-optics with 'speechMarkTypes' instead"  #-}

-- | Specifies whether the input text is plain text or SSML. The default value is plain text. 
--
-- /Note:/ Consider using 'textType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssstTextType :: Lens.Lens' StartSpeechSynthesisTask (Core.Maybe Types.TextType)
ssstTextType = Lens.field @"textType"
{-# INLINEABLE ssstTextType #-}
{-# DEPRECATED textType "Use generic-lens or generic-optics with 'textType' instead"  #-}

instance Core.ToQuery StartSpeechSynthesisTask where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartSpeechSynthesisTask where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON StartSpeechSynthesisTask where
        toJSON StartSpeechSynthesisTask{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("OutputFormat" Core..= outputFormat),
                  Core.Just ("OutputS3BucketName" Core..= outputS3BucketName),
                  Core.Just ("Text" Core..= text),
                  Core.Just ("VoiceId" Core..= voiceId),
                  ("Engine" Core..=) Core.<$> engine,
                  ("LanguageCode" Core..=) Core.<$> languageCode,
                  ("LexiconNames" Core..=) Core.<$> lexiconNames,
                  ("OutputS3KeyPrefix" Core..=) Core.<$> outputS3KeyPrefix,
                  ("SampleRate" Core..=) Core.<$> sampleRate,
                  ("SnsTopicArn" Core..=) Core.<$> snsTopicArn,
                  ("SpeechMarkTypes" Core..=) Core.<$> speechMarkTypes,
                  ("TextType" Core..=) Core.<$> textType])

instance Core.AWSRequest StartSpeechSynthesisTask where
        type Rs StartSpeechSynthesisTask = StartSpeechSynthesisTaskResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/v1/synthesisTasks",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartSpeechSynthesisTaskResponse' Core.<$>
                   (x Core..:? "SynthesisTask") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartSpeechSynthesisTaskResponse' smart constructor.
data StartSpeechSynthesisTaskResponse = StartSpeechSynthesisTaskResponse'
  { synthesisTask :: Core.Maybe Types.SynthesisTask
    -- ^ SynthesisTask object that provides information and attributes about a newly submitted speech synthesis task.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StartSpeechSynthesisTaskResponse' value with any optional fields omitted.
mkStartSpeechSynthesisTaskResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartSpeechSynthesisTaskResponse
mkStartSpeechSynthesisTaskResponse responseStatus
  = StartSpeechSynthesisTaskResponse'{synthesisTask = Core.Nothing,
                                      responseStatus}

-- | SynthesisTask object that provides information and attributes about a newly submitted speech synthesis task.
--
-- /Note:/ Consider using 'synthesisTask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssstrrsSynthesisTask :: Lens.Lens' StartSpeechSynthesisTaskResponse (Core.Maybe Types.SynthesisTask)
ssstrrsSynthesisTask = Lens.field @"synthesisTask"
{-# INLINEABLE ssstrrsSynthesisTask #-}
{-# DEPRECATED synthesisTask "Use generic-lens or generic-optics with 'synthesisTask' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssstrrsResponseStatus :: Lens.Lens' StartSpeechSynthesisTaskResponse Core.Int
ssstrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ssstrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
