{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.SynthesisTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Polly.Types.SynthesisTask
  ( SynthesisTask (..)
  -- * Smart constructor
  , mkSynthesisTask
  -- * Lenses
  , stCreationTime
  , stEngine
  , stLanguageCode
  , stLexiconNames
  , stOutputFormat
  , stOutputUri
  , stRequestCharacters
  , stSampleRate
  , stSnsTopicArn
  , stSpeechMarkTypes
  , stTaskId
  , stTaskStatus
  , stTaskStatusReason
  , stTextType
  , stVoiceId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Polly.Types.Engine as Types
import qualified Network.AWS.Polly.Types.LanguageCode as Types
import qualified Network.AWS.Polly.Types.LexiconName as Types
import qualified Network.AWS.Polly.Types.OutputFormat as Types
import qualified Network.AWS.Polly.Types.OutputUri as Types
import qualified Network.AWS.Polly.Types.SampleRate as Types
import qualified Network.AWS.Polly.Types.SnsTopicArn as Types
import qualified Network.AWS.Polly.Types.SpeechMarkType as Types
import qualified Network.AWS.Polly.Types.TaskId as Types
import qualified Network.AWS.Polly.Types.TaskStatus as Types
import qualified Network.AWS.Polly.Types.TaskStatusReason as Types
import qualified Network.AWS.Polly.Types.TextType as Types
import qualified Network.AWS.Polly.Types.VoiceId as Types
import qualified Network.AWS.Prelude as Core

-- | SynthesisTask object that provides information about a speech synthesis task.
--
-- /See:/ 'mkSynthesisTask' smart constructor.
data SynthesisTask = SynthesisTask'
  { creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ Timestamp for the time the synthesis task was started.
  , engine :: Core.Maybe Types.Engine
    -- ^ Specifies the engine (@standard@ or @neural@ ) for Amazon Polly to use when processing input text for speech synthesis. Using a voice that is not supported for the engine selected will result in an error.
  , languageCode :: Core.Maybe Types.LanguageCode
    -- ^ Optional language code for a synthesis task. This is only necessary if using a bilingual voice, such as Aditi, which can be used for either Indian English (en-IN) or Hindi (hi-IN). 
--
-- If a bilingual voice is used and no language code is specified, Amazon Polly will use the default language of the bilingual voice. The default language for any voice is the one returned by the <https://docs.aws.amazon.com/polly/latest/dg/API_DescribeVoices.html DescribeVoices> operation for the @LanguageCode@ parameter. For example, if no language code is specified, Aditi will use Indian English rather than Hindi.
  , lexiconNames :: Core.Maybe [Types.LexiconName]
    -- ^ List of one or more pronunciation lexicon names you want the service to apply during synthesis. Lexicons are applied only if the language of the lexicon is the same as the language of the voice. 
  , outputFormat :: Core.Maybe Types.OutputFormat
    -- ^ The format in which the returned output will be encoded. For audio stream, this will be mp3, ogg_vorbis, or pcm. For speech marks, this will be json. 
  , outputUri :: Core.Maybe Types.OutputUri
    -- ^ Pathway for the output speech file.
  , requestCharacters :: Core.Maybe Core.Int
    -- ^ Number of billable characters synthesized.
  , sampleRate :: Core.Maybe Types.SampleRate
    -- ^ The audio frequency specified in Hz.
--
-- The valid values for mp3 and ogg_vorbis are "8000", "16000", "22050", and "24000". The default value for standard voices is "22050". The default value for neural voices is "24000".
-- Valid values for pcm are "8000" and "16000" The default value is "16000". 
  , snsTopicArn :: Core.Maybe Types.SnsTopicArn
    -- ^ ARN for the SNS topic optionally used for providing status notification for a speech synthesis task.
  , speechMarkTypes :: Core.Maybe [Types.SpeechMarkType]
    -- ^ The type of speech marks returned for the input text.
  , taskId :: Core.Maybe Types.TaskId
    -- ^ The Amazon Polly generated identifier for a speech synthesis task.
  , taskStatus :: Core.Maybe Types.TaskStatus
    -- ^ Current status of the individual speech synthesis task.
  , taskStatusReason :: Core.Maybe Types.TaskStatusReason
    -- ^ Reason for the current status of a specific speech synthesis task, including errors if the task has failed.
  , textType :: Core.Maybe Types.TextType
    -- ^ Specifies whether the input text is plain text or SSML. The default value is plain text. 
  , voiceId :: Core.Maybe Types.VoiceId
    -- ^ Voice ID to use for the synthesis. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'SynthesisTask' value with any optional fields omitted.
mkSynthesisTask
    :: SynthesisTask
mkSynthesisTask
  = SynthesisTask'{creationTime = Core.Nothing,
                   engine = Core.Nothing, languageCode = Core.Nothing,
                   lexiconNames = Core.Nothing, outputFormat = Core.Nothing,
                   outputUri = Core.Nothing, requestCharacters = Core.Nothing,
                   sampleRate = Core.Nothing, snsTopicArn = Core.Nothing,
                   speechMarkTypes = Core.Nothing, taskId = Core.Nothing,
                   taskStatus = Core.Nothing, taskStatusReason = Core.Nothing,
                   textType = Core.Nothing, voiceId = Core.Nothing}

-- | Timestamp for the time the synthesis task was started.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stCreationTime :: Lens.Lens' SynthesisTask (Core.Maybe Core.NominalDiffTime)
stCreationTime = Lens.field @"creationTime"
{-# INLINEABLE stCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | Specifies the engine (@standard@ or @neural@ ) for Amazon Polly to use when processing input text for speech synthesis. Using a voice that is not supported for the engine selected will result in an error.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stEngine :: Lens.Lens' SynthesisTask (Core.Maybe Types.Engine)
stEngine = Lens.field @"engine"
{-# INLINEABLE stEngine #-}
{-# DEPRECATED engine "Use generic-lens or generic-optics with 'engine' instead"  #-}

-- | Optional language code for a synthesis task. This is only necessary if using a bilingual voice, such as Aditi, which can be used for either Indian English (en-IN) or Hindi (hi-IN). 
--
-- If a bilingual voice is used and no language code is specified, Amazon Polly will use the default language of the bilingual voice. The default language for any voice is the one returned by the <https://docs.aws.amazon.com/polly/latest/dg/API_DescribeVoices.html DescribeVoices> operation for the @LanguageCode@ parameter. For example, if no language code is specified, Aditi will use Indian English rather than Hindi.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stLanguageCode :: Lens.Lens' SynthesisTask (Core.Maybe Types.LanguageCode)
stLanguageCode = Lens.field @"languageCode"
{-# INLINEABLE stLanguageCode #-}
{-# DEPRECATED languageCode "Use generic-lens or generic-optics with 'languageCode' instead"  #-}

-- | List of one or more pronunciation lexicon names you want the service to apply during synthesis. Lexicons are applied only if the language of the lexicon is the same as the language of the voice. 
--
-- /Note:/ Consider using 'lexiconNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stLexiconNames :: Lens.Lens' SynthesisTask (Core.Maybe [Types.LexiconName])
stLexiconNames = Lens.field @"lexiconNames"
{-# INLINEABLE stLexiconNames #-}
{-# DEPRECATED lexiconNames "Use generic-lens or generic-optics with 'lexiconNames' instead"  #-}

-- | The format in which the returned output will be encoded. For audio stream, this will be mp3, ogg_vorbis, or pcm. For speech marks, this will be json. 
--
-- /Note:/ Consider using 'outputFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stOutputFormat :: Lens.Lens' SynthesisTask (Core.Maybe Types.OutputFormat)
stOutputFormat = Lens.field @"outputFormat"
{-# INLINEABLE stOutputFormat #-}
{-# DEPRECATED outputFormat "Use generic-lens or generic-optics with 'outputFormat' instead"  #-}

-- | Pathway for the output speech file.
--
-- /Note:/ Consider using 'outputUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stOutputUri :: Lens.Lens' SynthesisTask (Core.Maybe Types.OutputUri)
stOutputUri = Lens.field @"outputUri"
{-# INLINEABLE stOutputUri #-}
{-# DEPRECATED outputUri "Use generic-lens or generic-optics with 'outputUri' instead"  #-}

-- | Number of billable characters synthesized.
--
-- /Note:/ Consider using 'requestCharacters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stRequestCharacters :: Lens.Lens' SynthesisTask (Core.Maybe Core.Int)
stRequestCharacters = Lens.field @"requestCharacters"
{-# INLINEABLE stRequestCharacters #-}
{-# DEPRECATED requestCharacters "Use generic-lens or generic-optics with 'requestCharacters' instead"  #-}

-- | The audio frequency specified in Hz.
--
-- The valid values for mp3 and ogg_vorbis are "8000", "16000", "22050", and "24000". The default value for standard voices is "22050". The default value for neural voices is "24000".
-- Valid values for pcm are "8000" and "16000" The default value is "16000". 
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stSampleRate :: Lens.Lens' SynthesisTask (Core.Maybe Types.SampleRate)
stSampleRate = Lens.field @"sampleRate"
{-# INLINEABLE stSampleRate #-}
{-# DEPRECATED sampleRate "Use generic-lens or generic-optics with 'sampleRate' instead"  #-}

-- | ARN for the SNS topic optionally used for providing status notification for a speech synthesis task.
--
-- /Note:/ Consider using 'snsTopicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stSnsTopicArn :: Lens.Lens' SynthesisTask (Core.Maybe Types.SnsTopicArn)
stSnsTopicArn = Lens.field @"snsTopicArn"
{-# INLINEABLE stSnsTopicArn #-}
{-# DEPRECATED snsTopicArn "Use generic-lens or generic-optics with 'snsTopicArn' instead"  #-}

-- | The type of speech marks returned for the input text.
--
-- /Note:/ Consider using 'speechMarkTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stSpeechMarkTypes :: Lens.Lens' SynthesisTask (Core.Maybe [Types.SpeechMarkType])
stSpeechMarkTypes = Lens.field @"speechMarkTypes"
{-# INLINEABLE stSpeechMarkTypes #-}
{-# DEPRECATED speechMarkTypes "Use generic-lens or generic-optics with 'speechMarkTypes' instead"  #-}

-- | The Amazon Polly generated identifier for a speech synthesis task.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stTaskId :: Lens.Lens' SynthesisTask (Core.Maybe Types.TaskId)
stTaskId = Lens.field @"taskId"
{-# INLINEABLE stTaskId #-}
{-# DEPRECATED taskId "Use generic-lens or generic-optics with 'taskId' instead"  #-}

-- | Current status of the individual speech synthesis task.
--
-- /Note:/ Consider using 'taskStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stTaskStatus :: Lens.Lens' SynthesisTask (Core.Maybe Types.TaskStatus)
stTaskStatus = Lens.field @"taskStatus"
{-# INLINEABLE stTaskStatus #-}
{-# DEPRECATED taskStatus "Use generic-lens or generic-optics with 'taskStatus' instead"  #-}

-- | Reason for the current status of a specific speech synthesis task, including errors if the task has failed.
--
-- /Note:/ Consider using 'taskStatusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stTaskStatusReason :: Lens.Lens' SynthesisTask (Core.Maybe Types.TaskStatusReason)
stTaskStatusReason = Lens.field @"taskStatusReason"
{-# INLINEABLE stTaskStatusReason #-}
{-# DEPRECATED taskStatusReason "Use generic-lens or generic-optics with 'taskStatusReason' instead"  #-}

-- | Specifies whether the input text is plain text or SSML. The default value is plain text. 
--
-- /Note:/ Consider using 'textType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stTextType :: Lens.Lens' SynthesisTask (Core.Maybe Types.TextType)
stTextType = Lens.field @"textType"
{-# INLINEABLE stTextType #-}
{-# DEPRECATED textType "Use generic-lens or generic-optics with 'textType' instead"  #-}

-- | Voice ID to use for the synthesis. 
--
-- /Note:/ Consider using 'voiceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stVoiceId :: Lens.Lens' SynthesisTask (Core.Maybe Types.VoiceId)
stVoiceId = Lens.field @"voiceId"
{-# INLINEABLE stVoiceId #-}
{-# DEPRECATED voiceId "Use generic-lens or generic-optics with 'voiceId' instead"  #-}

instance Core.FromJSON SynthesisTask where
        parseJSON
          = Core.withObject "SynthesisTask" Core.$
              \ x ->
                SynthesisTask' Core.<$>
                  (x Core..:? "CreationTime") Core.<*> x Core..:? "Engine" Core.<*>
                    x Core..:? "LanguageCode"
                    Core.<*> x Core..:? "LexiconNames"
                    Core.<*> x Core..:? "OutputFormat"
                    Core.<*> x Core..:? "OutputUri"
                    Core.<*> x Core..:? "RequestCharacters"
                    Core.<*> x Core..:? "SampleRate"
                    Core.<*> x Core..:? "SnsTopicArn"
                    Core.<*> x Core..:? "SpeechMarkTypes"
                    Core.<*> x Core..:? "TaskId"
                    Core.<*> x Core..:? "TaskStatus"
                    Core.<*> x Core..:? "TaskStatusReason"
                    Core.<*> x Core..:? "TextType"
                    Core.<*> x Core..:? "VoiceId"
