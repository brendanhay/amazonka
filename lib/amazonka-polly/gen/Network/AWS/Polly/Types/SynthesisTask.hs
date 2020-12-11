-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.SynthesisTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Polly.Types.SynthesisTask
  ( SynthesisTask (..),

    -- * Smart constructor
    mkSynthesisTask,

    -- * Lenses
    stCreationTime,
    stLanguageCode,
    stSNSTopicARN,
    stTaskStatusReason,
    stTaskId,
    stRequestCharacters,
    stEngine,
    stSpeechMarkTypes,
    stSampleRate,
    stOutputFormat,
    stTextType,
    stVoiceId,
    stLexiconNames,
    stTaskStatus,
    stOutputURI,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Polly.Types.Engine
import Network.AWS.Polly.Types.LanguageCode
import Network.AWS.Polly.Types.OutputFormat
import Network.AWS.Polly.Types.SpeechMarkType
import Network.AWS.Polly.Types.TaskStatus
import Network.AWS.Polly.Types.TextType
import Network.AWS.Polly.Types.VoiceId
import qualified Network.AWS.Prelude as Lude

-- | SynthesisTask object that provides information about a speech synthesis task.
--
-- /See:/ 'mkSynthesisTask' smart constructor.
data SynthesisTask = SynthesisTask'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    languageCode :: Lude.Maybe LanguageCode,
    snsTopicARN :: Lude.Maybe Lude.Text,
    taskStatusReason :: Lude.Maybe Lude.Text,
    taskId :: Lude.Maybe Lude.Text,
    requestCharacters :: Lude.Maybe Lude.Int,
    engine :: Lude.Maybe Engine,
    speechMarkTypes :: Lude.Maybe [SpeechMarkType],
    sampleRate :: Lude.Maybe Lude.Text,
    outputFormat :: Lude.Maybe OutputFormat,
    textType :: Lude.Maybe TextType,
    voiceId :: Lude.Maybe VoiceId,
    lexiconNames :: Lude.Maybe [Lude.Text],
    taskStatus :: Lude.Maybe TaskStatus,
    outputURI :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SynthesisTask' with the minimum fields required to make a request.
--
-- * 'creationTime' - Timestamp for the time the synthesis task was started.
-- * 'engine' - Specifies the engine (@standard@ or @neural@ ) for Amazon Polly to use when processing input text for speech synthesis. Using a voice that is not supported for the engine selected will result in an error.
-- * 'languageCode' - Optional language code for a synthesis task. This is only necessary if using a bilingual voice, such as Aditi, which can be used for either Indian English (en-IN) or Hindi (hi-IN).
--
-- If a bilingual voice is used and no language code is specified, Amazon Polly will use the default language of the bilingual voice. The default language for any voice is the one returned by the <https://docs.aws.amazon.com/polly/latest/dg/API_DescribeVoices.html DescribeVoices> operation for the @LanguageCode@ parameter. For example, if no language code is specified, Aditi will use Indian English rather than Hindi.
-- * 'lexiconNames' - List of one or more pronunciation lexicon names you want the service to apply during synthesis. Lexicons are applied only if the language of the lexicon is the same as the language of the voice.
-- * 'outputFormat' - The format in which the returned output will be encoded. For audio stream, this will be mp3, ogg_vorbis, or pcm. For speech marks, this will be json.
-- * 'outputURI' - Pathway for the output speech file.
-- * 'requestCharacters' - Number of billable characters synthesized.
-- * 'sampleRate' - The audio frequency specified in Hz.
--
-- The valid values for mp3 and ogg_vorbis are "8000", "16000", "22050", and "24000". The default value for standard voices is "22050". The default value for neural voices is "24000".
-- Valid values for pcm are "8000" and "16000" The default value is "16000".
-- * 'snsTopicARN' - ARN for the SNS topic optionally used for providing status notification for a speech synthesis task.
-- * 'speechMarkTypes' - The type of speech marks returned for the input text.
-- * 'taskId' - The Amazon Polly generated identifier for a speech synthesis task.
-- * 'taskStatus' - Current status of the individual speech synthesis task.
-- * 'taskStatusReason' - Reason for the current status of a specific speech synthesis task, including errors if the task has failed.
-- * 'textType' - Specifies whether the input text is plain text or SSML. The default value is plain text.
-- * 'voiceId' - Voice ID to use for the synthesis.
mkSynthesisTask ::
  SynthesisTask
mkSynthesisTask =
  SynthesisTask'
    { creationTime = Lude.Nothing,
      languageCode = Lude.Nothing,
      snsTopicARN = Lude.Nothing,
      taskStatusReason = Lude.Nothing,
      taskId = Lude.Nothing,
      requestCharacters = Lude.Nothing,
      engine = Lude.Nothing,
      speechMarkTypes = Lude.Nothing,
      sampleRate = Lude.Nothing,
      outputFormat = Lude.Nothing,
      textType = Lude.Nothing,
      voiceId = Lude.Nothing,
      lexiconNames = Lude.Nothing,
      taskStatus = Lude.Nothing,
      outputURI = Lude.Nothing
    }

-- | Timestamp for the time the synthesis task was started.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stCreationTime :: Lens.Lens' SynthesisTask (Lude.Maybe Lude.Timestamp)
stCreationTime = Lens.lens (creationTime :: SynthesisTask -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: SynthesisTask)
{-# DEPRECATED stCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Optional language code for a synthesis task. This is only necessary if using a bilingual voice, such as Aditi, which can be used for either Indian English (en-IN) or Hindi (hi-IN).
--
-- If a bilingual voice is used and no language code is specified, Amazon Polly will use the default language of the bilingual voice. The default language for any voice is the one returned by the <https://docs.aws.amazon.com/polly/latest/dg/API_DescribeVoices.html DescribeVoices> operation for the @LanguageCode@ parameter. For example, if no language code is specified, Aditi will use Indian English rather than Hindi.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stLanguageCode :: Lens.Lens' SynthesisTask (Lude.Maybe LanguageCode)
stLanguageCode = Lens.lens (languageCode :: SynthesisTask -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: SynthesisTask)
{-# DEPRECATED stLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | ARN for the SNS topic optionally used for providing status notification for a speech synthesis task.
--
-- /Note:/ Consider using 'snsTopicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stSNSTopicARN :: Lens.Lens' SynthesisTask (Lude.Maybe Lude.Text)
stSNSTopicARN = Lens.lens (snsTopicARN :: SynthesisTask -> Lude.Maybe Lude.Text) (\s a -> s {snsTopicARN = a} :: SynthesisTask)
{-# DEPRECATED stSNSTopicARN "Use generic-lens or generic-optics with 'snsTopicARN' instead." #-}

-- | Reason for the current status of a specific speech synthesis task, including errors if the task has failed.
--
-- /Note:/ Consider using 'taskStatusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stTaskStatusReason :: Lens.Lens' SynthesisTask (Lude.Maybe Lude.Text)
stTaskStatusReason = Lens.lens (taskStatusReason :: SynthesisTask -> Lude.Maybe Lude.Text) (\s a -> s {taskStatusReason = a} :: SynthesisTask)
{-# DEPRECATED stTaskStatusReason "Use generic-lens or generic-optics with 'taskStatusReason' instead." #-}

-- | The Amazon Polly generated identifier for a speech synthesis task.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stTaskId :: Lens.Lens' SynthesisTask (Lude.Maybe Lude.Text)
stTaskId = Lens.lens (taskId :: SynthesisTask -> Lude.Maybe Lude.Text) (\s a -> s {taskId = a} :: SynthesisTask)
{-# DEPRECATED stTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

-- | Number of billable characters synthesized.
--
-- /Note:/ Consider using 'requestCharacters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stRequestCharacters :: Lens.Lens' SynthesisTask (Lude.Maybe Lude.Int)
stRequestCharacters = Lens.lens (requestCharacters :: SynthesisTask -> Lude.Maybe Lude.Int) (\s a -> s {requestCharacters = a} :: SynthesisTask)
{-# DEPRECATED stRequestCharacters "Use generic-lens or generic-optics with 'requestCharacters' instead." #-}

-- | Specifies the engine (@standard@ or @neural@ ) for Amazon Polly to use when processing input text for speech synthesis. Using a voice that is not supported for the engine selected will result in an error.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stEngine :: Lens.Lens' SynthesisTask (Lude.Maybe Engine)
stEngine = Lens.lens (engine :: SynthesisTask -> Lude.Maybe Engine) (\s a -> s {engine = a} :: SynthesisTask)
{-# DEPRECATED stEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The type of speech marks returned for the input text.
--
-- /Note:/ Consider using 'speechMarkTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stSpeechMarkTypes :: Lens.Lens' SynthesisTask (Lude.Maybe [SpeechMarkType])
stSpeechMarkTypes = Lens.lens (speechMarkTypes :: SynthesisTask -> Lude.Maybe [SpeechMarkType]) (\s a -> s {speechMarkTypes = a} :: SynthesisTask)
{-# DEPRECATED stSpeechMarkTypes "Use generic-lens or generic-optics with 'speechMarkTypes' instead." #-}

-- | The audio frequency specified in Hz.
--
-- The valid values for mp3 and ogg_vorbis are "8000", "16000", "22050", and "24000". The default value for standard voices is "22050". The default value for neural voices is "24000".
-- Valid values for pcm are "8000" and "16000" The default value is "16000".
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stSampleRate :: Lens.Lens' SynthesisTask (Lude.Maybe Lude.Text)
stSampleRate = Lens.lens (sampleRate :: SynthesisTask -> Lude.Maybe Lude.Text) (\s a -> s {sampleRate = a} :: SynthesisTask)
{-# DEPRECATED stSampleRate "Use generic-lens or generic-optics with 'sampleRate' instead." #-}

-- | The format in which the returned output will be encoded. For audio stream, this will be mp3, ogg_vorbis, or pcm. For speech marks, this will be json.
--
-- /Note:/ Consider using 'outputFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stOutputFormat :: Lens.Lens' SynthesisTask (Lude.Maybe OutputFormat)
stOutputFormat = Lens.lens (outputFormat :: SynthesisTask -> Lude.Maybe OutputFormat) (\s a -> s {outputFormat = a} :: SynthesisTask)
{-# DEPRECATED stOutputFormat "Use generic-lens or generic-optics with 'outputFormat' instead." #-}

-- | Specifies whether the input text is plain text or SSML. The default value is plain text.
--
-- /Note:/ Consider using 'textType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stTextType :: Lens.Lens' SynthesisTask (Lude.Maybe TextType)
stTextType = Lens.lens (textType :: SynthesisTask -> Lude.Maybe TextType) (\s a -> s {textType = a} :: SynthesisTask)
{-# DEPRECATED stTextType "Use generic-lens or generic-optics with 'textType' instead." #-}

-- | Voice ID to use for the synthesis.
--
-- /Note:/ Consider using 'voiceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stVoiceId :: Lens.Lens' SynthesisTask (Lude.Maybe VoiceId)
stVoiceId = Lens.lens (voiceId :: SynthesisTask -> Lude.Maybe VoiceId) (\s a -> s {voiceId = a} :: SynthesisTask)
{-# DEPRECATED stVoiceId "Use generic-lens or generic-optics with 'voiceId' instead." #-}

-- | List of one or more pronunciation lexicon names you want the service to apply during synthesis. Lexicons are applied only if the language of the lexicon is the same as the language of the voice.
--
-- /Note:/ Consider using 'lexiconNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stLexiconNames :: Lens.Lens' SynthesisTask (Lude.Maybe [Lude.Text])
stLexiconNames = Lens.lens (lexiconNames :: SynthesisTask -> Lude.Maybe [Lude.Text]) (\s a -> s {lexiconNames = a} :: SynthesisTask)
{-# DEPRECATED stLexiconNames "Use generic-lens or generic-optics with 'lexiconNames' instead." #-}

-- | Current status of the individual speech synthesis task.
--
-- /Note:/ Consider using 'taskStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stTaskStatus :: Lens.Lens' SynthesisTask (Lude.Maybe TaskStatus)
stTaskStatus = Lens.lens (taskStatus :: SynthesisTask -> Lude.Maybe TaskStatus) (\s a -> s {taskStatus = a} :: SynthesisTask)
{-# DEPRECATED stTaskStatus "Use generic-lens or generic-optics with 'taskStatus' instead." #-}

-- | Pathway for the output speech file.
--
-- /Note:/ Consider using 'outputURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stOutputURI :: Lens.Lens' SynthesisTask (Lude.Maybe Lude.Text)
stOutputURI = Lens.lens (outputURI :: SynthesisTask -> Lude.Maybe Lude.Text) (\s a -> s {outputURI = a} :: SynthesisTask)
{-# DEPRECATED stOutputURI "Use generic-lens or generic-optics with 'outputURI' instead." #-}

instance Lude.FromJSON SynthesisTask where
  parseJSON =
    Lude.withObject
      "SynthesisTask"
      ( \x ->
          SynthesisTask'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "LanguageCode")
            Lude.<*> (x Lude..:? "SnsTopicArn")
            Lude.<*> (x Lude..:? "TaskStatusReason")
            Lude.<*> (x Lude..:? "TaskId")
            Lude.<*> (x Lude..:? "RequestCharacters")
            Lude.<*> (x Lude..:? "Engine")
            Lude.<*> (x Lude..:? "SpeechMarkTypes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "SampleRate")
            Lude.<*> (x Lude..:? "OutputFormat")
            Lude.<*> (x Lude..:? "TextType")
            Lude.<*> (x Lude..:? "VoiceId")
            Lude.<*> (x Lude..:? "LexiconNames" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "TaskStatus")
            Lude.<*> (x Lude..:? "OutputUri")
      )
