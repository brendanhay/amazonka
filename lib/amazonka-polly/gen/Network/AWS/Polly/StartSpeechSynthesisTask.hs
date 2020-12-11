{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    StartSpeechSynthesisTask (..),
    mkStartSpeechSynthesisTask,

    -- ** Request lenses
    ssstLanguageCode,
    ssstSNSTopicARN,
    ssstOutputS3KeyPrefix,
    ssstEngine,
    ssstSpeechMarkTypes,
    ssstSampleRate,
    ssstTextType,
    ssstLexiconNames,
    ssstOutputFormat,
    ssstOutputS3BucketName,
    ssstText,
    ssstVoiceId,

    -- * Destructuring the response
    StartSpeechSynthesisTaskResponse (..),
    mkStartSpeechSynthesisTaskResponse,

    -- ** Response lenses
    ssstrsSynthesisTask,
    ssstrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Polly.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartSpeechSynthesisTask' smart constructor.
data StartSpeechSynthesisTask = StartSpeechSynthesisTask'
  { languageCode ::
      Lude.Maybe LanguageCode,
    snsTopicARN :: Lude.Maybe Lude.Text,
    outputS3KeyPrefix :: Lude.Maybe Lude.Text,
    engine :: Lude.Maybe Engine,
    speechMarkTypes ::
      Lude.Maybe [SpeechMarkType],
    sampleRate :: Lude.Maybe Lude.Text,
    textType :: Lude.Maybe TextType,
    lexiconNames :: Lude.Maybe [Lude.Text],
    outputFormat :: OutputFormat,
    outputS3BucketName :: Lude.Text,
    text :: Lude.Text,
    voiceId :: VoiceId
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartSpeechSynthesisTask' with the minimum fields required to make a request.
--
-- * 'engine' - Specifies the engine (@standard@ or @neural@ ) for Amazon Polly to use when processing input text for speech synthesis. Using a voice that is not supported for the engine selected will result in an error.
-- * 'languageCode' - Optional language code for the Speech Synthesis request. This is only necessary if using a bilingual voice, such as Aditi, which can be used for either Indian English (en-IN) or Hindi (hi-IN).
--
-- If a bilingual voice is used and no language code is specified, Amazon Polly will use the default language of the bilingual voice. The default language for any voice is the one returned by the <https://docs.aws.amazon.com/polly/latest/dg/API_DescribeVoices.html DescribeVoices> operation for the @LanguageCode@ parameter. For example, if no language code is specified, Aditi will use Indian English rather than Hindi.
-- * 'lexiconNames' - List of one or more pronunciation lexicon names you want the service to apply during synthesis. Lexicons are applied only if the language of the lexicon is the same as the language of the voice.
-- * 'outputFormat' - The format in which the returned output will be encoded. For audio stream, this will be mp3, ogg_vorbis, or pcm. For speech marks, this will be json.
-- * 'outputS3BucketName' - Amazon S3 bucket name to which the output file will be saved.
-- * 'outputS3KeyPrefix' - The Amazon S3 key prefix for the output speech file.
-- * 'sampleRate' - The audio frequency specified in Hz.
--
-- The valid values for mp3 and ogg_vorbis are "8000", "16000", "22050", and "24000". The default value for standard voices is "22050". The default value for neural voices is "24000".
-- Valid values for pcm are "8000" and "16000" The default value is "16000".
-- * 'snsTopicARN' - ARN for the SNS topic optionally used for providing status notification for a speech synthesis task.
-- * 'speechMarkTypes' - The type of speech marks returned for the input text.
-- * 'text' - The input text to synthesize. If you specify ssml as the TextType, follow the SSML format for the input text.
-- * 'textType' - Specifies whether the input text is plain text or SSML. The default value is plain text.
-- * 'voiceId' - Voice ID to use for the synthesis.
mkStartSpeechSynthesisTask ::
  -- | 'outputFormat'
  OutputFormat ->
  -- | 'outputS3BucketName'
  Lude.Text ->
  -- | 'text'
  Lude.Text ->
  -- | 'voiceId'
  VoiceId ->
  StartSpeechSynthesisTask
mkStartSpeechSynthesisTask
  pOutputFormat_
  pOutputS3BucketName_
  pText_
  pVoiceId_ =
    StartSpeechSynthesisTask'
      { languageCode = Lude.Nothing,
        snsTopicARN = Lude.Nothing,
        outputS3KeyPrefix = Lude.Nothing,
        engine = Lude.Nothing,
        speechMarkTypes = Lude.Nothing,
        sampleRate = Lude.Nothing,
        textType = Lude.Nothing,
        lexiconNames = Lude.Nothing,
        outputFormat = pOutputFormat_,
        outputS3BucketName = pOutputS3BucketName_,
        text = pText_,
        voiceId = pVoiceId_
      }

-- | Optional language code for the Speech Synthesis request. This is only necessary if using a bilingual voice, such as Aditi, which can be used for either Indian English (en-IN) or Hindi (hi-IN).
--
-- If a bilingual voice is used and no language code is specified, Amazon Polly will use the default language of the bilingual voice. The default language for any voice is the one returned by the <https://docs.aws.amazon.com/polly/latest/dg/API_DescribeVoices.html DescribeVoices> operation for the @LanguageCode@ parameter. For example, if no language code is specified, Aditi will use Indian English rather than Hindi.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssstLanguageCode :: Lens.Lens' StartSpeechSynthesisTask (Lude.Maybe LanguageCode)
ssstLanguageCode = Lens.lens (languageCode :: StartSpeechSynthesisTask -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: StartSpeechSynthesisTask)
{-# DEPRECATED ssstLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | ARN for the SNS topic optionally used for providing status notification for a speech synthesis task.
--
-- /Note:/ Consider using 'snsTopicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssstSNSTopicARN :: Lens.Lens' StartSpeechSynthesisTask (Lude.Maybe Lude.Text)
ssstSNSTopicARN = Lens.lens (snsTopicARN :: StartSpeechSynthesisTask -> Lude.Maybe Lude.Text) (\s a -> s {snsTopicARN = a} :: StartSpeechSynthesisTask)
{-# DEPRECATED ssstSNSTopicARN "Use generic-lens or generic-optics with 'snsTopicARN' instead." #-}

-- | The Amazon S3 key prefix for the output speech file.
--
-- /Note:/ Consider using 'outputS3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssstOutputS3KeyPrefix :: Lens.Lens' StartSpeechSynthesisTask (Lude.Maybe Lude.Text)
ssstOutputS3KeyPrefix = Lens.lens (outputS3KeyPrefix :: StartSpeechSynthesisTask -> Lude.Maybe Lude.Text) (\s a -> s {outputS3KeyPrefix = a} :: StartSpeechSynthesisTask)
{-# DEPRECATED ssstOutputS3KeyPrefix "Use generic-lens or generic-optics with 'outputS3KeyPrefix' instead." #-}

-- | Specifies the engine (@standard@ or @neural@ ) for Amazon Polly to use when processing input text for speech synthesis. Using a voice that is not supported for the engine selected will result in an error.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssstEngine :: Lens.Lens' StartSpeechSynthesisTask (Lude.Maybe Engine)
ssstEngine = Lens.lens (engine :: StartSpeechSynthesisTask -> Lude.Maybe Engine) (\s a -> s {engine = a} :: StartSpeechSynthesisTask)
{-# DEPRECATED ssstEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The type of speech marks returned for the input text.
--
-- /Note:/ Consider using 'speechMarkTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssstSpeechMarkTypes :: Lens.Lens' StartSpeechSynthesisTask (Lude.Maybe [SpeechMarkType])
ssstSpeechMarkTypes = Lens.lens (speechMarkTypes :: StartSpeechSynthesisTask -> Lude.Maybe [SpeechMarkType]) (\s a -> s {speechMarkTypes = a} :: StartSpeechSynthesisTask)
{-# DEPRECATED ssstSpeechMarkTypes "Use generic-lens or generic-optics with 'speechMarkTypes' instead." #-}

-- | The audio frequency specified in Hz.
--
-- The valid values for mp3 and ogg_vorbis are "8000", "16000", "22050", and "24000". The default value for standard voices is "22050". The default value for neural voices is "24000".
-- Valid values for pcm are "8000" and "16000" The default value is "16000".
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssstSampleRate :: Lens.Lens' StartSpeechSynthesisTask (Lude.Maybe Lude.Text)
ssstSampleRate = Lens.lens (sampleRate :: StartSpeechSynthesisTask -> Lude.Maybe Lude.Text) (\s a -> s {sampleRate = a} :: StartSpeechSynthesisTask)
{-# DEPRECATED ssstSampleRate "Use generic-lens or generic-optics with 'sampleRate' instead." #-}

-- | Specifies whether the input text is plain text or SSML. The default value is plain text.
--
-- /Note:/ Consider using 'textType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssstTextType :: Lens.Lens' StartSpeechSynthesisTask (Lude.Maybe TextType)
ssstTextType = Lens.lens (textType :: StartSpeechSynthesisTask -> Lude.Maybe TextType) (\s a -> s {textType = a} :: StartSpeechSynthesisTask)
{-# DEPRECATED ssstTextType "Use generic-lens or generic-optics with 'textType' instead." #-}

-- | List of one or more pronunciation lexicon names you want the service to apply during synthesis. Lexicons are applied only if the language of the lexicon is the same as the language of the voice.
--
-- /Note:/ Consider using 'lexiconNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssstLexiconNames :: Lens.Lens' StartSpeechSynthesisTask (Lude.Maybe [Lude.Text])
ssstLexiconNames = Lens.lens (lexiconNames :: StartSpeechSynthesisTask -> Lude.Maybe [Lude.Text]) (\s a -> s {lexiconNames = a} :: StartSpeechSynthesisTask)
{-# DEPRECATED ssstLexiconNames "Use generic-lens or generic-optics with 'lexiconNames' instead." #-}

-- | The format in which the returned output will be encoded. For audio stream, this will be mp3, ogg_vorbis, or pcm. For speech marks, this will be json.
--
-- /Note:/ Consider using 'outputFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssstOutputFormat :: Lens.Lens' StartSpeechSynthesisTask OutputFormat
ssstOutputFormat = Lens.lens (outputFormat :: StartSpeechSynthesisTask -> OutputFormat) (\s a -> s {outputFormat = a} :: StartSpeechSynthesisTask)
{-# DEPRECATED ssstOutputFormat "Use generic-lens or generic-optics with 'outputFormat' instead." #-}

-- | Amazon S3 bucket name to which the output file will be saved.
--
-- /Note:/ Consider using 'outputS3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssstOutputS3BucketName :: Lens.Lens' StartSpeechSynthesisTask Lude.Text
ssstOutputS3BucketName = Lens.lens (outputS3BucketName :: StartSpeechSynthesisTask -> Lude.Text) (\s a -> s {outputS3BucketName = a} :: StartSpeechSynthesisTask)
{-# DEPRECATED ssstOutputS3BucketName "Use generic-lens or generic-optics with 'outputS3BucketName' instead." #-}

-- | The input text to synthesize. If you specify ssml as the TextType, follow the SSML format for the input text.
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssstText :: Lens.Lens' StartSpeechSynthesisTask Lude.Text
ssstText = Lens.lens (text :: StartSpeechSynthesisTask -> Lude.Text) (\s a -> s {text = a} :: StartSpeechSynthesisTask)
{-# DEPRECATED ssstText "Use generic-lens or generic-optics with 'text' instead." #-}

-- | Voice ID to use for the synthesis.
--
-- /Note:/ Consider using 'voiceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssstVoiceId :: Lens.Lens' StartSpeechSynthesisTask VoiceId
ssstVoiceId = Lens.lens (voiceId :: StartSpeechSynthesisTask -> VoiceId) (\s a -> s {voiceId = a} :: StartSpeechSynthesisTask)
{-# DEPRECATED ssstVoiceId "Use generic-lens or generic-optics with 'voiceId' instead." #-}

instance Lude.AWSRequest StartSpeechSynthesisTask where
  type Rs StartSpeechSynthesisTask = StartSpeechSynthesisTaskResponse
  request = Req.postJSON pollyService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartSpeechSynthesisTaskResponse'
            Lude.<$> (x Lude..?> "SynthesisTask")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartSpeechSynthesisTask where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON StartSpeechSynthesisTask where
  toJSON StartSpeechSynthesisTask' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("LanguageCode" Lude..=) Lude.<$> languageCode,
            ("SnsTopicArn" Lude..=) Lude.<$> snsTopicARN,
            ("OutputS3KeyPrefix" Lude..=) Lude.<$> outputS3KeyPrefix,
            ("Engine" Lude..=) Lude.<$> engine,
            ("SpeechMarkTypes" Lude..=) Lude.<$> speechMarkTypes,
            ("SampleRate" Lude..=) Lude.<$> sampleRate,
            ("TextType" Lude..=) Lude.<$> textType,
            ("LexiconNames" Lude..=) Lude.<$> lexiconNames,
            Lude.Just ("OutputFormat" Lude..= outputFormat),
            Lude.Just ("OutputS3BucketName" Lude..= outputS3BucketName),
            Lude.Just ("Text" Lude..= text),
            Lude.Just ("VoiceId" Lude..= voiceId)
          ]
      )

instance Lude.ToPath StartSpeechSynthesisTask where
  toPath = Lude.const "/v1/synthesisTasks"

instance Lude.ToQuery StartSpeechSynthesisTask where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartSpeechSynthesisTaskResponse' smart constructor.
data StartSpeechSynthesisTaskResponse = StartSpeechSynthesisTaskResponse'
  { synthesisTask ::
      Lude.Maybe SynthesisTask,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartSpeechSynthesisTaskResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'synthesisTask' - SynthesisTask object that provides information and attributes about a newly submitted speech synthesis task.
mkStartSpeechSynthesisTaskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartSpeechSynthesisTaskResponse
mkStartSpeechSynthesisTaskResponse pResponseStatus_ =
  StartSpeechSynthesisTaskResponse'
    { synthesisTask = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | SynthesisTask object that provides information and attributes about a newly submitted speech synthesis task.
--
-- /Note:/ Consider using 'synthesisTask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssstrsSynthesisTask :: Lens.Lens' StartSpeechSynthesisTaskResponse (Lude.Maybe SynthesisTask)
ssstrsSynthesisTask = Lens.lens (synthesisTask :: StartSpeechSynthesisTaskResponse -> Lude.Maybe SynthesisTask) (\s a -> s {synthesisTask = a} :: StartSpeechSynthesisTaskResponse)
{-# DEPRECATED ssstrsSynthesisTask "Use generic-lens or generic-optics with 'synthesisTask' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssstrsResponseStatus :: Lens.Lens' StartSpeechSynthesisTaskResponse Lude.Int
ssstrsResponseStatus = Lens.lens (responseStatus :: StartSpeechSynthesisTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartSpeechSynthesisTaskResponse)
{-# DEPRECATED ssstrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
