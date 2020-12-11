{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.SynthesizeSpeech
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Synthesizes UTF-8 input, plain text or SSML, to a stream of bytes. SSML input must be valid, well-formed SSML. Some alphabets might not be available with all the voices (for example, Cyrillic might not be read at all by English voices) unless phoneme mapping is used. For more information, see <https://docs.aws.amazon.com/polly/latest/dg/how-text-to-speech-works.html How it Works> .
module Network.AWS.Polly.SynthesizeSpeech
  ( -- * Creating a request
    SynthesizeSpeech (..),
    mkSynthesizeSpeech,

    -- ** Request lenses
    ssLanguageCode,
    ssEngine,
    ssSpeechMarkTypes,
    ssSampleRate,
    ssTextType,
    ssLexiconNames,
    ssOutputFormat,
    ssText,
    ssVoiceId,

    -- * Destructuring the response
    SynthesizeSpeechResponse (..),
    mkSynthesizeSpeechResponse,

    -- ** Response lenses
    ssrsRequestCharacters,
    ssrsContentType,
    ssrsResponseStatus,
    ssrsAudioStream,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Polly.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSynthesizeSpeech' smart constructor.
data SynthesizeSpeech = SynthesizeSpeech'
  { languageCode ::
      Lude.Maybe LanguageCode,
    engine :: Lude.Maybe Engine,
    speechMarkTypes :: Lude.Maybe [SpeechMarkType],
    sampleRate :: Lude.Maybe Lude.Text,
    textType :: Lude.Maybe TextType,
    lexiconNames :: Lude.Maybe [Lude.Text],
    outputFormat :: OutputFormat,
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

-- | Creates a value of 'SynthesizeSpeech' with the minimum fields required to make a request.
--
-- * 'engine' - Specifies the engine (@standard@ or @neural@ ) for Amazon Polly to use when processing input text for speech synthesis. For information on Amazon Polly voices and which voices are available in standard-only, NTTS-only, and both standard and NTTS formats, see <https://docs.aws.amazon.com/polly/latest/dg/voicelist.html Available Voices> .
--
-- __NTTS-only voices__
-- When using NTTS-only voices such as Kevin (en-US), this parameter is required and must be set to @neural@ . If the engine is not specified, or is set to @standard@ , this will result in an error.
-- Type: String
-- Valid Values: @standard@ | @neural@
-- Required: Yes
-- __Standard voices__
-- For standard voices, this is not required; the engine parameter defaults to @standard@ . If the engine is not specified, or is set to @standard@ and an NTTS-only voice is selected, this will result in an error.
-- * 'languageCode' - Optional language code for the Synthesize Speech request. This is only necessary if using a bilingual voice, such as Aditi, which can be used for either Indian English (en-IN) or Hindi (hi-IN).
--
-- If a bilingual voice is used and no language code is specified, Amazon Polly will use the default language of the bilingual voice. The default language for any voice is the one returned by the <https://docs.aws.amazon.com/polly/latest/dg/API_DescribeVoices.html DescribeVoices> operation for the @LanguageCode@ parameter. For example, if no language code is specified, Aditi will use Indian English rather than Hindi.
-- * 'lexiconNames' - List of one or more pronunciation lexicon names you want the service to apply during synthesis. Lexicons are applied only if the language of the lexicon is the same as the language of the voice. For information about storing lexicons, see <https://docs.aws.amazon.com/polly/latest/dg/API_PutLexicon.html PutLexicon> .
-- * 'outputFormat' - The format in which the returned output will be encoded. For audio stream, this will be mp3, ogg_vorbis, or pcm. For speech marks, this will be json.
--
-- When pcm is used, the content returned is audio/pcm in a signed 16-bit, 1 channel (mono), little-endian format.
-- * 'sampleRate' - The audio frequency specified in Hz.
--
-- The valid values for mp3 and ogg_vorbis are "8000", "16000", "22050", and "24000". The default value for standard voices is "22050". The default value for neural voices is "24000".
-- Valid values for pcm are "8000" and "16000" The default value is "16000".
-- * 'speechMarkTypes' - The type of speech marks returned for the input text.
-- * 'text' - Input text to synthesize. If you specify @ssml@ as the @TextType@ , follow the SSML format for the input text.
-- * 'textType' - Specifies whether the input text is plain text or SSML. The default value is plain text. For more information, see <https://docs.aws.amazon.com/polly/latest/dg/ssml.html Using SSML> .
-- * 'voiceId' - Voice ID to use for the synthesis. You can get a list of available voice IDs by calling the <https://docs.aws.amazon.com/polly/latest/dg/API_DescribeVoices.html DescribeVoices> operation.
mkSynthesizeSpeech ::
  -- | 'outputFormat'
  OutputFormat ->
  -- | 'text'
  Lude.Text ->
  -- | 'voiceId'
  VoiceId ->
  SynthesizeSpeech
mkSynthesizeSpeech pOutputFormat_ pText_ pVoiceId_ =
  SynthesizeSpeech'
    { languageCode = Lude.Nothing,
      engine = Lude.Nothing,
      speechMarkTypes = Lude.Nothing,
      sampleRate = Lude.Nothing,
      textType = Lude.Nothing,
      lexiconNames = Lude.Nothing,
      outputFormat = pOutputFormat_,
      text = pText_,
      voiceId = pVoiceId_
    }

-- | Optional language code for the Synthesize Speech request. This is only necessary if using a bilingual voice, such as Aditi, which can be used for either Indian English (en-IN) or Hindi (hi-IN).
--
-- If a bilingual voice is used and no language code is specified, Amazon Polly will use the default language of the bilingual voice. The default language for any voice is the one returned by the <https://docs.aws.amazon.com/polly/latest/dg/API_DescribeVoices.html DescribeVoices> operation for the @LanguageCode@ parameter. For example, if no language code is specified, Aditi will use Indian English rather than Hindi.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssLanguageCode :: Lens.Lens' SynthesizeSpeech (Lude.Maybe LanguageCode)
ssLanguageCode = Lens.lens (languageCode :: SynthesizeSpeech -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: SynthesizeSpeech)
{-# DEPRECATED ssLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | Specifies the engine (@standard@ or @neural@ ) for Amazon Polly to use when processing input text for speech synthesis. For information on Amazon Polly voices and which voices are available in standard-only, NTTS-only, and both standard and NTTS formats, see <https://docs.aws.amazon.com/polly/latest/dg/voicelist.html Available Voices> .
--
-- __NTTS-only voices__
-- When using NTTS-only voices such as Kevin (en-US), this parameter is required and must be set to @neural@ . If the engine is not specified, or is set to @standard@ , this will result in an error.
-- Type: String
-- Valid Values: @standard@ | @neural@
-- Required: Yes
-- __Standard voices__
-- For standard voices, this is not required; the engine parameter defaults to @standard@ . If the engine is not specified, or is set to @standard@ and an NTTS-only voice is selected, this will result in an error.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssEngine :: Lens.Lens' SynthesizeSpeech (Lude.Maybe Engine)
ssEngine = Lens.lens (engine :: SynthesizeSpeech -> Lude.Maybe Engine) (\s a -> s {engine = a} :: SynthesizeSpeech)
{-# DEPRECATED ssEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The type of speech marks returned for the input text.
--
-- /Note:/ Consider using 'speechMarkTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssSpeechMarkTypes :: Lens.Lens' SynthesizeSpeech (Lude.Maybe [SpeechMarkType])
ssSpeechMarkTypes = Lens.lens (speechMarkTypes :: SynthesizeSpeech -> Lude.Maybe [SpeechMarkType]) (\s a -> s {speechMarkTypes = a} :: SynthesizeSpeech)
{-# DEPRECATED ssSpeechMarkTypes "Use generic-lens or generic-optics with 'speechMarkTypes' instead." #-}

-- | The audio frequency specified in Hz.
--
-- The valid values for mp3 and ogg_vorbis are "8000", "16000", "22050", and "24000". The default value for standard voices is "22050". The default value for neural voices is "24000".
-- Valid values for pcm are "8000" and "16000" The default value is "16000".
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssSampleRate :: Lens.Lens' SynthesizeSpeech (Lude.Maybe Lude.Text)
ssSampleRate = Lens.lens (sampleRate :: SynthesizeSpeech -> Lude.Maybe Lude.Text) (\s a -> s {sampleRate = a} :: SynthesizeSpeech)
{-# DEPRECATED ssSampleRate "Use generic-lens or generic-optics with 'sampleRate' instead." #-}

-- | Specifies whether the input text is plain text or SSML. The default value is plain text. For more information, see <https://docs.aws.amazon.com/polly/latest/dg/ssml.html Using SSML> .
--
-- /Note:/ Consider using 'textType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssTextType :: Lens.Lens' SynthesizeSpeech (Lude.Maybe TextType)
ssTextType = Lens.lens (textType :: SynthesizeSpeech -> Lude.Maybe TextType) (\s a -> s {textType = a} :: SynthesizeSpeech)
{-# DEPRECATED ssTextType "Use generic-lens or generic-optics with 'textType' instead." #-}

-- | List of one or more pronunciation lexicon names you want the service to apply during synthesis. Lexicons are applied only if the language of the lexicon is the same as the language of the voice. For information about storing lexicons, see <https://docs.aws.amazon.com/polly/latest/dg/API_PutLexicon.html PutLexicon> .
--
-- /Note:/ Consider using 'lexiconNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssLexiconNames :: Lens.Lens' SynthesizeSpeech (Lude.Maybe [Lude.Text])
ssLexiconNames = Lens.lens (lexiconNames :: SynthesizeSpeech -> Lude.Maybe [Lude.Text]) (\s a -> s {lexiconNames = a} :: SynthesizeSpeech)
{-# DEPRECATED ssLexiconNames "Use generic-lens or generic-optics with 'lexiconNames' instead." #-}

-- | The format in which the returned output will be encoded. For audio stream, this will be mp3, ogg_vorbis, or pcm. For speech marks, this will be json.
--
-- When pcm is used, the content returned is audio/pcm in a signed 16-bit, 1 channel (mono), little-endian format.
--
-- /Note:/ Consider using 'outputFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssOutputFormat :: Lens.Lens' SynthesizeSpeech OutputFormat
ssOutputFormat = Lens.lens (outputFormat :: SynthesizeSpeech -> OutputFormat) (\s a -> s {outputFormat = a} :: SynthesizeSpeech)
{-# DEPRECATED ssOutputFormat "Use generic-lens or generic-optics with 'outputFormat' instead." #-}

-- | Input text to synthesize. If you specify @ssml@ as the @TextType@ , follow the SSML format for the input text.
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssText :: Lens.Lens' SynthesizeSpeech Lude.Text
ssText = Lens.lens (text :: SynthesizeSpeech -> Lude.Text) (\s a -> s {text = a} :: SynthesizeSpeech)
{-# DEPRECATED ssText "Use generic-lens or generic-optics with 'text' instead." #-}

-- | Voice ID to use for the synthesis. You can get a list of available voice IDs by calling the <https://docs.aws.amazon.com/polly/latest/dg/API_DescribeVoices.html DescribeVoices> operation.
--
-- /Note:/ Consider using 'voiceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssVoiceId :: Lens.Lens' SynthesizeSpeech VoiceId
ssVoiceId = Lens.lens (voiceId :: SynthesizeSpeech -> VoiceId) (\s a -> s {voiceId = a} :: SynthesizeSpeech)
{-# DEPRECATED ssVoiceId "Use generic-lens or generic-optics with 'voiceId' instead." #-}

instance Lude.AWSRequest SynthesizeSpeech where
  type Rs SynthesizeSpeech = SynthesizeSpeechResponse
  request = Req.postJSON pollyService
  response =
    Res.receiveBody
      ( \s h x ->
          SynthesizeSpeechResponse'
            Lude.<$> (h Lude..#? "x-amzn-RequestCharacters")
            Lude.<*> (h Lude..#? "Content-Type")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (Lude.pure x)
      )

instance Lude.ToHeaders SynthesizeSpeech where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON SynthesizeSpeech where
  toJSON SynthesizeSpeech' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("LanguageCode" Lude..=) Lude.<$> languageCode,
            ("Engine" Lude..=) Lude.<$> engine,
            ("SpeechMarkTypes" Lude..=) Lude.<$> speechMarkTypes,
            ("SampleRate" Lude..=) Lude.<$> sampleRate,
            ("TextType" Lude..=) Lude.<$> textType,
            ("LexiconNames" Lude..=) Lude.<$> lexiconNames,
            Lude.Just ("OutputFormat" Lude..= outputFormat),
            Lude.Just ("Text" Lude..= text),
            Lude.Just ("VoiceId" Lude..= voiceId)
          ]
      )

instance Lude.ToPath SynthesizeSpeech where
  toPath = Lude.const "/v1/speech"

instance Lude.ToQuery SynthesizeSpeech where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSynthesizeSpeechResponse' smart constructor.
data SynthesizeSpeechResponse = SynthesizeSpeechResponse'
  { requestCharacters ::
      Lude.Maybe Lude.Int,
    contentType :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    audioStream :: Lude.RsBody
  }
  deriving stock (Lude.Show, Lude.Generic)

-- | Creates a value of 'SynthesizeSpeechResponse' with the minimum fields required to make a request.
--
-- * 'audioStream' - Stream containing the synthesized speech.
-- * 'contentType' - Specifies the type audio stream. This should reflect the @OutputFormat@ parameter in your request.
--
--
--     * If you request @mp3@ as the @OutputFormat@ , the @ContentType@ returned is audio/mpeg.
--
--
--     * If you request @ogg_vorbis@ as the @OutputFormat@ , the @ContentType@ returned is audio/ogg.
--
--
--     * If you request @pcm@ as the @OutputFormat@ , the @ContentType@ returned is audio/pcm in a signed 16-bit, 1 channel (mono), little-endian format.
--
--
--     * If you request @json@ as the @OutputFormat@ , the @ContentType@ returned is audio/json.
--
--
--
-- * 'requestCharacters' - Number of characters synthesized.
-- * 'responseStatus' - The response status code.
mkSynthesizeSpeechResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'audioStream'
  Lude.RsBody ->
  SynthesizeSpeechResponse
mkSynthesizeSpeechResponse pResponseStatus_ pAudioStream_ =
  SynthesizeSpeechResponse'
    { requestCharacters = Lude.Nothing,
      contentType = Lude.Nothing,
      responseStatus = pResponseStatus_,
      audioStream = pAudioStream_
    }

-- | Number of characters synthesized.
--
-- /Note:/ Consider using 'requestCharacters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssrsRequestCharacters :: Lens.Lens' SynthesizeSpeechResponse (Lude.Maybe Lude.Int)
ssrsRequestCharacters = Lens.lens (requestCharacters :: SynthesizeSpeechResponse -> Lude.Maybe Lude.Int) (\s a -> s {requestCharacters = a} :: SynthesizeSpeechResponse)
{-# DEPRECATED ssrsRequestCharacters "Use generic-lens or generic-optics with 'requestCharacters' instead." #-}

-- | Specifies the type audio stream. This should reflect the @OutputFormat@ parameter in your request.
--
--
--     * If you request @mp3@ as the @OutputFormat@ , the @ContentType@ returned is audio/mpeg.
--
--
--     * If you request @ogg_vorbis@ as the @OutputFormat@ , the @ContentType@ returned is audio/ogg.
--
--
--     * If you request @pcm@ as the @OutputFormat@ , the @ContentType@ returned is audio/pcm in a signed 16-bit, 1 channel (mono), little-endian format.
--
--
--     * If you request @json@ as the @OutputFormat@ , the @ContentType@ returned is audio/json.
--
--
--
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssrsContentType :: Lens.Lens' SynthesizeSpeechResponse (Lude.Maybe Lude.Text)
ssrsContentType = Lens.lens (contentType :: SynthesizeSpeechResponse -> Lude.Maybe Lude.Text) (\s a -> s {contentType = a} :: SynthesizeSpeechResponse)
{-# DEPRECATED ssrsContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssrsResponseStatus :: Lens.Lens' SynthesizeSpeechResponse Lude.Int
ssrsResponseStatus = Lens.lens (responseStatus :: SynthesizeSpeechResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SynthesizeSpeechResponse)
{-# DEPRECATED ssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Stream containing the synthesized speech.
--
-- /Note:/ Consider using 'audioStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssrsAudioStream :: Lens.Lens' SynthesizeSpeechResponse Lude.RsBody
ssrsAudioStream = Lens.lens (audioStream :: SynthesizeSpeechResponse -> Lude.RsBody) (\s a -> s {audioStream = a} :: SynthesizeSpeechResponse)
{-# DEPRECATED ssrsAudioStream "Use generic-lens or generic-optics with 'audioStream' instead." #-}
