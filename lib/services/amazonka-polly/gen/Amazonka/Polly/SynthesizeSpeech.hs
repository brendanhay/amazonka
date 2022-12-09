{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Polly.SynthesizeSpeech
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Synthesizes UTF-8 input, plain text or SSML, to a stream of bytes. SSML
-- input must be valid, well-formed SSML. Some alphabets might not be
-- available with all the voices (for example, Cyrillic might not be read
-- at all by English voices) unless phoneme mapping is used. For more
-- information, see
-- <https://docs.aws.amazon.com/polly/latest/dg/how-text-to-speech-works.html How it Works>.
module Amazonka.Polly.SynthesizeSpeech
  ( -- * Creating a Request
    SynthesizeSpeech (..),
    newSynthesizeSpeech,

    -- * Request Lenses
    synthesizeSpeech_engine,
    synthesizeSpeech_languageCode,
    synthesizeSpeech_lexiconNames,
    synthesizeSpeech_sampleRate,
    synthesizeSpeech_speechMarkTypes,
    synthesizeSpeech_textType,
    synthesizeSpeech_outputFormat,
    synthesizeSpeech_text,
    synthesizeSpeech_voiceId,

    -- * Destructuring the Response
    SynthesizeSpeechResponse (..),
    newSynthesizeSpeechResponse,

    -- * Response Lenses
    synthesizeSpeechResponse_contentType,
    synthesizeSpeechResponse_requestCharacters,
    synthesizeSpeechResponse_httpStatus,
    synthesizeSpeechResponse_audioStream,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Polly.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSynthesizeSpeech' smart constructor.
data SynthesizeSpeech = SynthesizeSpeech'
  { -- | Specifies the engine (@standard@ or @neural@) for Amazon Polly to use
    -- when processing input text for speech synthesis. For information on
    -- Amazon Polly voices and which voices are available in standard-only,
    -- NTTS-only, and both standard and NTTS formats, see
    -- <https://docs.aws.amazon.com/polly/latest/dg/voicelist.html Available Voices>.
    --
    -- __NTTS-only voices__
    --
    -- When using NTTS-only voices such as Kevin (en-US), this parameter is
    -- required and must be set to @neural@. If the engine is not specified, or
    -- is set to @standard@, this will result in an error.
    --
    -- Type: String
    --
    -- Valid Values: @standard@ | @neural@
    --
    -- Required: Yes
    --
    -- __Standard voices__
    --
    -- For standard voices, this is not required; the engine parameter defaults
    -- to @standard@. If the engine is not specified, or is set to @standard@
    -- and an NTTS-only voice is selected, this will result in an error.
    engine :: Prelude.Maybe Engine,
    -- | Optional language code for the Synthesize Speech request. This is only
    -- necessary if using a bilingual voice, such as Aditi, which can be used
    -- for either Indian English (en-IN) or Hindi (hi-IN).
    --
    -- If a bilingual voice is used and no language code is specified, Amazon
    -- Polly uses the default language of the bilingual voice. The default
    -- language for any voice is the one returned by the
    -- <https://docs.aws.amazon.com/polly/latest/dg/API_DescribeVoices.html DescribeVoices>
    -- operation for the @LanguageCode@ parameter. For example, if no language
    -- code is specified, Aditi will use Indian English rather than Hindi.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | List of one or more pronunciation lexicon names you want the service to
    -- apply during synthesis. Lexicons are applied only if the language of the
    -- lexicon is the same as the language of the voice. For information about
    -- storing lexicons, see
    -- <https://docs.aws.amazon.com/polly/latest/dg/API_PutLexicon.html PutLexicon>.
    lexiconNames :: Prelude.Maybe [Prelude.Text],
    -- | The audio frequency specified in Hz.
    --
    -- The valid values for mp3 and ogg_vorbis are \"8000\", \"16000\",
    -- \"22050\", and \"24000\". The default value for standard voices is
    -- \"22050\". The default value for neural voices is \"24000\".
    --
    -- Valid values for pcm are \"8000\" and \"16000\" The default value is
    -- \"16000\".
    sampleRate :: Prelude.Maybe Prelude.Text,
    -- | The type of speech marks returned for the input text.
    speechMarkTypes :: Prelude.Maybe [SpeechMarkType],
    -- | Specifies whether the input text is plain text or SSML. The default
    -- value is plain text. For more information, see
    -- <https://docs.aws.amazon.com/polly/latest/dg/ssml.html Using SSML>.
    textType :: Prelude.Maybe TextType,
    -- | The format in which the returned output will be encoded. For audio
    -- stream, this will be mp3, ogg_vorbis, or pcm. For speech marks, this
    -- will be json.
    --
    -- When pcm is used, the content returned is audio\/pcm in a signed 16-bit,
    -- 1 channel (mono), little-endian format.
    outputFormat :: OutputFormat,
    -- | Input text to synthesize. If you specify @ssml@ as the @TextType@,
    -- follow the SSML format for the input text.
    text :: Prelude.Text,
    -- | Voice ID to use for the synthesis. You can get a list of available voice
    -- IDs by calling the
    -- <https://docs.aws.amazon.com/polly/latest/dg/API_DescribeVoices.html DescribeVoices>
    -- operation.
    voiceId :: VoiceId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SynthesizeSpeech' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engine', 'synthesizeSpeech_engine' - Specifies the engine (@standard@ or @neural@) for Amazon Polly to use
-- when processing input text for speech synthesis. For information on
-- Amazon Polly voices and which voices are available in standard-only,
-- NTTS-only, and both standard and NTTS formats, see
-- <https://docs.aws.amazon.com/polly/latest/dg/voicelist.html Available Voices>.
--
-- __NTTS-only voices__
--
-- When using NTTS-only voices such as Kevin (en-US), this parameter is
-- required and must be set to @neural@. If the engine is not specified, or
-- is set to @standard@, this will result in an error.
--
-- Type: String
--
-- Valid Values: @standard@ | @neural@
--
-- Required: Yes
--
-- __Standard voices__
--
-- For standard voices, this is not required; the engine parameter defaults
-- to @standard@. If the engine is not specified, or is set to @standard@
-- and an NTTS-only voice is selected, this will result in an error.
--
-- 'languageCode', 'synthesizeSpeech_languageCode' - Optional language code for the Synthesize Speech request. This is only
-- necessary if using a bilingual voice, such as Aditi, which can be used
-- for either Indian English (en-IN) or Hindi (hi-IN).
--
-- If a bilingual voice is used and no language code is specified, Amazon
-- Polly uses the default language of the bilingual voice. The default
-- language for any voice is the one returned by the
-- <https://docs.aws.amazon.com/polly/latest/dg/API_DescribeVoices.html DescribeVoices>
-- operation for the @LanguageCode@ parameter. For example, if no language
-- code is specified, Aditi will use Indian English rather than Hindi.
--
-- 'lexiconNames', 'synthesizeSpeech_lexiconNames' - List of one or more pronunciation lexicon names you want the service to
-- apply during synthesis. Lexicons are applied only if the language of the
-- lexicon is the same as the language of the voice. For information about
-- storing lexicons, see
-- <https://docs.aws.amazon.com/polly/latest/dg/API_PutLexicon.html PutLexicon>.
--
-- 'sampleRate', 'synthesizeSpeech_sampleRate' - The audio frequency specified in Hz.
--
-- The valid values for mp3 and ogg_vorbis are \"8000\", \"16000\",
-- \"22050\", and \"24000\". The default value for standard voices is
-- \"22050\". The default value for neural voices is \"24000\".
--
-- Valid values for pcm are \"8000\" and \"16000\" The default value is
-- \"16000\".
--
-- 'speechMarkTypes', 'synthesizeSpeech_speechMarkTypes' - The type of speech marks returned for the input text.
--
-- 'textType', 'synthesizeSpeech_textType' - Specifies whether the input text is plain text or SSML. The default
-- value is plain text. For more information, see
-- <https://docs.aws.amazon.com/polly/latest/dg/ssml.html Using SSML>.
--
-- 'outputFormat', 'synthesizeSpeech_outputFormat' - The format in which the returned output will be encoded. For audio
-- stream, this will be mp3, ogg_vorbis, or pcm. For speech marks, this
-- will be json.
--
-- When pcm is used, the content returned is audio\/pcm in a signed 16-bit,
-- 1 channel (mono), little-endian format.
--
-- 'text', 'synthesizeSpeech_text' - Input text to synthesize. If you specify @ssml@ as the @TextType@,
-- follow the SSML format for the input text.
--
-- 'voiceId', 'synthesizeSpeech_voiceId' - Voice ID to use for the synthesis. You can get a list of available voice
-- IDs by calling the
-- <https://docs.aws.amazon.com/polly/latest/dg/API_DescribeVoices.html DescribeVoices>
-- operation.
newSynthesizeSpeech ::
  -- | 'outputFormat'
  OutputFormat ->
  -- | 'text'
  Prelude.Text ->
  -- | 'voiceId'
  VoiceId ->
  SynthesizeSpeech
newSynthesizeSpeech pOutputFormat_ pText_ pVoiceId_ =
  SynthesizeSpeech'
    { engine = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      lexiconNames = Prelude.Nothing,
      sampleRate = Prelude.Nothing,
      speechMarkTypes = Prelude.Nothing,
      textType = Prelude.Nothing,
      outputFormat = pOutputFormat_,
      text = pText_,
      voiceId = pVoiceId_
    }

-- | Specifies the engine (@standard@ or @neural@) for Amazon Polly to use
-- when processing input text for speech synthesis. For information on
-- Amazon Polly voices and which voices are available in standard-only,
-- NTTS-only, and both standard and NTTS formats, see
-- <https://docs.aws.amazon.com/polly/latest/dg/voicelist.html Available Voices>.
--
-- __NTTS-only voices__
--
-- When using NTTS-only voices such as Kevin (en-US), this parameter is
-- required and must be set to @neural@. If the engine is not specified, or
-- is set to @standard@, this will result in an error.
--
-- Type: String
--
-- Valid Values: @standard@ | @neural@
--
-- Required: Yes
--
-- __Standard voices__
--
-- For standard voices, this is not required; the engine parameter defaults
-- to @standard@. If the engine is not specified, or is set to @standard@
-- and an NTTS-only voice is selected, this will result in an error.
synthesizeSpeech_engine :: Lens.Lens' SynthesizeSpeech (Prelude.Maybe Engine)
synthesizeSpeech_engine = Lens.lens (\SynthesizeSpeech' {engine} -> engine) (\s@SynthesizeSpeech' {} a -> s {engine = a} :: SynthesizeSpeech)

-- | Optional language code for the Synthesize Speech request. This is only
-- necessary if using a bilingual voice, such as Aditi, which can be used
-- for either Indian English (en-IN) or Hindi (hi-IN).
--
-- If a bilingual voice is used and no language code is specified, Amazon
-- Polly uses the default language of the bilingual voice. The default
-- language for any voice is the one returned by the
-- <https://docs.aws.amazon.com/polly/latest/dg/API_DescribeVoices.html DescribeVoices>
-- operation for the @LanguageCode@ parameter. For example, if no language
-- code is specified, Aditi will use Indian English rather than Hindi.
synthesizeSpeech_languageCode :: Lens.Lens' SynthesizeSpeech (Prelude.Maybe LanguageCode)
synthesizeSpeech_languageCode = Lens.lens (\SynthesizeSpeech' {languageCode} -> languageCode) (\s@SynthesizeSpeech' {} a -> s {languageCode = a} :: SynthesizeSpeech)

-- | List of one or more pronunciation lexicon names you want the service to
-- apply during synthesis. Lexicons are applied only if the language of the
-- lexicon is the same as the language of the voice. For information about
-- storing lexicons, see
-- <https://docs.aws.amazon.com/polly/latest/dg/API_PutLexicon.html PutLexicon>.
synthesizeSpeech_lexiconNames :: Lens.Lens' SynthesizeSpeech (Prelude.Maybe [Prelude.Text])
synthesizeSpeech_lexiconNames = Lens.lens (\SynthesizeSpeech' {lexiconNames} -> lexiconNames) (\s@SynthesizeSpeech' {} a -> s {lexiconNames = a} :: SynthesizeSpeech) Prelude.. Lens.mapping Lens.coerced

-- | The audio frequency specified in Hz.
--
-- The valid values for mp3 and ogg_vorbis are \"8000\", \"16000\",
-- \"22050\", and \"24000\". The default value for standard voices is
-- \"22050\". The default value for neural voices is \"24000\".
--
-- Valid values for pcm are \"8000\" and \"16000\" The default value is
-- \"16000\".
synthesizeSpeech_sampleRate :: Lens.Lens' SynthesizeSpeech (Prelude.Maybe Prelude.Text)
synthesizeSpeech_sampleRate = Lens.lens (\SynthesizeSpeech' {sampleRate} -> sampleRate) (\s@SynthesizeSpeech' {} a -> s {sampleRate = a} :: SynthesizeSpeech)

-- | The type of speech marks returned for the input text.
synthesizeSpeech_speechMarkTypes :: Lens.Lens' SynthesizeSpeech (Prelude.Maybe [SpeechMarkType])
synthesizeSpeech_speechMarkTypes = Lens.lens (\SynthesizeSpeech' {speechMarkTypes} -> speechMarkTypes) (\s@SynthesizeSpeech' {} a -> s {speechMarkTypes = a} :: SynthesizeSpeech) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether the input text is plain text or SSML. The default
-- value is plain text. For more information, see
-- <https://docs.aws.amazon.com/polly/latest/dg/ssml.html Using SSML>.
synthesizeSpeech_textType :: Lens.Lens' SynthesizeSpeech (Prelude.Maybe TextType)
synthesizeSpeech_textType = Lens.lens (\SynthesizeSpeech' {textType} -> textType) (\s@SynthesizeSpeech' {} a -> s {textType = a} :: SynthesizeSpeech)

-- | The format in which the returned output will be encoded. For audio
-- stream, this will be mp3, ogg_vorbis, or pcm. For speech marks, this
-- will be json.
--
-- When pcm is used, the content returned is audio\/pcm in a signed 16-bit,
-- 1 channel (mono), little-endian format.
synthesizeSpeech_outputFormat :: Lens.Lens' SynthesizeSpeech OutputFormat
synthesizeSpeech_outputFormat = Lens.lens (\SynthesizeSpeech' {outputFormat} -> outputFormat) (\s@SynthesizeSpeech' {} a -> s {outputFormat = a} :: SynthesizeSpeech)

-- | Input text to synthesize. If you specify @ssml@ as the @TextType@,
-- follow the SSML format for the input text.
synthesizeSpeech_text :: Lens.Lens' SynthesizeSpeech Prelude.Text
synthesizeSpeech_text = Lens.lens (\SynthesizeSpeech' {text} -> text) (\s@SynthesizeSpeech' {} a -> s {text = a} :: SynthesizeSpeech)

-- | Voice ID to use for the synthesis. You can get a list of available voice
-- IDs by calling the
-- <https://docs.aws.amazon.com/polly/latest/dg/API_DescribeVoices.html DescribeVoices>
-- operation.
synthesizeSpeech_voiceId :: Lens.Lens' SynthesizeSpeech VoiceId
synthesizeSpeech_voiceId = Lens.lens (\SynthesizeSpeech' {voiceId} -> voiceId) (\s@SynthesizeSpeech' {} a -> s {voiceId = a} :: SynthesizeSpeech)

instance Core.AWSRequest SynthesizeSpeech where
  type
    AWSResponse SynthesizeSpeech =
      SynthesizeSpeechResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveBody
      ( \s h x ->
          SynthesizeSpeechResponse'
            Prelude.<$> (h Data..#? "Content-Type")
            Prelude.<*> (h Data..#? "x-amzn-RequestCharacters")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.pure x)
      )

instance Prelude.Hashable SynthesizeSpeech where
  hashWithSalt _salt SynthesizeSpeech' {..} =
    _salt `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` lexiconNames
      `Prelude.hashWithSalt` sampleRate
      `Prelude.hashWithSalt` speechMarkTypes
      `Prelude.hashWithSalt` textType
      `Prelude.hashWithSalt` outputFormat
      `Prelude.hashWithSalt` text
      `Prelude.hashWithSalt` voiceId

instance Prelude.NFData SynthesizeSpeech where
  rnf SynthesizeSpeech' {..} =
    Prelude.rnf engine
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf lexiconNames
      `Prelude.seq` Prelude.rnf sampleRate
      `Prelude.seq` Prelude.rnf speechMarkTypes
      `Prelude.seq` Prelude.rnf textType
      `Prelude.seq` Prelude.rnf outputFormat
      `Prelude.seq` Prelude.rnf text
      `Prelude.seq` Prelude.rnf voiceId

instance Data.ToHeaders SynthesizeSpeech where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON SynthesizeSpeech where
  toJSON SynthesizeSpeech' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Engine" Data..=) Prelude.<$> engine,
            ("LanguageCode" Data..=) Prelude.<$> languageCode,
            ("LexiconNames" Data..=) Prelude.<$> lexiconNames,
            ("SampleRate" Data..=) Prelude.<$> sampleRate,
            ("SpeechMarkTypes" Data..=)
              Prelude.<$> speechMarkTypes,
            ("TextType" Data..=) Prelude.<$> textType,
            Prelude.Just ("OutputFormat" Data..= outputFormat),
            Prelude.Just ("Text" Data..= text),
            Prelude.Just ("VoiceId" Data..= voiceId)
          ]
      )

instance Data.ToPath SynthesizeSpeech where
  toPath = Prelude.const "/v1/speech"

instance Data.ToQuery SynthesizeSpeech where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSynthesizeSpeechResponse' smart constructor.
data SynthesizeSpeechResponse = SynthesizeSpeechResponse'
  { -- | Specifies the type audio stream. This should reflect the @OutputFormat@
    -- parameter in your request.
    --
    -- -   If you request @mp3@ as the @OutputFormat@, the @ContentType@
    --     returned is audio\/mpeg.
    --
    -- -   If you request @ogg_vorbis@ as the @OutputFormat@, the @ContentType@
    --     returned is audio\/ogg.
    --
    -- -   If you request @pcm@ as the @OutputFormat@, the @ContentType@
    --     returned is audio\/pcm in a signed 16-bit, 1 channel (mono),
    --     little-endian format.
    --
    -- -   If you request @json@ as the @OutputFormat@, the @ContentType@
    --     returned is application\/x-json-stream.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | Number of characters synthesized.
    requestCharacters :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Stream containing the synthesized speech.
    audioStream :: Data.ResponseBody
  }
  deriving (Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SynthesizeSpeechResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'synthesizeSpeechResponse_contentType' - Specifies the type audio stream. This should reflect the @OutputFormat@
-- parameter in your request.
--
-- -   If you request @mp3@ as the @OutputFormat@, the @ContentType@
--     returned is audio\/mpeg.
--
-- -   If you request @ogg_vorbis@ as the @OutputFormat@, the @ContentType@
--     returned is audio\/ogg.
--
-- -   If you request @pcm@ as the @OutputFormat@, the @ContentType@
--     returned is audio\/pcm in a signed 16-bit, 1 channel (mono),
--     little-endian format.
--
-- -   If you request @json@ as the @OutputFormat@, the @ContentType@
--     returned is application\/x-json-stream.
--
-- 'requestCharacters', 'synthesizeSpeechResponse_requestCharacters' - Number of characters synthesized.
--
-- 'httpStatus', 'synthesizeSpeechResponse_httpStatus' - The response's http status code.
--
-- 'audioStream', 'synthesizeSpeechResponse_audioStream' - Stream containing the synthesized speech.
newSynthesizeSpeechResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'audioStream'
  Data.ResponseBody ->
  SynthesizeSpeechResponse
newSynthesizeSpeechResponse
  pHttpStatus_
  pAudioStream_ =
    SynthesizeSpeechResponse'
      { contentType =
          Prelude.Nothing,
        requestCharacters = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        audioStream = pAudioStream_
      }

-- | Specifies the type audio stream. This should reflect the @OutputFormat@
-- parameter in your request.
--
-- -   If you request @mp3@ as the @OutputFormat@, the @ContentType@
--     returned is audio\/mpeg.
--
-- -   If you request @ogg_vorbis@ as the @OutputFormat@, the @ContentType@
--     returned is audio\/ogg.
--
-- -   If you request @pcm@ as the @OutputFormat@, the @ContentType@
--     returned is audio\/pcm in a signed 16-bit, 1 channel (mono),
--     little-endian format.
--
-- -   If you request @json@ as the @OutputFormat@, the @ContentType@
--     returned is application\/x-json-stream.
synthesizeSpeechResponse_contentType :: Lens.Lens' SynthesizeSpeechResponse (Prelude.Maybe Prelude.Text)
synthesizeSpeechResponse_contentType = Lens.lens (\SynthesizeSpeechResponse' {contentType} -> contentType) (\s@SynthesizeSpeechResponse' {} a -> s {contentType = a} :: SynthesizeSpeechResponse)

-- | Number of characters synthesized.
synthesizeSpeechResponse_requestCharacters :: Lens.Lens' SynthesizeSpeechResponse (Prelude.Maybe Prelude.Int)
synthesizeSpeechResponse_requestCharacters = Lens.lens (\SynthesizeSpeechResponse' {requestCharacters} -> requestCharacters) (\s@SynthesizeSpeechResponse' {} a -> s {requestCharacters = a} :: SynthesizeSpeechResponse)

-- | The response's http status code.
synthesizeSpeechResponse_httpStatus :: Lens.Lens' SynthesizeSpeechResponse Prelude.Int
synthesizeSpeechResponse_httpStatus = Lens.lens (\SynthesizeSpeechResponse' {httpStatus} -> httpStatus) (\s@SynthesizeSpeechResponse' {} a -> s {httpStatus = a} :: SynthesizeSpeechResponse)

-- | Stream containing the synthesized speech.
synthesizeSpeechResponse_audioStream :: Lens.Lens' SynthesizeSpeechResponse Data.ResponseBody
synthesizeSpeechResponse_audioStream = Lens.lens (\SynthesizeSpeechResponse' {audioStream} -> audioStream) (\s@SynthesizeSpeechResponse' {} a -> s {audioStream = a} :: SynthesizeSpeechResponse)
