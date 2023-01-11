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
-- Module      : Amazonka.Polly.StartSpeechSynthesisTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the creation of an asynchronous synthesis task, by starting a new
-- @SpeechSynthesisTask@. This operation requires all the standard
-- information needed for speech synthesis, plus the name of an Amazon S3
-- bucket for the service to store the output of the synthesis task and two
-- optional parameters (@OutputS3KeyPrefix@ and @SnsTopicArn@). Once the
-- synthesis task is created, this operation will return a
-- @SpeechSynthesisTask@ object, which will include an identifier of this
-- task as well as the current status. The @SpeechSynthesisTask@ object is
-- available for 72 hours after starting the asynchronous synthesis task.
module Amazonka.Polly.StartSpeechSynthesisTask
  ( -- * Creating a Request
    StartSpeechSynthesisTask (..),
    newStartSpeechSynthesisTask,

    -- * Request Lenses
    startSpeechSynthesisTask_engine,
    startSpeechSynthesisTask_languageCode,
    startSpeechSynthesisTask_lexiconNames,
    startSpeechSynthesisTask_outputS3KeyPrefix,
    startSpeechSynthesisTask_sampleRate,
    startSpeechSynthesisTask_snsTopicArn,
    startSpeechSynthesisTask_speechMarkTypes,
    startSpeechSynthesisTask_textType,
    startSpeechSynthesisTask_outputFormat,
    startSpeechSynthesisTask_outputS3BucketName,
    startSpeechSynthesisTask_text,
    startSpeechSynthesisTask_voiceId,

    -- * Destructuring the Response
    StartSpeechSynthesisTaskResponse (..),
    newStartSpeechSynthesisTaskResponse,

    -- * Response Lenses
    startSpeechSynthesisTaskResponse_synthesisTask,
    startSpeechSynthesisTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Polly.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartSpeechSynthesisTask' smart constructor.
data StartSpeechSynthesisTask = StartSpeechSynthesisTask'
  { -- | Specifies the engine (@standard@ or @neural@) for Amazon Polly to use
    -- when processing input text for speech synthesis. Using a voice that is
    -- not supported for the engine selected will result in an error.
    engine :: Prelude.Maybe Engine,
    -- | Optional language code for the Speech Synthesis request. This is only
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
    -- lexicon is the same as the language of the voice.
    lexiconNames :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon S3 key prefix for the output speech file.
    outputS3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The audio frequency specified in Hz.
    --
    -- The valid values for mp3 and ogg_vorbis are \"8000\", \"16000\",
    -- \"22050\", and \"24000\". The default value for standard voices is
    -- \"22050\". The default value for neural voices is \"24000\".
    --
    -- Valid values for pcm are \"8000\" and \"16000\" The default value is
    -- \"16000\".
    sampleRate :: Prelude.Maybe Prelude.Text,
    -- | ARN for the SNS topic optionally used for providing status notification
    -- for a speech synthesis task.
    snsTopicArn :: Prelude.Maybe Prelude.Text,
    -- | The type of speech marks returned for the input text.
    speechMarkTypes :: Prelude.Maybe [SpeechMarkType],
    -- | Specifies whether the input text is plain text or SSML. The default
    -- value is plain text.
    textType :: Prelude.Maybe TextType,
    -- | The format in which the returned output will be encoded. For audio
    -- stream, this will be mp3, ogg_vorbis, or pcm. For speech marks, this
    -- will be json.
    outputFormat :: OutputFormat,
    -- | Amazon S3 bucket name to which the output file will be saved.
    outputS3BucketName :: Prelude.Text,
    -- | The input text to synthesize. If you specify ssml as the TextType,
    -- follow the SSML format for the input text.
    text :: Prelude.Text,
    -- | Voice ID to use for the synthesis.
    voiceId :: VoiceId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSpeechSynthesisTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engine', 'startSpeechSynthesisTask_engine' - Specifies the engine (@standard@ or @neural@) for Amazon Polly to use
-- when processing input text for speech synthesis. Using a voice that is
-- not supported for the engine selected will result in an error.
--
-- 'languageCode', 'startSpeechSynthesisTask_languageCode' - Optional language code for the Speech Synthesis request. This is only
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
-- 'lexiconNames', 'startSpeechSynthesisTask_lexiconNames' - List of one or more pronunciation lexicon names you want the service to
-- apply during synthesis. Lexicons are applied only if the language of the
-- lexicon is the same as the language of the voice.
--
-- 'outputS3KeyPrefix', 'startSpeechSynthesisTask_outputS3KeyPrefix' - The Amazon S3 key prefix for the output speech file.
--
-- 'sampleRate', 'startSpeechSynthesisTask_sampleRate' - The audio frequency specified in Hz.
--
-- The valid values for mp3 and ogg_vorbis are \"8000\", \"16000\",
-- \"22050\", and \"24000\". The default value for standard voices is
-- \"22050\". The default value for neural voices is \"24000\".
--
-- Valid values for pcm are \"8000\" and \"16000\" The default value is
-- \"16000\".
--
-- 'snsTopicArn', 'startSpeechSynthesisTask_snsTopicArn' - ARN for the SNS topic optionally used for providing status notification
-- for a speech synthesis task.
--
-- 'speechMarkTypes', 'startSpeechSynthesisTask_speechMarkTypes' - The type of speech marks returned for the input text.
--
-- 'textType', 'startSpeechSynthesisTask_textType' - Specifies whether the input text is plain text or SSML. The default
-- value is plain text.
--
-- 'outputFormat', 'startSpeechSynthesisTask_outputFormat' - The format in which the returned output will be encoded. For audio
-- stream, this will be mp3, ogg_vorbis, or pcm. For speech marks, this
-- will be json.
--
-- 'outputS3BucketName', 'startSpeechSynthesisTask_outputS3BucketName' - Amazon S3 bucket name to which the output file will be saved.
--
-- 'text', 'startSpeechSynthesisTask_text' - The input text to synthesize. If you specify ssml as the TextType,
-- follow the SSML format for the input text.
--
-- 'voiceId', 'startSpeechSynthesisTask_voiceId' - Voice ID to use for the synthesis.
newStartSpeechSynthesisTask ::
  -- | 'outputFormat'
  OutputFormat ->
  -- | 'outputS3BucketName'
  Prelude.Text ->
  -- | 'text'
  Prelude.Text ->
  -- | 'voiceId'
  VoiceId ->
  StartSpeechSynthesisTask
newStartSpeechSynthesisTask
  pOutputFormat_
  pOutputS3BucketName_
  pText_
  pVoiceId_ =
    StartSpeechSynthesisTask'
      { engine = Prelude.Nothing,
        languageCode = Prelude.Nothing,
        lexiconNames = Prelude.Nothing,
        outputS3KeyPrefix = Prelude.Nothing,
        sampleRate = Prelude.Nothing,
        snsTopicArn = Prelude.Nothing,
        speechMarkTypes = Prelude.Nothing,
        textType = Prelude.Nothing,
        outputFormat = pOutputFormat_,
        outputS3BucketName = pOutputS3BucketName_,
        text = pText_,
        voiceId = pVoiceId_
      }

-- | Specifies the engine (@standard@ or @neural@) for Amazon Polly to use
-- when processing input text for speech synthesis. Using a voice that is
-- not supported for the engine selected will result in an error.
startSpeechSynthesisTask_engine :: Lens.Lens' StartSpeechSynthesisTask (Prelude.Maybe Engine)
startSpeechSynthesisTask_engine = Lens.lens (\StartSpeechSynthesisTask' {engine} -> engine) (\s@StartSpeechSynthesisTask' {} a -> s {engine = a} :: StartSpeechSynthesisTask)

-- | Optional language code for the Speech Synthesis request. This is only
-- necessary if using a bilingual voice, such as Aditi, which can be used
-- for either Indian English (en-IN) or Hindi (hi-IN).
--
-- If a bilingual voice is used and no language code is specified, Amazon
-- Polly uses the default language of the bilingual voice. The default
-- language for any voice is the one returned by the
-- <https://docs.aws.amazon.com/polly/latest/dg/API_DescribeVoices.html DescribeVoices>
-- operation for the @LanguageCode@ parameter. For example, if no language
-- code is specified, Aditi will use Indian English rather than Hindi.
startSpeechSynthesisTask_languageCode :: Lens.Lens' StartSpeechSynthesisTask (Prelude.Maybe LanguageCode)
startSpeechSynthesisTask_languageCode = Lens.lens (\StartSpeechSynthesisTask' {languageCode} -> languageCode) (\s@StartSpeechSynthesisTask' {} a -> s {languageCode = a} :: StartSpeechSynthesisTask)

-- | List of one or more pronunciation lexicon names you want the service to
-- apply during synthesis. Lexicons are applied only if the language of the
-- lexicon is the same as the language of the voice.
startSpeechSynthesisTask_lexiconNames :: Lens.Lens' StartSpeechSynthesisTask (Prelude.Maybe [Prelude.Text])
startSpeechSynthesisTask_lexiconNames = Lens.lens (\StartSpeechSynthesisTask' {lexiconNames} -> lexiconNames) (\s@StartSpeechSynthesisTask' {} a -> s {lexiconNames = a} :: StartSpeechSynthesisTask) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon S3 key prefix for the output speech file.
startSpeechSynthesisTask_outputS3KeyPrefix :: Lens.Lens' StartSpeechSynthesisTask (Prelude.Maybe Prelude.Text)
startSpeechSynthesisTask_outputS3KeyPrefix = Lens.lens (\StartSpeechSynthesisTask' {outputS3KeyPrefix} -> outputS3KeyPrefix) (\s@StartSpeechSynthesisTask' {} a -> s {outputS3KeyPrefix = a} :: StartSpeechSynthesisTask)

-- | The audio frequency specified in Hz.
--
-- The valid values for mp3 and ogg_vorbis are \"8000\", \"16000\",
-- \"22050\", and \"24000\". The default value for standard voices is
-- \"22050\". The default value for neural voices is \"24000\".
--
-- Valid values for pcm are \"8000\" and \"16000\" The default value is
-- \"16000\".
startSpeechSynthesisTask_sampleRate :: Lens.Lens' StartSpeechSynthesisTask (Prelude.Maybe Prelude.Text)
startSpeechSynthesisTask_sampleRate = Lens.lens (\StartSpeechSynthesisTask' {sampleRate} -> sampleRate) (\s@StartSpeechSynthesisTask' {} a -> s {sampleRate = a} :: StartSpeechSynthesisTask)

-- | ARN for the SNS topic optionally used for providing status notification
-- for a speech synthesis task.
startSpeechSynthesisTask_snsTopicArn :: Lens.Lens' StartSpeechSynthesisTask (Prelude.Maybe Prelude.Text)
startSpeechSynthesisTask_snsTopicArn = Lens.lens (\StartSpeechSynthesisTask' {snsTopicArn} -> snsTopicArn) (\s@StartSpeechSynthesisTask' {} a -> s {snsTopicArn = a} :: StartSpeechSynthesisTask)

-- | The type of speech marks returned for the input text.
startSpeechSynthesisTask_speechMarkTypes :: Lens.Lens' StartSpeechSynthesisTask (Prelude.Maybe [SpeechMarkType])
startSpeechSynthesisTask_speechMarkTypes = Lens.lens (\StartSpeechSynthesisTask' {speechMarkTypes} -> speechMarkTypes) (\s@StartSpeechSynthesisTask' {} a -> s {speechMarkTypes = a} :: StartSpeechSynthesisTask) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether the input text is plain text or SSML. The default
-- value is plain text.
startSpeechSynthesisTask_textType :: Lens.Lens' StartSpeechSynthesisTask (Prelude.Maybe TextType)
startSpeechSynthesisTask_textType = Lens.lens (\StartSpeechSynthesisTask' {textType} -> textType) (\s@StartSpeechSynthesisTask' {} a -> s {textType = a} :: StartSpeechSynthesisTask)

-- | The format in which the returned output will be encoded. For audio
-- stream, this will be mp3, ogg_vorbis, or pcm. For speech marks, this
-- will be json.
startSpeechSynthesisTask_outputFormat :: Lens.Lens' StartSpeechSynthesisTask OutputFormat
startSpeechSynthesisTask_outputFormat = Lens.lens (\StartSpeechSynthesisTask' {outputFormat} -> outputFormat) (\s@StartSpeechSynthesisTask' {} a -> s {outputFormat = a} :: StartSpeechSynthesisTask)

-- | Amazon S3 bucket name to which the output file will be saved.
startSpeechSynthesisTask_outputS3BucketName :: Lens.Lens' StartSpeechSynthesisTask Prelude.Text
startSpeechSynthesisTask_outputS3BucketName = Lens.lens (\StartSpeechSynthesisTask' {outputS3BucketName} -> outputS3BucketName) (\s@StartSpeechSynthesisTask' {} a -> s {outputS3BucketName = a} :: StartSpeechSynthesisTask)

-- | The input text to synthesize. If you specify ssml as the TextType,
-- follow the SSML format for the input text.
startSpeechSynthesisTask_text :: Lens.Lens' StartSpeechSynthesisTask Prelude.Text
startSpeechSynthesisTask_text = Lens.lens (\StartSpeechSynthesisTask' {text} -> text) (\s@StartSpeechSynthesisTask' {} a -> s {text = a} :: StartSpeechSynthesisTask)

-- | Voice ID to use for the synthesis.
startSpeechSynthesisTask_voiceId :: Lens.Lens' StartSpeechSynthesisTask VoiceId
startSpeechSynthesisTask_voiceId = Lens.lens (\StartSpeechSynthesisTask' {voiceId} -> voiceId) (\s@StartSpeechSynthesisTask' {} a -> s {voiceId = a} :: StartSpeechSynthesisTask)

instance Core.AWSRequest StartSpeechSynthesisTask where
  type
    AWSResponse StartSpeechSynthesisTask =
      StartSpeechSynthesisTaskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartSpeechSynthesisTaskResponse'
            Prelude.<$> (x Data..?> "SynthesisTask")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartSpeechSynthesisTask where
  hashWithSalt _salt StartSpeechSynthesisTask' {..} =
    _salt `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` lexiconNames
      `Prelude.hashWithSalt` outputS3KeyPrefix
      `Prelude.hashWithSalt` sampleRate
      `Prelude.hashWithSalt` snsTopicArn
      `Prelude.hashWithSalt` speechMarkTypes
      `Prelude.hashWithSalt` textType
      `Prelude.hashWithSalt` outputFormat
      `Prelude.hashWithSalt` outputS3BucketName
      `Prelude.hashWithSalt` text
      `Prelude.hashWithSalt` voiceId

instance Prelude.NFData StartSpeechSynthesisTask where
  rnf StartSpeechSynthesisTask' {..} =
    Prelude.rnf engine
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf lexiconNames
      `Prelude.seq` Prelude.rnf outputS3KeyPrefix
      `Prelude.seq` Prelude.rnf sampleRate
      `Prelude.seq` Prelude.rnf snsTopicArn
      `Prelude.seq` Prelude.rnf speechMarkTypes
      `Prelude.seq` Prelude.rnf textType
      `Prelude.seq` Prelude.rnf outputFormat
      `Prelude.seq` Prelude.rnf outputS3BucketName
      `Prelude.seq` Prelude.rnf text
      `Prelude.seq` Prelude.rnf voiceId

instance Data.ToHeaders StartSpeechSynthesisTask where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON StartSpeechSynthesisTask where
  toJSON StartSpeechSynthesisTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Engine" Data..=) Prelude.<$> engine,
            ("LanguageCode" Data..=) Prelude.<$> languageCode,
            ("LexiconNames" Data..=) Prelude.<$> lexiconNames,
            ("OutputS3KeyPrefix" Data..=)
              Prelude.<$> outputS3KeyPrefix,
            ("SampleRate" Data..=) Prelude.<$> sampleRate,
            ("SnsTopicArn" Data..=) Prelude.<$> snsTopicArn,
            ("SpeechMarkTypes" Data..=)
              Prelude.<$> speechMarkTypes,
            ("TextType" Data..=) Prelude.<$> textType,
            Prelude.Just ("OutputFormat" Data..= outputFormat),
            Prelude.Just
              ("OutputS3BucketName" Data..= outputS3BucketName),
            Prelude.Just ("Text" Data..= text),
            Prelude.Just ("VoiceId" Data..= voiceId)
          ]
      )

instance Data.ToPath StartSpeechSynthesisTask where
  toPath = Prelude.const "/v1/synthesisTasks"

instance Data.ToQuery StartSpeechSynthesisTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartSpeechSynthesisTaskResponse' smart constructor.
data StartSpeechSynthesisTaskResponse = StartSpeechSynthesisTaskResponse'
  { -- | SynthesisTask object that provides information and attributes about a
    -- newly submitted speech synthesis task.
    synthesisTask :: Prelude.Maybe SynthesisTask,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSpeechSynthesisTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'synthesisTask', 'startSpeechSynthesisTaskResponse_synthesisTask' - SynthesisTask object that provides information and attributes about a
-- newly submitted speech synthesis task.
--
-- 'httpStatus', 'startSpeechSynthesisTaskResponse_httpStatus' - The response's http status code.
newStartSpeechSynthesisTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartSpeechSynthesisTaskResponse
newStartSpeechSynthesisTaskResponse pHttpStatus_ =
  StartSpeechSynthesisTaskResponse'
    { synthesisTask =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | SynthesisTask object that provides information and attributes about a
-- newly submitted speech synthesis task.
startSpeechSynthesisTaskResponse_synthesisTask :: Lens.Lens' StartSpeechSynthesisTaskResponse (Prelude.Maybe SynthesisTask)
startSpeechSynthesisTaskResponse_synthesisTask = Lens.lens (\StartSpeechSynthesisTaskResponse' {synthesisTask} -> synthesisTask) (\s@StartSpeechSynthesisTaskResponse' {} a -> s {synthesisTask = a} :: StartSpeechSynthesisTaskResponse)

-- | The response's http status code.
startSpeechSynthesisTaskResponse_httpStatus :: Lens.Lens' StartSpeechSynthesisTaskResponse Prelude.Int
startSpeechSynthesisTaskResponse_httpStatus = Lens.lens (\StartSpeechSynthesisTaskResponse' {httpStatus} -> httpStatus) (\s@StartSpeechSynthesisTaskResponse' {} a -> s {httpStatus = a} :: StartSpeechSynthesisTaskResponse)

instance
  Prelude.NFData
    StartSpeechSynthesisTaskResponse
  where
  rnf StartSpeechSynthesisTaskResponse' {..} =
    Prelude.rnf synthesisTask
      `Prelude.seq` Prelude.rnf httpStatus
