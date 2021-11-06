{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Polly.Types.SynthesisTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Polly.Types.SynthesisTask where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Polly.Types.Engine
import Amazonka.Polly.Types.LanguageCode
import Amazonka.Polly.Types.OutputFormat
import Amazonka.Polly.Types.SpeechMarkType
import Amazonka.Polly.Types.TaskStatus
import Amazonka.Polly.Types.TextType
import Amazonka.Polly.Types.VoiceId
import qualified Amazonka.Prelude as Prelude

-- | SynthesisTask object that provides information about a speech synthesis
-- task.
--
-- /See:/ 'newSynthesisTask' smart constructor.
data SynthesisTask = SynthesisTask'
  { -- | Timestamp for the time the synthesis task was started.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | Optional language code for a synthesis task. This is only necessary if
    -- using a bilingual voice, such as Aditi, which can be used for either
    -- Indian English (en-IN) or Hindi (hi-IN).
    --
    -- If a bilingual voice is used and no language code is specified, Amazon
    -- Polly uses the default language of the bilingual voice. The default
    -- language for any voice is the one returned by the
    -- <https://docs.aws.amazon.com/polly/latest/dg/API_DescribeVoices.html DescribeVoices>
    -- operation for the @LanguageCode@ parameter. For example, if no language
    -- code is specified, Aditi will use Indian English rather than Hindi.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | ARN for the SNS topic optionally used for providing status notification
    -- for a speech synthesis task.
    snsTopicArn :: Prelude.Maybe Prelude.Text,
    -- | Reason for the current status of a specific speech synthesis task,
    -- including errors if the task has failed.
    taskStatusReason :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Polly generated identifier for a speech synthesis task.
    taskId :: Prelude.Maybe Prelude.Text,
    -- | Number of billable characters synthesized.
    requestCharacters :: Prelude.Maybe Prelude.Int,
    -- | Specifies the engine (@standard@ or @neural@) for Amazon Polly to use
    -- when processing input text for speech synthesis. Using a voice that is
    -- not supported for the engine selected will result in an error.
    engine :: Prelude.Maybe Engine,
    -- | The type of speech marks returned for the input text.
    speechMarkTypes :: Prelude.Maybe [SpeechMarkType],
    -- | The audio frequency specified in Hz.
    --
    -- The valid values for mp3 and ogg_vorbis are \"8000\", \"16000\",
    -- \"22050\", and \"24000\". The default value for standard voices is
    -- \"22050\". The default value for neural voices is \"24000\".
    --
    -- Valid values for pcm are \"8000\" and \"16000\" The default value is
    -- \"16000\".
    sampleRate :: Prelude.Maybe Prelude.Text,
    -- | The format in which the returned output will be encoded. For audio
    -- stream, this will be mp3, ogg_vorbis, or pcm. For speech marks, this
    -- will be json.
    outputFormat :: Prelude.Maybe OutputFormat,
    -- | Specifies whether the input text is plain text or SSML. The default
    -- value is plain text.
    textType :: Prelude.Maybe TextType,
    -- | Voice ID to use for the synthesis.
    voiceId :: Prelude.Maybe VoiceId,
    -- | List of one or more pronunciation lexicon names you want the service to
    -- apply during synthesis. Lexicons are applied only if the language of the
    -- lexicon is the same as the language of the voice.
    lexiconNames :: Prelude.Maybe [Prelude.Text],
    -- | Current status of the individual speech synthesis task.
    taskStatus :: Prelude.Maybe TaskStatus,
    -- | Pathway for the output speech file.
    outputUri :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SynthesisTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'synthesisTask_creationTime' - Timestamp for the time the synthesis task was started.
--
-- 'languageCode', 'synthesisTask_languageCode' - Optional language code for a synthesis task. This is only necessary if
-- using a bilingual voice, such as Aditi, which can be used for either
-- Indian English (en-IN) or Hindi (hi-IN).
--
-- If a bilingual voice is used and no language code is specified, Amazon
-- Polly uses the default language of the bilingual voice. The default
-- language for any voice is the one returned by the
-- <https://docs.aws.amazon.com/polly/latest/dg/API_DescribeVoices.html DescribeVoices>
-- operation for the @LanguageCode@ parameter. For example, if no language
-- code is specified, Aditi will use Indian English rather than Hindi.
--
-- 'snsTopicArn', 'synthesisTask_snsTopicArn' - ARN for the SNS topic optionally used for providing status notification
-- for a speech synthesis task.
--
-- 'taskStatusReason', 'synthesisTask_taskStatusReason' - Reason for the current status of a specific speech synthesis task,
-- including errors if the task has failed.
--
-- 'taskId', 'synthesisTask_taskId' - The Amazon Polly generated identifier for a speech synthesis task.
--
-- 'requestCharacters', 'synthesisTask_requestCharacters' - Number of billable characters synthesized.
--
-- 'engine', 'synthesisTask_engine' - Specifies the engine (@standard@ or @neural@) for Amazon Polly to use
-- when processing input text for speech synthesis. Using a voice that is
-- not supported for the engine selected will result in an error.
--
-- 'speechMarkTypes', 'synthesisTask_speechMarkTypes' - The type of speech marks returned for the input text.
--
-- 'sampleRate', 'synthesisTask_sampleRate' - The audio frequency specified in Hz.
--
-- The valid values for mp3 and ogg_vorbis are \"8000\", \"16000\",
-- \"22050\", and \"24000\". The default value for standard voices is
-- \"22050\". The default value for neural voices is \"24000\".
--
-- Valid values for pcm are \"8000\" and \"16000\" The default value is
-- \"16000\".
--
-- 'outputFormat', 'synthesisTask_outputFormat' - The format in which the returned output will be encoded. For audio
-- stream, this will be mp3, ogg_vorbis, or pcm. For speech marks, this
-- will be json.
--
-- 'textType', 'synthesisTask_textType' - Specifies whether the input text is plain text or SSML. The default
-- value is plain text.
--
-- 'voiceId', 'synthesisTask_voiceId' - Voice ID to use for the synthesis.
--
-- 'lexiconNames', 'synthesisTask_lexiconNames' - List of one or more pronunciation lexicon names you want the service to
-- apply during synthesis. Lexicons are applied only if the language of the
-- lexicon is the same as the language of the voice.
--
-- 'taskStatus', 'synthesisTask_taskStatus' - Current status of the individual speech synthesis task.
--
-- 'outputUri', 'synthesisTask_outputUri' - Pathway for the output speech file.
newSynthesisTask ::
  SynthesisTask
newSynthesisTask =
  SynthesisTask'
    { creationTime = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      snsTopicArn = Prelude.Nothing,
      taskStatusReason = Prelude.Nothing,
      taskId = Prelude.Nothing,
      requestCharacters = Prelude.Nothing,
      engine = Prelude.Nothing,
      speechMarkTypes = Prelude.Nothing,
      sampleRate = Prelude.Nothing,
      outputFormat = Prelude.Nothing,
      textType = Prelude.Nothing,
      voiceId = Prelude.Nothing,
      lexiconNames = Prelude.Nothing,
      taskStatus = Prelude.Nothing,
      outputUri = Prelude.Nothing
    }

-- | Timestamp for the time the synthesis task was started.
synthesisTask_creationTime :: Lens.Lens' SynthesisTask (Prelude.Maybe Prelude.UTCTime)
synthesisTask_creationTime = Lens.lens (\SynthesisTask' {creationTime} -> creationTime) (\s@SynthesisTask' {} a -> s {creationTime = a} :: SynthesisTask) Prelude.. Lens.mapping Core._Time

-- | Optional language code for a synthesis task. This is only necessary if
-- using a bilingual voice, such as Aditi, which can be used for either
-- Indian English (en-IN) or Hindi (hi-IN).
--
-- If a bilingual voice is used and no language code is specified, Amazon
-- Polly uses the default language of the bilingual voice. The default
-- language for any voice is the one returned by the
-- <https://docs.aws.amazon.com/polly/latest/dg/API_DescribeVoices.html DescribeVoices>
-- operation for the @LanguageCode@ parameter. For example, if no language
-- code is specified, Aditi will use Indian English rather than Hindi.
synthesisTask_languageCode :: Lens.Lens' SynthesisTask (Prelude.Maybe LanguageCode)
synthesisTask_languageCode = Lens.lens (\SynthesisTask' {languageCode} -> languageCode) (\s@SynthesisTask' {} a -> s {languageCode = a} :: SynthesisTask)

-- | ARN for the SNS topic optionally used for providing status notification
-- for a speech synthesis task.
synthesisTask_snsTopicArn :: Lens.Lens' SynthesisTask (Prelude.Maybe Prelude.Text)
synthesisTask_snsTopicArn = Lens.lens (\SynthesisTask' {snsTopicArn} -> snsTopicArn) (\s@SynthesisTask' {} a -> s {snsTopicArn = a} :: SynthesisTask)

-- | Reason for the current status of a specific speech synthesis task,
-- including errors if the task has failed.
synthesisTask_taskStatusReason :: Lens.Lens' SynthesisTask (Prelude.Maybe Prelude.Text)
synthesisTask_taskStatusReason = Lens.lens (\SynthesisTask' {taskStatusReason} -> taskStatusReason) (\s@SynthesisTask' {} a -> s {taskStatusReason = a} :: SynthesisTask)

-- | The Amazon Polly generated identifier for a speech synthesis task.
synthesisTask_taskId :: Lens.Lens' SynthesisTask (Prelude.Maybe Prelude.Text)
synthesisTask_taskId = Lens.lens (\SynthesisTask' {taskId} -> taskId) (\s@SynthesisTask' {} a -> s {taskId = a} :: SynthesisTask)

-- | Number of billable characters synthesized.
synthesisTask_requestCharacters :: Lens.Lens' SynthesisTask (Prelude.Maybe Prelude.Int)
synthesisTask_requestCharacters = Lens.lens (\SynthesisTask' {requestCharacters} -> requestCharacters) (\s@SynthesisTask' {} a -> s {requestCharacters = a} :: SynthesisTask)

-- | Specifies the engine (@standard@ or @neural@) for Amazon Polly to use
-- when processing input text for speech synthesis. Using a voice that is
-- not supported for the engine selected will result in an error.
synthesisTask_engine :: Lens.Lens' SynthesisTask (Prelude.Maybe Engine)
synthesisTask_engine = Lens.lens (\SynthesisTask' {engine} -> engine) (\s@SynthesisTask' {} a -> s {engine = a} :: SynthesisTask)

-- | The type of speech marks returned for the input text.
synthesisTask_speechMarkTypes :: Lens.Lens' SynthesisTask (Prelude.Maybe [SpeechMarkType])
synthesisTask_speechMarkTypes = Lens.lens (\SynthesisTask' {speechMarkTypes} -> speechMarkTypes) (\s@SynthesisTask' {} a -> s {speechMarkTypes = a} :: SynthesisTask) Prelude.. Lens.mapping Lens.coerced

-- | The audio frequency specified in Hz.
--
-- The valid values for mp3 and ogg_vorbis are \"8000\", \"16000\",
-- \"22050\", and \"24000\". The default value for standard voices is
-- \"22050\". The default value for neural voices is \"24000\".
--
-- Valid values for pcm are \"8000\" and \"16000\" The default value is
-- \"16000\".
synthesisTask_sampleRate :: Lens.Lens' SynthesisTask (Prelude.Maybe Prelude.Text)
synthesisTask_sampleRate = Lens.lens (\SynthesisTask' {sampleRate} -> sampleRate) (\s@SynthesisTask' {} a -> s {sampleRate = a} :: SynthesisTask)

-- | The format in which the returned output will be encoded. For audio
-- stream, this will be mp3, ogg_vorbis, or pcm. For speech marks, this
-- will be json.
synthesisTask_outputFormat :: Lens.Lens' SynthesisTask (Prelude.Maybe OutputFormat)
synthesisTask_outputFormat = Lens.lens (\SynthesisTask' {outputFormat} -> outputFormat) (\s@SynthesisTask' {} a -> s {outputFormat = a} :: SynthesisTask)

-- | Specifies whether the input text is plain text or SSML. The default
-- value is plain text.
synthesisTask_textType :: Lens.Lens' SynthesisTask (Prelude.Maybe TextType)
synthesisTask_textType = Lens.lens (\SynthesisTask' {textType} -> textType) (\s@SynthesisTask' {} a -> s {textType = a} :: SynthesisTask)

-- | Voice ID to use for the synthesis.
synthesisTask_voiceId :: Lens.Lens' SynthesisTask (Prelude.Maybe VoiceId)
synthesisTask_voiceId = Lens.lens (\SynthesisTask' {voiceId} -> voiceId) (\s@SynthesisTask' {} a -> s {voiceId = a} :: SynthesisTask)

-- | List of one or more pronunciation lexicon names you want the service to
-- apply during synthesis. Lexicons are applied only if the language of the
-- lexicon is the same as the language of the voice.
synthesisTask_lexiconNames :: Lens.Lens' SynthesisTask (Prelude.Maybe [Prelude.Text])
synthesisTask_lexiconNames = Lens.lens (\SynthesisTask' {lexiconNames} -> lexiconNames) (\s@SynthesisTask' {} a -> s {lexiconNames = a} :: SynthesisTask) Prelude.. Lens.mapping Lens.coerced

-- | Current status of the individual speech synthesis task.
synthesisTask_taskStatus :: Lens.Lens' SynthesisTask (Prelude.Maybe TaskStatus)
synthesisTask_taskStatus = Lens.lens (\SynthesisTask' {taskStatus} -> taskStatus) (\s@SynthesisTask' {} a -> s {taskStatus = a} :: SynthesisTask)

-- | Pathway for the output speech file.
synthesisTask_outputUri :: Lens.Lens' SynthesisTask (Prelude.Maybe Prelude.Text)
synthesisTask_outputUri = Lens.lens (\SynthesisTask' {outputUri} -> outputUri) (\s@SynthesisTask' {} a -> s {outputUri = a} :: SynthesisTask)

instance Core.FromJSON SynthesisTask where
  parseJSON =
    Core.withObject
      "SynthesisTask"
      ( \x ->
          SynthesisTask'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "LanguageCode")
            Prelude.<*> (x Core..:? "SnsTopicArn")
            Prelude.<*> (x Core..:? "TaskStatusReason")
            Prelude.<*> (x Core..:? "TaskId")
            Prelude.<*> (x Core..:? "RequestCharacters")
            Prelude.<*> (x Core..:? "Engine")
            Prelude.<*> ( x Core..:? "SpeechMarkTypes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "SampleRate")
            Prelude.<*> (x Core..:? "OutputFormat")
            Prelude.<*> (x Core..:? "TextType")
            Prelude.<*> (x Core..:? "VoiceId")
            Prelude.<*> (x Core..:? "LexiconNames" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "TaskStatus")
            Prelude.<*> (x Core..:? "OutputUri")
      )

instance Prelude.Hashable SynthesisTask

instance Prelude.NFData SynthesisTask
