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
-- Module      : Network.AWS.Polly.Types.SynthesisTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Polly.Types.SynthesisTask where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Polly.Types.Engine
import Network.AWS.Polly.Types.LanguageCode
import Network.AWS.Polly.Types.OutputFormat
import Network.AWS.Polly.Types.SpeechMarkType
import Network.AWS.Polly.Types.TaskStatus
import Network.AWS.Polly.Types.TextType
import Network.AWS.Polly.Types.VoiceId

-- | SynthesisTask object that provides information about a speech synthesis
-- task.
--
-- /See:/ 'newSynthesisTask' smart constructor.
data SynthesisTask = SynthesisTask'
  { -- | Optional language code for a synthesis task. This is only necessary if
    -- using a bilingual voice, such as Aditi, which can be used for either
    -- Indian English (en-IN) or Hindi (hi-IN).
    --
    -- If a bilingual voice is used and no language code is specified, Amazon
    -- Polly will use the default language of the bilingual voice. The default
    -- language for any voice is the one returned by the
    -- <https://docs.aws.amazon.com/polly/latest/dg/API_DescribeVoices.html DescribeVoices>
    -- operation for the @LanguageCode@ parameter. For example, if no language
    -- code is specified, Aditi will use Indian English rather than Hindi.
    languageCode :: Core.Maybe LanguageCode,
    -- | Timestamp for the time the synthesis task was started.
    creationTime :: Core.Maybe Core.POSIX,
    -- | Pathway for the output speech file.
    outputUri :: Core.Maybe Core.Text,
    -- | The type of speech marks returned for the input text.
    speechMarkTypes :: Core.Maybe [SpeechMarkType],
    -- | List of one or more pronunciation lexicon names you want the service to
    -- apply during synthesis. Lexicons are applied only if the language of the
    -- lexicon is the same as the language of the voice.
    lexiconNames :: Core.Maybe [Core.Text],
    -- | Voice ID to use for the synthesis.
    voiceId :: Core.Maybe VoiceId,
    -- | The Amazon Polly generated identifier for a speech synthesis task.
    taskId :: Core.Maybe Core.Text,
    -- | Specifies whether the input text is plain text or SSML. The default
    -- value is plain text.
    textType :: Core.Maybe TextType,
    -- | The format in which the returned output will be encoded. For audio
    -- stream, this will be mp3, ogg_vorbis, or pcm. For speech marks, this
    -- will be json.
    outputFormat :: Core.Maybe OutputFormat,
    -- | The audio frequency specified in Hz.
    --
    -- The valid values for mp3 and ogg_vorbis are \"8000\", \"16000\",
    -- \"22050\", and \"24000\". The default value for standard voices is
    -- \"22050\". The default value for neural voices is \"24000\".
    --
    -- Valid values for pcm are \"8000\" and \"16000\" The default value is
    -- \"16000\".
    sampleRate :: Core.Maybe Core.Text,
    -- | Current status of the individual speech synthesis task.
    taskStatus :: Core.Maybe TaskStatus,
    -- | Specifies the engine (@standard@ or @neural@) for Amazon Polly to use
    -- when processing input text for speech synthesis. Using a voice that is
    -- not supported for the engine selected will result in an error.
    engine :: Core.Maybe Engine,
    -- | Number of billable characters synthesized.
    requestCharacters :: Core.Maybe Core.Int,
    -- | Reason for the current status of a specific speech synthesis task,
    -- including errors if the task has failed.
    taskStatusReason :: Core.Maybe Core.Text,
    -- | ARN for the SNS topic optionally used for providing status notification
    -- for a speech synthesis task.
    snsTopicArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SynthesisTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'synthesisTask_languageCode' - Optional language code for a synthesis task. This is only necessary if
-- using a bilingual voice, such as Aditi, which can be used for either
-- Indian English (en-IN) or Hindi (hi-IN).
--
-- If a bilingual voice is used and no language code is specified, Amazon
-- Polly will use the default language of the bilingual voice. The default
-- language for any voice is the one returned by the
-- <https://docs.aws.amazon.com/polly/latest/dg/API_DescribeVoices.html DescribeVoices>
-- operation for the @LanguageCode@ parameter. For example, if no language
-- code is specified, Aditi will use Indian English rather than Hindi.
--
-- 'creationTime', 'synthesisTask_creationTime' - Timestamp for the time the synthesis task was started.
--
-- 'outputUri', 'synthesisTask_outputUri' - Pathway for the output speech file.
--
-- 'speechMarkTypes', 'synthesisTask_speechMarkTypes' - The type of speech marks returned for the input text.
--
-- 'lexiconNames', 'synthesisTask_lexiconNames' - List of one or more pronunciation lexicon names you want the service to
-- apply during synthesis. Lexicons are applied only if the language of the
-- lexicon is the same as the language of the voice.
--
-- 'voiceId', 'synthesisTask_voiceId' - Voice ID to use for the synthesis.
--
-- 'taskId', 'synthesisTask_taskId' - The Amazon Polly generated identifier for a speech synthesis task.
--
-- 'textType', 'synthesisTask_textType' - Specifies whether the input text is plain text or SSML. The default
-- value is plain text.
--
-- 'outputFormat', 'synthesisTask_outputFormat' - The format in which the returned output will be encoded. For audio
-- stream, this will be mp3, ogg_vorbis, or pcm. For speech marks, this
-- will be json.
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
-- 'taskStatus', 'synthesisTask_taskStatus' - Current status of the individual speech synthesis task.
--
-- 'engine', 'synthesisTask_engine' - Specifies the engine (@standard@ or @neural@) for Amazon Polly to use
-- when processing input text for speech synthesis. Using a voice that is
-- not supported for the engine selected will result in an error.
--
-- 'requestCharacters', 'synthesisTask_requestCharacters' - Number of billable characters synthesized.
--
-- 'taskStatusReason', 'synthesisTask_taskStatusReason' - Reason for the current status of a specific speech synthesis task,
-- including errors if the task has failed.
--
-- 'snsTopicArn', 'synthesisTask_snsTopicArn' - ARN for the SNS topic optionally used for providing status notification
-- for a speech synthesis task.
newSynthesisTask ::
  SynthesisTask
newSynthesisTask =
  SynthesisTask'
    { languageCode = Core.Nothing,
      creationTime = Core.Nothing,
      outputUri = Core.Nothing,
      speechMarkTypes = Core.Nothing,
      lexiconNames = Core.Nothing,
      voiceId = Core.Nothing,
      taskId = Core.Nothing,
      textType = Core.Nothing,
      outputFormat = Core.Nothing,
      sampleRate = Core.Nothing,
      taskStatus = Core.Nothing,
      engine = Core.Nothing,
      requestCharacters = Core.Nothing,
      taskStatusReason = Core.Nothing,
      snsTopicArn = Core.Nothing
    }

-- | Optional language code for a synthesis task. This is only necessary if
-- using a bilingual voice, such as Aditi, which can be used for either
-- Indian English (en-IN) or Hindi (hi-IN).
--
-- If a bilingual voice is used and no language code is specified, Amazon
-- Polly will use the default language of the bilingual voice. The default
-- language for any voice is the one returned by the
-- <https://docs.aws.amazon.com/polly/latest/dg/API_DescribeVoices.html DescribeVoices>
-- operation for the @LanguageCode@ parameter. For example, if no language
-- code is specified, Aditi will use Indian English rather than Hindi.
synthesisTask_languageCode :: Lens.Lens' SynthesisTask (Core.Maybe LanguageCode)
synthesisTask_languageCode = Lens.lens (\SynthesisTask' {languageCode} -> languageCode) (\s@SynthesisTask' {} a -> s {languageCode = a} :: SynthesisTask)

-- | Timestamp for the time the synthesis task was started.
synthesisTask_creationTime :: Lens.Lens' SynthesisTask (Core.Maybe Core.UTCTime)
synthesisTask_creationTime = Lens.lens (\SynthesisTask' {creationTime} -> creationTime) (\s@SynthesisTask' {} a -> s {creationTime = a} :: SynthesisTask) Core.. Lens.mapping Core._Time

-- | Pathway for the output speech file.
synthesisTask_outputUri :: Lens.Lens' SynthesisTask (Core.Maybe Core.Text)
synthesisTask_outputUri = Lens.lens (\SynthesisTask' {outputUri} -> outputUri) (\s@SynthesisTask' {} a -> s {outputUri = a} :: SynthesisTask)

-- | The type of speech marks returned for the input text.
synthesisTask_speechMarkTypes :: Lens.Lens' SynthesisTask (Core.Maybe [SpeechMarkType])
synthesisTask_speechMarkTypes = Lens.lens (\SynthesisTask' {speechMarkTypes} -> speechMarkTypes) (\s@SynthesisTask' {} a -> s {speechMarkTypes = a} :: SynthesisTask) Core.. Lens.mapping Lens._Coerce

-- | List of one or more pronunciation lexicon names you want the service to
-- apply during synthesis. Lexicons are applied only if the language of the
-- lexicon is the same as the language of the voice.
synthesisTask_lexiconNames :: Lens.Lens' SynthesisTask (Core.Maybe [Core.Text])
synthesisTask_lexiconNames = Lens.lens (\SynthesisTask' {lexiconNames} -> lexiconNames) (\s@SynthesisTask' {} a -> s {lexiconNames = a} :: SynthesisTask) Core.. Lens.mapping Lens._Coerce

-- | Voice ID to use for the synthesis.
synthesisTask_voiceId :: Lens.Lens' SynthesisTask (Core.Maybe VoiceId)
synthesisTask_voiceId = Lens.lens (\SynthesisTask' {voiceId} -> voiceId) (\s@SynthesisTask' {} a -> s {voiceId = a} :: SynthesisTask)

-- | The Amazon Polly generated identifier for a speech synthesis task.
synthesisTask_taskId :: Lens.Lens' SynthesisTask (Core.Maybe Core.Text)
synthesisTask_taskId = Lens.lens (\SynthesisTask' {taskId} -> taskId) (\s@SynthesisTask' {} a -> s {taskId = a} :: SynthesisTask)

-- | Specifies whether the input text is plain text or SSML. The default
-- value is plain text.
synthesisTask_textType :: Lens.Lens' SynthesisTask (Core.Maybe TextType)
synthesisTask_textType = Lens.lens (\SynthesisTask' {textType} -> textType) (\s@SynthesisTask' {} a -> s {textType = a} :: SynthesisTask)

-- | The format in which the returned output will be encoded. For audio
-- stream, this will be mp3, ogg_vorbis, or pcm. For speech marks, this
-- will be json.
synthesisTask_outputFormat :: Lens.Lens' SynthesisTask (Core.Maybe OutputFormat)
synthesisTask_outputFormat = Lens.lens (\SynthesisTask' {outputFormat} -> outputFormat) (\s@SynthesisTask' {} a -> s {outputFormat = a} :: SynthesisTask)

-- | The audio frequency specified in Hz.
--
-- The valid values for mp3 and ogg_vorbis are \"8000\", \"16000\",
-- \"22050\", and \"24000\". The default value for standard voices is
-- \"22050\". The default value for neural voices is \"24000\".
--
-- Valid values for pcm are \"8000\" and \"16000\" The default value is
-- \"16000\".
synthesisTask_sampleRate :: Lens.Lens' SynthesisTask (Core.Maybe Core.Text)
synthesisTask_sampleRate = Lens.lens (\SynthesisTask' {sampleRate} -> sampleRate) (\s@SynthesisTask' {} a -> s {sampleRate = a} :: SynthesisTask)

-- | Current status of the individual speech synthesis task.
synthesisTask_taskStatus :: Lens.Lens' SynthesisTask (Core.Maybe TaskStatus)
synthesisTask_taskStatus = Lens.lens (\SynthesisTask' {taskStatus} -> taskStatus) (\s@SynthesisTask' {} a -> s {taskStatus = a} :: SynthesisTask)

-- | Specifies the engine (@standard@ or @neural@) for Amazon Polly to use
-- when processing input text for speech synthesis. Using a voice that is
-- not supported for the engine selected will result in an error.
synthesisTask_engine :: Lens.Lens' SynthesisTask (Core.Maybe Engine)
synthesisTask_engine = Lens.lens (\SynthesisTask' {engine} -> engine) (\s@SynthesisTask' {} a -> s {engine = a} :: SynthesisTask)

-- | Number of billable characters synthesized.
synthesisTask_requestCharacters :: Lens.Lens' SynthesisTask (Core.Maybe Core.Int)
synthesisTask_requestCharacters = Lens.lens (\SynthesisTask' {requestCharacters} -> requestCharacters) (\s@SynthesisTask' {} a -> s {requestCharacters = a} :: SynthesisTask)

-- | Reason for the current status of a specific speech synthesis task,
-- including errors if the task has failed.
synthesisTask_taskStatusReason :: Lens.Lens' SynthesisTask (Core.Maybe Core.Text)
synthesisTask_taskStatusReason = Lens.lens (\SynthesisTask' {taskStatusReason} -> taskStatusReason) (\s@SynthesisTask' {} a -> s {taskStatusReason = a} :: SynthesisTask)

-- | ARN for the SNS topic optionally used for providing status notification
-- for a speech synthesis task.
synthesisTask_snsTopicArn :: Lens.Lens' SynthesisTask (Core.Maybe Core.Text)
synthesisTask_snsTopicArn = Lens.lens (\SynthesisTask' {snsTopicArn} -> snsTopicArn) (\s@SynthesisTask' {} a -> s {snsTopicArn = a} :: SynthesisTask)

instance Core.FromJSON SynthesisTask where
  parseJSON =
    Core.withObject
      "SynthesisTask"
      ( \x ->
          SynthesisTask'
            Core.<$> (x Core..:? "LanguageCode")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "OutputUri")
            Core.<*> (x Core..:? "SpeechMarkTypes" Core..!= Core.mempty)
            Core.<*> (x Core..:? "LexiconNames" Core..!= Core.mempty)
            Core.<*> (x Core..:? "VoiceId")
            Core.<*> (x Core..:? "TaskId")
            Core.<*> (x Core..:? "TextType")
            Core.<*> (x Core..:? "OutputFormat")
            Core.<*> (x Core..:? "SampleRate")
            Core.<*> (x Core..:? "TaskStatus")
            Core.<*> (x Core..:? "Engine")
            Core.<*> (x Core..:? "RequestCharacters")
            Core.<*> (x Core..:? "TaskStatusReason")
            Core.<*> (x Core..:? "SnsTopicArn")
      )

instance Core.Hashable SynthesisTask

instance Core.NFData SynthesisTask
