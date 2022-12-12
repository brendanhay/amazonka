{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Polly.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Polly.Lens
  ( -- * Operations

    -- ** DeleteLexicon
    deleteLexicon_name,
    deleteLexiconResponse_httpStatus,

    -- ** DescribeVoices
    describeVoices_engine,
    describeVoices_includeAdditionalLanguageCodes,
    describeVoices_languageCode,
    describeVoices_nextToken,
    describeVoicesResponse_nextToken,
    describeVoicesResponse_voices,
    describeVoicesResponse_httpStatus,

    -- ** GetLexicon
    getLexicon_name,
    getLexiconResponse_lexicon,
    getLexiconResponse_lexiconAttributes,
    getLexiconResponse_httpStatus,

    -- ** GetSpeechSynthesisTask
    getSpeechSynthesisTask_taskId,
    getSpeechSynthesisTaskResponse_synthesisTask,
    getSpeechSynthesisTaskResponse_httpStatus,

    -- ** ListLexicons
    listLexicons_nextToken,
    listLexiconsResponse_lexicons,
    listLexiconsResponse_nextToken,
    listLexiconsResponse_httpStatus,

    -- ** ListSpeechSynthesisTasks
    listSpeechSynthesisTasks_maxResults,
    listSpeechSynthesisTasks_nextToken,
    listSpeechSynthesisTasks_status,
    listSpeechSynthesisTasksResponse_nextToken,
    listSpeechSynthesisTasksResponse_synthesisTasks,
    listSpeechSynthesisTasksResponse_httpStatus,

    -- ** PutLexicon
    putLexicon_name,
    putLexicon_content,
    putLexiconResponse_httpStatus,

    -- ** StartSpeechSynthesisTask
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
    startSpeechSynthesisTaskResponse_synthesisTask,
    startSpeechSynthesisTaskResponse_httpStatus,

    -- ** SynthesizeSpeech
    synthesizeSpeech_engine,
    synthesizeSpeech_languageCode,
    synthesizeSpeech_lexiconNames,
    synthesizeSpeech_sampleRate,
    synthesizeSpeech_speechMarkTypes,
    synthesizeSpeech_textType,
    synthesizeSpeech_outputFormat,
    synthesizeSpeech_text,
    synthesizeSpeech_voiceId,
    synthesizeSpeechResponse_contentType,
    synthesizeSpeechResponse_requestCharacters,
    synthesizeSpeechResponse_httpStatus,
    synthesizeSpeechResponse_audioStream,

    -- * Types

    -- ** Lexicon
    lexicon_content,
    lexicon_name,

    -- ** LexiconAttributes
    lexiconAttributes_alphabet,
    lexiconAttributes_languageCode,
    lexiconAttributes_lastModified,
    lexiconAttributes_lexemesCount,
    lexiconAttributes_lexiconArn,
    lexiconAttributes_size,

    -- ** LexiconDescription
    lexiconDescription_attributes,
    lexiconDescription_name,

    -- ** SynthesisTask
    synthesisTask_creationTime,
    synthesisTask_engine,
    synthesisTask_languageCode,
    synthesisTask_lexiconNames,
    synthesisTask_outputFormat,
    synthesisTask_outputUri,
    synthesisTask_requestCharacters,
    synthesisTask_sampleRate,
    synthesisTask_snsTopicArn,
    synthesisTask_speechMarkTypes,
    synthesisTask_taskId,
    synthesisTask_taskStatus,
    synthesisTask_taskStatusReason,
    synthesisTask_textType,
    synthesisTask_voiceId,

    -- ** Voice
    voice_additionalLanguageCodes,
    voice_gender,
    voice_id,
    voice_languageCode,
    voice_languageName,
    voice_name,
    voice_supportedEngines,
  )
where

import Amazonka.Polly.DeleteLexicon
import Amazonka.Polly.DescribeVoices
import Amazonka.Polly.GetLexicon
import Amazonka.Polly.GetSpeechSynthesisTask
import Amazonka.Polly.ListLexicons
import Amazonka.Polly.ListSpeechSynthesisTasks
import Amazonka.Polly.PutLexicon
import Amazonka.Polly.StartSpeechSynthesisTask
import Amazonka.Polly.SynthesizeSpeech
import Amazonka.Polly.Types.Lexicon
import Amazonka.Polly.Types.LexiconAttributes
import Amazonka.Polly.Types.LexiconDescription
import Amazonka.Polly.Types.SynthesisTask
import Amazonka.Polly.Types.Voice
