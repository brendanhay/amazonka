{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Polly.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    describeVoices_nextToken,
    describeVoices_languageCode,
    describeVoices_includeAdditionalLanguageCodes,
    describeVoices_engine,
    describeVoicesResponse_voices,
    describeVoicesResponse_nextToken,
    describeVoicesResponse_httpStatus,

    -- ** GetLexicon
    getLexicon_name,
    getLexiconResponse_lexiconAttributes,
    getLexiconResponse_lexicon,
    getLexiconResponse_httpStatus,

    -- ** GetSpeechSynthesisTask
    getSpeechSynthesisTask_taskId,
    getSpeechSynthesisTaskResponse_synthesisTask,
    getSpeechSynthesisTaskResponse_httpStatus,

    -- ** ListLexicons
    listLexicons_nextToken,
    listLexiconsResponse_nextToken,
    listLexiconsResponse_lexicons,
    listLexiconsResponse_httpStatus,

    -- ** ListSpeechSynthesisTasks
    listSpeechSynthesisTasks_nextToken,
    listSpeechSynthesisTasks_status,
    listSpeechSynthesisTasks_maxResults,
    listSpeechSynthesisTasksResponse_nextToken,
    listSpeechSynthesisTasksResponse_synthesisTasks,
    listSpeechSynthesisTasksResponse_httpStatus,

    -- ** PutLexicon
    putLexicon_name,
    putLexicon_content,
    putLexiconResponse_httpStatus,

    -- ** StartSpeechSynthesisTask
    startSpeechSynthesisTask_sampleRate,
    startSpeechSynthesisTask_speechMarkTypes,
    startSpeechSynthesisTask_snsTopicArn,
    startSpeechSynthesisTask_languageCode,
    startSpeechSynthesisTask_lexiconNames,
    startSpeechSynthesisTask_engine,
    startSpeechSynthesisTask_textType,
    startSpeechSynthesisTask_outputS3KeyPrefix,
    startSpeechSynthesisTask_outputFormat,
    startSpeechSynthesisTask_outputS3BucketName,
    startSpeechSynthesisTask_text,
    startSpeechSynthesisTask_voiceId,
    startSpeechSynthesisTaskResponse_synthesisTask,
    startSpeechSynthesisTaskResponse_httpStatus,

    -- ** SynthesizeSpeech
    synthesizeSpeech_sampleRate,
    synthesizeSpeech_speechMarkTypes,
    synthesizeSpeech_languageCode,
    synthesizeSpeech_lexiconNames,
    synthesizeSpeech_engine,
    synthesizeSpeech_textType,
    synthesizeSpeech_outputFormat,
    synthesizeSpeech_text,
    synthesizeSpeech_voiceId,
    synthesizeSpeechResponse_requestCharacters,
    synthesizeSpeechResponse_contentType,
    synthesizeSpeechResponse_httpStatus,
    synthesizeSpeechResponse_audioStream,

    -- * Types

    -- ** Lexicon
    lexicon_name,
    lexicon_content,

    -- ** LexiconAttributes
    lexiconAttributes_alphabet,
    lexiconAttributes_lexiconArn,
    lexiconAttributes_size,
    lexiconAttributes_languageCode,
    lexiconAttributes_lastModified,
    lexiconAttributes_lexemesCount,

    -- ** LexiconDescription
    lexiconDescription_name,
    lexiconDescription_attributes,

    -- ** SynthesisTask
    synthesisTask_voiceId,
    synthesisTask_taskStatusReason,
    synthesisTask_taskId,
    synthesisTask_sampleRate,
    synthesisTask_taskStatus,
    synthesisTask_speechMarkTypes,
    synthesisTask_outputFormat,
    synthesisTask_outputUri,
    synthesisTask_snsTopicArn,
    synthesisTask_languageCode,
    synthesisTask_requestCharacters,
    synthesisTask_lexiconNames,
    synthesisTask_engine,
    synthesisTask_creationTime,
    synthesisTask_textType,

    -- ** Voice
    voice_supportedEngines,
    voice_name,
    voice_additionalLanguageCodes,
    voice_id,
    voice_languageCode,
    voice_languageName,
    voice_gender,
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
