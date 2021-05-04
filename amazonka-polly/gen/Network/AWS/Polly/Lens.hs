{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Polly.Lens
  ( -- * Operations

    -- ** GetSpeechSynthesisTask
    getSpeechSynthesisTask_taskId,
    getSpeechSynthesisTaskResponse_synthesisTask,
    getSpeechSynthesisTaskResponse_httpStatus,

    -- ** GetLexicon
    getLexicon_name,
    getLexiconResponse_lexiconAttributes,
    getLexiconResponse_lexicon,
    getLexiconResponse_httpStatus,

    -- ** ListLexicons
    listLexicons_nextToken,
    listLexiconsResponse_nextToken,
    listLexiconsResponse_lexicons,
    listLexiconsResponse_httpStatus,

    -- ** DeleteLexicon
    deleteLexicon_name,
    deleteLexiconResponse_httpStatus,

    -- ** DescribeVoices
    describeVoices_languageCode,
    describeVoices_nextToken,
    describeVoices_includeAdditionalLanguageCodes,
    describeVoices_engine,
    describeVoicesResponse_nextToken,
    describeVoicesResponse_voices,
    describeVoicesResponse_httpStatus,

    -- ** StartSpeechSynthesisTask
    startSpeechSynthesisTask_languageCode,
    startSpeechSynthesisTask_speechMarkTypes,
    startSpeechSynthesisTask_lexiconNames,
    startSpeechSynthesisTask_textType,
    startSpeechSynthesisTask_sampleRate,
    startSpeechSynthesisTask_engine,
    startSpeechSynthesisTask_outputS3KeyPrefix,
    startSpeechSynthesisTask_snsTopicArn,
    startSpeechSynthesisTask_outputFormat,
    startSpeechSynthesisTask_outputS3BucketName,
    startSpeechSynthesisTask_text,
    startSpeechSynthesisTask_voiceId,
    startSpeechSynthesisTaskResponse_synthesisTask,
    startSpeechSynthesisTaskResponse_httpStatus,

    -- ** PutLexicon
    putLexicon_name,
    putLexicon_content,
    putLexiconResponse_httpStatus,

    -- ** SynthesizeSpeech
    synthesizeSpeech_languageCode,
    synthesizeSpeech_speechMarkTypes,
    synthesizeSpeech_lexiconNames,
    synthesizeSpeech_textType,
    synthesizeSpeech_sampleRate,
    synthesizeSpeech_engine,
    synthesizeSpeech_outputFormat,
    synthesizeSpeech_text,
    synthesizeSpeech_voiceId,
    synthesizeSpeechResponse_contentType,
    synthesizeSpeechResponse_requestCharacters,
    synthesizeSpeechResponse_httpStatus,
    synthesizeSpeechResponse_audioStream,

    -- ** ListSpeechSynthesisTasks
    listSpeechSynthesisTasks_status,
    listSpeechSynthesisTasks_nextToken,
    listSpeechSynthesisTasks_maxResults,
    listSpeechSynthesisTasksResponse_nextToken,
    listSpeechSynthesisTasksResponse_synthesisTasks,
    listSpeechSynthesisTasksResponse_httpStatus,

    -- * Types

    -- ** Lexicon
    lexicon_name,
    lexicon_content,

    -- ** LexiconAttributes
    lexiconAttributes_languageCode,
    lexiconAttributes_lexiconArn,
    lexiconAttributes_alphabet,
    lexiconAttributes_lexemesCount,
    lexiconAttributes_lastModified,
    lexiconAttributes_size,

    -- ** LexiconDescription
    lexiconDescription_attributes,
    lexiconDescription_name,

    -- ** SynthesisTask
    synthesisTask_languageCode,
    synthesisTask_creationTime,
    synthesisTask_outputUri,
    synthesisTask_speechMarkTypes,
    synthesisTask_lexiconNames,
    synthesisTask_voiceId,
    synthesisTask_taskId,
    synthesisTask_textType,
    synthesisTask_outputFormat,
    synthesisTask_sampleRate,
    synthesisTask_taskStatus,
    synthesisTask_engine,
    synthesisTask_requestCharacters,
    synthesisTask_taskStatusReason,
    synthesisTask_snsTopicArn,

    -- ** Voice
    voice_languageCode,
    voice_id,
    voice_gender,
    voice_name,
    voice_supportedEngines,
    voice_additionalLanguageCodes,
    voice_languageName,
  )
where

import Network.AWS.Polly.DeleteLexicon
import Network.AWS.Polly.DescribeVoices
import Network.AWS.Polly.GetLexicon
import Network.AWS.Polly.GetSpeechSynthesisTask
import Network.AWS.Polly.ListLexicons
import Network.AWS.Polly.ListSpeechSynthesisTasks
import Network.AWS.Polly.PutLexicon
import Network.AWS.Polly.StartSpeechSynthesisTask
import Network.AWS.Polly.SynthesizeSpeech
import Network.AWS.Polly.Types.Lexicon
import Network.AWS.Polly.Types.LexiconAttributes
import Network.AWS.Polly.Types.LexiconDescription
import Network.AWS.Polly.Types.SynthesisTask
import Network.AWS.Polly.Types.Voice
