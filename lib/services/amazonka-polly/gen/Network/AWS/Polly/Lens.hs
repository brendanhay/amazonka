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

    -- ** GetLexicon
    getLexicon_name,
    getLexiconResponse_lexiconAttributes,
    getLexiconResponse_lexicon,
    getLexiconResponse_httpStatus,

    -- ** GetSpeechSynthesisTask
    getSpeechSynthesisTask_taskId,
    getSpeechSynthesisTaskResponse_synthesisTask,
    getSpeechSynthesisTaskResponse_httpStatus,

    -- ** DescribeVoices
    describeVoices_languageCode,
    describeVoices_engine,
    describeVoices_nextToken,
    describeVoices_includeAdditionalLanguageCodes,
    describeVoicesResponse_nextToken,
    describeVoicesResponse_voices,
    describeVoicesResponse_httpStatus,

    -- ** ListLexicons
    listLexicons_nextToken,
    listLexiconsResponse_lexicons,
    listLexiconsResponse_nextToken,
    listLexiconsResponse_httpStatus,

    -- ** SynthesizeSpeech
    synthesizeSpeech_languageCode,
    synthesizeSpeech_engine,
    synthesizeSpeech_speechMarkTypes,
    synthesizeSpeech_sampleRate,
    synthesizeSpeech_textType,
    synthesizeSpeech_lexiconNames,
    synthesizeSpeech_outputFormat,
    synthesizeSpeech_text,
    synthesizeSpeech_voiceId,
    synthesizeSpeechResponse_requestCharacters,
    synthesizeSpeechResponse_contentType,
    synthesizeSpeechResponse_httpStatus,
    synthesizeSpeechResponse_audioStream,

    -- ** ListSpeechSynthesisTasks
    listSpeechSynthesisTasks_status,
    listSpeechSynthesisTasks_nextToken,
    listSpeechSynthesisTasks_maxResults,
    listSpeechSynthesisTasksResponse_nextToken,
    listSpeechSynthesisTasksResponse_synthesisTasks,
    listSpeechSynthesisTasksResponse_httpStatus,

    -- ** PutLexicon
    putLexicon_name,
    putLexicon_content,
    putLexiconResponse_httpStatus,

    -- ** DeleteLexicon
    deleteLexicon_name,
    deleteLexiconResponse_httpStatus,

    -- ** StartSpeechSynthesisTask
    startSpeechSynthesisTask_languageCode,
    startSpeechSynthesisTask_snsTopicArn,
    startSpeechSynthesisTask_outputS3KeyPrefix,
    startSpeechSynthesisTask_engine,
    startSpeechSynthesisTask_speechMarkTypes,
    startSpeechSynthesisTask_sampleRate,
    startSpeechSynthesisTask_textType,
    startSpeechSynthesisTask_lexiconNames,
    startSpeechSynthesisTask_outputFormat,
    startSpeechSynthesisTask_outputS3BucketName,
    startSpeechSynthesisTask_text,
    startSpeechSynthesisTask_voiceId,
    startSpeechSynthesisTaskResponse_synthesisTask,
    startSpeechSynthesisTaskResponse_httpStatus,

    -- * Types

    -- ** Lexicon
    lexicon_content,
    lexicon_name,

    -- ** LexiconAttributes
    lexiconAttributes_languageCode,
    lexiconAttributes_size,
    lexiconAttributes_lexemesCount,
    lexiconAttributes_lexiconArn,
    lexiconAttributes_alphabet,
    lexiconAttributes_lastModified,

    -- ** LexiconDescription
    lexiconDescription_attributes,
    lexiconDescription_name,

    -- ** SynthesisTask
    synthesisTask_creationTime,
    synthesisTask_languageCode,
    synthesisTask_snsTopicArn,
    synthesisTask_taskStatusReason,
    synthesisTask_taskId,
    synthesisTask_requestCharacters,
    synthesisTask_engine,
    synthesisTask_speechMarkTypes,
    synthesisTask_sampleRate,
    synthesisTask_outputFormat,
    synthesisTask_textType,
    synthesisTask_voiceId,
    synthesisTask_lexiconNames,
    synthesisTask_taskStatus,
    synthesisTask_outputUri,

    -- ** Voice
    voice_languageCode,
    voice_languageName,
    voice_gender,
    voice_name,
    voice_id,
    voice_additionalLanguageCodes,
    voice_supportedEngines,
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
