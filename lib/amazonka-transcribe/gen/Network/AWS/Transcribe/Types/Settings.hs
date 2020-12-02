{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.Settings where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Transcribe.Types.VocabularyFilterMethod

-- | Provides optional settings for the @StartTranscriptionJob@ operation.
--
--
--
-- /See:/ 'settings' smart constructor.
data Settings = Settings'
  { _sVocabularyName :: !(Maybe Text),
    _sMaxAlternatives :: !(Maybe Nat),
    _sChannelIdentification :: !(Maybe Bool),
    _sShowAlternatives :: !(Maybe Bool),
    _sMaxSpeakerLabels :: !(Maybe Nat),
    _sVocabularyFilterName :: !(Maybe Text),
    _sShowSpeakerLabels :: !(Maybe Bool),
    _sVocabularyFilterMethod :: !(Maybe VocabularyFilterMethod)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sVocabularyName' - The name of a vocabulary to use when processing the transcription job.
--
-- * 'sMaxAlternatives' - The number of alternative transcriptions that the service should return. If you specify the @MaxAlternatives@ field, you must set the @ShowAlternatives@ field to true.
--
-- * 'sChannelIdentification' - Instructs Amazon Transcribe to process each audio channel separately and then merge the transcription output of each channel into a single transcription.  Amazon Transcribe also produces a transcription of each item detected on an audio channel, including the start time and end time of the item and alternative transcriptions of the item including the confidence that Amazon Transcribe has in the transcription. You can't set both @ShowSpeakerLabels@ and @ChannelIdentification@ in the same request. If you set both, your request returns a @BadRequestException@ .
--
-- * 'sShowAlternatives' - Determines whether the transcription contains alternative transcriptions. If you set the @ShowAlternatives@ field to true, you must also set the maximum number of alternatives to return in the @MaxAlternatives@ field.
--
-- * 'sMaxSpeakerLabels' - The maximum number of speakers to identify in the input audio. If there are more speakers in the audio than this number, multiple speakers are identified as a single speaker. If you specify the @MaxSpeakerLabels@ field, you must set the @ShowSpeakerLabels@ field to true.
--
-- * 'sVocabularyFilterName' - The name of the vocabulary filter to use when transcribing the audio. The filter that you specify must have the same language code as the transcription job.
--
-- * 'sShowSpeakerLabels' - Determines whether the transcription job uses speaker recognition to identify different speakers in the input audio. Speaker recognition labels individual speakers in the audio file. If you set the @ShowSpeakerLabels@ field to true, you must also set the maximum number of speaker labels @MaxSpeakerLabels@ field. You can't set both @ShowSpeakerLabels@ and @ChannelIdentification@ in the same request. If you set both, your request returns a @BadRequestException@ .
--
-- * 'sVocabularyFilterMethod' - Set to @mask@ to remove filtered text from the transcript and replace it with three asterisks ("***") as placeholder text. Set to @remove@ to remove filtered text from the transcript without using placeholder text.
settings ::
  Settings
settings =
  Settings'
    { _sVocabularyName = Nothing,
      _sMaxAlternatives = Nothing,
      _sChannelIdentification = Nothing,
      _sShowAlternatives = Nothing,
      _sMaxSpeakerLabels = Nothing,
      _sVocabularyFilterName = Nothing,
      _sShowSpeakerLabels = Nothing,
      _sVocabularyFilterMethod = Nothing
    }

-- | The name of a vocabulary to use when processing the transcription job.
sVocabularyName :: Lens' Settings (Maybe Text)
sVocabularyName = lens _sVocabularyName (\s a -> s {_sVocabularyName = a})

-- | The number of alternative transcriptions that the service should return. If you specify the @MaxAlternatives@ field, you must set the @ShowAlternatives@ field to true.
sMaxAlternatives :: Lens' Settings (Maybe Natural)
sMaxAlternatives = lens _sMaxAlternatives (\s a -> s {_sMaxAlternatives = a}) . mapping _Nat

-- | Instructs Amazon Transcribe to process each audio channel separately and then merge the transcription output of each channel into a single transcription.  Amazon Transcribe also produces a transcription of each item detected on an audio channel, including the start time and end time of the item and alternative transcriptions of the item including the confidence that Amazon Transcribe has in the transcription. You can't set both @ShowSpeakerLabels@ and @ChannelIdentification@ in the same request. If you set both, your request returns a @BadRequestException@ .
sChannelIdentification :: Lens' Settings (Maybe Bool)
sChannelIdentification = lens _sChannelIdentification (\s a -> s {_sChannelIdentification = a})

-- | Determines whether the transcription contains alternative transcriptions. If you set the @ShowAlternatives@ field to true, you must also set the maximum number of alternatives to return in the @MaxAlternatives@ field.
sShowAlternatives :: Lens' Settings (Maybe Bool)
sShowAlternatives = lens _sShowAlternatives (\s a -> s {_sShowAlternatives = a})

-- | The maximum number of speakers to identify in the input audio. If there are more speakers in the audio than this number, multiple speakers are identified as a single speaker. If you specify the @MaxSpeakerLabels@ field, you must set the @ShowSpeakerLabels@ field to true.
sMaxSpeakerLabels :: Lens' Settings (Maybe Natural)
sMaxSpeakerLabels = lens _sMaxSpeakerLabels (\s a -> s {_sMaxSpeakerLabels = a}) . mapping _Nat

-- | The name of the vocabulary filter to use when transcribing the audio. The filter that you specify must have the same language code as the transcription job.
sVocabularyFilterName :: Lens' Settings (Maybe Text)
sVocabularyFilterName = lens _sVocabularyFilterName (\s a -> s {_sVocabularyFilterName = a})

-- | Determines whether the transcription job uses speaker recognition to identify different speakers in the input audio. Speaker recognition labels individual speakers in the audio file. If you set the @ShowSpeakerLabels@ field to true, you must also set the maximum number of speaker labels @MaxSpeakerLabels@ field. You can't set both @ShowSpeakerLabels@ and @ChannelIdentification@ in the same request. If you set both, your request returns a @BadRequestException@ .
sShowSpeakerLabels :: Lens' Settings (Maybe Bool)
sShowSpeakerLabels = lens _sShowSpeakerLabels (\s a -> s {_sShowSpeakerLabels = a})

-- | Set to @mask@ to remove filtered text from the transcript and replace it with three asterisks ("***") as placeholder text. Set to @remove@ to remove filtered text from the transcript without using placeholder text.
sVocabularyFilterMethod :: Lens' Settings (Maybe VocabularyFilterMethod)
sVocabularyFilterMethod = lens _sVocabularyFilterMethod (\s a -> s {_sVocabularyFilterMethod = a})

instance FromJSON Settings where
  parseJSON =
    withObject
      "Settings"
      ( \x ->
          Settings'
            <$> (x .:? "VocabularyName")
            <*> (x .:? "MaxAlternatives")
            <*> (x .:? "ChannelIdentification")
            <*> (x .:? "ShowAlternatives")
            <*> (x .:? "MaxSpeakerLabels")
            <*> (x .:? "VocabularyFilterName")
            <*> (x .:? "ShowSpeakerLabels")
            <*> (x .:? "VocabularyFilterMethod")
      )

instance Hashable Settings

instance NFData Settings

instance ToJSON Settings where
  toJSON Settings' {..} =
    object
      ( catMaybes
          [ ("VocabularyName" .=) <$> _sVocabularyName,
            ("MaxAlternatives" .=) <$> _sMaxAlternatives,
            ("ChannelIdentification" .=) <$> _sChannelIdentification,
            ("ShowAlternatives" .=) <$> _sShowAlternatives,
            ("MaxSpeakerLabels" .=) <$> _sMaxSpeakerLabels,
            ("VocabularyFilterName" .=) <$> _sVocabularyFilterName,
            ("ShowSpeakerLabels" .=) <$> _sShowSpeakerLabels,
            ("VocabularyFilterMethod" .=) <$> _sVocabularyFilterMethod
          ]
      )
