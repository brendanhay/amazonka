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
-- Module      : Amazonka.Transcribe.Types.Settings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.Settings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transcribe.Types.VocabularyFilterMethod

-- | Provides optional settings for the @StartTranscriptionJob@ operation.
--
-- /See:/ 'newSettings' smart constructor.
data Settings = Settings'
  { -- | The name of a vocabulary to use when processing the transcription job.
    vocabularyName :: Prelude.Maybe Prelude.Text,
    -- | The number of alternative transcriptions that the service should return.
    -- If you specify the @MaxAlternatives@ field, you must set the
    -- @ShowAlternatives@ field to true.
    maxAlternatives :: Prelude.Maybe Prelude.Natural,
    -- | Instructs Amazon Transcribe to process each audio channel separately and
    -- then merge the transcription output of each channel into a single
    -- transcription.
    --
    -- Amazon Transcribe also produces a transcription of each item detected on
    -- an audio channel, including the start time and end time of the item and
    -- alternative transcriptions of the item including the confidence that
    -- Amazon Transcribe has in the transcription.
    --
    -- You can\'t set both @ShowSpeakerLabels@ and @ChannelIdentification@ in
    -- the same request. If you set both, your request returns a
    -- @BadRequestException@.
    channelIdentification :: Prelude.Maybe Prelude.Bool,
    -- | Determines whether the transcription contains alternative
    -- transcriptions. If you set the @ShowAlternatives@ field to true, you
    -- must also set the maximum number of alternatives to return in the
    -- @MaxAlternatives@ field.
    showAlternatives :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of speakers to identify in the input audio. If there
    -- are more speakers in the audio than this number, multiple speakers are
    -- identified as a single speaker. If you specify the @MaxSpeakerLabels@
    -- field, you must set the @ShowSpeakerLabels@ field to true.
    maxSpeakerLabels :: Prelude.Maybe Prelude.Natural,
    -- | The name of the vocabulary filter to use when transcribing the audio.
    -- The filter that you specify must have the same language code as the
    -- transcription job.
    vocabularyFilterName :: Prelude.Maybe Prelude.Text,
    -- | Determines whether the transcription job uses speaker recognition to
    -- identify different speakers in the input audio. Speaker recognition
    -- labels individual speakers in the audio file. If you set the
    -- @ShowSpeakerLabels@ field to true, you must also set the maximum number
    -- of speaker labels @MaxSpeakerLabels@ field.
    --
    -- You can\'t set both @ShowSpeakerLabels@ and @ChannelIdentification@ in
    -- the same request. If you set both, your request returns a
    -- @BadRequestException@.
    showSpeakerLabels :: Prelude.Maybe Prelude.Bool,
    -- | Set to @mask@ to remove filtered text from the transcript and replace it
    -- with three asterisks (\"***\") as placeholder text. Set to @remove@ to
    -- remove filtered text from the transcript without using placeholder text.
    -- Set to @tag@ to mark the word in the transcription output that matches
    -- the vocabulary filter. When you set the filter method to @tag@, the
    -- words matching your vocabulary filter are not masked or removed.
    vocabularyFilterMethod :: Prelude.Maybe VocabularyFilterMethod
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vocabularyName', 'settings_vocabularyName' - The name of a vocabulary to use when processing the transcription job.
--
-- 'maxAlternatives', 'settings_maxAlternatives' - The number of alternative transcriptions that the service should return.
-- If you specify the @MaxAlternatives@ field, you must set the
-- @ShowAlternatives@ field to true.
--
-- 'channelIdentification', 'settings_channelIdentification' - Instructs Amazon Transcribe to process each audio channel separately and
-- then merge the transcription output of each channel into a single
-- transcription.
--
-- Amazon Transcribe also produces a transcription of each item detected on
-- an audio channel, including the start time and end time of the item and
-- alternative transcriptions of the item including the confidence that
-- Amazon Transcribe has in the transcription.
--
-- You can\'t set both @ShowSpeakerLabels@ and @ChannelIdentification@ in
-- the same request. If you set both, your request returns a
-- @BadRequestException@.
--
-- 'showAlternatives', 'settings_showAlternatives' - Determines whether the transcription contains alternative
-- transcriptions. If you set the @ShowAlternatives@ field to true, you
-- must also set the maximum number of alternatives to return in the
-- @MaxAlternatives@ field.
--
-- 'maxSpeakerLabels', 'settings_maxSpeakerLabels' - The maximum number of speakers to identify in the input audio. If there
-- are more speakers in the audio than this number, multiple speakers are
-- identified as a single speaker. If you specify the @MaxSpeakerLabels@
-- field, you must set the @ShowSpeakerLabels@ field to true.
--
-- 'vocabularyFilterName', 'settings_vocabularyFilterName' - The name of the vocabulary filter to use when transcribing the audio.
-- The filter that you specify must have the same language code as the
-- transcription job.
--
-- 'showSpeakerLabels', 'settings_showSpeakerLabels' - Determines whether the transcription job uses speaker recognition to
-- identify different speakers in the input audio. Speaker recognition
-- labels individual speakers in the audio file. If you set the
-- @ShowSpeakerLabels@ field to true, you must also set the maximum number
-- of speaker labels @MaxSpeakerLabels@ field.
--
-- You can\'t set both @ShowSpeakerLabels@ and @ChannelIdentification@ in
-- the same request. If you set both, your request returns a
-- @BadRequestException@.
--
-- 'vocabularyFilterMethod', 'settings_vocabularyFilterMethod' - Set to @mask@ to remove filtered text from the transcript and replace it
-- with three asterisks (\"***\") as placeholder text. Set to @remove@ to
-- remove filtered text from the transcript without using placeholder text.
-- Set to @tag@ to mark the word in the transcription output that matches
-- the vocabulary filter. When you set the filter method to @tag@, the
-- words matching your vocabulary filter are not masked or removed.
newSettings ::
  Settings
newSettings =
  Settings'
    { vocabularyName = Prelude.Nothing,
      maxAlternatives = Prelude.Nothing,
      channelIdentification = Prelude.Nothing,
      showAlternatives = Prelude.Nothing,
      maxSpeakerLabels = Prelude.Nothing,
      vocabularyFilterName = Prelude.Nothing,
      showSpeakerLabels = Prelude.Nothing,
      vocabularyFilterMethod = Prelude.Nothing
    }

-- | The name of a vocabulary to use when processing the transcription job.
settings_vocabularyName :: Lens.Lens' Settings (Prelude.Maybe Prelude.Text)
settings_vocabularyName = Lens.lens (\Settings' {vocabularyName} -> vocabularyName) (\s@Settings' {} a -> s {vocabularyName = a} :: Settings)

-- | The number of alternative transcriptions that the service should return.
-- If you specify the @MaxAlternatives@ field, you must set the
-- @ShowAlternatives@ field to true.
settings_maxAlternatives :: Lens.Lens' Settings (Prelude.Maybe Prelude.Natural)
settings_maxAlternatives = Lens.lens (\Settings' {maxAlternatives} -> maxAlternatives) (\s@Settings' {} a -> s {maxAlternatives = a} :: Settings)

-- | Instructs Amazon Transcribe to process each audio channel separately and
-- then merge the transcription output of each channel into a single
-- transcription.
--
-- Amazon Transcribe also produces a transcription of each item detected on
-- an audio channel, including the start time and end time of the item and
-- alternative transcriptions of the item including the confidence that
-- Amazon Transcribe has in the transcription.
--
-- You can\'t set both @ShowSpeakerLabels@ and @ChannelIdentification@ in
-- the same request. If you set both, your request returns a
-- @BadRequestException@.
settings_channelIdentification :: Lens.Lens' Settings (Prelude.Maybe Prelude.Bool)
settings_channelIdentification = Lens.lens (\Settings' {channelIdentification} -> channelIdentification) (\s@Settings' {} a -> s {channelIdentification = a} :: Settings)

-- | Determines whether the transcription contains alternative
-- transcriptions. If you set the @ShowAlternatives@ field to true, you
-- must also set the maximum number of alternatives to return in the
-- @MaxAlternatives@ field.
settings_showAlternatives :: Lens.Lens' Settings (Prelude.Maybe Prelude.Bool)
settings_showAlternatives = Lens.lens (\Settings' {showAlternatives} -> showAlternatives) (\s@Settings' {} a -> s {showAlternatives = a} :: Settings)

-- | The maximum number of speakers to identify in the input audio. If there
-- are more speakers in the audio than this number, multiple speakers are
-- identified as a single speaker. If you specify the @MaxSpeakerLabels@
-- field, you must set the @ShowSpeakerLabels@ field to true.
settings_maxSpeakerLabels :: Lens.Lens' Settings (Prelude.Maybe Prelude.Natural)
settings_maxSpeakerLabels = Lens.lens (\Settings' {maxSpeakerLabels} -> maxSpeakerLabels) (\s@Settings' {} a -> s {maxSpeakerLabels = a} :: Settings)

-- | The name of the vocabulary filter to use when transcribing the audio.
-- The filter that you specify must have the same language code as the
-- transcription job.
settings_vocabularyFilterName :: Lens.Lens' Settings (Prelude.Maybe Prelude.Text)
settings_vocabularyFilterName = Lens.lens (\Settings' {vocabularyFilterName} -> vocabularyFilterName) (\s@Settings' {} a -> s {vocabularyFilterName = a} :: Settings)

-- | Determines whether the transcription job uses speaker recognition to
-- identify different speakers in the input audio. Speaker recognition
-- labels individual speakers in the audio file. If you set the
-- @ShowSpeakerLabels@ field to true, you must also set the maximum number
-- of speaker labels @MaxSpeakerLabels@ field.
--
-- You can\'t set both @ShowSpeakerLabels@ and @ChannelIdentification@ in
-- the same request. If you set both, your request returns a
-- @BadRequestException@.
settings_showSpeakerLabels :: Lens.Lens' Settings (Prelude.Maybe Prelude.Bool)
settings_showSpeakerLabels = Lens.lens (\Settings' {showSpeakerLabels} -> showSpeakerLabels) (\s@Settings' {} a -> s {showSpeakerLabels = a} :: Settings)

-- | Set to @mask@ to remove filtered text from the transcript and replace it
-- with three asterisks (\"***\") as placeholder text. Set to @remove@ to
-- remove filtered text from the transcript without using placeholder text.
-- Set to @tag@ to mark the word in the transcription output that matches
-- the vocabulary filter. When you set the filter method to @tag@, the
-- words matching your vocabulary filter are not masked or removed.
settings_vocabularyFilterMethod :: Lens.Lens' Settings (Prelude.Maybe VocabularyFilterMethod)
settings_vocabularyFilterMethod = Lens.lens (\Settings' {vocabularyFilterMethod} -> vocabularyFilterMethod) (\s@Settings' {} a -> s {vocabularyFilterMethod = a} :: Settings)

instance Core.FromJSON Settings where
  parseJSON =
    Core.withObject
      "Settings"
      ( \x ->
          Settings'
            Prelude.<$> (x Core..:? "VocabularyName")
            Prelude.<*> (x Core..:? "MaxAlternatives")
            Prelude.<*> (x Core..:? "ChannelIdentification")
            Prelude.<*> (x Core..:? "ShowAlternatives")
            Prelude.<*> (x Core..:? "MaxSpeakerLabels")
            Prelude.<*> (x Core..:? "VocabularyFilterName")
            Prelude.<*> (x Core..:? "ShowSpeakerLabels")
            Prelude.<*> (x Core..:? "VocabularyFilterMethod")
      )

instance Prelude.Hashable Settings

instance Prelude.NFData Settings

instance Core.ToJSON Settings where
  toJSON Settings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("VocabularyName" Core..=)
              Prelude.<$> vocabularyName,
            ("MaxAlternatives" Core..=)
              Prelude.<$> maxAlternatives,
            ("ChannelIdentification" Core..=)
              Prelude.<$> channelIdentification,
            ("ShowAlternatives" Core..=)
              Prelude.<$> showAlternatives,
            ("MaxSpeakerLabels" Core..=)
              Prelude.<$> maxSpeakerLabels,
            ("VocabularyFilterName" Core..=)
              Prelude.<$> vocabularyFilterName,
            ("ShowSpeakerLabels" Core..=)
              Prelude.<$> showSpeakerLabels,
            ("VocabularyFilterMethod" Core..=)
              Prelude.<$> vocabularyFilterMethod
          ]
      )
