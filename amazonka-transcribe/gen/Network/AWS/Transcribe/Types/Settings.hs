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
-- Module      : Network.AWS.Transcribe.Types.Settings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.Settings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Transcribe.Types.VocabularyFilterMethod

-- | Provides optional settings for the @StartTranscriptionJob@ operation.
--
-- /See:/ 'newSettings' smart constructor.
data Settings = Settings'
  { -- | Set to @mask@ to remove filtered text from the transcript and replace it
    -- with three asterisks (\"***\") as placeholder text. Set to @remove@ to
    -- remove filtered text from the transcript without using placeholder text.
    vocabularyFilterMethod :: Core.Maybe VocabularyFilterMethod,
    -- | The name of the vocabulary filter to use when transcribing the audio.
    -- The filter that you specify must have the same language code as the
    -- transcription job.
    vocabularyFilterName :: Core.Maybe Core.Text,
    -- | Determines whether the transcription contains alternative
    -- transcriptions. If you set the @ShowAlternatives@ field to true, you
    -- must also set the maximum number of alternatives to return in the
    -- @MaxAlternatives@ field.
    showAlternatives :: Core.Maybe Core.Bool,
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
    channelIdentification :: Core.Maybe Core.Bool,
    -- | The number of alternative transcriptions that the service should return.
    -- If you specify the @MaxAlternatives@ field, you must set the
    -- @ShowAlternatives@ field to true.
    maxAlternatives :: Core.Maybe Core.Natural,
    -- | Determines whether the transcription job uses speaker recognition to
    -- identify different speakers in the input audio. Speaker recognition
    -- labels individual speakers in the audio file. If you set the
    -- @ShowSpeakerLabels@ field to true, you must also set the maximum number
    -- of speaker labels @MaxSpeakerLabels@ field.
    --
    -- You can\'t set both @ShowSpeakerLabels@ and @ChannelIdentification@ in
    -- the same request. If you set both, your request returns a
    -- @BadRequestException@.
    showSpeakerLabels :: Core.Maybe Core.Bool,
    -- | The name of a vocabulary to use when processing the transcription job.
    vocabularyName :: Core.Maybe Core.Text,
    -- | The maximum number of speakers to identify in the input audio. If there
    -- are more speakers in the audio than this number, multiple speakers are
    -- identified as a single speaker. If you specify the @MaxSpeakerLabels@
    -- field, you must set the @ShowSpeakerLabels@ field to true.
    maxSpeakerLabels :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vocabularyFilterMethod', 'settings_vocabularyFilterMethod' - Set to @mask@ to remove filtered text from the transcript and replace it
-- with three asterisks (\"***\") as placeholder text. Set to @remove@ to
-- remove filtered text from the transcript without using placeholder text.
--
-- 'vocabularyFilterName', 'settings_vocabularyFilterName' - The name of the vocabulary filter to use when transcribing the audio.
-- The filter that you specify must have the same language code as the
-- transcription job.
--
-- 'showAlternatives', 'settings_showAlternatives' - Determines whether the transcription contains alternative
-- transcriptions. If you set the @ShowAlternatives@ field to true, you
-- must also set the maximum number of alternatives to return in the
-- @MaxAlternatives@ field.
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
-- 'maxAlternatives', 'settings_maxAlternatives' - The number of alternative transcriptions that the service should return.
-- If you specify the @MaxAlternatives@ field, you must set the
-- @ShowAlternatives@ field to true.
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
-- 'vocabularyName', 'settings_vocabularyName' - The name of a vocabulary to use when processing the transcription job.
--
-- 'maxSpeakerLabels', 'settings_maxSpeakerLabels' - The maximum number of speakers to identify in the input audio. If there
-- are more speakers in the audio than this number, multiple speakers are
-- identified as a single speaker. If you specify the @MaxSpeakerLabels@
-- field, you must set the @ShowSpeakerLabels@ field to true.
newSettings ::
  Settings
newSettings =
  Settings'
    { vocabularyFilterMethod = Core.Nothing,
      vocabularyFilterName = Core.Nothing,
      showAlternatives = Core.Nothing,
      channelIdentification = Core.Nothing,
      maxAlternatives = Core.Nothing,
      showSpeakerLabels = Core.Nothing,
      vocabularyName = Core.Nothing,
      maxSpeakerLabels = Core.Nothing
    }

-- | Set to @mask@ to remove filtered text from the transcript and replace it
-- with three asterisks (\"***\") as placeholder text. Set to @remove@ to
-- remove filtered text from the transcript without using placeholder text.
settings_vocabularyFilterMethod :: Lens.Lens' Settings (Core.Maybe VocabularyFilterMethod)
settings_vocabularyFilterMethod = Lens.lens (\Settings' {vocabularyFilterMethod} -> vocabularyFilterMethod) (\s@Settings' {} a -> s {vocabularyFilterMethod = a} :: Settings)

-- | The name of the vocabulary filter to use when transcribing the audio.
-- The filter that you specify must have the same language code as the
-- transcription job.
settings_vocabularyFilterName :: Lens.Lens' Settings (Core.Maybe Core.Text)
settings_vocabularyFilterName = Lens.lens (\Settings' {vocabularyFilterName} -> vocabularyFilterName) (\s@Settings' {} a -> s {vocabularyFilterName = a} :: Settings)

-- | Determines whether the transcription contains alternative
-- transcriptions. If you set the @ShowAlternatives@ field to true, you
-- must also set the maximum number of alternatives to return in the
-- @MaxAlternatives@ field.
settings_showAlternatives :: Lens.Lens' Settings (Core.Maybe Core.Bool)
settings_showAlternatives = Lens.lens (\Settings' {showAlternatives} -> showAlternatives) (\s@Settings' {} a -> s {showAlternatives = a} :: Settings)

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
settings_channelIdentification :: Lens.Lens' Settings (Core.Maybe Core.Bool)
settings_channelIdentification = Lens.lens (\Settings' {channelIdentification} -> channelIdentification) (\s@Settings' {} a -> s {channelIdentification = a} :: Settings)

-- | The number of alternative transcriptions that the service should return.
-- If you specify the @MaxAlternatives@ field, you must set the
-- @ShowAlternatives@ field to true.
settings_maxAlternatives :: Lens.Lens' Settings (Core.Maybe Core.Natural)
settings_maxAlternatives = Lens.lens (\Settings' {maxAlternatives} -> maxAlternatives) (\s@Settings' {} a -> s {maxAlternatives = a} :: Settings)

-- | Determines whether the transcription job uses speaker recognition to
-- identify different speakers in the input audio. Speaker recognition
-- labels individual speakers in the audio file. If you set the
-- @ShowSpeakerLabels@ field to true, you must also set the maximum number
-- of speaker labels @MaxSpeakerLabels@ field.
--
-- You can\'t set both @ShowSpeakerLabels@ and @ChannelIdentification@ in
-- the same request. If you set both, your request returns a
-- @BadRequestException@.
settings_showSpeakerLabels :: Lens.Lens' Settings (Core.Maybe Core.Bool)
settings_showSpeakerLabels = Lens.lens (\Settings' {showSpeakerLabels} -> showSpeakerLabels) (\s@Settings' {} a -> s {showSpeakerLabels = a} :: Settings)

-- | The name of a vocabulary to use when processing the transcription job.
settings_vocabularyName :: Lens.Lens' Settings (Core.Maybe Core.Text)
settings_vocabularyName = Lens.lens (\Settings' {vocabularyName} -> vocabularyName) (\s@Settings' {} a -> s {vocabularyName = a} :: Settings)

-- | The maximum number of speakers to identify in the input audio. If there
-- are more speakers in the audio than this number, multiple speakers are
-- identified as a single speaker. If you specify the @MaxSpeakerLabels@
-- field, you must set the @ShowSpeakerLabels@ field to true.
settings_maxSpeakerLabels :: Lens.Lens' Settings (Core.Maybe Core.Natural)
settings_maxSpeakerLabels = Lens.lens (\Settings' {maxSpeakerLabels} -> maxSpeakerLabels) (\s@Settings' {} a -> s {maxSpeakerLabels = a} :: Settings)

instance Core.FromJSON Settings where
  parseJSON =
    Core.withObject
      "Settings"
      ( \x ->
          Settings'
            Core.<$> (x Core..:? "VocabularyFilterMethod")
            Core.<*> (x Core..:? "VocabularyFilterName")
            Core.<*> (x Core..:? "ShowAlternatives")
            Core.<*> (x Core..:? "ChannelIdentification")
            Core.<*> (x Core..:? "MaxAlternatives")
            Core.<*> (x Core..:? "ShowSpeakerLabels")
            Core.<*> (x Core..:? "VocabularyName")
            Core.<*> (x Core..:? "MaxSpeakerLabels")
      )

instance Core.Hashable Settings

instance Core.NFData Settings

instance Core.ToJSON Settings where
  toJSON Settings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("VocabularyFilterMethod" Core..=)
              Core.<$> vocabularyFilterMethod,
            ("VocabularyFilterName" Core..=)
              Core.<$> vocabularyFilterName,
            ("ShowAlternatives" Core..=)
              Core.<$> showAlternatives,
            ("ChannelIdentification" Core..=)
              Core.<$> channelIdentification,
            ("MaxAlternatives" Core..=) Core.<$> maxAlternatives,
            ("ShowSpeakerLabels" Core..=)
              Core.<$> showSpeakerLabels,
            ("VocabularyName" Core..=) Core.<$> vocabularyName,
            ("MaxSpeakerLabels" Core..=)
              Core.<$> maxSpeakerLabels
          ]
      )
