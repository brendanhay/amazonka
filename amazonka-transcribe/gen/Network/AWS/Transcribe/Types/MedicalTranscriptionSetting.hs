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
-- Module      : Network.AWS.Transcribe.Types.MedicalTranscriptionSetting
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.MedicalTranscriptionSetting where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Optional settings for the StartMedicalTranscriptionJob operation.
--
-- /See:/ 'newMedicalTranscriptionSetting' smart constructor.
data MedicalTranscriptionSetting = MedicalTranscriptionSetting'
  { -- | Determines whether alternative transcripts are generated along with the
    -- transcript that has the highest confidence. If you set
    -- @ShowAlternatives@ field to true, you must also set the maximum number
    -- of alternatives to return in the @MaxAlternatives@ field.
    showAlternatives :: Core.Maybe Core.Bool,
    -- | Instructs Amazon Transcribe Medical to process each audio channel
    -- separately and then merge the transcription output of each channel into
    -- a single transcription.
    --
    -- Amazon Transcribe Medical also produces a transcription of each item
    -- detected on an audio channel, including the start time and end time of
    -- the item and alternative transcriptions of item. The alternative
    -- transcriptions also come with confidence scores provided by Amazon
    -- Transcribe Medical.
    --
    -- You can\'t set both @ShowSpeakerLabels@ and @ChannelIdentification@ in
    -- the same request. If you set both, your request returns a
    -- @BadRequestException@
    channelIdentification :: Core.Maybe Core.Bool,
    -- | The maximum number of alternatives that you tell the service to return.
    -- If you specify the @MaxAlternatives@ field, you must set the
    -- @ShowAlternatives@ field to true.
    maxAlternatives :: Core.Maybe Core.Natural,
    -- | Determines whether the transcription job uses speaker recognition to
    -- identify different speakers in the input audio. Speaker recognition
    -- labels individual speakers in the audio file. If you set the
    -- @ShowSpeakerLabels@ field to true, you must also set the maximum number
    -- of speaker labels in the @MaxSpeakerLabels@ field.
    --
    -- You can\'t set both @ShowSpeakerLabels@ and @ChannelIdentification@ in
    -- the same request. If you set both, your request returns a
    -- @BadRequestException@.
    showSpeakerLabels :: Core.Maybe Core.Bool,
    -- | The name of the vocabulary to use when processing a medical
    -- transcription job.
    vocabularyName :: Core.Maybe Core.Text,
    -- | The maximum number of speakers to identify in the input audio. If there
    -- are more speakers in the audio than this number, multiple speakers are
    -- identified as a single speaker. If you specify the @MaxSpeakerLabels@
    -- field, you must set the @ShowSpeakerLabels@ field to true.
    maxSpeakerLabels :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MedicalTranscriptionSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'showAlternatives', 'medicalTranscriptionSetting_showAlternatives' - Determines whether alternative transcripts are generated along with the
-- transcript that has the highest confidence. If you set
-- @ShowAlternatives@ field to true, you must also set the maximum number
-- of alternatives to return in the @MaxAlternatives@ field.
--
-- 'channelIdentification', 'medicalTranscriptionSetting_channelIdentification' - Instructs Amazon Transcribe Medical to process each audio channel
-- separately and then merge the transcription output of each channel into
-- a single transcription.
--
-- Amazon Transcribe Medical also produces a transcription of each item
-- detected on an audio channel, including the start time and end time of
-- the item and alternative transcriptions of item. The alternative
-- transcriptions also come with confidence scores provided by Amazon
-- Transcribe Medical.
--
-- You can\'t set both @ShowSpeakerLabels@ and @ChannelIdentification@ in
-- the same request. If you set both, your request returns a
-- @BadRequestException@
--
-- 'maxAlternatives', 'medicalTranscriptionSetting_maxAlternatives' - The maximum number of alternatives that you tell the service to return.
-- If you specify the @MaxAlternatives@ field, you must set the
-- @ShowAlternatives@ field to true.
--
-- 'showSpeakerLabels', 'medicalTranscriptionSetting_showSpeakerLabels' - Determines whether the transcription job uses speaker recognition to
-- identify different speakers in the input audio. Speaker recognition
-- labels individual speakers in the audio file. If you set the
-- @ShowSpeakerLabels@ field to true, you must also set the maximum number
-- of speaker labels in the @MaxSpeakerLabels@ field.
--
-- You can\'t set both @ShowSpeakerLabels@ and @ChannelIdentification@ in
-- the same request. If you set both, your request returns a
-- @BadRequestException@.
--
-- 'vocabularyName', 'medicalTranscriptionSetting_vocabularyName' - The name of the vocabulary to use when processing a medical
-- transcription job.
--
-- 'maxSpeakerLabels', 'medicalTranscriptionSetting_maxSpeakerLabels' - The maximum number of speakers to identify in the input audio. If there
-- are more speakers in the audio than this number, multiple speakers are
-- identified as a single speaker. If you specify the @MaxSpeakerLabels@
-- field, you must set the @ShowSpeakerLabels@ field to true.
newMedicalTranscriptionSetting ::
  MedicalTranscriptionSetting
newMedicalTranscriptionSetting =
  MedicalTranscriptionSetting'
    { showAlternatives =
        Core.Nothing,
      channelIdentification = Core.Nothing,
      maxAlternatives = Core.Nothing,
      showSpeakerLabels = Core.Nothing,
      vocabularyName = Core.Nothing,
      maxSpeakerLabels = Core.Nothing
    }

-- | Determines whether alternative transcripts are generated along with the
-- transcript that has the highest confidence. If you set
-- @ShowAlternatives@ field to true, you must also set the maximum number
-- of alternatives to return in the @MaxAlternatives@ field.
medicalTranscriptionSetting_showAlternatives :: Lens.Lens' MedicalTranscriptionSetting (Core.Maybe Core.Bool)
medicalTranscriptionSetting_showAlternatives = Lens.lens (\MedicalTranscriptionSetting' {showAlternatives} -> showAlternatives) (\s@MedicalTranscriptionSetting' {} a -> s {showAlternatives = a} :: MedicalTranscriptionSetting)

-- | Instructs Amazon Transcribe Medical to process each audio channel
-- separately and then merge the transcription output of each channel into
-- a single transcription.
--
-- Amazon Transcribe Medical also produces a transcription of each item
-- detected on an audio channel, including the start time and end time of
-- the item and alternative transcriptions of item. The alternative
-- transcriptions also come with confidence scores provided by Amazon
-- Transcribe Medical.
--
-- You can\'t set both @ShowSpeakerLabels@ and @ChannelIdentification@ in
-- the same request. If you set both, your request returns a
-- @BadRequestException@
medicalTranscriptionSetting_channelIdentification :: Lens.Lens' MedicalTranscriptionSetting (Core.Maybe Core.Bool)
medicalTranscriptionSetting_channelIdentification = Lens.lens (\MedicalTranscriptionSetting' {channelIdentification} -> channelIdentification) (\s@MedicalTranscriptionSetting' {} a -> s {channelIdentification = a} :: MedicalTranscriptionSetting)

-- | The maximum number of alternatives that you tell the service to return.
-- If you specify the @MaxAlternatives@ field, you must set the
-- @ShowAlternatives@ field to true.
medicalTranscriptionSetting_maxAlternatives :: Lens.Lens' MedicalTranscriptionSetting (Core.Maybe Core.Natural)
medicalTranscriptionSetting_maxAlternatives = Lens.lens (\MedicalTranscriptionSetting' {maxAlternatives} -> maxAlternatives) (\s@MedicalTranscriptionSetting' {} a -> s {maxAlternatives = a} :: MedicalTranscriptionSetting)

-- | Determines whether the transcription job uses speaker recognition to
-- identify different speakers in the input audio. Speaker recognition
-- labels individual speakers in the audio file. If you set the
-- @ShowSpeakerLabels@ field to true, you must also set the maximum number
-- of speaker labels in the @MaxSpeakerLabels@ field.
--
-- You can\'t set both @ShowSpeakerLabels@ and @ChannelIdentification@ in
-- the same request. If you set both, your request returns a
-- @BadRequestException@.
medicalTranscriptionSetting_showSpeakerLabels :: Lens.Lens' MedicalTranscriptionSetting (Core.Maybe Core.Bool)
medicalTranscriptionSetting_showSpeakerLabels = Lens.lens (\MedicalTranscriptionSetting' {showSpeakerLabels} -> showSpeakerLabels) (\s@MedicalTranscriptionSetting' {} a -> s {showSpeakerLabels = a} :: MedicalTranscriptionSetting)

-- | The name of the vocabulary to use when processing a medical
-- transcription job.
medicalTranscriptionSetting_vocabularyName :: Lens.Lens' MedicalTranscriptionSetting (Core.Maybe Core.Text)
medicalTranscriptionSetting_vocabularyName = Lens.lens (\MedicalTranscriptionSetting' {vocabularyName} -> vocabularyName) (\s@MedicalTranscriptionSetting' {} a -> s {vocabularyName = a} :: MedicalTranscriptionSetting)

-- | The maximum number of speakers to identify in the input audio. If there
-- are more speakers in the audio than this number, multiple speakers are
-- identified as a single speaker. If you specify the @MaxSpeakerLabels@
-- field, you must set the @ShowSpeakerLabels@ field to true.
medicalTranscriptionSetting_maxSpeakerLabels :: Lens.Lens' MedicalTranscriptionSetting (Core.Maybe Core.Natural)
medicalTranscriptionSetting_maxSpeakerLabels = Lens.lens (\MedicalTranscriptionSetting' {maxSpeakerLabels} -> maxSpeakerLabels) (\s@MedicalTranscriptionSetting' {} a -> s {maxSpeakerLabels = a} :: MedicalTranscriptionSetting)

instance Core.FromJSON MedicalTranscriptionSetting where
  parseJSON =
    Core.withObject
      "MedicalTranscriptionSetting"
      ( \x ->
          MedicalTranscriptionSetting'
            Core.<$> (x Core..:? "ShowAlternatives")
            Core.<*> (x Core..:? "ChannelIdentification")
            Core.<*> (x Core..:? "MaxAlternatives")
            Core.<*> (x Core..:? "ShowSpeakerLabels")
            Core.<*> (x Core..:? "VocabularyName")
            Core.<*> (x Core..:? "MaxSpeakerLabels")
      )

instance Core.Hashable MedicalTranscriptionSetting

instance Core.NFData MedicalTranscriptionSetting

instance Core.ToJSON MedicalTranscriptionSetting where
  toJSON MedicalTranscriptionSetting' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ShowAlternatives" Core..=)
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
