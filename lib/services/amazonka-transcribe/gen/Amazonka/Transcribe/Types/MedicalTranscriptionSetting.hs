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
-- Module      : Amazonka.Transcribe.Types.MedicalTranscriptionSetting
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.MedicalTranscriptionSetting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Optional settings for the StartMedicalTranscriptionJob operation.
--
-- /See:/ 'newMedicalTranscriptionSetting' smart constructor.
data MedicalTranscriptionSetting = MedicalTranscriptionSetting'
  { -- | The name of the vocabulary to use when processing a medical
    -- transcription job.
    vocabularyName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of alternatives that you tell the service to return.
    -- If you specify the @MaxAlternatives@ field, you must set the
    -- @ShowAlternatives@ field to true.
    maxAlternatives :: Prelude.Maybe Prelude.Natural,
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
    channelIdentification :: Prelude.Maybe Prelude.Bool,
    -- | Determines whether alternative transcripts are generated along with the
    -- transcript that has the highest confidence. If you set
    -- @ShowAlternatives@ field to true, you must also set the maximum number
    -- of alternatives to return in the @MaxAlternatives@ field.
    showAlternatives :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of speakers to identify in the input audio. If there
    -- are more speakers in the audio than this number, multiple speakers are
    -- identified as a single speaker. If you specify the @MaxSpeakerLabels@
    -- field, you must set the @ShowSpeakerLabels@ field to true.
    maxSpeakerLabels :: Prelude.Maybe Prelude.Natural,
    -- | Determines whether the transcription job uses speaker recognition to
    -- identify different speakers in the input audio. Speaker recognition
    -- labels individual speakers in the audio file. If you set the
    -- @ShowSpeakerLabels@ field to true, you must also set the maximum number
    -- of speaker labels in the @MaxSpeakerLabels@ field.
    --
    -- You can\'t set both @ShowSpeakerLabels@ and @ChannelIdentification@ in
    -- the same request. If you set both, your request returns a
    -- @BadRequestException@.
    showSpeakerLabels :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MedicalTranscriptionSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vocabularyName', 'medicalTranscriptionSetting_vocabularyName' - The name of the vocabulary to use when processing a medical
-- transcription job.
--
-- 'maxAlternatives', 'medicalTranscriptionSetting_maxAlternatives' - The maximum number of alternatives that you tell the service to return.
-- If you specify the @MaxAlternatives@ field, you must set the
-- @ShowAlternatives@ field to true.
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
-- 'showAlternatives', 'medicalTranscriptionSetting_showAlternatives' - Determines whether alternative transcripts are generated along with the
-- transcript that has the highest confidence. If you set
-- @ShowAlternatives@ field to true, you must also set the maximum number
-- of alternatives to return in the @MaxAlternatives@ field.
--
-- 'maxSpeakerLabels', 'medicalTranscriptionSetting_maxSpeakerLabels' - The maximum number of speakers to identify in the input audio. If there
-- are more speakers in the audio than this number, multiple speakers are
-- identified as a single speaker. If you specify the @MaxSpeakerLabels@
-- field, you must set the @ShowSpeakerLabels@ field to true.
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
newMedicalTranscriptionSetting ::
  MedicalTranscriptionSetting
newMedicalTranscriptionSetting =
  MedicalTranscriptionSetting'
    { vocabularyName =
        Prelude.Nothing,
      maxAlternatives = Prelude.Nothing,
      channelIdentification = Prelude.Nothing,
      showAlternatives = Prelude.Nothing,
      maxSpeakerLabels = Prelude.Nothing,
      showSpeakerLabels = Prelude.Nothing
    }

-- | The name of the vocabulary to use when processing a medical
-- transcription job.
medicalTranscriptionSetting_vocabularyName :: Lens.Lens' MedicalTranscriptionSetting (Prelude.Maybe Prelude.Text)
medicalTranscriptionSetting_vocabularyName = Lens.lens (\MedicalTranscriptionSetting' {vocabularyName} -> vocabularyName) (\s@MedicalTranscriptionSetting' {} a -> s {vocabularyName = a} :: MedicalTranscriptionSetting)

-- | The maximum number of alternatives that you tell the service to return.
-- If you specify the @MaxAlternatives@ field, you must set the
-- @ShowAlternatives@ field to true.
medicalTranscriptionSetting_maxAlternatives :: Lens.Lens' MedicalTranscriptionSetting (Prelude.Maybe Prelude.Natural)
medicalTranscriptionSetting_maxAlternatives = Lens.lens (\MedicalTranscriptionSetting' {maxAlternatives} -> maxAlternatives) (\s@MedicalTranscriptionSetting' {} a -> s {maxAlternatives = a} :: MedicalTranscriptionSetting)

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
medicalTranscriptionSetting_channelIdentification :: Lens.Lens' MedicalTranscriptionSetting (Prelude.Maybe Prelude.Bool)
medicalTranscriptionSetting_channelIdentification = Lens.lens (\MedicalTranscriptionSetting' {channelIdentification} -> channelIdentification) (\s@MedicalTranscriptionSetting' {} a -> s {channelIdentification = a} :: MedicalTranscriptionSetting)

-- | Determines whether alternative transcripts are generated along with the
-- transcript that has the highest confidence. If you set
-- @ShowAlternatives@ field to true, you must also set the maximum number
-- of alternatives to return in the @MaxAlternatives@ field.
medicalTranscriptionSetting_showAlternatives :: Lens.Lens' MedicalTranscriptionSetting (Prelude.Maybe Prelude.Bool)
medicalTranscriptionSetting_showAlternatives = Lens.lens (\MedicalTranscriptionSetting' {showAlternatives} -> showAlternatives) (\s@MedicalTranscriptionSetting' {} a -> s {showAlternatives = a} :: MedicalTranscriptionSetting)

-- | The maximum number of speakers to identify in the input audio. If there
-- are more speakers in the audio than this number, multiple speakers are
-- identified as a single speaker. If you specify the @MaxSpeakerLabels@
-- field, you must set the @ShowSpeakerLabels@ field to true.
medicalTranscriptionSetting_maxSpeakerLabels :: Lens.Lens' MedicalTranscriptionSetting (Prelude.Maybe Prelude.Natural)
medicalTranscriptionSetting_maxSpeakerLabels = Lens.lens (\MedicalTranscriptionSetting' {maxSpeakerLabels} -> maxSpeakerLabels) (\s@MedicalTranscriptionSetting' {} a -> s {maxSpeakerLabels = a} :: MedicalTranscriptionSetting)

-- | Determines whether the transcription job uses speaker recognition to
-- identify different speakers in the input audio. Speaker recognition
-- labels individual speakers in the audio file. If you set the
-- @ShowSpeakerLabels@ field to true, you must also set the maximum number
-- of speaker labels in the @MaxSpeakerLabels@ field.
--
-- You can\'t set both @ShowSpeakerLabels@ and @ChannelIdentification@ in
-- the same request. If you set both, your request returns a
-- @BadRequestException@.
medicalTranscriptionSetting_showSpeakerLabels :: Lens.Lens' MedicalTranscriptionSetting (Prelude.Maybe Prelude.Bool)
medicalTranscriptionSetting_showSpeakerLabels = Lens.lens (\MedicalTranscriptionSetting' {showSpeakerLabels} -> showSpeakerLabels) (\s@MedicalTranscriptionSetting' {} a -> s {showSpeakerLabels = a} :: MedicalTranscriptionSetting)

instance Core.FromJSON MedicalTranscriptionSetting where
  parseJSON =
    Core.withObject
      "MedicalTranscriptionSetting"
      ( \x ->
          MedicalTranscriptionSetting'
            Prelude.<$> (x Core..:? "VocabularyName")
            Prelude.<*> (x Core..:? "MaxAlternatives")
            Prelude.<*> (x Core..:? "ChannelIdentification")
            Prelude.<*> (x Core..:? "ShowAlternatives")
            Prelude.<*> (x Core..:? "MaxSpeakerLabels")
            Prelude.<*> (x Core..:? "ShowSpeakerLabels")
      )

instance Prelude.Hashable MedicalTranscriptionSetting where
  hashWithSalt _salt MedicalTranscriptionSetting' {..} =
    _salt `Prelude.hashWithSalt` vocabularyName
      `Prelude.hashWithSalt` maxAlternatives
      `Prelude.hashWithSalt` channelIdentification
      `Prelude.hashWithSalt` showAlternatives
      `Prelude.hashWithSalt` maxSpeakerLabels
      `Prelude.hashWithSalt` showSpeakerLabels

instance Prelude.NFData MedicalTranscriptionSetting where
  rnf MedicalTranscriptionSetting' {..} =
    Prelude.rnf vocabularyName
      `Prelude.seq` Prelude.rnf maxAlternatives
      `Prelude.seq` Prelude.rnf channelIdentification
      `Prelude.seq` Prelude.rnf showAlternatives
      `Prelude.seq` Prelude.rnf maxSpeakerLabels
      `Prelude.seq` Prelude.rnf showSpeakerLabels

instance Core.ToJSON MedicalTranscriptionSetting where
  toJSON MedicalTranscriptionSetting' {..} =
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
            ("ShowSpeakerLabels" Core..=)
              Prelude.<$> showSpeakerLabels
          ]
      )
