-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.MedicalTranscriptionSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.MedicalTranscriptionSetting
  ( MedicalTranscriptionSetting (..),

    -- * Smart constructor
    mkMedicalTranscriptionSetting,

    -- * Lenses
    mtsVocabularyName,
    mtsMaxAlternatives,
    mtsChannelIdentification,
    mtsShowAlternatives,
    mtsMaxSpeakerLabels,
    mtsShowSpeakerLabels,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Optional settings for the 'StartMedicalTranscriptionJob' operation.
--
-- /See:/ 'mkMedicalTranscriptionSetting' smart constructor.
data MedicalTranscriptionSetting = MedicalTranscriptionSetting'
  { vocabularyName ::
      Lude.Maybe Lude.Text,
    maxAlternatives ::
      Lude.Maybe Lude.Natural,
    channelIdentification ::
      Lude.Maybe Lude.Bool,
    showAlternatives ::
      Lude.Maybe Lude.Bool,
    maxSpeakerLabels ::
      Lude.Maybe Lude.Natural,
    showSpeakerLabels ::
      Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MedicalTranscriptionSetting' with the minimum fields required to make a request.
--
-- * 'channelIdentification' - Instructs Amazon Transcribe Medical to process each audio channel separately and then merge the transcription output of each channel into a single transcription.
--
-- Amazon Transcribe Medical also produces a transcription of each item detected on an audio channel, including the start time and end time of the item and alternative transcriptions of item. The alternative transcriptions also come with confidence scores provided by Amazon Transcribe Medical.
-- You can't set both @ShowSpeakerLabels@ and @ChannelIdentification@ in the same request. If you set both, your request returns a @BadRequestException@
-- * 'maxAlternatives' - The maximum number of alternatives that you tell the service to return. If you specify the @MaxAlternatives@ field, you must set the @ShowAlternatives@ field to true.
-- * 'maxSpeakerLabels' - The maximum number of speakers to identify in the input audio. If there are more speakers in the audio than this number, multiple speakers are identified as a single speaker. If you specify the @MaxSpeakerLabels@ field, you must set the @ShowSpeakerLabels@ field to true.
-- * 'showAlternatives' - Determines whether alternative transcripts are generated along with the transcript that has the highest confidence. If you set @ShowAlternatives@ field to true, you must also set the maximum number of alternatives to return in the @MaxAlternatives@ field.
-- * 'showSpeakerLabels' - Determines whether the transcription job uses speaker recognition to identify different speakers in the input audio. Speaker recognition labels individual speakers in the audio file. If you set the @ShowSpeakerLabels@ field to true, you must also set the maximum number of speaker labels in the @MaxSpeakerLabels@ field.
--
-- You can't set both @ShowSpeakerLabels@ and @ChannelIdentification@ in the same request. If you set both, your request returns a @BadRequestException@ .
-- * 'vocabularyName' - The name of the vocabulary to use when processing a medical transcription job.
mkMedicalTranscriptionSetting ::
  MedicalTranscriptionSetting
mkMedicalTranscriptionSetting =
  MedicalTranscriptionSetting'
    { vocabularyName = Lude.Nothing,
      maxAlternatives = Lude.Nothing,
      channelIdentification = Lude.Nothing,
      showAlternatives = Lude.Nothing,
      maxSpeakerLabels = Lude.Nothing,
      showSpeakerLabels = Lude.Nothing
    }

-- | The name of the vocabulary to use when processing a medical transcription job.
--
-- /Note:/ Consider using 'vocabularyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtsVocabularyName :: Lens.Lens' MedicalTranscriptionSetting (Lude.Maybe Lude.Text)
mtsVocabularyName = Lens.lens (vocabularyName :: MedicalTranscriptionSetting -> Lude.Maybe Lude.Text) (\s a -> s {vocabularyName = a} :: MedicalTranscriptionSetting)
{-# DEPRECATED mtsVocabularyName "Use generic-lens or generic-optics with 'vocabularyName' instead." #-}

-- | The maximum number of alternatives that you tell the service to return. If you specify the @MaxAlternatives@ field, you must set the @ShowAlternatives@ field to true.
--
-- /Note:/ Consider using 'maxAlternatives' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtsMaxAlternatives :: Lens.Lens' MedicalTranscriptionSetting (Lude.Maybe Lude.Natural)
mtsMaxAlternatives = Lens.lens (maxAlternatives :: MedicalTranscriptionSetting -> Lude.Maybe Lude.Natural) (\s a -> s {maxAlternatives = a} :: MedicalTranscriptionSetting)
{-# DEPRECATED mtsMaxAlternatives "Use generic-lens or generic-optics with 'maxAlternatives' instead." #-}

-- | Instructs Amazon Transcribe Medical to process each audio channel separately and then merge the transcription output of each channel into a single transcription.
--
-- Amazon Transcribe Medical also produces a transcription of each item detected on an audio channel, including the start time and end time of the item and alternative transcriptions of item. The alternative transcriptions also come with confidence scores provided by Amazon Transcribe Medical.
-- You can't set both @ShowSpeakerLabels@ and @ChannelIdentification@ in the same request. If you set both, your request returns a @BadRequestException@
--
-- /Note:/ Consider using 'channelIdentification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtsChannelIdentification :: Lens.Lens' MedicalTranscriptionSetting (Lude.Maybe Lude.Bool)
mtsChannelIdentification = Lens.lens (channelIdentification :: MedicalTranscriptionSetting -> Lude.Maybe Lude.Bool) (\s a -> s {channelIdentification = a} :: MedicalTranscriptionSetting)
{-# DEPRECATED mtsChannelIdentification "Use generic-lens or generic-optics with 'channelIdentification' instead." #-}

-- | Determines whether alternative transcripts are generated along with the transcript that has the highest confidence. If you set @ShowAlternatives@ field to true, you must also set the maximum number of alternatives to return in the @MaxAlternatives@ field.
--
-- /Note:/ Consider using 'showAlternatives' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtsShowAlternatives :: Lens.Lens' MedicalTranscriptionSetting (Lude.Maybe Lude.Bool)
mtsShowAlternatives = Lens.lens (showAlternatives :: MedicalTranscriptionSetting -> Lude.Maybe Lude.Bool) (\s a -> s {showAlternatives = a} :: MedicalTranscriptionSetting)
{-# DEPRECATED mtsShowAlternatives "Use generic-lens or generic-optics with 'showAlternatives' instead." #-}

-- | The maximum number of speakers to identify in the input audio. If there are more speakers in the audio than this number, multiple speakers are identified as a single speaker. If you specify the @MaxSpeakerLabels@ field, you must set the @ShowSpeakerLabels@ field to true.
--
-- /Note:/ Consider using 'maxSpeakerLabels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtsMaxSpeakerLabels :: Lens.Lens' MedicalTranscriptionSetting (Lude.Maybe Lude.Natural)
mtsMaxSpeakerLabels = Lens.lens (maxSpeakerLabels :: MedicalTranscriptionSetting -> Lude.Maybe Lude.Natural) (\s a -> s {maxSpeakerLabels = a} :: MedicalTranscriptionSetting)
{-# DEPRECATED mtsMaxSpeakerLabels "Use generic-lens or generic-optics with 'maxSpeakerLabels' instead." #-}

-- | Determines whether the transcription job uses speaker recognition to identify different speakers in the input audio. Speaker recognition labels individual speakers in the audio file. If you set the @ShowSpeakerLabels@ field to true, you must also set the maximum number of speaker labels in the @MaxSpeakerLabels@ field.
--
-- You can't set both @ShowSpeakerLabels@ and @ChannelIdentification@ in the same request. If you set both, your request returns a @BadRequestException@ .
--
-- /Note:/ Consider using 'showSpeakerLabels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtsShowSpeakerLabels :: Lens.Lens' MedicalTranscriptionSetting (Lude.Maybe Lude.Bool)
mtsShowSpeakerLabels = Lens.lens (showSpeakerLabels :: MedicalTranscriptionSetting -> Lude.Maybe Lude.Bool) (\s a -> s {showSpeakerLabels = a} :: MedicalTranscriptionSetting)
{-# DEPRECATED mtsShowSpeakerLabels "Use generic-lens or generic-optics with 'showSpeakerLabels' instead." #-}

instance Lude.FromJSON MedicalTranscriptionSetting where
  parseJSON =
    Lude.withObject
      "MedicalTranscriptionSetting"
      ( \x ->
          MedicalTranscriptionSetting'
            Lude.<$> (x Lude..:? "VocabularyName")
            Lude.<*> (x Lude..:? "MaxAlternatives")
            Lude.<*> (x Lude..:? "ChannelIdentification")
            Lude.<*> (x Lude..:? "ShowAlternatives")
            Lude.<*> (x Lude..:? "MaxSpeakerLabels")
            Lude.<*> (x Lude..:? "ShowSpeakerLabels")
      )

instance Lude.ToJSON MedicalTranscriptionSetting where
  toJSON MedicalTranscriptionSetting' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("VocabularyName" Lude..=) Lude.<$> vocabularyName,
            ("MaxAlternatives" Lude..=) Lude.<$> maxAlternatives,
            ("ChannelIdentification" Lude..=) Lude.<$> channelIdentification,
            ("ShowAlternatives" Lude..=) Lude.<$> showAlternatives,
            ("MaxSpeakerLabels" Lude..=) Lude.<$> maxSpeakerLabels,
            ("ShowSpeakerLabels" Lude..=) Lude.<$> showSpeakerLabels
          ]
      )
