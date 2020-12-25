{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    mtsChannelIdentification,
    mtsMaxAlternatives,
    mtsMaxSpeakerLabels,
    mtsShowAlternatives,
    mtsShowSpeakerLabels,
    mtsVocabularyName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Transcribe.Types.VocabularyName as Types

-- | Optional settings for the 'StartMedicalTranscriptionJob' operation.
--
-- /See:/ 'mkMedicalTranscriptionSetting' smart constructor.
data MedicalTranscriptionSetting = MedicalTranscriptionSetting'
  { -- | Instructs Amazon Transcribe Medical to process each audio channel separately and then merge the transcription output of each channel into a single transcription.
    --
    -- Amazon Transcribe Medical also produces a transcription of each item detected on an audio channel, including the start time and end time of the item and alternative transcriptions of item. The alternative transcriptions also come with confidence scores provided by Amazon Transcribe Medical.
    -- You can't set both @ShowSpeakerLabels@ and @ChannelIdentification@ in the same request. If you set both, your request returns a @BadRequestException@
    channelIdentification :: Core.Maybe Core.Bool,
    -- | The maximum number of alternatives that you tell the service to return. If you specify the @MaxAlternatives@ field, you must set the @ShowAlternatives@ field to true.
    maxAlternatives :: Core.Maybe Core.Natural,
    -- | The maximum number of speakers to identify in the input audio. If there are more speakers in the audio than this number, multiple speakers are identified as a single speaker. If you specify the @MaxSpeakerLabels@ field, you must set the @ShowSpeakerLabels@ field to true.
    maxSpeakerLabels :: Core.Maybe Core.Natural,
    -- | Determines whether alternative transcripts are generated along with the transcript that has the highest confidence. If you set @ShowAlternatives@ field to true, you must also set the maximum number of alternatives to return in the @MaxAlternatives@ field.
    showAlternatives :: Core.Maybe Core.Bool,
    -- | Determines whether the transcription job uses speaker recognition to identify different speakers in the input audio. Speaker recognition labels individual speakers in the audio file. If you set the @ShowSpeakerLabels@ field to true, you must also set the maximum number of speaker labels in the @MaxSpeakerLabels@ field.
    --
    -- You can't set both @ShowSpeakerLabels@ and @ChannelIdentification@ in the same request. If you set both, your request returns a @BadRequestException@ .
    showSpeakerLabels :: Core.Maybe Core.Bool,
    -- | The name of the vocabulary to use when processing a medical transcription job.
    vocabularyName :: Core.Maybe Types.VocabularyName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MedicalTranscriptionSetting' value with any optional fields omitted.
mkMedicalTranscriptionSetting ::
  MedicalTranscriptionSetting
mkMedicalTranscriptionSetting =
  MedicalTranscriptionSetting'
    { channelIdentification =
        Core.Nothing,
      maxAlternatives = Core.Nothing,
      maxSpeakerLabels = Core.Nothing,
      showAlternatives = Core.Nothing,
      showSpeakerLabels = Core.Nothing,
      vocabularyName = Core.Nothing
    }

-- | Instructs Amazon Transcribe Medical to process each audio channel separately and then merge the transcription output of each channel into a single transcription.
--
-- Amazon Transcribe Medical also produces a transcription of each item detected on an audio channel, including the start time and end time of the item and alternative transcriptions of item. The alternative transcriptions also come with confidence scores provided by Amazon Transcribe Medical.
-- You can't set both @ShowSpeakerLabels@ and @ChannelIdentification@ in the same request. If you set both, your request returns a @BadRequestException@
--
-- /Note:/ Consider using 'channelIdentification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtsChannelIdentification :: Lens.Lens' MedicalTranscriptionSetting (Core.Maybe Core.Bool)
mtsChannelIdentification = Lens.field @"channelIdentification"
{-# DEPRECATED mtsChannelIdentification "Use generic-lens or generic-optics with 'channelIdentification' instead." #-}

-- | The maximum number of alternatives that you tell the service to return. If you specify the @MaxAlternatives@ field, you must set the @ShowAlternatives@ field to true.
--
-- /Note:/ Consider using 'maxAlternatives' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtsMaxAlternatives :: Lens.Lens' MedicalTranscriptionSetting (Core.Maybe Core.Natural)
mtsMaxAlternatives = Lens.field @"maxAlternatives"
{-# DEPRECATED mtsMaxAlternatives "Use generic-lens or generic-optics with 'maxAlternatives' instead." #-}

-- | The maximum number of speakers to identify in the input audio. If there are more speakers in the audio than this number, multiple speakers are identified as a single speaker. If you specify the @MaxSpeakerLabels@ field, you must set the @ShowSpeakerLabels@ field to true.
--
-- /Note:/ Consider using 'maxSpeakerLabels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtsMaxSpeakerLabels :: Lens.Lens' MedicalTranscriptionSetting (Core.Maybe Core.Natural)
mtsMaxSpeakerLabels = Lens.field @"maxSpeakerLabels"
{-# DEPRECATED mtsMaxSpeakerLabels "Use generic-lens or generic-optics with 'maxSpeakerLabels' instead." #-}

-- | Determines whether alternative transcripts are generated along with the transcript that has the highest confidence. If you set @ShowAlternatives@ field to true, you must also set the maximum number of alternatives to return in the @MaxAlternatives@ field.
--
-- /Note:/ Consider using 'showAlternatives' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtsShowAlternatives :: Lens.Lens' MedicalTranscriptionSetting (Core.Maybe Core.Bool)
mtsShowAlternatives = Lens.field @"showAlternatives"
{-# DEPRECATED mtsShowAlternatives "Use generic-lens or generic-optics with 'showAlternatives' instead." #-}

-- | Determines whether the transcription job uses speaker recognition to identify different speakers in the input audio. Speaker recognition labels individual speakers in the audio file. If you set the @ShowSpeakerLabels@ field to true, you must also set the maximum number of speaker labels in the @MaxSpeakerLabels@ field.
--
-- You can't set both @ShowSpeakerLabels@ and @ChannelIdentification@ in the same request. If you set both, your request returns a @BadRequestException@ .
--
-- /Note:/ Consider using 'showSpeakerLabels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtsShowSpeakerLabels :: Lens.Lens' MedicalTranscriptionSetting (Core.Maybe Core.Bool)
mtsShowSpeakerLabels = Lens.field @"showSpeakerLabels"
{-# DEPRECATED mtsShowSpeakerLabels "Use generic-lens or generic-optics with 'showSpeakerLabels' instead." #-}

-- | The name of the vocabulary to use when processing a medical transcription job.
--
-- /Note:/ Consider using 'vocabularyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtsVocabularyName :: Lens.Lens' MedicalTranscriptionSetting (Core.Maybe Types.VocabularyName)
mtsVocabularyName = Lens.field @"vocabularyName"
{-# DEPRECATED mtsVocabularyName "Use generic-lens or generic-optics with 'vocabularyName' instead." #-}

instance Core.FromJSON MedicalTranscriptionSetting where
  toJSON MedicalTranscriptionSetting {..} =
    Core.object
      ( Core.catMaybes
          [ ("ChannelIdentification" Core..=) Core.<$> channelIdentification,
            ("MaxAlternatives" Core..=) Core.<$> maxAlternatives,
            ("MaxSpeakerLabels" Core..=) Core.<$> maxSpeakerLabels,
            ("ShowAlternatives" Core..=) Core.<$> showAlternatives,
            ("ShowSpeakerLabels" Core..=) Core.<$> showSpeakerLabels,
            ("VocabularyName" Core..=) Core.<$> vocabularyName
          ]
      )

instance Core.FromJSON MedicalTranscriptionSetting where
  parseJSON =
    Core.withObject "MedicalTranscriptionSetting" Core.$
      \x ->
        MedicalTranscriptionSetting'
          Core.<$> (x Core..:? "ChannelIdentification")
          Core.<*> (x Core..:? "MaxAlternatives")
          Core.<*> (x Core..:? "MaxSpeakerLabels")
          Core.<*> (x Core..:? "ShowAlternatives")
          Core.<*> (x Core..:? "ShowSpeakerLabels")
          Core.<*> (x Core..:? "VocabularyName")
