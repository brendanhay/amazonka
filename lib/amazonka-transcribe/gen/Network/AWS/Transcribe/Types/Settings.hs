{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.Settings
  ( Settings (..),

    -- * Smart constructor
    mkSettings,

    -- * Lenses
    sVocabularyName,
    sMaxAlternatives,
    sChannelIdentification,
    sShowAlternatives,
    sMaxSpeakerLabels,
    sVocabularyFilterName,
    sShowSpeakerLabels,
    sVocabularyFilterMethod,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Transcribe.Types.VocabularyFilterMethod

-- | Provides optional settings for the @StartTranscriptionJob@ operation.
--
-- /See:/ 'mkSettings' smart constructor.
data Settings = Settings'
  { vocabularyName :: Lude.Maybe Lude.Text,
    maxAlternatives :: Lude.Maybe Lude.Natural,
    channelIdentification :: Lude.Maybe Lude.Bool,
    showAlternatives :: Lude.Maybe Lude.Bool,
    maxSpeakerLabels :: Lude.Maybe Lude.Natural,
    vocabularyFilterName :: Lude.Maybe Lude.Text,
    showSpeakerLabels :: Lude.Maybe Lude.Bool,
    vocabularyFilterMethod :: Lude.Maybe VocabularyFilterMethod
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Settings' with the minimum fields required to make a request.
--
-- * 'channelIdentification' - Instructs Amazon Transcribe to process each audio channel separately and then merge the transcription output of each channel into a single transcription.
--
-- Amazon Transcribe also produces a transcription of each item detected on an audio channel, including the start time and end time of the item and alternative transcriptions of the item including the confidence that Amazon Transcribe has in the transcription.
-- You can't set both @ShowSpeakerLabels@ and @ChannelIdentification@ in the same request. If you set both, your request returns a @BadRequestException@ .
-- * 'maxAlternatives' - The number of alternative transcriptions that the service should return. If you specify the @MaxAlternatives@ field, you must set the @ShowAlternatives@ field to true.
-- * 'maxSpeakerLabels' - The maximum number of speakers to identify in the input audio. If there are more speakers in the audio than this number, multiple speakers are identified as a single speaker. If you specify the @MaxSpeakerLabels@ field, you must set the @ShowSpeakerLabels@ field to true.
-- * 'showAlternatives' - Determines whether the transcription contains alternative transcriptions. If you set the @ShowAlternatives@ field to true, you must also set the maximum number of alternatives to return in the @MaxAlternatives@ field.
-- * 'showSpeakerLabels' - Determines whether the transcription job uses speaker recognition to identify different speakers in the input audio. Speaker recognition labels individual speakers in the audio file. If you set the @ShowSpeakerLabels@ field to true, you must also set the maximum number of speaker labels @MaxSpeakerLabels@ field.
--
-- You can't set both @ShowSpeakerLabels@ and @ChannelIdentification@ in the same request. If you set both, your request returns a @BadRequestException@ .
-- * 'vocabularyFilterMethod' - Set to @mask@ to remove filtered text from the transcript and replace it with three asterisks ("***") as placeholder text. Set to @remove@ to remove filtered text from the transcript without using placeholder text.
-- * 'vocabularyFilterName' - The name of the vocabulary filter to use when transcribing the audio. The filter that you specify must have the same language code as the transcription job.
-- * 'vocabularyName' - The name of a vocabulary to use when processing the transcription job.
mkSettings ::
  Settings
mkSettings =
  Settings'
    { vocabularyName = Lude.Nothing,
      maxAlternatives = Lude.Nothing,
      channelIdentification = Lude.Nothing,
      showAlternatives = Lude.Nothing,
      maxSpeakerLabels = Lude.Nothing,
      vocabularyFilterName = Lude.Nothing,
      showSpeakerLabels = Lude.Nothing,
      vocabularyFilterMethod = Lude.Nothing
    }

-- | The name of a vocabulary to use when processing the transcription job.
--
-- /Note:/ Consider using 'vocabularyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sVocabularyName :: Lens.Lens' Settings (Lude.Maybe Lude.Text)
sVocabularyName = Lens.lens (vocabularyName :: Settings -> Lude.Maybe Lude.Text) (\s a -> s {vocabularyName = a} :: Settings)
{-# DEPRECATED sVocabularyName "Use generic-lens or generic-optics with 'vocabularyName' instead." #-}

-- | The number of alternative transcriptions that the service should return. If you specify the @MaxAlternatives@ field, you must set the @ShowAlternatives@ field to true.
--
-- /Note:/ Consider using 'maxAlternatives' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMaxAlternatives :: Lens.Lens' Settings (Lude.Maybe Lude.Natural)
sMaxAlternatives = Lens.lens (maxAlternatives :: Settings -> Lude.Maybe Lude.Natural) (\s a -> s {maxAlternatives = a} :: Settings)
{-# DEPRECATED sMaxAlternatives "Use generic-lens or generic-optics with 'maxAlternatives' instead." #-}

-- | Instructs Amazon Transcribe to process each audio channel separately and then merge the transcription output of each channel into a single transcription.
--
-- Amazon Transcribe also produces a transcription of each item detected on an audio channel, including the start time and end time of the item and alternative transcriptions of the item including the confidence that Amazon Transcribe has in the transcription.
-- You can't set both @ShowSpeakerLabels@ and @ChannelIdentification@ in the same request. If you set both, your request returns a @BadRequestException@ .
--
-- /Note:/ Consider using 'channelIdentification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sChannelIdentification :: Lens.Lens' Settings (Lude.Maybe Lude.Bool)
sChannelIdentification = Lens.lens (channelIdentification :: Settings -> Lude.Maybe Lude.Bool) (\s a -> s {channelIdentification = a} :: Settings)
{-# DEPRECATED sChannelIdentification "Use generic-lens or generic-optics with 'channelIdentification' instead." #-}

-- | Determines whether the transcription contains alternative transcriptions. If you set the @ShowAlternatives@ field to true, you must also set the maximum number of alternatives to return in the @MaxAlternatives@ field.
--
-- /Note:/ Consider using 'showAlternatives' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sShowAlternatives :: Lens.Lens' Settings (Lude.Maybe Lude.Bool)
sShowAlternatives = Lens.lens (showAlternatives :: Settings -> Lude.Maybe Lude.Bool) (\s a -> s {showAlternatives = a} :: Settings)
{-# DEPRECATED sShowAlternatives "Use generic-lens or generic-optics with 'showAlternatives' instead." #-}

-- | The maximum number of speakers to identify in the input audio. If there are more speakers in the audio than this number, multiple speakers are identified as a single speaker. If you specify the @MaxSpeakerLabels@ field, you must set the @ShowSpeakerLabels@ field to true.
--
-- /Note:/ Consider using 'maxSpeakerLabels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMaxSpeakerLabels :: Lens.Lens' Settings (Lude.Maybe Lude.Natural)
sMaxSpeakerLabels = Lens.lens (maxSpeakerLabels :: Settings -> Lude.Maybe Lude.Natural) (\s a -> s {maxSpeakerLabels = a} :: Settings)
{-# DEPRECATED sMaxSpeakerLabels "Use generic-lens or generic-optics with 'maxSpeakerLabels' instead." #-}

-- | The name of the vocabulary filter to use when transcribing the audio. The filter that you specify must have the same language code as the transcription job.
--
-- /Note:/ Consider using 'vocabularyFilterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sVocabularyFilterName :: Lens.Lens' Settings (Lude.Maybe Lude.Text)
sVocabularyFilterName = Lens.lens (vocabularyFilterName :: Settings -> Lude.Maybe Lude.Text) (\s a -> s {vocabularyFilterName = a} :: Settings)
{-# DEPRECATED sVocabularyFilterName "Use generic-lens or generic-optics with 'vocabularyFilterName' instead." #-}

-- | Determines whether the transcription job uses speaker recognition to identify different speakers in the input audio. Speaker recognition labels individual speakers in the audio file. If you set the @ShowSpeakerLabels@ field to true, you must also set the maximum number of speaker labels @MaxSpeakerLabels@ field.
--
-- You can't set both @ShowSpeakerLabels@ and @ChannelIdentification@ in the same request. If you set both, your request returns a @BadRequestException@ .
--
-- /Note:/ Consider using 'showSpeakerLabels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sShowSpeakerLabels :: Lens.Lens' Settings (Lude.Maybe Lude.Bool)
sShowSpeakerLabels = Lens.lens (showSpeakerLabels :: Settings -> Lude.Maybe Lude.Bool) (\s a -> s {showSpeakerLabels = a} :: Settings)
{-# DEPRECATED sShowSpeakerLabels "Use generic-lens or generic-optics with 'showSpeakerLabels' instead." #-}

-- | Set to @mask@ to remove filtered text from the transcript and replace it with three asterisks ("***") as placeholder text. Set to @remove@ to remove filtered text from the transcript without using placeholder text.
--
-- /Note:/ Consider using 'vocabularyFilterMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sVocabularyFilterMethod :: Lens.Lens' Settings (Lude.Maybe VocabularyFilterMethod)
sVocabularyFilterMethod = Lens.lens (vocabularyFilterMethod :: Settings -> Lude.Maybe VocabularyFilterMethod) (\s a -> s {vocabularyFilterMethod = a} :: Settings)
{-# DEPRECATED sVocabularyFilterMethod "Use generic-lens or generic-optics with 'vocabularyFilterMethod' instead." #-}

instance Lude.FromJSON Settings where
  parseJSON =
    Lude.withObject
      "Settings"
      ( \x ->
          Settings'
            Lude.<$> (x Lude..:? "VocabularyName")
            Lude.<*> (x Lude..:? "MaxAlternatives")
            Lude.<*> (x Lude..:? "ChannelIdentification")
            Lude.<*> (x Lude..:? "ShowAlternatives")
            Lude.<*> (x Lude..:? "MaxSpeakerLabels")
            Lude.<*> (x Lude..:? "VocabularyFilterName")
            Lude.<*> (x Lude..:? "ShowSpeakerLabels")
            Lude.<*> (x Lude..:? "VocabularyFilterMethod")
      )

instance Lude.ToJSON Settings where
  toJSON Settings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("VocabularyName" Lude..=) Lude.<$> vocabularyName,
            ("MaxAlternatives" Lude..=) Lude.<$> maxAlternatives,
            ("ChannelIdentification" Lude..=) Lude.<$> channelIdentification,
            ("ShowAlternatives" Lude..=) Lude.<$> showAlternatives,
            ("MaxSpeakerLabels" Lude..=) Lude.<$> maxSpeakerLabels,
            ("VocabularyFilterName" Lude..=) Lude.<$> vocabularyFilterName,
            ("ShowSpeakerLabels" Lude..=) Lude.<$> showSpeakerLabels,
            ("VocabularyFilterMethod" Lude..=)
              Lude.<$> vocabularyFilterMethod
          ]
      )
