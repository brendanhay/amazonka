{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Transcribe.Types.Settings
  ( Settings (..)
  -- * Smart constructor
  , mkSettings
  -- * Lenses
  , sChannelIdentification
  , sMaxAlternatives
  , sMaxSpeakerLabels
  , sShowAlternatives
  , sShowSpeakerLabels
  , sVocabularyFilterMethod
  , sVocabularyFilterName
  , sVocabularyName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Transcribe.Types.VocabularyFilterMethod as Types
import qualified Network.AWS.Transcribe.Types.VocabularyFilterName as Types
import qualified Network.AWS.Transcribe.Types.VocabularyName as Types

-- | Provides optional settings for the @StartTranscriptionJob@ operation.
--
-- /See:/ 'mkSettings' smart constructor.
data Settings = Settings'
  { channelIdentification :: Core.Maybe Core.Bool
    -- ^ Instructs Amazon Transcribe to process each audio channel separately and then merge the transcription output of each channel into a single transcription. 
--
-- Amazon Transcribe also produces a transcription of each item detected on an audio channel, including the start time and end time of the item and alternative transcriptions of the item including the confidence that Amazon Transcribe has in the transcription.
-- You can't set both @ShowSpeakerLabels@ and @ChannelIdentification@ in the same request. If you set both, your request returns a @BadRequestException@ .
  , maxAlternatives :: Core.Maybe Core.Natural
    -- ^ The number of alternative transcriptions that the service should return. If you specify the @MaxAlternatives@ field, you must set the @ShowAlternatives@ field to true.
  , maxSpeakerLabels :: Core.Maybe Core.Natural
    -- ^ The maximum number of speakers to identify in the input audio. If there are more speakers in the audio than this number, multiple speakers are identified as a single speaker. If you specify the @MaxSpeakerLabels@ field, you must set the @ShowSpeakerLabels@ field to true.
  , showAlternatives :: Core.Maybe Core.Bool
    -- ^ Determines whether the transcription contains alternative transcriptions. If you set the @ShowAlternatives@ field to true, you must also set the maximum number of alternatives to return in the @MaxAlternatives@ field.
  , showSpeakerLabels :: Core.Maybe Core.Bool
    -- ^ Determines whether the transcription job uses speaker recognition to identify different speakers in the input audio. Speaker recognition labels individual speakers in the audio file. If you set the @ShowSpeakerLabels@ field to true, you must also set the maximum number of speaker labels @MaxSpeakerLabels@ field.
--
-- You can't set both @ShowSpeakerLabels@ and @ChannelIdentification@ in the same request. If you set both, your request returns a @BadRequestException@ .
  , vocabularyFilterMethod :: Core.Maybe Types.VocabularyFilterMethod
    -- ^ Set to @mask@ to remove filtered text from the transcript and replace it with three asterisks ("***") as placeholder text. Set to @remove@ to remove filtered text from the transcript without using placeholder text.
  , vocabularyFilterName :: Core.Maybe Types.VocabularyFilterName
    -- ^ The name of the vocabulary filter to use when transcribing the audio. The filter that you specify must have the same language code as the transcription job.
  , vocabularyName :: Core.Maybe Types.VocabularyName
    -- ^ The name of a vocabulary to use when processing the transcription job.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Settings' value with any optional fields omitted.
mkSettings
    :: Settings
mkSettings
  = Settings'{channelIdentification = Core.Nothing,
              maxAlternatives = Core.Nothing, maxSpeakerLabels = Core.Nothing,
              showAlternatives = Core.Nothing, showSpeakerLabels = Core.Nothing,
              vocabularyFilterMethod = Core.Nothing,
              vocabularyFilterName = Core.Nothing, vocabularyName = Core.Nothing}

-- | Instructs Amazon Transcribe to process each audio channel separately and then merge the transcription output of each channel into a single transcription. 
--
-- Amazon Transcribe also produces a transcription of each item detected on an audio channel, including the start time and end time of the item and alternative transcriptions of the item including the confidence that Amazon Transcribe has in the transcription.
-- You can't set both @ShowSpeakerLabels@ and @ChannelIdentification@ in the same request. If you set both, your request returns a @BadRequestException@ .
--
-- /Note:/ Consider using 'channelIdentification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sChannelIdentification :: Lens.Lens' Settings (Core.Maybe Core.Bool)
sChannelIdentification = Lens.field @"channelIdentification"
{-# INLINEABLE sChannelIdentification #-}
{-# DEPRECATED channelIdentification "Use generic-lens or generic-optics with 'channelIdentification' instead"  #-}

-- | The number of alternative transcriptions that the service should return. If you specify the @MaxAlternatives@ field, you must set the @ShowAlternatives@ field to true.
--
-- /Note:/ Consider using 'maxAlternatives' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMaxAlternatives :: Lens.Lens' Settings (Core.Maybe Core.Natural)
sMaxAlternatives = Lens.field @"maxAlternatives"
{-# INLINEABLE sMaxAlternatives #-}
{-# DEPRECATED maxAlternatives "Use generic-lens or generic-optics with 'maxAlternatives' instead"  #-}

-- | The maximum number of speakers to identify in the input audio. If there are more speakers in the audio than this number, multiple speakers are identified as a single speaker. If you specify the @MaxSpeakerLabels@ field, you must set the @ShowSpeakerLabels@ field to true.
--
-- /Note:/ Consider using 'maxSpeakerLabels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMaxSpeakerLabels :: Lens.Lens' Settings (Core.Maybe Core.Natural)
sMaxSpeakerLabels = Lens.field @"maxSpeakerLabels"
{-# INLINEABLE sMaxSpeakerLabels #-}
{-# DEPRECATED maxSpeakerLabels "Use generic-lens or generic-optics with 'maxSpeakerLabels' instead"  #-}

-- | Determines whether the transcription contains alternative transcriptions. If you set the @ShowAlternatives@ field to true, you must also set the maximum number of alternatives to return in the @MaxAlternatives@ field.
--
-- /Note:/ Consider using 'showAlternatives' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sShowAlternatives :: Lens.Lens' Settings (Core.Maybe Core.Bool)
sShowAlternatives = Lens.field @"showAlternatives"
{-# INLINEABLE sShowAlternatives #-}
{-# DEPRECATED showAlternatives "Use generic-lens or generic-optics with 'showAlternatives' instead"  #-}

-- | Determines whether the transcription job uses speaker recognition to identify different speakers in the input audio. Speaker recognition labels individual speakers in the audio file. If you set the @ShowSpeakerLabels@ field to true, you must also set the maximum number of speaker labels @MaxSpeakerLabels@ field.
--
-- You can't set both @ShowSpeakerLabels@ and @ChannelIdentification@ in the same request. If you set both, your request returns a @BadRequestException@ .
--
-- /Note:/ Consider using 'showSpeakerLabels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sShowSpeakerLabels :: Lens.Lens' Settings (Core.Maybe Core.Bool)
sShowSpeakerLabels = Lens.field @"showSpeakerLabels"
{-# INLINEABLE sShowSpeakerLabels #-}
{-# DEPRECATED showSpeakerLabels "Use generic-lens or generic-optics with 'showSpeakerLabels' instead"  #-}

-- | Set to @mask@ to remove filtered text from the transcript and replace it with three asterisks ("***") as placeholder text. Set to @remove@ to remove filtered text from the transcript without using placeholder text.
--
-- /Note:/ Consider using 'vocabularyFilterMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sVocabularyFilterMethod :: Lens.Lens' Settings (Core.Maybe Types.VocabularyFilterMethod)
sVocabularyFilterMethod = Lens.field @"vocabularyFilterMethod"
{-# INLINEABLE sVocabularyFilterMethod #-}
{-# DEPRECATED vocabularyFilterMethod "Use generic-lens or generic-optics with 'vocabularyFilterMethod' instead"  #-}

-- | The name of the vocabulary filter to use when transcribing the audio. The filter that you specify must have the same language code as the transcription job.
--
-- /Note:/ Consider using 'vocabularyFilterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sVocabularyFilterName :: Lens.Lens' Settings (Core.Maybe Types.VocabularyFilterName)
sVocabularyFilterName = Lens.field @"vocabularyFilterName"
{-# INLINEABLE sVocabularyFilterName #-}
{-# DEPRECATED vocabularyFilterName "Use generic-lens or generic-optics with 'vocabularyFilterName' instead"  #-}

-- | The name of a vocabulary to use when processing the transcription job.
--
-- /Note:/ Consider using 'vocabularyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sVocabularyName :: Lens.Lens' Settings (Core.Maybe Types.VocabularyName)
sVocabularyName = Lens.field @"vocabularyName"
{-# INLINEABLE sVocabularyName #-}
{-# DEPRECATED vocabularyName "Use generic-lens or generic-optics with 'vocabularyName' instead"  #-}

instance Core.FromJSON Settings where
        toJSON Settings{..}
          = Core.object
              (Core.catMaybes
                 [("ChannelIdentification" Core..=) Core.<$> channelIdentification,
                  ("MaxAlternatives" Core..=) Core.<$> maxAlternatives,
                  ("MaxSpeakerLabels" Core..=) Core.<$> maxSpeakerLabels,
                  ("ShowAlternatives" Core..=) Core.<$> showAlternatives,
                  ("ShowSpeakerLabels" Core..=) Core.<$> showSpeakerLabels,
                  ("VocabularyFilterMethod" Core..=) Core.<$> vocabularyFilterMethod,
                  ("VocabularyFilterName" Core..=) Core.<$> vocabularyFilterName,
                  ("VocabularyName" Core..=) Core.<$> vocabularyName])

instance Core.FromJSON Settings where
        parseJSON
          = Core.withObject "Settings" Core.$
              \ x ->
                Settings' Core.<$>
                  (x Core..:? "ChannelIdentification") Core.<*>
                    x Core..:? "MaxAlternatives"
                    Core.<*> x Core..:? "MaxSpeakerLabels"
                    Core.<*> x Core..:? "ShowAlternatives"
                    Core.<*> x Core..:? "ShowSpeakerLabels"
                    Core.<*> x Core..:? "VocabularyFilterMethod"
                    Core.<*> x Core..:? "VocabularyFilterName"
                    Core.<*> x Core..:? "VocabularyName"
