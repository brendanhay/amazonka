{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Eac3AtmosSettings
  ( Eac3AtmosSettings (..)
  -- * Smart constructor
  , mkEac3AtmosSettings
  -- * Lenses
  , easBitrate
  , easBitstreamMode
  , easCodingMode
  , easDialogueIntelligence
  , easDynamicRangeCompressionLine
  , easDynamicRangeCompressionRf
  , easLoRoCenterMixLevel
  , easLoRoSurroundMixLevel
  , easLtRtCenterMixLevel
  , easLtRtSurroundMixLevel
  , easMeteringMode
  , easSampleRate
  , easSpeechThreshold
  , easStereoDownmix
  , easSurroundExMode
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.Eac3AtmosBitstreamMode as Types
import qualified Network.AWS.MediaConvert.Types.Eac3AtmosCodingMode as Types
import qualified Network.AWS.MediaConvert.Types.Eac3AtmosDialogueIntelligence as Types
import qualified Network.AWS.MediaConvert.Types.Eac3AtmosDynamicRangeCompressionLine as Types
import qualified Network.AWS.MediaConvert.Types.Eac3AtmosDynamicRangeCompressionRf as Types
import qualified Network.AWS.MediaConvert.Types.Eac3AtmosMeteringMode as Types
import qualified Network.AWS.MediaConvert.Types.Eac3AtmosStereoDownmix as Types
import qualified Network.AWS.MediaConvert.Types.Eac3AtmosSurroundExMode as Types
import qualified Network.AWS.Prelude as Core

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value EAC3_ATMOS.
--
-- /See:/ 'mkEac3AtmosSettings' smart constructor.
data Eac3AtmosSettings = Eac3AtmosSettings'
  { bitrate :: Core.Maybe Core.Natural
    -- ^ Specify the average bitrate in bits per second.
--
-- Valid values: 384k, 448k, 640k, 768k
  , bitstreamMode :: Core.Maybe Types.Eac3AtmosBitstreamMode
    -- ^ Specify the bitstream mode for the E-AC-3 stream that the encoder emits. For more information about the EAC3 bitstream mode, see ATSC A/52-2012 (Annex E).
  , codingMode :: Core.Maybe Types.Eac3AtmosCodingMode
    -- ^ The coding mode for Dolby Digital Plus JOC (Atmos) is always 9.1.6 (CODING_MODE_9_1_6).
  , dialogueIntelligence :: Core.Maybe Types.Eac3AtmosDialogueIntelligence
    -- ^ Enable Dolby Dialogue Intelligence to adjust loudness based on dialogue analysis.
  , dynamicRangeCompressionLine :: Core.Maybe Types.Eac3AtmosDynamicRangeCompressionLine
    -- ^ Specify the absolute peak level for a signal with dynamic range compression.
  , dynamicRangeCompressionRf :: Core.Maybe Types.Eac3AtmosDynamicRangeCompressionRf
    -- ^ Specify how the service limits the audio dynamic range when compressing the audio.
  , loRoCenterMixLevel :: Core.Maybe Core.Double
    -- ^ Specify a value for the following Dolby Atmos setting: Left only/Right only center mix
--
-- (Lo/Ro center). MediaConvert uses this value for downmixing. How the service uses this
-- value depends on the value that you choose for Stereo downmix (Eac3AtmosStereoDownmix).
-- Valid values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, and -6.0.
  , loRoSurroundMixLevel :: Core.Maybe Core.Double
    -- ^ Specify a value for the following Dolby Atmos setting: Left only/Right only (Lo/Ro surround). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3AtmosStereoDownmix). Valid values: -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel.
  , ltRtCenterMixLevel :: Core.Maybe Core.Double
    -- ^ Specify a value for the following Dolby Atmos setting: Left total/Right total center mix (Lt/Rt center). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3AtmosStereoDownmix). Valid values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, and -6.0.
  , ltRtSurroundMixLevel :: Core.Maybe Core.Double
    -- ^ Specify a value for the following Dolby Atmos setting: Left total/Right total surround mix (Lt/Rt surround). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3AtmosStereoDownmix). Valid values: -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel.
  , meteringMode :: Core.Maybe Types.Eac3AtmosMeteringMode
    -- ^ Choose how the service meters the loudness of your audio.
  , sampleRate :: Core.Maybe Core.Natural
    -- ^ This value is always 48000. It represents the sample rate in Hz.
  , speechThreshold :: Core.Maybe Core.Natural
    -- ^ Specify the percentage of audio content that must be speech before the encoder uses the measured speech loudness as the overall program loudness.
  , stereoDownmix :: Core.Maybe Types.Eac3AtmosStereoDownmix
    -- ^ Choose how the service does stereo downmixing.
  , surroundExMode :: Core.Maybe Types.Eac3AtmosSurroundExMode
    -- ^ Specify whether your input audio has an additional center rear surround channel matrix encoded into your left and right surround channels.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Eac3AtmosSettings' value with any optional fields omitted.
mkEac3AtmosSettings
    :: Eac3AtmosSettings
mkEac3AtmosSettings
  = Eac3AtmosSettings'{bitrate = Core.Nothing,
                       bitstreamMode = Core.Nothing, codingMode = Core.Nothing,
                       dialogueIntelligence = Core.Nothing,
                       dynamicRangeCompressionLine = Core.Nothing,
                       dynamicRangeCompressionRf = Core.Nothing,
                       loRoCenterMixLevel = Core.Nothing,
                       loRoSurroundMixLevel = Core.Nothing,
                       ltRtCenterMixLevel = Core.Nothing,
                       ltRtSurroundMixLevel = Core.Nothing, meteringMode = Core.Nothing,
                       sampleRate = Core.Nothing, speechThreshold = Core.Nothing,
                       stereoDownmix = Core.Nothing, surroundExMode = Core.Nothing}

-- | Specify the average bitrate in bits per second.
--
-- Valid values: 384k, 448k, 640k, 768k
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
easBitrate :: Lens.Lens' Eac3AtmosSettings (Core.Maybe Core.Natural)
easBitrate = Lens.field @"bitrate"
{-# INLINEABLE easBitrate #-}
{-# DEPRECATED bitrate "Use generic-lens or generic-optics with 'bitrate' instead"  #-}

-- | Specify the bitstream mode for the E-AC-3 stream that the encoder emits. For more information about the EAC3 bitstream mode, see ATSC A/52-2012 (Annex E).
--
-- /Note:/ Consider using 'bitstreamMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
easBitstreamMode :: Lens.Lens' Eac3AtmosSettings (Core.Maybe Types.Eac3AtmosBitstreamMode)
easBitstreamMode = Lens.field @"bitstreamMode"
{-# INLINEABLE easBitstreamMode #-}
{-# DEPRECATED bitstreamMode "Use generic-lens or generic-optics with 'bitstreamMode' instead"  #-}

-- | The coding mode for Dolby Digital Plus JOC (Atmos) is always 9.1.6 (CODING_MODE_9_1_6).
--
-- /Note:/ Consider using 'codingMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
easCodingMode :: Lens.Lens' Eac3AtmosSettings (Core.Maybe Types.Eac3AtmosCodingMode)
easCodingMode = Lens.field @"codingMode"
{-# INLINEABLE easCodingMode #-}
{-# DEPRECATED codingMode "Use generic-lens or generic-optics with 'codingMode' instead"  #-}

-- | Enable Dolby Dialogue Intelligence to adjust loudness based on dialogue analysis.
--
-- /Note:/ Consider using 'dialogueIntelligence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
easDialogueIntelligence :: Lens.Lens' Eac3AtmosSettings (Core.Maybe Types.Eac3AtmosDialogueIntelligence)
easDialogueIntelligence = Lens.field @"dialogueIntelligence"
{-# INLINEABLE easDialogueIntelligence #-}
{-# DEPRECATED dialogueIntelligence "Use generic-lens or generic-optics with 'dialogueIntelligence' instead"  #-}

-- | Specify the absolute peak level for a signal with dynamic range compression.
--
-- /Note:/ Consider using 'dynamicRangeCompressionLine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
easDynamicRangeCompressionLine :: Lens.Lens' Eac3AtmosSettings (Core.Maybe Types.Eac3AtmosDynamicRangeCompressionLine)
easDynamicRangeCompressionLine = Lens.field @"dynamicRangeCompressionLine"
{-# INLINEABLE easDynamicRangeCompressionLine #-}
{-# DEPRECATED dynamicRangeCompressionLine "Use generic-lens or generic-optics with 'dynamicRangeCompressionLine' instead"  #-}

-- | Specify how the service limits the audio dynamic range when compressing the audio.
--
-- /Note:/ Consider using 'dynamicRangeCompressionRf' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
easDynamicRangeCompressionRf :: Lens.Lens' Eac3AtmosSettings (Core.Maybe Types.Eac3AtmosDynamicRangeCompressionRf)
easDynamicRangeCompressionRf = Lens.field @"dynamicRangeCompressionRf"
{-# INLINEABLE easDynamicRangeCompressionRf #-}
{-# DEPRECATED dynamicRangeCompressionRf "Use generic-lens or generic-optics with 'dynamicRangeCompressionRf' instead"  #-}

-- | Specify a value for the following Dolby Atmos setting: Left only/Right only center mix
--
-- (Lo/Ro center). MediaConvert uses this value for downmixing. How the service uses this
-- value depends on the value that you choose for Stereo downmix (Eac3AtmosStereoDownmix).
-- Valid values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, and -6.0.
--
-- /Note:/ Consider using 'loRoCenterMixLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
easLoRoCenterMixLevel :: Lens.Lens' Eac3AtmosSettings (Core.Maybe Core.Double)
easLoRoCenterMixLevel = Lens.field @"loRoCenterMixLevel"
{-# INLINEABLE easLoRoCenterMixLevel #-}
{-# DEPRECATED loRoCenterMixLevel "Use generic-lens or generic-optics with 'loRoCenterMixLevel' instead"  #-}

-- | Specify a value for the following Dolby Atmos setting: Left only/Right only (Lo/Ro surround). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3AtmosStereoDownmix). Valid values: -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel.
--
-- /Note:/ Consider using 'loRoSurroundMixLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
easLoRoSurroundMixLevel :: Lens.Lens' Eac3AtmosSettings (Core.Maybe Core.Double)
easLoRoSurroundMixLevel = Lens.field @"loRoSurroundMixLevel"
{-# INLINEABLE easLoRoSurroundMixLevel #-}
{-# DEPRECATED loRoSurroundMixLevel "Use generic-lens or generic-optics with 'loRoSurroundMixLevel' instead"  #-}

-- | Specify a value for the following Dolby Atmos setting: Left total/Right total center mix (Lt/Rt center). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3AtmosStereoDownmix). Valid values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, and -6.0.
--
-- /Note:/ Consider using 'ltRtCenterMixLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
easLtRtCenterMixLevel :: Lens.Lens' Eac3AtmosSettings (Core.Maybe Core.Double)
easLtRtCenterMixLevel = Lens.field @"ltRtCenterMixLevel"
{-# INLINEABLE easLtRtCenterMixLevel #-}
{-# DEPRECATED ltRtCenterMixLevel "Use generic-lens or generic-optics with 'ltRtCenterMixLevel' instead"  #-}

-- | Specify a value for the following Dolby Atmos setting: Left total/Right total surround mix (Lt/Rt surround). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3AtmosStereoDownmix). Valid values: -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel.
--
-- /Note:/ Consider using 'ltRtSurroundMixLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
easLtRtSurroundMixLevel :: Lens.Lens' Eac3AtmosSettings (Core.Maybe Core.Double)
easLtRtSurroundMixLevel = Lens.field @"ltRtSurroundMixLevel"
{-# INLINEABLE easLtRtSurroundMixLevel #-}
{-# DEPRECATED ltRtSurroundMixLevel "Use generic-lens or generic-optics with 'ltRtSurroundMixLevel' instead"  #-}

-- | Choose how the service meters the loudness of your audio.
--
-- /Note:/ Consider using 'meteringMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
easMeteringMode :: Lens.Lens' Eac3AtmosSettings (Core.Maybe Types.Eac3AtmosMeteringMode)
easMeteringMode = Lens.field @"meteringMode"
{-# INLINEABLE easMeteringMode #-}
{-# DEPRECATED meteringMode "Use generic-lens or generic-optics with 'meteringMode' instead"  #-}

-- | This value is always 48000. It represents the sample rate in Hz.
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
easSampleRate :: Lens.Lens' Eac3AtmosSettings (Core.Maybe Core.Natural)
easSampleRate = Lens.field @"sampleRate"
{-# INLINEABLE easSampleRate #-}
{-# DEPRECATED sampleRate "Use generic-lens or generic-optics with 'sampleRate' instead"  #-}

-- | Specify the percentage of audio content that must be speech before the encoder uses the measured speech loudness as the overall program loudness.
--
-- /Note:/ Consider using 'speechThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
easSpeechThreshold :: Lens.Lens' Eac3AtmosSettings (Core.Maybe Core.Natural)
easSpeechThreshold = Lens.field @"speechThreshold"
{-# INLINEABLE easSpeechThreshold #-}
{-# DEPRECATED speechThreshold "Use generic-lens or generic-optics with 'speechThreshold' instead"  #-}

-- | Choose how the service does stereo downmixing.
--
-- /Note:/ Consider using 'stereoDownmix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
easStereoDownmix :: Lens.Lens' Eac3AtmosSettings (Core.Maybe Types.Eac3AtmosStereoDownmix)
easStereoDownmix = Lens.field @"stereoDownmix"
{-# INLINEABLE easStereoDownmix #-}
{-# DEPRECATED stereoDownmix "Use generic-lens or generic-optics with 'stereoDownmix' instead"  #-}

-- | Specify whether your input audio has an additional center rear surround channel matrix encoded into your left and right surround channels.
--
-- /Note:/ Consider using 'surroundExMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
easSurroundExMode :: Lens.Lens' Eac3AtmosSettings (Core.Maybe Types.Eac3AtmosSurroundExMode)
easSurroundExMode = Lens.field @"surroundExMode"
{-# INLINEABLE easSurroundExMode #-}
{-# DEPRECATED surroundExMode "Use generic-lens or generic-optics with 'surroundExMode' instead"  #-}

instance Core.FromJSON Eac3AtmosSettings where
        toJSON Eac3AtmosSettings{..}
          = Core.object
              (Core.catMaybes
                 [("bitrate" Core..=) Core.<$> bitrate,
                  ("bitstreamMode" Core..=) Core.<$> bitstreamMode,
                  ("codingMode" Core..=) Core.<$> codingMode,
                  ("dialogueIntelligence" Core..=) Core.<$> dialogueIntelligence,
                  ("dynamicRangeCompressionLine" Core..=) Core.<$>
                    dynamicRangeCompressionLine,
                  ("dynamicRangeCompressionRf" Core..=) Core.<$>
                    dynamicRangeCompressionRf,
                  ("loRoCenterMixLevel" Core..=) Core.<$> loRoCenterMixLevel,
                  ("loRoSurroundMixLevel" Core..=) Core.<$> loRoSurroundMixLevel,
                  ("ltRtCenterMixLevel" Core..=) Core.<$> ltRtCenterMixLevel,
                  ("ltRtSurroundMixLevel" Core..=) Core.<$> ltRtSurroundMixLevel,
                  ("meteringMode" Core..=) Core.<$> meteringMode,
                  ("sampleRate" Core..=) Core.<$> sampleRate,
                  ("speechThreshold" Core..=) Core.<$> speechThreshold,
                  ("stereoDownmix" Core..=) Core.<$> stereoDownmix,
                  ("surroundExMode" Core..=) Core.<$> surroundExMode])

instance Core.FromJSON Eac3AtmosSettings where
        parseJSON
          = Core.withObject "Eac3AtmosSettings" Core.$
              \ x ->
                Eac3AtmosSettings' Core.<$>
                  (x Core..:? "bitrate") Core.<*> x Core..:? "bitstreamMode" Core.<*>
                    x Core..:? "codingMode"
                    Core.<*> x Core..:? "dialogueIntelligence"
                    Core.<*> x Core..:? "dynamicRangeCompressionLine"
                    Core.<*> x Core..:? "dynamicRangeCompressionRf"
                    Core.<*> x Core..:? "loRoCenterMixLevel"
                    Core.<*> x Core..:? "loRoSurroundMixLevel"
                    Core.<*> x Core..:? "ltRtCenterMixLevel"
                    Core.<*> x Core..:? "ltRtSurroundMixLevel"
                    Core.<*> x Core..:? "meteringMode"
                    Core.<*> x Core..:? "sampleRate"
                    Core.<*> x Core..:? "speechThreshold"
                    Core.<*> x Core..:? "stereoDownmix"
                    Core.<*> x Core..:? "surroundExMode"
