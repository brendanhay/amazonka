{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AacSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AacSettings
  ( AacSettings (..)
  -- * Smart constructor
  , mkAacSettings
  -- * Lenses
  , asAudioDescriptionBroadcasterMix
  , asBitrate
  , asCodecProfile
  , asCodingMode
  , asRateControlMode
  , asRawFormat
  , asSampleRate
  , asSpecification
  , asVbrQuality
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.AacAudioDescriptionBroadcasterMix as Types
import qualified Network.AWS.MediaConvert.Types.AacCodecProfile as Types
import qualified Network.AWS.MediaConvert.Types.AacCodingMode as Types
import qualified Network.AWS.MediaConvert.Types.AacRateControlMode as Types
import qualified Network.AWS.MediaConvert.Types.AacRawFormat as Types
import qualified Network.AWS.MediaConvert.Types.AacSpecification as Types
import qualified Network.AWS.MediaConvert.Types.AacVbrQuality as Types
import qualified Network.AWS.Prelude as Core

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AAC. The service accepts one of two mutually exclusive groups of AAC settings--VBR and CBR. To select one of these modes, set the value of Bitrate control mode (rateControlMode) to "VBR" or "CBR".  In VBR mode, you control the audio quality with the setting VBR quality (vbrQuality). In CBR mode, you use the setting Bitrate (bitrate). Defaults and valid values depend on the rate control mode.
--
-- /See:/ 'mkAacSettings' smart constructor.
data AacSettings = AacSettings'
  { audioDescriptionBroadcasterMix :: Core.Maybe Types.AacAudioDescriptionBroadcasterMix
    -- ^ Choose BROADCASTER_MIXED_AD when the input contains pre-mixed main audio + audio description (AD) as a stereo pair. The value for AudioType will be set to 3, which signals to downstream systems that this stream contains "broadcaster mixed AD". Note that the input received by the encoder must contain pre-mixed audio; the encoder does not perform the mixing. When you choose BROADCASTER_MIXED_AD, the encoder ignores any values you provide in AudioType and  FollowInputAudioType. Choose NORMAL when the input does not contain pre-mixed audio + audio description (AD). In this case, the encoder will use any values you provide for AudioType and FollowInputAudioType.
  , bitrate :: Core.Maybe Core.Natural
    -- ^ Specify the average bitrate in bits per second. The set of valid values for this setting is: 6000, 8000, 10000, 12000, 14000, 16000, 20000, 24000, 28000, 32000, 40000, 48000, 56000, 64000, 80000, 96000, 112000, 128000, 160000, 192000, 224000, 256000, 288000, 320000, 384000, 448000, 512000, 576000, 640000, 768000, 896000, 1024000. The value you set is also constrained by the values that you choose for Profile (codecProfile), Bitrate control mode (codingMode), and Sample rate (sampleRate). Default values depend on Bitrate control mode and Profile.
  , codecProfile :: Core.Maybe Types.AacCodecProfile
    -- ^ AAC Profile.
  , codingMode :: Core.Maybe Types.AacCodingMode
    -- ^ Mono (Audio Description), Mono, Stereo, or 5.1 channel layout. Valid values depend on rate control mode and profile. "1.0 - Audio Description (Receiver Mix)" setting receives a stereo description plus control track and emits a mono AAC encode of the description track, with control data emitted in the PES header as per ETSI TS 101 154 Annex E.
  , rateControlMode :: Core.Maybe Types.AacRateControlMode
    -- ^ Rate Control Mode.
  , rawFormat :: Core.Maybe Types.AacRawFormat
    -- ^ Enables LATM/LOAS AAC output. Note that if you use LATM/LOAS AAC in an output, you must choose "No container" for the output container.
  , sampleRate :: Core.Maybe Core.Natural
    -- ^ Sample rate in Hz. Valid values depend on rate control mode and profile.
  , specification :: Core.Maybe Types.AacSpecification
    -- ^ Use MPEG-2 AAC instead of MPEG-4 AAC audio for raw or MPEG-2 Transport Stream containers.
  , vbrQuality :: Core.Maybe Types.AacVbrQuality
    -- ^ VBR Quality Level - Only used if rate_control_mode is VBR.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AacSettings' value with any optional fields omitted.
mkAacSettings
    :: AacSettings
mkAacSettings
  = AacSettings'{audioDescriptionBroadcasterMix = Core.Nothing,
                 bitrate = Core.Nothing, codecProfile = Core.Nothing,
                 codingMode = Core.Nothing, rateControlMode = Core.Nothing,
                 rawFormat = Core.Nothing, sampleRate = Core.Nothing,
                 specification = Core.Nothing, vbrQuality = Core.Nothing}

-- | Choose BROADCASTER_MIXED_AD when the input contains pre-mixed main audio + audio description (AD) as a stereo pair. The value for AudioType will be set to 3, which signals to downstream systems that this stream contains "broadcaster mixed AD". Note that the input received by the encoder must contain pre-mixed audio; the encoder does not perform the mixing. When you choose BROADCASTER_MIXED_AD, the encoder ignores any values you provide in AudioType and  FollowInputAudioType. Choose NORMAL when the input does not contain pre-mixed audio + audio description (AD). In this case, the encoder will use any values you provide for AudioType and FollowInputAudioType.
--
-- /Note:/ Consider using 'audioDescriptionBroadcasterMix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asAudioDescriptionBroadcasterMix :: Lens.Lens' AacSettings (Core.Maybe Types.AacAudioDescriptionBroadcasterMix)
asAudioDescriptionBroadcasterMix = Lens.field @"audioDescriptionBroadcasterMix"
{-# INLINEABLE asAudioDescriptionBroadcasterMix #-}
{-# DEPRECATED audioDescriptionBroadcasterMix "Use generic-lens or generic-optics with 'audioDescriptionBroadcasterMix' instead"  #-}

-- | Specify the average bitrate in bits per second. The set of valid values for this setting is: 6000, 8000, 10000, 12000, 14000, 16000, 20000, 24000, 28000, 32000, 40000, 48000, 56000, 64000, 80000, 96000, 112000, 128000, 160000, 192000, 224000, 256000, 288000, 320000, 384000, 448000, 512000, 576000, 640000, 768000, 896000, 1024000. The value you set is also constrained by the values that you choose for Profile (codecProfile), Bitrate control mode (codingMode), and Sample rate (sampleRate). Default values depend on Bitrate control mode and Profile.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asBitrate :: Lens.Lens' AacSettings (Core.Maybe Core.Natural)
asBitrate = Lens.field @"bitrate"
{-# INLINEABLE asBitrate #-}
{-# DEPRECATED bitrate "Use generic-lens or generic-optics with 'bitrate' instead"  #-}

-- | AAC Profile.
--
-- /Note:/ Consider using 'codecProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asCodecProfile :: Lens.Lens' AacSettings (Core.Maybe Types.AacCodecProfile)
asCodecProfile = Lens.field @"codecProfile"
{-# INLINEABLE asCodecProfile #-}
{-# DEPRECATED codecProfile "Use generic-lens or generic-optics with 'codecProfile' instead"  #-}

-- | Mono (Audio Description), Mono, Stereo, or 5.1 channel layout. Valid values depend on rate control mode and profile. "1.0 - Audio Description (Receiver Mix)" setting receives a stereo description plus control track and emits a mono AAC encode of the description track, with control data emitted in the PES header as per ETSI TS 101 154 Annex E.
--
-- /Note:/ Consider using 'codingMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asCodingMode :: Lens.Lens' AacSettings (Core.Maybe Types.AacCodingMode)
asCodingMode = Lens.field @"codingMode"
{-# INLINEABLE asCodingMode #-}
{-# DEPRECATED codingMode "Use generic-lens or generic-optics with 'codingMode' instead"  #-}

-- | Rate Control Mode.
--
-- /Note:/ Consider using 'rateControlMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asRateControlMode :: Lens.Lens' AacSettings (Core.Maybe Types.AacRateControlMode)
asRateControlMode = Lens.field @"rateControlMode"
{-# INLINEABLE asRateControlMode #-}
{-# DEPRECATED rateControlMode "Use generic-lens or generic-optics with 'rateControlMode' instead"  #-}

-- | Enables LATM/LOAS AAC output. Note that if you use LATM/LOAS AAC in an output, you must choose "No container" for the output container.
--
-- /Note:/ Consider using 'rawFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asRawFormat :: Lens.Lens' AacSettings (Core.Maybe Types.AacRawFormat)
asRawFormat = Lens.field @"rawFormat"
{-# INLINEABLE asRawFormat #-}
{-# DEPRECATED rawFormat "Use generic-lens or generic-optics with 'rawFormat' instead"  #-}

-- | Sample rate in Hz. Valid values depend on rate control mode and profile.
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asSampleRate :: Lens.Lens' AacSettings (Core.Maybe Core.Natural)
asSampleRate = Lens.field @"sampleRate"
{-# INLINEABLE asSampleRate #-}
{-# DEPRECATED sampleRate "Use generic-lens or generic-optics with 'sampleRate' instead"  #-}

-- | Use MPEG-2 AAC instead of MPEG-4 AAC audio for raw or MPEG-2 Transport Stream containers.
--
-- /Note:/ Consider using 'specification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asSpecification :: Lens.Lens' AacSettings (Core.Maybe Types.AacSpecification)
asSpecification = Lens.field @"specification"
{-# INLINEABLE asSpecification #-}
{-# DEPRECATED specification "Use generic-lens or generic-optics with 'specification' instead"  #-}

-- | VBR Quality Level - Only used if rate_control_mode is VBR.
--
-- /Note:/ Consider using 'vbrQuality' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asVbrQuality :: Lens.Lens' AacSettings (Core.Maybe Types.AacVbrQuality)
asVbrQuality = Lens.field @"vbrQuality"
{-# INLINEABLE asVbrQuality #-}
{-# DEPRECATED vbrQuality "Use generic-lens or generic-optics with 'vbrQuality' instead"  #-}

instance Core.FromJSON AacSettings where
        toJSON AacSettings{..}
          = Core.object
              (Core.catMaybes
                 [("audioDescriptionBroadcasterMix" Core..=) Core.<$>
                    audioDescriptionBroadcasterMix,
                  ("bitrate" Core..=) Core.<$> bitrate,
                  ("codecProfile" Core..=) Core.<$> codecProfile,
                  ("codingMode" Core..=) Core.<$> codingMode,
                  ("rateControlMode" Core..=) Core.<$> rateControlMode,
                  ("rawFormat" Core..=) Core.<$> rawFormat,
                  ("sampleRate" Core..=) Core.<$> sampleRate,
                  ("specification" Core..=) Core.<$> specification,
                  ("vbrQuality" Core..=) Core.<$> vbrQuality])

instance Core.FromJSON AacSettings where
        parseJSON
          = Core.withObject "AacSettings" Core.$
              \ x ->
                AacSettings' Core.<$>
                  (x Core..:? "audioDescriptionBroadcasterMix") Core.<*>
                    x Core..:? "bitrate"
                    Core.<*> x Core..:? "codecProfile"
                    Core.<*> x Core..:? "codingMode"
                    Core.<*> x Core..:? "rateControlMode"
                    Core.<*> x Core..:? "rawFormat"
                    Core.<*> x Core..:? "sampleRate"
                    Core.<*> x Core..:? "specification"
                    Core.<*> x Core..:? "vbrQuality"
