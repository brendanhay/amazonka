{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AacSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.AacSettings
  ( AacSettings (..)
  -- * Smart constructor
  , mkAacSettings
  -- * Lenses
  , asBitrate
  , asCodingMode
  , asInputType
  , asProfile
  , asRateControlMode
  , asRawFormat
  , asSampleRate
  , asSpec
  , asVbrQuality
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.AacCodingMode as Types
import qualified Network.AWS.MediaLive.Types.AacInputType as Types
import qualified Network.AWS.MediaLive.Types.AacProfile as Types
import qualified Network.AWS.MediaLive.Types.AacRateControlMode as Types
import qualified Network.AWS.MediaLive.Types.AacRawFormat as Types
import qualified Network.AWS.MediaLive.Types.AacSpec as Types
import qualified Network.AWS.MediaLive.Types.AacVbrQuality as Types
import qualified Network.AWS.Prelude as Core

-- | Aac Settings
--
-- /See:/ 'mkAacSettings' smart constructor.
data AacSettings = AacSettings'
  { bitrate :: Core.Maybe Core.Double
    -- ^ Average bitrate in bits/second. Valid values depend on rate control mode and profile.
  , codingMode :: Core.Maybe Types.AacCodingMode
    -- ^ Mono, Stereo, or 5.1 channel layout. Valid values depend on rate control mode and profile. The adReceiverMix setting receives a stereo description plus control track and emits a mono AAC encode of the description track, with control data emitted in the PES header as per ETSI TS 101 154 Annex E.
  , inputType :: Core.Maybe Types.AacInputType
    -- ^ Set to "broadcasterMixedAd" when input contains pre-mixed main audio + AD (narration) as a stereo pair.  The Audio Type field (audioType) will be set to 3, which signals to downstream systems that this stream contains "broadcaster mixed AD". Note that the input received by the encoder must contain pre-mixed audio; the encoder does not perform the mixing. The values in audioTypeControl and audioType (in AudioDescription) are ignored when set to broadcasterMixedAd.
--
--
-- Leave set to "normal" when input does not contain pre-mixed audio + AD.
  , profile :: Core.Maybe Types.AacProfile
    -- ^ AAC Profile.
  , rateControlMode :: Core.Maybe Types.AacRateControlMode
    -- ^ Rate Control Mode.
  , rawFormat :: Core.Maybe Types.AacRawFormat
    -- ^ Sets LATM / LOAS AAC output for raw containers.
  , sampleRate :: Core.Maybe Core.Double
    -- ^ Sample rate in Hz. Valid values depend on rate control mode and profile.
  , spec :: Core.Maybe Types.AacSpec
    -- ^ Use MPEG-2 AAC audio instead of MPEG-4 AAC audio for raw or MPEG-2 Transport Stream containers.
  , vbrQuality :: Core.Maybe Types.AacVbrQuality
    -- ^ VBR Quality Level - Only used if rateControlMode is VBR.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AacSettings' value with any optional fields omitted.
mkAacSettings
    :: AacSettings
mkAacSettings
  = AacSettings'{bitrate = Core.Nothing, codingMode = Core.Nothing,
                 inputType = Core.Nothing, profile = Core.Nothing,
                 rateControlMode = Core.Nothing, rawFormat = Core.Nothing,
                 sampleRate = Core.Nothing, spec = Core.Nothing,
                 vbrQuality = Core.Nothing}

-- | Average bitrate in bits/second. Valid values depend on rate control mode and profile.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asBitrate :: Lens.Lens' AacSettings (Core.Maybe Core.Double)
asBitrate = Lens.field @"bitrate"
{-# INLINEABLE asBitrate #-}
{-# DEPRECATED bitrate "Use generic-lens or generic-optics with 'bitrate' instead"  #-}

-- | Mono, Stereo, or 5.1 channel layout. Valid values depend on rate control mode and profile. The adReceiverMix setting receives a stereo description plus control track and emits a mono AAC encode of the description track, with control data emitted in the PES header as per ETSI TS 101 154 Annex E.
--
-- /Note:/ Consider using 'codingMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asCodingMode :: Lens.Lens' AacSettings (Core.Maybe Types.AacCodingMode)
asCodingMode = Lens.field @"codingMode"
{-# INLINEABLE asCodingMode #-}
{-# DEPRECATED codingMode "Use generic-lens or generic-optics with 'codingMode' instead"  #-}

-- | Set to "broadcasterMixedAd" when input contains pre-mixed main audio + AD (narration) as a stereo pair.  The Audio Type field (audioType) will be set to 3, which signals to downstream systems that this stream contains "broadcaster mixed AD". Note that the input received by the encoder must contain pre-mixed audio; the encoder does not perform the mixing. The values in audioTypeControl and audioType (in AudioDescription) are ignored when set to broadcasterMixedAd.
--
--
-- Leave set to "normal" when input does not contain pre-mixed audio + AD.
--
-- /Note:/ Consider using 'inputType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asInputType :: Lens.Lens' AacSettings (Core.Maybe Types.AacInputType)
asInputType = Lens.field @"inputType"
{-# INLINEABLE asInputType #-}
{-# DEPRECATED inputType "Use generic-lens or generic-optics with 'inputType' instead"  #-}

-- | AAC Profile.
--
-- /Note:/ Consider using 'profile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asProfile :: Lens.Lens' AacSettings (Core.Maybe Types.AacProfile)
asProfile = Lens.field @"profile"
{-# INLINEABLE asProfile #-}
{-# DEPRECATED profile "Use generic-lens or generic-optics with 'profile' instead"  #-}

-- | Rate Control Mode.
--
-- /Note:/ Consider using 'rateControlMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asRateControlMode :: Lens.Lens' AacSettings (Core.Maybe Types.AacRateControlMode)
asRateControlMode = Lens.field @"rateControlMode"
{-# INLINEABLE asRateControlMode #-}
{-# DEPRECATED rateControlMode "Use generic-lens or generic-optics with 'rateControlMode' instead"  #-}

-- | Sets LATM / LOAS AAC output for raw containers.
--
-- /Note:/ Consider using 'rawFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asRawFormat :: Lens.Lens' AacSettings (Core.Maybe Types.AacRawFormat)
asRawFormat = Lens.field @"rawFormat"
{-# INLINEABLE asRawFormat #-}
{-# DEPRECATED rawFormat "Use generic-lens or generic-optics with 'rawFormat' instead"  #-}

-- | Sample rate in Hz. Valid values depend on rate control mode and profile.
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asSampleRate :: Lens.Lens' AacSettings (Core.Maybe Core.Double)
asSampleRate = Lens.field @"sampleRate"
{-# INLINEABLE asSampleRate #-}
{-# DEPRECATED sampleRate "Use generic-lens or generic-optics with 'sampleRate' instead"  #-}

-- | Use MPEG-2 AAC audio instead of MPEG-4 AAC audio for raw or MPEG-2 Transport Stream containers.
--
-- /Note:/ Consider using 'spec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asSpec :: Lens.Lens' AacSettings (Core.Maybe Types.AacSpec)
asSpec = Lens.field @"spec"
{-# INLINEABLE asSpec #-}
{-# DEPRECATED spec "Use generic-lens or generic-optics with 'spec' instead"  #-}

-- | VBR Quality Level - Only used if rateControlMode is VBR.
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
                 [("bitrate" Core..=) Core.<$> bitrate,
                  ("codingMode" Core..=) Core.<$> codingMode,
                  ("inputType" Core..=) Core.<$> inputType,
                  ("profile" Core..=) Core.<$> profile,
                  ("rateControlMode" Core..=) Core.<$> rateControlMode,
                  ("rawFormat" Core..=) Core.<$> rawFormat,
                  ("sampleRate" Core..=) Core.<$> sampleRate,
                  ("spec" Core..=) Core.<$> spec,
                  ("vbrQuality" Core..=) Core.<$> vbrQuality])

instance Core.FromJSON AacSettings where
        parseJSON
          = Core.withObject "AacSettings" Core.$
              \ x ->
                AacSettings' Core.<$>
                  (x Core..:? "bitrate") Core.<*> x Core..:? "codingMode" Core.<*>
                    x Core..:? "inputType"
                    Core.<*> x Core..:? "profile"
                    Core.<*> x Core..:? "rateControlMode"
                    Core.<*> x Core..:? "rawFormat"
                    Core.<*> x Core..:? "sampleRate"
                    Core.<*> x Core..:? "spec"
                    Core.<*> x Core..:? "vbrQuality"
