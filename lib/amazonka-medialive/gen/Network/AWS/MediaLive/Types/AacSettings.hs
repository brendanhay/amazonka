-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AacSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AacSettings
  ( AacSettings (..),

    -- * Smart constructor
    mkAacSettings,

    -- * Lenses
    aRawFormat,
    aCodingMode,
    aProfile,
    aRateControlMode,
    aSampleRate,
    aSpec,
    aBitrate,
    aVbrQuality,
    aInputType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AacCodingMode
import Network.AWS.MediaLive.Types.AacInputType
import Network.AWS.MediaLive.Types.AacProfile
import Network.AWS.MediaLive.Types.AacRateControlMode
import Network.AWS.MediaLive.Types.AacRawFormat
import Network.AWS.MediaLive.Types.AacSpec
import Network.AWS.MediaLive.Types.AacVbrQuality
import qualified Network.AWS.Prelude as Lude

-- | Aac Settings
--
-- /See:/ 'mkAacSettings' smart constructor.
data AacSettings = AacSettings'
  { rawFormat ::
      Lude.Maybe AacRawFormat,
    codingMode :: Lude.Maybe AacCodingMode,
    profile :: Lude.Maybe AacProfile,
    rateControlMode :: Lude.Maybe AacRateControlMode,
    sampleRate :: Lude.Maybe Lude.Double,
    spec :: Lude.Maybe AacSpec,
    bitrate :: Lude.Maybe Lude.Double,
    vbrQuality :: Lude.Maybe AacVbrQuality,
    inputType :: Lude.Maybe AacInputType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AacSettings' with the minimum fields required to make a request.
--
-- * 'bitrate' - Average bitrate in bits/second. Valid values depend on rate control mode and profile.
-- * 'codingMode' - Mono, Stereo, or 5.1 channel layout. Valid values depend on rate control mode and profile. The adReceiverMix setting receives a stereo description plus control track and emits a mono AAC encode of the description track, with control data emitted in the PES header as per ETSI TS 101 154 Annex E.
-- * 'inputType' - Set to "broadcasterMixedAd" when input contains pre-mixed main audio + AD (narration) as a stereo pair.  The Audio Type field (audioType) will be set to 3, which signals to downstream systems that this stream contains "broadcaster mixed AD". Note that the input received by the encoder must contain pre-mixed audio; the encoder does not perform the mixing. The values in audioTypeControl and audioType (in AudioDescription) are ignored when set to broadcasterMixedAd.
--
--
-- Leave set to "normal" when input does not contain pre-mixed audio + AD.
-- * 'profile' - AAC Profile.
-- * 'rateControlMode' - Rate Control Mode.
-- * 'rawFormat' - Sets LATM / LOAS AAC output for raw containers.
-- * 'sampleRate' - Sample rate in Hz. Valid values depend on rate control mode and profile.
-- * 'spec' - Use MPEG-2 AAC audio instead of MPEG-4 AAC audio for raw or MPEG-2 Transport Stream containers.
-- * 'vbrQuality' - VBR Quality Level - Only used if rateControlMode is VBR.
mkAacSettings ::
  AacSettings
mkAacSettings =
  AacSettings'
    { rawFormat = Lude.Nothing,
      codingMode = Lude.Nothing,
      profile = Lude.Nothing,
      rateControlMode = Lude.Nothing,
      sampleRate = Lude.Nothing,
      spec = Lude.Nothing,
      bitrate = Lude.Nothing,
      vbrQuality = Lude.Nothing,
      inputType = Lude.Nothing
    }

-- | Sets LATM / LOAS AAC output for raw containers.
--
-- /Note:/ Consider using 'rawFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aRawFormat :: Lens.Lens' AacSettings (Lude.Maybe AacRawFormat)
aRawFormat = Lens.lens (rawFormat :: AacSettings -> Lude.Maybe AacRawFormat) (\s a -> s {rawFormat = a} :: AacSettings)
{-# DEPRECATED aRawFormat "Use generic-lens or generic-optics with 'rawFormat' instead." #-}

-- | Mono, Stereo, or 5.1 channel layout. Valid values depend on rate control mode and profile. The adReceiverMix setting receives a stereo description plus control track and emits a mono AAC encode of the description track, with control data emitted in the PES header as per ETSI TS 101 154 Annex E.
--
-- /Note:/ Consider using 'codingMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCodingMode :: Lens.Lens' AacSettings (Lude.Maybe AacCodingMode)
aCodingMode = Lens.lens (codingMode :: AacSettings -> Lude.Maybe AacCodingMode) (\s a -> s {codingMode = a} :: AacSettings)
{-# DEPRECATED aCodingMode "Use generic-lens or generic-optics with 'codingMode' instead." #-}

-- | AAC Profile.
--
-- /Note:/ Consider using 'profile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aProfile :: Lens.Lens' AacSettings (Lude.Maybe AacProfile)
aProfile = Lens.lens (profile :: AacSettings -> Lude.Maybe AacProfile) (\s a -> s {profile = a} :: AacSettings)
{-# DEPRECATED aProfile "Use generic-lens or generic-optics with 'profile' instead." #-}

-- | Rate Control Mode.
--
-- /Note:/ Consider using 'rateControlMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aRateControlMode :: Lens.Lens' AacSettings (Lude.Maybe AacRateControlMode)
aRateControlMode = Lens.lens (rateControlMode :: AacSettings -> Lude.Maybe AacRateControlMode) (\s a -> s {rateControlMode = a} :: AacSettings)
{-# DEPRECATED aRateControlMode "Use generic-lens or generic-optics with 'rateControlMode' instead." #-}

-- | Sample rate in Hz. Valid values depend on rate control mode and profile.
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aSampleRate :: Lens.Lens' AacSettings (Lude.Maybe Lude.Double)
aSampleRate = Lens.lens (sampleRate :: AacSettings -> Lude.Maybe Lude.Double) (\s a -> s {sampleRate = a} :: AacSettings)
{-# DEPRECATED aSampleRate "Use generic-lens or generic-optics with 'sampleRate' instead." #-}

-- | Use MPEG-2 AAC audio instead of MPEG-4 AAC audio for raw or MPEG-2 Transport Stream containers.
--
-- /Note:/ Consider using 'spec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aSpec :: Lens.Lens' AacSettings (Lude.Maybe AacSpec)
aSpec = Lens.lens (spec :: AacSettings -> Lude.Maybe AacSpec) (\s a -> s {spec = a} :: AacSettings)
{-# DEPRECATED aSpec "Use generic-lens or generic-optics with 'spec' instead." #-}

-- | Average bitrate in bits/second. Valid values depend on rate control mode and profile.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aBitrate :: Lens.Lens' AacSettings (Lude.Maybe Lude.Double)
aBitrate = Lens.lens (bitrate :: AacSettings -> Lude.Maybe Lude.Double) (\s a -> s {bitrate = a} :: AacSettings)
{-# DEPRECATED aBitrate "Use generic-lens or generic-optics with 'bitrate' instead." #-}

-- | VBR Quality Level - Only used if rateControlMode is VBR.
--
-- /Note:/ Consider using 'vbrQuality' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aVbrQuality :: Lens.Lens' AacSettings (Lude.Maybe AacVbrQuality)
aVbrQuality = Lens.lens (vbrQuality :: AacSettings -> Lude.Maybe AacVbrQuality) (\s a -> s {vbrQuality = a} :: AacSettings)
{-# DEPRECATED aVbrQuality "Use generic-lens or generic-optics with 'vbrQuality' instead." #-}

-- | Set to "broadcasterMixedAd" when input contains pre-mixed main audio + AD (narration) as a stereo pair.  The Audio Type field (audioType) will be set to 3, which signals to downstream systems that this stream contains "broadcaster mixed AD". Note that the input received by the encoder must contain pre-mixed audio; the encoder does not perform the mixing. The values in audioTypeControl and audioType (in AudioDescription) are ignored when set to broadcasterMixedAd.
--
--
-- Leave set to "normal" when input does not contain pre-mixed audio + AD.
--
-- /Note:/ Consider using 'inputType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aInputType :: Lens.Lens' AacSettings (Lude.Maybe AacInputType)
aInputType = Lens.lens (inputType :: AacSettings -> Lude.Maybe AacInputType) (\s a -> s {inputType = a} :: AacSettings)
{-# DEPRECATED aInputType "Use generic-lens or generic-optics with 'inputType' instead." #-}

instance Lude.FromJSON AacSettings where
  parseJSON =
    Lude.withObject
      "AacSettings"
      ( \x ->
          AacSettings'
            Lude.<$> (x Lude..:? "rawFormat")
            Lude.<*> (x Lude..:? "codingMode")
            Lude.<*> (x Lude..:? "profile")
            Lude.<*> (x Lude..:? "rateControlMode")
            Lude.<*> (x Lude..:? "sampleRate")
            Lude.<*> (x Lude..:? "spec")
            Lude.<*> (x Lude..:? "bitrate")
            Lude.<*> (x Lude..:? "vbrQuality")
            Lude.<*> (x Lude..:? "inputType")
      )

instance Lude.ToJSON AacSettings where
  toJSON AacSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("rawFormat" Lude..=) Lude.<$> rawFormat,
            ("codingMode" Lude..=) Lude.<$> codingMode,
            ("profile" Lude..=) Lude.<$> profile,
            ("rateControlMode" Lude..=) Lude.<$> rateControlMode,
            ("sampleRate" Lude..=) Lude.<$> sampleRate,
            ("spec" Lude..=) Lude.<$> spec,
            ("bitrate" Lude..=) Lude.<$> bitrate,
            ("vbrQuality" Lude..=) Lude.<$> vbrQuality,
            ("inputType" Lude..=) Lude.<$> inputType
          ]
      )
