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
-- Module      : Network.AWS.MediaLive.Types.Eac3Settings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Eac3Settings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.Eac3AttenuationControl
import Network.AWS.MediaLive.Types.Eac3BitstreamMode
import Network.AWS.MediaLive.Types.Eac3CodingMode
import Network.AWS.MediaLive.Types.Eac3DcFilter
import Network.AWS.MediaLive.Types.Eac3DrcLine
import Network.AWS.MediaLive.Types.Eac3DrcRf
import Network.AWS.MediaLive.Types.Eac3LfeControl
import Network.AWS.MediaLive.Types.Eac3LfeFilter
import Network.AWS.MediaLive.Types.Eac3MetadataControl
import Network.AWS.MediaLive.Types.Eac3PassthroughControl
import Network.AWS.MediaLive.Types.Eac3PhaseControl
import Network.AWS.MediaLive.Types.Eac3StereoDownmix
import Network.AWS.MediaLive.Types.Eac3SurroundExMode
import Network.AWS.MediaLive.Types.Eac3SurroundMode

-- | Eac3 Settings
--
-- /See:/ 'newEac3Settings' smart constructor.
data Eac3Settings = Eac3Settings'
  { -- | Left only\/Right only center mix level. Only used for 3\/2 coding mode.
    loRoCenterMixLevel :: Core.Maybe Core.Double,
    -- | Left total\/Right total center mix level. Only used for 3\/2 coding
    -- mode.
    ltRtCenterMixLevel :: Core.Maybe Core.Double,
    -- | Sets the dialnorm for the output. If blank and input audio is Dolby
    -- Digital Plus, dialnorm will be passed through.
    dialnorm :: Core.Maybe Core.Natural,
    -- | Dolby Digital Plus coding mode. Determines number of channels.
    codingMode :: Core.Maybe Eac3CodingMode,
    -- | When encoding 3\/2 audio, setting to lfe enables the LFE channel
    lfeControl :: Core.Maybe Eac3LfeControl,
    -- | Left only\/Right only surround mix level. Only used for 3\/2 coding
    -- mode.
    loRoSurroundMixLevel :: Core.Maybe Core.Double,
    -- | Left total\/Right total surround mix level. Only used for 3\/2 coding
    -- mode.
    ltRtSurroundMixLevel :: Core.Maybe Core.Double,
    -- | When set to enabled, applies a 120Hz lowpass filter to the LFE channel
    -- prior to encoding. Only valid with codingMode32 coding mode.
    lfeFilter :: Core.Maybe Eac3LfeFilter,
    -- | When set to enabled, activates a DC highpass filter for all input
    -- channels.
    dcFilter :: Core.Maybe Eac3DcFilter,
    -- | Stereo downmix preference. Only used for 3\/2 coding mode.
    stereoDownmix :: Core.Maybe Eac3StereoDownmix,
    -- | Sets the profile for heavy Dolby dynamic range compression, ensures that
    -- the instantaneous signal peaks do not exceed specified levels.
    drcRf :: Core.Maybe Eac3DrcRf,
    -- | Sets the Dolby dynamic range compression profile.
    drcLine :: Core.Maybe Eac3DrcLine,
    -- | Specifies the bitstream mode (bsmod) for the emitted E-AC-3 stream. See
    -- ATSC A\/52-2012 (Annex E) for background on these values.
    bitstreamMode :: Core.Maybe Eac3BitstreamMode,
    -- | When encoding 3\/2 audio, sets whether an extra center back surround
    -- channel is matrix encoded into the left and right surround channels.
    surroundExMode :: Core.Maybe Eac3SurroundExMode,
    -- | When set to shift90Degrees, applies a 90-degree phase shift to the
    -- surround channels. Only used for 3\/2 coding mode.
    phaseControl :: Core.Maybe Eac3PhaseControl,
    -- | When set to whenPossible, input DD+ audio will be passed through if it
    -- is present on the input. This detection is dynamic over the life of the
    -- transcode. Inputs that alternate between DD+ and non-DD+ content will
    -- have a consistent DD+ output as the system alternates between
    -- passthrough and encoding.
    passthroughControl :: Core.Maybe Eac3PassthroughControl,
    -- | Average bitrate in bits\/second. Valid bitrates depend on the coding
    -- mode.
    bitrate :: Core.Maybe Core.Double,
    -- | When set to attenuate3Db, applies a 3 dB attenuation to the surround
    -- channels. Only used for 3\/2 coding mode.
    attenuationControl :: Core.Maybe Eac3AttenuationControl,
    -- | When encoding 2\/0 audio, sets whether Dolby Surround is matrix encoded
    -- into the two channels.
    surroundMode :: Core.Maybe Eac3SurroundMode,
    -- | When set to followInput, encoder metadata will be sourced from the DD,
    -- DD+, or DolbyE decoder that supplied this audio data. If audio was not
    -- supplied from one of these streams, then the static metadata settings
    -- will be used.
    metadataControl :: Core.Maybe Eac3MetadataControl
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Eac3Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loRoCenterMixLevel', 'eac3Settings_loRoCenterMixLevel' - Left only\/Right only center mix level. Only used for 3\/2 coding mode.
--
-- 'ltRtCenterMixLevel', 'eac3Settings_ltRtCenterMixLevel' - Left total\/Right total center mix level. Only used for 3\/2 coding
-- mode.
--
-- 'dialnorm', 'eac3Settings_dialnorm' - Sets the dialnorm for the output. If blank and input audio is Dolby
-- Digital Plus, dialnorm will be passed through.
--
-- 'codingMode', 'eac3Settings_codingMode' - Dolby Digital Plus coding mode. Determines number of channels.
--
-- 'lfeControl', 'eac3Settings_lfeControl' - When encoding 3\/2 audio, setting to lfe enables the LFE channel
--
-- 'loRoSurroundMixLevel', 'eac3Settings_loRoSurroundMixLevel' - Left only\/Right only surround mix level. Only used for 3\/2 coding
-- mode.
--
-- 'ltRtSurroundMixLevel', 'eac3Settings_ltRtSurroundMixLevel' - Left total\/Right total surround mix level. Only used for 3\/2 coding
-- mode.
--
-- 'lfeFilter', 'eac3Settings_lfeFilter' - When set to enabled, applies a 120Hz lowpass filter to the LFE channel
-- prior to encoding. Only valid with codingMode32 coding mode.
--
-- 'dcFilter', 'eac3Settings_dcFilter' - When set to enabled, activates a DC highpass filter for all input
-- channels.
--
-- 'stereoDownmix', 'eac3Settings_stereoDownmix' - Stereo downmix preference. Only used for 3\/2 coding mode.
--
-- 'drcRf', 'eac3Settings_drcRf' - Sets the profile for heavy Dolby dynamic range compression, ensures that
-- the instantaneous signal peaks do not exceed specified levels.
--
-- 'drcLine', 'eac3Settings_drcLine' - Sets the Dolby dynamic range compression profile.
--
-- 'bitstreamMode', 'eac3Settings_bitstreamMode' - Specifies the bitstream mode (bsmod) for the emitted E-AC-3 stream. See
-- ATSC A\/52-2012 (Annex E) for background on these values.
--
-- 'surroundExMode', 'eac3Settings_surroundExMode' - When encoding 3\/2 audio, sets whether an extra center back surround
-- channel is matrix encoded into the left and right surround channels.
--
-- 'phaseControl', 'eac3Settings_phaseControl' - When set to shift90Degrees, applies a 90-degree phase shift to the
-- surround channels. Only used for 3\/2 coding mode.
--
-- 'passthroughControl', 'eac3Settings_passthroughControl' - When set to whenPossible, input DD+ audio will be passed through if it
-- is present on the input. This detection is dynamic over the life of the
-- transcode. Inputs that alternate between DD+ and non-DD+ content will
-- have a consistent DD+ output as the system alternates between
-- passthrough and encoding.
--
-- 'bitrate', 'eac3Settings_bitrate' - Average bitrate in bits\/second. Valid bitrates depend on the coding
-- mode.
--
-- 'attenuationControl', 'eac3Settings_attenuationControl' - When set to attenuate3Db, applies a 3 dB attenuation to the surround
-- channels. Only used for 3\/2 coding mode.
--
-- 'surroundMode', 'eac3Settings_surroundMode' - When encoding 2\/0 audio, sets whether Dolby Surround is matrix encoded
-- into the two channels.
--
-- 'metadataControl', 'eac3Settings_metadataControl' - When set to followInput, encoder metadata will be sourced from the DD,
-- DD+, or DolbyE decoder that supplied this audio data. If audio was not
-- supplied from one of these streams, then the static metadata settings
-- will be used.
newEac3Settings ::
  Eac3Settings
newEac3Settings =
  Eac3Settings'
    { loRoCenterMixLevel = Core.Nothing,
      ltRtCenterMixLevel = Core.Nothing,
      dialnorm = Core.Nothing,
      codingMode = Core.Nothing,
      lfeControl = Core.Nothing,
      loRoSurroundMixLevel = Core.Nothing,
      ltRtSurroundMixLevel = Core.Nothing,
      lfeFilter = Core.Nothing,
      dcFilter = Core.Nothing,
      stereoDownmix = Core.Nothing,
      drcRf = Core.Nothing,
      drcLine = Core.Nothing,
      bitstreamMode = Core.Nothing,
      surroundExMode = Core.Nothing,
      phaseControl = Core.Nothing,
      passthroughControl = Core.Nothing,
      bitrate = Core.Nothing,
      attenuationControl = Core.Nothing,
      surroundMode = Core.Nothing,
      metadataControl = Core.Nothing
    }

-- | Left only\/Right only center mix level. Only used for 3\/2 coding mode.
eac3Settings_loRoCenterMixLevel :: Lens.Lens' Eac3Settings (Core.Maybe Core.Double)
eac3Settings_loRoCenterMixLevel = Lens.lens (\Eac3Settings' {loRoCenterMixLevel} -> loRoCenterMixLevel) (\s@Eac3Settings' {} a -> s {loRoCenterMixLevel = a} :: Eac3Settings)

-- | Left total\/Right total center mix level. Only used for 3\/2 coding
-- mode.
eac3Settings_ltRtCenterMixLevel :: Lens.Lens' Eac3Settings (Core.Maybe Core.Double)
eac3Settings_ltRtCenterMixLevel = Lens.lens (\Eac3Settings' {ltRtCenterMixLevel} -> ltRtCenterMixLevel) (\s@Eac3Settings' {} a -> s {ltRtCenterMixLevel = a} :: Eac3Settings)

-- | Sets the dialnorm for the output. If blank and input audio is Dolby
-- Digital Plus, dialnorm will be passed through.
eac3Settings_dialnorm :: Lens.Lens' Eac3Settings (Core.Maybe Core.Natural)
eac3Settings_dialnorm = Lens.lens (\Eac3Settings' {dialnorm} -> dialnorm) (\s@Eac3Settings' {} a -> s {dialnorm = a} :: Eac3Settings)

-- | Dolby Digital Plus coding mode. Determines number of channels.
eac3Settings_codingMode :: Lens.Lens' Eac3Settings (Core.Maybe Eac3CodingMode)
eac3Settings_codingMode = Lens.lens (\Eac3Settings' {codingMode} -> codingMode) (\s@Eac3Settings' {} a -> s {codingMode = a} :: Eac3Settings)

-- | When encoding 3\/2 audio, setting to lfe enables the LFE channel
eac3Settings_lfeControl :: Lens.Lens' Eac3Settings (Core.Maybe Eac3LfeControl)
eac3Settings_lfeControl = Lens.lens (\Eac3Settings' {lfeControl} -> lfeControl) (\s@Eac3Settings' {} a -> s {lfeControl = a} :: Eac3Settings)

-- | Left only\/Right only surround mix level. Only used for 3\/2 coding
-- mode.
eac3Settings_loRoSurroundMixLevel :: Lens.Lens' Eac3Settings (Core.Maybe Core.Double)
eac3Settings_loRoSurroundMixLevel = Lens.lens (\Eac3Settings' {loRoSurroundMixLevel} -> loRoSurroundMixLevel) (\s@Eac3Settings' {} a -> s {loRoSurroundMixLevel = a} :: Eac3Settings)

-- | Left total\/Right total surround mix level. Only used for 3\/2 coding
-- mode.
eac3Settings_ltRtSurroundMixLevel :: Lens.Lens' Eac3Settings (Core.Maybe Core.Double)
eac3Settings_ltRtSurroundMixLevel = Lens.lens (\Eac3Settings' {ltRtSurroundMixLevel} -> ltRtSurroundMixLevel) (\s@Eac3Settings' {} a -> s {ltRtSurroundMixLevel = a} :: Eac3Settings)

-- | When set to enabled, applies a 120Hz lowpass filter to the LFE channel
-- prior to encoding. Only valid with codingMode32 coding mode.
eac3Settings_lfeFilter :: Lens.Lens' Eac3Settings (Core.Maybe Eac3LfeFilter)
eac3Settings_lfeFilter = Lens.lens (\Eac3Settings' {lfeFilter} -> lfeFilter) (\s@Eac3Settings' {} a -> s {lfeFilter = a} :: Eac3Settings)

-- | When set to enabled, activates a DC highpass filter for all input
-- channels.
eac3Settings_dcFilter :: Lens.Lens' Eac3Settings (Core.Maybe Eac3DcFilter)
eac3Settings_dcFilter = Lens.lens (\Eac3Settings' {dcFilter} -> dcFilter) (\s@Eac3Settings' {} a -> s {dcFilter = a} :: Eac3Settings)

-- | Stereo downmix preference. Only used for 3\/2 coding mode.
eac3Settings_stereoDownmix :: Lens.Lens' Eac3Settings (Core.Maybe Eac3StereoDownmix)
eac3Settings_stereoDownmix = Lens.lens (\Eac3Settings' {stereoDownmix} -> stereoDownmix) (\s@Eac3Settings' {} a -> s {stereoDownmix = a} :: Eac3Settings)

-- | Sets the profile for heavy Dolby dynamic range compression, ensures that
-- the instantaneous signal peaks do not exceed specified levels.
eac3Settings_drcRf :: Lens.Lens' Eac3Settings (Core.Maybe Eac3DrcRf)
eac3Settings_drcRf = Lens.lens (\Eac3Settings' {drcRf} -> drcRf) (\s@Eac3Settings' {} a -> s {drcRf = a} :: Eac3Settings)

-- | Sets the Dolby dynamic range compression profile.
eac3Settings_drcLine :: Lens.Lens' Eac3Settings (Core.Maybe Eac3DrcLine)
eac3Settings_drcLine = Lens.lens (\Eac3Settings' {drcLine} -> drcLine) (\s@Eac3Settings' {} a -> s {drcLine = a} :: Eac3Settings)

-- | Specifies the bitstream mode (bsmod) for the emitted E-AC-3 stream. See
-- ATSC A\/52-2012 (Annex E) for background on these values.
eac3Settings_bitstreamMode :: Lens.Lens' Eac3Settings (Core.Maybe Eac3BitstreamMode)
eac3Settings_bitstreamMode = Lens.lens (\Eac3Settings' {bitstreamMode} -> bitstreamMode) (\s@Eac3Settings' {} a -> s {bitstreamMode = a} :: Eac3Settings)

-- | When encoding 3\/2 audio, sets whether an extra center back surround
-- channel is matrix encoded into the left and right surround channels.
eac3Settings_surroundExMode :: Lens.Lens' Eac3Settings (Core.Maybe Eac3SurroundExMode)
eac3Settings_surroundExMode = Lens.lens (\Eac3Settings' {surroundExMode} -> surroundExMode) (\s@Eac3Settings' {} a -> s {surroundExMode = a} :: Eac3Settings)

-- | When set to shift90Degrees, applies a 90-degree phase shift to the
-- surround channels. Only used for 3\/2 coding mode.
eac3Settings_phaseControl :: Lens.Lens' Eac3Settings (Core.Maybe Eac3PhaseControl)
eac3Settings_phaseControl = Lens.lens (\Eac3Settings' {phaseControl} -> phaseControl) (\s@Eac3Settings' {} a -> s {phaseControl = a} :: Eac3Settings)

-- | When set to whenPossible, input DD+ audio will be passed through if it
-- is present on the input. This detection is dynamic over the life of the
-- transcode. Inputs that alternate between DD+ and non-DD+ content will
-- have a consistent DD+ output as the system alternates between
-- passthrough and encoding.
eac3Settings_passthroughControl :: Lens.Lens' Eac3Settings (Core.Maybe Eac3PassthroughControl)
eac3Settings_passthroughControl = Lens.lens (\Eac3Settings' {passthroughControl} -> passthroughControl) (\s@Eac3Settings' {} a -> s {passthroughControl = a} :: Eac3Settings)

-- | Average bitrate in bits\/second. Valid bitrates depend on the coding
-- mode.
eac3Settings_bitrate :: Lens.Lens' Eac3Settings (Core.Maybe Core.Double)
eac3Settings_bitrate = Lens.lens (\Eac3Settings' {bitrate} -> bitrate) (\s@Eac3Settings' {} a -> s {bitrate = a} :: Eac3Settings)

-- | When set to attenuate3Db, applies a 3 dB attenuation to the surround
-- channels. Only used for 3\/2 coding mode.
eac3Settings_attenuationControl :: Lens.Lens' Eac3Settings (Core.Maybe Eac3AttenuationControl)
eac3Settings_attenuationControl = Lens.lens (\Eac3Settings' {attenuationControl} -> attenuationControl) (\s@Eac3Settings' {} a -> s {attenuationControl = a} :: Eac3Settings)

-- | When encoding 2\/0 audio, sets whether Dolby Surround is matrix encoded
-- into the two channels.
eac3Settings_surroundMode :: Lens.Lens' Eac3Settings (Core.Maybe Eac3SurroundMode)
eac3Settings_surroundMode = Lens.lens (\Eac3Settings' {surroundMode} -> surroundMode) (\s@Eac3Settings' {} a -> s {surroundMode = a} :: Eac3Settings)

-- | When set to followInput, encoder metadata will be sourced from the DD,
-- DD+, or DolbyE decoder that supplied this audio data. If audio was not
-- supplied from one of these streams, then the static metadata settings
-- will be used.
eac3Settings_metadataControl :: Lens.Lens' Eac3Settings (Core.Maybe Eac3MetadataControl)
eac3Settings_metadataControl = Lens.lens (\Eac3Settings' {metadataControl} -> metadataControl) (\s@Eac3Settings' {} a -> s {metadataControl = a} :: Eac3Settings)

instance Core.FromJSON Eac3Settings where
  parseJSON =
    Core.withObject
      "Eac3Settings"
      ( \x ->
          Eac3Settings'
            Core.<$> (x Core..:? "loRoCenterMixLevel")
            Core.<*> (x Core..:? "ltRtCenterMixLevel")
            Core.<*> (x Core..:? "dialnorm")
            Core.<*> (x Core..:? "codingMode")
            Core.<*> (x Core..:? "lfeControl")
            Core.<*> (x Core..:? "loRoSurroundMixLevel")
            Core.<*> (x Core..:? "ltRtSurroundMixLevel")
            Core.<*> (x Core..:? "lfeFilter")
            Core.<*> (x Core..:? "dcFilter")
            Core.<*> (x Core..:? "stereoDownmix")
            Core.<*> (x Core..:? "drcRf")
            Core.<*> (x Core..:? "drcLine")
            Core.<*> (x Core..:? "bitstreamMode")
            Core.<*> (x Core..:? "surroundExMode")
            Core.<*> (x Core..:? "phaseControl")
            Core.<*> (x Core..:? "passthroughControl")
            Core.<*> (x Core..:? "bitrate")
            Core.<*> (x Core..:? "attenuationControl")
            Core.<*> (x Core..:? "surroundMode")
            Core.<*> (x Core..:? "metadataControl")
      )

instance Core.Hashable Eac3Settings

instance Core.NFData Eac3Settings

instance Core.ToJSON Eac3Settings where
  toJSON Eac3Settings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("loRoCenterMixLevel" Core..=)
              Core.<$> loRoCenterMixLevel,
            ("ltRtCenterMixLevel" Core..=)
              Core.<$> ltRtCenterMixLevel,
            ("dialnorm" Core..=) Core.<$> dialnorm,
            ("codingMode" Core..=) Core.<$> codingMode,
            ("lfeControl" Core..=) Core.<$> lfeControl,
            ("loRoSurroundMixLevel" Core..=)
              Core.<$> loRoSurroundMixLevel,
            ("ltRtSurroundMixLevel" Core..=)
              Core.<$> ltRtSurroundMixLevel,
            ("lfeFilter" Core..=) Core.<$> lfeFilter,
            ("dcFilter" Core..=) Core.<$> dcFilter,
            ("stereoDownmix" Core..=) Core.<$> stereoDownmix,
            ("drcRf" Core..=) Core.<$> drcRf,
            ("drcLine" Core..=) Core.<$> drcLine,
            ("bitstreamMode" Core..=) Core.<$> bitstreamMode,
            ("surroundExMode" Core..=) Core.<$> surroundExMode,
            ("phaseControl" Core..=) Core.<$> phaseControl,
            ("passthroughControl" Core..=)
              Core.<$> passthroughControl,
            ("bitrate" Core..=) Core.<$> bitrate,
            ("attenuationControl" Core..=)
              Core.<$> attenuationControl,
            ("surroundMode" Core..=) Core.<$> surroundMode,
            ("metadataControl" Core..=)
              Core.<$> metadataControl
          ]
      )
