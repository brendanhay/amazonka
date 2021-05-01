{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaLive.Types.Ac3Settings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Ac3Settings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.Ac3BitstreamMode
import Network.AWS.MediaLive.Types.Ac3CodingMode
import Network.AWS.MediaLive.Types.Ac3DrcProfile
import Network.AWS.MediaLive.Types.Ac3LfeFilter
import Network.AWS.MediaLive.Types.Ac3MetadataControl
import qualified Network.AWS.Prelude as Prelude

-- | Ac3 Settings
--
-- /See:/ 'newAc3Settings' smart constructor.
data Ac3Settings = Ac3Settings'
  { -- | Sets the dialnorm for the output. If excluded and input audio is Dolby
    -- Digital, dialnorm will be passed through.
    dialnorm :: Prelude.Maybe Prelude.Natural,
    -- | If set to filmStandard, adds dynamic range compression signaling to the
    -- output bitstream as defined in the Dolby Digital specification.
    drcProfile :: Prelude.Maybe Ac3DrcProfile,
    -- | Dolby Digital coding mode. Determines number of channels.
    codingMode :: Prelude.Maybe Ac3CodingMode,
    -- | When set to enabled, applies a 120Hz lowpass filter to the LFE channel
    -- prior to encoding. Only valid in codingMode32Lfe mode.
    lfeFilter :: Prelude.Maybe Ac3LfeFilter,
    -- | Specifies the bitstream mode (bsmod) for the emitted AC-3 stream. See
    -- ATSC A\/52-2012 for background on these values.
    bitstreamMode :: Prelude.Maybe Ac3BitstreamMode,
    -- | Average bitrate in bits\/second. Valid bitrates depend on the coding
    -- mode.
    bitrate :: Prelude.Maybe Prelude.Double,
    -- | When set to \"followInput\", encoder metadata will be sourced from the
    -- DD, DD+, or DolbyE decoder that supplied this audio data. If audio was
    -- not supplied from one of these streams, then the static metadata
    -- settings will be used.
    metadataControl :: Prelude.Maybe Ac3MetadataControl
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Ac3Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dialnorm', 'ac3Settings_dialnorm' - Sets the dialnorm for the output. If excluded and input audio is Dolby
-- Digital, dialnorm will be passed through.
--
-- 'drcProfile', 'ac3Settings_drcProfile' - If set to filmStandard, adds dynamic range compression signaling to the
-- output bitstream as defined in the Dolby Digital specification.
--
-- 'codingMode', 'ac3Settings_codingMode' - Dolby Digital coding mode. Determines number of channels.
--
-- 'lfeFilter', 'ac3Settings_lfeFilter' - When set to enabled, applies a 120Hz lowpass filter to the LFE channel
-- prior to encoding. Only valid in codingMode32Lfe mode.
--
-- 'bitstreamMode', 'ac3Settings_bitstreamMode' - Specifies the bitstream mode (bsmod) for the emitted AC-3 stream. See
-- ATSC A\/52-2012 for background on these values.
--
-- 'bitrate', 'ac3Settings_bitrate' - Average bitrate in bits\/second. Valid bitrates depend on the coding
-- mode.
--
-- 'metadataControl', 'ac3Settings_metadataControl' - When set to \"followInput\", encoder metadata will be sourced from the
-- DD, DD+, or DolbyE decoder that supplied this audio data. If audio was
-- not supplied from one of these streams, then the static metadata
-- settings will be used.
newAc3Settings ::
  Ac3Settings
newAc3Settings =
  Ac3Settings'
    { dialnorm = Prelude.Nothing,
      drcProfile = Prelude.Nothing,
      codingMode = Prelude.Nothing,
      lfeFilter = Prelude.Nothing,
      bitstreamMode = Prelude.Nothing,
      bitrate = Prelude.Nothing,
      metadataControl = Prelude.Nothing
    }

-- | Sets the dialnorm for the output. If excluded and input audio is Dolby
-- Digital, dialnorm will be passed through.
ac3Settings_dialnorm :: Lens.Lens' Ac3Settings (Prelude.Maybe Prelude.Natural)
ac3Settings_dialnorm = Lens.lens (\Ac3Settings' {dialnorm} -> dialnorm) (\s@Ac3Settings' {} a -> s {dialnorm = a} :: Ac3Settings)

-- | If set to filmStandard, adds dynamic range compression signaling to the
-- output bitstream as defined in the Dolby Digital specification.
ac3Settings_drcProfile :: Lens.Lens' Ac3Settings (Prelude.Maybe Ac3DrcProfile)
ac3Settings_drcProfile = Lens.lens (\Ac3Settings' {drcProfile} -> drcProfile) (\s@Ac3Settings' {} a -> s {drcProfile = a} :: Ac3Settings)

-- | Dolby Digital coding mode. Determines number of channels.
ac3Settings_codingMode :: Lens.Lens' Ac3Settings (Prelude.Maybe Ac3CodingMode)
ac3Settings_codingMode = Lens.lens (\Ac3Settings' {codingMode} -> codingMode) (\s@Ac3Settings' {} a -> s {codingMode = a} :: Ac3Settings)

-- | When set to enabled, applies a 120Hz lowpass filter to the LFE channel
-- prior to encoding. Only valid in codingMode32Lfe mode.
ac3Settings_lfeFilter :: Lens.Lens' Ac3Settings (Prelude.Maybe Ac3LfeFilter)
ac3Settings_lfeFilter = Lens.lens (\Ac3Settings' {lfeFilter} -> lfeFilter) (\s@Ac3Settings' {} a -> s {lfeFilter = a} :: Ac3Settings)

-- | Specifies the bitstream mode (bsmod) for the emitted AC-3 stream. See
-- ATSC A\/52-2012 for background on these values.
ac3Settings_bitstreamMode :: Lens.Lens' Ac3Settings (Prelude.Maybe Ac3BitstreamMode)
ac3Settings_bitstreamMode = Lens.lens (\Ac3Settings' {bitstreamMode} -> bitstreamMode) (\s@Ac3Settings' {} a -> s {bitstreamMode = a} :: Ac3Settings)

-- | Average bitrate in bits\/second. Valid bitrates depend on the coding
-- mode.
ac3Settings_bitrate :: Lens.Lens' Ac3Settings (Prelude.Maybe Prelude.Double)
ac3Settings_bitrate = Lens.lens (\Ac3Settings' {bitrate} -> bitrate) (\s@Ac3Settings' {} a -> s {bitrate = a} :: Ac3Settings)

-- | When set to \"followInput\", encoder metadata will be sourced from the
-- DD, DD+, or DolbyE decoder that supplied this audio data. If audio was
-- not supplied from one of these streams, then the static metadata
-- settings will be used.
ac3Settings_metadataControl :: Lens.Lens' Ac3Settings (Prelude.Maybe Ac3MetadataControl)
ac3Settings_metadataControl = Lens.lens (\Ac3Settings' {metadataControl} -> metadataControl) (\s@Ac3Settings' {} a -> s {metadataControl = a} :: Ac3Settings)

instance Prelude.FromJSON Ac3Settings where
  parseJSON =
    Prelude.withObject
      "Ac3Settings"
      ( \x ->
          Ac3Settings'
            Prelude.<$> (x Prelude..:? "dialnorm")
            Prelude.<*> (x Prelude..:? "drcProfile")
            Prelude.<*> (x Prelude..:? "codingMode")
            Prelude.<*> (x Prelude..:? "lfeFilter")
            Prelude.<*> (x Prelude..:? "bitstreamMode")
            Prelude.<*> (x Prelude..:? "bitrate")
            Prelude.<*> (x Prelude..:? "metadataControl")
      )

instance Prelude.Hashable Ac3Settings

instance Prelude.NFData Ac3Settings

instance Prelude.ToJSON Ac3Settings where
  toJSON Ac3Settings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("dialnorm" Prelude..=) Prelude.<$> dialnorm,
            ("drcProfile" Prelude..=) Prelude.<$> drcProfile,
            ("codingMode" Prelude..=) Prelude.<$> codingMode,
            ("lfeFilter" Prelude..=) Prelude.<$> lfeFilter,
            ("bitstreamMode" Prelude..=)
              Prelude.<$> bitstreamMode,
            ("bitrate" Prelude..=) Prelude.<$> bitrate,
            ("metadataControl" Prelude..=)
              Prelude.<$> metadataControl
          ]
      )
