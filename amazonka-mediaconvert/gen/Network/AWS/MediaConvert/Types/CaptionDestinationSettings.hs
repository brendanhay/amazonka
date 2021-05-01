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
-- Module      : Network.AWS.MediaConvert.Types.CaptionDestinationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CaptionDestinationSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.BurninDestinationSettings
import Network.AWS.MediaConvert.Types.CaptionDestinationType
import Network.AWS.MediaConvert.Types.DvbSubDestinationSettings
import Network.AWS.MediaConvert.Types.EmbeddedDestinationSettings
import Network.AWS.MediaConvert.Types.ImscDestinationSettings
import Network.AWS.MediaConvert.Types.SccDestinationSettings
import Network.AWS.MediaConvert.Types.TeletextDestinationSettings
import Network.AWS.MediaConvert.Types.TtmlDestinationSettings
import qualified Network.AWS.Prelude as Prelude

-- | Specific settings required by destination type. Note that
-- burnin_destination_settings are not available if the source of the
-- caption data is Embedded or Teletext.
--
-- /See:/ 'newCaptionDestinationSettings' smart constructor.
data CaptionDestinationSettings = CaptionDestinationSettings'
  { -- | Settings specific to embedded\/ancillary caption outputs, including
    -- 608\/708 Channel destination number.
    embeddedDestinationSettings :: Prelude.Maybe EmbeddedDestinationSettings,
    -- | Specify the format for this set of captions on this output. The default
    -- format is embedded without SCTE-20. Other options are embedded with
    -- SCTE-20, burn-in, DVB-sub, IMSC, SCC, SRT, teletext, TTML, and web-VTT.
    -- If you are using SCTE-20, choose SCTE-20 plus embedded
    -- (SCTE20_PLUS_EMBEDDED) to create an output that complies with the
    -- SCTE-43 spec. To create a non-compliant output where the embedded
    -- captions come first, choose Embedded plus SCTE-20
    -- (EMBEDDED_PLUS_SCTE20).
    destinationType :: Prelude.Maybe CaptionDestinationType,
    -- | DVB-Sub Destination Settings
    dvbSubDestinationSettings :: Prelude.Maybe DvbSubDestinationSettings,
    -- | Settings for Teletext caption output
    teletextDestinationSettings :: Prelude.Maybe TeletextDestinationSettings,
    -- | Settings specific to TTML caption outputs, including Pass style
    -- information (TtmlStylePassthrough).
    ttmlDestinationSettings :: Prelude.Maybe TtmlDestinationSettings,
    -- | Burn-In Destination Settings.
    burninDestinationSettings :: Prelude.Maybe BurninDestinationSettings,
    -- | Settings specific to IMSC caption outputs.
    imscDestinationSettings :: Prelude.Maybe ImscDestinationSettings,
    -- | Settings for SCC caption output.
    sccDestinationSettings :: Prelude.Maybe SccDestinationSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CaptionDestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'embeddedDestinationSettings', 'captionDestinationSettings_embeddedDestinationSettings' - Settings specific to embedded\/ancillary caption outputs, including
-- 608\/708 Channel destination number.
--
-- 'destinationType', 'captionDestinationSettings_destinationType' - Specify the format for this set of captions on this output. The default
-- format is embedded without SCTE-20. Other options are embedded with
-- SCTE-20, burn-in, DVB-sub, IMSC, SCC, SRT, teletext, TTML, and web-VTT.
-- If you are using SCTE-20, choose SCTE-20 plus embedded
-- (SCTE20_PLUS_EMBEDDED) to create an output that complies with the
-- SCTE-43 spec. To create a non-compliant output where the embedded
-- captions come first, choose Embedded plus SCTE-20
-- (EMBEDDED_PLUS_SCTE20).
--
-- 'dvbSubDestinationSettings', 'captionDestinationSettings_dvbSubDestinationSettings' - DVB-Sub Destination Settings
--
-- 'teletextDestinationSettings', 'captionDestinationSettings_teletextDestinationSettings' - Settings for Teletext caption output
--
-- 'ttmlDestinationSettings', 'captionDestinationSettings_ttmlDestinationSettings' - Settings specific to TTML caption outputs, including Pass style
-- information (TtmlStylePassthrough).
--
-- 'burninDestinationSettings', 'captionDestinationSettings_burninDestinationSettings' - Burn-In Destination Settings.
--
-- 'imscDestinationSettings', 'captionDestinationSettings_imscDestinationSettings' - Settings specific to IMSC caption outputs.
--
-- 'sccDestinationSettings', 'captionDestinationSettings_sccDestinationSettings' - Settings for SCC caption output.
newCaptionDestinationSettings ::
  CaptionDestinationSettings
newCaptionDestinationSettings =
  CaptionDestinationSettings'
    { embeddedDestinationSettings =
        Prelude.Nothing,
      destinationType = Prelude.Nothing,
      dvbSubDestinationSettings = Prelude.Nothing,
      teletextDestinationSettings = Prelude.Nothing,
      ttmlDestinationSettings = Prelude.Nothing,
      burninDestinationSettings = Prelude.Nothing,
      imscDestinationSettings = Prelude.Nothing,
      sccDestinationSettings = Prelude.Nothing
    }

-- | Settings specific to embedded\/ancillary caption outputs, including
-- 608\/708 Channel destination number.
captionDestinationSettings_embeddedDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe EmbeddedDestinationSettings)
captionDestinationSettings_embeddedDestinationSettings = Lens.lens (\CaptionDestinationSettings' {embeddedDestinationSettings} -> embeddedDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {embeddedDestinationSettings = a} :: CaptionDestinationSettings)

-- | Specify the format for this set of captions on this output. The default
-- format is embedded without SCTE-20. Other options are embedded with
-- SCTE-20, burn-in, DVB-sub, IMSC, SCC, SRT, teletext, TTML, and web-VTT.
-- If you are using SCTE-20, choose SCTE-20 plus embedded
-- (SCTE20_PLUS_EMBEDDED) to create an output that complies with the
-- SCTE-43 spec. To create a non-compliant output where the embedded
-- captions come first, choose Embedded plus SCTE-20
-- (EMBEDDED_PLUS_SCTE20).
captionDestinationSettings_destinationType :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe CaptionDestinationType)
captionDestinationSettings_destinationType = Lens.lens (\CaptionDestinationSettings' {destinationType} -> destinationType) (\s@CaptionDestinationSettings' {} a -> s {destinationType = a} :: CaptionDestinationSettings)

-- | DVB-Sub Destination Settings
captionDestinationSettings_dvbSubDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe DvbSubDestinationSettings)
captionDestinationSettings_dvbSubDestinationSettings = Lens.lens (\CaptionDestinationSettings' {dvbSubDestinationSettings} -> dvbSubDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {dvbSubDestinationSettings = a} :: CaptionDestinationSettings)

-- | Settings for Teletext caption output
captionDestinationSettings_teletextDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe TeletextDestinationSettings)
captionDestinationSettings_teletextDestinationSettings = Lens.lens (\CaptionDestinationSettings' {teletextDestinationSettings} -> teletextDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {teletextDestinationSettings = a} :: CaptionDestinationSettings)

-- | Settings specific to TTML caption outputs, including Pass style
-- information (TtmlStylePassthrough).
captionDestinationSettings_ttmlDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe TtmlDestinationSettings)
captionDestinationSettings_ttmlDestinationSettings = Lens.lens (\CaptionDestinationSettings' {ttmlDestinationSettings} -> ttmlDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {ttmlDestinationSettings = a} :: CaptionDestinationSettings)

-- | Burn-In Destination Settings.
captionDestinationSettings_burninDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe BurninDestinationSettings)
captionDestinationSettings_burninDestinationSettings = Lens.lens (\CaptionDestinationSettings' {burninDestinationSettings} -> burninDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {burninDestinationSettings = a} :: CaptionDestinationSettings)

-- | Settings specific to IMSC caption outputs.
captionDestinationSettings_imscDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe ImscDestinationSettings)
captionDestinationSettings_imscDestinationSettings = Lens.lens (\CaptionDestinationSettings' {imscDestinationSettings} -> imscDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {imscDestinationSettings = a} :: CaptionDestinationSettings)

-- | Settings for SCC caption output.
captionDestinationSettings_sccDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe SccDestinationSettings)
captionDestinationSettings_sccDestinationSettings = Lens.lens (\CaptionDestinationSettings' {sccDestinationSettings} -> sccDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {sccDestinationSettings = a} :: CaptionDestinationSettings)

instance Prelude.FromJSON CaptionDestinationSettings where
  parseJSON =
    Prelude.withObject
      "CaptionDestinationSettings"
      ( \x ->
          CaptionDestinationSettings'
            Prelude.<$> (x Prelude..:? "embeddedDestinationSettings")
            Prelude.<*> (x Prelude..:? "destinationType")
            Prelude.<*> (x Prelude..:? "dvbSubDestinationSettings")
            Prelude.<*> (x Prelude..:? "teletextDestinationSettings")
            Prelude.<*> (x Prelude..:? "ttmlDestinationSettings")
            Prelude.<*> (x Prelude..:? "burninDestinationSettings")
            Prelude.<*> (x Prelude..:? "imscDestinationSettings")
            Prelude.<*> (x Prelude..:? "sccDestinationSettings")
      )

instance Prelude.Hashable CaptionDestinationSettings

instance Prelude.NFData CaptionDestinationSettings

instance Prelude.ToJSON CaptionDestinationSettings where
  toJSON CaptionDestinationSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("embeddedDestinationSettings" Prelude..=)
              Prelude.<$> embeddedDestinationSettings,
            ("destinationType" Prelude..=)
              Prelude.<$> destinationType,
            ("dvbSubDestinationSettings" Prelude..=)
              Prelude.<$> dvbSubDestinationSettings,
            ("teletextDestinationSettings" Prelude..=)
              Prelude.<$> teletextDestinationSettings,
            ("ttmlDestinationSettings" Prelude..=)
              Prelude.<$> ttmlDestinationSettings,
            ("burninDestinationSettings" Prelude..=)
              Prelude.<$> burninDestinationSettings,
            ("imscDestinationSettings" Prelude..=)
              Prelude.<$> imscDestinationSettings,
            ("sccDestinationSettings" Prelude..=)
              Prelude.<$> sccDestinationSettings
          ]
      )
