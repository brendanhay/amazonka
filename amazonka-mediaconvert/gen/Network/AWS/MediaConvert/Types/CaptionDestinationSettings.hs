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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.BurninDestinationSettings
import Network.AWS.MediaConvert.Types.CaptionDestinationType
import Network.AWS.MediaConvert.Types.DvbSubDestinationSettings
import Network.AWS.MediaConvert.Types.EmbeddedDestinationSettings
import Network.AWS.MediaConvert.Types.ImscDestinationSettings
import Network.AWS.MediaConvert.Types.SccDestinationSettings
import Network.AWS.MediaConvert.Types.TeletextDestinationSettings
import Network.AWS.MediaConvert.Types.TtmlDestinationSettings

-- | Specific settings required by destination type. Note that
-- burnin_destination_settings are not available if the source of the
-- caption data is Embedded or Teletext.
--
-- /See:/ 'newCaptionDestinationSettings' smart constructor.
data CaptionDestinationSettings = CaptionDestinationSettings'
  { -- | Settings specific to embedded\/ancillary caption outputs, including
    -- 608\/708 Channel destination number.
    embeddedDestinationSettings :: Core.Maybe EmbeddedDestinationSettings,
    -- | Specify the format for this set of captions on this output. The default
    -- format is embedded without SCTE-20. Other options are embedded with
    -- SCTE-20, burn-in, DVB-sub, IMSC, SCC, SRT, teletext, TTML, and web-VTT.
    -- If you are using SCTE-20, choose SCTE-20 plus embedded
    -- (SCTE20_PLUS_EMBEDDED) to create an output that complies with the
    -- SCTE-43 spec. To create a non-compliant output where the embedded
    -- captions come first, choose Embedded plus SCTE-20
    -- (EMBEDDED_PLUS_SCTE20).
    destinationType :: Core.Maybe CaptionDestinationType,
    -- | DVB-Sub Destination Settings
    dvbSubDestinationSettings :: Core.Maybe DvbSubDestinationSettings,
    -- | Settings for Teletext caption output
    teletextDestinationSettings :: Core.Maybe TeletextDestinationSettings,
    -- | Settings specific to TTML caption outputs, including Pass style
    -- information (TtmlStylePassthrough).
    ttmlDestinationSettings :: Core.Maybe TtmlDestinationSettings,
    -- | Burn-In Destination Settings.
    burninDestinationSettings :: Core.Maybe BurninDestinationSettings,
    -- | Settings specific to IMSC caption outputs.
    imscDestinationSettings :: Core.Maybe ImscDestinationSettings,
    -- | Settings for SCC caption output.
    sccDestinationSettings :: Core.Maybe SccDestinationSettings
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      destinationType = Core.Nothing,
      dvbSubDestinationSettings = Core.Nothing,
      teletextDestinationSettings = Core.Nothing,
      ttmlDestinationSettings = Core.Nothing,
      burninDestinationSettings = Core.Nothing,
      imscDestinationSettings = Core.Nothing,
      sccDestinationSettings = Core.Nothing
    }

-- | Settings specific to embedded\/ancillary caption outputs, including
-- 608\/708 Channel destination number.
captionDestinationSettings_embeddedDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe EmbeddedDestinationSettings)
captionDestinationSettings_embeddedDestinationSettings = Lens.lens (\CaptionDestinationSettings' {embeddedDestinationSettings} -> embeddedDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {embeddedDestinationSettings = a} :: CaptionDestinationSettings)

-- | Specify the format for this set of captions on this output. The default
-- format is embedded without SCTE-20. Other options are embedded with
-- SCTE-20, burn-in, DVB-sub, IMSC, SCC, SRT, teletext, TTML, and web-VTT.
-- If you are using SCTE-20, choose SCTE-20 plus embedded
-- (SCTE20_PLUS_EMBEDDED) to create an output that complies with the
-- SCTE-43 spec. To create a non-compliant output where the embedded
-- captions come first, choose Embedded plus SCTE-20
-- (EMBEDDED_PLUS_SCTE20).
captionDestinationSettings_destinationType :: Lens.Lens' CaptionDestinationSettings (Core.Maybe CaptionDestinationType)
captionDestinationSettings_destinationType = Lens.lens (\CaptionDestinationSettings' {destinationType} -> destinationType) (\s@CaptionDestinationSettings' {} a -> s {destinationType = a} :: CaptionDestinationSettings)

-- | DVB-Sub Destination Settings
captionDestinationSettings_dvbSubDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe DvbSubDestinationSettings)
captionDestinationSettings_dvbSubDestinationSettings = Lens.lens (\CaptionDestinationSettings' {dvbSubDestinationSettings} -> dvbSubDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {dvbSubDestinationSettings = a} :: CaptionDestinationSettings)

-- | Settings for Teletext caption output
captionDestinationSettings_teletextDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe TeletextDestinationSettings)
captionDestinationSettings_teletextDestinationSettings = Lens.lens (\CaptionDestinationSettings' {teletextDestinationSettings} -> teletextDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {teletextDestinationSettings = a} :: CaptionDestinationSettings)

-- | Settings specific to TTML caption outputs, including Pass style
-- information (TtmlStylePassthrough).
captionDestinationSettings_ttmlDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe TtmlDestinationSettings)
captionDestinationSettings_ttmlDestinationSettings = Lens.lens (\CaptionDestinationSettings' {ttmlDestinationSettings} -> ttmlDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {ttmlDestinationSettings = a} :: CaptionDestinationSettings)

-- | Burn-In Destination Settings.
captionDestinationSettings_burninDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe BurninDestinationSettings)
captionDestinationSettings_burninDestinationSettings = Lens.lens (\CaptionDestinationSettings' {burninDestinationSettings} -> burninDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {burninDestinationSettings = a} :: CaptionDestinationSettings)

-- | Settings specific to IMSC caption outputs.
captionDestinationSettings_imscDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe ImscDestinationSettings)
captionDestinationSettings_imscDestinationSettings = Lens.lens (\CaptionDestinationSettings' {imscDestinationSettings} -> imscDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {imscDestinationSettings = a} :: CaptionDestinationSettings)

-- | Settings for SCC caption output.
captionDestinationSettings_sccDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe SccDestinationSettings)
captionDestinationSettings_sccDestinationSettings = Lens.lens (\CaptionDestinationSettings' {sccDestinationSettings} -> sccDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {sccDestinationSettings = a} :: CaptionDestinationSettings)

instance Core.FromJSON CaptionDestinationSettings where
  parseJSON =
    Core.withObject
      "CaptionDestinationSettings"
      ( \x ->
          CaptionDestinationSettings'
            Core.<$> (x Core..:? "embeddedDestinationSettings")
            Core.<*> (x Core..:? "destinationType")
            Core.<*> (x Core..:? "dvbSubDestinationSettings")
            Core.<*> (x Core..:? "teletextDestinationSettings")
            Core.<*> (x Core..:? "ttmlDestinationSettings")
            Core.<*> (x Core..:? "burninDestinationSettings")
            Core.<*> (x Core..:? "imscDestinationSettings")
            Core.<*> (x Core..:? "sccDestinationSettings")
      )

instance Core.Hashable CaptionDestinationSettings

instance Core.NFData CaptionDestinationSettings

instance Core.ToJSON CaptionDestinationSettings where
  toJSON CaptionDestinationSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("embeddedDestinationSettings" Core..=)
              Core.<$> embeddedDestinationSettings,
            ("destinationType" Core..=) Core.<$> destinationType,
            ("dvbSubDestinationSettings" Core..=)
              Core.<$> dvbSubDestinationSettings,
            ("teletextDestinationSettings" Core..=)
              Core.<$> teletextDestinationSettings,
            ("ttmlDestinationSettings" Core..=)
              Core.<$> ttmlDestinationSettings,
            ("burninDestinationSettings" Core..=)
              Core.<$> burninDestinationSettings,
            ("imscDestinationSettings" Core..=)
              Core.<$> imscDestinationSettings,
            ("sccDestinationSettings" Core..=)
              Core.<$> sccDestinationSettings
          ]
      )
