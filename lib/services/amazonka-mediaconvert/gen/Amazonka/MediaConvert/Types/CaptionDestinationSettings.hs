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
-- Module      : Amazonka.MediaConvert.Types.CaptionDestinationSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.CaptionDestinationSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.BurninDestinationSettings
import Amazonka.MediaConvert.Types.CaptionDestinationType
import Amazonka.MediaConvert.Types.DvbSubDestinationSettings
import Amazonka.MediaConvert.Types.EmbeddedDestinationSettings
import Amazonka.MediaConvert.Types.ImscDestinationSettings
import Amazonka.MediaConvert.Types.SccDestinationSettings
import Amazonka.MediaConvert.Types.SrtDestinationSettings
import Amazonka.MediaConvert.Types.TeletextDestinationSettings
import Amazonka.MediaConvert.Types.TtmlDestinationSettings
import Amazonka.MediaConvert.Types.WebvttDestinationSettings
import qualified Amazonka.Prelude as Prelude

-- | Settings related to one captions tab on the MediaConvert console. In
-- your job JSON, an instance of captions DestinationSettings is equivalent
-- to one captions tab in the console. Usually, one captions tab
-- corresponds to one output captions track. Depending on your output
-- captions format, one tab might correspond to a set of output captions
-- tracks. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/including-captions.html.
--
-- /See:/ 'newCaptionDestinationSettings' smart constructor.
data CaptionDestinationSettings = CaptionDestinationSettings'
  { -- | Burn-in is a captions delivery method, rather than a captions format.
    -- Burn-in writes the captions directly on your video frames, replacing
    -- pixels of video content with the captions. Set up burn-in captions in
    -- the same output as your video. For more information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/burn-in-output-captions.html.
    -- When you work directly in your JSON job specification, include this
    -- object and any required children when you set destinationType to
    -- BURN_IN.
    burninDestinationSettings :: Prelude.Maybe BurninDestinationSettings,
    -- | Specify the format for this set of captions on this output. The default
    -- format is embedded without SCTE-20. Note that your choice of video
    -- output container constrains your choice of output captions format. For
    -- more information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/captions-support-tables.html.
    -- If you are using SCTE-20 and you want to create an output that complies
    -- with the SCTE-43 spec, choose SCTE-20 plus embedded
    -- (SCTE20_PLUS_EMBEDDED). To create a non-compliant output where the
    -- embedded captions come first, choose Embedded plus SCTE-20
    -- (EMBEDDED_PLUS_SCTE20).
    destinationType :: Prelude.Maybe CaptionDestinationType,
    -- | Settings related to DVB-Sub captions. Set up DVB-Sub captions in the
    -- same output as your video. For more information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/dvb-sub-output-captions.html.
    -- When you work directly in your JSON job specification, include this
    -- object and any required children when you set destinationType to
    -- DVB_SUB.
    dvbSubDestinationSettings :: Prelude.Maybe DvbSubDestinationSettings,
    -- | Settings related to CEA\/EIA-608 and CEA\/EIA-708 (also called embedded
    -- or ancillary) captions. Set up embedded captions in the same output as
    -- your video. For more information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/embedded-output-captions.html.
    -- When you work directly in your JSON job specification, include this
    -- object and any required children when you set destinationType to
    -- EMBEDDED, EMBEDDED_PLUS_SCTE20, or SCTE20_PLUS_EMBEDDED.
    embeddedDestinationSettings :: Prelude.Maybe EmbeddedDestinationSettings,
    -- | Settings related to IMSC captions. IMSC is a sidecar format that holds
    -- captions in a file that is separate from the video container. Set up
    -- sidecar captions in the same output group, but different output from
    -- your video. For more information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/ttml-and-webvtt-output-captions.html.
    -- When you work directly in your JSON job specification, include this
    -- object and any required children when you set destinationType to IMSC.
    imscDestinationSettings :: Prelude.Maybe ImscDestinationSettings,
    -- | Settings related to SCC captions. SCC is a sidecar format that holds
    -- captions in a file that is separate from the video container. Set up
    -- sidecar captions in the same output group, but different output from
    -- your video. For more information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/scc-srt-output-captions.html.
    -- When you work directly in your JSON job specification, include this
    -- object and any required children when you set destinationType to SCC.
    sccDestinationSettings :: Prelude.Maybe SccDestinationSettings,
    -- | Settings related to SRT captions. SRT is a sidecar format that holds
    -- captions in a file that is separate from the video container. Set up
    -- sidecar captions in the same output group, but different output from
    -- your video. When you work directly in your JSON job specification,
    -- include this object and any required children when you set
    -- destinationType to SRT.
    srtDestinationSettings :: Prelude.Maybe SrtDestinationSettings,
    -- | Settings related to teletext captions. Set up teletext captions in the
    -- same output as your video. For more information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/teletext-output-captions.html.
    -- When you work directly in your JSON job specification, include this
    -- object and any required children when you set destinationType to
    -- TELETEXT.
    teletextDestinationSettings :: Prelude.Maybe TeletextDestinationSettings,
    -- | Settings related to TTML captions. TTML is a sidecar format that holds
    -- captions in a file that is separate from the video container. Set up
    -- sidecar captions in the same output group, but different output from
    -- your video. For more information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/ttml-and-webvtt-output-captions.html.
    -- When you work directly in your JSON job specification, include this
    -- object and any required children when you set destinationType to TTML.
    ttmlDestinationSettings :: Prelude.Maybe TtmlDestinationSettings,
    -- | Settings related to WebVTT captions. WebVTT is a sidecar format that
    -- holds captions in a file that is separate from the video container. Set
    -- up sidecar captions in the same output group, but different output from
    -- your video. For more information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/ttml-and-webvtt-output-captions.html.
    -- When you work directly in your JSON job specification, include this
    -- object and any required children when you set destinationType to WebVTT.
    webvttDestinationSettings :: Prelude.Maybe WebvttDestinationSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CaptionDestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'burninDestinationSettings', 'captionDestinationSettings_burninDestinationSettings' - Burn-in is a captions delivery method, rather than a captions format.
-- Burn-in writes the captions directly on your video frames, replacing
-- pixels of video content with the captions. Set up burn-in captions in
-- the same output as your video. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/burn-in-output-captions.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set destinationType to
-- BURN_IN.
--
-- 'destinationType', 'captionDestinationSettings_destinationType' - Specify the format for this set of captions on this output. The default
-- format is embedded without SCTE-20. Note that your choice of video
-- output container constrains your choice of output captions format. For
-- more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/captions-support-tables.html.
-- If you are using SCTE-20 and you want to create an output that complies
-- with the SCTE-43 spec, choose SCTE-20 plus embedded
-- (SCTE20_PLUS_EMBEDDED). To create a non-compliant output where the
-- embedded captions come first, choose Embedded plus SCTE-20
-- (EMBEDDED_PLUS_SCTE20).
--
-- 'dvbSubDestinationSettings', 'captionDestinationSettings_dvbSubDestinationSettings' - Settings related to DVB-Sub captions. Set up DVB-Sub captions in the
-- same output as your video. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/dvb-sub-output-captions.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set destinationType to
-- DVB_SUB.
--
-- 'embeddedDestinationSettings', 'captionDestinationSettings_embeddedDestinationSettings' - Settings related to CEA\/EIA-608 and CEA\/EIA-708 (also called embedded
-- or ancillary) captions. Set up embedded captions in the same output as
-- your video. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/embedded-output-captions.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set destinationType to
-- EMBEDDED, EMBEDDED_PLUS_SCTE20, or SCTE20_PLUS_EMBEDDED.
--
-- 'imscDestinationSettings', 'captionDestinationSettings_imscDestinationSettings' - Settings related to IMSC captions. IMSC is a sidecar format that holds
-- captions in a file that is separate from the video container. Set up
-- sidecar captions in the same output group, but different output from
-- your video. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/ttml-and-webvtt-output-captions.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set destinationType to IMSC.
--
-- 'sccDestinationSettings', 'captionDestinationSettings_sccDestinationSettings' - Settings related to SCC captions. SCC is a sidecar format that holds
-- captions in a file that is separate from the video container. Set up
-- sidecar captions in the same output group, but different output from
-- your video. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/scc-srt-output-captions.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set destinationType to SCC.
--
-- 'srtDestinationSettings', 'captionDestinationSettings_srtDestinationSettings' - Settings related to SRT captions. SRT is a sidecar format that holds
-- captions in a file that is separate from the video container. Set up
-- sidecar captions in the same output group, but different output from
-- your video. When you work directly in your JSON job specification,
-- include this object and any required children when you set
-- destinationType to SRT.
--
-- 'teletextDestinationSettings', 'captionDestinationSettings_teletextDestinationSettings' - Settings related to teletext captions. Set up teletext captions in the
-- same output as your video. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/teletext-output-captions.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set destinationType to
-- TELETEXT.
--
-- 'ttmlDestinationSettings', 'captionDestinationSettings_ttmlDestinationSettings' - Settings related to TTML captions. TTML is a sidecar format that holds
-- captions in a file that is separate from the video container. Set up
-- sidecar captions in the same output group, but different output from
-- your video. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/ttml-and-webvtt-output-captions.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set destinationType to TTML.
--
-- 'webvttDestinationSettings', 'captionDestinationSettings_webvttDestinationSettings' - Settings related to WebVTT captions. WebVTT is a sidecar format that
-- holds captions in a file that is separate from the video container. Set
-- up sidecar captions in the same output group, but different output from
-- your video. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/ttml-and-webvtt-output-captions.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set destinationType to WebVTT.
newCaptionDestinationSettings ::
  CaptionDestinationSettings
newCaptionDestinationSettings =
  CaptionDestinationSettings'
    { burninDestinationSettings =
        Prelude.Nothing,
      destinationType = Prelude.Nothing,
      dvbSubDestinationSettings = Prelude.Nothing,
      embeddedDestinationSettings = Prelude.Nothing,
      imscDestinationSettings = Prelude.Nothing,
      sccDestinationSettings = Prelude.Nothing,
      srtDestinationSettings = Prelude.Nothing,
      teletextDestinationSettings = Prelude.Nothing,
      ttmlDestinationSettings = Prelude.Nothing,
      webvttDestinationSettings = Prelude.Nothing
    }

-- | Burn-in is a captions delivery method, rather than a captions format.
-- Burn-in writes the captions directly on your video frames, replacing
-- pixels of video content with the captions. Set up burn-in captions in
-- the same output as your video. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/burn-in-output-captions.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set destinationType to
-- BURN_IN.
captionDestinationSettings_burninDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe BurninDestinationSettings)
captionDestinationSettings_burninDestinationSettings = Lens.lens (\CaptionDestinationSettings' {burninDestinationSettings} -> burninDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {burninDestinationSettings = a} :: CaptionDestinationSettings)

-- | Specify the format for this set of captions on this output. The default
-- format is embedded without SCTE-20. Note that your choice of video
-- output container constrains your choice of output captions format. For
-- more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/captions-support-tables.html.
-- If you are using SCTE-20 and you want to create an output that complies
-- with the SCTE-43 spec, choose SCTE-20 plus embedded
-- (SCTE20_PLUS_EMBEDDED). To create a non-compliant output where the
-- embedded captions come first, choose Embedded plus SCTE-20
-- (EMBEDDED_PLUS_SCTE20).
captionDestinationSettings_destinationType :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe CaptionDestinationType)
captionDestinationSettings_destinationType = Lens.lens (\CaptionDestinationSettings' {destinationType} -> destinationType) (\s@CaptionDestinationSettings' {} a -> s {destinationType = a} :: CaptionDestinationSettings)

-- | Settings related to DVB-Sub captions. Set up DVB-Sub captions in the
-- same output as your video. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/dvb-sub-output-captions.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set destinationType to
-- DVB_SUB.
captionDestinationSettings_dvbSubDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe DvbSubDestinationSettings)
captionDestinationSettings_dvbSubDestinationSettings = Lens.lens (\CaptionDestinationSettings' {dvbSubDestinationSettings} -> dvbSubDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {dvbSubDestinationSettings = a} :: CaptionDestinationSettings)

-- | Settings related to CEA\/EIA-608 and CEA\/EIA-708 (also called embedded
-- or ancillary) captions. Set up embedded captions in the same output as
-- your video. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/embedded-output-captions.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set destinationType to
-- EMBEDDED, EMBEDDED_PLUS_SCTE20, or SCTE20_PLUS_EMBEDDED.
captionDestinationSettings_embeddedDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe EmbeddedDestinationSettings)
captionDestinationSettings_embeddedDestinationSettings = Lens.lens (\CaptionDestinationSettings' {embeddedDestinationSettings} -> embeddedDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {embeddedDestinationSettings = a} :: CaptionDestinationSettings)

-- | Settings related to IMSC captions. IMSC is a sidecar format that holds
-- captions in a file that is separate from the video container. Set up
-- sidecar captions in the same output group, but different output from
-- your video. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/ttml-and-webvtt-output-captions.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set destinationType to IMSC.
captionDestinationSettings_imscDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe ImscDestinationSettings)
captionDestinationSettings_imscDestinationSettings = Lens.lens (\CaptionDestinationSettings' {imscDestinationSettings} -> imscDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {imscDestinationSettings = a} :: CaptionDestinationSettings)

-- | Settings related to SCC captions. SCC is a sidecar format that holds
-- captions in a file that is separate from the video container. Set up
-- sidecar captions in the same output group, but different output from
-- your video. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/scc-srt-output-captions.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set destinationType to SCC.
captionDestinationSettings_sccDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe SccDestinationSettings)
captionDestinationSettings_sccDestinationSettings = Lens.lens (\CaptionDestinationSettings' {sccDestinationSettings} -> sccDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {sccDestinationSettings = a} :: CaptionDestinationSettings)

-- | Settings related to SRT captions. SRT is a sidecar format that holds
-- captions in a file that is separate from the video container. Set up
-- sidecar captions in the same output group, but different output from
-- your video. When you work directly in your JSON job specification,
-- include this object and any required children when you set
-- destinationType to SRT.
captionDestinationSettings_srtDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe SrtDestinationSettings)
captionDestinationSettings_srtDestinationSettings = Lens.lens (\CaptionDestinationSettings' {srtDestinationSettings} -> srtDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {srtDestinationSettings = a} :: CaptionDestinationSettings)

-- | Settings related to teletext captions. Set up teletext captions in the
-- same output as your video. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/teletext-output-captions.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set destinationType to
-- TELETEXT.
captionDestinationSettings_teletextDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe TeletextDestinationSettings)
captionDestinationSettings_teletextDestinationSettings = Lens.lens (\CaptionDestinationSettings' {teletextDestinationSettings} -> teletextDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {teletextDestinationSettings = a} :: CaptionDestinationSettings)

-- | Settings related to TTML captions. TTML is a sidecar format that holds
-- captions in a file that is separate from the video container. Set up
-- sidecar captions in the same output group, but different output from
-- your video. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/ttml-and-webvtt-output-captions.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set destinationType to TTML.
captionDestinationSettings_ttmlDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe TtmlDestinationSettings)
captionDestinationSettings_ttmlDestinationSettings = Lens.lens (\CaptionDestinationSettings' {ttmlDestinationSettings} -> ttmlDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {ttmlDestinationSettings = a} :: CaptionDestinationSettings)

-- | Settings related to WebVTT captions. WebVTT is a sidecar format that
-- holds captions in a file that is separate from the video container. Set
-- up sidecar captions in the same output group, but different output from
-- your video. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/ttml-and-webvtt-output-captions.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set destinationType to WebVTT.
captionDestinationSettings_webvttDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe WebvttDestinationSettings)
captionDestinationSettings_webvttDestinationSettings = Lens.lens (\CaptionDestinationSettings' {webvttDestinationSettings} -> webvttDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {webvttDestinationSettings = a} :: CaptionDestinationSettings)

instance Data.FromJSON CaptionDestinationSettings where
  parseJSON =
    Data.withObject
      "CaptionDestinationSettings"
      ( \x ->
          CaptionDestinationSettings'
            Prelude.<$> (x Data..:? "burninDestinationSettings")
            Prelude.<*> (x Data..:? "destinationType")
            Prelude.<*> (x Data..:? "dvbSubDestinationSettings")
            Prelude.<*> (x Data..:? "embeddedDestinationSettings")
            Prelude.<*> (x Data..:? "imscDestinationSettings")
            Prelude.<*> (x Data..:? "sccDestinationSettings")
            Prelude.<*> (x Data..:? "srtDestinationSettings")
            Prelude.<*> (x Data..:? "teletextDestinationSettings")
            Prelude.<*> (x Data..:? "ttmlDestinationSettings")
            Prelude.<*> (x Data..:? "webvttDestinationSettings")
      )

instance Prelude.Hashable CaptionDestinationSettings where
  hashWithSalt _salt CaptionDestinationSettings' {..} =
    _salt
      `Prelude.hashWithSalt` burninDestinationSettings
      `Prelude.hashWithSalt` destinationType
      `Prelude.hashWithSalt` dvbSubDestinationSettings
      `Prelude.hashWithSalt` embeddedDestinationSettings
      `Prelude.hashWithSalt` imscDestinationSettings
      `Prelude.hashWithSalt` sccDestinationSettings
      `Prelude.hashWithSalt` srtDestinationSettings
      `Prelude.hashWithSalt` teletextDestinationSettings
      `Prelude.hashWithSalt` ttmlDestinationSettings
      `Prelude.hashWithSalt` webvttDestinationSettings

instance Prelude.NFData CaptionDestinationSettings where
  rnf CaptionDestinationSettings' {..} =
    Prelude.rnf burninDestinationSettings
      `Prelude.seq` Prelude.rnf destinationType
      `Prelude.seq` Prelude.rnf dvbSubDestinationSettings
      `Prelude.seq` Prelude.rnf embeddedDestinationSettings
      `Prelude.seq` Prelude.rnf imscDestinationSettings
      `Prelude.seq` Prelude.rnf sccDestinationSettings
      `Prelude.seq` Prelude.rnf srtDestinationSettings
      `Prelude.seq` Prelude.rnf teletextDestinationSettings
      `Prelude.seq` Prelude.rnf ttmlDestinationSettings
      `Prelude.seq` Prelude.rnf webvttDestinationSettings

instance Data.ToJSON CaptionDestinationSettings where
  toJSON CaptionDestinationSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("burninDestinationSettings" Data..=)
              Prelude.<$> burninDestinationSettings,
            ("destinationType" Data..=)
              Prelude.<$> destinationType,
            ("dvbSubDestinationSettings" Data..=)
              Prelude.<$> dvbSubDestinationSettings,
            ("embeddedDestinationSettings" Data..=)
              Prelude.<$> embeddedDestinationSettings,
            ("imscDestinationSettings" Data..=)
              Prelude.<$> imscDestinationSettings,
            ("sccDestinationSettings" Data..=)
              Prelude.<$> sccDestinationSettings,
            ("srtDestinationSettings" Data..=)
              Prelude.<$> srtDestinationSettings,
            ("teletextDestinationSettings" Data..=)
              Prelude.<$> teletextDestinationSettings,
            ("ttmlDestinationSettings" Data..=)
              Prelude.<$> ttmlDestinationSettings,
            ("webvttDestinationSettings" Data..=)
              Prelude.<$> webvttDestinationSettings
          ]
      )
