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
-- Module      : Network.AWS.MediaLive.Types.CaptionDestinationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.CaptionDestinationSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AribDestinationSettings
import Network.AWS.MediaLive.Types.BurnInDestinationSettings
import Network.AWS.MediaLive.Types.DvbSubDestinationSettings
import Network.AWS.MediaLive.Types.EbuTtDDestinationSettings
import Network.AWS.MediaLive.Types.EmbeddedDestinationSettings
import Network.AWS.MediaLive.Types.EmbeddedPlusScte20DestinationSettings
import Network.AWS.MediaLive.Types.RtmpCaptionInfoDestinationSettings
import Network.AWS.MediaLive.Types.Scte20PlusEmbeddedDestinationSettings
import Network.AWS.MediaLive.Types.Scte27DestinationSettings
import Network.AWS.MediaLive.Types.SmpteTtDestinationSettings
import Network.AWS.MediaLive.Types.TeletextDestinationSettings
import Network.AWS.MediaLive.Types.TtmlDestinationSettings
import Network.AWS.MediaLive.Types.WebvttDestinationSettings
import qualified Network.AWS.Prelude as Prelude

-- | Caption Destination Settings
--
-- /See:/ 'newCaptionDestinationSettings' smart constructor.
data CaptionDestinationSettings = CaptionDestinationSettings'
  { webvttDestinationSettings :: Prelude.Maybe WebvttDestinationSettings,
    embeddedDestinationSettings :: Prelude.Maybe EmbeddedDestinationSettings,
    aribDestinationSettings :: Prelude.Maybe AribDestinationSettings,
    scte20PlusEmbeddedDestinationSettings :: Prelude.Maybe Scte20PlusEmbeddedDestinationSettings,
    embeddedPlusScte20DestinationSettings :: Prelude.Maybe EmbeddedPlusScte20DestinationSettings,
    dvbSubDestinationSettings :: Prelude.Maybe DvbSubDestinationSettings,
    scte27DestinationSettings :: Prelude.Maybe Scte27DestinationSettings,
    rtmpCaptionInfoDestinationSettings :: Prelude.Maybe RtmpCaptionInfoDestinationSettings,
    ebuTtDDestinationSettings :: Prelude.Maybe EbuTtDDestinationSettings,
    teletextDestinationSettings :: Prelude.Maybe TeletextDestinationSettings,
    smpteTtDestinationSettings :: Prelude.Maybe SmpteTtDestinationSettings,
    ttmlDestinationSettings :: Prelude.Maybe TtmlDestinationSettings,
    burnInDestinationSettings :: Prelude.Maybe BurnInDestinationSettings
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
-- 'webvttDestinationSettings', 'captionDestinationSettings_webvttDestinationSettings' - Undocumented member.
--
-- 'embeddedDestinationSettings', 'captionDestinationSettings_embeddedDestinationSettings' - Undocumented member.
--
-- 'aribDestinationSettings', 'captionDestinationSettings_aribDestinationSettings' - Undocumented member.
--
-- 'scte20PlusEmbeddedDestinationSettings', 'captionDestinationSettings_scte20PlusEmbeddedDestinationSettings' - Undocumented member.
--
-- 'embeddedPlusScte20DestinationSettings', 'captionDestinationSettings_embeddedPlusScte20DestinationSettings' - Undocumented member.
--
-- 'dvbSubDestinationSettings', 'captionDestinationSettings_dvbSubDestinationSettings' - Undocumented member.
--
-- 'scte27DestinationSettings', 'captionDestinationSettings_scte27DestinationSettings' - Undocumented member.
--
-- 'rtmpCaptionInfoDestinationSettings', 'captionDestinationSettings_rtmpCaptionInfoDestinationSettings' - Undocumented member.
--
-- 'ebuTtDDestinationSettings', 'captionDestinationSettings_ebuTtDDestinationSettings' - Undocumented member.
--
-- 'teletextDestinationSettings', 'captionDestinationSettings_teletextDestinationSettings' - Undocumented member.
--
-- 'smpteTtDestinationSettings', 'captionDestinationSettings_smpteTtDestinationSettings' - Undocumented member.
--
-- 'ttmlDestinationSettings', 'captionDestinationSettings_ttmlDestinationSettings' - Undocumented member.
--
-- 'burnInDestinationSettings', 'captionDestinationSettings_burnInDestinationSettings' - Undocumented member.
newCaptionDestinationSettings ::
  CaptionDestinationSettings
newCaptionDestinationSettings =
  CaptionDestinationSettings'
    { webvttDestinationSettings =
        Prelude.Nothing,
      embeddedDestinationSettings = Prelude.Nothing,
      aribDestinationSettings = Prelude.Nothing,
      scte20PlusEmbeddedDestinationSettings =
        Prelude.Nothing,
      embeddedPlusScte20DestinationSettings =
        Prelude.Nothing,
      dvbSubDestinationSettings = Prelude.Nothing,
      scte27DestinationSettings = Prelude.Nothing,
      rtmpCaptionInfoDestinationSettings =
        Prelude.Nothing,
      ebuTtDDestinationSettings = Prelude.Nothing,
      teletextDestinationSettings = Prelude.Nothing,
      smpteTtDestinationSettings = Prelude.Nothing,
      ttmlDestinationSettings = Prelude.Nothing,
      burnInDestinationSettings = Prelude.Nothing
    }

-- | Undocumented member.
captionDestinationSettings_webvttDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe WebvttDestinationSettings)
captionDestinationSettings_webvttDestinationSettings = Lens.lens (\CaptionDestinationSettings' {webvttDestinationSettings} -> webvttDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {webvttDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_embeddedDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe EmbeddedDestinationSettings)
captionDestinationSettings_embeddedDestinationSettings = Lens.lens (\CaptionDestinationSettings' {embeddedDestinationSettings} -> embeddedDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {embeddedDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_aribDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe AribDestinationSettings)
captionDestinationSettings_aribDestinationSettings = Lens.lens (\CaptionDestinationSettings' {aribDestinationSettings} -> aribDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {aribDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_scte20PlusEmbeddedDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe Scte20PlusEmbeddedDestinationSettings)
captionDestinationSettings_scte20PlusEmbeddedDestinationSettings = Lens.lens (\CaptionDestinationSettings' {scte20PlusEmbeddedDestinationSettings} -> scte20PlusEmbeddedDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {scte20PlusEmbeddedDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_embeddedPlusScte20DestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe EmbeddedPlusScte20DestinationSettings)
captionDestinationSettings_embeddedPlusScte20DestinationSettings = Lens.lens (\CaptionDestinationSettings' {embeddedPlusScte20DestinationSettings} -> embeddedPlusScte20DestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {embeddedPlusScte20DestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_dvbSubDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe DvbSubDestinationSettings)
captionDestinationSettings_dvbSubDestinationSettings = Lens.lens (\CaptionDestinationSettings' {dvbSubDestinationSettings} -> dvbSubDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {dvbSubDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_scte27DestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe Scte27DestinationSettings)
captionDestinationSettings_scte27DestinationSettings = Lens.lens (\CaptionDestinationSettings' {scte27DestinationSettings} -> scte27DestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {scte27DestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_rtmpCaptionInfoDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe RtmpCaptionInfoDestinationSettings)
captionDestinationSettings_rtmpCaptionInfoDestinationSettings = Lens.lens (\CaptionDestinationSettings' {rtmpCaptionInfoDestinationSettings} -> rtmpCaptionInfoDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {rtmpCaptionInfoDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_ebuTtDDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe EbuTtDDestinationSettings)
captionDestinationSettings_ebuTtDDestinationSettings = Lens.lens (\CaptionDestinationSettings' {ebuTtDDestinationSettings} -> ebuTtDDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {ebuTtDDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_teletextDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe TeletextDestinationSettings)
captionDestinationSettings_teletextDestinationSettings = Lens.lens (\CaptionDestinationSettings' {teletextDestinationSettings} -> teletextDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {teletextDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_smpteTtDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe SmpteTtDestinationSettings)
captionDestinationSettings_smpteTtDestinationSettings = Lens.lens (\CaptionDestinationSettings' {smpteTtDestinationSettings} -> smpteTtDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {smpteTtDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_ttmlDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe TtmlDestinationSettings)
captionDestinationSettings_ttmlDestinationSettings = Lens.lens (\CaptionDestinationSettings' {ttmlDestinationSettings} -> ttmlDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {ttmlDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_burnInDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe BurnInDestinationSettings)
captionDestinationSettings_burnInDestinationSettings = Lens.lens (\CaptionDestinationSettings' {burnInDestinationSettings} -> burnInDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {burnInDestinationSettings = a} :: CaptionDestinationSettings)

instance Prelude.FromJSON CaptionDestinationSettings where
  parseJSON =
    Prelude.withObject
      "CaptionDestinationSettings"
      ( \x ->
          CaptionDestinationSettings'
            Prelude.<$> (x Prelude..:? "webvttDestinationSettings")
            Prelude.<*> (x Prelude..:? "embeddedDestinationSettings")
            Prelude.<*> (x Prelude..:? "aribDestinationSettings")
            Prelude.<*> ( x
                            Prelude..:? "scte20PlusEmbeddedDestinationSettings"
                        )
            Prelude.<*> ( x
                            Prelude..:? "embeddedPlusScte20DestinationSettings"
                        )
            Prelude.<*> (x Prelude..:? "dvbSubDestinationSettings")
            Prelude.<*> (x Prelude..:? "scte27DestinationSettings")
            Prelude.<*> (x Prelude..:? "rtmpCaptionInfoDestinationSettings")
            Prelude.<*> (x Prelude..:? "ebuTtDDestinationSettings")
            Prelude.<*> (x Prelude..:? "teletextDestinationSettings")
            Prelude.<*> (x Prelude..:? "smpteTtDestinationSettings")
            Prelude.<*> (x Prelude..:? "ttmlDestinationSettings")
            Prelude.<*> (x Prelude..:? "burnInDestinationSettings")
      )

instance Prelude.Hashable CaptionDestinationSettings

instance Prelude.NFData CaptionDestinationSettings

instance Prelude.ToJSON CaptionDestinationSettings where
  toJSON CaptionDestinationSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("webvttDestinationSettings" Prelude..=)
              Prelude.<$> webvttDestinationSettings,
            ("embeddedDestinationSettings" Prelude..=)
              Prelude.<$> embeddedDestinationSettings,
            ("aribDestinationSettings" Prelude..=)
              Prelude.<$> aribDestinationSettings,
            ("scte20PlusEmbeddedDestinationSettings" Prelude..=)
              Prelude.<$> scte20PlusEmbeddedDestinationSettings,
            ("embeddedPlusScte20DestinationSettings" Prelude..=)
              Prelude.<$> embeddedPlusScte20DestinationSettings,
            ("dvbSubDestinationSettings" Prelude..=)
              Prelude.<$> dvbSubDestinationSettings,
            ("scte27DestinationSettings" Prelude..=)
              Prelude.<$> scte27DestinationSettings,
            ("rtmpCaptionInfoDestinationSettings" Prelude..=)
              Prelude.<$> rtmpCaptionInfoDestinationSettings,
            ("ebuTtDDestinationSettings" Prelude..=)
              Prelude.<$> ebuTtDDestinationSettings,
            ("teletextDestinationSettings" Prelude..=)
              Prelude.<$> teletextDestinationSettings,
            ("smpteTtDestinationSettings" Prelude..=)
              Prelude.<$> smpteTtDestinationSettings,
            ("ttmlDestinationSettings" Prelude..=)
              Prelude.<$> ttmlDestinationSettings,
            ("burnInDestinationSettings" Prelude..=)
              Prelude.<$> burnInDestinationSettings
          ]
      )
