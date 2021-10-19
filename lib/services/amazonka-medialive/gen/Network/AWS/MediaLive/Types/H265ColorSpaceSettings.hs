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
-- Module      : Network.AWS.MediaLive.Types.H265ColorSpaceSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265ColorSpaceSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.ColorSpacePassthroughSettings
import Network.AWS.MediaLive.Types.Hdr10Settings
import Network.AWS.MediaLive.Types.Rec601Settings
import Network.AWS.MediaLive.Types.Rec709Settings
import qualified Network.AWS.Prelude as Prelude

-- | H265 Color Space Settings
--
-- /See:/ 'newH265ColorSpaceSettings' smart constructor.
data H265ColorSpaceSettings = H265ColorSpaceSettings'
  { hdr10Settings :: Prelude.Maybe Hdr10Settings,
    rec709Settings :: Prelude.Maybe Rec709Settings,
    rec601Settings :: Prelude.Maybe Rec601Settings,
    colorSpacePassthroughSettings :: Prelude.Maybe ColorSpacePassthroughSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'H265ColorSpaceSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hdr10Settings', 'h265ColorSpaceSettings_hdr10Settings' - Undocumented member.
--
-- 'rec709Settings', 'h265ColorSpaceSettings_rec709Settings' - Undocumented member.
--
-- 'rec601Settings', 'h265ColorSpaceSettings_rec601Settings' - Undocumented member.
--
-- 'colorSpacePassthroughSettings', 'h265ColorSpaceSettings_colorSpacePassthroughSettings' - Undocumented member.
newH265ColorSpaceSettings ::
  H265ColorSpaceSettings
newH265ColorSpaceSettings =
  H265ColorSpaceSettings'
    { hdr10Settings =
        Prelude.Nothing,
      rec709Settings = Prelude.Nothing,
      rec601Settings = Prelude.Nothing,
      colorSpacePassthroughSettings = Prelude.Nothing
    }

-- | Undocumented member.
h265ColorSpaceSettings_hdr10Settings :: Lens.Lens' H265ColorSpaceSettings (Prelude.Maybe Hdr10Settings)
h265ColorSpaceSettings_hdr10Settings = Lens.lens (\H265ColorSpaceSettings' {hdr10Settings} -> hdr10Settings) (\s@H265ColorSpaceSettings' {} a -> s {hdr10Settings = a} :: H265ColorSpaceSettings)

-- | Undocumented member.
h265ColorSpaceSettings_rec709Settings :: Lens.Lens' H265ColorSpaceSettings (Prelude.Maybe Rec709Settings)
h265ColorSpaceSettings_rec709Settings = Lens.lens (\H265ColorSpaceSettings' {rec709Settings} -> rec709Settings) (\s@H265ColorSpaceSettings' {} a -> s {rec709Settings = a} :: H265ColorSpaceSettings)

-- | Undocumented member.
h265ColorSpaceSettings_rec601Settings :: Lens.Lens' H265ColorSpaceSettings (Prelude.Maybe Rec601Settings)
h265ColorSpaceSettings_rec601Settings = Lens.lens (\H265ColorSpaceSettings' {rec601Settings} -> rec601Settings) (\s@H265ColorSpaceSettings' {} a -> s {rec601Settings = a} :: H265ColorSpaceSettings)

-- | Undocumented member.
h265ColorSpaceSettings_colorSpacePassthroughSettings :: Lens.Lens' H265ColorSpaceSettings (Prelude.Maybe ColorSpacePassthroughSettings)
h265ColorSpaceSettings_colorSpacePassthroughSettings = Lens.lens (\H265ColorSpaceSettings' {colorSpacePassthroughSettings} -> colorSpacePassthroughSettings) (\s@H265ColorSpaceSettings' {} a -> s {colorSpacePassthroughSettings = a} :: H265ColorSpaceSettings)

instance Core.FromJSON H265ColorSpaceSettings where
  parseJSON =
    Core.withObject
      "H265ColorSpaceSettings"
      ( \x ->
          H265ColorSpaceSettings'
            Prelude.<$> (x Core..:? "hdr10Settings")
            Prelude.<*> (x Core..:? "rec709Settings")
            Prelude.<*> (x Core..:? "rec601Settings")
            Prelude.<*> (x Core..:? "colorSpacePassthroughSettings")
      )

instance Prelude.Hashable H265ColorSpaceSettings

instance Prelude.NFData H265ColorSpaceSettings

instance Core.ToJSON H265ColorSpaceSettings where
  toJSON H265ColorSpaceSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("hdr10Settings" Core..=) Prelude.<$> hdr10Settings,
            ("rec709Settings" Core..=)
              Prelude.<$> rec709Settings,
            ("rec601Settings" Core..=)
              Prelude.<$> rec601Settings,
            ("colorSpacePassthroughSettings" Core..=)
              Prelude.<$> colorSpacePassthroughSettings
          ]
      )
