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
-- Module      : Network.AWS.MediaLive.Types.H264ColorSpaceSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264ColorSpaceSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.ColorSpacePassthroughSettings
import Network.AWS.MediaLive.Types.Rec601Settings
import Network.AWS.MediaLive.Types.Rec709Settings

-- | H264 Color Space Settings
--
-- /See:/ 'newH264ColorSpaceSettings' smart constructor.
data H264ColorSpaceSettings = H264ColorSpaceSettings'
  { rec601Settings :: Core.Maybe Rec601Settings,
    rec709Settings :: Core.Maybe Rec709Settings,
    colorSpacePassthroughSettings :: Core.Maybe ColorSpacePassthroughSettings
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'H264ColorSpaceSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rec601Settings', 'h264ColorSpaceSettings_rec601Settings' - Undocumented member.
--
-- 'rec709Settings', 'h264ColorSpaceSettings_rec709Settings' - Undocumented member.
--
-- 'colorSpacePassthroughSettings', 'h264ColorSpaceSettings_colorSpacePassthroughSettings' - Undocumented member.
newH264ColorSpaceSettings ::
  H264ColorSpaceSettings
newH264ColorSpaceSettings =
  H264ColorSpaceSettings'
    { rec601Settings =
        Core.Nothing,
      rec709Settings = Core.Nothing,
      colorSpacePassthroughSettings = Core.Nothing
    }

-- | Undocumented member.
h264ColorSpaceSettings_rec601Settings :: Lens.Lens' H264ColorSpaceSettings (Core.Maybe Rec601Settings)
h264ColorSpaceSettings_rec601Settings = Lens.lens (\H264ColorSpaceSettings' {rec601Settings} -> rec601Settings) (\s@H264ColorSpaceSettings' {} a -> s {rec601Settings = a} :: H264ColorSpaceSettings)

-- | Undocumented member.
h264ColorSpaceSettings_rec709Settings :: Lens.Lens' H264ColorSpaceSettings (Core.Maybe Rec709Settings)
h264ColorSpaceSettings_rec709Settings = Lens.lens (\H264ColorSpaceSettings' {rec709Settings} -> rec709Settings) (\s@H264ColorSpaceSettings' {} a -> s {rec709Settings = a} :: H264ColorSpaceSettings)

-- | Undocumented member.
h264ColorSpaceSettings_colorSpacePassthroughSettings :: Lens.Lens' H264ColorSpaceSettings (Core.Maybe ColorSpacePassthroughSettings)
h264ColorSpaceSettings_colorSpacePassthroughSettings = Lens.lens (\H264ColorSpaceSettings' {colorSpacePassthroughSettings} -> colorSpacePassthroughSettings) (\s@H264ColorSpaceSettings' {} a -> s {colorSpacePassthroughSettings = a} :: H264ColorSpaceSettings)

instance Core.FromJSON H264ColorSpaceSettings where
  parseJSON =
    Core.withObject
      "H264ColorSpaceSettings"
      ( \x ->
          H264ColorSpaceSettings'
            Core.<$> (x Core..:? "rec601Settings")
            Core.<*> (x Core..:? "rec709Settings")
            Core.<*> (x Core..:? "colorSpacePassthroughSettings")
      )

instance Core.Hashable H264ColorSpaceSettings

instance Core.NFData H264ColorSpaceSettings

instance Core.ToJSON H264ColorSpaceSettings where
  toJSON H264ColorSpaceSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("rec601Settings" Core..=) Core.<$> rec601Settings,
            ("rec709Settings" Core..=) Core.<$> rec709Settings,
            ("colorSpacePassthroughSettings" Core..=)
              Core.<$> colorSpacePassthroughSettings
          ]
      )
