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
-- Module      : Amazonka.MediaLive.Types.H264ColorSpaceSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.H264ColorSpaceSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.ColorSpacePassthroughSettings
import Amazonka.MediaLive.Types.Rec601Settings
import Amazonka.MediaLive.Types.Rec709Settings
import qualified Amazonka.Prelude as Prelude

-- | H264 Color Space Settings
--
-- /See:/ 'newH264ColorSpaceSettings' smart constructor.
data H264ColorSpaceSettings = H264ColorSpaceSettings'
  { colorSpacePassthroughSettings :: Prelude.Maybe ColorSpacePassthroughSettings,
    rec601Settings :: Prelude.Maybe Rec601Settings,
    rec709Settings :: Prelude.Maybe Rec709Settings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'H264ColorSpaceSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'colorSpacePassthroughSettings', 'h264ColorSpaceSettings_colorSpacePassthroughSettings' - Undocumented member.
--
-- 'rec601Settings', 'h264ColorSpaceSettings_rec601Settings' - Undocumented member.
--
-- 'rec709Settings', 'h264ColorSpaceSettings_rec709Settings' - Undocumented member.
newH264ColorSpaceSettings ::
  H264ColorSpaceSettings
newH264ColorSpaceSettings =
  H264ColorSpaceSettings'
    { colorSpacePassthroughSettings =
        Prelude.Nothing,
      rec601Settings = Prelude.Nothing,
      rec709Settings = Prelude.Nothing
    }

-- | Undocumented member.
h264ColorSpaceSettings_colorSpacePassthroughSettings :: Lens.Lens' H264ColorSpaceSettings (Prelude.Maybe ColorSpacePassthroughSettings)
h264ColorSpaceSettings_colorSpacePassthroughSettings = Lens.lens (\H264ColorSpaceSettings' {colorSpacePassthroughSettings} -> colorSpacePassthroughSettings) (\s@H264ColorSpaceSettings' {} a -> s {colorSpacePassthroughSettings = a} :: H264ColorSpaceSettings)

-- | Undocumented member.
h264ColorSpaceSettings_rec601Settings :: Lens.Lens' H264ColorSpaceSettings (Prelude.Maybe Rec601Settings)
h264ColorSpaceSettings_rec601Settings = Lens.lens (\H264ColorSpaceSettings' {rec601Settings} -> rec601Settings) (\s@H264ColorSpaceSettings' {} a -> s {rec601Settings = a} :: H264ColorSpaceSettings)

-- | Undocumented member.
h264ColorSpaceSettings_rec709Settings :: Lens.Lens' H264ColorSpaceSettings (Prelude.Maybe Rec709Settings)
h264ColorSpaceSettings_rec709Settings = Lens.lens (\H264ColorSpaceSettings' {rec709Settings} -> rec709Settings) (\s@H264ColorSpaceSettings' {} a -> s {rec709Settings = a} :: H264ColorSpaceSettings)

instance Data.FromJSON H264ColorSpaceSettings where
  parseJSON =
    Data.withObject
      "H264ColorSpaceSettings"
      ( \x ->
          H264ColorSpaceSettings'
            Prelude.<$> (x Data..:? "colorSpacePassthroughSettings")
            Prelude.<*> (x Data..:? "rec601Settings")
            Prelude.<*> (x Data..:? "rec709Settings")
      )

instance Prelude.Hashable H264ColorSpaceSettings where
  hashWithSalt _salt H264ColorSpaceSettings' {..} =
    _salt
      `Prelude.hashWithSalt` colorSpacePassthroughSettings
      `Prelude.hashWithSalt` rec601Settings
      `Prelude.hashWithSalt` rec709Settings

instance Prelude.NFData H264ColorSpaceSettings where
  rnf H264ColorSpaceSettings' {..} =
    Prelude.rnf colorSpacePassthroughSettings
      `Prelude.seq` Prelude.rnf rec601Settings
      `Prelude.seq` Prelude.rnf rec709Settings

instance Data.ToJSON H264ColorSpaceSettings where
  toJSON H264ColorSpaceSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("colorSpacePassthroughSettings" Data..=)
              Prelude.<$> colorSpacePassthroughSettings,
            ("rec601Settings" Data..=)
              Prelude.<$> rec601Settings,
            ("rec709Settings" Data..=)
              Prelude.<$> rec709Settings
          ]
      )
