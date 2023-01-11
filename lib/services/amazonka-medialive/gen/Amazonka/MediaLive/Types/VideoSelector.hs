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
-- Module      : Amazonka.MediaLive.Types.VideoSelector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.VideoSelector where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.VideoSelectorColorSpace
import Amazonka.MediaLive.Types.VideoSelectorColorSpaceSettings
import Amazonka.MediaLive.Types.VideoSelectorColorSpaceUsage
import Amazonka.MediaLive.Types.VideoSelectorSettings
import qualified Amazonka.Prelude as Prelude

-- | Specifies a particular video stream within an input source. An input may
-- have only a single video selector.
--
-- /See:/ 'newVideoSelector' smart constructor.
data VideoSelector = VideoSelector'
  { -- | Specifies the color space of an input. This setting works in tandem with
    -- colorSpaceUsage and a video description\'s colorSpaceSettingsChoice to
    -- determine if any conversion will be performed.
    colorSpace :: Prelude.Maybe VideoSelectorColorSpace,
    -- | Color space settings
    colorSpaceSettings :: Prelude.Maybe VideoSelectorColorSpaceSettings,
    -- | Applies only if colorSpace is a value other than follow. This field
    -- controls how the value in the colorSpace field will be used. fallback
    -- means that when the input does include color space data, that data will
    -- be used, but when the input has no color space data, the value in
    -- colorSpace will be used. Choose fallback if your input is sometimes
    -- missing color space data, but when it does have color space data, that
    -- data is correct. force means to always use the value in colorSpace.
    -- Choose force if your input usually has no color space data or might have
    -- unreliable color space data.
    colorSpaceUsage :: Prelude.Maybe VideoSelectorColorSpaceUsage,
    -- | The video selector settings.
    selectorSettings :: Prelude.Maybe VideoSelectorSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VideoSelector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'colorSpace', 'videoSelector_colorSpace' - Specifies the color space of an input. This setting works in tandem with
-- colorSpaceUsage and a video description\'s colorSpaceSettingsChoice to
-- determine if any conversion will be performed.
--
-- 'colorSpaceSettings', 'videoSelector_colorSpaceSettings' - Color space settings
--
-- 'colorSpaceUsage', 'videoSelector_colorSpaceUsage' - Applies only if colorSpace is a value other than follow. This field
-- controls how the value in the colorSpace field will be used. fallback
-- means that when the input does include color space data, that data will
-- be used, but when the input has no color space data, the value in
-- colorSpace will be used. Choose fallback if your input is sometimes
-- missing color space data, but when it does have color space data, that
-- data is correct. force means to always use the value in colorSpace.
-- Choose force if your input usually has no color space data or might have
-- unreliable color space data.
--
-- 'selectorSettings', 'videoSelector_selectorSettings' - The video selector settings.
newVideoSelector ::
  VideoSelector
newVideoSelector =
  VideoSelector'
    { colorSpace = Prelude.Nothing,
      colorSpaceSettings = Prelude.Nothing,
      colorSpaceUsage = Prelude.Nothing,
      selectorSettings = Prelude.Nothing
    }

-- | Specifies the color space of an input. This setting works in tandem with
-- colorSpaceUsage and a video description\'s colorSpaceSettingsChoice to
-- determine if any conversion will be performed.
videoSelector_colorSpace :: Lens.Lens' VideoSelector (Prelude.Maybe VideoSelectorColorSpace)
videoSelector_colorSpace = Lens.lens (\VideoSelector' {colorSpace} -> colorSpace) (\s@VideoSelector' {} a -> s {colorSpace = a} :: VideoSelector)

-- | Color space settings
videoSelector_colorSpaceSettings :: Lens.Lens' VideoSelector (Prelude.Maybe VideoSelectorColorSpaceSettings)
videoSelector_colorSpaceSettings = Lens.lens (\VideoSelector' {colorSpaceSettings} -> colorSpaceSettings) (\s@VideoSelector' {} a -> s {colorSpaceSettings = a} :: VideoSelector)

-- | Applies only if colorSpace is a value other than follow. This field
-- controls how the value in the colorSpace field will be used. fallback
-- means that when the input does include color space data, that data will
-- be used, but when the input has no color space data, the value in
-- colorSpace will be used. Choose fallback if your input is sometimes
-- missing color space data, but when it does have color space data, that
-- data is correct. force means to always use the value in colorSpace.
-- Choose force if your input usually has no color space data or might have
-- unreliable color space data.
videoSelector_colorSpaceUsage :: Lens.Lens' VideoSelector (Prelude.Maybe VideoSelectorColorSpaceUsage)
videoSelector_colorSpaceUsage = Lens.lens (\VideoSelector' {colorSpaceUsage} -> colorSpaceUsage) (\s@VideoSelector' {} a -> s {colorSpaceUsage = a} :: VideoSelector)

-- | The video selector settings.
videoSelector_selectorSettings :: Lens.Lens' VideoSelector (Prelude.Maybe VideoSelectorSettings)
videoSelector_selectorSettings = Lens.lens (\VideoSelector' {selectorSettings} -> selectorSettings) (\s@VideoSelector' {} a -> s {selectorSettings = a} :: VideoSelector)

instance Data.FromJSON VideoSelector where
  parseJSON =
    Data.withObject
      "VideoSelector"
      ( \x ->
          VideoSelector'
            Prelude.<$> (x Data..:? "colorSpace")
            Prelude.<*> (x Data..:? "colorSpaceSettings")
            Prelude.<*> (x Data..:? "colorSpaceUsage")
            Prelude.<*> (x Data..:? "selectorSettings")
      )

instance Prelude.Hashable VideoSelector where
  hashWithSalt _salt VideoSelector' {..} =
    _salt `Prelude.hashWithSalt` colorSpace
      `Prelude.hashWithSalt` colorSpaceSettings
      `Prelude.hashWithSalt` colorSpaceUsage
      `Prelude.hashWithSalt` selectorSettings

instance Prelude.NFData VideoSelector where
  rnf VideoSelector' {..} =
    Prelude.rnf colorSpace
      `Prelude.seq` Prelude.rnf colorSpaceSettings
      `Prelude.seq` Prelude.rnf colorSpaceUsage
      `Prelude.seq` Prelude.rnf selectorSettings

instance Data.ToJSON VideoSelector where
  toJSON VideoSelector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("colorSpace" Data..=) Prelude.<$> colorSpace,
            ("colorSpaceSettings" Data..=)
              Prelude.<$> colorSpaceSettings,
            ("colorSpaceUsage" Data..=)
              Prelude.<$> colorSpaceUsage,
            ("selectorSettings" Data..=)
              Prelude.<$> selectorSettings
          ]
      )
