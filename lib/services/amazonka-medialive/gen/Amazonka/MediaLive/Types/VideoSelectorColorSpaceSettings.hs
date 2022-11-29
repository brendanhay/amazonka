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
-- Module      : Amazonka.MediaLive.Types.VideoSelectorColorSpaceSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.VideoSelectorColorSpaceSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types.Hdr10Settings
import qualified Amazonka.Prelude as Prelude

-- | Video Selector Color Space Settings
--
-- /See:/ 'newVideoSelectorColorSpaceSettings' smart constructor.
data VideoSelectorColorSpaceSettings = VideoSelectorColorSpaceSettings'
  { hdr10Settings :: Prelude.Maybe Hdr10Settings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VideoSelectorColorSpaceSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hdr10Settings', 'videoSelectorColorSpaceSettings_hdr10Settings' - Undocumented member.
newVideoSelectorColorSpaceSettings ::
  VideoSelectorColorSpaceSettings
newVideoSelectorColorSpaceSettings =
  VideoSelectorColorSpaceSettings'
    { hdr10Settings =
        Prelude.Nothing
    }

-- | Undocumented member.
videoSelectorColorSpaceSettings_hdr10Settings :: Lens.Lens' VideoSelectorColorSpaceSettings (Prelude.Maybe Hdr10Settings)
videoSelectorColorSpaceSettings_hdr10Settings = Lens.lens (\VideoSelectorColorSpaceSettings' {hdr10Settings} -> hdr10Settings) (\s@VideoSelectorColorSpaceSettings' {} a -> s {hdr10Settings = a} :: VideoSelectorColorSpaceSettings)

instance
  Core.FromJSON
    VideoSelectorColorSpaceSettings
  where
  parseJSON =
    Core.withObject
      "VideoSelectorColorSpaceSettings"
      ( \x ->
          VideoSelectorColorSpaceSettings'
            Prelude.<$> (x Core..:? "hdr10Settings")
      )

instance
  Prelude.Hashable
    VideoSelectorColorSpaceSettings
  where
  hashWithSalt
    _salt
    VideoSelectorColorSpaceSettings' {..} =
      _salt `Prelude.hashWithSalt` hdr10Settings

instance
  Prelude.NFData
    VideoSelectorColorSpaceSettings
  where
  rnf VideoSelectorColorSpaceSettings' {..} =
    Prelude.rnf hdr10Settings

instance Core.ToJSON VideoSelectorColorSpaceSettings where
  toJSON VideoSelectorColorSpaceSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("hdr10Settings" Core..=)
              Prelude.<$> hdr10Settings
          ]
      )
