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
-- Module      : Network.AWS.MediaConvert.Types.VideoDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.VideoDetail where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains details about the output\'s video stream
--
-- /See:/ 'newVideoDetail' smart constructor.
data VideoDetail = VideoDetail'
  { -- | Width in pixels for the output
    widthInPx :: Core.Maybe Core.Int,
    -- | Height in pixels for the output
    heightInPx :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VideoDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'widthInPx', 'videoDetail_widthInPx' - Width in pixels for the output
--
-- 'heightInPx', 'videoDetail_heightInPx' - Height in pixels for the output
newVideoDetail ::
  VideoDetail
newVideoDetail =
  VideoDetail'
    { widthInPx = Core.Nothing,
      heightInPx = Core.Nothing
    }

-- | Width in pixels for the output
videoDetail_widthInPx :: Lens.Lens' VideoDetail (Core.Maybe Core.Int)
videoDetail_widthInPx = Lens.lens (\VideoDetail' {widthInPx} -> widthInPx) (\s@VideoDetail' {} a -> s {widthInPx = a} :: VideoDetail)

-- | Height in pixels for the output
videoDetail_heightInPx :: Lens.Lens' VideoDetail (Core.Maybe Core.Int)
videoDetail_heightInPx = Lens.lens (\VideoDetail' {heightInPx} -> heightInPx) (\s@VideoDetail' {} a -> s {heightInPx = a} :: VideoDetail)

instance Core.FromJSON VideoDetail where
  parseJSON =
    Core.withObject
      "VideoDetail"
      ( \x ->
          VideoDetail'
            Core.<$> (x Core..:? "widthInPx")
            Core.<*> (x Core..:? "heightInPx")
      )

instance Core.Hashable VideoDetail

instance Core.NFData VideoDetail
