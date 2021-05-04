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
-- Module      : Network.AWS.MediaConvert.Types.VideoDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.VideoDetail where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains details about the output\'s video stream
--
-- /See:/ 'newVideoDetail' smart constructor.
data VideoDetail = VideoDetail'
  { -- | Width in pixels for the output
    widthInPx :: Prelude.Maybe Prelude.Int,
    -- | Height in pixels for the output
    heightInPx :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { widthInPx = Prelude.Nothing,
      heightInPx = Prelude.Nothing
    }

-- | Width in pixels for the output
videoDetail_widthInPx :: Lens.Lens' VideoDetail (Prelude.Maybe Prelude.Int)
videoDetail_widthInPx = Lens.lens (\VideoDetail' {widthInPx} -> widthInPx) (\s@VideoDetail' {} a -> s {widthInPx = a} :: VideoDetail)

-- | Height in pixels for the output
videoDetail_heightInPx :: Lens.Lens' VideoDetail (Prelude.Maybe Prelude.Int)
videoDetail_heightInPx = Lens.lens (\VideoDetail' {heightInPx} -> heightInPx) (\s@VideoDetail' {} a -> s {heightInPx = a} :: VideoDetail)

instance Prelude.FromJSON VideoDetail where
  parseJSON =
    Prelude.withObject
      "VideoDetail"
      ( \x ->
          VideoDetail'
            Prelude.<$> (x Prelude..:? "widthInPx")
            Prelude.<*> (x Prelude..:? "heightInPx")
      )

instance Prelude.Hashable VideoDetail

instance Prelude.NFData VideoDetail
