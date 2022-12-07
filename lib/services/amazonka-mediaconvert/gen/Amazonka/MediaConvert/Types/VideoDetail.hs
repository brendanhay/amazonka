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
-- Module      : Amazonka.MediaConvert.Types.VideoDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.VideoDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details about the output\'s video stream
--
-- /See:/ 'newVideoDetail' smart constructor.
data VideoDetail = VideoDetail'
  { -- | Width in pixels for the output
    widthInPx :: Prelude.Maybe Prelude.Int,
    -- | Height in pixels for the output
    heightInPx :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON VideoDetail where
  parseJSON =
    Data.withObject
      "VideoDetail"
      ( \x ->
          VideoDetail'
            Prelude.<$> (x Data..:? "widthInPx")
            Prelude.<*> (x Data..:? "heightInPx")
      )

instance Prelude.Hashable VideoDetail where
  hashWithSalt _salt VideoDetail' {..} =
    _salt `Prelude.hashWithSalt` widthInPx
      `Prelude.hashWithSalt` heightInPx

instance Prelude.NFData VideoDetail where
  rnf VideoDetail' {..} =
    Prelude.rnf widthInPx
      `Prelude.seq` Prelude.rnf heightInPx
