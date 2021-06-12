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
-- Module      : Network.AWS.CloudTrail.Types.TrailInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.TrailInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a CloudTrail trail, including the trail\'s name, home
-- region, and Amazon Resource Name (ARN).
--
-- /See:/ 'newTrailInfo' smart constructor.
data TrailInfo = TrailInfo'
  { -- | The ARN of a trail.
    trailARN :: Core.Maybe Core.Text,
    -- | The AWS region in which a trail was created.
    homeRegion :: Core.Maybe Core.Text,
    -- | The name of a trail.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TrailInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trailARN', 'trailInfo_trailARN' - The ARN of a trail.
--
-- 'homeRegion', 'trailInfo_homeRegion' - The AWS region in which a trail was created.
--
-- 'name', 'trailInfo_name' - The name of a trail.
newTrailInfo ::
  TrailInfo
newTrailInfo =
  TrailInfo'
    { trailARN = Core.Nothing,
      homeRegion = Core.Nothing,
      name = Core.Nothing
    }

-- | The ARN of a trail.
trailInfo_trailARN :: Lens.Lens' TrailInfo (Core.Maybe Core.Text)
trailInfo_trailARN = Lens.lens (\TrailInfo' {trailARN} -> trailARN) (\s@TrailInfo' {} a -> s {trailARN = a} :: TrailInfo)

-- | The AWS region in which a trail was created.
trailInfo_homeRegion :: Lens.Lens' TrailInfo (Core.Maybe Core.Text)
trailInfo_homeRegion = Lens.lens (\TrailInfo' {homeRegion} -> homeRegion) (\s@TrailInfo' {} a -> s {homeRegion = a} :: TrailInfo)

-- | The name of a trail.
trailInfo_name :: Lens.Lens' TrailInfo (Core.Maybe Core.Text)
trailInfo_name = Lens.lens (\TrailInfo' {name} -> name) (\s@TrailInfo' {} a -> s {name = a} :: TrailInfo)

instance Core.FromJSON TrailInfo where
  parseJSON =
    Core.withObject
      "TrailInfo"
      ( \x ->
          TrailInfo'
            Core.<$> (x Core..:? "TrailARN")
            Core.<*> (x Core..:? "HomeRegion")
            Core.<*> (x Core..:? "Name")
      )

instance Core.Hashable TrailInfo

instance Core.NFData TrailInfo
