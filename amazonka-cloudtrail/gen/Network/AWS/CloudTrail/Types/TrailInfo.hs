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
-- Module      : Network.AWS.CloudTrail.Types.TrailInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.TrailInfo where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a CloudTrail trail, including the trail\'s name, home
-- region, and Amazon Resource Name (ARN).
--
-- /See:/ 'newTrailInfo' smart constructor.
data TrailInfo = TrailInfo'
  { -- | The ARN of a trail.
    trailARN :: Prelude.Maybe Prelude.Text,
    -- | The AWS region in which a trail was created.
    homeRegion :: Prelude.Maybe Prelude.Text,
    -- | The name of a trail.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { trailARN = Prelude.Nothing,
      homeRegion = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The ARN of a trail.
trailInfo_trailARN :: Lens.Lens' TrailInfo (Prelude.Maybe Prelude.Text)
trailInfo_trailARN = Lens.lens (\TrailInfo' {trailARN} -> trailARN) (\s@TrailInfo' {} a -> s {trailARN = a} :: TrailInfo)

-- | The AWS region in which a trail was created.
trailInfo_homeRegion :: Lens.Lens' TrailInfo (Prelude.Maybe Prelude.Text)
trailInfo_homeRegion = Lens.lens (\TrailInfo' {homeRegion} -> homeRegion) (\s@TrailInfo' {} a -> s {homeRegion = a} :: TrailInfo)

-- | The name of a trail.
trailInfo_name :: Lens.Lens' TrailInfo (Prelude.Maybe Prelude.Text)
trailInfo_name = Lens.lens (\TrailInfo' {name} -> name) (\s@TrailInfo' {} a -> s {name = a} :: TrailInfo)

instance Prelude.FromJSON TrailInfo where
  parseJSON =
    Prelude.withObject
      "TrailInfo"
      ( \x ->
          TrailInfo'
            Prelude.<$> (x Prelude..:? "TrailARN")
            Prelude.<*> (x Prelude..:? "HomeRegion")
            Prelude.<*> (x Prelude..:? "Name")
      )

instance Prelude.Hashable TrailInfo

instance Prelude.NFData TrailInfo
