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
-- Module      : Amazonka.EC2.Types.SpotPlacementScore
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SpotPlacementScore where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The Spot placement score for this Region or Availability Zone. The score
-- is calculated based on the assumption that the @capacity-optimized@
-- allocation strategy is used and that all of the Availability Zones in
-- the Region can be used.
--
-- /See:/ 'newSpotPlacementScore' smart constructor.
data SpotPlacementScore = SpotPlacementScore'
  { -- | The Availability Zone.
    availabilityZoneId :: Prelude.Maybe Prelude.Text,
    -- | The Region.
    region :: Prelude.Maybe Prelude.Text,
    -- | The placement score, on a scale from @1@ to @10@. A score of @10@
    -- indicates that your Spot request is highly likely to succeed in this
    -- Region or Availability Zone. A score of @1@ indicates that your Spot
    -- request is not likely to succeed.
    score :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SpotPlacementScore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZoneId', 'spotPlacementScore_availabilityZoneId' - The Availability Zone.
--
-- 'region', 'spotPlacementScore_region' - The Region.
--
-- 'score', 'spotPlacementScore_score' - The placement score, on a scale from @1@ to @10@. A score of @10@
-- indicates that your Spot request is highly likely to succeed in this
-- Region or Availability Zone. A score of @1@ indicates that your Spot
-- request is not likely to succeed.
newSpotPlacementScore ::
  SpotPlacementScore
newSpotPlacementScore =
  SpotPlacementScore'
    { availabilityZoneId =
        Prelude.Nothing,
      region = Prelude.Nothing,
      score = Prelude.Nothing
    }

-- | The Availability Zone.
spotPlacementScore_availabilityZoneId :: Lens.Lens' SpotPlacementScore (Prelude.Maybe Prelude.Text)
spotPlacementScore_availabilityZoneId = Lens.lens (\SpotPlacementScore' {availabilityZoneId} -> availabilityZoneId) (\s@SpotPlacementScore' {} a -> s {availabilityZoneId = a} :: SpotPlacementScore)

-- | The Region.
spotPlacementScore_region :: Lens.Lens' SpotPlacementScore (Prelude.Maybe Prelude.Text)
spotPlacementScore_region = Lens.lens (\SpotPlacementScore' {region} -> region) (\s@SpotPlacementScore' {} a -> s {region = a} :: SpotPlacementScore)

-- | The placement score, on a scale from @1@ to @10@. A score of @10@
-- indicates that your Spot request is highly likely to succeed in this
-- Region or Availability Zone. A score of @1@ indicates that your Spot
-- request is not likely to succeed.
spotPlacementScore_score :: Lens.Lens' SpotPlacementScore (Prelude.Maybe Prelude.Int)
spotPlacementScore_score = Lens.lens (\SpotPlacementScore' {score} -> score) (\s@SpotPlacementScore' {} a -> s {score = a} :: SpotPlacementScore)

instance Data.FromXML SpotPlacementScore where
  parseXML x =
    SpotPlacementScore'
      Prelude.<$> (x Data..@? "availabilityZoneId")
      Prelude.<*> (x Data..@? "region")
      Prelude.<*> (x Data..@? "score")

instance Prelude.Hashable SpotPlacementScore where
  hashWithSalt _salt SpotPlacementScore' {..} =
    _salt `Prelude.hashWithSalt` availabilityZoneId
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` score

instance Prelude.NFData SpotPlacementScore where
  rnf SpotPlacementScore' {..} =
    Prelude.rnf availabilityZoneId
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf score
