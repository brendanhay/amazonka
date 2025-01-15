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
-- Module      : Amazonka.EMR.Types.PlacementType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.PlacementType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Amazon EC2 Availability Zone configuration of the cluster (job
-- flow).
--
-- /See:/ 'newPlacementType' smart constructor.
data PlacementType = PlacementType'
  { -- | The Amazon EC2 Availability Zone for the cluster. @AvailabilityZone@ is
    -- used for uniform instance groups, while @AvailabilityZones@ (plural) is
    -- used for instance fleets.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | When multiple Availability Zones are specified, Amazon EMR evaluates
    -- them and launches instances in the optimal Availability Zone.
    -- @AvailabilityZones@ is used for instance fleets, while
    -- @AvailabilityZone@ (singular) is used for uniform instance groups.
    --
    -- The instance fleet configuration is available only in Amazon EMR
    -- versions 4.8.0 and later, excluding 5.0.x versions.
    availabilityZones :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PlacementType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZone', 'placementType_availabilityZone' - The Amazon EC2 Availability Zone for the cluster. @AvailabilityZone@ is
-- used for uniform instance groups, while @AvailabilityZones@ (plural) is
-- used for instance fleets.
--
-- 'availabilityZones', 'placementType_availabilityZones' - When multiple Availability Zones are specified, Amazon EMR evaluates
-- them and launches instances in the optimal Availability Zone.
-- @AvailabilityZones@ is used for instance fleets, while
-- @AvailabilityZone@ (singular) is used for uniform instance groups.
--
-- The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions.
newPlacementType ::
  PlacementType
newPlacementType =
  PlacementType'
    { availabilityZone = Prelude.Nothing,
      availabilityZones = Prelude.Nothing
    }

-- | The Amazon EC2 Availability Zone for the cluster. @AvailabilityZone@ is
-- used for uniform instance groups, while @AvailabilityZones@ (plural) is
-- used for instance fleets.
placementType_availabilityZone :: Lens.Lens' PlacementType (Prelude.Maybe Prelude.Text)
placementType_availabilityZone = Lens.lens (\PlacementType' {availabilityZone} -> availabilityZone) (\s@PlacementType' {} a -> s {availabilityZone = a} :: PlacementType)

-- | When multiple Availability Zones are specified, Amazon EMR evaluates
-- them and launches instances in the optimal Availability Zone.
-- @AvailabilityZones@ is used for instance fleets, while
-- @AvailabilityZone@ (singular) is used for uniform instance groups.
--
-- The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions.
placementType_availabilityZones :: Lens.Lens' PlacementType (Prelude.Maybe [Prelude.Text])
placementType_availabilityZones = Lens.lens (\PlacementType' {availabilityZones} -> availabilityZones) (\s@PlacementType' {} a -> s {availabilityZones = a} :: PlacementType) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable PlacementType where
  hashWithSalt _salt PlacementType' {..} =
    _salt
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` availabilityZones

instance Prelude.NFData PlacementType where
  rnf PlacementType' {..} =
    Prelude.rnf availabilityZone `Prelude.seq`
      Prelude.rnf availabilityZones

instance Data.ToJSON PlacementType where
  toJSON PlacementType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AvailabilityZone" Data..=)
              Prelude.<$> availabilityZone,
            ("AvailabilityZones" Data..=)
              Prelude.<$> availabilityZones
          ]
      )
