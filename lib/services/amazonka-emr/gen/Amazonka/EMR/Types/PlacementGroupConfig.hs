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
-- Module      : Amazonka.EMR.Types.PlacementGroupConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.PlacementGroupConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types.InstanceRoleType
import Amazonka.EMR.Types.PlacementGroupStrategy
import qualified Amazonka.Prelude as Prelude

-- | Placement group configuration for an Amazon EMR cluster. The
-- configuration specifies the placement strategy that can be applied to
-- instance roles during cluster creation.
--
-- To use this configuration, consider attaching managed policy
-- AmazonElasticMapReducePlacementGroupPolicy to the Amazon EMR role.
--
-- /See:/ 'newPlacementGroupConfig' smart constructor.
data PlacementGroupConfig = PlacementGroupConfig'
  { -- | Amazon EC2 Placement Group strategy associated with instance role.
    --
    -- Starting with Amazon EMR release 5.23.0, the only supported placement
    -- strategy is @SPREAD@ for the @MASTER@ instance role.
    placementStrategy :: Prelude.Maybe PlacementGroupStrategy,
    -- | Role of the instance in the cluster.
    --
    -- Starting with Amazon EMR release 5.23.0, the only supported instance
    -- role is @MASTER@.
    instanceRole :: InstanceRoleType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PlacementGroupConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'placementStrategy', 'placementGroupConfig_placementStrategy' - Amazon EC2 Placement Group strategy associated with instance role.
--
-- Starting with Amazon EMR release 5.23.0, the only supported placement
-- strategy is @SPREAD@ for the @MASTER@ instance role.
--
-- 'instanceRole', 'placementGroupConfig_instanceRole' - Role of the instance in the cluster.
--
-- Starting with Amazon EMR release 5.23.0, the only supported instance
-- role is @MASTER@.
newPlacementGroupConfig ::
  -- | 'instanceRole'
  InstanceRoleType ->
  PlacementGroupConfig
newPlacementGroupConfig pInstanceRole_ =
  PlacementGroupConfig'
    { placementStrategy =
        Prelude.Nothing,
      instanceRole = pInstanceRole_
    }

-- | Amazon EC2 Placement Group strategy associated with instance role.
--
-- Starting with Amazon EMR release 5.23.0, the only supported placement
-- strategy is @SPREAD@ for the @MASTER@ instance role.
placementGroupConfig_placementStrategy :: Lens.Lens' PlacementGroupConfig (Prelude.Maybe PlacementGroupStrategy)
placementGroupConfig_placementStrategy = Lens.lens (\PlacementGroupConfig' {placementStrategy} -> placementStrategy) (\s@PlacementGroupConfig' {} a -> s {placementStrategy = a} :: PlacementGroupConfig)

-- | Role of the instance in the cluster.
--
-- Starting with Amazon EMR release 5.23.0, the only supported instance
-- role is @MASTER@.
placementGroupConfig_instanceRole :: Lens.Lens' PlacementGroupConfig InstanceRoleType
placementGroupConfig_instanceRole = Lens.lens (\PlacementGroupConfig' {instanceRole} -> instanceRole) (\s@PlacementGroupConfig' {} a -> s {instanceRole = a} :: PlacementGroupConfig)

instance Data.FromJSON PlacementGroupConfig where
  parseJSON =
    Data.withObject
      "PlacementGroupConfig"
      ( \x ->
          PlacementGroupConfig'
            Prelude.<$> (x Data..:? "PlacementStrategy")
            Prelude.<*> (x Data..: "InstanceRole")
      )

instance Prelude.Hashable PlacementGroupConfig where
  hashWithSalt _salt PlacementGroupConfig' {..} =
    _salt
      `Prelude.hashWithSalt` placementStrategy
      `Prelude.hashWithSalt` instanceRole

instance Prelude.NFData PlacementGroupConfig where
  rnf PlacementGroupConfig' {..} =
    Prelude.rnf placementStrategy
      `Prelude.seq` Prelude.rnf instanceRole

instance Data.ToJSON PlacementGroupConfig where
  toJSON PlacementGroupConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PlacementStrategy" Data..=)
              Prelude.<$> placementStrategy,
            Prelude.Just ("InstanceRole" Data..= instanceRole)
          ]
      )
