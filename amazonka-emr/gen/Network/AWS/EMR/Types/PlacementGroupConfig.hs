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
-- Module      : Network.AWS.EMR.Types.PlacementGroupConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.PlacementGroupConfig where

import Network.AWS.EMR.Types.InstanceRoleType
import Network.AWS.EMR.Types.PlacementGroupStrategy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Placement group configuration for an Amazon EMR cluster. The
-- configuration specifies the placement strategy that can be applied to
-- instance roles during cluster creation.
--
-- To use this configuration, consider attaching managed policy
-- AmazonElasticMapReducePlacementGroupPolicy to the EMR role.
--
-- /See:/ 'newPlacementGroupConfig' smart constructor.
data PlacementGroupConfig = PlacementGroupConfig'
  { -- | EC2 Placement Group strategy associated with instance role.
    --
    -- Starting with Amazon EMR version 5.23.0, the only supported placement
    -- strategy is @SPREAD@ for the @MASTER@ instance role.
    placementStrategy :: Prelude.Maybe PlacementGroupStrategy,
    -- | Role of the instance in the cluster.
    --
    -- Starting with Amazon EMR version 5.23.0, the only supported instance
    -- role is @MASTER@.
    instanceRole :: InstanceRoleType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PlacementGroupConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'placementStrategy', 'placementGroupConfig_placementStrategy' - EC2 Placement Group strategy associated with instance role.
--
-- Starting with Amazon EMR version 5.23.0, the only supported placement
-- strategy is @SPREAD@ for the @MASTER@ instance role.
--
-- 'instanceRole', 'placementGroupConfig_instanceRole' - Role of the instance in the cluster.
--
-- Starting with Amazon EMR version 5.23.0, the only supported instance
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

-- | EC2 Placement Group strategy associated with instance role.
--
-- Starting with Amazon EMR version 5.23.0, the only supported placement
-- strategy is @SPREAD@ for the @MASTER@ instance role.
placementGroupConfig_placementStrategy :: Lens.Lens' PlacementGroupConfig (Prelude.Maybe PlacementGroupStrategy)
placementGroupConfig_placementStrategy = Lens.lens (\PlacementGroupConfig' {placementStrategy} -> placementStrategy) (\s@PlacementGroupConfig' {} a -> s {placementStrategy = a} :: PlacementGroupConfig)

-- | Role of the instance in the cluster.
--
-- Starting with Amazon EMR version 5.23.0, the only supported instance
-- role is @MASTER@.
placementGroupConfig_instanceRole :: Lens.Lens' PlacementGroupConfig InstanceRoleType
placementGroupConfig_instanceRole = Lens.lens (\PlacementGroupConfig' {instanceRole} -> instanceRole) (\s@PlacementGroupConfig' {} a -> s {instanceRole = a} :: PlacementGroupConfig)

instance Prelude.FromJSON PlacementGroupConfig where
  parseJSON =
    Prelude.withObject
      "PlacementGroupConfig"
      ( \x ->
          PlacementGroupConfig'
            Prelude.<$> (x Prelude..:? "PlacementStrategy")
            Prelude.<*> (x Prelude..: "InstanceRole")
      )

instance Prelude.Hashable PlacementGroupConfig

instance Prelude.NFData PlacementGroupConfig

instance Prelude.ToJSON PlacementGroupConfig where
  toJSON PlacementGroupConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("PlacementStrategy" Prelude..=)
              Prelude.<$> placementStrategy,
            Prelude.Just
              ("InstanceRole" Prelude..= instanceRole)
          ]
      )
