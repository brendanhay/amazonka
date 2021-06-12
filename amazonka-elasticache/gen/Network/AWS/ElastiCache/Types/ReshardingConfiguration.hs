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
-- Module      : Network.AWS.ElastiCache.Types.ReshardingConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ReshardingConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A list of @PreferredAvailabilityZones@ objects that specifies the
-- configuration of a node group in the resharded cluster.
--
-- /See:/ 'newReshardingConfiguration' smart constructor.
data ReshardingConfiguration = ReshardingConfiguration'
  { -- | A list of preferred availability zones for the nodes in this cluster.
    preferredAvailabilityZones :: Core.Maybe [Core.Text],
    -- | Either the ElastiCache for Redis supplied 4-digit id or a user supplied
    -- id for the node group these configuration values apply to.
    nodeGroupId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReshardingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'preferredAvailabilityZones', 'reshardingConfiguration_preferredAvailabilityZones' - A list of preferred availability zones for the nodes in this cluster.
--
-- 'nodeGroupId', 'reshardingConfiguration_nodeGroupId' - Either the ElastiCache for Redis supplied 4-digit id or a user supplied
-- id for the node group these configuration values apply to.
newReshardingConfiguration ::
  ReshardingConfiguration
newReshardingConfiguration =
  ReshardingConfiguration'
    { preferredAvailabilityZones =
        Core.Nothing,
      nodeGroupId = Core.Nothing
    }

-- | A list of preferred availability zones for the nodes in this cluster.
reshardingConfiguration_preferredAvailabilityZones :: Lens.Lens' ReshardingConfiguration (Core.Maybe [Core.Text])
reshardingConfiguration_preferredAvailabilityZones = Lens.lens (\ReshardingConfiguration' {preferredAvailabilityZones} -> preferredAvailabilityZones) (\s@ReshardingConfiguration' {} a -> s {preferredAvailabilityZones = a} :: ReshardingConfiguration) Core.. Lens.mapping Lens._Coerce

-- | Either the ElastiCache for Redis supplied 4-digit id or a user supplied
-- id for the node group these configuration values apply to.
reshardingConfiguration_nodeGroupId :: Lens.Lens' ReshardingConfiguration (Core.Maybe Core.Text)
reshardingConfiguration_nodeGroupId = Lens.lens (\ReshardingConfiguration' {nodeGroupId} -> nodeGroupId) (\s@ReshardingConfiguration' {} a -> s {nodeGroupId = a} :: ReshardingConfiguration)

instance Core.Hashable ReshardingConfiguration

instance Core.NFData ReshardingConfiguration

instance Core.ToQuery ReshardingConfiguration where
  toQuery ReshardingConfiguration' {..} =
    Core.mconcat
      [ "PreferredAvailabilityZones"
          Core.=: Core.toQuery
            ( Core.toQueryList "AvailabilityZone"
                Core.<$> preferredAvailabilityZones
            ),
        "NodeGroupId" Core.=: nodeGroupId
      ]
