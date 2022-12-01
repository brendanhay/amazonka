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
-- Module      : Amazonka.ElastiCache.Types.RegionalConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.RegionalConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElastiCache.Types.ReshardingConfiguration
import qualified Amazonka.Prelude as Prelude

-- | A list of the replication groups
--
-- /See:/ 'newRegionalConfiguration' smart constructor.
data RegionalConfiguration = RegionalConfiguration'
  { -- | The name of the secondary cluster
    replicationGroupId :: Prelude.Text,
    -- | The Amazon region where the cluster is stored
    replicationGroupRegion :: Prelude.Text,
    -- | A list of @PreferredAvailabilityZones@ objects that specifies the
    -- configuration of a node group in the resharded cluster.
    reshardingConfiguration :: [ReshardingConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegionalConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationGroupId', 'regionalConfiguration_replicationGroupId' - The name of the secondary cluster
--
-- 'replicationGroupRegion', 'regionalConfiguration_replicationGroupRegion' - The Amazon region where the cluster is stored
--
-- 'reshardingConfiguration', 'regionalConfiguration_reshardingConfiguration' - A list of @PreferredAvailabilityZones@ objects that specifies the
-- configuration of a node group in the resharded cluster.
newRegionalConfiguration ::
  -- | 'replicationGroupId'
  Prelude.Text ->
  -- | 'replicationGroupRegion'
  Prelude.Text ->
  RegionalConfiguration
newRegionalConfiguration
  pReplicationGroupId_
  pReplicationGroupRegion_ =
    RegionalConfiguration'
      { replicationGroupId =
          pReplicationGroupId_,
        replicationGroupRegion = pReplicationGroupRegion_,
        reshardingConfiguration = Prelude.mempty
      }

-- | The name of the secondary cluster
regionalConfiguration_replicationGroupId :: Lens.Lens' RegionalConfiguration Prelude.Text
regionalConfiguration_replicationGroupId = Lens.lens (\RegionalConfiguration' {replicationGroupId} -> replicationGroupId) (\s@RegionalConfiguration' {} a -> s {replicationGroupId = a} :: RegionalConfiguration)

-- | The Amazon region where the cluster is stored
regionalConfiguration_replicationGroupRegion :: Lens.Lens' RegionalConfiguration Prelude.Text
regionalConfiguration_replicationGroupRegion = Lens.lens (\RegionalConfiguration' {replicationGroupRegion} -> replicationGroupRegion) (\s@RegionalConfiguration' {} a -> s {replicationGroupRegion = a} :: RegionalConfiguration)

-- | A list of @PreferredAvailabilityZones@ objects that specifies the
-- configuration of a node group in the resharded cluster.
regionalConfiguration_reshardingConfiguration :: Lens.Lens' RegionalConfiguration [ReshardingConfiguration]
regionalConfiguration_reshardingConfiguration = Lens.lens (\RegionalConfiguration' {reshardingConfiguration} -> reshardingConfiguration) (\s@RegionalConfiguration' {} a -> s {reshardingConfiguration = a} :: RegionalConfiguration) Prelude.. Lens.coerced

instance Prelude.Hashable RegionalConfiguration where
  hashWithSalt _salt RegionalConfiguration' {..} =
    _salt `Prelude.hashWithSalt` replicationGroupId
      `Prelude.hashWithSalt` replicationGroupRegion
      `Prelude.hashWithSalt` reshardingConfiguration

instance Prelude.NFData RegionalConfiguration where
  rnf RegionalConfiguration' {..} =
    Prelude.rnf replicationGroupId
      `Prelude.seq` Prelude.rnf replicationGroupRegion
      `Prelude.seq` Prelude.rnf reshardingConfiguration

instance Core.ToQuery RegionalConfiguration where
  toQuery RegionalConfiguration' {..} =
    Prelude.mconcat
      [ "ReplicationGroupId" Core.=: replicationGroupId,
        "ReplicationGroupRegion"
          Core.=: replicationGroupRegion,
        "ReshardingConfiguration"
          Core.=: Core.toQueryList
            "ReshardingConfiguration"
            reshardingConfiguration
      ]
