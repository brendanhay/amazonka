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
-- Module      : Network.AWS.Redshift.Types.OrderableClusterOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.OrderableClusterOption where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.AvailabilityZone

-- | Describes an orderable cluster option.
--
-- /See:/ 'newOrderableClusterOption' smart constructor.
data OrderableClusterOption = OrderableClusterOption'
  { -- | A list of availability zones for the orderable cluster.
    availabilityZones :: Core.Maybe [AvailabilityZone],
    -- | The cluster type, for example @multi-node@.
    clusterType :: Core.Maybe Core.Text,
    -- | The node type for the orderable cluster.
    nodeType :: Core.Maybe Core.Text,
    -- | The version of the orderable cluster.
    clusterVersion :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OrderableClusterOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZones', 'orderableClusterOption_availabilityZones' - A list of availability zones for the orderable cluster.
--
-- 'clusterType', 'orderableClusterOption_clusterType' - The cluster type, for example @multi-node@.
--
-- 'nodeType', 'orderableClusterOption_nodeType' - The node type for the orderable cluster.
--
-- 'clusterVersion', 'orderableClusterOption_clusterVersion' - The version of the orderable cluster.
newOrderableClusterOption ::
  OrderableClusterOption
newOrderableClusterOption =
  OrderableClusterOption'
    { availabilityZones =
        Core.Nothing,
      clusterType = Core.Nothing,
      nodeType = Core.Nothing,
      clusterVersion = Core.Nothing
    }

-- | A list of availability zones for the orderable cluster.
orderableClusterOption_availabilityZones :: Lens.Lens' OrderableClusterOption (Core.Maybe [AvailabilityZone])
orderableClusterOption_availabilityZones = Lens.lens (\OrderableClusterOption' {availabilityZones} -> availabilityZones) (\s@OrderableClusterOption' {} a -> s {availabilityZones = a} :: OrderableClusterOption) Core.. Lens.mapping Lens._Coerce

-- | The cluster type, for example @multi-node@.
orderableClusterOption_clusterType :: Lens.Lens' OrderableClusterOption (Core.Maybe Core.Text)
orderableClusterOption_clusterType = Lens.lens (\OrderableClusterOption' {clusterType} -> clusterType) (\s@OrderableClusterOption' {} a -> s {clusterType = a} :: OrderableClusterOption)

-- | The node type for the orderable cluster.
orderableClusterOption_nodeType :: Lens.Lens' OrderableClusterOption (Core.Maybe Core.Text)
orderableClusterOption_nodeType = Lens.lens (\OrderableClusterOption' {nodeType} -> nodeType) (\s@OrderableClusterOption' {} a -> s {nodeType = a} :: OrderableClusterOption)

-- | The version of the orderable cluster.
orderableClusterOption_clusterVersion :: Lens.Lens' OrderableClusterOption (Core.Maybe Core.Text)
orderableClusterOption_clusterVersion = Lens.lens (\OrderableClusterOption' {clusterVersion} -> clusterVersion) (\s@OrderableClusterOption' {} a -> s {clusterVersion = a} :: OrderableClusterOption)

instance Core.FromXML OrderableClusterOption where
  parseXML x =
    OrderableClusterOption'
      Core.<$> ( x Core..@? "AvailabilityZones" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "AvailabilityZone")
               )
      Core.<*> (x Core..@? "ClusterType")
      Core.<*> (x Core..@? "NodeType")
      Core.<*> (x Core..@? "ClusterVersion")

instance Core.Hashable OrderableClusterOption

instance Core.NFData OrderableClusterOption
