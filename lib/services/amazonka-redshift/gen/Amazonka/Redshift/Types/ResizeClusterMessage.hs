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
-- Module      : Amazonka.Redshift.Types.ResizeClusterMessage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.ResizeClusterMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

-- | Describes a resize cluster operation. For example, a scheduled action to
-- run the @ResizeCluster@ API operation.
--
-- /See:/ 'newResizeClusterMessage' smart constructor.
data ResizeClusterMessage = ResizeClusterMessage'
  { -- | A boolean value indicating whether the resize operation is using the
    -- classic resize process. If you don\'t provide this parameter or set the
    -- value to @false@, the resize type is elastic.
    classic :: Prelude.Maybe Prelude.Bool,
    -- | The new cluster type for the specified cluster.
    clusterType :: Prelude.Maybe Prelude.Text,
    -- | The new node type for the nodes you are adding. If not specified, the
    -- cluster\'s current node type is used.
    nodeType :: Prelude.Maybe Prelude.Text,
    -- | The new number of nodes for the cluster. If not specified, the
    -- cluster\'s current number of nodes is used.
    numberOfNodes :: Prelude.Maybe Prelude.Int,
    -- | The identifier of the reserved node.
    reservedNodeId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the target reserved node offering.
    targetReservedNodeOfferingId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the cluster to resize.
    clusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResizeClusterMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'classic', 'resizeClusterMessage_classic' - A boolean value indicating whether the resize operation is using the
-- classic resize process. If you don\'t provide this parameter or set the
-- value to @false@, the resize type is elastic.
--
-- 'clusterType', 'resizeClusterMessage_clusterType' - The new cluster type for the specified cluster.
--
-- 'nodeType', 'resizeClusterMessage_nodeType' - The new node type for the nodes you are adding. If not specified, the
-- cluster\'s current node type is used.
--
-- 'numberOfNodes', 'resizeClusterMessage_numberOfNodes' - The new number of nodes for the cluster. If not specified, the
-- cluster\'s current number of nodes is used.
--
-- 'reservedNodeId', 'resizeClusterMessage_reservedNodeId' - The identifier of the reserved node.
--
-- 'targetReservedNodeOfferingId', 'resizeClusterMessage_targetReservedNodeOfferingId' - The identifier of the target reserved node offering.
--
-- 'clusterIdentifier', 'resizeClusterMessage_clusterIdentifier' - The unique identifier for the cluster to resize.
newResizeClusterMessage ::
  -- | 'clusterIdentifier'
  Prelude.Text ->
  ResizeClusterMessage
newResizeClusterMessage pClusterIdentifier_ =
  ResizeClusterMessage'
    { classic = Prelude.Nothing,
      clusterType = Prelude.Nothing,
      nodeType = Prelude.Nothing,
      numberOfNodes = Prelude.Nothing,
      reservedNodeId = Prelude.Nothing,
      targetReservedNodeOfferingId = Prelude.Nothing,
      clusterIdentifier = pClusterIdentifier_
    }

-- | A boolean value indicating whether the resize operation is using the
-- classic resize process. If you don\'t provide this parameter or set the
-- value to @false@, the resize type is elastic.
resizeClusterMessage_classic :: Lens.Lens' ResizeClusterMessage (Prelude.Maybe Prelude.Bool)
resizeClusterMessage_classic = Lens.lens (\ResizeClusterMessage' {classic} -> classic) (\s@ResizeClusterMessage' {} a -> s {classic = a} :: ResizeClusterMessage)

-- | The new cluster type for the specified cluster.
resizeClusterMessage_clusterType :: Lens.Lens' ResizeClusterMessage (Prelude.Maybe Prelude.Text)
resizeClusterMessage_clusterType = Lens.lens (\ResizeClusterMessage' {clusterType} -> clusterType) (\s@ResizeClusterMessage' {} a -> s {clusterType = a} :: ResizeClusterMessage)

-- | The new node type for the nodes you are adding. If not specified, the
-- cluster\'s current node type is used.
resizeClusterMessage_nodeType :: Lens.Lens' ResizeClusterMessage (Prelude.Maybe Prelude.Text)
resizeClusterMessage_nodeType = Lens.lens (\ResizeClusterMessage' {nodeType} -> nodeType) (\s@ResizeClusterMessage' {} a -> s {nodeType = a} :: ResizeClusterMessage)

-- | The new number of nodes for the cluster. If not specified, the
-- cluster\'s current number of nodes is used.
resizeClusterMessage_numberOfNodes :: Lens.Lens' ResizeClusterMessage (Prelude.Maybe Prelude.Int)
resizeClusterMessage_numberOfNodes = Lens.lens (\ResizeClusterMessage' {numberOfNodes} -> numberOfNodes) (\s@ResizeClusterMessage' {} a -> s {numberOfNodes = a} :: ResizeClusterMessage)

-- | The identifier of the reserved node.
resizeClusterMessage_reservedNodeId :: Lens.Lens' ResizeClusterMessage (Prelude.Maybe Prelude.Text)
resizeClusterMessage_reservedNodeId = Lens.lens (\ResizeClusterMessage' {reservedNodeId} -> reservedNodeId) (\s@ResizeClusterMessage' {} a -> s {reservedNodeId = a} :: ResizeClusterMessage)

-- | The identifier of the target reserved node offering.
resizeClusterMessage_targetReservedNodeOfferingId :: Lens.Lens' ResizeClusterMessage (Prelude.Maybe Prelude.Text)
resizeClusterMessage_targetReservedNodeOfferingId = Lens.lens (\ResizeClusterMessage' {targetReservedNodeOfferingId} -> targetReservedNodeOfferingId) (\s@ResizeClusterMessage' {} a -> s {targetReservedNodeOfferingId = a} :: ResizeClusterMessage)

-- | The unique identifier for the cluster to resize.
resizeClusterMessage_clusterIdentifier :: Lens.Lens' ResizeClusterMessage Prelude.Text
resizeClusterMessage_clusterIdentifier = Lens.lens (\ResizeClusterMessage' {clusterIdentifier} -> clusterIdentifier) (\s@ResizeClusterMessage' {} a -> s {clusterIdentifier = a} :: ResizeClusterMessage)

instance Data.FromXML ResizeClusterMessage where
  parseXML x =
    ResizeClusterMessage'
      Prelude.<$> (x Data..@? "Classic")
      Prelude.<*> (x Data..@? "ClusterType")
      Prelude.<*> (x Data..@? "NodeType")
      Prelude.<*> (x Data..@? "NumberOfNodes")
      Prelude.<*> (x Data..@? "ReservedNodeId")
      Prelude.<*> (x Data..@? "TargetReservedNodeOfferingId")
      Prelude.<*> (x Data..@ "ClusterIdentifier")

instance Prelude.Hashable ResizeClusterMessage where
  hashWithSalt _salt ResizeClusterMessage' {..} =
    _salt
      `Prelude.hashWithSalt` classic
      `Prelude.hashWithSalt` clusterType
      `Prelude.hashWithSalt` nodeType
      `Prelude.hashWithSalt` numberOfNodes
      `Prelude.hashWithSalt` reservedNodeId
      `Prelude.hashWithSalt` targetReservedNodeOfferingId
      `Prelude.hashWithSalt` clusterIdentifier

instance Prelude.NFData ResizeClusterMessage where
  rnf ResizeClusterMessage' {..} =
    Prelude.rnf classic
      `Prelude.seq` Prelude.rnf clusterType
      `Prelude.seq` Prelude.rnf nodeType
      `Prelude.seq` Prelude.rnf numberOfNodes
      `Prelude.seq` Prelude.rnf reservedNodeId
      `Prelude.seq` Prelude.rnf targetReservedNodeOfferingId
      `Prelude.seq` Prelude.rnf clusterIdentifier

instance Data.ToQuery ResizeClusterMessage where
  toQuery ResizeClusterMessage' {..} =
    Prelude.mconcat
      [ "Classic" Data.=: classic,
        "ClusterType" Data.=: clusterType,
        "NodeType" Data.=: nodeType,
        "NumberOfNodes" Data.=: numberOfNodes,
        "ReservedNodeId" Data.=: reservedNodeId,
        "TargetReservedNodeOfferingId"
          Data.=: targetReservedNodeOfferingId,
        "ClusterIdentifier" Data.=: clusterIdentifier
      ]
