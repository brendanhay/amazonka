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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.ResizeClusterMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

-- | Describes a resize cluster operation. For example, a scheduled action to
-- run the @ResizeCluster@ API operation.
--
-- /See:/ 'newResizeClusterMessage' smart constructor.
data ResizeClusterMessage = ResizeClusterMessage'
  { -- | The new number of nodes for the cluster. If not specified, the
    -- cluster\'s current number of nodes is used.
    numberOfNodes :: Prelude.Maybe Prelude.Int,
    -- | A boolean value indicating whether the resize operation is using the
    -- classic resize process. If you don\'t provide this parameter or set the
    -- value to @false@, the resize type is elastic.
    classic :: Prelude.Maybe Prelude.Bool,
    -- | The new cluster type for the specified cluster.
    clusterType :: Prelude.Maybe Prelude.Text,
    -- | The new node type for the nodes you are adding. If not specified, the
    -- cluster\'s current node type is used.
    nodeType :: Prelude.Maybe Prelude.Text,
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
-- 'numberOfNodes', 'resizeClusterMessage_numberOfNodes' - The new number of nodes for the cluster. If not specified, the
-- cluster\'s current number of nodes is used.
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
-- 'clusterIdentifier', 'resizeClusterMessage_clusterIdentifier' - The unique identifier for the cluster to resize.
newResizeClusterMessage ::
  -- | 'clusterIdentifier'
  Prelude.Text ->
  ResizeClusterMessage
newResizeClusterMessage pClusterIdentifier_ =
  ResizeClusterMessage'
    { numberOfNodes =
        Prelude.Nothing,
      classic = Prelude.Nothing,
      clusterType = Prelude.Nothing,
      nodeType = Prelude.Nothing,
      clusterIdentifier = pClusterIdentifier_
    }

-- | The new number of nodes for the cluster. If not specified, the
-- cluster\'s current number of nodes is used.
resizeClusterMessage_numberOfNodes :: Lens.Lens' ResizeClusterMessage (Prelude.Maybe Prelude.Int)
resizeClusterMessage_numberOfNodes = Lens.lens (\ResizeClusterMessage' {numberOfNodes} -> numberOfNodes) (\s@ResizeClusterMessage' {} a -> s {numberOfNodes = a} :: ResizeClusterMessage)

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

-- | The unique identifier for the cluster to resize.
resizeClusterMessage_clusterIdentifier :: Lens.Lens' ResizeClusterMessage Prelude.Text
resizeClusterMessage_clusterIdentifier = Lens.lens (\ResizeClusterMessage' {clusterIdentifier} -> clusterIdentifier) (\s@ResizeClusterMessage' {} a -> s {clusterIdentifier = a} :: ResizeClusterMessage)

instance Core.FromXML ResizeClusterMessage where
  parseXML x =
    ResizeClusterMessage'
      Prelude.<$> (x Core..@? "NumberOfNodes")
      Prelude.<*> (x Core..@? "Classic")
      Prelude.<*> (x Core..@? "ClusterType")
      Prelude.<*> (x Core..@? "NodeType")
      Prelude.<*> (x Core..@ "ClusterIdentifier")

instance Prelude.Hashable ResizeClusterMessage where
  hashWithSalt _salt ResizeClusterMessage' {..} =
    _salt `Prelude.hashWithSalt` numberOfNodes
      `Prelude.hashWithSalt` classic
      `Prelude.hashWithSalt` clusterType
      `Prelude.hashWithSalt` nodeType
      `Prelude.hashWithSalt` clusterIdentifier

instance Prelude.NFData ResizeClusterMessage where
  rnf ResizeClusterMessage' {..} =
    Prelude.rnf numberOfNodes
      `Prelude.seq` Prelude.rnf classic
      `Prelude.seq` Prelude.rnf clusterType
      `Prelude.seq` Prelude.rnf nodeType
      `Prelude.seq` Prelude.rnf clusterIdentifier

instance Core.ToQuery ResizeClusterMessage where
  toQuery ResizeClusterMessage' {..} =
    Prelude.mconcat
      [ "NumberOfNodes" Core.=: numberOfNodes,
        "Classic" Core.=: classic,
        "ClusterType" Core.=: clusterType,
        "NodeType" Core.=: nodeType,
        "ClusterIdentifier" Core.=: clusterIdentifier
      ]
