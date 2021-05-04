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
-- Module      : Network.AWS.Redshift.Types.ResizeClusterMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ResizeClusterMessage where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal

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
    -- | The new number of nodes for the cluster. If not specified, the
    -- cluster\'s current number of nodes is used.
    numberOfNodes :: Prelude.Maybe Prelude.Int,
    -- | The new node type for the nodes you are adding. If not specified, the
    -- cluster\'s current node type is used.
    nodeType :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the cluster to resize.
    clusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'numberOfNodes', 'resizeClusterMessage_numberOfNodes' - The new number of nodes for the cluster. If not specified, the
-- cluster\'s current number of nodes is used.
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
    { classic = Prelude.Nothing,
      clusterType = Prelude.Nothing,
      numberOfNodes = Prelude.Nothing,
      nodeType = Prelude.Nothing,
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

-- | The new number of nodes for the cluster. If not specified, the
-- cluster\'s current number of nodes is used.
resizeClusterMessage_numberOfNodes :: Lens.Lens' ResizeClusterMessage (Prelude.Maybe Prelude.Int)
resizeClusterMessage_numberOfNodes = Lens.lens (\ResizeClusterMessage' {numberOfNodes} -> numberOfNodes) (\s@ResizeClusterMessage' {} a -> s {numberOfNodes = a} :: ResizeClusterMessage)

-- | The new node type for the nodes you are adding. If not specified, the
-- cluster\'s current node type is used.
resizeClusterMessage_nodeType :: Lens.Lens' ResizeClusterMessage (Prelude.Maybe Prelude.Text)
resizeClusterMessage_nodeType = Lens.lens (\ResizeClusterMessage' {nodeType} -> nodeType) (\s@ResizeClusterMessage' {} a -> s {nodeType = a} :: ResizeClusterMessage)

-- | The unique identifier for the cluster to resize.
resizeClusterMessage_clusterIdentifier :: Lens.Lens' ResizeClusterMessage Prelude.Text
resizeClusterMessage_clusterIdentifier = Lens.lens (\ResizeClusterMessage' {clusterIdentifier} -> clusterIdentifier) (\s@ResizeClusterMessage' {} a -> s {clusterIdentifier = a} :: ResizeClusterMessage)

instance Prelude.FromXML ResizeClusterMessage where
  parseXML x =
    ResizeClusterMessage'
      Prelude.<$> (x Prelude..@? "Classic")
      Prelude.<*> (x Prelude..@? "ClusterType")
      Prelude.<*> (x Prelude..@? "NumberOfNodes")
      Prelude.<*> (x Prelude..@? "NodeType")
      Prelude.<*> (x Prelude..@ "ClusterIdentifier")

instance Prelude.Hashable ResizeClusterMessage

instance Prelude.NFData ResizeClusterMessage

instance Prelude.ToQuery ResizeClusterMessage where
  toQuery ResizeClusterMessage' {..} =
    Prelude.mconcat
      [ "Classic" Prelude.=: classic,
        "ClusterType" Prelude.=: clusterType,
        "NumberOfNodes" Prelude.=: numberOfNodes,
        "NodeType" Prelude.=: nodeType,
        "ClusterIdentifier" Prelude.=: clusterIdentifier
      ]
