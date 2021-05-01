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
-- Module      : Network.AWS.Redshift.Types.ClusterNode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterNode where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal

-- | The identifier of a node in a cluster.
--
-- /See:/ 'newClusterNode' smart constructor.
data ClusterNode = ClusterNode'
  { -- | Whether the node is a leader node or a compute node.
    nodeRole :: Prelude.Maybe Prelude.Text,
    -- | The public IP address of a node within a cluster.
    publicIPAddress :: Prelude.Maybe Prelude.Text,
    -- | The private IP address of a node within a cluster.
    privateIPAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ClusterNode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nodeRole', 'clusterNode_nodeRole' - Whether the node is a leader node or a compute node.
--
-- 'publicIPAddress', 'clusterNode_publicIPAddress' - The public IP address of a node within a cluster.
--
-- 'privateIPAddress', 'clusterNode_privateIPAddress' - The private IP address of a node within a cluster.
newClusterNode ::
  ClusterNode
newClusterNode =
  ClusterNode'
    { nodeRole = Prelude.Nothing,
      publicIPAddress = Prelude.Nothing,
      privateIPAddress = Prelude.Nothing
    }

-- | Whether the node is a leader node or a compute node.
clusterNode_nodeRole :: Lens.Lens' ClusterNode (Prelude.Maybe Prelude.Text)
clusterNode_nodeRole = Lens.lens (\ClusterNode' {nodeRole} -> nodeRole) (\s@ClusterNode' {} a -> s {nodeRole = a} :: ClusterNode)

-- | The public IP address of a node within a cluster.
clusterNode_publicIPAddress :: Lens.Lens' ClusterNode (Prelude.Maybe Prelude.Text)
clusterNode_publicIPAddress = Lens.lens (\ClusterNode' {publicIPAddress} -> publicIPAddress) (\s@ClusterNode' {} a -> s {publicIPAddress = a} :: ClusterNode)

-- | The private IP address of a node within a cluster.
clusterNode_privateIPAddress :: Lens.Lens' ClusterNode (Prelude.Maybe Prelude.Text)
clusterNode_privateIPAddress = Lens.lens (\ClusterNode' {privateIPAddress} -> privateIPAddress) (\s@ClusterNode' {} a -> s {privateIPAddress = a} :: ClusterNode)

instance Prelude.FromXML ClusterNode where
  parseXML x =
    ClusterNode'
      Prelude.<$> (x Prelude..@? "NodeRole")
      Prelude.<*> (x Prelude..@? "PublicIPAddress")
      Prelude.<*> (x Prelude..@? "PrivateIPAddress")

instance Prelude.Hashable ClusterNode

instance Prelude.NFData ClusterNode
