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
-- Module      : Amazonka.Redshift.Types.ClusterNode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.ClusterNode where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

-- | The identifier of a node in a cluster.
--
-- /See:/ 'newClusterNode' smart constructor.
data ClusterNode = ClusterNode'
  { -- | Whether the node is a leader node or a compute node.
    nodeRole :: Prelude.Maybe Prelude.Text,
    -- | The private IP address of a node within a cluster.
    privateIPAddress :: Prelude.Maybe Prelude.Text,
    -- | The public IP address of a node within a cluster.
    publicIPAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'privateIPAddress', 'clusterNode_privateIPAddress' - The private IP address of a node within a cluster.
--
-- 'publicIPAddress', 'clusterNode_publicIPAddress' - The public IP address of a node within a cluster.
newClusterNode ::
  ClusterNode
newClusterNode =
  ClusterNode'
    { nodeRole = Prelude.Nothing,
      privateIPAddress = Prelude.Nothing,
      publicIPAddress = Prelude.Nothing
    }

-- | Whether the node is a leader node or a compute node.
clusterNode_nodeRole :: Lens.Lens' ClusterNode (Prelude.Maybe Prelude.Text)
clusterNode_nodeRole = Lens.lens (\ClusterNode' {nodeRole} -> nodeRole) (\s@ClusterNode' {} a -> s {nodeRole = a} :: ClusterNode)

-- | The private IP address of a node within a cluster.
clusterNode_privateIPAddress :: Lens.Lens' ClusterNode (Prelude.Maybe Prelude.Text)
clusterNode_privateIPAddress = Lens.lens (\ClusterNode' {privateIPAddress} -> privateIPAddress) (\s@ClusterNode' {} a -> s {privateIPAddress = a} :: ClusterNode)

-- | The public IP address of a node within a cluster.
clusterNode_publicIPAddress :: Lens.Lens' ClusterNode (Prelude.Maybe Prelude.Text)
clusterNode_publicIPAddress = Lens.lens (\ClusterNode' {publicIPAddress} -> publicIPAddress) (\s@ClusterNode' {} a -> s {publicIPAddress = a} :: ClusterNode)

instance Data.FromXML ClusterNode where
  parseXML x =
    ClusterNode'
      Prelude.<$> (x Data..@? "NodeRole")
      Prelude.<*> (x Data..@? "PrivateIPAddress")
      Prelude.<*> (x Data..@? "PublicIPAddress")

instance Prelude.Hashable ClusterNode where
  hashWithSalt _salt ClusterNode' {..} =
    _salt
      `Prelude.hashWithSalt` nodeRole
      `Prelude.hashWithSalt` privateIPAddress
      `Prelude.hashWithSalt` publicIPAddress

instance Prelude.NFData ClusterNode where
  rnf ClusterNode' {..} =
    Prelude.rnf nodeRole
      `Prelude.seq` Prelude.rnf privateIPAddress
      `Prelude.seq` Prelude.rnf publicIPAddress
