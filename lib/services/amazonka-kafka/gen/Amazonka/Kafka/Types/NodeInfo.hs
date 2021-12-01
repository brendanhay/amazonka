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
-- Module      : Amazonka.Kafka.Types.NodeInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.NodeInfo where

import qualified Amazonka.Core as Core
import Amazonka.Kafka.Types.BrokerNodeInfo
import Amazonka.Kafka.Types.NodeType
import Amazonka.Kafka.Types.ZookeeperNodeInfo
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The node information object.
--
-- /See:/ 'newNodeInfo' smart constructor.
data NodeInfo = NodeInfo'
  { -- | The start time.
    addedToClusterTime :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the node.
    nodeARN :: Prelude.Maybe Prelude.Text,
    -- | The ZookeeperNodeInfo.
    zookeeperNodeInfo :: Prelude.Maybe ZookeeperNodeInfo,
    -- | The instance type.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The broker node info.
    brokerNodeInfo :: Prelude.Maybe BrokerNodeInfo,
    -- | The node type.
    nodeType :: Prelude.Maybe NodeType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NodeInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addedToClusterTime', 'nodeInfo_addedToClusterTime' - The start time.
--
-- 'nodeARN', 'nodeInfo_nodeARN' - The Amazon Resource Name (ARN) of the node.
--
-- 'zookeeperNodeInfo', 'nodeInfo_zookeeperNodeInfo' - The ZookeeperNodeInfo.
--
-- 'instanceType', 'nodeInfo_instanceType' - The instance type.
--
-- 'brokerNodeInfo', 'nodeInfo_brokerNodeInfo' - The broker node info.
--
-- 'nodeType', 'nodeInfo_nodeType' - The node type.
newNodeInfo ::
  NodeInfo
newNodeInfo =
  NodeInfo'
    { addedToClusterTime = Prelude.Nothing,
      nodeARN = Prelude.Nothing,
      zookeeperNodeInfo = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      brokerNodeInfo = Prelude.Nothing,
      nodeType = Prelude.Nothing
    }

-- | The start time.
nodeInfo_addedToClusterTime :: Lens.Lens' NodeInfo (Prelude.Maybe Prelude.Text)
nodeInfo_addedToClusterTime = Lens.lens (\NodeInfo' {addedToClusterTime} -> addedToClusterTime) (\s@NodeInfo' {} a -> s {addedToClusterTime = a} :: NodeInfo)

-- | The Amazon Resource Name (ARN) of the node.
nodeInfo_nodeARN :: Lens.Lens' NodeInfo (Prelude.Maybe Prelude.Text)
nodeInfo_nodeARN = Lens.lens (\NodeInfo' {nodeARN} -> nodeARN) (\s@NodeInfo' {} a -> s {nodeARN = a} :: NodeInfo)

-- | The ZookeeperNodeInfo.
nodeInfo_zookeeperNodeInfo :: Lens.Lens' NodeInfo (Prelude.Maybe ZookeeperNodeInfo)
nodeInfo_zookeeperNodeInfo = Lens.lens (\NodeInfo' {zookeeperNodeInfo} -> zookeeperNodeInfo) (\s@NodeInfo' {} a -> s {zookeeperNodeInfo = a} :: NodeInfo)

-- | The instance type.
nodeInfo_instanceType :: Lens.Lens' NodeInfo (Prelude.Maybe Prelude.Text)
nodeInfo_instanceType = Lens.lens (\NodeInfo' {instanceType} -> instanceType) (\s@NodeInfo' {} a -> s {instanceType = a} :: NodeInfo)

-- | The broker node info.
nodeInfo_brokerNodeInfo :: Lens.Lens' NodeInfo (Prelude.Maybe BrokerNodeInfo)
nodeInfo_brokerNodeInfo = Lens.lens (\NodeInfo' {brokerNodeInfo} -> brokerNodeInfo) (\s@NodeInfo' {} a -> s {brokerNodeInfo = a} :: NodeInfo)

-- | The node type.
nodeInfo_nodeType :: Lens.Lens' NodeInfo (Prelude.Maybe NodeType)
nodeInfo_nodeType = Lens.lens (\NodeInfo' {nodeType} -> nodeType) (\s@NodeInfo' {} a -> s {nodeType = a} :: NodeInfo)

instance Core.FromJSON NodeInfo where
  parseJSON =
    Core.withObject
      "NodeInfo"
      ( \x ->
          NodeInfo'
            Prelude.<$> (x Core..:? "addedToClusterTime")
            Prelude.<*> (x Core..:? "nodeARN")
            Prelude.<*> (x Core..:? "zookeeperNodeInfo")
            Prelude.<*> (x Core..:? "instanceType")
            Prelude.<*> (x Core..:? "brokerNodeInfo")
            Prelude.<*> (x Core..:? "nodeType")
      )

instance Prelude.Hashable NodeInfo where
  hashWithSalt salt' NodeInfo' {..} =
    salt' `Prelude.hashWithSalt` nodeType
      `Prelude.hashWithSalt` brokerNodeInfo
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` zookeeperNodeInfo
      `Prelude.hashWithSalt` nodeARN
      `Prelude.hashWithSalt` addedToClusterTime

instance Prelude.NFData NodeInfo where
  rnf NodeInfo' {..} =
    Prelude.rnf addedToClusterTime
      `Prelude.seq` Prelude.rnf nodeType
      `Prelude.seq` Prelude.rnf brokerNodeInfo
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf zookeeperNodeInfo
      `Prelude.seq` Prelude.rnf nodeARN
