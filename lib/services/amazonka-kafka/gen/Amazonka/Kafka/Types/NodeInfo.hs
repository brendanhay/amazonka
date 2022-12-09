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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.NodeInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types.BrokerNodeInfo
import Amazonka.Kafka.Types.NodeType
import Amazonka.Kafka.Types.ZookeeperNodeInfo
import qualified Amazonka.Prelude as Prelude

-- | The node information object.
--
-- /See:/ 'newNodeInfo' smart constructor.
data NodeInfo = NodeInfo'
  { -- | The start time.
    addedToClusterTime :: Prelude.Maybe Prelude.Text,
    -- | The broker node info.
    brokerNodeInfo :: Prelude.Maybe BrokerNodeInfo,
    -- | The instance type.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the node.
    nodeARN :: Prelude.Maybe Prelude.Text,
    -- | The node type.
    nodeType :: Prelude.Maybe NodeType,
    -- | The ZookeeperNodeInfo.
    zookeeperNodeInfo :: Prelude.Maybe ZookeeperNodeInfo
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
-- 'brokerNodeInfo', 'nodeInfo_brokerNodeInfo' - The broker node info.
--
-- 'instanceType', 'nodeInfo_instanceType' - The instance type.
--
-- 'nodeARN', 'nodeInfo_nodeARN' - The Amazon Resource Name (ARN) of the node.
--
-- 'nodeType', 'nodeInfo_nodeType' - The node type.
--
-- 'zookeeperNodeInfo', 'nodeInfo_zookeeperNodeInfo' - The ZookeeperNodeInfo.
newNodeInfo ::
  NodeInfo
newNodeInfo =
  NodeInfo'
    { addedToClusterTime = Prelude.Nothing,
      brokerNodeInfo = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      nodeARN = Prelude.Nothing,
      nodeType = Prelude.Nothing,
      zookeeperNodeInfo = Prelude.Nothing
    }

-- | The start time.
nodeInfo_addedToClusterTime :: Lens.Lens' NodeInfo (Prelude.Maybe Prelude.Text)
nodeInfo_addedToClusterTime = Lens.lens (\NodeInfo' {addedToClusterTime} -> addedToClusterTime) (\s@NodeInfo' {} a -> s {addedToClusterTime = a} :: NodeInfo)

-- | The broker node info.
nodeInfo_brokerNodeInfo :: Lens.Lens' NodeInfo (Prelude.Maybe BrokerNodeInfo)
nodeInfo_brokerNodeInfo = Lens.lens (\NodeInfo' {brokerNodeInfo} -> brokerNodeInfo) (\s@NodeInfo' {} a -> s {brokerNodeInfo = a} :: NodeInfo)

-- | The instance type.
nodeInfo_instanceType :: Lens.Lens' NodeInfo (Prelude.Maybe Prelude.Text)
nodeInfo_instanceType = Lens.lens (\NodeInfo' {instanceType} -> instanceType) (\s@NodeInfo' {} a -> s {instanceType = a} :: NodeInfo)

-- | The Amazon Resource Name (ARN) of the node.
nodeInfo_nodeARN :: Lens.Lens' NodeInfo (Prelude.Maybe Prelude.Text)
nodeInfo_nodeARN = Lens.lens (\NodeInfo' {nodeARN} -> nodeARN) (\s@NodeInfo' {} a -> s {nodeARN = a} :: NodeInfo)

-- | The node type.
nodeInfo_nodeType :: Lens.Lens' NodeInfo (Prelude.Maybe NodeType)
nodeInfo_nodeType = Lens.lens (\NodeInfo' {nodeType} -> nodeType) (\s@NodeInfo' {} a -> s {nodeType = a} :: NodeInfo)

-- | The ZookeeperNodeInfo.
nodeInfo_zookeeperNodeInfo :: Lens.Lens' NodeInfo (Prelude.Maybe ZookeeperNodeInfo)
nodeInfo_zookeeperNodeInfo = Lens.lens (\NodeInfo' {zookeeperNodeInfo} -> zookeeperNodeInfo) (\s@NodeInfo' {} a -> s {zookeeperNodeInfo = a} :: NodeInfo)

instance Data.FromJSON NodeInfo where
  parseJSON =
    Data.withObject
      "NodeInfo"
      ( \x ->
          NodeInfo'
            Prelude.<$> (x Data..:? "addedToClusterTime")
            Prelude.<*> (x Data..:? "brokerNodeInfo")
            Prelude.<*> (x Data..:? "instanceType")
            Prelude.<*> (x Data..:? "nodeARN")
            Prelude.<*> (x Data..:? "nodeType")
            Prelude.<*> (x Data..:? "zookeeperNodeInfo")
      )

instance Prelude.Hashable NodeInfo where
  hashWithSalt _salt NodeInfo' {..} =
    _salt `Prelude.hashWithSalt` addedToClusterTime
      `Prelude.hashWithSalt` brokerNodeInfo
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` nodeARN
      `Prelude.hashWithSalt` nodeType
      `Prelude.hashWithSalt` zookeeperNodeInfo

instance Prelude.NFData NodeInfo where
  rnf NodeInfo' {..} =
    Prelude.rnf addedToClusterTime
      `Prelude.seq` Prelude.rnf brokerNodeInfo
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf nodeARN
      `Prelude.seq` Prelude.rnf nodeType
      `Prelude.seq` Prelude.rnf zookeeperNodeInfo
