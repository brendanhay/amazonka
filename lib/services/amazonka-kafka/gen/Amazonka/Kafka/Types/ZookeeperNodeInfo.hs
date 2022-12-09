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
-- Module      : Amazonka.Kafka.Types.ZookeeperNodeInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.ZookeeperNodeInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Zookeeper node information.
--
-- /See:/ 'newZookeeperNodeInfo' smart constructor.
data ZookeeperNodeInfo = ZookeeperNodeInfo'
  { -- | The attached elastic network interface of the broker.
    attachedENIId :: Prelude.Maybe Prelude.Text,
    -- | The virtual private cloud (VPC) IP address of the client.
    clientVpcIpAddress :: Prelude.Maybe Prelude.Text,
    -- | Endpoints for accessing the ZooKeeper.
    endpoints :: Prelude.Maybe [Prelude.Text],
    -- | The role-specific ID for Zookeeper.
    zookeeperId :: Prelude.Maybe Prelude.Double,
    -- | The version of Zookeeper.
    zookeeperVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ZookeeperNodeInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachedENIId', 'zookeeperNodeInfo_attachedENIId' - The attached elastic network interface of the broker.
--
-- 'clientVpcIpAddress', 'zookeeperNodeInfo_clientVpcIpAddress' - The virtual private cloud (VPC) IP address of the client.
--
-- 'endpoints', 'zookeeperNodeInfo_endpoints' - Endpoints for accessing the ZooKeeper.
--
-- 'zookeeperId', 'zookeeperNodeInfo_zookeeperId' - The role-specific ID for Zookeeper.
--
-- 'zookeeperVersion', 'zookeeperNodeInfo_zookeeperVersion' - The version of Zookeeper.
newZookeeperNodeInfo ::
  ZookeeperNodeInfo
newZookeeperNodeInfo =
  ZookeeperNodeInfo'
    { attachedENIId = Prelude.Nothing,
      clientVpcIpAddress = Prelude.Nothing,
      endpoints = Prelude.Nothing,
      zookeeperId = Prelude.Nothing,
      zookeeperVersion = Prelude.Nothing
    }

-- | The attached elastic network interface of the broker.
zookeeperNodeInfo_attachedENIId :: Lens.Lens' ZookeeperNodeInfo (Prelude.Maybe Prelude.Text)
zookeeperNodeInfo_attachedENIId = Lens.lens (\ZookeeperNodeInfo' {attachedENIId} -> attachedENIId) (\s@ZookeeperNodeInfo' {} a -> s {attachedENIId = a} :: ZookeeperNodeInfo)

-- | The virtual private cloud (VPC) IP address of the client.
zookeeperNodeInfo_clientVpcIpAddress :: Lens.Lens' ZookeeperNodeInfo (Prelude.Maybe Prelude.Text)
zookeeperNodeInfo_clientVpcIpAddress = Lens.lens (\ZookeeperNodeInfo' {clientVpcIpAddress} -> clientVpcIpAddress) (\s@ZookeeperNodeInfo' {} a -> s {clientVpcIpAddress = a} :: ZookeeperNodeInfo)

-- | Endpoints for accessing the ZooKeeper.
zookeeperNodeInfo_endpoints :: Lens.Lens' ZookeeperNodeInfo (Prelude.Maybe [Prelude.Text])
zookeeperNodeInfo_endpoints = Lens.lens (\ZookeeperNodeInfo' {endpoints} -> endpoints) (\s@ZookeeperNodeInfo' {} a -> s {endpoints = a} :: ZookeeperNodeInfo) Prelude.. Lens.mapping Lens.coerced

-- | The role-specific ID for Zookeeper.
zookeeperNodeInfo_zookeeperId :: Lens.Lens' ZookeeperNodeInfo (Prelude.Maybe Prelude.Double)
zookeeperNodeInfo_zookeeperId = Lens.lens (\ZookeeperNodeInfo' {zookeeperId} -> zookeeperId) (\s@ZookeeperNodeInfo' {} a -> s {zookeeperId = a} :: ZookeeperNodeInfo)

-- | The version of Zookeeper.
zookeeperNodeInfo_zookeeperVersion :: Lens.Lens' ZookeeperNodeInfo (Prelude.Maybe Prelude.Text)
zookeeperNodeInfo_zookeeperVersion = Lens.lens (\ZookeeperNodeInfo' {zookeeperVersion} -> zookeeperVersion) (\s@ZookeeperNodeInfo' {} a -> s {zookeeperVersion = a} :: ZookeeperNodeInfo)

instance Data.FromJSON ZookeeperNodeInfo where
  parseJSON =
    Data.withObject
      "ZookeeperNodeInfo"
      ( \x ->
          ZookeeperNodeInfo'
            Prelude.<$> (x Data..:? "attachedENIId")
            Prelude.<*> (x Data..:? "clientVpcIpAddress")
            Prelude.<*> (x Data..:? "endpoints" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "zookeeperId")
            Prelude.<*> (x Data..:? "zookeeperVersion")
      )

instance Prelude.Hashable ZookeeperNodeInfo where
  hashWithSalt _salt ZookeeperNodeInfo' {..} =
    _salt `Prelude.hashWithSalt` attachedENIId
      `Prelude.hashWithSalt` clientVpcIpAddress
      `Prelude.hashWithSalt` endpoints
      `Prelude.hashWithSalt` zookeeperId
      `Prelude.hashWithSalt` zookeeperVersion

instance Prelude.NFData ZookeeperNodeInfo where
  rnf ZookeeperNodeInfo' {..} =
    Prelude.rnf attachedENIId
      `Prelude.seq` Prelude.rnf clientVpcIpAddress
      `Prelude.seq` Prelude.rnf endpoints
      `Prelude.seq` Prelude.rnf zookeeperId
      `Prelude.seq` Prelude.rnf zookeeperVersion
