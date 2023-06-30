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
-- Module      : Amazonka.Kafka.Types.BrokerNodeInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.BrokerNodeInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types.BrokerSoftwareInfo
import qualified Amazonka.Prelude as Prelude

-- | BrokerNodeInfo
--
-- /See:/ 'newBrokerNodeInfo' smart constructor.
data BrokerNodeInfo = BrokerNodeInfo'
  { -- | The attached elastic network interface of the broker.
    attachedENIId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the broker.
    brokerId :: Prelude.Maybe Prelude.Double,
    -- | The client subnet to which this broker node belongs.
    clientSubnet :: Prelude.Maybe Prelude.Text,
    -- | The virtual private cloud (VPC) of the client.
    clientVpcIpAddress :: Prelude.Maybe Prelude.Text,
    -- | Information about the version of software currently deployed on the
    -- Apache Kafka brokers in the cluster.
    currentBrokerSoftwareInfo :: Prelude.Maybe BrokerSoftwareInfo,
    -- | Endpoints for accessing the broker.
    endpoints :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BrokerNodeInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachedENIId', 'brokerNodeInfo_attachedENIId' - The attached elastic network interface of the broker.
--
-- 'brokerId', 'brokerNodeInfo_brokerId' - The ID of the broker.
--
-- 'clientSubnet', 'brokerNodeInfo_clientSubnet' - The client subnet to which this broker node belongs.
--
-- 'clientVpcIpAddress', 'brokerNodeInfo_clientVpcIpAddress' - The virtual private cloud (VPC) of the client.
--
-- 'currentBrokerSoftwareInfo', 'brokerNodeInfo_currentBrokerSoftwareInfo' - Information about the version of software currently deployed on the
-- Apache Kafka brokers in the cluster.
--
-- 'endpoints', 'brokerNodeInfo_endpoints' - Endpoints for accessing the broker.
newBrokerNodeInfo ::
  BrokerNodeInfo
newBrokerNodeInfo =
  BrokerNodeInfo'
    { attachedENIId = Prelude.Nothing,
      brokerId = Prelude.Nothing,
      clientSubnet = Prelude.Nothing,
      clientVpcIpAddress = Prelude.Nothing,
      currentBrokerSoftwareInfo = Prelude.Nothing,
      endpoints = Prelude.Nothing
    }

-- | The attached elastic network interface of the broker.
brokerNodeInfo_attachedENIId :: Lens.Lens' BrokerNodeInfo (Prelude.Maybe Prelude.Text)
brokerNodeInfo_attachedENIId = Lens.lens (\BrokerNodeInfo' {attachedENIId} -> attachedENIId) (\s@BrokerNodeInfo' {} a -> s {attachedENIId = a} :: BrokerNodeInfo)

-- | The ID of the broker.
brokerNodeInfo_brokerId :: Lens.Lens' BrokerNodeInfo (Prelude.Maybe Prelude.Double)
brokerNodeInfo_brokerId = Lens.lens (\BrokerNodeInfo' {brokerId} -> brokerId) (\s@BrokerNodeInfo' {} a -> s {brokerId = a} :: BrokerNodeInfo)

-- | The client subnet to which this broker node belongs.
brokerNodeInfo_clientSubnet :: Lens.Lens' BrokerNodeInfo (Prelude.Maybe Prelude.Text)
brokerNodeInfo_clientSubnet = Lens.lens (\BrokerNodeInfo' {clientSubnet} -> clientSubnet) (\s@BrokerNodeInfo' {} a -> s {clientSubnet = a} :: BrokerNodeInfo)

-- | The virtual private cloud (VPC) of the client.
brokerNodeInfo_clientVpcIpAddress :: Lens.Lens' BrokerNodeInfo (Prelude.Maybe Prelude.Text)
brokerNodeInfo_clientVpcIpAddress = Lens.lens (\BrokerNodeInfo' {clientVpcIpAddress} -> clientVpcIpAddress) (\s@BrokerNodeInfo' {} a -> s {clientVpcIpAddress = a} :: BrokerNodeInfo)

-- | Information about the version of software currently deployed on the
-- Apache Kafka brokers in the cluster.
brokerNodeInfo_currentBrokerSoftwareInfo :: Lens.Lens' BrokerNodeInfo (Prelude.Maybe BrokerSoftwareInfo)
brokerNodeInfo_currentBrokerSoftwareInfo = Lens.lens (\BrokerNodeInfo' {currentBrokerSoftwareInfo} -> currentBrokerSoftwareInfo) (\s@BrokerNodeInfo' {} a -> s {currentBrokerSoftwareInfo = a} :: BrokerNodeInfo)

-- | Endpoints for accessing the broker.
brokerNodeInfo_endpoints :: Lens.Lens' BrokerNodeInfo (Prelude.Maybe [Prelude.Text])
brokerNodeInfo_endpoints = Lens.lens (\BrokerNodeInfo' {endpoints} -> endpoints) (\s@BrokerNodeInfo' {} a -> s {endpoints = a} :: BrokerNodeInfo) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON BrokerNodeInfo where
  parseJSON =
    Data.withObject
      "BrokerNodeInfo"
      ( \x ->
          BrokerNodeInfo'
            Prelude.<$> (x Data..:? "attachedENIId")
            Prelude.<*> (x Data..:? "brokerId")
            Prelude.<*> (x Data..:? "clientSubnet")
            Prelude.<*> (x Data..:? "clientVpcIpAddress")
            Prelude.<*> (x Data..:? "currentBrokerSoftwareInfo")
            Prelude.<*> (x Data..:? "endpoints" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable BrokerNodeInfo where
  hashWithSalt _salt BrokerNodeInfo' {..} =
    _salt
      `Prelude.hashWithSalt` attachedENIId
      `Prelude.hashWithSalt` brokerId
      `Prelude.hashWithSalt` clientSubnet
      `Prelude.hashWithSalt` clientVpcIpAddress
      `Prelude.hashWithSalt` currentBrokerSoftwareInfo
      `Prelude.hashWithSalt` endpoints

instance Prelude.NFData BrokerNodeInfo where
  rnf BrokerNodeInfo' {..} =
    Prelude.rnf attachedENIId
      `Prelude.seq` Prelude.rnf brokerId
      `Prelude.seq` Prelude.rnf clientSubnet
      `Prelude.seq` Prelude.rnf clientVpcIpAddress
      `Prelude.seq` Prelude.rnf currentBrokerSoftwareInfo
      `Prelude.seq` Prelude.rnf endpoints
