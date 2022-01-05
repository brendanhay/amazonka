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
-- Module      : Amazonka.Kafka.Types.BrokerNodeGroupInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.BrokerNodeGroupInfo where

import qualified Amazonka.Core as Core
import Amazonka.Kafka.Types.BrokerAZDistribution
import Amazonka.Kafka.Types.StorageInfo
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the setup to be used for Kafka broker nodes in the cluster.
--
-- /See:/ 'newBrokerNodeGroupInfo' smart constructor.
data BrokerNodeGroupInfo = BrokerNodeGroupInfo'
  { -- | Contains information about storage volumes attached to MSK broker nodes.
    storageInfo :: Prelude.Maybe StorageInfo,
    -- | The distribution of broker nodes across Availability Zones. This is an
    -- optional parameter. If you don\'t specify it, Amazon MSK gives it the
    -- value DEFAULT. You can also explicitly set this parameter to the value
    -- DEFAULT. No other values are currently allowed.
    --
    -- Amazon MSK distributes the broker nodes evenly across the Availability
    -- Zones that correspond to the subnets you provide when you create the
    -- cluster.
    brokerAZDistribution :: Prelude.Maybe BrokerAZDistribution,
    -- | The AWS security groups to associate with the elastic network interfaces
    -- in order to specify who can connect to and communicate with the Amazon
    -- MSK cluster. If you don\'t specify a security group, Amazon MSK uses the
    -- default security group associated with the VPC.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The list of subnets to connect to in the client virtual private cloud
    -- (VPC). AWS creates elastic network interfaces inside these subnets.
    -- Client applications use elastic network interfaces to produce and
    -- consume data. Client subnets can\'t be in Availability Zone us-east-1e.
    clientSubnets :: [Prelude.Text],
    -- | The type of Amazon EC2 instances to use for Kafka brokers. The following
    -- instance types are allowed: kafka.m5.large, kafka.m5.xlarge,
    -- kafka.m5.2xlarge, kafka.m5.4xlarge, kafka.m5.12xlarge, and
    -- kafka.m5.24xlarge.
    instanceType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BrokerNodeGroupInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'storageInfo', 'brokerNodeGroupInfo_storageInfo' - Contains information about storage volumes attached to MSK broker nodes.
--
-- 'brokerAZDistribution', 'brokerNodeGroupInfo_brokerAZDistribution' - The distribution of broker nodes across Availability Zones. This is an
-- optional parameter. If you don\'t specify it, Amazon MSK gives it the
-- value DEFAULT. You can also explicitly set this parameter to the value
-- DEFAULT. No other values are currently allowed.
--
-- Amazon MSK distributes the broker nodes evenly across the Availability
-- Zones that correspond to the subnets you provide when you create the
-- cluster.
--
-- 'securityGroups', 'brokerNodeGroupInfo_securityGroups' - The AWS security groups to associate with the elastic network interfaces
-- in order to specify who can connect to and communicate with the Amazon
-- MSK cluster. If you don\'t specify a security group, Amazon MSK uses the
-- default security group associated with the VPC.
--
-- 'clientSubnets', 'brokerNodeGroupInfo_clientSubnets' - The list of subnets to connect to in the client virtual private cloud
-- (VPC). AWS creates elastic network interfaces inside these subnets.
-- Client applications use elastic network interfaces to produce and
-- consume data. Client subnets can\'t be in Availability Zone us-east-1e.
--
-- 'instanceType', 'brokerNodeGroupInfo_instanceType' - The type of Amazon EC2 instances to use for Kafka brokers. The following
-- instance types are allowed: kafka.m5.large, kafka.m5.xlarge,
-- kafka.m5.2xlarge, kafka.m5.4xlarge, kafka.m5.12xlarge, and
-- kafka.m5.24xlarge.
newBrokerNodeGroupInfo ::
  -- | 'instanceType'
  Prelude.Text ->
  BrokerNodeGroupInfo
newBrokerNodeGroupInfo pInstanceType_ =
  BrokerNodeGroupInfo'
    { storageInfo = Prelude.Nothing,
      brokerAZDistribution = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      clientSubnets = Prelude.mempty,
      instanceType = pInstanceType_
    }

-- | Contains information about storage volumes attached to MSK broker nodes.
brokerNodeGroupInfo_storageInfo :: Lens.Lens' BrokerNodeGroupInfo (Prelude.Maybe StorageInfo)
brokerNodeGroupInfo_storageInfo = Lens.lens (\BrokerNodeGroupInfo' {storageInfo} -> storageInfo) (\s@BrokerNodeGroupInfo' {} a -> s {storageInfo = a} :: BrokerNodeGroupInfo)

-- | The distribution of broker nodes across Availability Zones. This is an
-- optional parameter. If you don\'t specify it, Amazon MSK gives it the
-- value DEFAULT. You can also explicitly set this parameter to the value
-- DEFAULT. No other values are currently allowed.
--
-- Amazon MSK distributes the broker nodes evenly across the Availability
-- Zones that correspond to the subnets you provide when you create the
-- cluster.
brokerNodeGroupInfo_brokerAZDistribution :: Lens.Lens' BrokerNodeGroupInfo (Prelude.Maybe BrokerAZDistribution)
brokerNodeGroupInfo_brokerAZDistribution = Lens.lens (\BrokerNodeGroupInfo' {brokerAZDistribution} -> brokerAZDistribution) (\s@BrokerNodeGroupInfo' {} a -> s {brokerAZDistribution = a} :: BrokerNodeGroupInfo)

-- | The AWS security groups to associate with the elastic network interfaces
-- in order to specify who can connect to and communicate with the Amazon
-- MSK cluster. If you don\'t specify a security group, Amazon MSK uses the
-- default security group associated with the VPC.
brokerNodeGroupInfo_securityGroups :: Lens.Lens' BrokerNodeGroupInfo (Prelude.Maybe [Prelude.Text])
brokerNodeGroupInfo_securityGroups = Lens.lens (\BrokerNodeGroupInfo' {securityGroups} -> securityGroups) (\s@BrokerNodeGroupInfo' {} a -> s {securityGroups = a} :: BrokerNodeGroupInfo) Prelude.. Lens.mapping Lens.coerced

-- | The list of subnets to connect to in the client virtual private cloud
-- (VPC). AWS creates elastic network interfaces inside these subnets.
-- Client applications use elastic network interfaces to produce and
-- consume data. Client subnets can\'t be in Availability Zone us-east-1e.
brokerNodeGroupInfo_clientSubnets :: Lens.Lens' BrokerNodeGroupInfo [Prelude.Text]
brokerNodeGroupInfo_clientSubnets = Lens.lens (\BrokerNodeGroupInfo' {clientSubnets} -> clientSubnets) (\s@BrokerNodeGroupInfo' {} a -> s {clientSubnets = a} :: BrokerNodeGroupInfo) Prelude.. Lens.coerced

-- | The type of Amazon EC2 instances to use for Kafka brokers. The following
-- instance types are allowed: kafka.m5.large, kafka.m5.xlarge,
-- kafka.m5.2xlarge, kafka.m5.4xlarge, kafka.m5.12xlarge, and
-- kafka.m5.24xlarge.
brokerNodeGroupInfo_instanceType :: Lens.Lens' BrokerNodeGroupInfo Prelude.Text
brokerNodeGroupInfo_instanceType = Lens.lens (\BrokerNodeGroupInfo' {instanceType} -> instanceType) (\s@BrokerNodeGroupInfo' {} a -> s {instanceType = a} :: BrokerNodeGroupInfo)

instance Core.FromJSON BrokerNodeGroupInfo where
  parseJSON =
    Core.withObject
      "BrokerNodeGroupInfo"
      ( \x ->
          BrokerNodeGroupInfo'
            Prelude.<$> (x Core..:? "storageInfo")
            Prelude.<*> (x Core..:? "brokerAZDistribution")
            Prelude.<*> (x Core..:? "securityGroups" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "clientSubnets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "instanceType")
      )

instance Prelude.Hashable BrokerNodeGroupInfo where
  hashWithSalt _salt BrokerNodeGroupInfo' {..} =
    _salt `Prelude.hashWithSalt` storageInfo
      `Prelude.hashWithSalt` brokerAZDistribution
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` clientSubnets
      `Prelude.hashWithSalt` instanceType

instance Prelude.NFData BrokerNodeGroupInfo where
  rnf BrokerNodeGroupInfo' {..} =
    Prelude.rnf storageInfo
      `Prelude.seq` Prelude.rnf brokerAZDistribution
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf clientSubnets
      `Prelude.seq` Prelude.rnf instanceType

instance Core.ToJSON BrokerNodeGroupInfo where
  toJSON BrokerNodeGroupInfo' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("storageInfo" Core..=) Prelude.<$> storageInfo,
            ("brokerAZDistribution" Core..=)
              Prelude.<$> brokerAZDistribution,
            ("securityGroups" Core..=)
              Prelude.<$> securityGroups,
            Prelude.Just ("clientSubnets" Core..= clientSubnets),
            Prelude.Just ("instanceType" Core..= instanceType)
          ]
      )
