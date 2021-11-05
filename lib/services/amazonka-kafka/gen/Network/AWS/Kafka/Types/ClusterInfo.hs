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
-- Module      : Amazonka.Kafka.Types.ClusterInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.ClusterInfo where

import qualified Amazonka.Core as Core
import Amazonka.Kafka.Types.BrokerNodeGroupInfo
import Amazonka.Kafka.Types.BrokerSoftwareInfo
import Amazonka.Kafka.Types.ClientAuthentication
import Amazonka.Kafka.Types.ClusterState
import Amazonka.Kafka.Types.EncryptionInfo
import Amazonka.Kafka.Types.EnhancedMonitoring
import Amazonka.Kafka.Types.LoggingInfo
import Amazonka.Kafka.Types.OpenMonitoring
import Amazonka.Kafka.Types.StateInfo
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Returns information about a cluster.
--
-- /See:/ 'newClusterInfo' smart constructor.
data ClusterInfo = ClusterInfo'
  { -- | The time when the cluster was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | Arn of active cluster operation.
    activeOperationArn :: Prelude.Maybe Prelude.Text,
    -- | The state of the cluster. The possible states are ACTIVE, CREATING,
    -- DELETING, FAILED, HEALING, MAINTENANCE, REBOOTING_BROKER, and UPDATING.
    state :: Prelude.Maybe ClusterState,
    -- | The Amazon Resource Name (ARN) that uniquely identifies the cluster.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | The number of broker nodes in the cluster.
    numberOfBrokerNodes :: Prelude.Maybe Prelude.Int,
    -- | Specifies which metrics are gathered for the MSK cluster. This property
    -- has the following possible values: DEFAULT, PER_BROKER,
    -- PER_TOPIC_PER_BROKER, and PER_TOPIC_PER_PARTITION. For a list of the
    -- metrics associated with each of these levels of monitoring, see
    -- <https://docs.aws.amazon.com/msk/latest/developerguide/monitoring.html Monitoring>.
    enhancedMonitoring :: Prelude.Maybe EnhancedMonitoring,
    -- | Information about the broker nodes.
    brokerNodeGroupInfo :: Prelude.Maybe BrokerNodeGroupInfo,
    -- | Settings for open monitoring using Prometheus.
    openMonitoring :: Prelude.Maybe OpenMonitoring,
    -- | Information about the version of software currently deployed on the
    -- Kafka brokers in the cluster.
    currentBrokerSoftwareInfo :: Prelude.Maybe BrokerSoftwareInfo,
    -- | The current version of the MSK cluster.
    currentVersion :: Prelude.Maybe Prelude.Text,
    stateInfo :: Prelude.Maybe StateInfo,
    loggingInfo :: Prelude.Maybe LoggingInfo,
    -- | The connection string to use to connect to the Apache ZooKeeper cluster.
    zookeeperConnectString :: Prelude.Maybe Prelude.Text,
    -- | The name of the cluster.
    clusterName :: Prelude.Maybe Prelude.Text,
    -- | The connection string to use to connect to zookeeper cluster on Tls
    -- port.
    zookeeperConnectStringTls :: Prelude.Maybe Prelude.Text,
    -- | Includes all client authentication information.
    clientAuthentication :: Prelude.Maybe ClientAuthentication,
    -- | Tags attached to the cluster.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Includes all encryption-related information.
    encryptionInfo :: Prelude.Maybe EncryptionInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClusterInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'clusterInfo_creationTime' - The time when the cluster was created.
--
-- 'activeOperationArn', 'clusterInfo_activeOperationArn' - Arn of active cluster operation.
--
-- 'state', 'clusterInfo_state' - The state of the cluster. The possible states are ACTIVE, CREATING,
-- DELETING, FAILED, HEALING, MAINTENANCE, REBOOTING_BROKER, and UPDATING.
--
-- 'clusterArn', 'clusterInfo_clusterArn' - The Amazon Resource Name (ARN) that uniquely identifies the cluster.
--
-- 'numberOfBrokerNodes', 'clusterInfo_numberOfBrokerNodes' - The number of broker nodes in the cluster.
--
-- 'enhancedMonitoring', 'clusterInfo_enhancedMonitoring' - Specifies which metrics are gathered for the MSK cluster. This property
-- has the following possible values: DEFAULT, PER_BROKER,
-- PER_TOPIC_PER_BROKER, and PER_TOPIC_PER_PARTITION. For a list of the
-- metrics associated with each of these levels of monitoring, see
-- <https://docs.aws.amazon.com/msk/latest/developerguide/monitoring.html Monitoring>.
--
-- 'brokerNodeGroupInfo', 'clusterInfo_brokerNodeGroupInfo' - Information about the broker nodes.
--
-- 'openMonitoring', 'clusterInfo_openMonitoring' - Settings for open monitoring using Prometheus.
--
-- 'currentBrokerSoftwareInfo', 'clusterInfo_currentBrokerSoftwareInfo' - Information about the version of software currently deployed on the
-- Kafka brokers in the cluster.
--
-- 'currentVersion', 'clusterInfo_currentVersion' - The current version of the MSK cluster.
--
-- 'stateInfo', 'clusterInfo_stateInfo' - Undocumented member.
--
-- 'loggingInfo', 'clusterInfo_loggingInfo' - Undocumented member.
--
-- 'zookeeperConnectString', 'clusterInfo_zookeeperConnectString' - The connection string to use to connect to the Apache ZooKeeper cluster.
--
-- 'clusterName', 'clusterInfo_clusterName' - The name of the cluster.
--
-- 'zookeeperConnectStringTls', 'clusterInfo_zookeeperConnectStringTls' - The connection string to use to connect to zookeeper cluster on Tls
-- port.
--
-- 'clientAuthentication', 'clusterInfo_clientAuthentication' - Includes all client authentication information.
--
-- 'tags', 'clusterInfo_tags' - Tags attached to the cluster.
--
-- 'encryptionInfo', 'clusterInfo_encryptionInfo' - Includes all encryption-related information.
newClusterInfo ::
  ClusterInfo
newClusterInfo =
  ClusterInfo'
    { creationTime = Prelude.Nothing,
      activeOperationArn = Prelude.Nothing,
      state = Prelude.Nothing,
      clusterArn = Prelude.Nothing,
      numberOfBrokerNodes = Prelude.Nothing,
      enhancedMonitoring = Prelude.Nothing,
      brokerNodeGroupInfo = Prelude.Nothing,
      openMonitoring = Prelude.Nothing,
      currentBrokerSoftwareInfo = Prelude.Nothing,
      currentVersion = Prelude.Nothing,
      stateInfo = Prelude.Nothing,
      loggingInfo = Prelude.Nothing,
      zookeeperConnectString = Prelude.Nothing,
      clusterName = Prelude.Nothing,
      zookeeperConnectStringTls = Prelude.Nothing,
      clientAuthentication = Prelude.Nothing,
      tags = Prelude.Nothing,
      encryptionInfo = Prelude.Nothing
    }

-- | The time when the cluster was created.
clusterInfo_creationTime :: Lens.Lens' ClusterInfo (Prelude.Maybe Prelude.UTCTime)
clusterInfo_creationTime = Lens.lens (\ClusterInfo' {creationTime} -> creationTime) (\s@ClusterInfo' {} a -> s {creationTime = a} :: ClusterInfo) Prelude.. Lens.mapping Core._Time

-- | Arn of active cluster operation.
clusterInfo_activeOperationArn :: Lens.Lens' ClusterInfo (Prelude.Maybe Prelude.Text)
clusterInfo_activeOperationArn = Lens.lens (\ClusterInfo' {activeOperationArn} -> activeOperationArn) (\s@ClusterInfo' {} a -> s {activeOperationArn = a} :: ClusterInfo)

-- | The state of the cluster. The possible states are ACTIVE, CREATING,
-- DELETING, FAILED, HEALING, MAINTENANCE, REBOOTING_BROKER, and UPDATING.
clusterInfo_state :: Lens.Lens' ClusterInfo (Prelude.Maybe ClusterState)
clusterInfo_state = Lens.lens (\ClusterInfo' {state} -> state) (\s@ClusterInfo' {} a -> s {state = a} :: ClusterInfo)

-- | The Amazon Resource Name (ARN) that uniquely identifies the cluster.
clusterInfo_clusterArn :: Lens.Lens' ClusterInfo (Prelude.Maybe Prelude.Text)
clusterInfo_clusterArn = Lens.lens (\ClusterInfo' {clusterArn} -> clusterArn) (\s@ClusterInfo' {} a -> s {clusterArn = a} :: ClusterInfo)

-- | The number of broker nodes in the cluster.
clusterInfo_numberOfBrokerNodes :: Lens.Lens' ClusterInfo (Prelude.Maybe Prelude.Int)
clusterInfo_numberOfBrokerNodes = Lens.lens (\ClusterInfo' {numberOfBrokerNodes} -> numberOfBrokerNodes) (\s@ClusterInfo' {} a -> s {numberOfBrokerNodes = a} :: ClusterInfo)

-- | Specifies which metrics are gathered for the MSK cluster. This property
-- has the following possible values: DEFAULT, PER_BROKER,
-- PER_TOPIC_PER_BROKER, and PER_TOPIC_PER_PARTITION. For a list of the
-- metrics associated with each of these levels of monitoring, see
-- <https://docs.aws.amazon.com/msk/latest/developerguide/monitoring.html Monitoring>.
clusterInfo_enhancedMonitoring :: Lens.Lens' ClusterInfo (Prelude.Maybe EnhancedMonitoring)
clusterInfo_enhancedMonitoring = Lens.lens (\ClusterInfo' {enhancedMonitoring} -> enhancedMonitoring) (\s@ClusterInfo' {} a -> s {enhancedMonitoring = a} :: ClusterInfo)

-- | Information about the broker nodes.
clusterInfo_brokerNodeGroupInfo :: Lens.Lens' ClusterInfo (Prelude.Maybe BrokerNodeGroupInfo)
clusterInfo_brokerNodeGroupInfo = Lens.lens (\ClusterInfo' {brokerNodeGroupInfo} -> brokerNodeGroupInfo) (\s@ClusterInfo' {} a -> s {brokerNodeGroupInfo = a} :: ClusterInfo)

-- | Settings for open monitoring using Prometheus.
clusterInfo_openMonitoring :: Lens.Lens' ClusterInfo (Prelude.Maybe OpenMonitoring)
clusterInfo_openMonitoring = Lens.lens (\ClusterInfo' {openMonitoring} -> openMonitoring) (\s@ClusterInfo' {} a -> s {openMonitoring = a} :: ClusterInfo)

-- | Information about the version of software currently deployed on the
-- Kafka brokers in the cluster.
clusterInfo_currentBrokerSoftwareInfo :: Lens.Lens' ClusterInfo (Prelude.Maybe BrokerSoftwareInfo)
clusterInfo_currentBrokerSoftwareInfo = Lens.lens (\ClusterInfo' {currentBrokerSoftwareInfo} -> currentBrokerSoftwareInfo) (\s@ClusterInfo' {} a -> s {currentBrokerSoftwareInfo = a} :: ClusterInfo)

-- | The current version of the MSK cluster.
clusterInfo_currentVersion :: Lens.Lens' ClusterInfo (Prelude.Maybe Prelude.Text)
clusterInfo_currentVersion = Lens.lens (\ClusterInfo' {currentVersion} -> currentVersion) (\s@ClusterInfo' {} a -> s {currentVersion = a} :: ClusterInfo)

-- | Undocumented member.
clusterInfo_stateInfo :: Lens.Lens' ClusterInfo (Prelude.Maybe StateInfo)
clusterInfo_stateInfo = Lens.lens (\ClusterInfo' {stateInfo} -> stateInfo) (\s@ClusterInfo' {} a -> s {stateInfo = a} :: ClusterInfo)

-- | Undocumented member.
clusterInfo_loggingInfo :: Lens.Lens' ClusterInfo (Prelude.Maybe LoggingInfo)
clusterInfo_loggingInfo = Lens.lens (\ClusterInfo' {loggingInfo} -> loggingInfo) (\s@ClusterInfo' {} a -> s {loggingInfo = a} :: ClusterInfo)

-- | The connection string to use to connect to the Apache ZooKeeper cluster.
clusterInfo_zookeeperConnectString :: Lens.Lens' ClusterInfo (Prelude.Maybe Prelude.Text)
clusterInfo_zookeeperConnectString = Lens.lens (\ClusterInfo' {zookeeperConnectString} -> zookeeperConnectString) (\s@ClusterInfo' {} a -> s {zookeeperConnectString = a} :: ClusterInfo)

-- | The name of the cluster.
clusterInfo_clusterName :: Lens.Lens' ClusterInfo (Prelude.Maybe Prelude.Text)
clusterInfo_clusterName = Lens.lens (\ClusterInfo' {clusterName} -> clusterName) (\s@ClusterInfo' {} a -> s {clusterName = a} :: ClusterInfo)

-- | The connection string to use to connect to zookeeper cluster on Tls
-- port.
clusterInfo_zookeeperConnectStringTls :: Lens.Lens' ClusterInfo (Prelude.Maybe Prelude.Text)
clusterInfo_zookeeperConnectStringTls = Lens.lens (\ClusterInfo' {zookeeperConnectStringTls} -> zookeeperConnectStringTls) (\s@ClusterInfo' {} a -> s {zookeeperConnectStringTls = a} :: ClusterInfo)

-- | Includes all client authentication information.
clusterInfo_clientAuthentication :: Lens.Lens' ClusterInfo (Prelude.Maybe ClientAuthentication)
clusterInfo_clientAuthentication = Lens.lens (\ClusterInfo' {clientAuthentication} -> clientAuthentication) (\s@ClusterInfo' {} a -> s {clientAuthentication = a} :: ClusterInfo)

-- | Tags attached to the cluster.
clusterInfo_tags :: Lens.Lens' ClusterInfo (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
clusterInfo_tags = Lens.lens (\ClusterInfo' {tags} -> tags) (\s@ClusterInfo' {} a -> s {tags = a} :: ClusterInfo) Prelude.. Lens.mapping Lens.coerced

-- | Includes all encryption-related information.
clusterInfo_encryptionInfo :: Lens.Lens' ClusterInfo (Prelude.Maybe EncryptionInfo)
clusterInfo_encryptionInfo = Lens.lens (\ClusterInfo' {encryptionInfo} -> encryptionInfo) (\s@ClusterInfo' {} a -> s {encryptionInfo = a} :: ClusterInfo)

instance Core.FromJSON ClusterInfo where
  parseJSON =
    Core.withObject
      "ClusterInfo"
      ( \x ->
          ClusterInfo'
            Prelude.<$> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "activeOperationArn")
            Prelude.<*> (x Core..:? "state")
            Prelude.<*> (x Core..:? "clusterArn")
            Prelude.<*> (x Core..:? "numberOfBrokerNodes")
            Prelude.<*> (x Core..:? "enhancedMonitoring")
            Prelude.<*> (x Core..:? "brokerNodeGroupInfo")
            Prelude.<*> (x Core..:? "openMonitoring")
            Prelude.<*> (x Core..:? "currentBrokerSoftwareInfo")
            Prelude.<*> (x Core..:? "currentVersion")
            Prelude.<*> (x Core..:? "stateInfo")
            Prelude.<*> (x Core..:? "loggingInfo")
            Prelude.<*> (x Core..:? "zookeeperConnectString")
            Prelude.<*> (x Core..:? "clusterName")
            Prelude.<*> (x Core..:? "zookeeperConnectStringTls")
            Prelude.<*> (x Core..:? "clientAuthentication")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "encryptionInfo")
      )

instance Prelude.Hashable ClusterInfo

instance Prelude.NFData ClusterInfo
