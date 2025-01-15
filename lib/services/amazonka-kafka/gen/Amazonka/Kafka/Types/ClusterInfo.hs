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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.ClusterInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types.BrokerNodeGroupInfo
import Amazonka.Kafka.Types.BrokerSoftwareInfo
import Amazonka.Kafka.Types.ClientAuthentication
import Amazonka.Kafka.Types.ClusterState
import Amazonka.Kafka.Types.EncryptionInfo
import Amazonka.Kafka.Types.EnhancedMonitoring
import Amazonka.Kafka.Types.LoggingInfo
import Amazonka.Kafka.Types.OpenMonitoring
import Amazonka.Kafka.Types.StateInfo
import Amazonka.Kafka.Types.StorageMode
import qualified Amazonka.Prelude as Prelude

-- | Returns information about a cluster.
--
-- /See:/ 'newClusterInfo' smart constructor.
data ClusterInfo = ClusterInfo'
  { -- | Arn of active cluster operation.
    activeOperationArn :: Prelude.Maybe Prelude.Text,
    -- | Information about the broker nodes.
    brokerNodeGroupInfo :: Prelude.Maybe BrokerNodeGroupInfo,
    -- | Includes all client authentication information.
    clientAuthentication :: Prelude.Maybe ClientAuthentication,
    -- | The Amazon Resource Name (ARN) that uniquely identifies the cluster.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the cluster.
    clusterName :: Prelude.Maybe Prelude.Text,
    -- | The time when the cluster was created.
    creationTime :: Prelude.Maybe Data.ISO8601,
    -- | Information about the version of software currently deployed on the
    -- Apache Kafka brokers in the cluster.
    currentBrokerSoftwareInfo :: Prelude.Maybe BrokerSoftwareInfo,
    -- | The current version of the MSK cluster.
    currentVersion :: Prelude.Maybe Prelude.Text,
    -- | Includes all encryption-related information.
    encryptionInfo :: Prelude.Maybe EncryptionInfo,
    -- | Specifies which metrics are gathered for the MSK cluster. This property
    -- has the following possible values: DEFAULT, PER_BROKER,
    -- PER_TOPIC_PER_BROKER, and PER_TOPIC_PER_PARTITION. For a list of the
    -- metrics associated with each of these levels of monitoring, see
    -- <https://docs.aws.amazon.com/msk/latest/developerguide/monitoring.html Monitoring>.
    enhancedMonitoring :: Prelude.Maybe EnhancedMonitoring,
    loggingInfo :: Prelude.Maybe LoggingInfo,
    -- | The number of broker nodes in the cluster.
    numberOfBrokerNodes :: Prelude.Maybe Prelude.Int,
    -- | Settings for open monitoring using Prometheus.
    openMonitoring :: Prelude.Maybe OpenMonitoring,
    -- | The state of the cluster. The possible states are ACTIVE, CREATING,
    -- DELETING, FAILED, HEALING, MAINTENANCE, REBOOTING_BROKER, and UPDATING.
    state :: Prelude.Maybe ClusterState,
    stateInfo :: Prelude.Maybe StateInfo,
    -- | This controls storage mode for supported storage tiers.
    storageMode :: Prelude.Maybe StorageMode,
    -- | Tags attached to the cluster.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The connection string to use to connect to the Apache ZooKeeper cluster.
    zookeeperConnectString :: Prelude.Maybe Prelude.Text,
    -- | The connection string to use to connect to zookeeper cluster on Tls
    -- port.
    zookeeperConnectStringTls :: Prelude.Maybe Prelude.Text
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
-- 'activeOperationArn', 'clusterInfo_activeOperationArn' - Arn of active cluster operation.
--
-- 'brokerNodeGroupInfo', 'clusterInfo_brokerNodeGroupInfo' - Information about the broker nodes.
--
-- 'clientAuthentication', 'clusterInfo_clientAuthentication' - Includes all client authentication information.
--
-- 'clusterArn', 'clusterInfo_clusterArn' - The Amazon Resource Name (ARN) that uniquely identifies the cluster.
--
-- 'clusterName', 'clusterInfo_clusterName' - The name of the cluster.
--
-- 'creationTime', 'clusterInfo_creationTime' - The time when the cluster was created.
--
-- 'currentBrokerSoftwareInfo', 'clusterInfo_currentBrokerSoftwareInfo' - Information about the version of software currently deployed on the
-- Apache Kafka brokers in the cluster.
--
-- 'currentVersion', 'clusterInfo_currentVersion' - The current version of the MSK cluster.
--
-- 'encryptionInfo', 'clusterInfo_encryptionInfo' - Includes all encryption-related information.
--
-- 'enhancedMonitoring', 'clusterInfo_enhancedMonitoring' - Specifies which metrics are gathered for the MSK cluster. This property
-- has the following possible values: DEFAULT, PER_BROKER,
-- PER_TOPIC_PER_BROKER, and PER_TOPIC_PER_PARTITION. For a list of the
-- metrics associated with each of these levels of monitoring, see
-- <https://docs.aws.amazon.com/msk/latest/developerguide/monitoring.html Monitoring>.
--
-- 'loggingInfo', 'clusterInfo_loggingInfo' - Undocumented member.
--
-- 'numberOfBrokerNodes', 'clusterInfo_numberOfBrokerNodes' - The number of broker nodes in the cluster.
--
-- 'openMonitoring', 'clusterInfo_openMonitoring' - Settings for open monitoring using Prometheus.
--
-- 'state', 'clusterInfo_state' - The state of the cluster. The possible states are ACTIVE, CREATING,
-- DELETING, FAILED, HEALING, MAINTENANCE, REBOOTING_BROKER, and UPDATING.
--
-- 'stateInfo', 'clusterInfo_stateInfo' - Undocumented member.
--
-- 'storageMode', 'clusterInfo_storageMode' - This controls storage mode for supported storage tiers.
--
-- 'tags', 'clusterInfo_tags' - Tags attached to the cluster.
--
-- 'zookeeperConnectString', 'clusterInfo_zookeeperConnectString' - The connection string to use to connect to the Apache ZooKeeper cluster.
--
-- 'zookeeperConnectStringTls', 'clusterInfo_zookeeperConnectStringTls' - The connection string to use to connect to zookeeper cluster on Tls
-- port.
newClusterInfo ::
  ClusterInfo
newClusterInfo =
  ClusterInfo'
    { activeOperationArn = Prelude.Nothing,
      brokerNodeGroupInfo = Prelude.Nothing,
      clientAuthentication = Prelude.Nothing,
      clusterArn = Prelude.Nothing,
      clusterName = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      currentBrokerSoftwareInfo = Prelude.Nothing,
      currentVersion = Prelude.Nothing,
      encryptionInfo = Prelude.Nothing,
      enhancedMonitoring = Prelude.Nothing,
      loggingInfo = Prelude.Nothing,
      numberOfBrokerNodes = Prelude.Nothing,
      openMonitoring = Prelude.Nothing,
      state = Prelude.Nothing,
      stateInfo = Prelude.Nothing,
      storageMode = Prelude.Nothing,
      tags = Prelude.Nothing,
      zookeeperConnectString = Prelude.Nothing,
      zookeeperConnectStringTls = Prelude.Nothing
    }

-- | Arn of active cluster operation.
clusterInfo_activeOperationArn :: Lens.Lens' ClusterInfo (Prelude.Maybe Prelude.Text)
clusterInfo_activeOperationArn = Lens.lens (\ClusterInfo' {activeOperationArn} -> activeOperationArn) (\s@ClusterInfo' {} a -> s {activeOperationArn = a} :: ClusterInfo)

-- | Information about the broker nodes.
clusterInfo_brokerNodeGroupInfo :: Lens.Lens' ClusterInfo (Prelude.Maybe BrokerNodeGroupInfo)
clusterInfo_brokerNodeGroupInfo = Lens.lens (\ClusterInfo' {brokerNodeGroupInfo} -> brokerNodeGroupInfo) (\s@ClusterInfo' {} a -> s {brokerNodeGroupInfo = a} :: ClusterInfo)

-- | Includes all client authentication information.
clusterInfo_clientAuthentication :: Lens.Lens' ClusterInfo (Prelude.Maybe ClientAuthentication)
clusterInfo_clientAuthentication = Lens.lens (\ClusterInfo' {clientAuthentication} -> clientAuthentication) (\s@ClusterInfo' {} a -> s {clientAuthentication = a} :: ClusterInfo)

-- | The Amazon Resource Name (ARN) that uniquely identifies the cluster.
clusterInfo_clusterArn :: Lens.Lens' ClusterInfo (Prelude.Maybe Prelude.Text)
clusterInfo_clusterArn = Lens.lens (\ClusterInfo' {clusterArn} -> clusterArn) (\s@ClusterInfo' {} a -> s {clusterArn = a} :: ClusterInfo)

-- | The name of the cluster.
clusterInfo_clusterName :: Lens.Lens' ClusterInfo (Prelude.Maybe Prelude.Text)
clusterInfo_clusterName = Lens.lens (\ClusterInfo' {clusterName} -> clusterName) (\s@ClusterInfo' {} a -> s {clusterName = a} :: ClusterInfo)

-- | The time when the cluster was created.
clusterInfo_creationTime :: Lens.Lens' ClusterInfo (Prelude.Maybe Prelude.UTCTime)
clusterInfo_creationTime = Lens.lens (\ClusterInfo' {creationTime} -> creationTime) (\s@ClusterInfo' {} a -> s {creationTime = a} :: ClusterInfo) Prelude.. Lens.mapping Data._Time

-- | Information about the version of software currently deployed on the
-- Apache Kafka brokers in the cluster.
clusterInfo_currentBrokerSoftwareInfo :: Lens.Lens' ClusterInfo (Prelude.Maybe BrokerSoftwareInfo)
clusterInfo_currentBrokerSoftwareInfo = Lens.lens (\ClusterInfo' {currentBrokerSoftwareInfo} -> currentBrokerSoftwareInfo) (\s@ClusterInfo' {} a -> s {currentBrokerSoftwareInfo = a} :: ClusterInfo)

-- | The current version of the MSK cluster.
clusterInfo_currentVersion :: Lens.Lens' ClusterInfo (Prelude.Maybe Prelude.Text)
clusterInfo_currentVersion = Lens.lens (\ClusterInfo' {currentVersion} -> currentVersion) (\s@ClusterInfo' {} a -> s {currentVersion = a} :: ClusterInfo)

-- | Includes all encryption-related information.
clusterInfo_encryptionInfo :: Lens.Lens' ClusterInfo (Prelude.Maybe EncryptionInfo)
clusterInfo_encryptionInfo = Lens.lens (\ClusterInfo' {encryptionInfo} -> encryptionInfo) (\s@ClusterInfo' {} a -> s {encryptionInfo = a} :: ClusterInfo)

-- | Specifies which metrics are gathered for the MSK cluster. This property
-- has the following possible values: DEFAULT, PER_BROKER,
-- PER_TOPIC_PER_BROKER, and PER_TOPIC_PER_PARTITION. For a list of the
-- metrics associated with each of these levels of monitoring, see
-- <https://docs.aws.amazon.com/msk/latest/developerguide/monitoring.html Monitoring>.
clusterInfo_enhancedMonitoring :: Lens.Lens' ClusterInfo (Prelude.Maybe EnhancedMonitoring)
clusterInfo_enhancedMonitoring = Lens.lens (\ClusterInfo' {enhancedMonitoring} -> enhancedMonitoring) (\s@ClusterInfo' {} a -> s {enhancedMonitoring = a} :: ClusterInfo)

-- | Undocumented member.
clusterInfo_loggingInfo :: Lens.Lens' ClusterInfo (Prelude.Maybe LoggingInfo)
clusterInfo_loggingInfo = Lens.lens (\ClusterInfo' {loggingInfo} -> loggingInfo) (\s@ClusterInfo' {} a -> s {loggingInfo = a} :: ClusterInfo)

-- | The number of broker nodes in the cluster.
clusterInfo_numberOfBrokerNodes :: Lens.Lens' ClusterInfo (Prelude.Maybe Prelude.Int)
clusterInfo_numberOfBrokerNodes = Lens.lens (\ClusterInfo' {numberOfBrokerNodes} -> numberOfBrokerNodes) (\s@ClusterInfo' {} a -> s {numberOfBrokerNodes = a} :: ClusterInfo)

-- | Settings for open monitoring using Prometheus.
clusterInfo_openMonitoring :: Lens.Lens' ClusterInfo (Prelude.Maybe OpenMonitoring)
clusterInfo_openMonitoring = Lens.lens (\ClusterInfo' {openMonitoring} -> openMonitoring) (\s@ClusterInfo' {} a -> s {openMonitoring = a} :: ClusterInfo)

-- | The state of the cluster. The possible states are ACTIVE, CREATING,
-- DELETING, FAILED, HEALING, MAINTENANCE, REBOOTING_BROKER, and UPDATING.
clusterInfo_state :: Lens.Lens' ClusterInfo (Prelude.Maybe ClusterState)
clusterInfo_state = Lens.lens (\ClusterInfo' {state} -> state) (\s@ClusterInfo' {} a -> s {state = a} :: ClusterInfo)

-- | Undocumented member.
clusterInfo_stateInfo :: Lens.Lens' ClusterInfo (Prelude.Maybe StateInfo)
clusterInfo_stateInfo = Lens.lens (\ClusterInfo' {stateInfo} -> stateInfo) (\s@ClusterInfo' {} a -> s {stateInfo = a} :: ClusterInfo)

-- | This controls storage mode for supported storage tiers.
clusterInfo_storageMode :: Lens.Lens' ClusterInfo (Prelude.Maybe StorageMode)
clusterInfo_storageMode = Lens.lens (\ClusterInfo' {storageMode} -> storageMode) (\s@ClusterInfo' {} a -> s {storageMode = a} :: ClusterInfo)

-- | Tags attached to the cluster.
clusterInfo_tags :: Lens.Lens' ClusterInfo (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
clusterInfo_tags = Lens.lens (\ClusterInfo' {tags} -> tags) (\s@ClusterInfo' {} a -> s {tags = a} :: ClusterInfo) Prelude.. Lens.mapping Lens.coerced

-- | The connection string to use to connect to the Apache ZooKeeper cluster.
clusterInfo_zookeeperConnectString :: Lens.Lens' ClusterInfo (Prelude.Maybe Prelude.Text)
clusterInfo_zookeeperConnectString = Lens.lens (\ClusterInfo' {zookeeperConnectString} -> zookeeperConnectString) (\s@ClusterInfo' {} a -> s {zookeeperConnectString = a} :: ClusterInfo)

-- | The connection string to use to connect to zookeeper cluster on Tls
-- port.
clusterInfo_zookeeperConnectStringTls :: Lens.Lens' ClusterInfo (Prelude.Maybe Prelude.Text)
clusterInfo_zookeeperConnectStringTls = Lens.lens (\ClusterInfo' {zookeeperConnectStringTls} -> zookeeperConnectStringTls) (\s@ClusterInfo' {} a -> s {zookeeperConnectStringTls = a} :: ClusterInfo)

instance Data.FromJSON ClusterInfo where
  parseJSON =
    Data.withObject
      "ClusterInfo"
      ( \x ->
          ClusterInfo'
            Prelude.<$> (x Data..:? "activeOperationArn")
            Prelude.<*> (x Data..:? "brokerNodeGroupInfo")
            Prelude.<*> (x Data..:? "clientAuthentication")
            Prelude.<*> (x Data..:? "clusterArn")
            Prelude.<*> (x Data..:? "clusterName")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "currentBrokerSoftwareInfo")
            Prelude.<*> (x Data..:? "currentVersion")
            Prelude.<*> (x Data..:? "encryptionInfo")
            Prelude.<*> (x Data..:? "enhancedMonitoring")
            Prelude.<*> (x Data..:? "loggingInfo")
            Prelude.<*> (x Data..:? "numberOfBrokerNodes")
            Prelude.<*> (x Data..:? "openMonitoring")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "stateInfo")
            Prelude.<*> (x Data..:? "storageMode")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "zookeeperConnectString")
            Prelude.<*> (x Data..:? "zookeeperConnectStringTls")
      )

instance Prelude.Hashable ClusterInfo where
  hashWithSalt _salt ClusterInfo' {..} =
    _salt
      `Prelude.hashWithSalt` activeOperationArn
      `Prelude.hashWithSalt` brokerNodeGroupInfo
      `Prelude.hashWithSalt` clientAuthentication
      `Prelude.hashWithSalt` clusterArn
      `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` currentBrokerSoftwareInfo
      `Prelude.hashWithSalt` currentVersion
      `Prelude.hashWithSalt` encryptionInfo
      `Prelude.hashWithSalt` enhancedMonitoring
      `Prelude.hashWithSalt` loggingInfo
      `Prelude.hashWithSalt` numberOfBrokerNodes
      `Prelude.hashWithSalt` openMonitoring
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` stateInfo
      `Prelude.hashWithSalt` storageMode
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` zookeeperConnectString
      `Prelude.hashWithSalt` zookeeperConnectStringTls

instance Prelude.NFData ClusterInfo where
  rnf ClusterInfo' {..} =
    Prelude.rnf activeOperationArn `Prelude.seq`
      Prelude.rnf brokerNodeGroupInfo `Prelude.seq`
        Prelude.rnf clientAuthentication `Prelude.seq`
          Prelude.rnf clusterArn `Prelude.seq`
            Prelude.rnf clusterName `Prelude.seq`
              Prelude.rnf creationTime `Prelude.seq`
                Prelude.rnf currentBrokerSoftwareInfo `Prelude.seq`
                  Prelude.rnf currentVersion `Prelude.seq`
                    Prelude.rnf encryptionInfo `Prelude.seq`
                      Prelude.rnf enhancedMonitoring `Prelude.seq`
                        Prelude.rnf loggingInfo `Prelude.seq`
                          Prelude.rnf numberOfBrokerNodes `Prelude.seq`
                            Prelude.rnf openMonitoring `Prelude.seq`
                              Prelude.rnf state `Prelude.seq`
                                Prelude.rnf stateInfo `Prelude.seq`
                                  Prelude.rnf storageMode `Prelude.seq`
                                    Prelude.rnf tags `Prelude.seq`
                                      Prelude.rnf zookeeperConnectString `Prelude.seq`
                                        Prelude.rnf
                                          zookeeperConnectStringTls
