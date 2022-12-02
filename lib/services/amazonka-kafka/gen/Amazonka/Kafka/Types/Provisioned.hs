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
-- Module      : Amazonka.Kafka.Types.Provisioned
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.Provisioned where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types.BrokerNodeGroupInfo
import Amazonka.Kafka.Types.BrokerSoftwareInfo
import Amazonka.Kafka.Types.ClientAuthentication
import Amazonka.Kafka.Types.EncryptionInfo
import Amazonka.Kafka.Types.EnhancedMonitoring
import Amazonka.Kafka.Types.LoggingInfo
import Amazonka.Kafka.Types.OpenMonitoringInfo
import Amazonka.Kafka.Types.StorageMode
import qualified Amazonka.Prelude as Prelude

-- | Provisioned cluster.
--
-- /See:/ 'newProvisioned' smart constructor.
data Provisioned = Provisioned'
  { -- | The settings for open monitoring.
    openMonitoring :: Prelude.Maybe OpenMonitoringInfo,
    -- | This controls storage mode for supported storage tiers.
    storageMode :: Prelude.Maybe StorageMode,
    -- | Includes all encryption-related information.
    encryptionInfo :: Prelude.Maybe EncryptionInfo,
    -- | Includes all client authentication information.
    clientAuthentication :: Prelude.Maybe ClientAuthentication,
    -- | The connection string to use to connect to the Apache ZooKeeper cluster.
    zookeeperConnectString :: Prelude.Maybe Prelude.Text,
    -- | Log delivery information for the cluster.
    loggingInfo :: Prelude.Maybe LoggingInfo,
    -- | Information about the Apache Kafka version deployed on the brokers.
    currentBrokerSoftwareInfo :: Prelude.Maybe BrokerSoftwareInfo,
    -- | The connection string to use to connect to the Apache ZooKeeper cluster
    -- on a TLS port.
    zookeeperConnectStringTls :: Prelude.Maybe Prelude.Text,
    -- | Specifies the level of monitoring for the MSK cluster. The possible
    -- values are DEFAULT, PER_BROKER, PER_TOPIC_PER_BROKER, and
    -- PER_TOPIC_PER_PARTITION.
    enhancedMonitoring :: Prelude.Maybe EnhancedMonitoring,
    -- | Information about the brokers.
    brokerNodeGroupInfo :: BrokerNodeGroupInfo,
    -- | The number of broker nodes in the cluster.
    numberOfBrokerNodes :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Provisioned' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'openMonitoring', 'provisioned_openMonitoring' - The settings for open monitoring.
--
-- 'storageMode', 'provisioned_storageMode' - This controls storage mode for supported storage tiers.
--
-- 'encryptionInfo', 'provisioned_encryptionInfo' - Includes all encryption-related information.
--
-- 'clientAuthentication', 'provisioned_clientAuthentication' - Includes all client authentication information.
--
-- 'zookeeperConnectString', 'provisioned_zookeeperConnectString' - The connection string to use to connect to the Apache ZooKeeper cluster.
--
-- 'loggingInfo', 'provisioned_loggingInfo' - Log delivery information for the cluster.
--
-- 'currentBrokerSoftwareInfo', 'provisioned_currentBrokerSoftwareInfo' - Information about the Apache Kafka version deployed on the brokers.
--
-- 'zookeeperConnectStringTls', 'provisioned_zookeeperConnectStringTls' - The connection string to use to connect to the Apache ZooKeeper cluster
-- on a TLS port.
--
-- 'enhancedMonitoring', 'provisioned_enhancedMonitoring' - Specifies the level of monitoring for the MSK cluster. The possible
-- values are DEFAULT, PER_BROKER, PER_TOPIC_PER_BROKER, and
-- PER_TOPIC_PER_PARTITION.
--
-- 'brokerNodeGroupInfo', 'provisioned_brokerNodeGroupInfo' - Information about the brokers.
--
-- 'numberOfBrokerNodes', 'provisioned_numberOfBrokerNodes' - The number of broker nodes in the cluster.
newProvisioned ::
  -- | 'brokerNodeGroupInfo'
  BrokerNodeGroupInfo ->
  -- | 'numberOfBrokerNodes'
  Prelude.Natural ->
  Provisioned
newProvisioned
  pBrokerNodeGroupInfo_
  pNumberOfBrokerNodes_ =
    Provisioned'
      { openMonitoring = Prelude.Nothing,
        storageMode = Prelude.Nothing,
        encryptionInfo = Prelude.Nothing,
        clientAuthentication = Prelude.Nothing,
        zookeeperConnectString = Prelude.Nothing,
        loggingInfo = Prelude.Nothing,
        currentBrokerSoftwareInfo = Prelude.Nothing,
        zookeeperConnectStringTls = Prelude.Nothing,
        enhancedMonitoring = Prelude.Nothing,
        brokerNodeGroupInfo = pBrokerNodeGroupInfo_,
        numberOfBrokerNodes = pNumberOfBrokerNodes_
      }

-- | The settings for open monitoring.
provisioned_openMonitoring :: Lens.Lens' Provisioned (Prelude.Maybe OpenMonitoringInfo)
provisioned_openMonitoring = Lens.lens (\Provisioned' {openMonitoring} -> openMonitoring) (\s@Provisioned' {} a -> s {openMonitoring = a} :: Provisioned)

-- | This controls storage mode for supported storage tiers.
provisioned_storageMode :: Lens.Lens' Provisioned (Prelude.Maybe StorageMode)
provisioned_storageMode = Lens.lens (\Provisioned' {storageMode} -> storageMode) (\s@Provisioned' {} a -> s {storageMode = a} :: Provisioned)

-- | Includes all encryption-related information.
provisioned_encryptionInfo :: Lens.Lens' Provisioned (Prelude.Maybe EncryptionInfo)
provisioned_encryptionInfo = Lens.lens (\Provisioned' {encryptionInfo} -> encryptionInfo) (\s@Provisioned' {} a -> s {encryptionInfo = a} :: Provisioned)

-- | Includes all client authentication information.
provisioned_clientAuthentication :: Lens.Lens' Provisioned (Prelude.Maybe ClientAuthentication)
provisioned_clientAuthentication = Lens.lens (\Provisioned' {clientAuthentication} -> clientAuthentication) (\s@Provisioned' {} a -> s {clientAuthentication = a} :: Provisioned)

-- | The connection string to use to connect to the Apache ZooKeeper cluster.
provisioned_zookeeperConnectString :: Lens.Lens' Provisioned (Prelude.Maybe Prelude.Text)
provisioned_zookeeperConnectString = Lens.lens (\Provisioned' {zookeeperConnectString} -> zookeeperConnectString) (\s@Provisioned' {} a -> s {zookeeperConnectString = a} :: Provisioned)

-- | Log delivery information for the cluster.
provisioned_loggingInfo :: Lens.Lens' Provisioned (Prelude.Maybe LoggingInfo)
provisioned_loggingInfo = Lens.lens (\Provisioned' {loggingInfo} -> loggingInfo) (\s@Provisioned' {} a -> s {loggingInfo = a} :: Provisioned)

-- | Information about the Apache Kafka version deployed on the brokers.
provisioned_currentBrokerSoftwareInfo :: Lens.Lens' Provisioned (Prelude.Maybe BrokerSoftwareInfo)
provisioned_currentBrokerSoftwareInfo = Lens.lens (\Provisioned' {currentBrokerSoftwareInfo} -> currentBrokerSoftwareInfo) (\s@Provisioned' {} a -> s {currentBrokerSoftwareInfo = a} :: Provisioned)

-- | The connection string to use to connect to the Apache ZooKeeper cluster
-- on a TLS port.
provisioned_zookeeperConnectStringTls :: Lens.Lens' Provisioned (Prelude.Maybe Prelude.Text)
provisioned_zookeeperConnectStringTls = Lens.lens (\Provisioned' {zookeeperConnectStringTls} -> zookeeperConnectStringTls) (\s@Provisioned' {} a -> s {zookeeperConnectStringTls = a} :: Provisioned)

-- | Specifies the level of monitoring for the MSK cluster. The possible
-- values are DEFAULT, PER_BROKER, PER_TOPIC_PER_BROKER, and
-- PER_TOPIC_PER_PARTITION.
provisioned_enhancedMonitoring :: Lens.Lens' Provisioned (Prelude.Maybe EnhancedMonitoring)
provisioned_enhancedMonitoring = Lens.lens (\Provisioned' {enhancedMonitoring} -> enhancedMonitoring) (\s@Provisioned' {} a -> s {enhancedMonitoring = a} :: Provisioned)

-- | Information about the brokers.
provisioned_brokerNodeGroupInfo :: Lens.Lens' Provisioned BrokerNodeGroupInfo
provisioned_brokerNodeGroupInfo = Lens.lens (\Provisioned' {brokerNodeGroupInfo} -> brokerNodeGroupInfo) (\s@Provisioned' {} a -> s {brokerNodeGroupInfo = a} :: Provisioned)

-- | The number of broker nodes in the cluster.
provisioned_numberOfBrokerNodes :: Lens.Lens' Provisioned Prelude.Natural
provisioned_numberOfBrokerNodes = Lens.lens (\Provisioned' {numberOfBrokerNodes} -> numberOfBrokerNodes) (\s@Provisioned' {} a -> s {numberOfBrokerNodes = a} :: Provisioned)

instance Data.FromJSON Provisioned where
  parseJSON =
    Data.withObject
      "Provisioned"
      ( \x ->
          Provisioned'
            Prelude.<$> (x Data..:? "openMonitoring")
            Prelude.<*> (x Data..:? "storageMode")
            Prelude.<*> (x Data..:? "encryptionInfo")
            Prelude.<*> (x Data..:? "clientAuthentication")
            Prelude.<*> (x Data..:? "zookeeperConnectString")
            Prelude.<*> (x Data..:? "loggingInfo")
            Prelude.<*> (x Data..:? "currentBrokerSoftwareInfo")
            Prelude.<*> (x Data..:? "zookeeperConnectStringTls")
            Prelude.<*> (x Data..:? "enhancedMonitoring")
            Prelude.<*> (x Data..: "brokerNodeGroupInfo")
            Prelude.<*> (x Data..: "numberOfBrokerNodes")
      )

instance Prelude.Hashable Provisioned where
  hashWithSalt _salt Provisioned' {..} =
    _salt `Prelude.hashWithSalt` openMonitoring
      `Prelude.hashWithSalt` storageMode
      `Prelude.hashWithSalt` encryptionInfo
      `Prelude.hashWithSalt` clientAuthentication
      `Prelude.hashWithSalt` zookeeperConnectString
      `Prelude.hashWithSalt` loggingInfo
      `Prelude.hashWithSalt` currentBrokerSoftwareInfo
      `Prelude.hashWithSalt` zookeeperConnectStringTls
      `Prelude.hashWithSalt` enhancedMonitoring
      `Prelude.hashWithSalt` brokerNodeGroupInfo
      `Prelude.hashWithSalt` numberOfBrokerNodes

instance Prelude.NFData Provisioned where
  rnf Provisioned' {..} =
    Prelude.rnf openMonitoring
      `Prelude.seq` Prelude.rnf storageMode
      `Prelude.seq` Prelude.rnf encryptionInfo
      `Prelude.seq` Prelude.rnf clientAuthentication
      `Prelude.seq` Prelude.rnf zookeeperConnectString
      `Prelude.seq` Prelude.rnf loggingInfo
      `Prelude.seq` Prelude.rnf currentBrokerSoftwareInfo
      `Prelude.seq` Prelude.rnf zookeeperConnectStringTls
      `Prelude.seq` Prelude.rnf enhancedMonitoring
      `Prelude.seq` Prelude.rnf brokerNodeGroupInfo
      `Prelude.seq` Prelude.rnf numberOfBrokerNodes
