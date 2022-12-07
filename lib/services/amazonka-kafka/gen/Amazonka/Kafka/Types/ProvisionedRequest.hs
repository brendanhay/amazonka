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
-- Module      : Amazonka.Kafka.Types.ProvisionedRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.ProvisionedRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types.BrokerNodeGroupInfo
import Amazonka.Kafka.Types.ClientAuthentication
import Amazonka.Kafka.Types.ConfigurationInfo
import Amazonka.Kafka.Types.EncryptionInfo
import Amazonka.Kafka.Types.EnhancedMonitoring
import Amazonka.Kafka.Types.LoggingInfo
import Amazonka.Kafka.Types.OpenMonitoringInfo
import Amazonka.Kafka.Types.StorageMode
import qualified Amazonka.Prelude as Prelude

-- | Provisioned cluster request.
--
-- /See:/ 'newProvisionedRequest' smart constructor.
data ProvisionedRequest = ProvisionedRequest'
  { -- | The settings for open monitoring.
    openMonitoring :: Prelude.Maybe OpenMonitoringInfo,
    -- | This controls storage mode for supported storage tiers.
    storageMode :: Prelude.Maybe StorageMode,
    -- | Includes all encryption-related information.
    encryptionInfo :: Prelude.Maybe EncryptionInfo,
    -- | Includes all client authentication information.
    clientAuthentication :: Prelude.Maybe ClientAuthentication,
    -- | Log delivery information for the cluster.
    loggingInfo :: Prelude.Maybe LoggingInfo,
    -- | Represents the configuration that you want Amazon MSK to use for the
    -- brokers in a cluster.
    configurationInfo :: Prelude.Maybe ConfigurationInfo,
    -- | Specifies the level of monitoring for the MSK cluster. The possible
    -- values are DEFAULT, PER_BROKER, PER_TOPIC_PER_BROKER, and
    -- PER_TOPIC_PER_PARTITION.
    enhancedMonitoring :: Prelude.Maybe EnhancedMonitoring,
    -- | Information about the brokers.
    brokerNodeGroupInfo :: BrokerNodeGroupInfo,
    -- | The Apache Kafka version that you want for the cluster.
    kafkaVersion :: Prelude.Text,
    -- | The number of broker nodes in the cluster.
    numberOfBrokerNodes :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProvisionedRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'openMonitoring', 'provisionedRequest_openMonitoring' - The settings for open monitoring.
--
-- 'storageMode', 'provisionedRequest_storageMode' - This controls storage mode for supported storage tiers.
--
-- 'encryptionInfo', 'provisionedRequest_encryptionInfo' - Includes all encryption-related information.
--
-- 'clientAuthentication', 'provisionedRequest_clientAuthentication' - Includes all client authentication information.
--
-- 'loggingInfo', 'provisionedRequest_loggingInfo' - Log delivery information for the cluster.
--
-- 'configurationInfo', 'provisionedRequest_configurationInfo' - Represents the configuration that you want Amazon MSK to use for the
-- brokers in a cluster.
--
-- 'enhancedMonitoring', 'provisionedRequest_enhancedMonitoring' - Specifies the level of monitoring for the MSK cluster. The possible
-- values are DEFAULT, PER_BROKER, PER_TOPIC_PER_BROKER, and
-- PER_TOPIC_PER_PARTITION.
--
-- 'brokerNodeGroupInfo', 'provisionedRequest_brokerNodeGroupInfo' - Information about the brokers.
--
-- 'kafkaVersion', 'provisionedRequest_kafkaVersion' - The Apache Kafka version that you want for the cluster.
--
-- 'numberOfBrokerNodes', 'provisionedRequest_numberOfBrokerNodes' - The number of broker nodes in the cluster.
newProvisionedRequest ::
  -- | 'brokerNodeGroupInfo'
  BrokerNodeGroupInfo ->
  -- | 'kafkaVersion'
  Prelude.Text ->
  -- | 'numberOfBrokerNodes'
  Prelude.Natural ->
  ProvisionedRequest
newProvisionedRequest
  pBrokerNodeGroupInfo_
  pKafkaVersion_
  pNumberOfBrokerNodes_ =
    ProvisionedRequest'
      { openMonitoring =
          Prelude.Nothing,
        storageMode = Prelude.Nothing,
        encryptionInfo = Prelude.Nothing,
        clientAuthentication = Prelude.Nothing,
        loggingInfo = Prelude.Nothing,
        configurationInfo = Prelude.Nothing,
        enhancedMonitoring = Prelude.Nothing,
        brokerNodeGroupInfo = pBrokerNodeGroupInfo_,
        kafkaVersion = pKafkaVersion_,
        numberOfBrokerNodes = pNumberOfBrokerNodes_
      }

-- | The settings for open monitoring.
provisionedRequest_openMonitoring :: Lens.Lens' ProvisionedRequest (Prelude.Maybe OpenMonitoringInfo)
provisionedRequest_openMonitoring = Lens.lens (\ProvisionedRequest' {openMonitoring} -> openMonitoring) (\s@ProvisionedRequest' {} a -> s {openMonitoring = a} :: ProvisionedRequest)

-- | This controls storage mode for supported storage tiers.
provisionedRequest_storageMode :: Lens.Lens' ProvisionedRequest (Prelude.Maybe StorageMode)
provisionedRequest_storageMode = Lens.lens (\ProvisionedRequest' {storageMode} -> storageMode) (\s@ProvisionedRequest' {} a -> s {storageMode = a} :: ProvisionedRequest)

-- | Includes all encryption-related information.
provisionedRequest_encryptionInfo :: Lens.Lens' ProvisionedRequest (Prelude.Maybe EncryptionInfo)
provisionedRequest_encryptionInfo = Lens.lens (\ProvisionedRequest' {encryptionInfo} -> encryptionInfo) (\s@ProvisionedRequest' {} a -> s {encryptionInfo = a} :: ProvisionedRequest)

-- | Includes all client authentication information.
provisionedRequest_clientAuthentication :: Lens.Lens' ProvisionedRequest (Prelude.Maybe ClientAuthentication)
provisionedRequest_clientAuthentication = Lens.lens (\ProvisionedRequest' {clientAuthentication} -> clientAuthentication) (\s@ProvisionedRequest' {} a -> s {clientAuthentication = a} :: ProvisionedRequest)

-- | Log delivery information for the cluster.
provisionedRequest_loggingInfo :: Lens.Lens' ProvisionedRequest (Prelude.Maybe LoggingInfo)
provisionedRequest_loggingInfo = Lens.lens (\ProvisionedRequest' {loggingInfo} -> loggingInfo) (\s@ProvisionedRequest' {} a -> s {loggingInfo = a} :: ProvisionedRequest)

-- | Represents the configuration that you want Amazon MSK to use for the
-- brokers in a cluster.
provisionedRequest_configurationInfo :: Lens.Lens' ProvisionedRequest (Prelude.Maybe ConfigurationInfo)
provisionedRequest_configurationInfo = Lens.lens (\ProvisionedRequest' {configurationInfo} -> configurationInfo) (\s@ProvisionedRequest' {} a -> s {configurationInfo = a} :: ProvisionedRequest)

-- | Specifies the level of monitoring for the MSK cluster. The possible
-- values are DEFAULT, PER_BROKER, PER_TOPIC_PER_BROKER, and
-- PER_TOPIC_PER_PARTITION.
provisionedRequest_enhancedMonitoring :: Lens.Lens' ProvisionedRequest (Prelude.Maybe EnhancedMonitoring)
provisionedRequest_enhancedMonitoring = Lens.lens (\ProvisionedRequest' {enhancedMonitoring} -> enhancedMonitoring) (\s@ProvisionedRequest' {} a -> s {enhancedMonitoring = a} :: ProvisionedRequest)

-- | Information about the brokers.
provisionedRequest_brokerNodeGroupInfo :: Lens.Lens' ProvisionedRequest BrokerNodeGroupInfo
provisionedRequest_brokerNodeGroupInfo = Lens.lens (\ProvisionedRequest' {brokerNodeGroupInfo} -> brokerNodeGroupInfo) (\s@ProvisionedRequest' {} a -> s {brokerNodeGroupInfo = a} :: ProvisionedRequest)

-- | The Apache Kafka version that you want for the cluster.
provisionedRequest_kafkaVersion :: Lens.Lens' ProvisionedRequest Prelude.Text
provisionedRequest_kafkaVersion = Lens.lens (\ProvisionedRequest' {kafkaVersion} -> kafkaVersion) (\s@ProvisionedRequest' {} a -> s {kafkaVersion = a} :: ProvisionedRequest)

-- | The number of broker nodes in the cluster.
provisionedRequest_numberOfBrokerNodes :: Lens.Lens' ProvisionedRequest Prelude.Natural
provisionedRequest_numberOfBrokerNodes = Lens.lens (\ProvisionedRequest' {numberOfBrokerNodes} -> numberOfBrokerNodes) (\s@ProvisionedRequest' {} a -> s {numberOfBrokerNodes = a} :: ProvisionedRequest)

instance Prelude.Hashable ProvisionedRequest where
  hashWithSalt _salt ProvisionedRequest' {..} =
    _salt `Prelude.hashWithSalt` openMonitoring
      `Prelude.hashWithSalt` storageMode
      `Prelude.hashWithSalt` encryptionInfo
      `Prelude.hashWithSalt` clientAuthentication
      `Prelude.hashWithSalt` loggingInfo
      `Prelude.hashWithSalt` configurationInfo
      `Prelude.hashWithSalt` enhancedMonitoring
      `Prelude.hashWithSalt` brokerNodeGroupInfo
      `Prelude.hashWithSalt` kafkaVersion
      `Prelude.hashWithSalt` numberOfBrokerNodes

instance Prelude.NFData ProvisionedRequest where
  rnf ProvisionedRequest' {..} =
    Prelude.rnf openMonitoring
      `Prelude.seq` Prelude.rnf storageMode
      `Prelude.seq` Prelude.rnf encryptionInfo
      `Prelude.seq` Prelude.rnf clientAuthentication
      `Prelude.seq` Prelude.rnf loggingInfo
      `Prelude.seq` Prelude.rnf configurationInfo
      `Prelude.seq` Prelude.rnf enhancedMonitoring
      `Prelude.seq` Prelude.rnf brokerNodeGroupInfo
      `Prelude.seq` Prelude.rnf kafkaVersion
      `Prelude.seq` Prelude.rnf numberOfBrokerNodes

instance Data.ToJSON ProvisionedRequest where
  toJSON ProvisionedRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("openMonitoring" Data..=)
              Prelude.<$> openMonitoring,
            ("storageMode" Data..=) Prelude.<$> storageMode,
            ("encryptionInfo" Data..=)
              Prelude.<$> encryptionInfo,
            ("clientAuthentication" Data..=)
              Prelude.<$> clientAuthentication,
            ("loggingInfo" Data..=) Prelude.<$> loggingInfo,
            ("configurationInfo" Data..=)
              Prelude.<$> configurationInfo,
            ("enhancedMonitoring" Data..=)
              Prelude.<$> enhancedMonitoring,
            Prelude.Just
              ("brokerNodeGroupInfo" Data..= brokerNodeGroupInfo),
            Prelude.Just ("kafkaVersion" Data..= kafkaVersion),
            Prelude.Just
              ("numberOfBrokerNodes" Data..= numberOfBrokerNodes)
          ]
      )
