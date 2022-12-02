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
-- Module      : Amazonka.KafkaConnect.Types.ConnectorSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.ConnectorSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KafkaConnect.Types.CapacityDescription
import Amazonka.KafkaConnect.Types.ConnectorState
import Amazonka.KafkaConnect.Types.KafkaClusterClientAuthenticationDescription
import Amazonka.KafkaConnect.Types.KafkaClusterDescription
import Amazonka.KafkaConnect.Types.KafkaClusterEncryptionInTransitDescription
import Amazonka.KafkaConnect.Types.LogDeliveryDescription
import Amazonka.KafkaConnect.Types.PluginDescription
import Amazonka.KafkaConnect.Types.WorkerConfigurationDescription
import qualified Amazonka.Prelude as Prelude

-- | Summary of a connector.
--
-- /See:/ 'newConnectorSummary' smart constructor.
data ConnectorSummary = ConnectorSummary'
  { -- | The description of the connector.
    connectorDescription :: Prelude.Maybe Prelude.Text,
    -- | Details of encryption in transit to the Apache Kafka cluster.
    kafkaClusterEncryptionInTransit :: Prelude.Maybe KafkaClusterEncryptionInTransitDescription,
    -- | The version of Kafka Connect. It has to be compatible with both the
    -- Apache Kafka cluster\'s version and the plugins.
    kafkaConnectVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role used by the connector to
    -- access Amazon Web Services resources.
    serviceExecutionRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the connector.
    connectorArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies which plugins were used for this connector.
    plugins :: Prelude.Maybe [PluginDescription],
    -- | The type of client authentication used to connect to the Apache Kafka
    -- cluster. The value is NONE when no client authentication is used.
    kafkaClusterClientAuthentication :: Prelude.Maybe KafkaClusterClientAuthenticationDescription,
    -- | The current version of the connector.
    currentVersion :: Prelude.Maybe Prelude.Text,
    -- | The settings for delivering connector logs to Amazon CloudWatch Logs.
    logDelivery :: Prelude.Maybe LogDeliveryDescription,
    -- | The name of the connector.
    connectorName :: Prelude.Maybe Prelude.Text,
    -- | The time that the connector was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The details of the Apache Kafka cluster to which the connector is
    -- connected.
    kafkaCluster :: Prelude.Maybe KafkaClusterDescription,
    -- | The connector\'s compute capacity settings.
    capacity :: Prelude.Maybe CapacityDescription,
    -- | The state of the connector.
    connectorState :: Prelude.Maybe ConnectorState,
    -- | The worker configurations that are in use with the connector.
    workerConfiguration :: Prelude.Maybe WorkerConfigurationDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectorSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectorDescription', 'connectorSummary_connectorDescription' - The description of the connector.
--
-- 'kafkaClusterEncryptionInTransit', 'connectorSummary_kafkaClusterEncryptionInTransit' - Details of encryption in transit to the Apache Kafka cluster.
--
-- 'kafkaConnectVersion', 'connectorSummary_kafkaConnectVersion' - The version of Kafka Connect. It has to be compatible with both the
-- Apache Kafka cluster\'s version and the plugins.
--
-- 'serviceExecutionRoleArn', 'connectorSummary_serviceExecutionRoleArn' - The Amazon Resource Name (ARN) of the IAM role used by the connector to
-- access Amazon Web Services resources.
--
-- 'connectorArn', 'connectorSummary_connectorArn' - The Amazon Resource Name (ARN) of the connector.
--
-- 'plugins', 'connectorSummary_plugins' - Specifies which plugins were used for this connector.
--
-- 'kafkaClusterClientAuthentication', 'connectorSummary_kafkaClusterClientAuthentication' - The type of client authentication used to connect to the Apache Kafka
-- cluster. The value is NONE when no client authentication is used.
--
-- 'currentVersion', 'connectorSummary_currentVersion' - The current version of the connector.
--
-- 'logDelivery', 'connectorSummary_logDelivery' - The settings for delivering connector logs to Amazon CloudWatch Logs.
--
-- 'connectorName', 'connectorSummary_connectorName' - The name of the connector.
--
-- 'creationTime', 'connectorSummary_creationTime' - The time that the connector was created.
--
-- 'kafkaCluster', 'connectorSummary_kafkaCluster' - The details of the Apache Kafka cluster to which the connector is
-- connected.
--
-- 'capacity', 'connectorSummary_capacity' - The connector\'s compute capacity settings.
--
-- 'connectorState', 'connectorSummary_connectorState' - The state of the connector.
--
-- 'workerConfiguration', 'connectorSummary_workerConfiguration' - The worker configurations that are in use with the connector.
newConnectorSummary ::
  ConnectorSummary
newConnectorSummary =
  ConnectorSummary'
    { connectorDescription =
        Prelude.Nothing,
      kafkaClusterEncryptionInTransit = Prelude.Nothing,
      kafkaConnectVersion = Prelude.Nothing,
      serviceExecutionRoleArn = Prelude.Nothing,
      connectorArn = Prelude.Nothing,
      plugins = Prelude.Nothing,
      kafkaClusterClientAuthentication = Prelude.Nothing,
      currentVersion = Prelude.Nothing,
      logDelivery = Prelude.Nothing,
      connectorName = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      kafkaCluster = Prelude.Nothing,
      capacity = Prelude.Nothing,
      connectorState = Prelude.Nothing,
      workerConfiguration = Prelude.Nothing
    }

-- | The description of the connector.
connectorSummary_connectorDescription :: Lens.Lens' ConnectorSummary (Prelude.Maybe Prelude.Text)
connectorSummary_connectorDescription = Lens.lens (\ConnectorSummary' {connectorDescription} -> connectorDescription) (\s@ConnectorSummary' {} a -> s {connectorDescription = a} :: ConnectorSummary)

-- | Details of encryption in transit to the Apache Kafka cluster.
connectorSummary_kafkaClusterEncryptionInTransit :: Lens.Lens' ConnectorSummary (Prelude.Maybe KafkaClusterEncryptionInTransitDescription)
connectorSummary_kafkaClusterEncryptionInTransit = Lens.lens (\ConnectorSummary' {kafkaClusterEncryptionInTransit} -> kafkaClusterEncryptionInTransit) (\s@ConnectorSummary' {} a -> s {kafkaClusterEncryptionInTransit = a} :: ConnectorSummary)

-- | The version of Kafka Connect. It has to be compatible with both the
-- Apache Kafka cluster\'s version and the plugins.
connectorSummary_kafkaConnectVersion :: Lens.Lens' ConnectorSummary (Prelude.Maybe Prelude.Text)
connectorSummary_kafkaConnectVersion = Lens.lens (\ConnectorSummary' {kafkaConnectVersion} -> kafkaConnectVersion) (\s@ConnectorSummary' {} a -> s {kafkaConnectVersion = a} :: ConnectorSummary)

-- | The Amazon Resource Name (ARN) of the IAM role used by the connector to
-- access Amazon Web Services resources.
connectorSummary_serviceExecutionRoleArn :: Lens.Lens' ConnectorSummary (Prelude.Maybe Prelude.Text)
connectorSummary_serviceExecutionRoleArn = Lens.lens (\ConnectorSummary' {serviceExecutionRoleArn} -> serviceExecutionRoleArn) (\s@ConnectorSummary' {} a -> s {serviceExecutionRoleArn = a} :: ConnectorSummary)

-- | The Amazon Resource Name (ARN) of the connector.
connectorSummary_connectorArn :: Lens.Lens' ConnectorSummary (Prelude.Maybe Prelude.Text)
connectorSummary_connectorArn = Lens.lens (\ConnectorSummary' {connectorArn} -> connectorArn) (\s@ConnectorSummary' {} a -> s {connectorArn = a} :: ConnectorSummary)

-- | Specifies which plugins were used for this connector.
connectorSummary_plugins :: Lens.Lens' ConnectorSummary (Prelude.Maybe [PluginDescription])
connectorSummary_plugins = Lens.lens (\ConnectorSummary' {plugins} -> plugins) (\s@ConnectorSummary' {} a -> s {plugins = a} :: ConnectorSummary) Prelude.. Lens.mapping Lens.coerced

-- | The type of client authentication used to connect to the Apache Kafka
-- cluster. The value is NONE when no client authentication is used.
connectorSummary_kafkaClusterClientAuthentication :: Lens.Lens' ConnectorSummary (Prelude.Maybe KafkaClusterClientAuthenticationDescription)
connectorSummary_kafkaClusterClientAuthentication = Lens.lens (\ConnectorSummary' {kafkaClusterClientAuthentication} -> kafkaClusterClientAuthentication) (\s@ConnectorSummary' {} a -> s {kafkaClusterClientAuthentication = a} :: ConnectorSummary)

-- | The current version of the connector.
connectorSummary_currentVersion :: Lens.Lens' ConnectorSummary (Prelude.Maybe Prelude.Text)
connectorSummary_currentVersion = Lens.lens (\ConnectorSummary' {currentVersion} -> currentVersion) (\s@ConnectorSummary' {} a -> s {currentVersion = a} :: ConnectorSummary)

-- | The settings for delivering connector logs to Amazon CloudWatch Logs.
connectorSummary_logDelivery :: Lens.Lens' ConnectorSummary (Prelude.Maybe LogDeliveryDescription)
connectorSummary_logDelivery = Lens.lens (\ConnectorSummary' {logDelivery} -> logDelivery) (\s@ConnectorSummary' {} a -> s {logDelivery = a} :: ConnectorSummary)

-- | The name of the connector.
connectorSummary_connectorName :: Lens.Lens' ConnectorSummary (Prelude.Maybe Prelude.Text)
connectorSummary_connectorName = Lens.lens (\ConnectorSummary' {connectorName} -> connectorName) (\s@ConnectorSummary' {} a -> s {connectorName = a} :: ConnectorSummary)

-- | The time that the connector was created.
connectorSummary_creationTime :: Lens.Lens' ConnectorSummary (Prelude.Maybe Prelude.UTCTime)
connectorSummary_creationTime = Lens.lens (\ConnectorSummary' {creationTime} -> creationTime) (\s@ConnectorSummary' {} a -> s {creationTime = a} :: ConnectorSummary) Prelude.. Lens.mapping Data._Time

-- | The details of the Apache Kafka cluster to which the connector is
-- connected.
connectorSummary_kafkaCluster :: Lens.Lens' ConnectorSummary (Prelude.Maybe KafkaClusterDescription)
connectorSummary_kafkaCluster = Lens.lens (\ConnectorSummary' {kafkaCluster} -> kafkaCluster) (\s@ConnectorSummary' {} a -> s {kafkaCluster = a} :: ConnectorSummary)

-- | The connector\'s compute capacity settings.
connectorSummary_capacity :: Lens.Lens' ConnectorSummary (Prelude.Maybe CapacityDescription)
connectorSummary_capacity = Lens.lens (\ConnectorSummary' {capacity} -> capacity) (\s@ConnectorSummary' {} a -> s {capacity = a} :: ConnectorSummary)

-- | The state of the connector.
connectorSummary_connectorState :: Lens.Lens' ConnectorSummary (Prelude.Maybe ConnectorState)
connectorSummary_connectorState = Lens.lens (\ConnectorSummary' {connectorState} -> connectorState) (\s@ConnectorSummary' {} a -> s {connectorState = a} :: ConnectorSummary)

-- | The worker configurations that are in use with the connector.
connectorSummary_workerConfiguration :: Lens.Lens' ConnectorSummary (Prelude.Maybe WorkerConfigurationDescription)
connectorSummary_workerConfiguration = Lens.lens (\ConnectorSummary' {workerConfiguration} -> workerConfiguration) (\s@ConnectorSummary' {} a -> s {workerConfiguration = a} :: ConnectorSummary)

instance Data.FromJSON ConnectorSummary where
  parseJSON =
    Data.withObject
      "ConnectorSummary"
      ( \x ->
          ConnectorSummary'
            Prelude.<$> (x Data..:? "connectorDescription")
            Prelude.<*> (x Data..:? "kafkaClusterEncryptionInTransit")
            Prelude.<*> (x Data..:? "kafkaConnectVersion")
            Prelude.<*> (x Data..:? "serviceExecutionRoleArn")
            Prelude.<*> (x Data..:? "connectorArn")
            Prelude.<*> (x Data..:? "plugins" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "kafkaClusterClientAuthentication")
            Prelude.<*> (x Data..:? "currentVersion")
            Prelude.<*> (x Data..:? "logDelivery")
            Prelude.<*> (x Data..:? "connectorName")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "kafkaCluster")
            Prelude.<*> (x Data..:? "capacity")
            Prelude.<*> (x Data..:? "connectorState")
            Prelude.<*> (x Data..:? "workerConfiguration")
      )

instance Prelude.Hashable ConnectorSummary where
  hashWithSalt _salt ConnectorSummary' {..} =
    _salt `Prelude.hashWithSalt` connectorDescription
      `Prelude.hashWithSalt` kafkaClusterEncryptionInTransit
      `Prelude.hashWithSalt` kafkaConnectVersion
      `Prelude.hashWithSalt` serviceExecutionRoleArn
      `Prelude.hashWithSalt` connectorArn
      `Prelude.hashWithSalt` plugins
      `Prelude.hashWithSalt` kafkaClusterClientAuthentication
      `Prelude.hashWithSalt` currentVersion
      `Prelude.hashWithSalt` logDelivery
      `Prelude.hashWithSalt` connectorName
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` kafkaCluster
      `Prelude.hashWithSalt` capacity
      `Prelude.hashWithSalt` connectorState
      `Prelude.hashWithSalt` workerConfiguration

instance Prelude.NFData ConnectorSummary where
  rnf ConnectorSummary' {..} =
    Prelude.rnf connectorDescription
      `Prelude.seq` Prelude.rnf kafkaClusterEncryptionInTransit
      `Prelude.seq` Prelude.rnf kafkaConnectVersion
      `Prelude.seq` Prelude.rnf serviceExecutionRoleArn
      `Prelude.seq` Prelude.rnf connectorArn
      `Prelude.seq` Prelude.rnf plugins
      `Prelude.seq` Prelude.rnf kafkaClusterClientAuthentication
      `Prelude.seq` Prelude.rnf currentVersion
      `Prelude.seq` Prelude.rnf logDelivery
      `Prelude.seq` Prelude.rnf connectorName
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf kafkaCluster
      `Prelude.seq` Prelude.rnf capacity
      `Prelude.seq` Prelude.rnf connectorState
      `Prelude.seq` Prelude.rnf workerConfiguration
