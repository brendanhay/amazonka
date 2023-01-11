{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.KafkaConnect.DescribeConnector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summary information about the connector.
module Amazonka.KafkaConnect.DescribeConnector
  ( -- * Creating a Request
    DescribeConnector (..),
    newDescribeConnector,

    -- * Request Lenses
    describeConnector_connectorArn,

    -- * Destructuring the Response
    DescribeConnectorResponse (..),
    newDescribeConnectorResponse,

    -- * Response Lenses
    describeConnectorResponse_capacity,
    describeConnectorResponse_connectorArn,
    describeConnectorResponse_connectorConfiguration,
    describeConnectorResponse_connectorDescription,
    describeConnectorResponse_connectorName,
    describeConnectorResponse_connectorState,
    describeConnectorResponse_creationTime,
    describeConnectorResponse_currentVersion,
    describeConnectorResponse_kafkaCluster,
    describeConnectorResponse_kafkaClusterClientAuthentication,
    describeConnectorResponse_kafkaClusterEncryptionInTransit,
    describeConnectorResponse_kafkaConnectVersion,
    describeConnectorResponse_logDelivery,
    describeConnectorResponse_plugins,
    describeConnectorResponse_serviceExecutionRoleArn,
    describeConnectorResponse_stateDescription,
    describeConnectorResponse_workerConfiguration,
    describeConnectorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KafkaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeConnector' smart constructor.
data DescribeConnector = DescribeConnector'
  { -- | The Amazon Resource Name (ARN) of the connector that you want to
    -- describe.
    connectorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConnector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectorArn', 'describeConnector_connectorArn' - The Amazon Resource Name (ARN) of the connector that you want to
-- describe.
newDescribeConnector ::
  -- | 'connectorArn'
  Prelude.Text ->
  DescribeConnector
newDescribeConnector pConnectorArn_ =
  DescribeConnector' {connectorArn = pConnectorArn_}

-- | The Amazon Resource Name (ARN) of the connector that you want to
-- describe.
describeConnector_connectorArn :: Lens.Lens' DescribeConnector Prelude.Text
describeConnector_connectorArn = Lens.lens (\DescribeConnector' {connectorArn} -> connectorArn) (\s@DescribeConnector' {} a -> s {connectorArn = a} :: DescribeConnector)

instance Core.AWSRequest DescribeConnector where
  type
    AWSResponse DescribeConnector =
      DescribeConnectorResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConnectorResponse'
            Prelude.<$> (x Data..?> "capacity")
            Prelude.<*> (x Data..?> "connectorArn")
            Prelude.<*> ( x Data..?> "connectorConfiguration"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "connectorDescription")
            Prelude.<*> (x Data..?> "connectorName")
            Prelude.<*> (x Data..?> "connectorState")
            Prelude.<*> (x Data..?> "creationTime")
            Prelude.<*> (x Data..?> "currentVersion")
            Prelude.<*> (x Data..?> "kafkaCluster")
            Prelude.<*> (x Data..?> "kafkaClusterClientAuthentication")
            Prelude.<*> (x Data..?> "kafkaClusterEncryptionInTransit")
            Prelude.<*> (x Data..?> "kafkaConnectVersion")
            Prelude.<*> (x Data..?> "logDelivery")
            Prelude.<*> (x Data..?> "plugins" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "serviceExecutionRoleArn")
            Prelude.<*> (x Data..?> "stateDescription")
            Prelude.<*> (x Data..?> "workerConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeConnector where
  hashWithSalt _salt DescribeConnector' {..} =
    _salt `Prelude.hashWithSalt` connectorArn

instance Prelude.NFData DescribeConnector where
  rnf DescribeConnector' {..} = Prelude.rnf connectorArn

instance Data.ToHeaders DescribeConnector where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeConnector where
  toPath DescribeConnector' {..} =
    Prelude.mconcat
      ["/v1/connectors/", Data.toBS connectorArn]

instance Data.ToQuery DescribeConnector where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeConnectorResponse' smart constructor.
data DescribeConnectorResponse = DescribeConnectorResponse'
  { -- | Information about the capacity of the connector, whether it is auto
    -- scaled or provisioned.
    capacity :: Prelude.Maybe CapacityDescription,
    -- | The Amazon Resource Name (ARN) of the connector.
    connectorArn :: Prelude.Maybe Prelude.Text,
    -- | A map of keys to values that represent the configuration for the
    -- connector.
    connectorConfiguration :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | A summary description of the connector.
    connectorDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the connector.
    connectorName :: Prelude.Maybe Prelude.Text,
    -- | The state of the connector.
    connectorState :: Prelude.Maybe ConnectorState,
    -- | The time the connector was created.
    creationTime :: Prelude.Maybe Data.ISO8601,
    -- | The current version of the connector.
    currentVersion :: Prelude.Maybe Prelude.Text,
    -- | The Apache Kafka cluster that the connector is connected to.
    kafkaCluster :: Prelude.Maybe KafkaClusterDescription,
    -- | The type of client authentication used to connect to the Apache Kafka
    -- cluster. The value is NONE when no client authentication is used.
    kafkaClusterClientAuthentication :: Prelude.Maybe KafkaClusterClientAuthenticationDescription,
    -- | Details of encryption in transit to the Apache Kafka cluster.
    kafkaClusterEncryptionInTransit :: Prelude.Maybe KafkaClusterEncryptionInTransitDescription,
    -- | The version of Kafka Connect. It has to be compatible with both the
    -- Apache Kafka cluster\'s version and the plugins.
    kafkaConnectVersion :: Prelude.Maybe Prelude.Text,
    -- | Details about delivering logs to Amazon CloudWatch Logs.
    logDelivery :: Prelude.Maybe LogDeliveryDescription,
    -- | Specifies which plugins were used for this connector.
    plugins :: Prelude.Maybe [PluginDescription],
    -- | The Amazon Resource Name (ARN) of the IAM role used by the connector to
    -- access Amazon Web Services resources.
    serviceExecutionRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Details about the state of a connector.
    stateDescription :: Prelude.Maybe StateDescription,
    -- | Specifies which worker configuration was used for the connector.
    workerConfiguration :: Prelude.Maybe WorkerConfigurationDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConnectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacity', 'describeConnectorResponse_capacity' - Information about the capacity of the connector, whether it is auto
-- scaled or provisioned.
--
-- 'connectorArn', 'describeConnectorResponse_connectorArn' - The Amazon Resource Name (ARN) of the connector.
--
-- 'connectorConfiguration', 'describeConnectorResponse_connectorConfiguration' - A map of keys to values that represent the configuration for the
-- connector.
--
-- 'connectorDescription', 'describeConnectorResponse_connectorDescription' - A summary description of the connector.
--
-- 'connectorName', 'describeConnectorResponse_connectorName' - The name of the connector.
--
-- 'connectorState', 'describeConnectorResponse_connectorState' - The state of the connector.
--
-- 'creationTime', 'describeConnectorResponse_creationTime' - The time the connector was created.
--
-- 'currentVersion', 'describeConnectorResponse_currentVersion' - The current version of the connector.
--
-- 'kafkaCluster', 'describeConnectorResponse_kafkaCluster' - The Apache Kafka cluster that the connector is connected to.
--
-- 'kafkaClusterClientAuthentication', 'describeConnectorResponse_kafkaClusterClientAuthentication' - The type of client authentication used to connect to the Apache Kafka
-- cluster. The value is NONE when no client authentication is used.
--
-- 'kafkaClusterEncryptionInTransit', 'describeConnectorResponse_kafkaClusterEncryptionInTransit' - Details of encryption in transit to the Apache Kafka cluster.
--
-- 'kafkaConnectVersion', 'describeConnectorResponse_kafkaConnectVersion' - The version of Kafka Connect. It has to be compatible with both the
-- Apache Kafka cluster\'s version and the plugins.
--
-- 'logDelivery', 'describeConnectorResponse_logDelivery' - Details about delivering logs to Amazon CloudWatch Logs.
--
-- 'plugins', 'describeConnectorResponse_plugins' - Specifies which plugins were used for this connector.
--
-- 'serviceExecutionRoleArn', 'describeConnectorResponse_serviceExecutionRoleArn' - The Amazon Resource Name (ARN) of the IAM role used by the connector to
-- access Amazon Web Services resources.
--
-- 'stateDescription', 'describeConnectorResponse_stateDescription' - Details about the state of a connector.
--
-- 'workerConfiguration', 'describeConnectorResponse_workerConfiguration' - Specifies which worker configuration was used for the connector.
--
-- 'httpStatus', 'describeConnectorResponse_httpStatus' - The response's http status code.
newDescribeConnectorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeConnectorResponse
newDescribeConnectorResponse pHttpStatus_ =
  DescribeConnectorResponse'
    { capacity =
        Prelude.Nothing,
      connectorArn = Prelude.Nothing,
      connectorConfiguration = Prelude.Nothing,
      connectorDescription = Prelude.Nothing,
      connectorName = Prelude.Nothing,
      connectorState = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      currentVersion = Prelude.Nothing,
      kafkaCluster = Prelude.Nothing,
      kafkaClusterClientAuthentication =
        Prelude.Nothing,
      kafkaClusterEncryptionInTransit =
        Prelude.Nothing,
      kafkaConnectVersion = Prelude.Nothing,
      logDelivery = Prelude.Nothing,
      plugins = Prelude.Nothing,
      serviceExecutionRoleArn = Prelude.Nothing,
      stateDescription = Prelude.Nothing,
      workerConfiguration = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the capacity of the connector, whether it is auto
-- scaled or provisioned.
describeConnectorResponse_capacity :: Lens.Lens' DescribeConnectorResponse (Prelude.Maybe CapacityDescription)
describeConnectorResponse_capacity = Lens.lens (\DescribeConnectorResponse' {capacity} -> capacity) (\s@DescribeConnectorResponse' {} a -> s {capacity = a} :: DescribeConnectorResponse)

-- | The Amazon Resource Name (ARN) of the connector.
describeConnectorResponse_connectorArn :: Lens.Lens' DescribeConnectorResponse (Prelude.Maybe Prelude.Text)
describeConnectorResponse_connectorArn = Lens.lens (\DescribeConnectorResponse' {connectorArn} -> connectorArn) (\s@DescribeConnectorResponse' {} a -> s {connectorArn = a} :: DescribeConnectorResponse)

-- | A map of keys to values that represent the configuration for the
-- connector.
describeConnectorResponse_connectorConfiguration :: Lens.Lens' DescribeConnectorResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeConnectorResponse_connectorConfiguration = Lens.lens (\DescribeConnectorResponse' {connectorConfiguration} -> connectorConfiguration) (\s@DescribeConnectorResponse' {} a -> s {connectorConfiguration = a} :: DescribeConnectorResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | A summary description of the connector.
describeConnectorResponse_connectorDescription :: Lens.Lens' DescribeConnectorResponse (Prelude.Maybe Prelude.Text)
describeConnectorResponse_connectorDescription = Lens.lens (\DescribeConnectorResponse' {connectorDescription} -> connectorDescription) (\s@DescribeConnectorResponse' {} a -> s {connectorDescription = a} :: DescribeConnectorResponse)

-- | The name of the connector.
describeConnectorResponse_connectorName :: Lens.Lens' DescribeConnectorResponse (Prelude.Maybe Prelude.Text)
describeConnectorResponse_connectorName = Lens.lens (\DescribeConnectorResponse' {connectorName} -> connectorName) (\s@DescribeConnectorResponse' {} a -> s {connectorName = a} :: DescribeConnectorResponse)

-- | The state of the connector.
describeConnectorResponse_connectorState :: Lens.Lens' DescribeConnectorResponse (Prelude.Maybe ConnectorState)
describeConnectorResponse_connectorState = Lens.lens (\DescribeConnectorResponse' {connectorState} -> connectorState) (\s@DescribeConnectorResponse' {} a -> s {connectorState = a} :: DescribeConnectorResponse)

-- | The time the connector was created.
describeConnectorResponse_creationTime :: Lens.Lens' DescribeConnectorResponse (Prelude.Maybe Prelude.UTCTime)
describeConnectorResponse_creationTime = Lens.lens (\DescribeConnectorResponse' {creationTime} -> creationTime) (\s@DescribeConnectorResponse' {} a -> s {creationTime = a} :: DescribeConnectorResponse) Prelude.. Lens.mapping Data._Time

-- | The current version of the connector.
describeConnectorResponse_currentVersion :: Lens.Lens' DescribeConnectorResponse (Prelude.Maybe Prelude.Text)
describeConnectorResponse_currentVersion = Lens.lens (\DescribeConnectorResponse' {currentVersion} -> currentVersion) (\s@DescribeConnectorResponse' {} a -> s {currentVersion = a} :: DescribeConnectorResponse)

-- | The Apache Kafka cluster that the connector is connected to.
describeConnectorResponse_kafkaCluster :: Lens.Lens' DescribeConnectorResponse (Prelude.Maybe KafkaClusterDescription)
describeConnectorResponse_kafkaCluster = Lens.lens (\DescribeConnectorResponse' {kafkaCluster} -> kafkaCluster) (\s@DescribeConnectorResponse' {} a -> s {kafkaCluster = a} :: DescribeConnectorResponse)

-- | The type of client authentication used to connect to the Apache Kafka
-- cluster. The value is NONE when no client authentication is used.
describeConnectorResponse_kafkaClusterClientAuthentication :: Lens.Lens' DescribeConnectorResponse (Prelude.Maybe KafkaClusterClientAuthenticationDescription)
describeConnectorResponse_kafkaClusterClientAuthentication = Lens.lens (\DescribeConnectorResponse' {kafkaClusterClientAuthentication} -> kafkaClusterClientAuthentication) (\s@DescribeConnectorResponse' {} a -> s {kafkaClusterClientAuthentication = a} :: DescribeConnectorResponse)

-- | Details of encryption in transit to the Apache Kafka cluster.
describeConnectorResponse_kafkaClusterEncryptionInTransit :: Lens.Lens' DescribeConnectorResponse (Prelude.Maybe KafkaClusterEncryptionInTransitDescription)
describeConnectorResponse_kafkaClusterEncryptionInTransit = Lens.lens (\DescribeConnectorResponse' {kafkaClusterEncryptionInTransit} -> kafkaClusterEncryptionInTransit) (\s@DescribeConnectorResponse' {} a -> s {kafkaClusterEncryptionInTransit = a} :: DescribeConnectorResponse)

-- | The version of Kafka Connect. It has to be compatible with both the
-- Apache Kafka cluster\'s version and the plugins.
describeConnectorResponse_kafkaConnectVersion :: Lens.Lens' DescribeConnectorResponse (Prelude.Maybe Prelude.Text)
describeConnectorResponse_kafkaConnectVersion = Lens.lens (\DescribeConnectorResponse' {kafkaConnectVersion} -> kafkaConnectVersion) (\s@DescribeConnectorResponse' {} a -> s {kafkaConnectVersion = a} :: DescribeConnectorResponse)

-- | Details about delivering logs to Amazon CloudWatch Logs.
describeConnectorResponse_logDelivery :: Lens.Lens' DescribeConnectorResponse (Prelude.Maybe LogDeliveryDescription)
describeConnectorResponse_logDelivery = Lens.lens (\DescribeConnectorResponse' {logDelivery} -> logDelivery) (\s@DescribeConnectorResponse' {} a -> s {logDelivery = a} :: DescribeConnectorResponse)

-- | Specifies which plugins were used for this connector.
describeConnectorResponse_plugins :: Lens.Lens' DescribeConnectorResponse (Prelude.Maybe [PluginDescription])
describeConnectorResponse_plugins = Lens.lens (\DescribeConnectorResponse' {plugins} -> plugins) (\s@DescribeConnectorResponse' {} a -> s {plugins = a} :: DescribeConnectorResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the IAM role used by the connector to
-- access Amazon Web Services resources.
describeConnectorResponse_serviceExecutionRoleArn :: Lens.Lens' DescribeConnectorResponse (Prelude.Maybe Prelude.Text)
describeConnectorResponse_serviceExecutionRoleArn = Lens.lens (\DescribeConnectorResponse' {serviceExecutionRoleArn} -> serviceExecutionRoleArn) (\s@DescribeConnectorResponse' {} a -> s {serviceExecutionRoleArn = a} :: DescribeConnectorResponse)

-- | Details about the state of a connector.
describeConnectorResponse_stateDescription :: Lens.Lens' DescribeConnectorResponse (Prelude.Maybe StateDescription)
describeConnectorResponse_stateDescription = Lens.lens (\DescribeConnectorResponse' {stateDescription} -> stateDescription) (\s@DescribeConnectorResponse' {} a -> s {stateDescription = a} :: DescribeConnectorResponse)

-- | Specifies which worker configuration was used for the connector.
describeConnectorResponse_workerConfiguration :: Lens.Lens' DescribeConnectorResponse (Prelude.Maybe WorkerConfigurationDescription)
describeConnectorResponse_workerConfiguration = Lens.lens (\DescribeConnectorResponse' {workerConfiguration} -> workerConfiguration) (\s@DescribeConnectorResponse' {} a -> s {workerConfiguration = a} :: DescribeConnectorResponse)

-- | The response's http status code.
describeConnectorResponse_httpStatus :: Lens.Lens' DescribeConnectorResponse Prelude.Int
describeConnectorResponse_httpStatus = Lens.lens (\DescribeConnectorResponse' {httpStatus} -> httpStatus) (\s@DescribeConnectorResponse' {} a -> s {httpStatus = a} :: DescribeConnectorResponse)

instance Prelude.NFData DescribeConnectorResponse where
  rnf DescribeConnectorResponse' {..} =
    Prelude.rnf capacity
      `Prelude.seq` Prelude.rnf connectorArn
      `Prelude.seq` Prelude.rnf connectorConfiguration
      `Prelude.seq` Prelude.rnf connectorDescription
      `Prelude.seq` Prelude.rnf connectorName
      `Prelude.seq` Prelude.rnf connectorState
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf currentVersion
      `Prelude.seq` Prelude.rnf kafkaCluster
      `Prelude.seq` Prelude.rnf kafkaClusterClientAuthentication
      `Prelude.seq` Prelude.rnf kafkaClusterEncryptionInTransit
      `Prelude.seq` Prelude.rnf kafkaConnectVersion
      `Prelude.seq` Prelude.rnf logDelivery
      `Prelude.seq` Prelude.rnf plugins
      `Prelude.seq` Prelude.rnf serviceExecutionRoleArn
      `Prelude.seq` Prelude.rnf stateDescription
      `Prelude.seq` Prelude.rnf workerConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
