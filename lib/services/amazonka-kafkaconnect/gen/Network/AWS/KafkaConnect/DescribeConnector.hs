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
-- Module      : Network.AWS.KafkaConnect.DescribeConnector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summary information about the connector.
module Network.AWS.KafkaConnect.DescribeConnector
  ( -- * Creating a Request
    DescribeConnector (..),
    newDescribeConnector,

    -- * Request Lenses
    describeConnector_connectorArn,

    -- * Destructuring the Response
    DescribeConnectorResponse (..),
    newDescribeConnectorResponse,

    -- * Response Lenses
    describeConnectorResponse_creationTime,
    describeConnectorResponse_kafkaCluster,
    describeConnectorResponse_kafkaConnectVersion,
    describeConnectorResponse_logDelivery,
    describeConnectorResponse_currentVersion,
    describeConnectorResponse_connectorConfiguration,
    describeConnectorResponse_workerConfiguration,
    describeConnectorResponse_connectorArn,
    describeConnectorResponse_connectorName,
    describeConnectorResponse_connectorState,
    describeConnectorResponse_capacity,
    describeConnectorResponse_plugins,
    describeConnectorResponse_connectorDescription,
    describeConnectorResponse_kafkaClusterClientAuthentication,
    describeConnectorResponse_kafkaClusterEncryptionInTransit,
    describeConnectorResponse_serviceExecutionRoleArn,
    describeConnectorResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.KafkaConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConnectorResponse'
            Prelude.<$> (x Core..?> "creationTime")
            Prelude.<*> (x Core..?> "kafkaCluster")
            Prelude.<*> (x Core..?> "kafkaConnectVersion")
            Prelude.<*> (x Core..?> "logDelivery")
            Prelude.<*> (x Core..?> "currentVersion")
            Prelude.<*> ( x Core..?> "connectorConfiguration"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "workerConfiguration")
            Prelude.<*> (x Core..?> "connectorArn")
            Prelude.<*> (x Core..?> "connectorName")
            Prelude.<*> (x Core..?> "connectorState")
            Prelude.<*> (x Core..?> "capacity")
            Prelude.<*> (x Core..?> "plugins" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "connectorDescription")
            Prelude.<*> (x Core..?> "kafkaClusterClientAuthentication")
            Prelude.<*> (x Core..?> "kafkaClusterEncryptionInTransit")
            Prelude.<*> (x Core..?> "serviceExecutionRoleArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeConnector

instance Prelude.NFData DescribeConnector

instance Core.ToHeaders DescribeConnector where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeConnector where
  toPath DescribeConnector' {..} =
    Prelude.mconcat
      ["/v1/connectors/", Core.toBS connectorArn]

instance Core.ToQuery DescribeConnector where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeConnectorResponse' smart constructor.
data DescribeConnectorResponse = DescribeConnectorResponse'
  { -- | The time the connector was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The Apache Kafka cluster that the connector is connected to.
    kafkaCluster :: Prelude.Maybe KafkaClusterDescription,
    -- | The version of Kafka Connect. It has to be compatible with both the
    -- Apache Kafka cluster\'s version and the plugins.
    kafkaConnectVersion :: Prelude.Maybe Prelude.Text,
    -- | Details about delivering logs to Amazon CloudWatch Logs.
    logDelivery :: Prelude.Maybe LogDeliveryDescription,
    -- | The current version of the connector.
    currentVersion :: Prelude.Maybe Prelude.Text,
    -- | A map of keys to values that represent the configuration for the
    -- connector.
    connectorConfiguration :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies which worker configuration was used for the connector.
    workerConfiguration :: Prelude.Maybe WorkerConfigurationDescription,
    -- | The Amazon Resource Name (ARN) of the connector.
    connectorArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the connector.
    connectorName :: Prelude.Maybe Prelude.Text,
    -- | The state of the connector.
    connectorState :: Prelude.Maybe ConnectorState,
    -- | Information about the capacity of the connector, whether it is auto
    -- scaled or provisioned.
    capacity :: Prelude.Maybe CapacityDescription,
    -- | Specifies which plugins were used for this connector.
    plugins :: Prelude.Maybe [PluginDescription],
    -- | A summary description of the connector.
    connectorDescription :: Prelude.Maybe Prelude.Text,
    -- | The type of client authentication used to connect to the Apache Kafka
    -- cluster. The value is NONE when no client authentication is used.
    kafkaClusterClientAuthentication :: Prelude.Maybe KafkaClusterClientAuthenticationDescription,
    -- | Details of encryption in transit to the Apache Kafka cluster.
    kafkaClusterEncryptionInTransit :: Prelude.Maybe KafkaClusterEncryptionInTransitDescription,
    -- | The Amazon Resource Name (ARN) of the IAM role used by the connector to
    -- access Amazon Web Services resources.
    serviceExecutionRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConnectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'describeConnectorResponse_creationTime' - The time the connector was created.
--
-- 'kafkaCluster', 'describeConnectorResponse_kafkaCluster' - The Apache Kafka cluster that the connector is connected to.
--
-- 'kafkaConnectVersion', 'describeConnectorResponse_kafkaConnectVersion' - The version of Kafka Connect. It has to be compatible with both the
-- Apache Kafka cluster\'s version and the plugins.
--
-- 'logDelivery', 'describeConnectorResponse_logDelivery' - Details about delivering logs to Amazon CloudWatch Logs.
--
-- 'currentVersion', 'describeConnectorResponse_currentVersion' - The current version of the connector.
--
-- 'connectorConfiguration', 'describeConnectorResponse_connectorConfiguration' - A map of keys to values that represent the configuration for the
-- connector.
--
-- 'workerConfiguration', 'describeConnectorResponse_workerConfiguration' - Specifies which worker configuration was used for the connector.
--
-- 'connectorArn', 'describeConnectorResponse_connectorArn' - The Amazon Resource Name (ARN) of the connector.
--
-- 'connectorName', 'describeConnectorResponse_connectorName' - The name of the connector.
--
-- 'connectorState', 'describeConnectorResponse_connectorState' - The state of the connector.
--
-- 'capacity', 'describeConnectorResponse_capacity' - Information about the capacity of the connector, whether it is auto
-- scaled or provisioned.
--
-- 'plugins', 'describeConnectorResponse_plugins' - Specifies which plugins were used for this connector.
--
-- 'connectorDescription', 'describeConnectorResponse_connectorDescription' - A summary description of the connector.
--
-- 'kafkaClusterClientAuthentication', 'describeConnectorResponse_kafkaClusterClientAuthentication' - The type of client authentication used to connect to the Apache Kafka
-- cluster. The value is NONE when no client authentication is used.
--
-- 'kafkaClusterEncryptionInTransit', 'describeConnectorResponse_kafkaClusterEncryptionInTransit' - Details of encryption in transit to the Apache Kafka cluster.
--
-- 'serviceExecutionRoleArn', 'describeConnectorResponse_serviceExecutionRoleArn' - The Amazon Resource Name (ARN) of the IAM role used by the connector to
-- access Amazon Web Services resources.
--
-- 'httpStatus', 'describeConnectorResponse_httpStatus' - The response's http status code.
newDescribeConnectorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeConnectorResponse
newDescribeConnectorResponse pHttpStatus_ =
  DescribeConnectorResponse'
    { creationTime =
        Prelude.Nothing,
      kafkaCluster = Prelude.Nothing,
      kafkaConnectVersion = Prelude.Nothing,
      logDelivery = Prelude.Nothing,
      currentVersion = Prelude.Nothing,
      connectorConfiguration = Prelude.Nothing,
      workerConfiguration = Prelude.Nothing,
      connectorArn = Prelude.Nothing,
      connectorName = Prelude.Nothing,
      connectorState = Prelude.Nothing,
      capacity = Prelude.Nothing,
      plugins = Prelude.Nothing,
      connectorDescription = Prelude.Nothing,
      kafkaClusterClientAuthentication =
        Prelude.Nothing,
      kafkaClusterEncryptionInTransit =
        Prelude.Nothing,
      serviceExecutionRoleArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time the connector was created.
describeConnectorResponse_creationTime :: Lens.Lens' DescribeConnectorResponse (Prelude.Maybe Prelude.UTCTime)
describeConnectorResponse_creationTime = Lens.lens (\DescribeConnectorResponse' {creationTime} -> creationTime) (\s@DescribeConnectorResponse' {} a -> s {creationTime = a} :: DescribeConnectorResponse) Prelude.. Lens.mapping Core._Time

-- | The Apache Kafka cluster that the connector is connected to.
describeConnectorResponse_kafkaCluster :: Lens.Lens' DescribeConnectorResponse (Prelude.Maybe KafkaClusterDescription)
describeConnectorResponse_kafkaCluster = Lens.lens (\DescribeConnectorResponse' {kafkaCluster} -> kafkaCluster) (\s@DescribeConnectorResponse' {} a -> s {kafkaCluster = a} :: DescribeConnectorResponse)

-- | The version of Kafka Connect. It has to be compatible with both the
-- Apache Kafka cluster\'s version and the plugins.
describeConnectorResponse_kafkaConnectVersion :: Lens.Lens' DescribeConnectorResponse (Prelude.Maybe Prelude.Text)
describeConnectorResponse_kafkaConnectVersion = Lens.lens (\DescribeConnectorResponse' {kafkaConnectVersion} -> kafkaConnectVersion) (\s@DescribeConnectorResponse' {} a -> s {kafkaConnectVersion = a} :: DescribeConnectorResponse)

-- | Details about delivering logs to Amazon CloudWatch Logs.
describeConnectorResponse_logDelivery :: Lens.Lens' DescribeConnectorResponse (Prelude.Maybe LogDeliveryDescription)
describeConnectorResponse_logDelivery = Lens.lens (\DescribeConnectorResponse' {logDelivery} -> logDelivery) (\s@DescribeConnectorResponse' {} a -> s {logDelivery = a} :: DescribeConnectorResponse)

-- | The current version of the connector.
describeConnectorResponse_currentVersion :: Lens.Lens' DescribeConnectorResponse (Prelude.Maybe Prelude.Text)
describeConnectorResponse_currentVersion = Lens.lens (\DescribeConnectorResponse' {currentVersion} -> currentVersion) (\s@DescribeConnectorResponse' {} a -> s {currentVersion = a} :: DescribeConnectorResponse)

-- | A map of keys to values that represent the configuration for the
-- connector.
describeConnectorResponse_connectorConfiguration :: Lens.Lens' DescribeConnectorResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeConnectorResponse_connectorConfiguration = Lens.lens (\DescribeConnectorResponse' {connectorConfiguration} -> connectorConfiguration) (\s@DescribeConnectorResponse' {} a -> s {connectorConfiguration = a} :: DescribeConnectorResponse) Prelude.. Lens.mapping Lens.coerced

-- | Specifies which worker configuration was used for the connector.
describeConnectorResponse_workerConfiguration :: Lens.Lens' DescribeConnectorResponse (Prelude.Maybe WorkerConfigurationDescription)
describeConnectorResponse_workerConfiguration = Lens.lens (\DescribeConnectorResponse' {workerConfiguration} -> workerConfiguration) (\s@DescribeConnectorResponse' {} a -> s {workerConfiguration = a} :: DescribeConnectorResponse)

-- | The Amazon Resource Name (ARN) of the connector.
describeConnectorResponse_connectorArn :: Lens.Lens' DescribeConnectorResponse (Prelude.Maybe Prelude.Text)
describeConnectorResponse_connectorArn = Lens.lens (\DescribeConnectorResponse' {connectorArn} -> connectorArn) (\s@DescribeConnectorResponse' {} a -> s {connectorArn = a} :: DescribeConnectorResponse)

-- | The name of the connector.
describeConnectorResponse_connectorName :: Lens.Lens' DescribeConnectorResponse (Prelude.Maybe Prelude.Text)
describeConnectorResponse_connectorName = Lens.lens (\DescribeConnectorResponse' {connectorName} -> connectorName) (\s@DescribeConnectorResponse' {} a -> s {connectorName = a} :: DescribeConnectorResponse)

-- | The state of the connector.
describeConnectorResponse_connectorState :: Lens.Lens' DescribeConnectorResponse (Prelude.Maybe ConnectorState)
describeConnectorResponse_connectorState = Lens.lens (\DescribeConnectorResponse' {connectorState} -> connectorState) (\s@DescribeConnectorResponse' {} a -> s {connectorState = a} :: DescribeConnectorResponse)

-- | Information about the capacity of the connector, whether it is auto
-- scaled or provisioned.
describeConnectorResponse_capacity :: Lens.Lens' DescribeConnectorResponse (Prelude.Maybe CapacityDescription)
describeConnectorResponse_capacity = Lens.lens (\DescribeConnectorResponse' {capacity} -> capacity) (\s@DescribeConnectorResponse' {} a -> s {capacity = a} :: DescribeConnectorResponse)

-- | Specifies which plugins were used for this connector.
describeConnectorResponse_plugins :: Lens.Lens' DescribeConnectorResponse (Prelude.Maybe [PluginDescription])
describeConnectorResponse_plugins = Lens.lens (\DescribeConnectorResponse' {plugins} -> plugins) (\s@DescribeConnectorResponse' {} a -> s {plugins = a} :: DescribeConnectorResponse) Prelude.. Lens.mapping Lens.coerced

-- | A summary description of the connector.
describeConnectorResponse_connectorDescription :: Lens.Lens' DescribeConnectorResponse (Prelude.Maybe Prelude.Text)
describeConnectorResponse_connectorDescription = Lens.lens (\DescribeConnectorResponse' {connectorDescription} -> connectorDescription) (\s@DescribeConnectorResponse' {} a -> s {connectorDescription = a} :: DescribeConnectorResponse)

-- | The type of client authentication used to connect to the Apache Kafka
-- cluster. The value is NONE when no client authentication is used.
describeConnectorResponse_kafkaClusterClientAuthentication :: Lens.Lens' DescribeConnectorResponse (Prelude.Maybe KafkaClusterClientAuthenticationDescription)
describeConnectorResponse_kafkaClusterClientAuthentication = Lens.lens (\DescribeConnectorResponse' {kafkaClusterClientAuthentication} -> kafkaClusterClientAuthentication) (\s@DescribeConnectorResponse' {} a -> s {kafkaClusterClientAuthentication = a} :: DescribeConnectorResponse)

-- | Details of encryption in transit to the Apache Kafka cluster.
describeConnectorResponse_kafkaClusterEncryptionInTransit :: Lens.Lens' DescribeConnectorResponse (Prelude.Maybe KafkaClusterEncryptionInTransitDescription)
describeConnectorResponse_kafkaClusterEncryptionInTransit = Lens.lens (\DescribeConnectorResponse' {kafkaClusterEncryptionInTransit} -> kafkaClusterEncryptionInTransit) (\s@DescribeConnectorResponse' {} a -> s {kafkaClusterEncryptionInTransit = a} :: DescribeConnectorResponse)

-- | The Amazon Resource Name (ARN) of the IAM role used by the connector to
-- access Amazon Web Services resources.
describeConnectorResponse_serviceExecutionRoleArn :: Lens.Lens' DescribeConnectorResponse (Prelude.Maybe Prelude.Text)
describeConnectorResponse_serviceExecutionRoleArn = Lens.lens (\DescribeConnectorResponse' {serviceExecutionRoleArn} -> serviceExecutionRoleArn) (\s@DescribeConnectorResponse' {} a -> s {serviceExecutionRoleArn = a} :: DescribeConnectorResponse)

-- | The response's http status code.
describeConnectorResponse_httpStatus :: Lens.Lens' DescribeConnectorResponse Prelude.Int
describeConnectorResponse_httpStatus = Lens.lens (\DescribeConnectorResponse' {httpStatus} -> httpStatus) (\s@DescribeConnectorResponse' {} a -> s {httpStatus = a} :: DescribeConnectorResponse)

instance Prelude.NFData DescribeConnectorResponse
