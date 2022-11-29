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
-- Module      : Amazonka.KafkaConnect.CreateConnector
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a connector using the specified properties.
module Amazonka.KafkaConnect.CreateConnector
  ( -- * Creating a Request
    CreateConnector (..),
    newCreateConnector,

    -- * Request Lenses
    createConnector_connectorDescription,
    createConnector_logDelivery,
    createConnector_workerConfiguration,
    createConnector_capacity,
    createConnector_connectorConfiguration,
    createConnector_connectorName,
    createConnector_kafkaCluster,
    createConnector_kafkaClusterClientAuthentication,
    createConnector_kafkaClusterEncryptionInTransit,
    createConnector_kafkaConnectVersion,
    createConnector_plugins,
    createConnector_serviceExecutionRoleArn,

    -- * Destructuring the Response
    CreateConnectorResponse (..),
    newCreateConnectorResponse,

    -- * Response Lenses
    createConnectorResponse_connectorArn,
    createConnectorResponse_connectorName,
    createConnectorResponse_connectorState,
    createConnectorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KafkaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateConnector' smart constructor.
data CreateConnector = CreateConnector'
  { -- | A summary description of the connector.
    connectorDescription :: Prelude.Maybe Prelude.Text,
    -- | Details about log delivery.
    logDelivery :: Prelude.Maybe LogDelivery,
    -- | Specifies which worker configuration to use with the connector.
    workerConfiguration :: Prelude.Maybe WorkerConfiguration,
    -- | Information about the capacity allocated to the connector. Exactly one
    -- of the two properties must be specified.
    capacity :: Capacity,
    -- | A map of keys to values that represent the configuration for the
    -- connector.
    connectorConfiguration :: Core.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the connector.
    connectorName :: Prelude.Text,
    -- | Specifies which Apache Kafka cluster to connect to.
    kafkaCluster :: KafkaCluster,
    -- | Details of the client authentication used by the Apache Kafka cluster.
    kafkaClusterClientAuthentication :: KafkaClusterClientAuthentication,
    -- | Details of encryption in transit to the Apache Kafka cluster.
    kafkaClusterEncryptionInTransit :: KafkaClusterEncryptionInTransit,
    -- | The version of Kafka Connect. It has to be compatible with both the
    -- Apache Kafka cluster\'s version and the plugins.
    kafkaConnectVersion :: Prelude.Text,
    -- | Specifies which plugins to use for the connector.
    plugins :: [Plugin],
    -- | The Amazon Resource Name (ARN) of the IAM role used by the connector to
    -- access the Amazon Web Services resources that it needs. The types of
    -- resources depends on the logic of the connector. For example, a
    -- connector that has Amazon S3 as a destination must have permissions that
    -- allow it to write to the S3 destination bucket.
    serviceExecutionRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConnector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectorDescription', 'createConnector_connectorDescription' - A summary description of the connector.
--
-- 'logDelivery', 'createConnector_logDelivery' - Details about log delivery.
--
-- 'workerConfiguration', 'createConnector_workerConfiguration' - Specifies which worker configuration to use with the connector.
--
-- 'capacity', 'createConnector_capacity' - Information about the capacity allocated to the connector. Exactly one
-- of the two properties must be specified.
--
-- 'connectorConfiguration', 'createConnector_connectorConfiguration' - A map of keys to values that represent the configuration for the
-- connector.
--
-- 'connectorName', 'createConnector_connectorName' - The name of the connector.
--
-- 'kafkaCluster', 'createConnector_kafkaCluster' - Specifies which Apache Kafka cluster to connect to.
--
-- 'kafkaClusterClientAuthentication', 'createConnector_kafkaClusterClientAuthentication' - Details of the client authentication used by the Apache Kafka cluster.
--
-- 'kafkaClusterEncryptionInTransit', 'createConnector_kafkaClusterEncryptionInTransit' - Details of encryption in transit to the Apache Kafka cluster.
--
-- 'kafkaConnectVersion', 'createConnector_kafkaConnectVersion' - The version of Kafka Connect. It has to be compatible with both the
-- Apache Kafka cluster\'s version and the plugins.
--
-- 'plugins', 'createConnector_plugins' - Specifies which plugins to use for the connector.
--
-- 'serviceExecutionRoleArn', 'createConnector_serviceExecutionRoleArn' - The Amazon Resource Name (ARN) of the IAM role used by the connector to
-- access the Amazon Web Services resources that it needs. The types of
-- resources depends on the logic of the connector. For example, a
-- connector that has Amazon S3 as a destination must have permissions that
-- allow it to write to the S3 destination bucket.
newCreateConnector ::
  -- | 'capacity'
  Capacity ->
  -- | 'connectorName'
  Prelude.Text ->
  -- | 'kafkaCluster'
  KafkaCluster ->
  -- | 'kafkaClusterClientAuthentication'
  KafkaClusterClientAuthentication ->
  -- | 'kafkaClusterEncryptionInTransit'
  KafkaClusterEncryptionInTransit ->
  -- | 'kafkaConnectVersion'
  Prelude.Text ->
  -- | 'serviceExecutionRoleArn'
  Prelude.Text ->
  CreateConnector
newCreateConnector
  pCapacity_
  pConnectorName_
  pKafkaCluster_
  pKafkaClusterClientAuthentication_
  pKafkaClusterEncryptionInTransit_
  pKafkaConnectVersion_
  pServiceExecutionRoleArn_ =
    CreateConnector'
      { connectorDescription =
          Prelude.Nothing,
        logDelivery = Prelude.Nothing,
        workerConfiguration = Prelude.Nothing,
        capacity = pCapacity_,
        connectorConfiguration = Prelude.mempty,
        connectorName = pConnectorName_,
        kafkaCluster = pKafkaCluster_,
        kafkaClusterClientAuthentication =
          pKafkaClusterClientAuthentication_,
        kafkaClusterEncryptionInTransit =
          pKafkaClusterEncryptionInTransit_,
        kafkaConnectVersion = pKafkaConnectVersion_,
        plugins = Prelude.mempty,
        serviceExecutionRoleArn = pServiceExecutionRoleArn_
      }

-- | A summary description of the connector.
createConnector_connectorDescription :: Lens.Lens' CreateConnector (Prelude.Maybe Prelude.Text)
createConnector_connectorDescription = Lens.lens (\CreateConnector' {connectorDescription} -> connectorDescription) (\s@CreateConnector' {} a -> s {connectorDescription = a} :: CreateConnector)

-- | Details about log delivery.
createConnector_logDelivery :: Lens.Lens' CreateConnector (Prelude.Maybe LogDelivery)
createConnector_logDelivery = Lens.lens (\CreateConnector' {logDelivery} -> logDelivery) (\s@CreateConnector' {} a -> s {logDelivery = a} :: CreateConnector)

-- | Specifies which worker configuration to use with the connector.
createConnector_workerConfiguration :: Lens.Lens' CreateConnector (Prelude.Maybe WorkerConfiguration)
createConnector_workerConfiguration = Lens.lens (\CreateConnector' {workerConfiguration} -> workerConfiguration) (\s@CreateConnector' {} a -> s {workerConfiguration = a} :: CreateConnector)

-- | Information about the capacity allocated to the connector. Exactly one
-- of the two properties must be specified.
createConnector_capacity :: Lens.Lens' CreateConnector Capacity
createConnector_capacity = Lens.lens (\CreateConnector' {capacity} -> capacity) (\s@CreateConnector' {} a -> s {capacity = a} :: CreateConnector)

-- | A map of keys to values that represent the configuration for the
-- connector.
createConnector_connectorConfiguration :: Lens.Lens' CreateConnector (Prelude.HashMap Prelude.Text Prelude.Text)
createConnector_connectorConfiguration = Lens.lens (\CreateConnector' {connectorConfiguration} -> connectorConfiguration) (\s@CreateConnector' {} a -> s {connectorConfiguration = a} :: CreateConnector) Prelude.. Core._Sensitive Prelude.. Lens.coerced

-- | The name of the connector.
createConnector_connectorName :: Lens.Lens' CreateConnector Prelude.Text
createConnector_connectorName = Lens.lens (\CreateConnector' {connectorName} -> connectorName) (\s@CreateConnector' {} a -> s {connectorName = a} :: CreateConnector)

-- | Specifies which Apache Kafka cluster to connect to.
createConnector_kafkaCluster :: Lens.Lens' CreateConnector KafkaCluster
createConnector_kafkaCluster = Lens.lens (\CreateConnector' {kafkaCluster} -> kafkaCluster) (\s@CreateConnector' {} a -> s {kafkaCluster = a} :: CreateConnector)

-- | Details of the client authentication used by the Apache Kafka cluster.
createConnector_kafkaClusterClientAuthentication :: Lens.Lens' CreateConnector KafkaClusterClientAuthentication
createConnector_kafkaClusterClientAuthentication = Lens.lens (\CreateConnector' {kafkaClusterClientAuthentication} -> kafkaClusterClientAuthentication) (\s@CreateConnector' {} a -> s {kafkaClusterClientAuthentication = a} :: CreateConnector)

-- | Details of encryption in transit to the Apache Kafka cluster.
createConnector_kafkaClusterEncryptionInTransit :: Lens.Lens' CreateConnector KafkaClusterEncryptionInTransit
createConnector_kafkaClusterEncryptionInTransit = Lens.lens (\CreateConnector' {kafkaClusterEncryptionInTransit} -> kafkaClusterEncryptionInTransit) (\s@CreateConnector' {} a -> s {kafkaClusterEncryptionInTransit = a} :: CreateConnector)

-- | The version of Kafka Connect. It has to be compatible with both the
-- Apache Kafka cluster\'s version and the plugins.
createConnector_kafkaConnectVersion :: Lens.Lens' CreateConnector Prelude.Text
createConnector_kafkaConnectVersion = Lens.lens (\CreateConnector' {kafkaConnectVersion} -> kafkaConnectVersion) (\s@CreateConnector' {} a -> s {kafkaConnectVersion = a} :: CreateConnector)

-- | Specifies which plugins to use for the connector.
createConnector_plugins :: Lens.Lens' CreateConnector [Plugin]
createConnector_plugins = Lens.lens (\CreateConnector' {plugins} -> plugins) (\s@CreateConnector' {} a -> s {plugins = a} :: CreateConnector) Prelude.. Lens.coerced

-- | The Amazon Resource Name (ARN) of the IAM role used by the connector to
-- access the Amazon Web Services resources that it needs. The types of
-- resources depends on the logic of the connector. For example, a
-- connector that has Amazon S3 as a destination must have permissions that
-- allow it to write to the S3 destination bucket.
createConnector_serviceExecutionRoleArn :: Lens.Lens' CreateConnector Prelude.Text
createConnector_serviceExecutionRoleArn = Lens.lens (\CreateConnector' {serviceExecutionRoleArn} -> serviceExecutionRoleArn) (\s@CreateConnector' {} a -> s {serviceExecutionRoleArn = a} :: CreateConnector)

instance Core.AWSRequest CreateConnector where
  type
    AWSResponse CreateConnector =
      CreateConnectorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateConnectorResponse'
            Prelude.<$> (x Core..?> "connectorArn")
            Prelude.<*> (x Core..?> "connectorName")
            Prelude.<*> (x Core..?> "connectorState")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateConnector where
  hashWithSalt _salt CreateConnector' {..} =
    _salt `Prelude.hashWithSalt` connectorDescription
      `Prelude.hashWithSalt` logDelivery
      `Prelude.hashWithSalt` workerConfiguration
      `Prelude.hashWithSalt` capacity
      `Prelude.hashWithSalt` connectorConfiguration
      `Prelude.hashWithSalt` connectorName
      `Prelude.hashWithSalt` kafkaCluster
      `Prelude.hashWithSalt` kafkaClusterClientAuthentication
      `Prelude.hashWithSalt` kafkaClusterEncryptionInTransit
      `Prelude.hashWithSalt` kafkaConnectVersion
      `Prelude.hashWithSalt` plugins
      `Prelude.hashWithSalt` serviceExecutionRoleArn

instance Prelude.NFData CreateConnector where
  rnf CreateConnector' {..} =
    Prelude.rnf connectorDescription
      `Prelude.seq` Prelude.rnf logDelivery
      `Prelude.seq` Prelude.rnf workerConfiguration
      `Prelude.seq` Prelude.rnf capacity
      `Prelude.seq` Prelude.rnf connectorConfiguration
      `Prelude.seq` Prelude.rnf connectorName
      `Prelude.seq` Prelude.rnf kafkaCluster
      `Prelude.seq` Prelude.rnf kafkaClusterClientAuthentication
      `Prelude.seq` Prelude.rnf kafkaClusterEncryptionInTransit
      `Prelude.seq` Prelude.rnf kafkaConnectVersion
      `Prelude.seq` Prelude.rnf plugins
      `Prelude.seq` Prelude.rnf serviceExecutionRoleArn

instance Core.ToHeaders CreateConnector where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateConnector where
  toJSON CreateConnector' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("connectorDescription" Core..=)
              Prelude.<$> connectorDescription,
            ("logDelivery" Core..=) Prelude.<$> logDelivery,
            ("workerConfiguration" Core..=)
              Prelude.<$> workerConfiguration,
            Prelude.Just ("capacity" Core..= capacity),
            Prelude.Just
              ( "connectorConfiguration"
                  Core..= connectorConfiguration
              ),
            Prelude.Just ("connectorName" Core..= connectorName),
            Prelude.Just ("kafkaCluster" Core..= kafkaCluster),
            Prelude.Just
              ( "kafkaClusterClientAuthentication"
                  Core..= kafkaClusterClientAuthentication
              ),
            Prelude.Just
              ( "kafkaClusterEncryptionInTransit"
                  Core..= kafkaClusterEncryptionInTransit
              ),
            Prelude.Just
              ("kafkaConnectVersion" Core..= kafkaConnectVersion),
            Prelude.Just ("plugins" Core..= plugins),
            Prelude.Just
              ( "serviceExecutionRoleArn"
                  Core..= serviceExecutionRoleArn
              )
          ]
      )

instance Core.ToPath CreateConnector where
  toPath = Prelude.const "/v1/connectors"

instance Core.ToQuery CreateConnector where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateConnectorResponse' smart constructor.
data CreateConnectorResponse = CreateConnectorResponse'
  { -- | The Amazon Resource Name (ARN) that Amazon assigned to the connector.
    connectorArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the connector.
    connectorName :: Prelude.Maybe Prelude.Text,
    -- | The state of the connector.
    connectorState :: Prelude.Maybe ConnectorState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConnectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectorArn', 'createConnectorResponse_connectorArn' - The Amazon Resource Name (ARN) that Amazon assigned to the connector.
--
-- 'connectorName', 'createConnectorResponse_connectorName' - The name of the connector.
--
-- 'connectorState', 'createConnectorResponse_connectorState' - The state of the connector.
--
-- 'httpStatus', 'createConnectorResponse_httpStatus' - The response's http status code.
newCreateConnectorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateConnectorResponse
newCreateConnectorResponse pHttpStatus_ =
  CreateConnectorResponse'
    { connectorArn =
        Prelude.Nothing,
      connectorName = Prelude.Nothing,
      connectorState = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) that Amazon assigned to the connector.
createConnectorResponse_connectorArn :: Lens.Lens' CreateConnectorResponse (Prelude.Maybe Prelude.Text)
createConnectorResponse_connectorArn = Lens.lens (\CreateConnectorResponse' {connectorArn} -> connectorArn) (\s@CreateConnectorResponse' {} a -> s {connectorArn = a} :: CreateConnectorResponse)

-- | The name of the connector.
createConnectorResponse_connectorName :: Lens.Lens' CreateConnectorResponse (Prelude.Maybe Prelude.Text)
createConnectorResponse_connectorName = Lens.lens (\CreateConnectorResponse' {connectorName} -> connectorName) (\s@CreateConnectorResponse' {} a -> s {connectorName = a} :: CreateConnectorResponse)

-- | The state of the connector.
createConnectorResponse_connectorState :: Lens.Lens' CreateConnectorResponse (Prelude.Maybe ConnectorState)
createConnectorResponse_connectorState = Lens.lens (\CreateConnectorResponse' {connectorState} -> connectorState) (\s@CreateConnectorResponse' {} a -> s {connectorState = a} :: CreateConnectorResponse)

-- | The response's http status code.
createConnectorResponse_httpStatus :: Lens.Lens' CreateConnectorResponse Prelude.Int
createConnectorResponse_httpStatus = Lens.lens (\CreateConnectorResponse' {httpStatus} -> httpStatus) (\s@CreateConnectorResponse' {} a -> s {httpStatus = a} :: CreateConnectorResponse)

instance Prelude.NFData CreateConnectorResponse where
  rnf CreateConnectorResponse' {..} =
    Prelude.rnf connectorArn
      `Prelude.seq` Prelude.rnf connectorName
      `Prelude.seq` Prelude.rnf connectorState
      `Prelude.seq` Prelude.rnf httpStatus
