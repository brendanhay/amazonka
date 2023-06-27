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
-- Module      : Amazonka.Kafka.CreateCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new MSK cluster.
module Amazonka.Kafka.CreateCluster
  ( -- * Creating a Request
    CreateCluster (..),
    newCreateCluster,

    -- * Request Lenses
    createCluster_clientAuthentication,
    createCluster_configurationInfo,
    createCluster_encryptionInfo,
    createCluster_enhancedMonitoring,
    createCluster_loggingInfo,
    createCluster_openMonitoring,
    createCluster_storageMode,
    createCluster_tags,
    createCluster_brokerNodeGroupInfo,
    createCluster_kafkaVersion,
    createCluster_numberOfBrokerNodes,
    createCluster_clusterName,

    -- * Destructuring the Response
    CreateClusterResponse (..),
    newCreateClusterResponse,

    -- * Response Lenses
    createClusterResponse_clusterArn,
    createClusterResponse_clusterName,
    createClusterResponse_state,
    createClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCluster' smart constructor.
data CreateCluster = CreateCluster'
  { -- | Includes all client authentication related information.
    clientAuthentication :: Prelude.Maybe ClientAuthentication,
    -- | Represents the configuration that you want MSK to use for the brokers in
    -- a cluster.
    configurationInfo :: Prelude.Maybe ConfigurationInfo,
    -- | Includes all encryption-related information.
    encryptionInfo :: Prelude.Maybe EncryptionInfo,
    -- | Specifies the level of monitoring for the MSK cluster. The possible
    -- values are DEFAULT, PER_BROKER, PER_TOPIC_PER_BROKER, and
    -- PER_TOPIC_PER_PARTITION.
    enhancedMonitoring :: Prelude.Maybe EnhancedMonitoring,
    loggingInfo :: Prelude.Maybe LoggingInfo,
    -- | The settings for open monitoring.
    openMonitoring :: Prelude.Maybe OpenMonitoringInfo,
    -- | This controls storage mode for supported storage tiers.
    storageMode :: Prelude.Maybe StorageMode,
    -- | Create tags when creating the cluster.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Information about the broker nodes in the cluster.
    brokerNodeGroupInfo :: BrokerNodeGroupInfo,
    -- | The version of Apache Kafka.
    kafkaVersion :: Prelude.Text,
    -- | The number of broker nodes in the cluster.
    numberOfBrokerNodes :: Prelude.Natural,
    -- | The name of the cluster.
    clusterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientAuthentication', 'createCluster_clientAuthentication' - Includes all client authentication related information.
--
-- 'configurationInfo', 'createCluster_configurationInfo' - Represents the configuration that you want MSK to use for the brokers in
-- a cluster.
--
-- 'encryptionInfo', 'createCluster_encryptionInfo' - Includes all encryption-related information.
--
-- 'enhancedMonitoring', 'createCluster_enhancedMonitoring' - Specifies the level of monitoring for the MSK cluster. The possible
-- values are DEFAULT, PER_BROKER, PER_TOPIC_PER_BROKER, and
-- PER_TOPIC_PER_PARTITION.
--
-- 'loggingInfo', 'createCluster_loggingInfo' - Undocumented member.
--
-- 'openMonitoring', 'createCluster_openMonitoring' - The settings for open monitoring.
--
-- 'storageMode', 'createCluster_storageMode' - This controls storage mode for supported storage tiers.
--
-- 'tags', 'createCluster_tags' - Create tags when creating the cluster.
--
-- 'brokerNodeGroupInfo', 'createCluster_brokerNodeGroupInfo' - Information about the broker nodes in the cluster.
--
-- 'kafkaVersion', 'createCluster_kafkaVersion' - The version of Apache Kafka.
--
-- 'numberOfBrokerNodes', 'createCluster_numberOfBrokerNodes' - The number of broker nodes in the cluster.
--
-- 'clusterName', 'createCluster_clusterName' - The name of the cluster.
newCreateCluster ::
  -- | 'brokerNodeGroupInfo'
  BrokerNodeGroupInfo ->
  -- | 'kafkaVersion'
  Prelude.Text ->
  -- | 'numberOfBrokerNodes'
  Prelude.Natural ->
  -- | 'clusterName'
  Prelude.Text ->
  CreateCluster
newCreateCluster
  pBrokerNodeGroupInfo_
  pKafkaVersion_
  pNumberOfBrokerNodes_
  pClusterName_ =
    CreateCluster'
      { clientAuthentication =
          Prelude.Nothing,
        configurationInfo = Prelude.Nothing,
        encryptionInfo = Prelude.Nothing,
        enhancedMonitoring = Prelude.Nothing,
        loggingInfo = Prelude.Nothing,
        openMonitoring = Prelude.Nothing,
        storageMode = Prelude.Nothing,
        tags = Prelude.Nothing,
        brokerNodeGroupInfo = pBrokerNodeGroupInfo_,
        kafkaVersion = pKafkaVersion_,
        numberOfBrokerNodes = pNumberOfBrokerNodes_,
        clusterName = pClusterName_
      }

-- | Includes all client authentication related information.
createCluster_clientAuthentication :: Lens.Lens' CreateCluster (Prelude.Maybe ClientAuthentication)
createCluster_clientAuthentication = Lens.lens (\CreateCluster' {clientAuthentication} -> clientAuthentication) (\s@CreateCluster' {} a -> s {clientAuthentication = a} :: CreateCluster)

-- | Represents the configuration that you want MSK to use for the brokers in
-- a cluster.
createCluster_configurationInfo :: Lens.Lens' CreateCluster (Prelude.Maybe ConfigurationInfo)
createCluster_configurationInfo = Lens.lens (\CreateCluster' {configurationInfo} -> configurationInfo) (\s@CreateCluster' {} a -> s {configurationInfo = a} :: CreateCluster)

-- | Includes all encryption-related information.
createCluster_encryptionInfo :: Lens.Lens' CreateCluster (Prelude.Maybe EncryptionInfo)
createCluster_encryptionInfo = Lens.lens (\CreateCluster' {encryptionInfo} -> encryptionInfo) (\s@CreateCluster' {} a -> s {encryptionInfo = a} :: CreateCluster)

-- | Specifies the level of monitoring for the MSK cluster. The possible
-- values are DEFAULT, PER_BROKER, PER_TOPIC_PER_BROKER, and
-- PER_TOPIC_PER_PARTITION.
createCluster_enhancedMonitoring :: Lens.Lens' CreateCluster (Prelude.Maybe EnhancedMonitoring)
createCluster_enhancedMonitoring = Lens.lens (\CreateCluster' {enhancedMonitoring} -> enhancedMonitoring) (\s@CreateCluster' {} a -> s {enhancedMonitoring = a} :: CreateCluster)

-- | Undocumented member.
createCluster_loggingInfo :: Lens.Lens' CreateCluster (Prelude.Maybe LoggingInfo)
createCluster_loggingInfo = Lens.lens (\CreateCluster' {loggingInfo} -> loggingInfo) (\s@CreateCluster' {} a -> s {loggingInfo = a} :: CreateCluster)

-- | The settings for open monitoring.
createCluster_openMonitoring :: Lens.Lens' CreateCluster (Prelude.Maybe OpenMonitoringInfo)
createCluster_openMonitoring = Lens.lens (\CreateCluster' {openMonitoring} -> openMonitoring) (\s@CreateCluster' {} a -> s {openMonitoring = a} :: CreateCluster)

-- | This controls storage mode for supported storage tiers.
createCluster_storageMode :: Lens.Lens' CreateCluster (Prelude.Maybe StorageMode)
createCluster_storageMode = Lens.lens (\CreateCluster' {storageMode} -> storageMode) (\s@CreateCluster' {} a -> s {storageMode = a} :: CreateCluster)

-- | Create tags when creating the cluster.
createCluster_tags :: Lens.Lens' CreateCluster (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createCluster_tags = Lens.lens (\CreateCluster' {tags} -> tags) (\s@CreateCluster' {} a -> s {tags = a} :: CreateCluster) Prelude.. Lens.mapping Lens.coerced

-- | Information about the broker nodes in the cluster.
createCluster_brokerNodeGroupInfo :: Lens.Lens' CreateCluster BrokerNodeGroupInfo
createCluster_brokerNodeGroupInfo = Lens.lens (\CreateCluster' {brokerNodeGroupInfo} -> brokerNodeGroupInfo) (\s@CreateCluster' {} a -> s {brokerNodeGroupInfo = a} :: CreateCluster)

-- | The version of Apache Kafka.
createCluster_kafkaVersion :: Lens.Lens' CreateCluster Prelude.Text
createCluster_kafkaVersion = Lens.lens (\CreateCluster' {kafkaVersion} -> kafkaVersion) (\s@CreateCluster' {} a -> s {kafkaVersion = a} :: CreateCluster)

-- | The number of broker nodes in the cluster.
createCluster_numberOfBrokerNodes :: Lens.Lens' CreateCluster Prelude.Natural
createCluster_numberOfBrokerNodes = Lens.lens (\CreateCluster' {numberOfBrokerNodes} -> numberOfBrokerNodes) (\s@CreateCluster' {} a -> s {numberOfBrokerNodes = a} :: CreateCluster)

-- | The name of the cluster.
createCluster_clusterName :: Lens.Lens' CreateCluster Prelude.Text
createCluster_clusterName = Lens.lens (\CreateCluster' {clusterName} -> clusterName) (\s@CreateCluster' {} a -> s {clusterName = a} :: CreateCluster)

instance Core.AWSRequest CreateCluster where
  type
    AWSResponse CreateCluster =
      CreateClusterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateClusterResponse'
            Prelude.<$> (x Data..?> "clusterArn")
            Prelude.<*> (x Data..?> "clusterName")
            Prelude.<*> (x Data..?> "state")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCluster where
  hashWithSalt _salt CreateCluster' {..} =
    _salt
      `Prelude.hashWithSalt` clientAuthentication
      `Prelude.hashWithSalt` configurationInfo
      `Prelude.hashWithSalt` encryptionInfo
      `Prelude.hashWithSalt` enhancedMonitoring
      `Prelude.hashWithSalt` loggingInfo
      `Prelude.hashWithSalt` openMonitoring
      `Prelude.hashWithSalt` storageMode
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` brokerNodeGroupInfo
      `Prelude.hashWithSalt` kafkaVersion
      `Prelude.hashWithSalt` numberOfBrokerNodes
      `Prelude.hashWithSalt` clusterName

instance Prelude.NFData CreateCluster where
  rnf CreateCluster' {..} =
    Prelude.rnf clientAuthentication
      `Prelude.seq` Prelude.rnf configurationInfo
      `Prelude.seq` Prelude.rnf encryptionInfo
      `Prelude.seq` Prelude.rnf enhancedMonitoring
      `Prelude.seq` Prelude.rnf loggingInfo
      `Prelude.seq` Prelude.rnf openMonitoring
      `Prelude.seq` Prelude.rnf storageMode
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf brokerNodeGroupInfo
      `Prelude.seq` Prelude.rnf kafkaVersion
      `Prelude.seq` Prelude.rnf numberOfBrokerNodes
      `Prelude.seq` Prelude.rnf clusterName

instance Data.ToHeaders CreateCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateCluster where
  toJSON CreateCluster' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientAuthentication" Data..=)
              Prelude.<$> clientAuthentication,
            ("configurationInfo" Data..=)
              Prelude.<$> configurationInfo,
            ("encryptionInfo" Data..=)
              Prelude.<$> encryptionInfo,
            ("enhancedMonitoring" Data..=)
              Prelude.<$> enhancedMonitoring,
            ("loggingInfo" Data..=) Prelude.<$> loggingInfo,
            ("openMonitoring" Data..=)
              Prelude.<$> openMonitoring,
            ("storageMode" Data..=) Prelude.<$> storageMode,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("brokerNodeGroupInfo" Data..= brokerNodeGroupInfo),
            Prelude.Just ("kafkaVersion" Data..= kafkaVersion),
            Prelude.Just
              ("numberOfBrokerNodes" Data..= numberOfBrokerNodes),
            Prelude.Just ("clusterName" Data..= clusterName)
          ]
      )

instance Data.ToPath CreateCluster where
  toPath = Prelude.const "/v1/clusters"

instance Data.ToQuery CreateCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateClusterResponse' smart constructor.
data CreateClusterResponse = CreateClusterResponse'
  { -- | The Amazon Resource Name (ARN) of the cluster.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the MSK cluster.
    clusterName :: Prelude.Maybe Prelude.Text,
    -- | The state of the cluster. The possible states are ACTIVE, CREATING,
    -- DELETING, FAILED, HEALING, MAINTENANCE, REBOOTING_BROKER, and UPDATING.
    state :: Prelude.Maybe ClusterState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'createClusterResponse_clusterArn' - The Amazon Resource Name (ARN) of the cluster.
--
-- 'clusterName', 'createClusterResponse_clusterName' - The name of the MSK cluster.
--
-- 'state', 'createClusterResponse_state' - The state of the cluster. The possible states are ACTIVE, CREATING,
-- DELETING, FAILED, HEALING, MAINTENANCE, REBOOTING_BROKER, and UPDATING.
--
-- 'httpStatus', 'createClusterResponse_httpStatus' - The response's http status code.
newCreateClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateClusterResponse
newCreateClusterResponse pHttpStatus_ =
  CreateClusterResponse'
    { clusterArn =
        Prelude.Nothing,
      clusterName = Prelude.Nothing,
      state = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the cluster.
createClusterResponse_clusterArn :: Lens.Lens' CreateClusterResponse (Prelude.Maybe Prelude.Text)
createClusterResponse_clusterArn = Lens.lens (\CreateClusterResponse' {clusterArn} -> clusterArn) (\s@CreateClusterResponse' {} a -> s {clusterArn = a} :: CreateClusterResponse)

-- | The name of the MSK cluster.
createClusterResponse_clusterName :: Lens.Lens' CreateClusterResponse (Prelude.Maybe Prelude.Text)
createClusterResponse_clusterName = Lens.lens (\CreateClusterResponse' {clusterName} -> clusterName) (\s@CreateClusterResponse' {} a -> s {clusterName = a} :: CreateClusterResponse)

-- | The state of the cluster. The possible states are ACTIVE, CREATING,
-- DELETING, FAILED, HEALING, MAINTENANCE, REBOOTING_BROKER, and UPDATING.
createClusterResponse_state :: Lens.Lens' CreateClusterResponse (Prelude.Maybe ClusterState)
createClusterResponse_state = Lens.lens (\CreateClusterResponse' {state} -> state) (\s@CreateClusterResponse' {} a -> s {state = a} :: CreateClusterResponse)

-- | The response's http status code.
createClusterResponse_httpStatus :: Lens.Lens' CreateClusterResponse Prelude.Int
createClusterResponse_httpStatus = Lens.lens (\CreateClusterResponse' {httpStatus} -> httpStatus) (\s@CreateClusterResponse' {} a -> s {httpStatus = a} :: CreateClusterResponse)

instance Prelude.NFData CreateClusterResponse where
  rnf CreateClusterResponse' {..} =
    Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf httpStatus
