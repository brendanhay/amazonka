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
-- Module      : Amazonka.EKS.Types.Cluster
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.Cluster where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types.Certificate
import Amazonka.EKS.Types.ClusterHealth
import Amazonka.EKS.Types.ClusterStatus
import Amazonka.EKS.Types.ConnectorConfigResponse
import Amazonka.EKS.Types.EncryptionConfig
import Amazonka.EKS.Types.Identity
import Amazonka.EKS.Types.KubernetesNetworkConfigResponse
import Amazonka.EKS.Types.Logging
import Amazonka.EKS.Types.OutpostConfigResponse
import Amazonka.EKS.Types.VpcConfigResponse
import qualified Amazonka.Prelude as Prelude

-- | An object representing an Amazon EKS cluster.
--
-- /See:/ 'newCluster' smart constructor.
data Cluster = Cluster'
  { -- | The Amazon Resource Name (ARN) of the cluster.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The @certificate-authority-data@ for your cluster.
    certificateAuthority :: Prelude.Maybe Certificate,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The configuration used to connect to a cluster for registration.
    connectorConfig :: Prelude.Maybe ConnectorConfigResponse,
    -- | The Unix epoch timestamp in seconds for when the cluster was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The encryption configuration for the cluster.
    encryptionConfig :: Prelude.Maybe [EncryptionConfig],
    -- | The endpoint for your Kubernetes API server.
    endpoint :: Prelude.Maybe Prelude.Text,
    -- | An object representing the health of your local Amazon EKS cluster on an
    -- Amazon Web Services Outpost. This object isn\'t available for clusters
    -- on the Amazon Web Services cloud.
    health :: Prelude.Maybe ClusterHealth,
    -- | The ID of your local Amazon EKS cluster on an Amazon Web Services
    -- Outpost. This property isn\'t available for an Amazon EKS cluster on the
    -- Amazon Web Services cloud.
    id :: Prelude.Maybe Prelude.Text,
    -- | The identity provider information for the cluster.
    identity :: Prelude.Maybe Identity,
    -- | The Kubernetes network configuration for the cluster.
    kubernetesNetworkConfig :: Prelude.Maybe KubernetesNetworkConfigResponse,
    -- | The logging configuration for your cluster.
    logging :: Prelude.Maybe Logging,
    -- | The name of the cluster.
    name :: Prelude.Maybe Prelude.Text,
    -- | An object representing the configuration of your local Amazon EKS
    -- cluster on an Amazon Web Services Outpost. This object isn\'t available
    -- for clusters on the Amazon Web Services cloud.
    outpostConfig :: Prelude.Maybe OutpostConfigResponse,
    -- | The platform version of your Amazon EKS cluster. For more information,
    -- see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/platform-versions.html Platform Versions>
    -- in the //Amazon EKS User Guide// .
    platformVersion :: Prelude.Maybe Prelude.Text,
    -- | The VPC configuration used by the cluster control plane. Amazon EKS VPC
    -- resources have specific requirements to work properly with Kubernetes.
    -- For more information, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/network_reqs.html Cluster VPC Considerations>
    -- and
    -- <https://docs.aws.amazon.com/eks/latest/userguide/sec-group-reqs.html Cluster Security Group Considerations>
    -- in the /Amazon EKS User Guide/.
    resourcesVpcConfig :: Prelude.Maybe VpcConfigResponse,
    -- | The Amazon Resource Name (ARN) of the IAM role that provides permissions
    -- for the Kubernetes control plane to make calls to Amazon Web Services
    -- API operations on your behalf.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The current status of the cluster.
    status :: Prelude.Maybe ClusterStatus,
    -- | The metadata that you apply to the cluster to assist with categorization
    -- and organization. Each tag consists of a key and an optional value. You
    -- define both. Cluster tags do not propagate to any other resources
    -- associated with the cluster.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Kubernetes server version for the cluster.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Cluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'cluster_arn' - The Amazon Resource Name (ARN) of the cluster.
--
-- 'certificateAuthority', 'cluster_certificateAuthority' - The @certificate-authority-data@ for your cluster.
--
-- 'clientRequestToken', 'cluster_clientRequestToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'connectorConfig', 'cluster_connectorConfig' - The configuration used to connect to a cluster for registration.
--
-- 'createdAt', 'cluster_createdAt' - The Unix epoch timestamp in seconds for when the cluster was created.
--
-- 'encryptionConfig', 'cluster_encryptionConfig' - The encryption configuration for the cluster.
--
-- 'endpoint', 'cluster_endpoint' - The endpoint for your Kubernetes API server.
--
-- 'health', 'cluster_health' - An object representing the health of your local Amazon EKS cluster on an
-- Amazon Web Services Outpost. This object isn\'t available for clusters
-- on the Amazon Web Services cloud.
--
-- 'id', 'cluster_id' - The ID of your local Amazon EKS cluster on an Amazon Web Services
-- Outpost. This property isn\'t available for an Amazon EKS cluster on the
-- Amazon Web Services cloud.
--
-- 'identity', 'cluster_identity' - The identity provider information for the cluster.
--
-- 'kubernetesNetworkConfig', 'cluster_kubernetesNetworkConfig' - The Kubernetes network configuration for the cluster.
--
-- 'logging', 'cluster_logging' - The logging configuration for your cluster.
--
-- 'name', 'cluster_name' - The name of the cluster.
--
-- 'outpostConfig', 'cluster_outpostConfig' - An object representing the configuration of your local Amazon EKS
-- cluster on an Amazon Web Services Outpost. This object isn\'t available
-- for clusters on the Amazon Web Services cloud.
--
-- 'platformVersion', 'cluster_platformVersion' - The platform version of your Amazon EKS cluster. For more information,
-- see
-- <https://docs.aws.amazon.com/eks/latest/userguide/platform-versions.html Platform Versions>
-- in the //Amazon EKS User Guide// .
--
-- 'resourcesVpcConfig', 'cluster_resourcesVpcConfig' - The VPC configuration used by the cluster control plane. Amazon EKS VPC
-- resources have specific requirements to work properly with Kubernetes.
-- For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/network_reqs.html Cluster VPC Considerations>
-- and
-- <https://docs.aws.amazon.com/eks/latest/userguide/sec-group-reqs.html Cluster Security Group Considerations>
-- in the /Amazon EKS User Guide/.
--
-- 'roleArn', 'cluster_roleArn' - The Amazon Resource Name (ARN) of the IAM role that provides permissions
-- for the Kubernetes control plane to make calls to Amazon Web Services
-- API operations on your behalf.
--
-- 'status', 'cluster_status' - The current status of the cluster.
--
-- 'tags', 'cluster_tags' - The metadata that you apply to the cluster to assist with categorization
-- and organization. Each tag consists of a key and an optional value. You
-- define both. Cluster tags do not propagate to any other resources
-- associated with the cluster.
--
-- 'version', 'cluster_version' - The Kubernetes server version for the cluster.
newCluster ::
  Cluster
newCluster =
  Cluster'
    { arn = Prelude.Nothing,
      certificateAuthority = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      connectorConfig = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      encryptionConfig = Prelude.Nothing,
      endpoint = Prelude.Nothing,
      health = Prelude.Nothing,
      id = Prelude.Nothing,
      identity = Prelude.Nothing,
      kubernetesNetworkConfig = Prelude.Nothing,
      logging = Prelude.Nothing,
      name = Prelude.Nothing,
      outpostConfig = Prelude.Nothing,
      platformVersion = Prelude.Nothing,
      resourcesVpcConfig = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      status = Prelude.Nothing,
      tags = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the cluster.
cluster_arn :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_arn = Lens.lens (\Cluster' {arn} -> arn) (\s@Cluster' {} a -> s {arn = a} :: Cluster)

-- | The @certificate-authority-data@ for your cluster.
cluster_certificateAuthority :: Lens.Lens' Cluster (Prelude.Maybe Certificate)
cluster_certificateAuthority = Lens.lens (\Cluster' {certificateAuthority} -> certificateAuthority) (\s@Cluster' {} a -> s {certificateAuthority = a} :: Cluster)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
cluster_clientRequestToken :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_clientRequestToken = Lens.lens (\Cluster' {clientRequestToken} -> clientRequestToken) (\s@Cluster' {} a -> s {clientRequestToken = a} :: Cluster)

-- | The configuration used to connect to a cluster for registration.
cluster_connectorConfig :: Lens.Lens' Cluster (Prelude.Maybe ConnectorConfigResponse)
cluster_connectorConfig = Lens.lens (\Cluster' {connectorConfig} -> connectorConfig) (\s@Cluster' {} a -> s {connectorConfig = a} :: Cluster)

-- | The Unix epoch timestamp in seconds for when the cluster was created.
cluster_createdAt :: Lens.Lens' Cluster (Prelude.Maybe Prelude.UTCTime)
cluster_createdAt = Lens.lens (\Cluster' {createdAt} -> createdAt) (\s@Cluster' {} a -> s {createdAt = a} :: Cluster) Prelude.. Lens.mapping Data._Time

-- | The encryption configuration for the cluster.
cluster_encryptionConfig :: Lens.Lens' Cluster (Prelude.Maybe [EncryptionConfig])
cluster_encryptionConfig = Lens.lens (\Cluster' {encryptionConfig} -> encryptionConfig) (\s@Cluster' {} a -> s {encryptionConfig = a} :: Cluster) Prelude.. Lens.mapping Lens.coerced

-- | The endpoint for your Kubernetes API server.
cluster_endpoint :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_endpoint = Lens.lens (\Cluster' {endpoint} -> endpoint) (\s@Cluster' {} a -> s {endpoint = a} :: Cluster)

-- | An object representing the health of your local Amazon EKS cluster on an
-- Amazon Web Services Outpost. This object isn\'t available for clusters
-- on the Amazon Web Services cloud.
cluster_health :: Lens.Lens' Cluster (Prelude.Maybe ClusterHealth)
cluster_health = Lens.lens (\Cluster' {health} -> health) (\s@Cluster' {} a -> s {health = a} :: Cluster)

-- | The ID of your local Amazon EKS cluster on an Amazon Web Services
-- Outpost. This property isn\'t available for an Amazon EKS cluster on the
-- Amazon Web Services cloud.
cluster_id :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_id = Lens.lens (\Cluster' {id} -> id) (\s@Cluster' {} a -> s {id = a} :: Cluster)

-- | The identity provider information for the cluster.
cluster_identity :: Lens.Lens' Cluster (Prelude.Maybe Identity)
cluster_identity = Lens.lens (\Cluster' {identity} -> identity) (\s@Cluster' {} a -> s {identity = a} :: Cluster)

-- | The Kubernetes network configuration for the cluster.
cluster_kubernetesNetworkConfig :: Lens.Lens' Cluster (Prelude.Maybe KubernetesNetworkConfigResponse)
cluster_kubernetesNetworkConfig = Lens.lens (\Cluster' {kubernetesNetworkConfig} -> kubernetesNetworkConfig) (\s@Cluster' {} a -> s {kubernetesNetworkConfig = a} :: Cluster)

-- | The logging configuration for your cluster.
cluster_logging :: Lens.Lens' Cluster (Prelude.Maybe Logging)
cluster_logging = Lens.lens (\Cluster' {logging} -> logging) (\s@Cluster' {} a -> s {logging = a} :: Cluster)

-- | The name of the cluster.
cluster_name :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_name = Lens.lens (\Cluster' {name} -> name) (\s@Cluster' {} a -> s {name = a} :: Cluster)

-- | An object representing the configuration of your local Amazon EKS
-- cluster on an Amazon Web Services Outpost. This object isn\'t available
-- for clusters on the Amazon Web Services cloud.
cluster_outpostConfig :: Lens.Lens' Cluster (Prelude.Maybe OutpostConfigResponse)
cluster_outpostConfig = Lens.lens (\Cluster' {outpostConfig} -> outpostConfig) (\s@Cluster' {} a -> s {outpostConfig = a} :: Cluster)

-- | The platform version of your Amazon EKS cluster. For more information,
-- see
-- <https://docs.aws.amazon.com/eks/latest/userguide/platform-versions.html Platform Versions>
-- in the //Amazon EKS User Guide// .
cluster_platformVersion :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_platformVersion = Lens.lens (\Cluster' {platformVersion} -> platformVersion) (\s@Cluster' {} a -> s {platformVersion = a} :: Cluster)

-- | The VPC configuration used by the cluster control plane. Amazon EKS VPC
-- resources have specific requirements to work properly with Kubernetes.
-- For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/network_reqs.html Cluster VPC Considerations>
-- and
-- <https://docs.aws.amazon.com/eks/latest/userguide/sec-group-reqs.html Cluster Security Group Considerations>
-- in the /Amazon EKS User Guide/.
cluster_resourcesVpcConfig :: Lens.Lens' Cluster (Prelude.Maybe VpcConfigResponse)
cluster_resourcesVpcConfig = Lens.lens (\Cluster' {resourcesVpcConfig} -> resourcesVpcConfig) (\s@Cluster' {} a -> s {resourcesVpcConfig = a} :: Cluster)

-- | The Amazon Resource Name (ARN) of the IAM role that provides permissions
-- for the Kubernetes control plane to make calls to Amazon Web Services
-- API operations on your behalf.
cluster_roleArn :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_roleArn = Lens.lens (\Cluster' {roleArn} -> roleArn) (\s@Cluster' {} a -> s {roleArn = a} :: Cluster)

-- | The current status of the cluster.
cluster_status :: Lens.Lens' Cluster (Prelude.Maybe ClusterStatus)
cluster_status = Lens.lens (\Cluster' {status} -> status) (\s@Cluster' {} a -> s {status = a} :: Cluster)

-- | The metadata that you apply to the cluster to assist with categorization
-- and organization. Each tag consists of a key and an optional value. You
-- define both. Cluster tags do not propagate to any other resources
-- associated with the cluster.
cluster_tags :: Lens.Lens' Cluster (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
cluster_tags = Lens.lens (\Cluster' {tags} -> tags) (\s@Cluster' {} a -> s {tags = a} :: Cluster) Prelude.. Lens.mapping Lens.coerced

-- | The Kubernetes server version for the cluster.
cluster_version :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_version = Lens.lens (\Cluster' {version} -> version) (\s@Cluster' {} a -> s {version = a} :: Cluster)

instance Data.FromJSON Cluster where
  parseJSON =
    Data.withObject
      "Cluster"
      ( \x ->
          Cluster'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "certificateAuthority")
            Prelude.<*> (x Data..:? "clientRequestToken")
            Prelude.<*> (x Data..:? "connectorConfig")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> ( x Data..:? "encryptionConfig"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "endpoint")
            Prelude.<*> (x Data..:? "health")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "identity")
            Prelude.<*> (x Data..:? "kubernetesNetworkConfig")
            Prelude.<*> (x Data..:? "logging")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "outpostConfig")
            Prelude.<*> (x Data..:? "platformVersion")
            Prelude.<*> (x Data..:? "resourcesVpcConfig")
            Prelude.<*> (x Data..:? "roleArn")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "version")
      )

instance Prelude.Hashable Cluster where
  hashWithSalt _salt Cluster' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` certificateAuthority
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` connectorConfig
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` encryptionConfig
      `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` health
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` identity
      `Prelude.hashWithSalt` kubernetesNetworkConfig
      `Prelude.hashWithSalt` logging
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` outpostConfig
      `Prelude.hashWithSalt` platformVersion
      `Prelude.hashWithSalt` resourcesVpcConfig
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` version

instance Prelude.NFData Cluster where
  rnf Cluster' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf certificateAuthority
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf connectorConfig
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf encryptionConfig
      `Prelude.seq` Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf health
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf identity
      `Prelude.seq` Prelude.rnf kubernetesNetworkConfig
      `Prelude.seq` Prelude.rnf logging
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf outpostConfig
      `Prelude.seq` Prelude.rnf platformVersion
      `Prelude.seq` Prelude.rnf resourcesVpcConfig
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf version
