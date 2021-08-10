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
-- Module      : Network.AWS.EKS.Types.Cluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.Cluster where

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.Types.Certificate
import Network.AWS.EKS.Types.ClusterStatus
import Network.AWS.EKS.Types.EncryptionConfig
import Network.AWS.EKS.Types.Identity
import Network.AWS.EKS.Types.KubernetesNetworkConfigResponse
import Network.AWS.EKS.Types.Logging
import Network.AWS.EKS.Types.VpcConfigResponse
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing an Amazon EKS cluster.
--
-- /See:/ 'newCluster' smart constructor.
data Cluster = Cluster'
  { -- | The current status of the cluster.
    status :: Prelude.Maybe ClusterStatus,
    -- | The Amazon Resource Name (ARN) of the IAM role that provides permissions
    -- for the Kubernetes control plane to make calls to AWS API operations on
    -- your behalf.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The VPC configuration used by the cluster control plane. Amazon EKS VPC
    -- resources have specific requirements to work properly with Kubernetes.
    -- For more information, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/network_reqs.html Cluster VPC Considerations>
    -- and
    -- <https://docs.aws.amazon.com/eks/latest/userguide/sec-group-reqs.html Cluster Security Group Considerations>
    -- in the /Amazon EKS User Guide/.
    resourcesVpcConfig :: Prelude.Maybe VpcConfigResponse,
    -- | The Kubernetes network configuration for the cluster.
    kubernetesNetworkConfig :: Prelude.Maybe KubernetesNetworkConfigResponse,
    -- | The identity provider information for the cluster.
    identity :: Prelude.Maybe Identity,
    -- | The logging configuration for your cluster.
    logging :: Prelude.Maybe Logging,
    -- | The Unix epoch timestamp in seconds for when the cluster was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The platform version of your Amazon EKS cluster. For more information,
    -- see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/platform-versions.html Platform Versions>
    -- in the //Amazon EKS User Guide// .
    platformVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the cluster.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The encryption configuration for the cluster.
    encryptionConfig :: Prelude.Maybe [EncryptionConfig],
    -- | The Kubernetes server version for the cluster.
    version :: Prelude.Maybe Prelude.Text,
    -- | The name of the cluster.
    name :: Prelude.Maybe Prelude.Text,
    -- | The @certificate-authority-data@ for your cluster.
    certificateAuthority :: Prelude.Maybe Certificate,
    -- | The metadata that you apply to the cluster to assist with categorization
    -- and organization. Each tag consists of a key and an optional value, both
    -- of which you define. Cluster tags do not propagate to any other
    -- resources associated with the cluster.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The endpoint for your Kubernetes API server.
    endpoint :: Prelude.Maybe Prelude.Text,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientRequestToken :: Prelude.Maybe Prelude.Text
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
-- 'status', 'cluster_status' - The current status of the cluster.
--
-- 'roleArn', 'cluster_roleArn' - The Amazon Resource Name (ARN) of the IAM role that provides permissions
-- for the Kubernetes control plane to make calls to AWS API operations on
-- your behalf.
--
-- 'resourcesVpcConfig', 'cluster_resourcesVpcConfig' - The VPC configuration used by the cluster control plane. Amazon EKS VPC
-- resources have specific requirements to work properly with Kubernetes.
-- For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/network_reqs.html Cluster VPC Considerations>
-- and
-- <https://docs.aws.amazon.com/eks/latest/userguide/sec-group-reqs.html Cluster Security Group Considerations>
-- in the /Amazon EKS User Guide/.
--
-- 'kubernetesNetworkConfig', 'cluster_kubernetesNetworkConfig' - The Kubernetes network configuration for the cluster.
--
-- 'identity', 'cluster_identity' - The identity provider information for the cluster.
--
-- 'logging', 'cluster_logging' - The logging configuration for your cluster.
--
-- 'createdAt', 'cluster_createdAt' - The Unix epoch timestamp in seconds for when the cluster was created.
--
-- 'platformVersion', 'cluster_platformVersion' - The platform version of your Amazon EKS cluster. For more information,
-- see
-- <https://docs.aws.amazon.com/eks/latest/userguide/platform-versions.html Platform Versions>
-- in the //Amazon EKS User Guide// .
--
-- 'arn', 'cluster_arn' - The Amazon Resource Name (ARN) of the cluster.
--
-- 'encryptionConfig', 'cluster_encryptionConfig' - The encryption configuration for the cluster.
--
-- 'version', 'cluster_version' - The Kubernetes server version for the cluster.
--
-- 'name', 'cluster_name' - The name of the cluster.
--
-- 'certificateAuthority', 'cluster_certificateAuthority' - The @certificate-authority-data@ for your cluster.
--
-- 'tags', 'cluster_tags' - The metadata that you apply to the cluster to assist with categorization
-- and organization. Each tag consists of a key and an optional value, both
-- of which you define. Cluster tags do not propagate to any other
-- resources associated with the cluster.
--
-- 'endpoint', 'cluster_endpoint' - The endpoint for your Kubernetes API server.
--
-- 'clientRequestToken', 'cluster_clientRequestToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
newCluster ::
  Cluster
newCluster =
  Cluster'
    { status = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      resourcesVpcConfig = Prelude.Nothing,
      kubernetesNetworkConfig = Prelude.Nothing,
      identity = Prelude.Nothing,
      logging = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      platformVersion = Prelude.Nothing,
      arn = Prelude.Nothing,
      encryptionConfig = Prelude.Nothing,
      version = Prelude.Nothing,
      name = Prelude.Nothing,
      certificateAuthority = Prelude.Nothing,
      tags = Prelude.Nothing,
      endpoint = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing
    }

-- | The current status of the cluster.
cluster_status :: Lens.Lens' Cluster (Prelude.Maybe ClusterStatus)
cluster_status = Lens.lens (\Cluster' {status} -> status) (\s@Cluster' {} a -> s {status = a} :: Cluster)

-- | The Amazon Resource Name (ARN) of the IAM role that provides permissions
-- for the Kubernetes control plane to make calls to AWS API operations on
-- your behalf.
cluster_roleArn :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_roleArn = Lens.lens (\Cluster' {roleArn} -> roleArn) (\s@Cluster' {} a -> s {roleArn = a} :: Cluster)

-- | The VPC configuration used by the cluster control plane. Amazon EKS VPC
-- resources have specific requirements to work properly with Kubernetes.
-- For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/network_reqs.html Cluster VPC Considerations>
-- and
-- <https://docs.aws.amazon.com/eks/latest/userguide/sec-group-reqs.html Cluster Security Group Considerations>
-- in the /Amazon EKS User Guide/.
cluster_resourcesVpcConfig :: Lens.Lens' Cluster (Prelude.Maybe VpcConfigResponse)
cluster_resourcesVpcConfig = Lens.lens (\Cluster' {resourcesVpcConfig} -> resourcesVpcConfig) (\s@Cluster' {} a -> s {resourcesVpcConfig = a} :: Cluster)

-- | The Kubernetes network configuration for the cluster.
cluster_kubernetesNetworkConfig :: Lens.Lens' Cluster (Prelude.Maybe KubernetesNetworkConfigResponse)
cluster_kubernetesNetworkConfig = Lens.lens (\Cluster' {kubernetesNetworkConfig} -> kubernetesNetworkConfig) (\s@Cluster' {} a -> s {kubernetesNetworkConfig = a} :: Cluster)

-- | The identity provider information for the cluster.
cluster_identity :: Lens.Lens' Cluster (Prelude.Maybe Identity)
cluster_identity = Lens.lens (\Cluster' {identity} -> identity) (\s@Cluster' {} a -> s {identity = a} :: Cluster)

-- | The logging configuration for your cluster.
cluster_logging :: Lens.Lens' Cluster (Prelude.Maybe Logging)
cluster_logging = Lens.lens (\Cluster' {logging} -> logging) (\s@Cluster' {} a -> s {logging = a} :: Cluster)

-- | The Unix epoch timestamp in seconds for when the cluster was created.
cluster_createdAt :: Lens.Lens' Cluster (Prelude.Maybe Prelude.UTCTime)
cluster_createdAt = Lens.lens (\Cluster' {createdAt} -> createdAt) (\s@Cluster' {} a -> s {createdAt = a} :: Cluster) Prelude.. Lens.mapping Core._Time

-- | The platform version of your Amazon EKS cluster. For more information,
-- see
-- <https://docs.aws.amazon.com/eks/latest/userguide/platform-versions.html Platform Versions>
-- in the //Amazon EKS User Guide// .
cluster_platformVersion :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_platformVersion = Lens.lens (\Cluster' {platformVersion} -> platformVersion) (\s@Cluster' {} a -> s {platformVersion = a} :: Cluster)

-- | The Amazon Resource Name (ARN) of the cluster.
cluster_arn :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_arn = Lens.lens (\Cluster' {arn} -> arn) (\s@Cluster' {} a -> s {arn = a} :: Cluster)

-- | The encryption configuration for the cluster.
cluster_encryptionConfig :: Lens.Lens' Cluster (Prelude.Maybe [EncryptionConfig])
cluster_encryptionConfig = Lens.lens (\Cluster' {encryptionConfig} -> encryptionConfig) (\s@Cluster' {} a -> s {encryptionConfig = a} :: Cluster) Prelude.. Lens.mapping Lens._Coerce

-- | The Kubernetes server version for the cluster.
cluster_version :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_version = Lens.lens (\Cluster' {version} -> version) (\s@Cluster' {} a -> s {version = a} :: Cluster)

-- | The name of the cluster.
cluster_name :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_name = Lens.lens (\Cluster' {name} -> name) (\s@Cluster' {} a -> s {name = a} :: Cluster)

-- | The @certificate-authority-data@ for your cluster.
cluster_certificateAuthority :: Lens.Lens' Cluster (Prelude.Maybe Certificate)
cluster_certificateAuthority = Lens.lens (\Cluster' {certificateAuthority} -> certificateAuthority) (\s@Cluster' {} a -> s {certificateAuthority = a} :: Cluster)

-- | The metadata that you apply to the cluster to assist with categorization
-- and organization. Each tag consists of a key and an optional value, both
-- of which you define. Cluster tags do not propagate to any other
-- resources associated with the cluster.
cluster_tags :: Lens.Lens' Cluster (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
cluster_tags = Lens.lens (\Cluster' {tags} -> tags) (\s@Cluster' {} a -> s {tags = a} :: Cluster) Prelude.. Lens.mapping Lens._Coerce

-- | The endpoint for your Kubernetes API server.
cluster_endpoint :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_endpoint = Lens.lens (\Cluster' {endpoint} -> endpoint) (\s@Cluster' {} a -> s {endpoint = a} :: Cluster)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
cluster_clientRequestToken :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_clientRequestToken = Lens.lens (\Cluster' {clientRequestToken} -> clientRequestToken) (\s@Cluster' {} a -> s {clientRequestToken = a} :: Cluster)

instance Core.FromJSON Cluster where
  parseJSON =
    Core.withObject
      "Cluster"
      ( \x ->
          Cluster'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "roleArn")
            Prelude.<*> (x Core..:? "resourcesVpcConfig")
            Prelude.<*> (x Core..:? "kubernetesNetworkConfig")
            Prelude.<*> (x Core..:? "identity")
            Prelude.<*> (x Core..:? "logging")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "platformVersion")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> ( x Core..:? "encryptionConfig"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "version")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "certificateAuthority")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "endpoint")
            Prelude.<*> (x Core..:? "clientRequestToken")
      )

instance Prelude.Hashable Cluster

instance Prelude.NFData Cluster
