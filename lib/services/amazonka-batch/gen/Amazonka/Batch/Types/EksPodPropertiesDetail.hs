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
-- Module      : Amazonka.Batch.Types.EksPodPropertiesDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.EksPodPropertiesDetail where

import Amazonka.Batch.Types.EksContainerDetail
import Amazonka.Batch.Types.EksVolume
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The details for the pod.
--
-- /See:/ 'newEksPodPropertiesDetail' smart constructor.
data EksPodPropertiesDetail = EksPodPropertiesDetail'
  { -- | The properties of the container that\'s used on the Amazon EKS pod.
    containers :: Prelude.Maybe [EksContainerDetail],
    -- | The name of the service account that\'s used to run the pod. For more
    -- information, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/service-accounts.html Kubernetes service accounts>
    -- and
    -- <https://docs.aws.amazon.com/eks/latest/userguide/associate-service-account-role.html Configure a Kubernetes service account to assume an IAM role>
    -- in the /Amazon EKS User Guide/ and
    -- <https://kubernetes.io/docs/tasks/configure-pod-container/configure-service-account/ Configure service accounts for pods>
    -- in the /Kubernetes documentation/.
    serviceAccountName :: Prelude.Maybe Prelude.Text,
    -- | The name of the pod for this job.
    podName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the volumes for a job definition using Amazon EKS resources.
    volumes :: Prelude.Maybe [EksVolume],
    -- | The name of the node for this job.
    nodeName :: Prelude.Maybe Prelude.Text,
    -- | The DNS policy for the pod. The default value is @ClusterFirst@. If the
    -- @hostNetwork@ parameter is not specified, the default is
    -- @ClusterFirstWithHostNet@. @ClusterFirst@ indicates that any DNS query
    -- that does not match the configured cluster domain suffix is forwarded to
    -- the upstream nameserver inherited from the node. If no value was
    -- specified for @dnsPolicy@ in the
    -- <https://docs.aws.amazon.com/batch/latest/APIReference/API_RegisterJobDefinition.html RegisterJobDefinition>
    -- API operation, then no value will be returned for @dnsPolicy@ by either
    -- of
    -- <https://docs.aws.amazon.com/batch/latest/APIReference/API_DescribeJobDefinitions.html DescribeJobDefinitions>
    -- or
    -- <https://docs.aws.amazon.com/batch/latest/APIReference/API_DescribeJobs.html DescribeJobs>
    -- API operations. The pod spec setting will contain either @ClusterFirst@
    -- or @ClusterFirstWithHostNet@, depending on the value of the
    -- @hostNetwork@ parameter. For more information, see
    -- <https://kubernetes.io/docs/concepts/services-networking/dns-pod-service/#pod-s-dns-policy Pod\'s DNS policy>
    -- in the /Kubernetes documentation/.
    --
    -- Valid values: @Default@ | @ClusterFirst@ | @ClusterFirstWithHostNet@
    dnsPolicy :: Prelude.Maybe Prelude.Text,
    -- | Indicates if the pod uses the hosts\' network IP address. The default
    -- value is @true@. Setting this to @false@ enables the Kubernetes pod
    -- networking model. Most Batch workloads are egress-only and don\'t
    -- require the overhead of IP allocation for each pod for incoming
    -- connections. For more information, see
    -- <https://kubernetes.io/docs/concepts/security/pod-security-policy/#host-namespaces Host namespaces>
    -- and
    -- <https://kubernetes.io/docs/concepts/workloads/pods/#pod-networking Pod networking>
    -- in the /Kubernetes documentation/.
    hostNetwork :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EksPodPropertiesDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containers', 'eksPodPropertiesDetail_containers' - The properties of the container that\'s used on the Amazon EKS pod.
--
-- 'serviceAccountName', 'eksPodPropertiesDetail_serviceAccountName' - The name of the service account that\'s used to run the pod. For more
-- information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/service-accounts.html Kubernetes service accounts>
-- and
-- <https://docs.aws.amazon.com/eks/latest/userguide/associate-service-account-role.html Configure a Kubernetes service account to assume an IAM role>
-- in the /Amazon EKS User Guide/ and
-- <https://kubernetes.io/docs/tasks/configure-pod-container/configure-service-account/ Configure service accounts for pods>
-- in the /Kubernetes documentation/.
--
-- 'podName', 'eksPodPropertiesDetail_podName' - The name of the pod for this job.
--
-- 'volumes', 'eksPodPropertiesDetail_volumes' - Specifies the volumes for a job definition using Amazon EKS resources.
--
-- 'nodeName', 'eksPodPropertiesDetail_nodeName' - The name of the node for this job.
--
-- 'dnsPolicy', 'eksPodPropertiesDetail_dnsPolicy' - The DNS policy for the pod. The default value is @ClusterFirst@. If the
-- @hostNetwork@ parameter is not specified, the default is
-- @ClusterFirstWithHostNet@. @ClusterFirst@ indicates that any DNS query
-- that does not match the configured cluster domain suffix is forwarded to
-- the upstream nameserver inherited from the node. If no value was
-- specified for @dnsPolicy@ in the
-- <https://docs.aws.amazon.com/batch/latest/APIReference/API_RegisterJobDefinition.html RegisterJobDefinition>
-- API operation, then no value will be returned for @dnsPolicy@ by either
-- of
-- <https://docs.aws.amazon.com/batch/latest/APIReference/API_DescribeJobDefinitions.html DescribeJobDefinitions>
-- or
-- <https://docs.aws.amazon.com/batch/latest/APIReference/API_DescribeJobs.html DescribeJobs>
-- API operations. The pod spec setting will contain either @ClusterFirst@
-- or @ClusterFirstWithHostNet@, depending on the value of the
-- @hostNetwork@ parameter. For more information, see
-- <https://kubernetes.io/docs/concepts/services-networking/dns-pod-service/#pod-s-dns-policy Pod\'s DNS policy>
-- in the /Kubernetes documentation/.
--
-- Valid values: @Default@ | @ClusterFirst@ | @ClusterFirstWithHostNet@
--
-- 'hostNetwork', 'eksPodPropertiesDetail_hostNetwork' - Indicates if the pod uses the hosts\' network IP address. The default
-- value is @true@. Setting this to @false@ enables the Kubernetes pod
-- networking model. Most Batch workloads are egress-only and don\'t
-- require the overhead of IP allocation for each pod for incoming
-- connections. For more information, see
-- <https://kubernetes.io/docs/concepts/security/pod-security-policy/#host-namespaces Host namespaces>
-- and
-- <https://kubernetes.io/docs/concepts/workloads/pods/#pod-networking Pod networking>
-- in the /Kubernetes documentation/.
newEksPodPropertiesDetail ::
  EksPodPropertiesDetail
newEksPodPropertiesDetail =
  EksPodPropertiesDetail'
    { containers =
        Prelude.Nothing,
      serviceAccountName = Prelude.Nothing,
      podName = Prelude.Nothing,
      volumes = Prelude.Nothing,
      nodeName = Prelude.Nothing,
      dnsPolicy = Prelude.Nothing,
      hostNetwork = Prelude.Nothing
    }

-- | The properties of the container that\'s used on the Amazon EKS pod.
eksPodPropertiesDetail_containers :: Lens.Lens' EksPodPropertiesDetail (Prelude.Maybe [EksContainerDetail])
eksPodPropertiesDetail_containers = Lens.lens (\EksPodPropertiesDetail' {containers} -> containers) (\s@EksPodPropertiesDetail' {} a -> s {containers = a} :: EksPodPropertiesDetail) Prelude.. Lens.mapping Lens.coerced

-- | The name of the service account that\'s used to run the pod. For more
-- information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/service-accounts.html Kubernetes service accounts>
-- and
-- <https://docs.aws.amazon.com/eks/latest/userguide/associate-service-account-role.html Configure a Kubernetes service account to assume an IAM role>
-- in the /Amazon EKS User Guide/ and
-- <https://kubernetes.io/docs/tasks/configure-pod-container/configure-service-account/ Configure service accounts for pods>
-- in the /Kubernetes documentation/.
eksPodPropertiesDetail_serviceAccountName :: Lens.Lens' EksPodPropertiesDetail (Prelude.Maybe Prelude.Text)
eksPodPropertiesDetail_serviceAccountName = Lens.lens (\EksPodPropertiesDetail' {serviceAccountName} -> serviceAccountName) (\s@EksPodPropertiesDetail' {} a -> s {serviceAccountName = a} :: EksPodPropertiesDetail)

-- | The name of the pod for this job.
eksPodPropertiesDetail_podName :: Lens.Lens' EksPodPropertiesDetail (Prelude.Maybe Prelude.Text)
eksPodPropertiesDetail_podName = Lens.lens (\EksPodPropertiesDetail' {podName} -> podName) (\s@EksPodPropertiesDetail' {} a -> s {podName = a} :: EksPodPropertiesDetail)

-- | Specifies the volumes for a job definition using Amazon EKS resources.
eksPodPropertiesDetail_volumes :: Lens.Lens' EksPodPropertiesDetail (Prelude.Maybe [EksVolume])
eksPodPropertiesDetail_volumes = Lens.lens (\EksPodPropertiesDetail' {volumes} -> volumes) (\s@EksPodPropertiesDetail' {} a -> s {volumes = a} :: EksPodPropertiesDetail) Prelude.. Lens.mapping Lens.coerced

-- | The name of the node for this job.
eksPodPropertiesDetail_nodeName :: Lens.Lens' EksPodPropertiesDetail (Prelude.Maybe Prelude.Text)
eksPodPropertiesDetail_nodeName = Lens.lens (\EksPodPropertiesDetail' {nodeName} -> nodeName) (\s@EksPodPropertiesDetail' {} a -> s {nodeName = a} :: EksPodPropertiesDetail)

-- | The DNS policy for the pod. The default value is @ClusterFirst@. If the
-- @hostNetwork@ parameter is not specified, the default is
-- @ClusterFirstWithHostNet@. @ClusterFirst@ indicates that any DNS query
-- that does not match the configured cluster domain suffix is forwarded to
-- the upstream nameserver inherited from the node. If no value was
-- specified for @dnsPolicy@ in the
-- <https://docs.aws.amazon.com/batch/latest/APIReference/API_RegisterJobDefinition.html RegisterJobDefinition>
-- API operation, then no value will be returned for @dnsPolicy@ by either
-- of
-- <https://docs.aws.amazon.com/batch/latest/APIReference/API_DescribeJobDefinitions.html DescribeJobDefinitions>
-- or
-- <https://docs.aws.amazon.com/batch/latest/APIReference/API_DescribeJobs.html DescribeJobs>
-- API operations. The pod spec setting will contain either @ClusterFirst@
-- or @ClusterFirstWithHostNet@, depending on the value of the
-- @hostNetwork@ parameter. For more information, see
-- <https://kubernetes.io/docs/concepts/services-networking/dns-pod-service/#pod-s-dns-policy Pod\'s DNS policy>
-- in the /Kubernetes documentation/.
--
-- Valid values: @Default@ | @ClusterFirst@ | @ClusterFirstWithHostNet@
eksPodPropertiesDetail_dnsPolicy :: Lens.Lens' EksPodPropertiesDetail (Prelude.Maybe Prelude.Text)
eksPodPropertiesDetail_dnsPolicy = Lens.lens (\EksPodPropertiesDetail' {dnsPolicy} -> dnsPolicy) (\s@EksPodPropertiesDetail' {} a -> s {dnsPolicy = a} :: EksPodPropertiesDetail)

-- | Indicates if the pod uses the hosts\' network IP address. The default
-- value is @true@. Setting this to @false@ enables the Kubernetes pod
-- networking model. Most Batch workloads are egress-only and don\'t
-- require the overhead of IP allocation for each pod for incoming
-- connections. For more information, see
-- <https://kubernetes.io/docs/concepts/security/pod-security-policy/#host-namespaces Host namespaces>
-- and
-- <https://kubernetes.io/docs/concepts/workloads/pods/#pod-networking Pod networking>
-- in the /Kubernetes documentation/.
eksPodPropertiesDetail_hostNetwork :: Lens.Lens' EksPodPropertiesDetail (Prelude.Maybe Prelude.Bool)
eksPodPropertiesDetail_hostNetwork = Lens.lens (\EksPodPropertiesDetail' {hostNetwork} -> hostNetwork) (\s@EksPodPropertiesDetail' {} a -> s {hostNetwork = a} :: EksPodPropertiesDetail)

instance Core.FromJSON EksPodPropertiesDetail where
  parseJSON =
    Core.withObject
      "EksPodPropertiesDetail"
      ( \x ->
          EksPodPropertiesDetail'
            Prelude.<$> (x Core..:? "containers" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "serviceAccountName")
            Prelude.<*> (x Core..:? "podName")
            Prelude.<*> (x Core..:? "volumes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "nodeName")
            Prelude.<*> (x Core..:? "dnsPolicy")
            Prelude.<*> (x Core..:? "hostNetwork")
      )

instance Prelude.Hashable EksPodPropertiesDetail where
  hashWithSalt _salt EksPodPropertiesDetail' {..} =
    _salt `Prelude.hashWithSalt` containers
      `Prelude.hashWithSalt` serviceAccountName
      `Prelude.hashWithSalt` podName
      `Prelude.hashWithSalt` volumes
      `Prelude.hashWithSalt` nodeName
      `Prelude.hashWithSalt` dnsPolicy
      `Prelude.hashWithSalt` hostNetwork

instance Prelude.NFData EksPodPropertiesDetail where
  rnf EksPodPropertiesDetail' {..} =
    Prelude.rnf containers
      `Prelude.seq` Prelude.rnf serviceAccountName
      `Prelude.seq` Prelude.rnf podName
      `Prelude.seq` Prelude.rnf volumes
      `Prelude.seq` Prelude.rnf nodeName
      `Prelude.seq` Prelude.rnf dnsPolicy
      `Prelude.seq` Prelude.rnf hostNetwork
