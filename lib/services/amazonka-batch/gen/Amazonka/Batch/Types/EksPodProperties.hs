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
-- Module      : Amazonka.Batch.Types.EksPodProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.EksPodProperties where

import Amazonka.Batch.Types.EksContainer
import Amazonka.Batch.Types.EksVolume
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The properties for the pod.
--
-- /See:/ 'newEksPodProperties' smart constructor.
data EksPodProperties = EksPodProperties'
  { -- | The properties of the container that\'s used on the Amazon EKS pod.
    containers :: Prelude.Maybe [EksContainer],
    -- | The DNS policy for the pod. The default value is @ClusterFirst@. If the
    -- @hostNetwork@ parameter is not specified, the default is
    -- @ClusterFirstWithHostNet@. @ClusterFirst@ indicates that any DNS query
    -- that does not match the configured cluster domain suffix is forwarded to
    -- the upstream nameserver inherited from the node. For more information,
    -- see
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
    hostNetwork :: Prelude.Maybe Prelude.Bool,
    -- | The name of the service account that\'s used to run the pod. For more
    -- information, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/service-accounts.html Kubernetes service accounts>
    -- and
    -- <https://docs.aws.amazon.com/eks/latest/userguide/associate-service-account-role.html Configure a Kubernetes service account to assume an IAM role>
    -- in the /Amazon EKS User Guide/ and
    -- <https://kubernetes.io/docs/tasks/configure-pod-container/configure-service-account/ Configure service accounts for pods>
    -- in the /Kubernetes documentation/.
    serviceAccountName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the volumes for a job definition that uses Amazon EKS
    -- resources.
    volumes :: Prelude.Maybe [EksVolume]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EksPodProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containers', 'eksPodProperties_containers' - The properties of the container that\'s used on the Amazon EKS pod.
--
-- 'dnsPolicy', 'eksPodProperties_dnsPolicy' - The DNS policy for the pod. The default value is @ClusterFirst@. If the
-- @hostNetwork@ parameter is not specified, the default is
-- @ClusterFirstWithHostNet@. @ClusterFirst@ indicates that any DNS query
-- that does not match the configured cluster domain suffix is forwarded to
-- the upstream nameserver inherited from the node. For more information,
-- see
-- <https://kubernetes.io/docs/concepts/services-networking/dns-pod-service/#pod-s-dns-policy Pod\'s DNS policy>
-- in the /Kubernetes documentation/.
--
-- Valid values: @Default@ | @ClusterFirst@ | @ClusterFirstWithHostNet@
--
-- 'hostNetwork', 'eksPodProperties_hostNetwork' - Indicates if the pod uses the hosts\' network IP address. The default
-- value is @true@. Setting this to @false@ enables the Kubernetes pod
-- networking model. Most Batch workloads are egress-only and don\'t
-- require the overhead of IP allocation for each pod for incoming
-- connections. For more information, see
-- <https://kubernetes.io/docs/concepts/security/pod-security-policy/#host-namespaces Host namespaces>
-- and
-- <https://kubernetes.io/docs/concepts/workloads/pods/#pod-networking Pod networking>
-- in the /Kubernetes documentation/.
--
-- 'serviceAccountName', 'eksPodProperties_serviceAccountName' - The name of the service account that\'s used to run the pod. For more
-- information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/service-accounts.html Kubernetes service accounts>
-- and
-- <https://docs.aws.amazon.com/eks/latest/userguide/associate-service-account-role.html Configure a Kubernetes service account to assume an IAM role>
-- in the /Amazon EKS User Guide/ and
-- <https://kubernetes.io/docs/tasks/configure-pod-container/configure-service-account/ Configure service accounts for pods>
-- in the /Kubernetes documentation/.
--
-- 'volumes', 'eksPodProperties_volumes' - Specifies the volumes for a job definition that uses Amazon EKS
-- resources.
newEksPodProperties ::
  EksPodProperties
newEksPodProperties =
  EksPodProperties'
    { containers = Prelude.Nothing,
      dnsPolicy = Prelude.Nothing,
      hostNetwork = Prelude.Nothing,
      serviceAccountName = Prelude.Nothing,
      volumes = Prelude.Nothing
    }

-- | The properties of the container that\'s used on the Amazon EKS pod.
eksPodProperties_containers :: Lens.Lens' EksPodProperties (Prelude.Maybe [EksContainer])
eksPodProperties_containers = Lens.lens (\EksPodProperties' {containers} -> containers) (\s@EksPodProperties' {} a -> s {containers = a} :: EksPodProperties) Prelude.. Lens.mapping Lens.coerced

-- | The DNS policy for the pod. The default value is @ClusterFirst@. If the
-- @hostNetwork@ parameter is not specified, the default is
-- @ClusterFirstWithHostNet@. @ClusterFirst@ indicates that any DNS query
-- that does not match the configured cluster domain suffix is forwarded to
-- the upstream nameserver inherited from the node. For more information,
-- see
-- <https://kubernetes.io/docs/concepts/services-networking/dns-pod-service/#pod-s-dns-policy Pod\'s DNS policy>
-- in the /Kubernetes documentation/.
--
-- Valid values: @Default@ | @ClusterFirst@ | @ClusterFirstWithHostNet@
eksPodProperties_dnsPolicy :: Lens.Lens' EksPodProperties (Prelude.Maybe Prelude.Text)
eksPodProperties_dnsPolicy = Lens.lens (\EksPodProperties' {dnsPolicy} -> dnsPolicy) (\s@EksPodProperties' {} a -> s {dnsPolicy = a} :: EksPodProperties)

-- | Indicates if the pod uses the hosts\' network IP address. The default
-- value is @true@. Setting this to @false@ enables the Kubernetes pod
-- networking model. Most Batch workloads are egress-only and don\'t
-- require the overhead of IP allocation for each pod for incoming
-- connections. For more information, see
-- <https://kubernetes.io/docs/concepts/security/pod-security-policy/#host-namespaces Host namespaces>
-- and
-- <https://kubernetes.io/docs/concepts/workloads/pods/#pod-networking Pod networking>
-- in the /Kubernetes documentation/.
eksPodProperties_hostNetwork :: Lens.Lens' EksPodProperties (Prelude.Maybe Prelude.Bool)
eksPodProperties_hostNetwork = Lens.lens (\EksPodProperties' {hostNetwork} -> hostNetwork) (\s@EksPodProperties' {} a -> s {hostNetwork = a} :: EksPodProperties)

-- | The name of the service account that\'s used to run the pod. For more
-- information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/service-accounts.html Kubernetes service accounts>
-- and
-- <https://docs.aws.amazon.com/eks/latest/userguide/associate-service-account-role.html Configure a Kubernetes service account to assume an IAM role>
-- in the /Amazon EKS User Guide/ and
-- <https://kubernetes.io/docs/tasks/configure-pod-container/configure-service-account/ Configure service accounts for pods>
-- in the /Kubernetes documentation/.
eksPodProperties_serviceAccountName :: Lens.Lens' EksPodProperties (Prelude.Maybe Prelude.Text)
eksPodProperties_serviceAccountName = Lens.lens (\EksPodProperties' {serviceAccountName} -> serviceAccountName) (\s@EksPodProperties' {} a -> s {serviceAccountName = a} :: EksPodProperties)

-- | Specifies the volumes for a job definition that uses Amazon EKS
-- resources.
eksPodProperties_volumes :: Lens.Lens' EksPodProperties (Prelude.Maybe [EksVolume])
eksPodProperties_volumes = Lens.lens (\EksPodProperties' {volumes} -> volumes) (\s@EksPodProperties' {} a -> s {volumes = a} :: EksPodProperties) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON EksPodProperties where
  parseJSON =
    Data.withObject
      "EksPodProperties"
      ( \x ->
          EksPodProperties'
            Prelude.<$> (x Data..:? "containers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "dnsPolicy")
            Prelude.<*> (x Data..:? "hostNetwork")
            Prelude.<*> (x Data..:? "serviceAccountName")
            Prelude.<*> (x Data..:? "volumes" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable EksPodProperties where
  hashWithSalt _salt EksPodProperties' {..} =
    _salt `Prelude.hashWithSalt` containers
      `Prelude.hashWithSalt` dnsPolicy
      `Prelude.hashWithSalt` hostNetwork
      `Prelude.hashWithSalt` serviceAccountName
      `Prelude.hashWithSalt` volumes

instance Prelude.NFData EksPodProperties where
  rnf EksPodProperties' {..} =
    Prelude.rnf containers
      `Prelude.seq` Prelude.rnf dnsPolicy
      `Prelude.seq` Prelude.rnf hostNetwork
      `Prelude.seq` Prelude.rnf serviceAccountName
      `Prelude.seq` Prelude.rnf volumes

instance Data.ToJSON EksPodProperties where
  toJSON EksPodProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("containers" Data..=) Prelude.<$> containers,
            ("dnsPolicy" Data..=) Prelude.<$> dnsPolicy,
            ("hostNetwork" Data..=) Prelude.<$> hostNetwork,
            ("serviceAccountName" Data..=)
              Prelude.<$> serviceAccountName,
            ("volumes" Data..=) Prelude.<$> volumes
          ]
      )
