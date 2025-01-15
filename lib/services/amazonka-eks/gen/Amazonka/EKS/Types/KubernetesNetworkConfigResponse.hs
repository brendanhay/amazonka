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
-- Module      : Amazonka.EKS.Types.KubernetesNetworkConfigResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.KubernetesNetworkConfigResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types.IpFamily
import qualified Amazonka.Prelude as Prelude

-- | The Kubernetes network configuration for the cluster. The response
-- contains a value for __serviceIpv6Cidr__ or __serviceIpv4Cidr__, but not
-- both.
--
-- /See:/ 'newKubernetesNetworkConfigResponse' smart constructor.
data KubernetesNetworkConfigResponse = KubernetesNetworkConfigResponse'
  { -- | The IP family used to assign Kubernetes pod and service IP addresses.
    -- The IP family is always @ipv4@, unless you have a @1.21@ or later
    -- cluster running version 1.10.1 or later of the Amazon VPC CNI add-on and
    -- specified @ipv6@ when you created the cluster.
    ipFamily :: Prelude.Maybe IpFamily,
    -- | The CIDR block that Kubernetes pod and service IP addresses are assigned
    -- from. Kubernetes assigns addresses from an IPv4 CIDR block assigned to a
    -- subnet that the node is in. If you didn\'t specify a CIDR block when you
    -- created the cluster, then Kubernetes assigns addresses from either the
    -- @10.100.0.0\/16@ or @172.20.0.0\/16@ CIDR blocks. If this was specified,
    -- then it was specified when the cluster was created and it can\'t be
    -- changed.
    serviceIpv4Cidr :: Prelude.Maybe Prelude.Text,
    -- | The CIDR block that Kubernetes pod and service IP addresses are assigned
    -- from if you created a 1.21 or later cluster with version 1.10.1 or later
    -- of the Amazon VPC CNI add-on and specified @ipv6@ for __ipFamily__ when
    -- you created the cluster. Kubernetes assigns service addresses from the
    -- unique local address range (@fc00::\/7@) because you can\'t specify a
    -- custom IPv6 CIDR block when you create the cluster.
    serviceIpv6Cidr :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KubernetesNetworkConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipFamily', 'kubernetesNetworkConfigResponse_ipFamily' - The IP family used to assign Kubernetes pod and service IP addresses.
-- The IP family is always @ipv4@, unless you have a @1.21@ or later
-- cluster running version 1.10.1 or later of the Amazon VPC CNI add-on and
-- specified @ipv6@ when you created the cluster.
--
-- 'serviceIpv4Cidr', 'kubernetesNetworkConfigResponse_serviceIpv4Cidr' - The CIDR block that Kubernetes pod and service IP addresses are assigned
-- from. Kubernetes assigns addresses from an IPv4 CIDR block assigned to a
-- subnet that the node is in. If you didn\'t specify a CIDR block when you
-- created the cluster, then Kubernetes assigns addresses from either the
-- @10.100.0.0\/16@ or @172.20.0.0\/16@ CIDR blocks. If this was specified,
-- then it was specified when the cluster was created and it can\'t be
-- changed.
--
-- 'serviceIpv6Cidr', 'kubernetesNetworkConfigResponse_serviceIpv6Cidr' - The CIDR block that Kubernetes pod and service IP addresses are assigned
-- from if you created a 1.21 or later cluster with version 1.10.1 or later
-- of the Amazon VPC CNI add-on and specified @ipv6@ for __ipFamily__ when
-- you created the cluster. Kubernetes assigns service addresses from the
-- unique local address range (@fc00::\/7@) because you can\'t specify a
-- custom IPv6 CIDR block when you create the cluster.
newKubernetesNetworkConfigResponse ::
  KubernetesNetworkConfigResponse
newKubernetesNetworkConfigResponse =
  KubernetesNetworkConfigResponse'
    { ipFamily =
        Prelude.Nothing,
      serviceIpv4Cidr = Prelude.Nothing,
      serviceIpv6Cidr = Prelude.Nothing
    }

-- | The IP family used to assign Kubernetes pod and service IP addresses.
-- The IP family is always @ipv4@, unless you have a @1.21@ or later
-- cluster running version 1.10.1 or later of the Amazon VPC CNI add-on and
-- specified @ipv6@ when you created the cluster.
kubernetesNetworkConfigResponse_ipFamily :: Lens.Lens' KubernetesNetworkConfigResponse (Prelude.Maybe IpFamily)
kubernetesNetworkConfigResponse_ipFamily = Lens.lens (\KubernetesNetworkConfigResponse' {ipFamily} -> ipFamily) (\s@KubernetesNetworkConfigResponse' {} a -> s {ipFamily = a} :: KubernetesNetworkConfigResponse)

-- | The CIDR block that Kubernetes pod and service IP addresses are assigned
-- from. Kubernetes assigns addresses from an IPv4 CIDR block assigned to a
-- subnet that the node is in. If you didn\'t specify a CIDR block when you
-- created the cluster, then Kubernetes assigns addresses from either the
-- @10.100.0.0\/16@ or @172.20.0.0\/16@ CIDR blocks. If this was specified,
-- then it was specified when the cluster was created and it can\'t be
-- changed.
kubernetesNetworkConfigResponse_serviceIpv4Cidr :: Lens.Lens' KubernetesNetworkConfigResponse (Prelude.Maybe Prelude.Text)
kubernetesNetworkConfigResponse_serviceIpv4Cidr = Lens.lens (\KubernetesNetworkConfigResponse' {serviceIpv4Cidr} -> serviceIpv4Cidr) (\s@KubernetesNetworkConfigResponse' {} a -> s {serviceIpv4Cidr = a} :: KubernetesNetworkConfigResponse)

-- | The CIDR block that Kubernetes pod and service IP addresses are assigned
-- from if you created a 1.21 or later cluster with version 1.10.1 or later
-- of the Amazon VPC CNI add-on and specified @ipv6@ for __ipFamily__ when
-- you created the cluster. Kubernetes assigns service addresses from the
-- unique local address range (@fc00::\/7@) because you can\'t specify a
-- custom IPv6 CIDR block when you create the cluster.
kubernetesNetworkConfigResponse_serviceIpv6Cidr :: Lens.Lens' KubernetesNetworkConfigResponse (Prelude.Maybe Prelude.Text)
kubernetesNetworkConfigResponse_serviceIpv6Cidr = Lens.lens (\KubernetesNetworkConfigResponse' {serviceIpv6Cidr} -> serviceIpv6Cidr) (\s@KubernetesNetworkConfigResponse' {} a -> s {serviceIpv6Cidr = a} :: KubernetesNetworkConfigResponse)

instance
  Data.FromJSON
    KubernetesNetworkConfigResponse
  where
  parseJSON =
    Data.withObject
      "KubernetesNetworkConfigResponse"
      ( \x ->
          KubernetesNetworkConfigResponse'
            Prelude.<$> (x Data..:? "ipFamily")
            Prelude.<*> (x Data..:? "serviceIpv4Cidr")
            Prelude.<*> (x Data..:? "serviceIpv6Cidr")
      )

instance
  Prelude.Hashable
    KubernetesNetworkConfigResponse
  where
  hashWithSalt
    _salt
    KubernetesNetworkConfigResponse' {..} =
      _salt
        `Prelude.hashWithSalt` ipFamily
        `Prelude.hashWithSalt` serviceIpv4Cidr
        `Prelude.hashWithSalt` serviceIpv6Cidr

instance
  Prelude.NFData
    KubernetesNetworkConfigResponse
  where
  rnf KubernetesNetworkConfigResponse' {..} =
    Prelude.rnf ipFamily `Prelude.seq`
      Prelude.rnf serviceIpv4Cidr `Prelude.seq`
        Prelude.rnf serviceIpv6Cidr
