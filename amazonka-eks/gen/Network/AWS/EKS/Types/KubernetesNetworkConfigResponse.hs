{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EKS.Types.KubernetesNetworkConfigResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.KubernetesNetworkConfigResponse where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The Kubernetes network configuration for the cluster.
--
-- /See:/ 'newKubernetesNetworkConfigResponse' smart constructor.
data KubernetesNetworkConfigResponse = KubernetesNetworkConfigResponse'
  { -- | The CIDR block that Kubernetes service IP addresses are assigned from.
    -- If you didn\'t specify a CIDR block when you created the cluster, then
    -- Kubernetes assigns addresses from either the 10.100.0.0\/16 or
    -- 172.20.0.0\/16 CIDR blocks. If this was specified, then it was specified
    -- when the cluster was created and it cannot be changed.
    serviceIpv4Cidr :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'KubernetesNetworkConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceIpv4Cidr', 'kubernetesNetworkConfigResponse_serviceIpv4Cidr' - The CIDR block that Kubernetes service IP addresses are assigned from.
-- If you didn\'t specify a CIDR block when you created the cluster, then
-- Kubernetes assigns addresses from either the 10.100.0.0\/16 or
-- 172.20.0.0\/16 CIDR blocks. If this was specified, then it was specified
-- when the cluster was created and it cannot be changed.
newKubernetesNetworkConfigResponse ::
  KubernetesNetworkConfigResponse
newKubernetesNetworkConfigResponse =
  KubernetesNetworkConfigResponse'
    { serviceIpv4Cidr =
        Prelude.Nothing
    }

-- | The CIDR block that Kubernetes service IP addresses are assigned from.
-- If you didn\'t specify a CIDR block when you created the cluster, then
-- Kubernetes assigns addresses from either the 10.100.0.0\/16 or
-- 172.20.0.0\/16 CIDR blocks. If this was specified, then it was specified
-- when the cluster was created and it cannot be changed.
kubernetesNetworkConfigResponse_serviceIpv4Cidr :: Lens.Lens' KubernetesNetworkConfigResponse (Prelude.Maybe Prelude.Text)
kubernetesNetworkConfigResponse_serviceIpv4Cidr = Lens.lens (\KubernetesNetworkConfigResponse' {serviceIpv4Cidr} -> serviceIpv4Cidr) (\s@KubernetesNetworkConfigResponse' {} a -> s {serviceIpv4Cidr = a} :: KubernetesNetworkConfigResponse)

instance
  Prelude.FromJSON
    KubernetesNetworkConfigResponse
  where
  parseJSON =
    Prelude.withObject
      "KubernetesNetworkConfigResponse"
      ( \x ->
          KubernetesNetworkConfigResponse'
            Prelude.<$> (x Prelude..:? "serviceIpv4Cidr")
      )

instance
  Prelude.Hashable
    KubernetesNetworkConfigResponse

instance
  Prelude.NFData
    KubernetesNetworkConfigResponse
