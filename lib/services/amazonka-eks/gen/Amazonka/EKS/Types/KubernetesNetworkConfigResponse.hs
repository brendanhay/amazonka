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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.KubernetesNetworkConfigResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Core.FromJSON
    KubernetesNetworkConfigResponse
  where
  parseJSON =
    Core.withObject
      "KubernetesNetworkConfigResponse"
      ( \x ->
          KubernetesNetworkConfigResponse'
            Prelude.<$> (x Core..:? "serviceIpv4Cidr")
      )

instance
  Prelude.Hashable
    KubernetesNetworkConfigResponse
  where
  hashWithSalt
    salt'
    KubernetesNetworkConfigResponse' {..} =
      salt' `Prelude.hashWithSalt` serviceIpv4Cidr

instance
  Prelude.NFData
    KubernetesNetworkConfigResponse
  where
  rnf KubernetesNetworkConfigResponse' {..} =
    Prelude.rnf serviceIpv4Cidr
