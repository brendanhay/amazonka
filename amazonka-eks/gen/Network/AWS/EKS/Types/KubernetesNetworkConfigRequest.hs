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
-- Module      : Network.AWS.EKS.Types.KubernetesNetworkConfigRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.KubernetesNetworkConfigRequest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The Kubernetes network configuration for the cluster.
--
-- /See:/ 'newKubernetesNetworkConfigRequest' smart constructor.
data KubernetesNetworkConfigRequest = KubernetesNetworkConfigRequest'
  { -- | The CIDR block to assign Kubernetes service IP addresses from. If you
    -- don\'t specify a block, Kubernetes assigns addresses from either the
    -- 10.100.0.0\/16 or 172.20.0.0\/16 CIDR blocks. We recommend that you
    -- specify a block that does not overlap with resources in other networks
    -- that are peered or connected to your VPC. The block must meet the
    -- following requirements:
    --
    -- -   Within one of the following private IP address blocks: 10.0.0.0\/8,
    --     172.16.0.0.0\/12, or 192.168.0.0\/16.
    --
    -- -   Doesn\'t overlap with any CIDR block assigned to the VPC that you
    --     selected for VPC.
    --
    -- -   Between \/24 and \/12.
    --
    -- You can only specify a custom CIDR block when you create a cluster and
    -- can\'t change this value once the cluster is created.
    serviceIpv4Cidr :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'KubernetesNetworkConfigRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceIpv4Cidr', 'kubernetesNetworkConfigRequest_serviceIpv4Cidr' - The CIDR block to assign Kubernetes service IP addresses from. If you
-- don\'t specify a block, Kubernetes assigns addresses from either the
-- 10.100.0.0\/16 or 172.20.0.0\/16 CIDR blocks. We recommend that you
-- specify a block that does not overlap with resources in other networks
-- that are peered or connected to your VPC. The block must meet the
-- following requirements:
--
-- -   Within one of the following private IP address blocks: 10.0.0.0\/8,
--     172.16.0.0.0\/12, or 192.168.0.0\/16.
--
-- -   Doesn\'t overlap with any CIDR block assigned to the VPC that you
--     selected for VPC.
--
-- -   Between \/24 and \/12.
--
-- You can only specify a custom CIDR block when you create a cluster and
-- can\'t change this value once the cluster is created.
newKubernetesNetworkConfigRequest ::
  KubernetesNetworkConfigRequest
newKubernetesNetworkConfigRequest =
  KubernetesNetworkConfigRequest'
    { serviceIpv4Cidr =
        Core.Nothing
    }

-- | The CIDR block to assign Kubernetes service IP addresses from. If you
-- don\'t specify a block, Kubernetes assigns addresses from either the
-- 10.100.0.0\/16 or 172.20.0.0\/16 CIDR blocks. We recommend that you
-- specify a block that does not overlap with resources in other networks
-- that are peered or connected to your VPC. The block must meet the
-- following requirements:
--
-- -   Within one of the following private IP address blocks: 10.0.0.0\/8,
--     172.16.0.0.0\/12, or 192.168.0.0\/16.
--
-- -   Doesn\'t overlap with any CIDR block assigned to the VPC that you
--     selected for VPC.
--
-- -   Between \/24 and \/12.
--
-- You can only specify a custom CIDR block when you create a cluster and
-- can\'t change this value once the cluster is created.
kubernetesNetworkConfigRequest_serviceIpv4Cidr :: Lens.Lens' KubernetesNetworkConfigRequest (Core.Maybe Core.Text)
kubernetesNetworkConfigRequest_serviceIpv4Cidr = Lens.lens (\KubernetesNetworkConfigRequest' {serviceIpv4Cidr} -> serviceIpv4Cidr) (\s@KubernetesNetworkConfigRequest' {} a -> s {serviceIpv4Cidr = a} :: KubernetesNetworkConfigRequest)

instance Core.Hashable KubernetesNetworkConfigRequest

instance Core.NFData KubernetesNetworkConfigRequest

instance Core.ToJSON KubernetesNetworkConfigRequest where
  toJSON KubernetesNetworkConfigRequest' {..} =
    Core.object
      ( Core.catMaybes
          [ ("serviceIpv4Cidr" Core..=)
              Core.<$> serviceIpv4Cidr
          ]
      )
