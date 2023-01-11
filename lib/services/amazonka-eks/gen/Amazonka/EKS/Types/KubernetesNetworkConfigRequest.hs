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
-- Module      : Amazonka.EKS.Types.KubernetesNetworkConfigRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.KubernetesNetworkConfigRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types.IpFamily
import qualified Amazonka.Prelude as Prelude

-- | The Kubernetes network configuration for the cluster.
--
-- /See:/ 'newKubernetesNetworkConfigRequest' smart constructor.
data KubernetesNetworkConfigRequest = KubernetesNetworkConfigRequest'
  { -- | Specify which IP family is used to assign Kubernetes pod and service IP
    -- addresses. If you don\'t specify a value, @ipv4@ is used by default. You
    -- can only specify an IP family when you create a cluster and can\'t
    -- change this value once the cluster is created. If you specify @ipv6@,
    -- the VPC and subnets that you specify for cluster creation must have both
    -- @IPv4@ and @IPv6@ CIDR blocks assigned to them. You can\'t specify
    -- @ipv6@ for clusters in China Regions.
    --
    -- You can only specify @ipv6@ for @1.21@ and later clusters that use
    -- version @1.10.1@ or later of the Amazon VPC CNI add-on. If you specify
    -- @ipv6@, then ensure that your VPC meets the requirements listed in the
    -- considerations listed in
    -- <https://docs.aws.amazon.com/eks/latest/userguide/cni-ipv6.html Assigning IPv6 addresses to pods and services>
    -- in the Amazon EKS User Guide. Kubernetes assigns services @IPv6@
    -- addresses from the unique local address range @(fc00::\/7)@. You can\'t
    -- specify a custom @IPv6@ CIDR block. Pod addresses are assigned from the
    -- subnet\'s @IPv6@ CIDR.
    ipFamily :: Prelude.Maybe IpFamily,
    -- | Don\'t specify a value if you select @ipv6@ for __ipFamily__. The CIDR
    -- block to assign Kubernetes service IP addresses from. If you don\'t
    -- specify a block, Kubernetes assigns addresses from either the
    -- @10.100.0.0\/16@ or @172.20.0.0\/16@ CIDR blocks. We recommend that you
    -- specify a block that does not overlap with resources in other networks
    -- that are peered or connected to your VPC. The block must meet the
    -- following requirements:
    --
    -- -   Within one of the following private IP address blocks:
    --     @10.0.0.0\/8@, @172.16.0.0\/12@, or @192.168.0.0\/16@.
    --
    -- -   Doesn\'t overlap with any CIDR block assigned to the VPC that you
    --     selected for VPC.
    --
    -- -   Between \/24 and \/12.
    --
    -- You can only specify a custom CIDR block when you create a cluster and
    -- can\'t change this value once the cluster is created.
    serviceIpv4Cidr :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KubernetesNetworkConfigRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipFamily', 'kubernetesNetworkConfigRequest_ipFamily' - Specify which IP family is used to assign Kubernetes pod and service IP
-- addresses. If you don\'t specify a value, @ipv4@ is used by default. You
-- can only specify an IP family when you create a cluster and can\'t
-- change this value once the cluster is created. If you specify @ipv6@,
-- the VPC and subnets that you specify for cluster creation must have both
-- @IPv4@ and @IPv6@ CIDR blocks assigned to them. You can\'t specify
-- @ipv6@ for clusters in China Regions.
--
-- You can only specify @ipv6@ for @1.21@ and later clusters that use
-- version @1.10.1@ or later of the Amazon VPC CNI add-on. If you specify
-- @ipv6@, then ensure that your VPC meets the requirements listed in the
-- considerations listed in
-- <https://docs.aws.amazon.com/eks/latest/userguide/cni-ipv6.html Assigning IPv6 addresses to pods and services>
-- in the Amazon EKS User Guide. Kubernetes assigns services @IPv6@
-- addresses from the unique local address range @(fc00::\/7)@. You can\'t
-- specify a custom @IPv6@ CIDR block. Pod addresses are assigned from the
-- subnet\'s @IPv6@ CIDR.
--
-- 'serviceIpv4Cidr', 'kubernetesNetworkConfigRequest_serviceIpv4Cidr' - Don\'t specify a value if you select @ipv6@ for __ipFamily__. The CIDR
-- block to assign Kubernetes service IP addresses from. If you don\'t
-- specify a block, Kubernetes assigns addresses from either the
-- @10.100.0.0\/16@ or @172.20.0.0\/16@ CIDR blocks. We recommend that you
-- specify a block that does not overlap with resources in other networks
-- that are peered or connected to your VPC. The block must meet the
-- following requirements:
--
-- -   Within one of the following private IP address blocks:
--     @10.0.0.0\/8@, @172.16.0.0\/12@, or @192.168.0.0\/16@.
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
    { ipFamily =
        Prelude.Nothing,
      serviceIpv4Cidr = Prelude.Nothing
    }

-- | Specify which IP family is used to assign Kubernetes pod and service IP
-- addresses. If you don\'t specify a value, @ipv4@ is used by default. You
-- can only specify an IP family when you create a cluster and can\'t
-- change this value once the cluster is created. If you specify @ipv6@,
-- the VPC and subnets that you specify for cluster creation must have both
-- @IPv4@ and @IPv6@ CIDR blocks assigned to them. You can\'t specify
-- @ipv6@ for clusters in China Regions.
--
-- You can only specify @ipv6@ for @1.21@ and later clusters that use
-- version @1.10.1@ or later of the Amazon VPC CNI add-on. If you specify
-- @ipv6@, then ensure that your VPC meets the requirements listed in the
-- considerations listed in
-- <https://docs.aws.amazon.com/eks/latest/userguide/cni-ipv6.html Assigning IPv6 addresses to pods and services>
-- in the Amazon EKS User Guide. Kubernetes assigns services @IPv6@
-- addresses from the unique local address range @(fc00::\/7)@. You can\'t
-- specify a custom @IPv6@ CIDR block. Pod addresses are assigned from the
-- subnet\'s @IPv6@ CIDR.
kubernetesNetworkConfigRequest_ipFamily :: Lens.Lens' KubernetesNetworkConfigRequest (Prelude.Maybe IpFamily)
kubernetesNetworkConfigRequest_ipFamily = Lens.lens (\KubernetesNetworkConfigRequest' {ipFamily} -> ipFamily) (\s@KubernetesNetworkConfigRequest' {} a -> s {ipFamily = a} :: KubernetesNetworkConfigRequest)

-- | Don\'t specify a value if you select @ipv6@ for __ipFamily__. The CIDR
-- block to assign Kubernetes service IP addresses from. If you don\'t
-- specify a block, Kubernetes assigns addresses from either the
-- @10.100.0.0\/16@ or @172.20.0.0\/16@ CIDR blocks. We recommend that you
-- specify a block that does not overlap with resources in other networks
-- that are peered or connected to your VPC. The block must meet the
-- following requirements:
--
-- -   Within one of the following private IP address blocks:
--     @10.0.0.0\/8@, @172.16.0.0\/12@, or @192.168.0.0\/16@.
--
-- -   Doesn\'t overlap with any CIDR block assigned to the VPC that you
--     selected for VPC.
--
-- -   Between \/24 and \/12.
--
-- You can only specify a custom CIDR block when you create a cluster and
-- can\'t change this value once the cluster is created.
kubernetesNetworkConfigRequest_serviceIpv4Cidr :: Lens.Lens' KubernetesNetworkConfigRequest (Prelude.Maybe Prelude.Text)
kubernetesNetworkConfigRequest_serviceIpv4Cidr = Lens.lens (\KubernetesNetworkConfigRequest' {serviceIpv4Cidr} -> serviceIpv4Cidr) (\s@KubernetesNetworkConfigRequest' {} a -> s {serviceIpv4Cidr = a} :: KubernetesNetworkConfigRequest)

instance
  Prelude.Hashable
    KubernetesNetworkConfigRequest
  where
  hashWithSalt
    _salt
    KubernetesNetworkConfigRequest' {..} =
      _salt `Prelude.hashWithSalt` ipFamily
        `Prelude.hashWithSalt` serviceIpv4Cidr

instance
  Prelude.NFData
    KubernetesNetworkConfigRequest
  where
  rnf KubernetesNetworkConfigRequest' {..} =
    Prelude.rnf ipFamily
      `Prelude.seq` Prelude.rnf serviceIpv4Cidr

instance Data.ToJSON KubernetesNetworkConfigRequest where
  toJSON KubernetesNetworkConfigRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ipFamily" Data..=) Prelude.<$> ipFamily,
            ("serviceIpv4Cidr" Data..=)
              Prelude.<$> serviceIpv4Cidr
          ]
      )
