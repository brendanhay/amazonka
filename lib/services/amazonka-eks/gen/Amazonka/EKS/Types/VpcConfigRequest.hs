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
-- Module      : Amazonka.EKS.Types.VpcConfigRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.VpcConfigRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object representing the VPC configuration to use for an Amazon EKS
-- cluster.
--
-- /See:/ 'newVpcConfigRequest' smart constructor.
data VpcConfigRequest = VpcConfigRequest'
  { -- | Set this value to @true@ to enable private access for your cluster\'s
    -- Kubernetes API server endpoint. If you enable private access, Kubernetes
    -- API requests from within your cluster\'s VPC use the private VPC
    -- endpoint. The default value for this parameter is @false@, which
    -- disables private access for your Kubernetes API server. If you disable
    -- private access and you have nodes or Fargate pods in the cluster, then
    -- ensure that @publicAccessCidrs@ includes the necessary CIDR blocks for
    -- communication with the nodes or Fargate pods. For more information, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/cluster-endpoint.html Amazon EKS cluster endpoint access control>
    -- in the //Amazon EKS User Guide// .
    endpointPrivateAccess :: Prelude.Maybe Prelude.Bool,
    -- | Set this value to @false@ to disable public access to your cluster\'s
    -- Kubernetes API server endpoint. If you disable public access, your
    -- cluster\'s Kubernetes API server can only receive requests from within
    -- the cluster VPC. The default value for this parameter is @true@, which
    -- enables public access for your Kubernetes API server. For more
    -- information, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/cluster-endpoint.html Amazon EKS cluster endpoint access control>
    -- in the //Amazon EKS User Guide// .
    endpointPublicAccess :: Prelude.Maybe Prelude.Bool,
    -- | The CIDR blocks that are allowed access to your cluster\'s public
    -- Kubernetes API server endpoint. Communication to the endpoint from
    -- addresses outside of the CIDR blocks that you specify is denied. The
    -- default value is @0.0.0.0\/0@. If you\'ve disabled private endpoint
    -- access and you have nodes or Fargate pods in the cluster, then ensure
    -- that you specify the necessary CIDR blocks. For more information, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/cluster-endpoint.html Amazon EKS cluster endpoint access control>
    -- in the //Amazon EKS User Guide// .
    publicAccessCidrs :: Prelude.Maybe [Prelude.Text],
    -- | Specify one or more security groups for the cross-account elastic
    -- network interfaces that Amazon EKS creates to use that allow
    -- communication between your nodes and the Kubernetes control plane. If
    -- you don\'t specify any security groups, then familiarize yourself with
    -- the difference between Amazon EKS defaults for clusters deployed with
    -- Kubernetes. For more information, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/sec-group-reqs.html Amazon EKS security group considerations>
    -- in the //Amazon EKS User Guide// .
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | Specify subnets for your Amazon EKS nodes. Amazon EKS creates
    -- cross-account elastic network interfaces in these subnets to allow
    -- communication between your nodes and the Kubernetes control plane.
    subnetIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcConfigRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointPrivateAccess', 'vpcConfigRequest_endpointPrivateAccess' - Set this value to @true@ to enable private access for your cluster\'s
-- Kubernetes API server endpoint. If you enable private access, Kubernetes
-- API requests from within your cluster\'s VPC use the private VPC
-- endpoint. The default value for this parameter is @false@, which
-- disables private access for your Kubernetes API server. If you disable
-- private access and you have nodes or Fargate pods in the cluster, then
-- ensure that @publicAccessCidrs@ includes the necessary CIDR blocks for
-- communication with the nodes or Fargate pods. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/cluster-endpoint.html Amazon EKS cluster endpoint access control>
-- in the //Amazon EKS User Guide// .
--
-- 'endpointPublicAccess', 'vpcConfigRequest_endpointPublicAccess' - Set this value to @false@ to disable public access to your cluster\'s
-- Kubernetes API server endpoint. If you disable public access, your
-- cluster\'s Kubernetes API server can only receive requests from within
-- the cluster VPC. The default value for this parameter is @true@, which
-- enables public access for your Kubernetes API server. For more
-- information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/cluster-endpoint.html Amazon EKS cluster endpoint access control>
-- in the //Amazon EKS User Guide// .
--
-- 'publicAccessCidrs', 'vpcConfigRequest_publicAccessCidrs' - The CIDR blocks that are allowed access to your cluster\'s public
-- Kubernetes API server endpoint. Communication to the endpoint from
-- addresses outside of the CIDR blocks that you specify is denied. The
-- default value is @0.0.0.0\/0@. If you\'ve disabled private endpoint
-- access and you have nodes or Fargate pods in the cluster, then ensure
-- that you specify the necessary CIDR blocks. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/cluster-endpoint.html Amazon EKS cluster endpoint access control>
-- in the //Amazon EKS User Guide// .
--
-- 'securityGroupIds', 'vpcConfigRequest_securityGroupIds' - Specify one or more security groups for the cross-account elastic
-- network interfaces that Amazon EKS creates to use that allow
-- communication between your nodes and the Kubernetes control plane. If
-- you don\'t specify any security groups, then familiarize yourself with
-- the difference between Amazon EKS defaults for clusters deployed with
-- Kubernetes. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/sec-group-reqs.html Amazon EKS security group considerations>
-- in the //Amazon EKS User Guide// .
--
-- 'subnetIds', 'vpcConfigRequest_subnetIds' - Specify subnets for your Amazon EKS nodes. Amazon EKS creates
-- cross-account elastic network interfaces in these subnets to allow
-- communication between your nodes and the Kubernetes control plane.
newVpcConfigRequest ::
  VpcConfigRequest
newVpcConfigRequest =
  VpcConfigRequest'
    { endpointPrivateAccess =
        Prelude.Nothing,
      endpointPublicAccess = Prelude.Nothing,
      publicAccessCidrs = Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      subnetIds = Prelude.Nothing
    }

-- | Set this value to @true@ to enable private access for your cluster\'s
-- Kubernetes API server endpoint. If you enable private access, Kubernetes
-- API requests from within your cluster\'s VPC use the private VPC
-- endpoint. The default value for this parameter is @false@, which
-- disables private access for your Kubernetes API server. If you disable
-- private access and you have nodes or Fargate pods in the cluster, then
-- ensure that @publicAccessCidrs@ includes the necessary CIDR blocks for
-- communication with the nodes or Fargate pods. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/cluster-endpoint.html Amazon EKS cluster endpoint access control>
-- in the //Amazon EKS User Guide// .
vpcConfigRequest_endpointPrivateAccess :: Lens.Lens' VpcConfigRequest (Prelude.Maybe Prelude.Bool)
vpcConfigRequest_endpointPrivateAccess = Lens.lens (\VpcConfigRequest' {endpointPrivateAccess} -> endpointPrivateAccess) (\s@VpcConfigRequest' {} a -> s {endpointPrivateAccess = a} :: VpcConfigRequest)

-- | Set this value to @false@ to disable public access to your cluster\'s
-- Kubernetes API server endpoint. If you disable public access, your
-- cluster\'s Kubernetes API server can only receive requests from within
-- the cluster VPC. The default value for this parameter is @true@, which
-- enables public access for your Kubernetes API server. For more
-- information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/cluster-endpoint.html Amazon EKS cluster endpoint access control>
-- in the //Amazon EKS User Guide// .
vpcConfigRequest_endpointPublicAccess :: Lens.Lens' VpcConfigRequest (Prelude.Maybe Prelude.Bool)
vpcConfigRequest_endpointPublicAccess = Lens.lens (\VpcConfigRequest' {endpointPublicAccess} -> endpointPublicAccess) (\s@VpcConfigRequest' {} a -> s {endpointPublicAccess = a} :: VpcConfigRequest)

-- | The CIDR blocks that are allowed access to your cluster\'s public
-- Kubernetes API server endpoint. Communication to the endpoint from
-- addresses outside of the CIDR blocks that you specify is denied. The
-- default value is @0.0.0.0\/0@. If you\'ve disabled private endpoint
-- access and you have nodes or Fargate pods in the cluster, then ensure
-- that you specify the necessary CIDR blocks. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/cluster-endpoint.html Amazon EKS cluster endpoint access control>
-- in the //Amazon EKS User Guide// .
vpcConfigRequest_publicAccessCidrs :: Lens.Lens' VpcConfigRequest (Prelude.Maybe [Prelude.Text])
vpcConfigRequest_publicAccessCidrs = Lens.lens (\VpcConfigRequest' {publicAccessCidrs} -> publicAccessCidrs) (\s@VpcConfigRequest' {} a -> s {publicAccessCidrs = a} :: VpcConfigRequest) Prelude.. Lens.mapping Lens.coerced

-- | Specify one or more security groups for the cross-account elastic
-- network interfaces that Amazon EKS creates to use that allow
-- communication between your nodes and the Kubernetes control plane. If
-- you don\'t specify any security groups, then familiarize yourself with
-- the difference between Amazon EKS defaults for clusters deployed with
-- Kubernetes. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/sec-group-reqs.html Amazon EKS security group considerations>
-- in the //Amazon EKS User Guide// .
vpcConfigRequest_securityGroupIds :: Lens.Lens' VpcConfigRequest (Prelude.Maybe [Prelude.Text])
vpcConfigRequest_securityGroupIds = Lens.lens (\VpcConfigRequest' {securityGroupIds} -> securityGroupIds) (\s@VpcConfigRequest' {} a -> s {securityGroupIds = a} :: VpcConfigRequest) Prelude.. Lens.mapping Lens.coerced

-- | Specify subnets for your Amazon EKS nodes. Amazon EKS creates
-- cross-account elastic network interfaces in these subnets to allow
-- communication between your nodes and the Kubernetes control plane.
vpcConfigRequest_subnetIds :: Lens.Lens' VpcConfigRequest (Prelude.Maybe [Prelude.Text])
vpcConfigRequest_subnetIds = Lens.lens (\VpcConfigRequest' {subnetIds} -> subnetIds) (\s@VpcConfigRequest' {} a -> s {subnetIds = a} :: VpcConfigRequest) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable VpcConfigRequest where
  hashWithSalt _salt VpcConfigRequest' {..} =
    _salt `Prelude.hashWithSalt` endpointPrivateAccess
      `Prelude.hashWithSalt` endpointPublicAccess
      `Prelude.hashWithSalt` publicAccessCidrs
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` subnetIds

instance Prelude.NFData VpcConfigRequest where
  rnf VpcConfigRequest' {..} =
    Prelude.rnf endpointPrivateAccess
      `Prelude.seq` Prelude.rnf endpointPublicAccess
      `Prelude.seq` Prelude.rnf publicAccessCidrs
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf subnetIds

instance Data.ToJSON VpcConfigRequest where
  toJSON VpcConfigRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("endpointPrivateAccess" Data..=)
              Prelude.<$> endpointPrivateAccess,
            ("endpointPublicAccess" Data..=)
              Prelude.<$> endpointPublicAccess,
            ("publicAccessCidrs" Data..=)
              Prelude.<$> publicAccessCidrs,
            ("securityGroupIds" Data..=)
              Prelude.<$> securityGroupIds,
            ("subnetIds" Data..=) Prelude.<$> subnetIds
          ]
      )
