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
-- Module      : Network.AWS.EKS.Types.VpcConfigRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.VpcConfigRequest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An object representing the VPC configuration to use for an Amazon EKS
-- cluster.
--
-- /See:/ 'newVpcConfigRequest' smart constructor.
data VpcConfigRequest = VpcConfigRequest'
  { -- | Specify one or more security groups for the cross-account elastic
    -- network interfaces that Amazon EKS creates to use to allow communication
    -- between your nodes and the Kubernetes control plane. If you don\'t
    -- specify any security groups, then familiarize yourself with the
    -- difference between Amazon EKS defaults for clusters deployed with
    -- Kubernetes:
    --
    -- -   1.14 Amazon EKS platform version @eks.2@ and earlier
    --
    -- -   1.14 Amazon EKS platform version @eks.3@ and later
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/sec-group-reqs.html Amazon EKS security group considerations>
    -- in the //Amazon EKS User Guide// .
    securityGroupIds :: Core.Maybe [Core.Text],
    -- | Set this value to @false@ to disable public access to your cluster\'s
    -- Kubernetes API server endpoint. If you disable public access, your
    -- cluster\'s Kubernetes API server can only receive requests from within
    -- the cluster VPC. The default value for this parameter is @true@, which
    -- enables public access for your Kubernetes API server. For more
    -- information, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/cluster-endpoint.html Amazon EKS Cluster Endpoint Access Control>
    -- in the //Amazon EKS User Guide// .
    endpointPublicAccess :: Core.Maybe Core.Bool,
    -- | Specify subnets for your Amazon EKS nodes. Amazon EKS creates
    -- cross-account elastic network interfaces in these subnets to allow
    -- communication between your nodes and the Kubernetes control plane.
    subnetIds :: Core.Maybe [Core.Text],
    -- | Set this value to @true@ to enable private access for your cluster\'s
    -- Kubernetes API server endpoint. If you enable private access, Kubernetes
    -- API requests from within your cluster\'s VPC use the private VPC
    -- endpoint. The default value for this parameter is @false@, which
    -- disables private access for your Kubernetes API server. If you disable
    -- private access and you have nodes or AWS Fargate pods in the cluster,
    -- then ensure that @publicAccessCidrs@ includes the necessary CIDR blocks
    -- for communication with the nodes or Fargate pods. For more information,
    -- see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/cluster-endpoint.html Amazon EKS Cluster Endpoint Access Control>
    -- in the //Amazon EKS User Guide// .
    endpointPrivateAccess :: Core.Maybe Core.Bool,
    -- | The CIDR blocks that are allowed access to your cluster\'s public
    -- Kubernetes API server endpoint. Communication to the endpoint from
    -- addresses outside of the CIDR blocks that you specify is denied. The
    -- default value is @0.0.0.0\/0@. If you\'ve disabled private endpoint
    -- access and you have nodes or AWS Fargate pods in the cluster, then
    -- ensure that you specify the necessary CIDR blocks. For more information,
    -- see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/cluster-endpoint.html Amazon EKS Cluster Endpoint Access Control>
    -- in the //Amazon EKS User Guide// .
    publicAccessCidrs :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VpcConfigRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'vpcConfigRequest_securityGroupIds' - Specify one or more security groups for the cross-account elastic
-- network interfaces that Amazon EKS creates to use to allow communication
-- between your nodes and the Kubernetes control plane. If you don\'t
-- specify any security groups, then familiarize yourself with the
-- difference between Amazon EKS defaults for clusters deployed with
-- Kubernetes:
--
-- -   1.14 Amazon EKS platform version @eks.2@ and earlier
--
-- -   1.14 Amazon EKS platform version @eks.3@ and later
--
-- For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/sec-group-reqs.html Amazon EKS security group considerations>
-- in the //Amazon EKS User Guide// .
--
-- 'endpointPublicAccess', 'vpcConfigRequest_endpointPublicAccess' - Set this value to @false@ to disable public access to your cluster\'s
-- Kubernetes API server endpoint. If you disable public access, your
-- cluster\'s Kubernetes API server can only receive requests from within
-- the cluster VPC. The default value for this parameter is @true@, which
-- enables public access for your Kubernetes API server. For more
-- information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/cluster-endpoint.html Amazon EKS Cluster Endpoint Access Control>
-- in the //Amazon EKS User Guide// .
--
-- 'subnetIds', 'vpcConfigRequest_subnetIds' - Specify subnets for your Amazon EKS nodes. Amazon EKS creates
-- cross-account elastic network interfaces in these subnets to allow
-- communication between your nodes and the Kubernetes control plane.
--
-- 'endpointPrivateAccess', 'vpcConfigRequest_endpointPrivateAccess' - Set this value to @true@ to enable private access for your cluster\'s
-- Kubernetes API server endpoint. If you enable private access, Kubernetes
-- API requests from within your cluster\'s VPC use the private VPC
-- endpoint. The default value for this parameter is @false@, which
-- disables private access for your Kubernetes API server. If you disable
-- private access and you have nodes or AWS Fargate pods in the cluster,
-- then ensure that @publicAccessCidrs@ includes the necessary CIDR blocks
-- for communication with the nodes or Fargate pods. For more information,
-- see
-- <https://docs.aws.amazon.com/eks/latest/userguide/cluster-endpoint.html Amazon EKS Cluster Endpoint Access Control>
-- in the //Amazon EKS User Guide// .
--
-- 'publicAccessCidrs', 'vpcConfigRequest_publicAccessCidrs' - The CIDR blocks that are allowed access to your cluster\'s public
-- Kubernetes API server endpoint. Communication to the endpoint from
-- addresses outside of the CIDR blocks that you specify is denied. The
-- default value is @0.0.0.0\/0@. If you\'ve disabled private endpoint
-- access and you have nodes or AWS Fargate pods in the cluster, then
-- ensure that you specify the necessary CIDR blocks. For more information,
-- see
-- <https://docs.aws.amazon.com/eks/latest/userguide/cluster-endpoint.html Amazon EKS Cluster Endpoint Access Control>
-- in the //Amazon EKS User Guide// .
newVpcConfigRequest ::
  VpcConfigRequest
newVpcConfigRequest =
  VpcConfigRequest'
    { securityGroupIds = Core.Nothing,
      endpointPublicAccess = Core.Nothing,
      subnetIds = Core.Nothing,
      endpointPrivateAccess = Core.Nothing,
      publicAccessCidrs = Core.Nothing
    }

-- | Specify one or more security groups for the cross-account elastic
-- network interfaces that Amazon EKS creates to use to allow communication
-- between your nodes and the Kubernetes control plane. If you don\'t
-- specify any security groups, then familiarize yourself with the
-- difference between Amazon EKS defaults for clusters deployed with
-- Kubernetes:
--
-- -   1.14 Amazon EKS platform version @eks.2@ and earlier
--
-- -   1.14 Amazon EKS platform version @eks.3@ and later
--
-- For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/sec-group-reqs.html Amazon EKS security group considerations>
-- in the //Amazon EKS User Guide// .
vpcConfigRequest_securityGroupIds :: Lens.Lens' VpcConfigRequest (Core.Maybe [Core.Text])
vpcConfigRequest_securityGroupIds = Lens.lens (\VpcConfigRequest' {securityGroupIds} -> securityGroupIds) (\s@VpcConfigRequest' {} a -> s {securityGroupIds = a} :: VpcConfigRequest) Core.. Lens.mapping Lens._Coerce

-- | Set this value to @false@ to disable public access to your cluster\'s
-- Kubernetes API server endpoint. If you disable public access, your
-- cluster\'s Kubernetes API server can only receive requests from within
-- the cluster VPC. The default value for this parameter is @true@, which
-- enables public access for your Kubernetes API server. For more
-- information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/cluster-endpoint.html Amazon EKS Cluster Endpoint Access Control>
-- in the //Amazon EKS User Guide// .
vpcConfigRequest_endpointPublicAccess :: Lens.Lens' VpcConfigRequest (Core.Maybe Core.Bool)
vpcConfigRequest_endpointPublicAccess = Lens.lens (\VpcConfigRequest' {endpointPublicAccess} -> endpointPublicAccess) (\s@VpcConfigRequest' {} a -> s {endpointPublicAccess = a} :: VpcConfigRequest)

-- | Specify subnets for your Amazon EKS nodes. Amazon EKS creates
-- cross-account elastic network interfaces in these subnets to allow
-- communication between your nodes and the Kubernetes control plane.
vpcConfigRequest_subnetIds :: Lens.Lens' VpcConfigRequest (Core.Maybe [Core.Text])
vpcConfigRequest_subnetIds = Lens.lens (\VpcConfigRequest' {subnetIds} -> subnetIds) (\s@VpcConfigRequest' {} a -> s {subnetIds = a} :: VpcConfigRequest) Core.. Lens.mapping Lens._Coerce

-- | Set this value to @true@ to enable private access for your cluster\'s
-- Kubernetes API server endpoint. If you enable private access, Kubernetes
-- API requests from within your cluster\'s VPC use the private VPC
-- endpoint. The default value for this parameter is @false@, which
-- disables private access for your Kubernetes API server. If you disable
-- private access and you have nodes or AWS Fargate pods in the cluster,
-- then ensure that @publicAccessCidrs@ includes the necessary CIDR blocks
-- for communication with the nodes or Fargate pods. For more information,
-- see
-- <https://docs.aws.amazon.com/eks/latest/userguide/cluster-endpoint.html Amazon EKS Cluster Endpoint Access Control>
-- in the //Amazon EKS User Guide// .
vpcConfigRequest_endpointPrivateAccess :: Lens.Lens' VpcConfigRequest (Core.Maybe Core.Bool)
vpcConfigRequest_endpointPrivateAccess = Lens.lens (\VpcConfigRequest' {endpointPrivateAccess} -> endpointPrivateAccess) (\s@VpcConfigRequest' {} a -> s {endpointPrivateAccess = a} :: VpcConfigRequest)

-- | The CIDR blocks that are allowed access to your cluster\'s public
-- Kubernetes API server endpoint. Communication to the endpoint from
-- addresses outside of the CIDR blocks that you specify is denied. The
-- default value is @0.0.0.0\/0@. If you\'ve disabled private endpoint
-- access and you have nodes or AWS Fargate pods in the cluster, then
-- ensure that you specify the necessary CIDR blocks. For more information,
-- see
-- <https://docs.aws.amazon.com/eks/latest/userguide/cluster-endpoint.html Amazon EKS Cluster Endpoint Access Control>
-- in the //Amazon EKS User Guide// .
vpcConfigRequest_publicAccessCidrs :: Lens.Lens' VpcConfigRequest (Core.Maybe [Core.Text])
vpcConfigRequest_publicAccessCidrs = Lens.lens (\VpcConfigRequest' {publicAccessCidrs} -> publicAccessCidrs) (\s@VpcConfigRequest' {} a -> s {publicAccessCidrs = a} :: VpcConfigRequest) Core.. Lens.mapping Lens._Coerce

instance Core.Hashable VpcConfigRequest

instance Core.NFData VpcConfigRequest

instance Core.ToJSON VpcConfigRequest where
  toJSON VpcConfigRequest' {..} =
    Core.object
      ( Core.catMaybes
          [ ("securityGroupIds" Core..=)
              Core.<$> securityGroupIds,
            ("endpointPublicAccess" Core..=)
              Core.<$> endpointPublicAccess,
            ("subnetIds" Core..=) Core.<$> subnetIds,
            ("endpointPrivateAccess" Core..=)
              Core.<$> endpointPrivateAccess,
            ("publicAccessCidrs" Core..=)
              Core.<$> publicAccessCidrs
          ]
      )
