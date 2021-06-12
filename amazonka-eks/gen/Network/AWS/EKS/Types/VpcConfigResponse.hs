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
-- Module      : Network.AWS.EKS.Types.VpcConfigResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.VpcConfigResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An object representing an Amazon EKS cluster VPC configuration response.
--
-- /See:/ 'newVpcConfigResponse' smart constructor.
data VpcConfigResponse = VpcConfigResponse'
  { -- | The security groups associated with the cross-account elastic network
    -- interfaces that are used to allow communication between your nodes and
    -- the Kubernetes control plane.
    securityGroupIds :: Core.Maybe [Core.Text],
    -- | This parameter indicates whether the Amazon EKS public API server
    -- endpoint is enabled. If the Amazon EKS public API server endpoint is
    -- disabled, your cluster\'s Kubernetes API server can only receive
    -- requests that originate from within the cluster VPC.
    endpointPublicAccess :: Core.Maybe Core.Bool,
    -- | The subnets associated with your cluster.
    subnetIds :: Core.Maybe [Core.Text],
    -- | The cluster security group that was created by Amazon EKS for the
    -- cluster. Managed node groups use this security group for
    -- control-plane-to-data-plane communication.
    clusterSecurityGroupId :: Core.Maybe Core.Text,
    -- | The VPC associated with your cluster.
    vpcId :: Core.Maybe Core.Text,
    -- | This parameter indicates whether the Amazon EKS private API server
    -- endpoint is enabled. If the Amazon EKS private API server endpoint is
    -- enabled, Kubernetes API requests that originate from within your
    -- cluster\'s VPC use the private VPC endpoint instead of traversing the
    -- internet. If this value is disabled and you have nodes or AWS Fargate
    -- pods in the cluster, then ensure that @publicAccessCidrs@ includes the
    -- necessary CIDR blocks for communication with the nodes or Fargate pods.
    -- For more information, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/cluster-endpoint.html Amazon EKS Cluster Endpoint Access Control>
    -- in the //Amazon EKS User Guide// .
    endpointPrivateAccess :: Core.Maybe Core.Bool,
    -- | The CIDR blocks that are allowed access to your cluster\'s public
    -- Kubernetes API server endpoint. Communication to the endpoint from
    -- addresses outside of the listed CIDR blocks is denied. The default value
    -- is @0.0.0.0\/0@. If you\'ve disabled private endpoint access and you
    -- have nodes or AWS Fargate pods in the cluster, then ensure that the
    -- necessary CIDR blocks are listed. For more information, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/cluster-endpoint.html Amazon EKS Cluster Endpoint Access Control>
    -- in the //Amazon EKS User Guide// .
    publicAccessCidrs :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VpcConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'vpcConfigResponse_securityGroupIds' - The security groups associated with the cross-account elastic network
-- interfaces that are used to allow communication between your nodes and
-- the Kubernetes control plane.
--
-- 'endpointPublicAccess', 'vpcConfigResponse_endpointPublicAccess' - This parameter indicates whether the Amazon EKS public API server
-- endpoint is enabled. If the Amazon EKS public API server endpoint is
-- disabled, your cluster\'s Kubernetes API server can only receive
-- requests that originate from within the cluster VPC.
--
-- 'subnetIds', 'vpcConfigResponse_subnetIds' - The subnets associated with your cluster.
--
-- 'clusterSecurityGroupId', 'vpcConfigResponse_clusterSecurityGroupId' - The cluster security group that was created by Amazon EKS for the
-- cluster. Managed node groups use this security group for
-- control-plane-to-data-plane communication.
--
-- 'vpcId', 'vpcConfigResponse_vpcId' - The VPC associated with your cluster.
--
-- 'endpointPrivateAccess', 'vpcConfigResponse_endpointPrivateAccess' - This parameter indicates whether the Amazon EKS private API server
-- endpoint is enabled. If the Amazon EKS private API server endpoint is
-- enabled, Kubernetes API requests that originate from within your
-- cluster\'s VPC use the private VPC endpoint instead of traversing the
-- internet. If this value is disabled and you have nodes or AWS Fargate
-- pods in the cluster, then ensure that @publicAccessCidrs@ includes the
-- necessary CIDR blocks for communication with the nodes or Fargate pods.
-- For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/cluster-endpoint.html Amazon EKS Cluster Endpoint Access Control>
-- in the //Amazon EKS User Guide// .
--
-- 'publicAccessCidrs', 'vpcConfigResponse_publicAccessCidrs' - The CIDR blocks that are allowed access to your cluster\'s public
-- Kubernetes API server endpoint. Communication to the endpoint from
-- addresses outside of the listed CIDR blocks is denied. The default value
-- is @0.0.0.0\/0@. If you\'ve disabled private endpoint access and you
-- have nodes or AWS Fargate pods in the cluster, then ensure that the
-- necessary CIDR blocks are listed. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/cluster-endpoint.html Amazon EKS Cluster Endpoint Access Control>
-- in the //Amazon EKS User Guide// .
newVpcConfigResponse ::
  VpcConfigResponse
newVpcConfigResponse =
  VpcConfigResponse'
    { securityGroupIds = Core.Nothing,
      endpointPublicAccess = Core.Nothing,
      subnetIds = Core.Nothing,
      clusterSecurityGroupId = Core.Nothing,
      vpcId = Core.Nothing,
      endpointPrivateAccess = Core.Nothing,
      publicAccessCidrs = Core.Nothing
    }

-- | The security groups associated with the cross-account elastic network
-- interfaces that are used to allow communication between your nodes and
-- the Kubernetes control plane.
vpcConfigResponse_securityGroupIds :: Lens.Lens' VpcConfigResponse (Core.Maybe [Core.Text])
vpcConfigResponse_securityGroupIds = Lens.lens (\VpcConfigResponse' {securityGroupIds} -> securityGroupIds) (\s@VpcConfigResponse' {} a -> s {securityGroupIds = a} :: VpcConfigResponse) Core.. Lens.mapping Lens._Coerce

-- | This parameter indicates whether the Amazon EKS public API server
-- endpoint is enabled. If the Amazon EKS public API server endpoint is
-- disabled, your cluster\'s Kubernetes API server can only receive
-- requests that originate from within the cluster VPC.
vpcConfigResponse_endpointPublicAccess :: Lens.Lens' VpcConfigResponse (Core.Maybe Core.Bool)
vpcConfigResponse_endpointPublicAccess = Lens.lens (\VpcConfigResponse' {endpointPublicAccess} -> endpointPublicAccess) (\s@VpcConfigResponse' {} a -> s {endpointPublicAccess = a} :: VpcConfigResponse)

-- | The subnets associated with your cluster.
vpcConfigResponse_subnetIds :: Lens.Lens' VpcConfigResponse (Core.Maybe [Core.Text])
vpcConfigResponse_subnetIds = Lens.lens (\VpcConfigResponse' {subnetIds} -> subnetIds) (\s@VpcConfigResponse' {} a -> s {subnetIds = a} :: VpcConfigResponse) Core.. Lens.mapping Lens._Coerce

-- | The cluster security group that was created by Amazon EKS for the
-- cluster. Managed node groups use this security group for
-- control-plane-to-data-plane communication.
vpcConfigResponse_clusterSecurityGroupId :: Lens.Lens' VpcConfigResponse (Core.Maybe Core.Text)
vpcConfigResponse_clusterSecurityGroupId = Lens.lens (\VpcConfigResponse' {clusterSecurityGroupId} -> clusterSecurityGroupId) (\s@VpcConfigResponse' {} a -> s {clusterSecurityGroupId = a} :: VpcConfigResponse)

-- | The VPC associated with your cluster.
vpcConfigResponse_vpcId :: Lens.Lens' VpcConfigResponse (Core.Maybe Core.Text)
vpcConfigResponse_vpcId = Lens.lens (\VpcConfigResponse' {vpcId} -> vpcId) (\s@VpcConfigResponse' {} a -> s {vpcId = a} :: VpcConfigResponse)

-- | This parameter indicates whether the Amazon EKS private API server
-- endpoint is enabled. If the Amazon EKS private API server endpoint is
-- enabled, Kubernetes API requests that originate from within your
-- cluster\'s VPC use the private VPC endpoint instead of traversing the
-- internet. If this value is disabled and you have nodes or AWS Fargate
-- pods in the cluster, then ensure that @publicAccessCidrs@ includes the
-- necessary CIDR blocks for communication with the nodes or Fargate pods.
-- For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/cluster-endpoint.html Amazon EKS Cluster Endpoint Access Control>
-- in the //Amazon EKS User Guide// .
vpcConfigResponse_endpointPrivateAccess :: Lens.Lens' VpcConfigResponse (Core.Maybe Core.Bool)
vpcConfigResponse_endpointPrivateAccess = Lens.lens (\VpcConfigResponse' {endpointPrivateAccess} -> endpointPrivateAccess) (\s@VpcConfigResponse' {} a -> s {endpointPrivateAccess = a} :: VpcConfigResponse)

-- | The CIDR blocks that are allowed access to your cluster\'s public
-- Kubernetes API server endpoint. Communication to the endpoint from
-- addresses outside of the listed CIDR blocks is denied. The default value
-- is @0.0.0.0\/0@. If you\'ve disabled private endpoint access and you
-- have nodes or AWS Fargate pods in the cluster, then ensure that the
-- necessary CIDR blocks are listed. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/cluster-endpoint.html Amazon EKS Cluster Endpoint Access Control>
-- in the //Amazon EKS User Guide// .
vpcConfigResponse_publicAccessCidrs :: Lens.Lens' VpcConfigResponse (Core.Maybe [Core.Text])
vpcConfigResponse_publicAccessCidrs = Lens.lens (\VpcConfigResponse' {publicAccessCidrs} -> publicAccessCidrs) (\s@VpcConfigResponse' {} a -> s {publicAccessCidrs = a} :: VpcConfigResponse) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON VpcConfigResponse where
  parseJSON =
    Core.withObject
      "VpcConfigResponse"
      ( \x ->
          VpcConfigResponse'
            Core.<$> (x Core..:? "securityGroupIds" Core..!= Core.mempty)
            Core.<*> (x Core..:? "endpointPublicAccess")
            Core.<*> (x Core..:? "subnetIds" Core..!= Core.mempty)
            Core.<*> (x Core..:? "clusterSecurityGroupId")
            Core.<*> (x Core..:? "vpcId")
            Core.<*> (x Core..:? "endpointPrivateAccess")
            Core.<*> ( x Core..:? "publicAccessCidrs"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable VpcConfigResponse

instance Core.NFData VpcConfigResponse
