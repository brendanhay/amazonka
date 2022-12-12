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
-- Module      : Amazonka.EKS.Types.VpcConfigResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.VpcConfigResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object representing an Amazon EKS cluster VPC configuration response.
--
-- /See:/ 'newVpcConfigResponse' smart constructor.
data VpcConfigResponse = VpcConfigResponse'
  { -- | The cluster security group that was created by Amazon EKS for the
    -- cluster. Managed node groups use this security group for
    -- control-plane-to-data-plane communication.
    clusterSecurityGroupId :: Prelude.Maybe Prelude.Text,
    -- | This parameter indicates whether the Amazon EKS private API server
    -- endpoint is enabled. If the Amazon EKS private API server endpoint is
    -- enabled, Kubernetes API requests that originate from within your
    -- cluster\'s VPC use the private VPC endpoint instead of traversing the
    -- internet. If this value is disabled and you have nodes or Fargate pods
    -- in the cluster, then ensure that @publicAccessCidrs@ includes the
    -- necessary CIDR blocks for communication with the nodes or Fargate pods.
    -- For more information, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/cluster-endpoint.html Amazon EKS cluster endpoint access control>
    -- in the //Amazon EKS User Guide// .
    endpointPrivateAccess :: Prelude.Maybe Prelude.Bool,
    -- | This parameter indicates whether the Amazon EKS public API server
    -- endpoint is enabled. If the Amazon EKS public API server endpoint is
    -- disabled, your cluster\'s Kubernetes API server can only receive
    -- requests that originate from within the cluster VPC.
    endpointPublicAccess :: Prelude.Maybe Prelude.Bool,
    -- | The CIDR blocks that are allowed access to your cluster\'s public
    -- Kubernetes API server endpoint. Communication to the endpoint from
    -- addresses outside of the listed CIDR blocks is denied. The default value
    -- is @0.0.0.0\/0@. If you\'ve disabled private endpoint access and you
    -- have nodes or Fargate pods in the cluster, then ensure that the
    -- necessary CIDR blocks are listed. For more information, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/cluster-endpoint.html Amazon EKS cluster endpoint access control>
    -- in the //Amazon EKS User Guide// .
    publicAccessCidrs :: Prelude.Maybe [Prelude.Text],
    -- | The security groups associated with the cross-account elastic network
    -- interfaces that are used to allow communication between your nodes and
    -- the Kubernetes control plane.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The subnets associated with your cluster.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The VPC associated with your cluster.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterSecurityGroupId', 'vpcConfigResponse_clusterSecurityGroupId' - The cluster security group that was created by Amazon EKS for the
-- cluster. Managed node groups use this security group for
-- control-plane-to-data-plane communication.
--
-- 'endpointPrivateAccess', 'vpcConfigResponse_endpointPrivateAccess' - This parameter indicates whether the Amazon EKS private API server
-- endpoint is enabled. If the Amazon EKS private API server endpoint is
-- enabled, Kubernetes API requests that originate from within your
-- cluster\'s VPC use the private VPC endpoint instead of traversing the
-- internet. If this value is disabled and you have nodes or Fargate pods
-- in the cluster, then ensure that @publicAccessCidrs@ includes the
-- necessary CIDR blocks for communication with the nodes or Fargate pods.
-- For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/cluster-endpoint.html Amazon EKS cluster endpoint access control>
-- in the //Amazon EKS User Guide// .
--
-- 'endpointPublicAccess', 'vpcConfigResponse_endpointPublicAccess' - This parameter indicates whether the Amazon EKS public API server
-- endpoint is enabled. If the Amazon EKS public API server endpoint is
-- disabled, your cluster\'s Kubernetes API server can only receive
-- requests that originate from within the cluster VPC.
--
-- 'publicAccessCidrs', 'vpcConfigResponse_publicAccessCidrs' - The CIDR blocks that are allowed access to your cluster\'s public
-- Kubernetes API server endpoint. Communication to the endpoint from
-- addresses outside of the listed CIDR blocks is denied. The default value
-- is @0.0.0.0\/0@. If you\'ve disabled private endpoint access and you
-- have nodes or Fargate pods in the cluster, then ensure that the
-- necessary CIDR blocks are listed. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/cluster-endpoint.html Amazon EKS cluster endpoint access control>
-- in the //Amazon EKS User Guide// .
--
-- 'securityGroupIds', 'vpcConfigResponse_securityGroupIds' - The security groups associated with the cross-account elastic network
-- interfaces that are used to allow communication between your nodes and
-- the Kubernetes control plane.
--
-- 'subnetIds', 'vpcConfigResponse_subnetIds' - The subnets associated with your cluster.
--
-- 'vpcId', 'vpcConfigResponse_vpcId' - The VPC associated with your cluster.
newVpcConfigResponse ::
  VpcConfigResponse
newVpcConfigResponse =
  VpcConfigResponse'
    { clusterSecurityGroupId =
        Prelude.Nothing,
      endpointPrivateAccess = Prelude.Nothing,
      endpointPublicAccess = Prelude.Nothing,
      publicAccessCidrs = Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The cluster security group that was created by Amazon EKS for the
-- cluster. Managed node groups use this security group for
-- control-plane-to-data-plane communication.
vpcConfigResponse_clusterSecurityGroupId :: Lens.Lens' VpcConfigResponse (Prelude.Maybe Prelude.Text)
vpcConfigResponse_clusterSecurityGroupId = Lens.lens (\VpcConfigResponse' {clusterSecurityGroupId} -> clusterSecurityGroupId) (\s@VpcConfigResponse' {} a -> s {clusterSecurityGroupId = a} :: VpcConfigResponse)

-- | This parameter indicates whether the Amazon EKS private API server
-- endpoint is enabled. If the Amazon EKS private API server endpoint is
-- enabled, Kubernetes API requests that originate from within your
-- cluster\'s VPC use the private VPC endpoint instead of traversing the
-- internet. If this value is disabled and you have nodes or Fargate pods
-- in the cluster, then ensure that @publicAccessCidrs@ includes the
-- necessary CIDR blocks for communication with the nodes or Fargate pods.
-- For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/cluster-endpoint.html Amazon EKS cluster endpoint access control>
-- in the //Amazon EKS User Guide// .
vpcConfigResponse_endpointPrivateAccess :: Lens.Lens' VpcConfigResponse (Prelude.Maybe Prelude.Bool)
vpcConfigResponse_endpointPrivateAccess = Lens.lens (\VpcConfigResponse' {endpointPrivateAccess} -> endpointPrivateAccess) (\s@VpcConfigResponse' {} a -> s {endpointPrivateAccess = a} :: VpcConfigResponse)

-- | This parameter indicates whether the Amazon EKS public API server
-- endpoint is enabled. If the Amazon EKS public API server endpoint is
-- disabled, your cluster\'s Kubernetes API server can only receive
-- requests that originate from within the cluster VPC.
vpcConfigResponse_endpointPublicAccess :: Lens.Lens' VpcConfigResponse (Prelude.Maybe Prelude.Bool)
vpcConfigResponse_endpointPublicAccess = Lens.lens (\VpcConfigResponse' {endpointPublicAccess} -> endpointPublicAccess) (\s@VpcConfigResponse' {} a -> s {endpointPublicAccess = a} :: VpcConfigResponse)

-- | The CIDR blocks that are allowed access to your cluster\'s public
-- Kubernetes API server endpoint. Communication to the endpoint from
-- addresses outside of the listed CIDR blocks is denied. The default value
-- is @0.0.0.0\/0@. If you\'ve disabled private endpoint access and you
-- have nodes or Fargate pods in the cluster, then ensure that the
-- necessary CIDR blocks are listed. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/cluster-endpoint.html Amazon EKS cluster endpoint access control>
-- in the //Amazon EKS User Guide// .
vpcConfigResponse_publicAccessCidrs :: Lens.Lens' VpcConfigResponse (Prelude.Maybe [Prelude.Text])
vpcConfigResponse_publicAccessCidrs = Lens.lens (\VpcConfigResponse' {publicAccessCidrs} -> publicAccessCidrs) (\s@VpcConfigResponse' {} a -> s {publicAccessCidrs = a} :: VpcConfigResponse) Prelude.. Lens.mapping Lens.coerced

-- | The security groups associated with the cross-account elastic network
-- interfaces that are used to allow communication between your nodes and
-- the Kubernetes control plane.
vpcConfigResponse_securityGroupIds :: Lens.Lens' VpcConfigResponse (Prelude.Maybe [Prelude.Text])
vpcConfigResponse_securityGroupIds = Lens.lens (\VpcConfigResponse' {securityGroupIds} -> securityGroupIds) (\s@VpcConfigResponse' {} a -> s {securityGroupIds = a} :: VpcConfigResponse) Prelude.. Lens.mapping Lens.coerced

-- | The subnets associated with your cluster.
vpcConfigResponse_subnetIds :: Lens.Lens' VpcConfigResponse (Prelude.Maybe [Prelude.Text])
vpcConfigResponse_subnetIds = Lens.lens (\VpcConfigResponse' {subnetIds} -> subnetIds) (\s@VpcConfigResponse' {} a -> s {subnetIds = a} :: VpcConfigResponse) Prelude.. Lens.mapping Lens.coerced

-- | The VPC associated with your cluster.
vpcConfigResponse_vpcId :: Lens.Lens' VpcConfigResponse (Prelude.Maybe Prelude.Text)
vpcConfigResponse_vpcId = Lens.lens (\VpcConfigResponse' {vpcId} -> vpcId) (\s@VpcConfigResponse' {} a -> s {vpcId = a} :: VpcConfigResponse)

instance Data.FromJSON VpcConfigResponse where
  parseJSON =
    Data.withObject
      "VpcConfigResponse"
      ( \x ->
          VpcConfigResponse'
            Prelude.<$> (x Data..:? "clusterSecurityGroupId")
            Prelude.<*> (x Data..:? "endpointPrivateAccess")
            Prelude.<*> (x Data..:? "endpointPublicAccess")
            Prelude.<*> ( x Data..:? "publicAccessCidrs"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "securityGroupIds"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "subnetIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "vpcId")
      )

instance Prelude.Hashable VpcConfigResponse where
  hashWithSalt _salt VpcConfigResponse' {..} =
    _salt `Prelude.hashWithSalt` clusterSecurityGroupId
      `Prelude.hashWithSalt` endpointPrivateAccess
      `Prelude.hashWithSalt` endpointPublicAccess
      `Prelude.hashWithSalt` publicAccessCidrs
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData VpcConfigResponse where
  rnf VpcConfigResponse' {..} =
    Prelude.rnf clusterSecurityGroupId
      `Prelude.seq` Prelude.rnf endpointPrivateAccess
      `Prelude.seq` Prelude.rnf endpointPublicAccess
      `Prelude.seq` Prelude.rnf publicAccessCidrs
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf vpcId
