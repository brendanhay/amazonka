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
-- Module      : Amazonka.EKS.Types.Issue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.Issue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types.NodegroupIssueCode
import qualified Amazonka.Prelude as Prelude

-- | An object representing an issue with an Amazon EKS resource.
--
-- /See:/ 'newIssue' smart constructor.
data Issue = Issue'
  { -- | A brief description of the error.
    --
    -- -   __AccessDenied__: Amazon EKS or one or more of your managed nodes is
    --     failing to authenticate or authorize with your Kubernetes cluster
    --     API server.
    --
    -- -   __AsgInstanceLaunchFailures__: Your Auto Scaling group is
    --     experiencing failures while attempting to launch instances.
    --
    -- -   __AutoScalingGroupNotFound__: We couldn\'t find the Auto Scaling
    --     group associated with the managed node group. You may be able to
    --     recreate an Auto Scaling group with the same settings to recover.
    --
    -- -   __ClusterUnreachable__: Amazon EKS or one or more of your managed
    --     nodes is unable to to communicate with your Kubernetes cluster API
    --     server. This can happen if there are network disruptions or if API
    --     servers are timing out processing requests.
    --
    -- -   __Ec2LaunchTemplateNotFound__: We couldn\'t find the Amazon EC2
    --     launch template for your managed node group. You may be able to
    --     recreate a launch template with the same settings to recover.
    --
    -- -   __Ec2LaunchTemplateVersionMismatch__: The Amazon EC2 launch template
    --     version for your managed node group does not match the version that
    --     Amazon EKS created. You may be able to revert to the version that
    --     Amazon EKS created to recover.
    --
    -- -   __Ec2SecurityGroupDeletionFailure__: We could not delete the remote
    --     access security group for your managed node group. Remove any
    --     dependencies from the security group.
    --
    -- -   __Ec2SecurityGroupNotFound__: We couldn\'t find the cluster security
    --     group for the cluster. You must recreate your cluster.
    --
    -- -   __Ec2SubnetInvalidConfiguration__: One or more Amazon EC2 subnets
    --     specified for a node group do not automatically assign public IP
    --     addresses to instances launched into it. If you want your instances
    --     to be assigned a public IP address, then you need to enable the
    --     @auto-assign public IP address@ setting for the subnet. See
    --     <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-ip-addressing.html#subnet-public-ip Modifying the public IPv4 addressing attribute for your subnet>
    --     in the /Amazon VPC User Guide/.
    --
    -- -   __IamInstanceProfileNotFound__: We couldn\'t find the IAM instance
    --     profile for your managed node group. You may be able to recreate an
    --     instance profile with the same settings to recover.
    --
    -- -   __IamNodeRoleNotFound__: We couldn\'t find the IAM role for your
    --     managed node group. You may be able to recreate an IAM role with the
    --     same settings to recover.
    --
    -- -   __InstanceLimitExceeded__: Your Amazon Web Services account is
    --     unable to launch any more instances of the specified instance type.
    --     You may be able to request an Amazon EC2 instance limit increase to
    --     recover.
    --
    -- -   __InsufficientFreeAddresses__: One or more of the subnets associated
    --     with your managed node group does not have enough available IP
    --     addresses for new nodes.
    --
    -- -   __InternalFailure__: These errors are usually caused by an Amazon
    --     EKS server-side issue.
    --
    -- -   __NodeCreationFailure__: Your launched instances are unable to
    --     register with your Amazon EKS cluster. Common causes of this failure
    --     are insufficient
    --     <https://docs.aws.amazon.com/eks/latest/userguide/create-node-role.html node IAM role>
    --     permissions or lack of outbound internet access for the nodes.
    code :: Prelude.Maybe NodegroupIssueCode,
    -- | The error message associated with the issue.
    message :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services resources that are afflicted by this issue.
    resourceIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Issue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'issue_code' - A brief description of the error.
--
-- -   __AccessDenied__: Amazon EKS or one or more of your managed nodes is
--     failing to authenticate or authorize with your Kubernetes cluster
--     API server.
--
-- -   __AsgInstanceLaunchFailures__: Your Auto Scaling group is
--     experiencing failures while attempting to launch instances.
--
-- -   __AutoScalingGroupNotFound__: We couldn\'t find the Auto Scaling
--     group associated with the managed node group. You may be able to
--     recreate an Auto Scaling group with the same settings to recover.
--
-- -   __ClusterUnreachable__: Amazon EKS or one or more of your managed
--     nodes is unable to to communicate with your Kubernetes cluster API
--     server. This can happen if there are network disruptions or if API
--     servers are timing out processing requests.
--
-- -   __Ec2LaunchTemplateNotFound__: We couldn\'t find the Amazon EC2
--     launch template for your managed node group. You may be able to
--     recreate a launch template with the same settings to recover.
--
-- -   __Ec2LaunchTemplateVersionMismatch__: The Amazon EC2 launch template
--     version for your managed node group does not match the version that
--     Amazon EKS created. You may be able to revert to the version that
--     Amazon EKS created to recover.
--
-- -   __Ec2SecurityGroupDeletionFailure__: We could not delete the remote
--     access security group for your managed node group. Remove any
--     dependencies from the security group.
--
-- -   __Ec2SecurityGroupNotFound__: We couldn\'t find the cluster security
--     group for the cluster. You must recreate your cluster.
--
-- -   __Ec2SubnetInvalidConfiguration__: One or more Amazon EC2 subnets
--     specified for a node group do not automatically assign public IP
--     addresses to instances launched into it. If you want your instances
--     to be assigned a public IP address, then you need to enable the
--     @auto-assign public IP address@ setting for the subnet. See
--     <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-ip-addressing.html#subnet-public-ip Modifying the public IPv4 addressing attribute for your subnet>
--     in the /Amazon VPC User Guide/.
--
-- -   __IamInstanceProfileNotFound__: We couldn\'t find the IAM instance
--     profile for your managed node group. You may be able to recreate an
--     instance profile with the same settings to recover.
--
-- -   __IamNodeRoleNotFound__: We couldn\'t find the IAM role for your
--     managed node group. You may be able to recreate an IAM role with the
--     same settings to recover.
--
-- -   __InstanceLimitExceeded__: Your Amazon Web Services account is
--     unable to launch any more instances of the specified instance type.
--     You may be able to request an Amazon EC2 instance limit increase to
--     recover.
--
-- -   __InsufficientFreeAddresses__: One or more of the subnets associated
--     with your managed node group does not have enough available IP
--     addresses for new nodes.
--
-- -   __InternalFailure__: These errors are usually caused by an Amazon
--     EKS server-side issue.
--
-- -   __NodeCreationFailure__: Your launched instances are unable to
--     register with your Amazon EKS cluster. Common causes of this failure
--     are insufficient
--     <https://docs.aws.amazon.com/eks/latest/userguide/create-node-role.html node IAM role>
--     permissions or lack of outbound internet access for the nodes.
--
-- 'message', 'issue_message' - The error message associated with the issue.
--
-- 'resourceIds', 'issue_resourceIds' - The Amazon Web Services resources that are afflicted by this issue.
newIssue ::
  Issue
newIssue =
  Issue'
    { code = Prelude.Nothing,
      message = Prelude.Nothing,
      resourceIds = Prelude.Nothing
    }

-- | A brief description of the error.
--
-- -   __AccessDenied__: Amazon EKS or one or more of your managed nodes is
--     failing to authenticate or authorize with your Kubernetes cluster
--     API server.
--
-- -   __AsgInstanceLaunchFailures__: Your Auto Scaling group is
--     experiencing failures while attempting to launch instances.
--
-- -   __AutoScalingGroupNotFound__: We couldn\'t find the Auto Scaling
--     group associated with the managed node group. You may be able to
--     recreate an Auto Scaling group with the same settings to recover.
--
-- -   __ClusterUnreachable__: Amazon EKS or one or more of your managed
--     nodes is unable to to communicate with your Kubernetes cluster API
--     server. This can happen if there are network disruptions or if API
--     servers are timing out processing requests.
--
-- -   __Ec2LaunchTemplateNotFound__: We couldn\'t find the Amazon EC2
--     launch template for your managed node group. You may be able to
--     recreate a launch template with the same settings to recover.
--
-- -   __Ec2LaunchTemplateVersionMismatch__: The Amazon EC2 launch template
--     version for your managed node group does not match the version that
--     Amazon EKS created. You may be able to revert to the version that
--     Amazon EKS created to recover.
--
-- -   __Ec2SecurityGroupDeletionFailure__: We could not delete the remote
--     access security group for your managed node group. Remove any
--     dependencies from the security group.
--
-- -   __Ec2SecurityGroupNotFound__: We couldn\'t find the cluster security
--     group for the cluster. You must recreate your cluster.
--
-- -   __Ec2SubnetInvalidConfiguration__: One or more Amazon EC2 subnets
--     specified for a node group do not automatically assign public IP
--     addresses to instances launched into it. If you want your instances
--     to be assigned a public IP address, then you need to enable the
--     @auto-assign public IP address@ setting for the subnet. See
--     <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-ip-addressing.html#subnet-public-ip Modifying the public IPv4 addressing attribute for your subnet>
--     in the /Amazon VPC User Guide/.
--
-- -   __IamInstanceProfileNotFound__: We couldn\'t find the IAM instance
--     profile for your managed node group. You may be able to recreate an
--     instance profile with the same settings to recover.
--
-- -   __IamNodeRoleNotFound__: We couldn\'t find the IAM role for your
--     managed node group. You may be able to recreate an IAM role with the
--     same settings to recover.
--
-- -   __InstanceLimitExceeded__: Your Amazon Web Services account is
--     unable to launch any more instances of the specified instance type.
--     You may be able to request an Amazon EC2 instance limit increase to
--     recover.
--
-- -   __InsufficientFreeAddresses__: One or more of the subnets associated
--     with your managed node group does not have enough available IP
--     addresses for new nodes.
--
-- -   __InternalFailure__: These errors are usually caused by an Amazon
--     EKS server-side issue.
--
-- -   __NodeCreationFailure__: Your launched instances are unable to
--     register with your Amazon EKS cluster. Common causes of this failure
--     are insufficient
--     <https://docs.aws.amazon.com/eks/latest/userguide/create-node-role.html node IAM role>
--     permissions or lack of outbound internet access for the nodes.
issue_code :: Lens.Lens' Issue (Prelude.Maybe NodegroupIssueCode)
issue_code = Lens.lens (\Issue' {code} -> code) (\s@Issue' {} a -> s {code = a} :: Issue)

-- | The error message associated with the issue.
issue_message :: Lens.Lens' Issue (Prelude.Maybe Prelude.Text)
issue_message = Lens.lens (\Issue' {message} -> message) (\s@Issue' {} a -> s {message = a} :: Issue)

-- | The Amazon Web Services resources that are afflicted by this issue.
issue_resourceIds :: Lens.Lens' Issue (Prelude.Maybe [Prelude.Text])
issue_resourceIds = Lens.lens (\Issue' {resourceIds} -> resourceIds) (\s@Issue' {} a -> s {resourceIds = a} :: Issue) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Issue where
  parseJSON =
    Data.withObject
      "Issue"
      ( \x ->
          Issue'
            Prelude.<$> (x Data..:? "code")
            Prelude.<*> (x Data..:? "message")
            Prelude.<*> (x Data..:? "resourceIds" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Issue where
  hashWithSalt _salt Issue' {..} =
    _salt `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` resourceIds

instance Prelude.NFData Issue where
  rnf Issue' {..} =
    Prelude.rnf code
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf resourceIds
