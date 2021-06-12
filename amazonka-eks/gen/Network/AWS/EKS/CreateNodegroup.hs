{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EKS.CreateNodegroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a managed node group for an Amazon EKS cluster. You can only
-- create a node group for your cluster that is equal to the current
-- Kubernetes version for the cluster. All node groups are created with the
-- latest AMI release version for the respective minor Kubernetes version
-- of the cluster, unless you deploy a custom AMI using a launch template.
-- For more information about using launch templates, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>.
--
-- An Amazon EKS managed node group is an Amazon EC2 Auto Scaling group and
-- associated Amazon EC2 instances that are managed by AWS for an Amazon
-- EKS cluster. Each node group uses a version of the Amazon EKS optimized
-- Amazon Linux 2 AMI. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/managed-node-groups.html Managed Node Groups>
-- in the /Amazon EKS User Guide/.
module Network.AWS.EKS.CreateNodegroup
  ( -- * Creating a Request
    CreateNodegroup (..),
    newCreateNodegroup,

    -- * Request Lenses
    createNodegroup_scalingConfig,
    createNodegroup_capacityType,
    createNodegroup_releaseVersion,
    createNodegroup_diskSize,
    createNodegroup_remoteAccess,
    createNodegroup_launchTemplate,
    createNodegroup_labels,
    createNodegroup_version,
    createNodegroup_tags,
    createNodegroup_clientRequestToken,
    createNodegroup_amiType,
    createNodegroup_instanceTypes,
    createNodegroup_clusterName,
    createNodegroup_nodegroupName,
    createNodegroup_subnets,
    createNodegroup_nodeRole,

    -- * Destructuring the Response
    CreateNodegroupResponse (..),
    newCreateNodegroupResponse,

    -- * Response Lenses
    createNodegroupResponse_nodegroup,
    createNodegroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateNodegroup' smart constructor.
data CreateNodegroup = CreateNodegroup'
  { -- | The scaling configuration details for the Auto Scaling group that is
    -- created for your node group.
    scalingConfig :: Core.Maybe NodegroupScalingConfig,
    -- | The capacity type for your node group.
    capacityType :: Core.Maybe CapacityTypes,
    -- | The AMI version of the Amazon EKS optimized AMI to use with your node
    -- group. By default, the latest available AMI version for the node
    -- group\'s current Kubernetes version is used. For more information, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/eks-linux-ami-versions.html Amazon EKS optimized Amazon Linux 2 AMI versions>
    -- in the /Amazon EKS User Guide/. If you specify @launchTemplate@, and
    -- your launch template uses a custom AMI, then don\'t specify
    -- @releaseVersion@, or the node group deployment will fail. For more
    -- information about using launch templates with Amazon EKS, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
    -- in the Amazon EKS User Guide.
    releaseVersion :: Core.Maybe Core.Text,
    -- | The root device disk size (in GiB) for your node group instances. The
    -- default disk size is 20 GiB. If you specify @launchTemplate@, then
    -- don\'t specify @diskSize@, or the node group deployment will fail. For
    -- more information about using launch templates with Amazon EKS, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
    -- in the Amazon EKS User Guide.
    diskSize :: Core.Maybe Core.Int,
    -- | The remote access (SSH) configuration to use with your node group. If
    -- you specify @launchTemplate@, then don\'t specify @remoteAccess@, or the
    -- node group deployment will fail. For more information about using launch
    -- templates with Amazon EKS, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
    -- in the Amazon EKS User Guide.
    remoteAccess :: Core.Maybe RemoteAccessConfig,
    -- | An object representing a node group\'s launch template specification. If
    -- specified, then do not specify @instanceTypes@, @diskSize@, or
    -- @remoteAccess@ and make sure that the launch template meets the
    -- requirements in @launchTemplateSpecification@.
    launchTemplate :: Core.Maybe LaunchTemplateSpecification,
    -- | The Kubernetes labels to be applied to the nodes in the node group when
    -- they are created.
    labels :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The Kubernetes version to use for your managed nodes. By default, the
    -- Kubernetes version of the cluster is used, and this is the only accepted
    -- specified value. If you specify @launchTemplate@, and your launch
    -- template uses a custom AMI, then don\'t specify @version@, or the node
    -- group deployment will fail. For more information about using launch
    -- templates with Amazon EKS, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
    -- in the Amazon EKS User Guide.
    version :: Core.Maybe Core.Text,
    -- | The metadata to apply to the node group to assist with categorization
    -- and organization. Each tag consists of a key and an optional value, both
    -- of which you define. Node group tags do not propagate to any other
    -- resources associated with the node group, such as the Amazon EC2
    -- instances or subnets.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientRequestToken :: Core.Maybe Core.Text,
    -- | The AMI type for your node group. GPU instance types should use the
    -- @AL2_x86_64_GPU@ AMI type. Non-GPU instances should use the @AL2_x86_64@
    -- AMI type. Arm instances should use the @AL2_ARM_64@ AMI type. All types
    -- use the Amazon EKS optimized Amazon Linux 2 AMI. If you specify
    -- @launchTemplate@, and your launch template uses a custom AMI, then
    -- don\'t specify @amiType@, or the node group deployment will fail. For
    -- more information about using launch templates with Amazon EKS, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
    -- in the Amazon EKS User Guide.
    amiType :: Core.Maybe AMITypes,
    -- | Specify the instance types for a node group. If you specify a GPU
    -- instance type, be sure to specify @AL2_x86_64_GPU@ with the @amiType@
    -- parameter. If you specify @launchTemplate@, then you can specify zero or
    -- one instance type in your launch template /or/ you can specify 0-20
    -- instance types for @instanceTypes@. If however, you specify an instance
    -- type in your launch template /and/ specify any @instanceTypes@, the node
    -- group deployment will fail. If you don\'t specify an instance type in a
    -- launch template or for @instanceTypes@, then @t3.medium@ is used, by
    -- default. If you specify @Spot@ for @capacityType@, then we recommend
    -- specifying multiple values for @instanceTypes@. For more information,
    -- see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/managed-node-groups.html#managed-node-group-capacity-types Managed node group capacity types>
    -- and
    -- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
    -- in the /Amazon EKS User Guide/.
    instanceTypes :: Core.Maybe [Core.Text],
    -- | The name of the cluster to create the node group in.
    clusterName :: Core.Text,
    -- | The unique name to give your node group.
    nodegroupName :: Core.Text,
    -- | The subnets to use for the Auto Scaling group that is created for your
    -- node group. These subnets must have the tag key
    -- @kubernetes.io\/cluster\/CLUSTER_NAME@ with a value of @shared@, where
    -- @CLUSTER_NAME@ is replaced with the name of your cluster. If you specify
    -- @launchTemplate@, then don\'t specify
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateNetworkInterface.html SubnetId>
    -- in your launch template, or the node group deployment will fail. For
    -- more information about using launch templates with Amazon EKS, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
    -- in the Amazon EKS User Guide.
    subnets :: [Core.Text],
    -- | The Amazon Resource Name (ARN) of the IAM role to associate with your
    -- node group. The Amazon EKS worker node @kubelet@ daemon makes calls to
    -- AWS APIs on your behalf. Nodes receive permissions for these API calls
    -- through an IAM instance profile and associated policies. Before you can
    -- launch nodes and register them into a cluster, you must create an IAM
    -- role for those nodes to use when they are launched. For more
    -- information, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/worker_node_IAM_role.html Amazon EKS node IAM role>
    -- in the //Amazon EKS User Guide// . If you specify @launchTemplate@, then
    -- don\'t specify
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_IamInstanceProfile.html IamInstanceProfile>
    -- in your launch template, or the node group deployment will fail. For
    -- more information about using launch templates with Amazon EKS, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
    -- in the Amazon EKS User Guide.
    nodeRole :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateNodegroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scalingConfig', 'createNodegroup_scalingConfig' - The scaling configuration details for the Auto Scaling group that is
-- created for your node group.
--
-- 'capacityType', 'createNodegroup_capacityType' - The capacity type for your node group.
--
-- 'releaseVersion', 'createNodegroup_releaseVersion' - The AMI version of the Amazon EKS optimized AMI to use with your node
-- group. By default, the latest available AMI version for the node
-- group\'s current Kubernetes version is used. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/eks-linux-ami-versions.html Amazon EKS optimized Amazon Linux 2 AMI versions>
-- in the /Amazon EKS User Guide/. If you specify @launchTemplate@, and
-- your launch template uses a custom AMI, then don\'t specify
-- @releaseVersion@, or the node group deployment will fail. For more
-- information about using launch templates with Amazon EKS, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the Amazon EKS User Guide.
--
-- 'diskSize', 'createNodegroup_diskSize' - The root device disk size (in GiB) for your node group instances. The
-- default disk size is 20 GiB. If you specify @launchTemplate@, then
-- don\'t specify @diskSize@, or the node group deployment will fail. For
-- more information about using launch templates with Amazon EKS, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the Amazon EKS User Guide.
--
-- 'remoteAccess', 'createNodegroup_remoteAccess' - The remote access (SSH) configuration to use with your node group. If
-- you specify @launchTemplate@, then don\'t specify @remoteAccess@, or the
-- node group deployment will fail. For more information about using launch
-- templates with Amazon EKS, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the Amazon EKS User Guide.
--
-- 'launchTemplate', 'createNodegroup_launchTemplate' - An object representing a node group\'s launch template specification. If
-- specified, then do not specify @instanceTypes@, @diskSize@, or
-- @remoteAccess@ and make sure that the launch template meets the
-- requirements in @launchTemplateSpecification@.
--
-- 'labels', 'createNodegroup_labels' - The Kubernetes labels to be applied to the nodes in the node group when
-- they are created.
--
-- 'version', 'createNodegroup_version' - The Kubernetes version to use for your managed nodes. By default, the
-- Kubernetes version of the cluster is used, and this is the only accepted
-- specified value. If you specify @launchTemplate@, and your launch
-- template uses a custom AMI, then don\'t specify @version@, or the node
-- group deployment will fail. For more information about using launch
-- templates with Amazon EKS, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the Amazon EKS User Guide.
--
-- 'tags', 'createNodegroup_tags' - The metadata to apply to the node group to assist with categorization
-- and organization. Each tag consists of a key and an optional value, both
-- of which you define. Node group tags do not propagate to any other
-- resources associated with the node group, such as the Amazon EC2
-- instances or subnets.
--
-- 'clientRequestToken', 'createNodegroup_clientRequestToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'amiType', 'createNodegroup_amiType' - The AMI type for your node group. GPU instance types should use the
-- @AL2_x86_64_GPU@ AMI type. Non-GPU instances should use the @AL2_x86_64@
-- AMI type. Arm instances should use the @AL2_ARM_64@ AMI type. All types
-- use the Amazon EKS optimized Amazon Linux 2 AMI. If you specify
-- @launchTemplate@, and your launch template uses a custom AMI, then
-- don\'t specify @amiType@, or the node group deployment will fail. For
-- more information about using launch templates with Amazon EKS, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the Amazon EKS User Guide.
--
-- 'instanceTypes', 'createNodegroup_instanceTypes' - Specify the instance types for a node group. If you specify a GPU
-- instance type, be sure to specify @AL2_x86_64_GPU@ with the @amiType@
-- parameter. If you specify @launchTemplate@, then you can specify zero or
-- one instance type in your launch template /or/ you can specify 0-20
-- instance types for @instanceTypes@. If however, you specify an instance
-- type in your launch template /and/ specify any @instanceTypes@, the node
-- group deployment will fail. If you don\'t specify an instance type in a
-- launch template or for @instanceTypes@, then @t3.medium@ is used, by
-- default. If you specify @Spot@ for @capacityType@, then we recommend
-- specifying multiple values for @instanceTypes@. For more information,
-- see
-- <https://docs.aws.amazon.com/eks/latest/userguide/managed-node-groups.html#managed-node-group-capacity-types Managed node group capacity types>
-- and
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the /Amazon EKS User Guide/.
--
-- 'clusterName', 'createNodegroup_clusterName' - The name of the cluster to create the node group in.
--
-- 'nodegroupName', 'createNodegroup_nodegroupName' - The unique name to give your node group.
--
-- 'subnets', 'createNodegroup_subnets' - The subnets to use for the Auto Scaling group that is created for your
-- node group. These subnets must have the tag key
-- @kubernetes.io\/cluster\/CLUSTER_NAME@ with a value of @shared@, where
-- @CLUSTER_NAME@ is replaced with the name of your cluster. If you specify
-- @launchTemplate@, then don\'t specify
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateNetworkInterface.html SubnetId>
-- in your launch template, or the node group deployment will fail. For
-- more information about using launch templates with Amazon EKS, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the Amazon EKS User Guide.
--
-- 'nodeRole', 'createNodegroup_nodeRole' - The Amazon Resource Name (ARN) of the IAM role to associate with your
-- node group. The Amazon EKS worker node @kubelet@ daemon makes calls to
-- AWS APIs on your behalf. Nodes receive permissions for these API calls
-- through an IAM instance profile and associated policies. Before you can
-- launch nodes and register them into a cluster, you must create an IAM
-- role for those nodes to use when they are launched. For more
-- information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/worker_node_IAM_role.html Amazon EKS node IAM role>
-- in the //Amazon EKS User Guide// . If you specify @launchTemplate@, then
-- don\'t specify
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_IamInstanceProfile.html IamInstanceProfile>
-- in your launch template, or the node group deployment will fail. For
-- more information about using launch templates with Amazon EKS, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the Amazon EKS User Guide.
newCreateNodegroup ::
  -- | 'clusterName'
  Core.Text ->
  -- | 'nodegroupName'
  Core.Text ->
  -- | 'nodeRole'
  Core.Text ->
  CreateNodegroup
newCreateNodegroup
  pClusterName_
  pNodegroupName_
  pNodeRole_ =
    CreateNodegroup'
      { scalingConfig = Core.Nothing,
        capacityType = Core.Nothing,
        releaseVersion = Core.Nothing,
        diskSize = Core.Nothing,
        remoteAccess = Core.Nothing,
        launchTemplate = Core.Nothing,
        labels = Core.Nothing,
        version = Core.Nothing,
        tags = Core.Nothing,
        clientRequestToken = Core.Nothing,
        amiType = Core.Nothing,
        instanceTypes = Core.Nothing,
        clusterName = pClusterName_,
        nodegroupName = pNodegroupName_,
        subnets = Core.mempty,
        nodeRole = pNodeRole_
      }

-- | The scaling configuration details for the Auto Scaling group that is
-- created for your node group.
createNodegroup_scalingConfig :: Lens.Lens' CreateNodegroup (Core.Maybe NodegroupScalingConfig)
createNodegroup_scalingConfig = Lens.lens (\CreateNodegroup' {scalingConfig} -> scalingConfig) (\s@CreateNodegroup' {} a -> s {scalingConfig = a} :: CreateNodegroup)

-- | The capacity type for your node group.
createNodegroup_capacityType :: Lens.Lens' CreateNodegroup (Core.Maybe CapacityTypes)
createNodegroup_capacityType = Lens.lens (\CreateNodegroup' {capacityType} -> capacityType) (\s@CreateNodegroup' {} a -> s {capacityType = a} :: CreateNodegroup)

-- | The AMI version of the Amazon EKS optimized AMI to use with your node
-- group. By default, the latest available AMI version for the node
-- group\'s current Kubernetes version is used. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/eks-linux-ami-versions.html Amazon EKS optimized Amazon Linux 2 AMI versions>
-- in the /Amazon EKS User Guide/. If you specify @launchTemplate@, and
-- your launch template uses a custom AMI, then don\'t specify
-- @releaseVersion@, or the node group deployment will fail. For more
-- information about using launch templates with Amazon EKS, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the Amazon EKS User Guide.
createNodegroup_releaseVersion :: Lens.Lens' CreateNodegroup (Core.Maybe Core.Text)
createNodegroup_releaseVersion = Lens.lens (\CreateNodegroup' {releaseVersion} -> releaseVersion) (\s@CreateNodegroup' {} a -> s {releaseVersion = a} :: CreateNodegroup)

-- | The root device disk size (in GiB) for your node group instances. The
-- default disk size is 20 GiB. If you specify @launchTemplate@, then
-- don\'t specify @diskSize@, or the node group deployment will fail. For
-- more information about using launch templates with Amazon EKS, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the Amazon EKS User Guide.
createNodegroup_diskSize :: Lens.Lens' CreateNodegroup (Core.Maybe Core.Int)
createNodegroup_diskSize = Lens.lens (\CreateNodegroup' {diskSize} -> diskSize) (\s@CreateNodegroup' {} a -> s {diskSize = a} :: CreateNodegroup)

-- | The remote access (SSH) configuration to use with your node group. If
-- you specify @launchTemplate@, then don\'t specify @remoteAccess@, or the
-- node group deployment will fail. For more information about using launch
-- templates with Amazon EKS, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the Amazon EKS User Guide.
createNodegroup_remoteAccess :: Lens.Lens' CreateNodegroup (Core.Maybe RemoteAccessConfig)
createNodegroup_remoteAccess = Lens.lens (\CreateNodegroup' {remoteAccess} -> remoteAccess) (\s@CreateNodegroup' {} a -> s {remoteAccess = a} :: CreateNodegroup)

-- | An object representing a node group\'s launch template specification. If
-- specified, then do not specify @instanceTypes@, @diskSize@, or
-- @remoteAccess@ and make sure that the launch template meets the
-- requirements in @launchTemplateSpecification@.
createNodegroup_launchTemplate :: Lens.Lens' CreateNodegroup (Core.Maybe LaunchTemplateSpecification)
createNodegroup_launchTemplate = Lens.lens (\CreateNodegroup' {launchTemplate} -> launchTemplate) (\s@CreateNodegroup' {} a -> s {launchTemplate = a} :: CreateNodegroup)

-- | The Kubernetes labels to be applied to the nodes in the node group when
-- they are created.
createNodegroup_labels :: Lens.Lens' CreateNodegroup (Core.Maybe (Core.HashMap Core.Text Core.Text))
createNodegroup_labels = Lens.lens (\CreateNodegroup' {labels} -> labels) (\s@CreateNodegroup' {} a -> s {labels = a} :: CreateNodegroup) Core.. Lens.mapping Lens._Coerce

-- | The Kubernetes version to use for your managed nodes. By default, the
-- Kubernetes version of the cluster is used, and this is the only accepted
-- specified value. If you specify @launchTemplate@, and your launch
-- template uses a custom AMI, then don\'t specify @version@, or the node
-- group deployment will fail. For more information about using launch
-- templates with Amazon EKS, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the Amazon EKS User Guide.
createNodegroup_version :: Lens.Lens' CreateNodegroup (Core.Maybe Core.Text)
createNodegroup_version = Lens.lens (\CreateNodegroup' {version} -> version) (\s@CreateNodegroup' {} a -> s {version = a} :: CreateNodegroup)

-- | The metadata to apply to the node group to assist with categorization
-- and organization. Each tag consists of a key and an optional value, both
-- of which you define. Node group tags do not propagate to any other
-- resources associated with the node group, such as the Amazon EC2
-- instances or subnets.
createNodegroup_tags :: Lens.Lens' CreateNodegroup (Core.Maybe (Core.HashMap Core.Text Core.Text))
createNodegroup_tags = Lens.lens (\CreateNodegroup' {tags} -> tags) (\s@CreateNodegroup' {} a -> s {tags = a} :: CreateNodegroup) Core.. Lens.mapping Lens._Coerce

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createNodegroup_clientRequestToken :: Lens.Lens' CreateNodegroup (Core.Maybe Core.Text)
createNodegroup_clientRequestToken = Lens.lens (\CreateNodegroup' {clientRequestToken} -> clientRequestToken) (\s@CreateNodegroup' {} a -> s {clientRequestToken = a} :: CreateNodegroup)

-- | The AMI type for your node group. GPU instance types should use the
-- @AL2_x86_64_GPU@ AMI type. Non-GPU instances should use the @AL2_x86_64@
-- AMI type. Arm instances should use the @AL2_ARM_64@ AMI type. All types
-- use the Amazon EKS optimized Amazon Linux 2 AMI. If you specify
-- @launchTemplate@, and your launch template uses a custom AMI, then
-- don\'t specify @amiType@, or the node group deployment will fail. For
-- more information about using launch templates with Amazon EKS, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the Amazon EKS User Guide.
createNodegroup_amiType :: Lens.Lens' CreateNodegroup (Core.Maybe AMITypes)
createNodegroup_amiType = Lens.lens (\CreateNodegroup' {amiType} -> amiType) (\s@CreateNodegroup' {} a -> s {amiType = a} :: CreateNodegroup)

-- | Specify the instance types for a node group. If you specify a GPU
-- instance type, be sure to specify @AL2_x86_64_GPU@ with the @amiType@
-- parameter. If you specify @launchTemplate@, then you can specify zero or
-- one instance type in your launch template /or/ you can specify 0-20
-- instance types for @instanceTypes@. If however, you specify an instance
-- type in your launch template /and/ specify any @instanceTypes@, the node
-- group deployment will fail. If you don\'t specify an instance type in a
-- launch template or for @instanceTypes@, then @t3.medium@ is used, by
-- default. If you specify @Spot@ for @capacityType@, then we recommend
-- specifying multiple values for @instanceTypes@. For more information,
-- see
-- <https://docs.aws.amazon.com/eks/latest/userguide/managed-node-groups.html#managed-node-group-capacity-types Managed node group capacity types>
-- and
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the /Amazon EKS User Guide/.
createNodegroup_instanceTypes :: Lens.Lens' CreateNodegroup (Core.Maybe [Core.Text])
createNodegroup_instanceTypes = Lens.lens (\CreateNodegroup' {instanceTypes} -> instanceTypes) (\s@CreateNodegroup' {} a -> s {instanceTypes = a} :: CreateNodegroup) Core.. Lens.mapping Lens._Coerce

-- | The name of the cluster to create the node group in.
createNodegroup_clusterName :: Lens.Lens' CreateNodegroup Core.Text
createNodegroup_clusterName = Lens.lens (\CreateNodegroup' {clusterName} -> clusterName) (\s@CreateNodegroup' {} a -> s {clusterName = a} :: CreateNodegroup)

-- | The unique name to give your node group.
createNodegroup_nodegroupName :: Lens.Lens' CreateNodegroup Core.Text
createNodegroup_nodegroupName = Lens.lens (\CreateNodegroup' {nodegroupName} -> nodegroupName) (\s@CreateNodegroup' {} a -> s {nodegroupName = a} :: CreateNodegroup)

-- | The subnets to use for the Auto Scaling group that is created for your
-- node group. These subnets must have the tag key
-- @kubernetes.io\/cluster\/CLUSTER_NAME@ with a value of @shared@, where
-- @CLUSTER_NAME@ is replaced with the name of your cluster. If you specify
-- @launchTemplate@, then don\'t specify
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateNetworkInterface.html SubnetId>
-- in your launch template, or the node group deployment will fail. For
-- more information about using launch templates with Amazon EKS, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the Amazon EKS User Guide.
createNodegroup_subnets :: Lens.Lens' CreateNodegroup [Core.Text]
createNodegroup_subnets = Lens.lens (\CreateNodegroup' {subnets} -> subnets) (\s@CreateNodegroup' {} a -> s {subnets = a} :: CreateNodegroup) Core.. Lens._Coerce

-- | The Amazon Resource Name (ARN) of the IAM role to associate with your
-- node group. The Amazon EKS worker node @kubelet@ daemon makes calls to
-- AWS APIs on your behalf. Nodes receive permissions for these API calls
-- through an IAM instance profile and associated policies. Before you can
-- launch nodes and register them into a cluster, you must create an IAM
-- role for those nodes to use when they are launched. For more
-- information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/worker_node_IAM_role.html Amazon EKS node IAM role>
-- in the //Amazon EKS User Guide// . If you specify @launchTemplate@, then
-- don\'t specify
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_IamInstanceProfile.html IamInstanceProfile>
-- in your launch template, or the node group deployment will fail. For
-- more information about using launch templates with Amazon EKS, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the Amazon EKS User Guide.
createNodegroup_nodeRole :: Lens.Lens' CreateNodegroup Core.Text
createNodegroup_nodeRole = Lens.lens (\CreateNodegroup' {nodeRole} -> nodeRole) (\s@CreateNodegroup' {} a -> s {nodeRole = a} :: CreateNodegroup)

instance Core.AWSRequest CreateNodegroup where
  type
    AWSResponse CreateNodegroup =
      CreateNodegroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateNodegroupResponse'
            Core.<$> (x Core..?> "nodegroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateNodegroup

instance Core.NFData CreateNodegroup

instance Core.ToHeaders CreateNodegroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateNodegroup where
  toJSON CreateNodegroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("scalingConfig" Core..=) Core.<$> scalingConfig,
            ("capacityType" Core..=) Core.<$> capacityType,
            ("releaseVersion" Core..=) Core.<$> releaseVersion,
            ("diskSize" Core..=) Core.<$> diskSize,
            ("remoteAccess" Core..=) Core.<$> remoteAccess,
            ("launchTemplate" Core..=) Core.<$> launchTemplate,
            ("labels" Core..=) Core.<$> labels,
            ("version" Core..=) Core.<$> version,
            ("tags" Core..=) Core.<$> tags,
            ("clientRequestToken" Core..=)
              Core.<$> clientRequestToken,
            ("amiType" Core..=) Core.<$> amiType,
            ("instanceTypes" Core..=) Core.<$> instanceTypes,
            Core.Just ("nodegroupName" Core..= nodegroupName),
            Core.Just ("subnets" Core..= subnets),
            Core.Just ("nodeRole" Core..= nodeRole)
          ]
      )

instance Core.ToPath CreateNodegroup where
  toPath CreateNodegroup' {..} =
    Core.mconcat
      ["/clusters/", Core.toBS clusterName, "/node-groups"]

instance Core.ToQuery CreateNodegroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateNodegroupResponse' smart constructor.
data CreateNodegroupResponse = CreateNodegroupResponse'
  { -- | The full description of your new node group.
    nodegroup :: Core.Maybe Nodegroup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateNodegroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nodegroup', 'createNodegroupResponse_nodegroup' - The full description of your new node group.
--
-- 'httpStatus', 'createNodegroupResponse_httpStatus' - The response's http status code.
newCreateNodegroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateNodegroupResponse
newCreateNodegroupResponse pHttpStatus_ =
  CreateNodegroupResponse'
    { nodegroup = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The full description of your new node group.
createNodegroupResponse_nodegroup :: Lens.Lens' CreateNodegroupResponse (Core.Maybe Nodegroup)
createNodegroupResponse_nodegroup = Lens.lens (\CreateNodegroupResponse' {nodegroup} -> nodegroup) (\s@CreateNodegroupResponse' {} a -> s {nodegroup = a} :: CreateNodegroupResponse)

-- | The response's http status code.
createNodegroupResponse_httpStatus :: Lens.Lens' CreateNodegroupResponse Core.Int
createNodegroupResponse_httpStatus = Lens.lens (\CreateNodegroupResponse' {httpStatus} -> httpStatus) (\s@CreateNodegroupResponse' {} a -> s {httpStatus = a} :: CreateNodegroupResponse)

instance Core.NFData CreateNodegroupResponse
