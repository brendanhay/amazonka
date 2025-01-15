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
-- Module      : Amazonka.EKS.CreateNodegroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
-- associated Amazon EC2 instances that are managed by Amazon Web Services
-- for an Amazon EKS cluster. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/managed-node-groups.html Managed node groups>
-- in the /Amazon EKS User Guide/.
--
-- Windows AMI types are only supported for commercial Regions that support
-- Windows Amazon EKS.
module Amazonka.EKS.CreateNodegroup
  ( -- * Creating a Request
    CreateNodegroup (..),
    newCreateNodegroup,

    -- * Request Lenses
    createNodegroup_amiType,
    createNodegroup_capacityType,
    createNodegroup_clientRequestToken,
    createNodegroup_diskSize,
    createNodegroup_instanceTypes,
    createNodegroup_labels,
    createNodegroup_launchTemplate,
    createNodegroup_releaseVersion,
    createNodegroup_remoteAccess,
    createNodegroup_scalingConfig,
    createNodegroup_tags,
    createNodegroup_taints,
    createNodegroup_updateConfig,
    createNodegroup_version,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateNodegroup' smart constructor.
data CreateNodegroup = CreateNodegroup'
  { -- | The AMI type for your node group. If you specify @launchTemplate@, and
    -- your launch template uses a custom AMI, then don\'t specify @amiType@,
    -- or the node group deployment will fail. If your launch template uses a
    -- Windows custom AMI, then add @eks:kube-proxy-windows@ to your Windows
    -- nodes @rolearn@ in the @aws-auth@ @ConfigMap@. For more information
    -- about using launch templates with Amazon EKS, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
    -- in the /Amazon EKS User Guide/.
    amiType :: Prelude.Maybe AMITypes,
    -- | The capacity type for your node group.
    capacityType :: Prelude.Maybe CapacityTypes,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The root device disk size (in GiB) for your node group instances. The
    -- default disk size is 20 GiB for Linux and Bottlerocket. The default disk
    -- size is 50 GiB for Windows. If you specify @launchTemplate@, then don\'t
    -- specify @diskSize@, or the node group deployment will fail. For more
    -- information about using launch templates with Amazon EKS, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
    -- in the /Amazon EKS User Guide/.
    diskSize :: Prelude.Maybe Prelude.Int,
    -- | Specify the instance types for a node group. If you specify a GPU
    -- instance type, make sure to also specify an applicable GPU AMI type with
    -- the @amiType@ parameter. If you specify @launchTemplate@, then you can
    -- specify zero or one instance type in your launch template /or/ you can
    -- specify 0-20 instance types for @instanceTypes@. If however, you specify
    -- an instance type in your launch template /and/ specify any
    -- @instanceTypes@, the node group deployment will fail. If you don\'t
    -- specify an instance type in a launch template or for @instanceTypes@,
    -- then @t3.medium@ is used, by default. If you specify @Spot@ for
    -- @capacityType@, then we recommend specifying multiple values for
    -- @instanceTypes@. For more information, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/managed-node-groups.html#managed-node-group-capacity-types Managed node group capacity types>
    -- and
    -- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
    -- in the /Amazon EKS User Guide/.
    instanceTypes :: Prelude.Maybe [Prelude.Text],
    -- | The Kubernetes labels to be applied to the nodes in the node group when
    -- they are created.
    labels :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | An object representing a node group\'s launch template specification. If
    -- specified, then do not specify @instanceTypes@, @diskSize@, or
    -- @remoteAccess@ and make sure that the launch template meets the
    -- requirements in @launchTemplateSpecification@.
    launchTemplate :: Prelude.Maybe LaunchTemplateSpecification,
    -- | The AMI version of the Amazon EKS optimized AMI to use with your node
    -- group. By default, the latest available AMI version for the node
    -- group\'s current Kubernetes version is used. For information about Linux
    -- versions, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/eks-linux-ami-versions.html Amazon EKS optimized Amazon Linux AMI versions>
    -- in the /Amazon EKS User Guide/. Amazon EKS managed node groups support
    -- the November 2022 and later releases of the Windows AMIs. For
    -- information about Windows versions, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/eks-ami-versions-windows.html Amazon EKS optimized Windows AMI versions>
    -- in the /Amazon EKS User Guide/.
    --
    -- If you specify @launchTemplate@, and your launch template uses a custom
    -- AMI, then don\'t specify @releaseVersion@, or the node group deployment
    -- will fail. For more information about using launch templates with Amazon
    -- EKS, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
    -- in the /Amazon EKS User Guide/.
    releaseVersion :: Prelude.Maybe Prelude.Text,
    -- | The remote access configuration to use with your node group. For Linux,
    -- the protocol is SSH. For Windows, the protocol is RDP. If you specify
    -- @launchTemplate@, then don\'t specify @remoteAccess@, or the node group
    -- deployment will fail. For more information about using launch templates
    -- with Amazon EKS, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
    -- in the /Amazon EKS User Guide/.
    remoteAccess :: Prelude.Maybe RemoteAccessConfig,
    -- | The scaling configuration details for the Auto Scaling group that is
    -- created for your node group.
    scalingConfig :: Prelude.Maybe NodegroupScalingConfig,
    -- | The metadata to apply to the node group to assist with categorization
    -- and organization. Each tag consists of a key and an optional value. You
    -- define both. Node group tags do not propagate to any other resources
    -- associated with the node group, such as the Amazon EC2 instances or
    -- subnets.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Kubernetes taints to be applied to the nodes in the node group. For
    -- more information, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/node-taints-managed-node-groups.html Node taints on managed node groups>.
    taints :: Prelude.Maybe [Taint],
    -- | The node group update configuration.
    updateConfig :: Prelude.Maybe NodegroupUpdateConfig,
    -- | The Kubernetes version to use for your managed nodes. By default, the
    -- Kubernetes version of the cluster is used, and this is the only accepted
    -- specified value. If you specify @launchTemplate@, and your launch
    -- template uses a custom AMI, then don\'t specify @version@, or the node
    -- group deployment will fail. For more information about using launch
    -- templates with Amazon EKS, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
    -- in the /Amazon EKS User Guide/.
    version :: Prelude.Maybe Prelude.Text,
    -- | The name of the cluster to create the node group in.
    clusterName :: Prelude.Text,
    -- | The unique name to give your node group.
    nodegroupName :: Prelude.Text,
    -- | The subnets to use for the Auto Scaling group that is created for your
    -- node group. If you specify @launchTemplate@, then don\'t specify
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateNetworkInterface.html SubnetId>
    -- in your launch template, or the node group deployment will fail. For
    -- more information about using launch templates with Amazon EKS, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
    -- in the /Amazon EKS User Guide/.
    subnets :: [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the IAM role to associate with your
    -- node group. The Amazon EKS worker node @kubelet@ daemon makes calls to
    -- Amazon Web Services APIs on your behalf. Nodes receive permissions for
    -- these API calls through an IAM instance profile and associated policies.
    -- Before you can launch nodes and register them into a cluster, you must
    -- create an IAM role for those nodes to use when they are launched. For
    -- more information, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/create-node-role.html Amazon EKS node IAM role>
    -- in the //Amazon EKS User Guide// . If you specify @launchTemplate@, then
    -- don\'t specify
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_IamInstanceProfile.html IamInstanceProfile>
    -- in your launch template, or the node group deployment will fail. For
    -- more information about using launch templates with Amazon EKS, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
    -- in the /Amazon EKS User Guide/.
    nodeRole :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNodegroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amiType', 'createNodegroup_amiType' - The AMI type for your node group. If you specify @launchTemplate@, and
-- your launch template uses a custom AMI, then don\'t specify @amiType@,
-- or the node group deployment will fail. If your launch template uses a
-- Windows custom AMI, then add @eks:kube-proxy-windows@ to your Windows
-- nodes @rolearn@ in the @aws-auth@ @ConfigMap@. For more information
-- about using launch templates with Amazon EKS, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the /Amazon EKS User Guide/.
--
-- 'capacityType', 'createNodegroup_capacityType' - The capacity type for your node group.
--
-- 'clientRequestToken', 'createNodegroup_clientRequestToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'diskSize', 'createNodegroup_diskSize' - The root device disk size (in GiB) for your node group instances. The
-- default disk size is 20 GiB for Linux and Bottlerocket. The default disk
-- size is 50 GiB for Windows. If you specify @launchTemplate@, then don\'t
-- specify @diskSize@, or the node group deployment will fail. For more
-- information about using launch templates with Amazon EKS, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the /Amazon EKS User Guide/.
--
-- 'instanceTypes', 'createNodegroup_instanceTypes' - Specify the instance types for a node group. If you specify a GPU
-- instance type, make sure to also specify an applicable GPU AMI type with
-- the @amiType@ parameter. If you specify @launchTemplate@, then you can
-- specify zero or one instance type in your launch template /or/ you can
-- specify 0-20 instance types for @instanceTypes@. If however, you specify
-- an instance type in your launch template /and/ specify any
-- @instanceTypes@, the node group deployment will fail. If you don\'t
-- specify an instance type in a launch template or for @instanceTypes@,
-- then @t3.medium@ is used, by default. If you specify @Spot@ for
-- @capacityType@, then we recommend specifying multiple values for
-- @instanceTypes@. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/managed-node-groups.html#managed-node-group-capacity-types Managed node group capacity types>
-- and
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the /Amazon EKS User Guide/.
--
-- 'labels', 'createNodegroup_labels' - The Kubernetes labels to be applied to the nodes in the node group when
-- they are created.
--
-- 'launchTemplate', 'createNodegroup_launchTemplate' - An object representing a node group\'s launch template specification. If
-- specified, then do not specify @instanceTypes@, @diskSize@, or
-- @remoteAccess@ and make sure that the launch template meets the
-- requirements in @launchTemplateSpecification@.
--
-- 'releaseVersion', 'createNodegroup_releaseVersion' - The AMI version of the Amazon EKS optimized AMI to use with your node
-- group. By default, the latest available AMI version for the node
-- group\'s current Kubernetes version is used. For information about Linux
-- versions, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/eks-linux-ami-versions.html Amazon EKS optimized Amazon Linux AMI versions>
-- in the /Amazon EKS User Guide/. Amazon EKS managed node groups support
-- the November 2022 and later releases of the Windows AMIs. For
-- information about Windows versions, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/eks-ami-versions-windows.html Amazon EKS optimized Windows AMI versions>
-- in the /Amazon EKS User Guide/.
--
-- If you specify @launchTemplate@, and your launch template uses a custom
-- AMI, then don\'t specify @releaseVersion@, or the node group deployment
-- will fail. For more information about using launch templates with Amazon
-- EKS, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the /Amazon EKS User Guide/.
--
-- 'remoteAccess', 'createNodegroup_remoteAccess' - The remote access configuration to use with your node group. For Linux,
-- the protocol is SSH. For Windows, the protocol is RDP. If you specify
-- @launchTemplate@, then don\'t specify @remoteAccess@, or the node group
-- deployment will fail. For more information about using launch templates
-- with Amazon EKS, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the /Amazon EKS User Guide/.
--
-- 'scalingConfig', 'createNodegroup_scalingConfig' - The scaling configuration details for the Auto Scaling group that is
-- created for your node group.
--
-- 'tags', 'createNodegroup_tags' - The metadata to apply to the node group to assist with categorization
-- and organization. Each tag consists of a key and an optional value. You
-- define both. Node group tags do not propagate to any other resources
-- associated with the node group, such as the Amazon EC2 instances or
-- subnets.
--
-- 'taints', 'createNodegroup_taints' - The Kubernetes taints to be applied to the nodes in the node group. For
-- more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/node-taints-managed-node-groups.html Node taints on managed node groups>.
--
-- 'updateConfig', 'createNodegroup_updateConfig' - The node group update configuration.
--
-- 'version', 'createNodegroup_version' - The Kubernetes version to use for your managed nodes. By default, the
-- Kubernetes version of the cluster is used, and this is the only accepted
-- specified value. If you specify @launchTemplate@, and your launch
-- template uses a custom AMI, then don\'t specify @version@, or the node
-- group deployment will fail. For more information about using launch
-- templates with Amazon EKS, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the /Amazon EKS User Guide/.
--
-- 'clusterName', 'createNodegroup_clusterName' - The name of the cluster to create the node group in.
--
-- 'nodegroupName', 'createNodegroup_nodegroupName' - The unique name to give your node group.
--
-- 'subnets', 'createNodegroup_subnets' - The subnets to use for the Auto Scaling group that is created for your
-- node group. If you specify @launchTemplate@, then don\'t specify
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateNetworkInterface.html SubnetId>
-- in your launch template, or the node group deployment will fail. For
-- more information about using launch templates with Amazon EKS, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the /Amazon EKS User Guide/.
--
-- 'nodeRole', 'createNodegroup_nodeRole' - The Amazon Resource Name (ARN) of the IAM role to associate with your
-- node group. The Amazon EKS worker node @kubelet@ daemon makes calls to
-- Amazon Web Services APIs on your behalf. Nodes receive permissions for
-- these API calls through an IAM instance profile and associated policies.
-- Before you can launch nodes and register them into a cluster, you must
-- create an IAM role for those nodes to use when they are launched. For
-- more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/create-node-role.html Amazon EKS node IAM role>
-- in the //Amazon EKS User Guide// . If you specify @launchTemplate@, then
-- don\'t specify
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_IamInstanceProfile.html IamInstanceProfile>
-- in your launch template, or the node group deployment will fail. For
-- more information about using launch templates with Amazon EKS, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the /Amazon EKS User Guide/.
newCreateNodegroup ::
  -- | 'clusterName'
  Prelude.Text ->
  -- | 'nodegroupName'
  Prelude.Text ->
  -- | 'nodeRole'
  Prelude.Text ->
  CreateNodegroup
newCreateNodegroup
  pClusterName_
  pNodegroupName_
  pNodeRole_ =
    CreateNodegroup'
      { amiType = Prelude.Nothing,
        capacityType = Prelude.Nothing,
        clientRequestToken = Prelude.Nothing,
        diskSize = Prelude.Nothing,
        instanceTypes = Prelude.Nothing,
        labels = Prelude.Nothing,
        launchTemplate = Prelude.Nothing,
        releaseVersion = Prelude.Nothing,
        remoteAccess = Prelude.Nothing,
        scalingConfig = Prelude.Nothing,
        tags = Prelude.Nothing,
        taints = Prelude.Nothing,
        updateConfig = Prelude.Nothing,
        version = Prelude.Nothing,
        clusterName = pClusterName_,
        nodegroupName = pNodegroupName_,
        subnets = Prelude.mempty,
        nodeRole = pNodeRole_
      }

-- | The AMI type for your node group. If you specify @launchTemplate@, and
-- your launch template uses a custom AMI, then don\'t specify @amiType@,
-- or the node group deployment will fail. If your launch template uses a
-- Windows custom AMI, then add @eks:kube-proxy-windows@ to your Windows
-- nodes @rolearn@ in the @aws-auth@ @ConfigMap@. For more information
-- about using launch templates with Amazon EKS, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the /Amazon EKS User Guide/.
createNodegroup_amiType :: Lens.Lens' CreateNodegroup (Prelude.Maybe AMITypes)
createNodegroup_amiType = Lens.lens (\CreateNodegroup' {amiType} -> amiType) (\s@CreateNodegroup' {} a -> s {amiType = a} :: CreateNodegroup)

-- | The capacity type for your node group.
createNodegroup_capacityType :: Lens.Lens' CreateNodegroup (Prelude.Maybe CapacityTypes)
createNodegroup_capacityType = Lens.lens (\CreateNodegroup' {capacityType} -> capacityType) (\s@CreateNodegroup' {} a -> s {capacityType = a} :: CreateNodegroup)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createNodegroup_clientRequestToken :: Lens.Lens' CreateNodegroup (Prelude.Maybe Prelude.Text)
createNodegroup_clientRequestToken = Lens.lens (\CreateNodegroup' {clientRequestToken} -> clientRequestToken) (\s@CreateNodegroup' {} a -> s {clientRequestToken = a} :: CreateNodegroup)

-- | The root device disk size (in GiB) for your node group instances. The
-- default disk size is 20 GiB for Linux and Bottlerocket. The default disk
-- size is 50 GiB for Windows. If you specify @launchTemplate@, then don\'t
-- specify @diskSize@, or the node group deployment will fail. For more
-- information about using launch templates with Amazon EKS, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the /Amazon EKS User Guide/.
createNodegroup_diskSize :: Lens.Lens' CreateNodegroup (Prelude.Maybe Prelude.Int)
createNodegroup_diskSize = Lens.lens (\CreateNodegroup' {diskSize} -> diskSize) (\s@CreateNodegroup' {} a -> s {diskSize = a} :: CreateNodegroup)

-- | Specify the instance types for a node group. If you specify a GPU
-- instance type, make sure to also specify an applicable GPU AMI type with
-- the @amiType@ parameter. If you specify @launchTemplate@, then you can
-- specify zero or one instance type in your launch template /or/ you can
-- specify 0-20 instance types for @instanceTypes@. If however, you specify
-- an instance type in your launch template /and/ specify any
-- @instanceTypes@, the node group deployment will fail. If you don\'t
-- specify an instance type in a launch template or for @instanceTypes@,
-- then @t3.medium@ is used, by default. If you specify @Spot@ for
-- @capacityType@, then we recommend specifying multiple values for
-- @instanceTypes@. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/managed-node-groups.html#managed-node-group-capacity-types Managed node group capacity types>
-- and
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the /Amazon EKS User Guide/.
createNodegroup_instanceTypes :: Lens.Lens' CreateNodegroup (Prelude.Maybe [Prelude.Text])
createNodegroup_instanceTypes = Lens.lens (\CreateNodegroup' {instanceTypes} -> instanceTypes) (\s@CreateNodegroup' {} a -> s {instanceTypes = a} :: CreateNodegroup) Prelude.. Lens.mapping Lens.coerced

-- | The Kubernetes labels to be applied to the nodes in the node group when
-- they are created.
createNodegroup_labels :: Lens.Lens' CreateNodegroup (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createNodegroup_labels = Lens.lens (\CreateNodegroup' {labels} -> labels) (\s@CreateNodegroup' {} a -> s {labels = a} :: CreateNodegroup) Prelude.. Lens.mapping Lens.coerced

-- | An object representing a node group\'s launch template specification. If
-- specified, then do not specify @instanceTypes@, @diskSize@, or
-- @remoteAccess@ and make sure that the launch template meets the
-- requirements in @launchTemplateSpecification@.
createNodegroup_launchTemplate :: Lens.Lens' CreateNodegroup (Prelude.Maybe LaunchTemplateSpecification)
createNodegroup_launchTemplate = Lens.lens (\CreateNodegroup' {launchTemplate} -> launchTemplate) (\s@CreateNodegroup' {} a -> s {launchTemplate = a} :: CreateNodegroup)

-- | The AMI version of the Amazon EKS optimized AMI to use with your node
-- group. By default, the latest available AMI version for the node
-- group\'s current Kubernetes version is used. For information about Linux
-- versions, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/eks-linux-ami-versions.html Amazon EKS optimized Amazon Linux AMI versions>
-- in the /Amazon EKS User Guide/. Amazon EKS managed node groups support
-- the November 2022 and later releases of the Windows AMIs. For
-- information about Windows versions, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/eks-ami-versions-windows.html Amazon EKS optimized Windows AMI versions>
-- in the /Amazon EKS User Guide/.
--
-- If you specify @launchTemplate@, and your launch template uses a custom
-- AMI, then don\'t specify @releaseVersion@, or the node group deployment
-- will fail. For more information about using launch templates with Amazon
-- EKS, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the /Amazon EKS User Guide/.
createNodegroup_releaseVersion :: Lens.Lens' CreateNodegroup (Prelude.Maybe Prelude.Text)
createNodegroup_releaseVersion = Lens.lens (\CreateNodegroup' {releaseVersion} -> releaseVersion) (\s@CreateNodegroup' {} a -> s {releaseVersion = a} :: CreateNodegroup)

-- | The remote access configuration to use with your node group. For Linux,
-- the protocol is SSH. For Windows, the protocol is RDP. If you specify
-- @launchTemplate@, then don\'t specify @remoteAccess@, or the node group
-- deployment will fail. For more information about using launch templates
-- with Amazon EKS, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the /Amazon EKS User Guide/.
createNodegroup_remoteAccess :: Lens.Lens' CreateNodegroup (Prelude.Maybe RemoteAccessConfig)
createNodegroup_remoteAccess = Lens.lens (\CreateNodegroup' {remoteAccess} -> remoteAccess) (\s@CreateNodegroup' {} a -> s {remoteAccess = a} :: CreateNodegroup)

-- | The scaling configuration details for the Auto Scaling group that is
-- created for your node group.
createNodegroup_scalingConfig :: Lens.Lens' CreateNodegroup (Prelude.Maybe NodegroupScalingConfig)
createNodegroup_scalingConfig = Lens.lens (\CreateNodegroup' {scalingConfig} -> scalingConfig) (\s@CreateNodegroup' {} a -> s {scalingConfig = a} :: CreateNodegroup)

-- | The metadata to apply to the node group to assist with categorization
-- and organization. Each tag consists of a key and an optional value. You
-- define both. Node group tags do not propagate to any other resources
-- associated with the node group, such as the Amazon EC2 instances or
-- subnets.
createNodegroup_tags :: Lens.Lens' CreateNodegroup (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createNodegroup_tags = Lens.lens (\CreateNodegroup' {tags} -> tags) (\s@CreateNodegroup' {} a -> s {tags = a} :: CreateNodegroup) Prelude.. Lens.mapping Lens.coerced

-- | The Kubernetes taints to be applied to the nodes in the node group. For
-- more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/node-taints-managed-node-groups.html Node taints on managed node groups>.
createNodegroup_taints :: Lens.Lens' CreateNodegroup (Prelude.Maybe [Taint])
createNodegroup_taints = Lens.lens (\CreateNodegroup' {taints} -> taints) (\s@CreateNodegroup' {} a -> s {taints = a} :: CreateNodegroup) Prelude.. Lens.mapping Lens.coerced

-- | The node group update configuration.
createNodegroup_updateConfig :: Lens.Lens' CreateNodegroup (Prelude.Maybe NodegroupUpdateConfig)
createNodegroup_updateConfig = Lens.lens (\CreateNodegroup' {updateConfig} -> updateConfig) (\s@CreateNodegroup' {} a -> s {updateConfig = a} :: CreateNodegroup)

-- | The Kubernetes version to use for your managed nodes. By default, the
-- Kubernetes version of the cluster is used, and this is the only accepted
-- specified value. If you specify @launchTemplate@, and your launch
-- template uses a custom AMI, then don\'t specify @version@, or the node
-- group deployment will fail. For more information about using launch
-- templates with Amazon EKS, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the /Amazon EKS User Guide/.
createNodegroup_version :: Lens.Lens' CreateNodegroup (Prelude.Maybe Prelude.Text)
createNodegroup_version = Lens.lens (\CreateNodegroup' {version} -> version) (\s@CreateNodegroup' {} a -> s {version = a} :: CreateNodegroup)

-- | The name of the cluster to create the node group in.
createNodegroup_clusterName :: Lens.Lens' CreateNodegroup Prelude.Text
createNodegroup_clusterName = Lens.lens (\CreateNodegroup' {clusterName} -> clusterName) (\s@CreateNodegroup' {} a -> s {clusterName = a} :: CreateNodegroup)

-- | The unique name to give your node group.
createNodegroup_nodegroupName :: Lens.Lens' CreateNodegroup Prelude.Text
createNodegroup_nodegroupName = Lens.lens (\CreateNodegroup' {nodegroupName} -> nodegroupName) (\s@CreateNodegroup' {} a -> s {nodegroupName = a} :: CreateNodegroup)

-- | The subnets to use for the Auto Scaling group that is created for your
-- node group. If you specify @launchTemplate@, then don\'t specify
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateNetworkInterface.html SubnetId>
-- in your launch template, or the node group deployment will fail. For
-- more information about using launch templates with Amazon EKS, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the /Amazon EKS User Guide/.
createNodegroup_subnets :: Lens.Lens' CreateNodegroup [Prelude.Text]
createNodegroup_subnets = Lens.lens (\CreateNodegroup' {subnets} -> subnets) (\s@CreateNodegroup' {} a -> s {subnets = a} :: CreateNodegroup) Prelude.. Lens.coerced

-- | The Amazon Resource Name (ARN) of the IAM role to associate with your
-- node group. The Amazon EKS worker node @kubelet@ daemon makes calls to
-- Amazon Web Services APIs on your behalf. Nodes receive permissions for
-- these API calls through an IAM instance profile and associated policies.
-- Before you can launch nodes and register them into a cluster, you must
-- create an IAM role for those nodes to use when they are launched. For
-- more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/create-node-role.html Amazon EKS node IAM role>
-- in the //Amazon EKS User Guide// . If you specify @launchTemplate@, then
-- don\'t specify
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_IamInstanceProfile.html IamInstanceProfile>
-- in your launch template, or the node group deployment will fail. For
-- more information about using launch templates with Amazon EKS, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the /Amazon EKS User Guide/.
createNodegroup_nodeRole :: Lens.Lens' CreateNodegroup Prelude.Text
createNodegroup_nodeRole = Lens.lens (\CreateNodegroup' {nodeRole} -> nodeRole) (\s@CreateNodegroup' {} a -> s {nodeRole = a} :: CreateNodegroup)

instance Core.AWSRequest CreateNodegroup where
  type
    AWSResponse CreateNodegroup =
      CreateNodegroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateNodegroupResponse'
            Prelude.<$> (x Data..?> "nodegroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateNodegroup where
  hashWithSalt _salt CreateNodegroup' {..} =
    _salt
      `Prelude.hashWithSalt` amiType
      `Prelude.hashWithSalt` capacityType
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` diskSize
      `Prelude.hashWithSalt` instanceTypes
      `Prelude.hashWithSalt` labels
      `Prelude.hashWithSalt` launchTemplate
      `Prelude.hashWithSalt` releaseVersion
      `Prelude.hashWithSalt` remoteAccess
      `Prelude.hashWithSalt` scalingConfig
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` taints
      `Prelude.hashWithSalt` updateConfig
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` nodegroupName
      `Prelude.hashWithSalt` subnets
      `Prelude.hashWithSalt` nodeRole

instance Prelude.NFData CreateNodegroup where
  rnf CreateNodegroup' {..} =
    Prelude.rnf amiType `Prelude.seq`
      Prelude.rnf capacityType `Prelude.seq`
        Prelude.rnf clientRequestToken `Prelude.seq`
          Prelude.rnf diskSize `Prelude.seq`
            Prelude.rnf instanceTypes `Prelude.seq`
              Prelude.rnf labels `Prelude.seq`
                Prelude.rnf launchTemplate `Prelude.seq`
                  Prelude.rnf releaseVersion `Prelude.seq`
                    Prelude.rnf remoteAccess `Prelude.seq`
                      Prelude.rnf scalingConfig `Prelude.seq`
                        Prelude.rnf tags `Prelude.seq`
                          Prelude.rnf taints `Prelude.seq`
                            Prelude.rnf updateConfig `Prelude.seq`
                              Prelude.rnf version `Prelude.seq`
                                Prelude.rnf clusterName `Prelude.seq`
                                  Prelude.rnf nodegroupName `Prelude.seq`
                                    Prelude.rnf subnets `Prelude.seq`
                                      Prelude.rnf nodeRole

instance Data.ToHeaders CreateNodegroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateNodegroup where
  toJSON CreateNodegroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("amiType" Data..=) Prelude.<$> amiType,
            ("capacityType" Data..=) Prelude.<$> capacityType,
            ("clientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("diskSize" Data..=) Prelude.<$> diskSize,
            ("instanceTypes" Data..=) Prelude.<$> instanceTypes,
            ("labels" Data..=) Prelude.<$> labels,
            ("launchTemplate" Data..=)
              Prelude.<$> launchTemplate,
            ("releaseVersion" Data..=)
              Prelude.<$> releaseVersion,
            ("remoteAccess" Data..=) Prelude.<$> remoteAccess,
            ("scalingConfig" Data..=) Prelude.<$> scalingConfig,
            ("tags" Data..=) Prelude.<$> tags,
            ("taints" Data..=) Prelude.<$> taints,
            ("updateConfig" Data..=) Prelude.<$> updateConfig,
            ("version" Data..=) Prelude.<$> version,
            Prelude.Just ("nodegroupName" Data..= nodegroupName),
            Prelude.Just ("subnets" Data..= subnets),
            Prelude.Just ("nodeRole" Data..= nodeRole)
          ]
      )

instance Data.ToPath CreateNodegroup where
  toPath CreateNodegroup' {..} =
    Prelude.mconcat
      ["/clusters/", Data.toBS clusterName, "/node-groups"]

instance Data.ToQuery CreateNodegroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateNodegroupResponse' smart constructor.
data CreateNodegroupResponse = CreateNodegroupResponse'
  { -- | The full description of your new node group.
    nodegroup :: Prelude.Maybe Nodegroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateNodegroupResponse
newCreateNodegroupResponse pHttpStatus_ =
  CreateNodegroupResponse'
    { nodegroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The full description of your new node group.
createNodegroupResponse_nodegroup :: Lens.Lens' CreateNodegroupResponse (Prelude.Maybe Nodegroup)
createNodegroupResponse_nodegroup = Lens.lens (\CreateNodegroupResponse' {nodegroup} -> nodegroup) (\s@CreateNodegroupResponse' {} a -> s {nodegroup = a} :: CreateNodegroupResponse)

-- | The response's http status code.
createNodegroupResponse_httpStatus :: Lens.Lens' CreateNodegroupResponse Prelude.Int
createNodegroupResponse_httpStatus = Lens.lens (\CreateNodegroupResponse' {httpStatus} -> httpStatus) (\s@CreateNodegroupResponse' {} a -> s {httpStatus = a} :: CreateNodegroupResponse)

instance Prelude.NFData CreateNodegroupResponse where
  rnf CreateNodegroupResponse' {..} =
    Prelude.rnf nodegroup `Prelude.seq`
      Prelude.rnf httpStatus
