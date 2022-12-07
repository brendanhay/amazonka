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
-- Module      : Amazonka.EKS.Types.Nodegroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.Nodegroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types.AMITypes
import Amazonka.EKS.Types.CapacityTypes
import Amazonka.EKS.Types.LaunchTemplateSpecification
import Amazonka.EKS.Types.NodegroupHealth
import Amazonka.EKS.Types.NodegroupResources
import Amazonka.EKS.Types.NodegroupScalingConfig
import Amazonka.EKS.Types.NodegroupStatus
import Amazonka.EKS.Types.NodegroupUpdateConfig
import Amazonka.EKS.Types.RemoteAccessConfig
import Amazonka.EKS.Types.Taint
import qualified Amazonka.Prelude as Prelude

-- | An object representing an Amazon EKS managed node group.
--
-- /See:/ 'newNodegroup' smart constructor.
data Nodegroup = Nodegroup'
  { -- | The metadata applied to the node group to assist with categorization and
    -- organization. Each tag consists of a key and an optional value. You
    -- define both. Node group tags do not propagate to any other resources
    -- associated with the node group, such as the Amazon EC2 instances or
    -- subnets.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | If the node group was deployed using a launch template with a custom
    -- AMI, then this is the AMI ID that was specified in the launch template.
    -- For node groups that weren\'t deployed using a launch template, this is
    -- the version of the Amazon EKS optimized AMI that the node group was
    -- deployed with.
    releaseVersion :: Prelude.Maybe Prelude.Text,
    -- | The Unix epoch timestamp in seconds for when the managed node group was
    -- last modified.
    modifiedAt :: Prelude.Maybe Data.POSIX,
    -- | If the node group wasn\'t deployed with a launch template, then this is
    -- the remote access configuration that is associated with the node group.
    -- If the node group was deployed with a launch template, then this is
    -- @null@.
    remoteAccess :: Prelude.Maybe RemoteAccessConfig,
    -- | If the node group wasn\'t deployed with a launch template, then this is
    -- the instance type that is associated with the node group. If the node
    -- group was deployed with a launch template, then this is @null@.
    instanceTypes :: Prelude.Maybe [Prelude.Text],
    -- | The node group update configuration.
    updateConfig :: Prelude.Maybe NodegroupUpdateConfig,
    -- | The capacity type of your managed node group.
    capacityType :: Prelude.Maybe CapacityTypes,
    -- | If the node group was deployed using a launch template with a custom
    -- AMI, then this is @CUSTOM@. For node groups that weren\'t deployed using
    -- a launch template, this is the AMI type that was specified in the node
    -- group configuration.
    amiType :: Prelude.Maybe AMITypes,
    -- | If the node group wasn\'t deployed with a launch template, then this is
    -- the disk size in the node group configuration. If the node group was
    -- deployed with a launch template, then this is @null@.
    diskSize :: Prelude.Maybe Prelude.Int,
    -- | The subnets that were specified for the Auto Scaling group that is
    -- associated with your node group.
    subnets :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) associated with the managed node group.
    nodegroupArn :: Prelude.Maybe Prelude.Text,
    -- | If a launch template was used to create the node group, then this is the
    -- launch template that was used.
    launchTemplate :: Prelude.Maybe LaunchTemplateSpecification,
    -- | The name associated with an Amazon EKS managed node group.
    nodegroupName :: Prelude.Maybe Prelude.Text,
    -- | The Kubernetes taints to be applied to the nodes in the node group when
    -- they are created. Effect is one of @No_Schedule@, @Prefer_No_Schedule@,
    -- or @No_Execute@. Kubernetes taints can be used together with tolerations
    -- to control how workloads are scheduled to your nodes. For more
    -- information, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/node-taints-managed-node-groups.html Node taints on managed node groups>.
    taints :: Prelude.Maybe [Taint],
    -- | The IAM role associated with your node group. The Amazon EKS node
    -- @kubelet@ daemon makes calls to Amazon Web Services APIs on your behalf.
    -- Nodes receive permissions for these API calls through an IAM instance
    -- profile and associated policies.
    nodeRole :: Prelude.Maybe Prelude.Text,
    -- | The current status of the managed node group.
    status :: Prelude.Maybe NodegroupStatus,
    -- | The Kubernetes labels applied to the nodes in the node group.
    --
    -- Only labels that are applied with the Amazon EKS API are shown here.
    -- There may be other Kubernetes labels applied to the nodes in this group.
    labels :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The health status of the node group. If there are issues with your node
    -- group\'s health, they are listed here.
    health :: Prelude.Maybe NodegroupHealth,
    -- | The resources associated with the node group, such as Auto Scaling
    -- groups and security groups for remote access.
    resources :: Prelude.Maybe NodegroupResources,
    -- | The scaling configuration details for the Auto Scaling group that is
    -- associated with your node group.
    scalingConfig :: Prelude.Maybe NodegroupScalingConfig,
    -- | The name of the cluster that the managed node group resides in.
    clusterName :: Prelude.Maybe Prelude.Text,
    -- | The Unix epoch timestamp in seconds for when the managed node group was
    -- created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The Kubernetes version of the managed node group.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Nodegroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'nodegroup_tags' - The metadata applied to the node group to assist with categorization and
-- organization. Each tag consists of a key and an optional value. You
-- define both. Node group tags do not propagate to any other resources
-- associated with the node group, such as the Amazon EC2 instances or
-- subnets.
--
-- 'releaseVersion', 'nodegroup_releaseVersion' - If the node group was deployed using a launch template with a custom
-- AMI, then this is the AMI ID that was specified in the launch template.
-- For node groups that weren\'t deployed using a launch template, this is
-- the version of the Amazon EKS optimized AMI that the node group was
-- deployed with.
--
-- 'modifiedAt', 'nodegroup_modifiedAt' - The Unix epoch timestamp in seconds for when the managed node group was
-- last modified.
--
-- 'remoteAccess', 'nodegroup_remoteAccess' - If the node group wasn\'t deployed with a launch template, then this is
-- the remote access configuration that is associated with the node group.
-- If the node group was deployed with a launch template, then this is
-- @null@.
--
-- 'instanceTypes', 'nodegroup_instanceTypes' - If the node group wasn\'t deployed with a launch template, then this is
-- the instance type that is associated with the node group. If the node
-- group was deployed with a launch template, then this is @null@.
--
-- 'updateConfig', 'nodegroup_updateConfig' - The node group update configuration.
--
-- 'capacityType', 'nodegroup_capacityType' - The capacity type of your managed node group.
--
-- 'amiType', 'nodegroup_amiType' - If the node group was deployed using a launch template with a custom
-- AMI, then this is @CUSTOM@. For node groups that weren\'t deployed using
-- a launch template, this is the AMI type that was specified in the node
-- group configuration.
--
-- 'diskSize', 'nodegroup_diskSize' - If the node group wasn\'t deployed with a launch template, then this is
-- the disk size in the node group configuration. If the node group was
-- deployed with a launch template, then this is @null@.
--
-- 'subnets', 'nodegroup_subnets' - The subnets that were specified for the Auto Scaling group that is
-- associated with your node group.
--
-- 'nodegroupArn', 'nodegroup_nodegroupArn' - The Amazon Resource Name (ARN) associated with the managed node group.
--
-- 'launchTemplate', 'nodegroup_launchTemplate' - If a launch template was used to create the node group, then this is the
-- launch template that was used.
--
-- 'nodegroupName', 'nodegroup_nodegroupName' - The name associated with an Amazon EKS managed node group.
--
-- 'taints', 'nodegroup_taints' - The Kubernetes taints to be applied to the nodes in the node group when
-- they are created. Effect is one of @No_Schedule@, @Prefer_No_Schedule@,
-- or @No_Execute@. Kubernetes taints can be used together with tolerations
-- to control how workloads are scheduled to your nodes. For more
-- information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/node-taints-managed-node-groups.html Node taints on managed node groups>.
--
-- 'nodeRole', 'nodegroup_nodeRole' - The IAM role associated with your node group. The Amazon EKS node
-- @kubelet@ daemon makes calls to Amazon Web Services APIs on your behalf.
-- Nodes receive permissions for these API calls through an IAM instance
-- profile and associated policies.
--
-- 'status', 'nodegroup_status' - The current status of the managed node group.
--
-- 'labels', 'nodegroup_labels' - The Kubernetes labels applied to the nodes in the node group.
--
-- Only labels that are applied with the Amazon EKS API are shown here.
-- There may be other Kubernetes labels applied to the nodes in this group.
--
-- 'health', 'nodegroup_health' - The health status of the node group. If there are issues with your node
-- group\'s health, they are listed here.
--
-- 'resources', 'nodegroup_resources' - The resources associated with the node group, such as Auto Scaling
-- groups and security groups for remote access.
--
-- 'scalingConfig', 'nodegroup_scalingConfig' - The scaling configuration details for the Auto Scaling group that is
-- associated with your node group.
--
-- 'clusterName', 'nodegroup_clusterName' - The name of the cluster that the managed node group resides in.
--
-- 'createdAt', 'nodegroup_createdAt' - The Unix epoch timestamp in seconds for when the managed node group was
-- created.
--
-- 'version', 'nodegroup_version' - The Kubernetes version of the managed node group.
newNodegroup ::
  Nodegroup
newNodegroup =
  Nodegroup'
    { tags = Prelude.Nothing,
      releaseVersion = Prelude.Nothing,
      modifiedAt = Prelude.Nothing,
      remoteAccess = Prelude.Nothing,
      instanceTypes = Prelude.Nothing,
      updateConfig = Prelude.Nothing,
      capacityType = Prelude.Nothing,
      amiType = Prelude.Nothing,
      diskSize = Prelude.Nothing,
      subnets = Prelude.Nothing,
      nodegroupArn = Prelude.Nothing,
      launchTemplate = Prelude.Nothing,
      nodegroupName = Prelude.Nothing,
      taints = Prelude.Nothing,
      nodeRole = Prelude.Nothing,
      status = Prelude.Nothing,
      labels = Prelude.Nothing,
      health = Prelude.Nothing,
      resources = Prelude.Nothing,
      scalingConfig = Prelude.Nothing,
      clusterName = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The metadata applied to the node group to assist with categorization and
-- organization. Each tag consists of a key and an optional value. You
-- define both. Node group tags do not propagate to any other resources
-- associated with the node group, such as the Amazon EC2 instances or
-- subnets.
nodegroup_tags :: Lens.Lens' Nodegroup (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
nodegroup_tags = Lens.lens (\Nodegroup' {tags} -> tags) (\s@Nodegroup' {} a -> s {tags = a} :: Nodegroup) Prelude.. Lens.mapping Lens.coerced

-- | If the node group was deployed using a launch template with a custom
-- AMI, then this is the AMI ID that was specified in the launch template.
-- For node groups that weren\'t deployed using a launch template, this is
-- the version of the Amazon EKS optimized AMI that the node group was
-- deployed with.
nodegroup_releaseVersion :: Lens.Lens' Nodegroup (Prelude.Maybe Prelude.Text)
nodegroup_releaseVersion = Lens.lens (\Nodegroup' {releaseVersion} -> releaseVersion) (\s@Nodegroup' {} a -> s {releaseVersion = a} :: Nodegroup)

-- | The Unix epoch timestamp in seconds for when the managed node group was
-- last modified.
nodegroup_modifiedAt :: Lens.Lens' Nodegroup (Prelude.Maybe Prelude.UTCTime)
nodegroup_modifiedAt = Lens.lens (\Nodegroup' {modifiedAt} -> modifiedAt) (\s@Nodegroup' {} a -> s {modifiedAt = a} :: Nodegroup) Prelude.. Lens.mapping Data._Time

-- | If the node group wasn\'t deployed with a launch template, then this is
-- the remote access configuration that is associated with the node group.
-- If the node group was deployed with a launch template, then this is
-- @null@.
nodegroup_remoteAccess :: Lens.Lens' Nodegroup (Prelude.Maybe RemoteAccessConfig)
nodegroup_remoteAccess = Lens.lens (\Nodegroup' {remoteAccess} -> remoteAccess) (\s@Nodegroup' {} a -> s {remoteAccess = a} :: Nodegroup)

-- | If the node group wasn\'t deployed with a launch template, then this is
-- the instance type that is associated with the node group. If the node
-- group was deployed with a launch template, then this is @null@.
nodegroup_instanceTypes :: Lens.Lens' Nodegroup (Prelude.Maybe [Prelude.Text])
nodegroup_instanceTypes = Lens.lens (\Nodegroup' {instanceTypes} -> instanceTypes) (\s@Nodegroup' {} a -> s {instanceTypes = a} :: Nodegroup) Prelude.. Lens.mapping Lens.coerced

-- | The node group update configuration.
nodegroup_updateConfig :: Lens.Lens' Nodegroup (Prelude.Maybe NodegroupUpdateConfig)
nodegroup_updateConfig = Lens.lens (\Nodegroup' {updateConfig} -> updateConfig) (\s@Nodegroup' {} a -> s {updateConfig = a} :: Nodegroup)

-- | The capacity type of your managed node group.
nodegroup_capacityType :: Lens.Lens' Nodegroup (Prelude.Maybe CapacityTypes)
nodegroup_capacityType = Lens.lens (\Nodegroup' {capacityType} -> capacityType) (\s@Nodegroup' {} a -> s {capacityType = a} :: Nodegroup)

-- | If the node group was deployed using a launch template with a custom
-- AMI, then this is @CUSTOM@. For node groups that weren\'t deployed using
-- a launch template, this is the AMI type that was specified in the node
-- group configuration.
nodegroup_amiType :: Lens.Lens' Nodegroup (Prelude.Maybe AMITypes)
nodegroup_amiType = Lens.lens (\Nodegroup' {amiType} -> amiType) (\s@Nodegroup' {} a -> s {amiType = a} :: Nodegroup)

-- | If the node group wasn\'t deployed with a launch template, then this is
-- the disk size in the node group configuration. If the node group was
-- deployed with a launch template, then this is @null@.
nodegroup_diskSize :: Lens.Lens' Nodegroup (Prelude.Maybe Prelude.Int)
nodegroup_diskSize = Lens.lens (\Nodegroup' {diskSize} -> diskSize) (\s@Nodegroup' {} a -> s {diskSize = a} :: Nodegroup)

-- | The subnets that were specified for the Auto Scaling group that is
-- associated with your node group.
nodegroup_subnets :: Lens.Lens' Nodegroup (Prelude.Maybe [Prelude.Text])
nodegroup_subnets = Lens.lens (\Nodegroup' {subnets} -> subnets) (\s@Nodegroup' {} a -> s {subnets = a} :: Nodegroup) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) associated with the managed node group.
nodegroup_nodegroupArn :: Lens.Lens' Nodegroup (Prelude.Maybe Prelude.Text)
nodegroup_nodegroupArn = Lens.lens (\Nodegroup' {nodegroupArn} -> nodegroupArn) (\s@Nodegroup' {} a -> s {nodegroupArn = a} :: Nodegroup)

-- | If a launch template was used to create the node group, then this is the
-- launch template that was used.
nodegroup_launchTemplate :: Lens.Lens' Nodegroup (Prelude.Maybe LaunchTemplateSpecification)
nodegroup_launchTemplate = Lens.lens (\Nodegroup' {launchTemplate} -> launchTemplate) (\s@Nodegroup' {} a -> s {launchTemplate = a} :: Nodegroup)

-- | The name associated with an Amazon EKS managed node group.
nodegroup_nodegroupName :: Lens.Lens' Nodegroup (Prelude.Maybe Prelude.Text)
nodegroup_nodegroupName = Lens.lens (\Nodegroup' {nodegroupName} -> nodegroupName) (\s@Nodegroup' {} a -> s {nodegroupName = a} :: Nodegroup)

-- | The Kubernetes taints to be applied to the nodes in the node group when
-- they are created. Effect is one of @No_Schedule@, @Prefer_No_Schedule@,
-- or @No_Execute@. Kubernetes taints can be used together with tolerations
-- to control how workloads are scheduled to your nodes. For more
-- information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/node-taints-managed-node-groups.html Node taints on managed node groups>.
nodegroup_taints :: Lens.Lens' Nodegroup (Prelude.Maybe [Taint])
nodegroup_taints = Lens.lens (\Nodegroup' {taints} -> taints) (\s@Nodegroup' {} a -> s {taints = a} :: Nodegroup) Prelude.. Lens.mapping Lens.coerced

-- | The IAM role associated with your node group. The Amazon EKS node
-- @kubelet@ daemon makes calls to Amazon Web Services APIs on your behalf.
-- Nodes receive permissions for these API calls through an IAM instance
-- profile and associated policies.
nodegroup_nodeRole :: Lens.Lens' Nodegroup (Prelude.Maybe Prelude.Text)
nodegroup_nodeRole = Lens.lens (\Nodegroup' {nodeRole} -> nodeRole) (\s@Nodegroup' {} a -> s {nodeRole = a} :: Nodegroup)

-- | The current status of the managed node group.
nodegroup_status :: Lens.Lens' Nodegroup (Prelude.Maybe NodegroupStatus)
nodegroup_status = Lens.lens (\Nodegroup' {status} -> status) (\s@Nodegroup' {} a -> s {status = a} :: Nodegroup)

-- | The Kubernetes labels applied to the nodes in the node group.
--
-- Only labels that are applied with the Amazon EKS API are shown here.
-- There may be other Kubernetes labels applied to the nodes in this group.
nodegroup_labels :: Lens.Lens' Nodegroup (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
nodegroup_labels = Lens.lens (\Nodegroup' {labels} -> labels) (\s@Nodegroup' {} a -> s {labels = a} :: Nodegroup) Prelude.. Lens.mapping Lens.coerced

-- | The health status of the node group. If there are issues with your node
-- group\'s health, they are listed here.
nodegroup_health :: Lens.Lens' Nodegroup (Prelude.Maybe NodegroupHealth)
nodegroup_health = Lens.lens (\Nodegroup' {health} -> health) (\s@Nodegroup' {} a -> s {health = a} :: Nodegroup)

-- | The resources associated with the node group, such as Auto Scaling
-- groups and security groups for remote access.
nodegroup_resources :: Lens.Lens' Nodegroup (Prelude.Maybe NodegroupResources)
nodegroup_resources = Lens.lens (\Nodegroup' {resources} -> resources) (\s@Nodegroup' {} a -> s {resources = a} :: Nodegroup)

-- | The scaling configuration details for the Auto Scaling group that is
-- associated with your node group.
nodegroup_scalingConfig :: Lens.Lens' Nodegroup (Prelude.Maybe NodegroupScalingConfig)
nodegroup_scalingConfig = Lens.lens (\Nodegroup' {scalingConfig} -> scalingConfig) (\s@Nodegroup' {} a -> s {scalingConfig = a} :: Nodegroup)

-- | The name of the cluster that the managed node group resides in.
nodegroup_clusterName :: Lens.Lens' Nodegroup (Prelude.Maybe Prelude.Text)
nodegroup_clusterName = Lens.lens (\Nodegroup' {clusterName} -> clusterName) (\s@Nodegroup' {} a -> s {clusterName = a} :: Nodegroup)

-- | The Unix epoch timestamp in seconds for when the managed node group was
-- created.
nodegroup_createdAt :: Lens.Lens' Nodegroup (Prelude.Maybe Prelude.UTCTime)
nodegroup_createdAt = Lens.lens (\Nodegroup' {createdAt} -> createdAt) (\s@Nodegroup' {} a -> s {createdAt = a} :: Nodegroup) Prelude.. Lens.mapping Data._Time

-- | The Kubernetes version of the managed node group.
nodegroup_version :: Lens.Lens' Nodegroup (Prelude.Maybe Prelude.Text)
nodegroup_version = Lens.lens (\Nodegroup' {version} -> version) (\s@Nodegroup' {} a -> s {version = a} :: Nodegroup)

instance Data.FromJSON Nodegroup where
  parseJSON =
    Data.withObject
      "Nodegroup"
      ( \x ->
          Nodegroup'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "releaseVersion")
            Prelude.<*> (x Data..:? "modifiedAt")
            Prelude.<*> (x Data..:? "remoteAccess")
            Prelude.<*> (x Data..:? "instanceTypes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "updateConfig")
            Prelude.<*> (x Data..:? "capacityType")
            Prelude.<*> (x Data..:? "amiType")
            Prelude.<*> (x Data..:? "diskSize")
            Prelude.<*> (x Data..:? "subnets" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "nodegroupArn")
            Prelude.<*> (x Data..:? "launchTemplate")
            Prelude.<*> (x Data..:? "nodegroupName")
            Prelude.<*> (x Data..:? "taints" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "nodeRole")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "labels" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "health")
            Prelude.<*> (x Data..:? "resources")
            Prelude.<*> (x Data..:? "scalingConfig")
            Prelude.<*> (x Data..:? "clusterName")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "version")
      )

instance Prelude.Hashable Nodegroup where
  hashWithSalt _salt Nodegroup' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` releaseVersion
      `Prelude.hashWithSalt` modifiedAt
      `Prelude.hashWithSalt` remoteAccess
      `Prelude.hashWithSalt` instanceTypes
      `Prelude.hashWithSalt` updateConfig
      `Prelude.hashWithSalt` capacityType
      `Prelude.hashWithSalt` amiType
      `Prelude.hashWithSalt` diskSize
      `Prelude.hashWithSalt` subnets
      `Prelude.hashWithSalt` nodegroupArn
      `Prelude.hashWithSalt` launchTemplate
      `Prelude.hashWithSalt` nodegroupName
      `Prelude.hashWithSalt` taints
      `Prelude.hashWithSalt` nodeRole
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` labels
      `Prelude.hashWithSalt` health
      `Prelude.hashWithSalt` resources
      `Prelude.hashWithSalt` scalingConfig
      `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` version

instance Prelude.NFData Nodegroup where
  rnf Nodegroup' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf releaseVersion
      `Prelude.seq` Prelude.rnf modifiedAt
      `Prelude.seq` Prelude.rnf remoteAccess
      `Prelude.seq` Prelude.rnf instanceTypes
      `Prelude.seq` Prelude.rnf updateConfig
      `Prelude.seq` Prelude.rnf capacityType
      `Prelude.seq` Prelude.rnf amiType
      `Prelude.seq` Prelude.rnf diskSize
      `Prelude.seq` Prelude.rnf subnets
      `Prelude.seq` Prelude.rnf nodegroupArn
      `Prelude.seq` Prelude.rnf launchTemplate
      `Prelude.seq` Prelude.rnf nodegroupName
      `Prelude.seq` Prelude.rnf taints
      `Prelude.seq` Prelude.rnf nodeRole
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf labels
      `Prelude.seq` Prelude.rnf health
      `Prelude.seq` Prelude.rnf resources
      `Prelude.seq` Prelude.rnf scalingConfig
      `Prelude.seq` Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf version
