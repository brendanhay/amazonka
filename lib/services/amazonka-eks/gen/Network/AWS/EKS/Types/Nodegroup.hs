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
-- Module      : Network.AWS.EKS.Types.Nodegroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.Nodegroup where

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.Types.AMITypes
import Network.AWS.EKS.Types.CapacityTypes
import Network.AWS.EKS.Types.LaunchTemplateSpecification
import Network.AWS.EKS.Types.NodegroupHealth
import Network.AWS.EKS.Types.NodegroupResources
import Network.AWS.EKS.Types.NodegroupScalingConfig
import Network.AWS.EKS.Types.NodegroupStatus
import Network.AWS.EKS.Types.NodegroupUpdateConfig
import Network.AWS.EKS.Types.RemoteAccessConfig
import Network.AWS.EKS.Types.Taint
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing an Amazon EKS managed node group.
--
-- /See:/ 'newNodegroup' smart constructor.
data Nodegroup = Nodegroup'
  { -- | The scaling configuration details for the Auto Scaling group that is
    -- associated with your node group.
    scalingConfig :: Prelude.Maybe NodegroupScalingConfig,
    -- | The capacity type of your managed node group.
    capacityType :: Prelude.Maybe CapacityTypes,
    -- | The current status of the managed node group.
    status :: Prelude.Maybe NodegroupStatus,
    -- | The Unix epoch timestamp in seconds for when the managed node group was
    -- last modified.
    modifiedAt :: Prelude.Maybe Core.POSIX,
    -- | If the node group was deployed using a launch template with a custom
    -- AMI, then this is the AMI ID that was specified in the launch template.
    -- For node groups that weren\'t deployed using a launch template, this is
    -- the version of the Amazon EKS optimized AMI that the node group was
    -- deployed with.
    releaseVersion :: Prelude.Maybe Prelude.Text,
    -- | The name associated with an Amazon EKS managed node group.
    nodegroupName :: Prelude.Maybe Prelude.Text,
    -- | If the node group wasn\'t deployed with a launch template, then this is
    -- the remote access configuration that is associated with the node group.
    -- If the node group was deployed with a launch template, then this is
    -- @null@.
    remoteAccess :: Prelude.Maybe RemoteAccessConfig,
    -- | If the node group wasn\'t deployed with a launch template, then this is
    -- the disk size in the node group configuration. If the node group was
    -- deployed with a launch template, then this is @null@.
    diskSize :: Prelude.Maybe Prelude.Int,
    -- | The Unix epoch timestamp in seconds for when the managed node group was
    -- created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The Kubernetes labels applied to the nodes in the node group.
    --
    -- Only labels that are applied with the Amazon EKS API are shown here.
    -- There may be other Kubernetes labels applied to the nodes in this group.
    labels :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | If a launch template was used to create the node group, then this is the
    -- launch template that was used.
    launchTemplate :: Prelude.Maybe LaunchTemplateSpecification,
    -- | The Kubernetes version of the managed node group.
    version :: Prelude.Maybe Prelude.Text,
    -- | The IAM role associated with your node group. The Amazon EKS node
    -- @kubelet@ daemon makes calls to Amazon Web Services APIs on your behalf.
    -- Nodes receive permissions for these API calls through an IAM instance
    -- profile and associated policies.
    nodeRole :: Prelude.Maybe Prelude.Text,
    -- | The health status of the node group. If there are issues with your node
    -- group\'s health, they are listed here.
    health :: Prelude.Maybe NodegroupHealth,
    -- | The resources associated with the node group, such as Auto Scaling
    -- groups and security groups for remote access.
    resources :: Prelude.Maybe NodegroupResources,
    -- | The metadata applied to the node group to assist with categorization and
    -- organization. Each tag consists of a key and an optional value, both of
    -- which you define. Node group tags do not propagate to any other
    -- resources associated with the node group, such as the Amazon EC2
    -- instances or subnets.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The node group update configuration.
    updateConfig :: Prelude.Maybe NodegroupUpdateConfig,
    -- | The subnets that were specified for the Auto Scaling group that is
    -- associated with your node group.
    subnets :: Prelude.Maybe [Prelude.Text],
    -- | The name of the cluster that the managed node group resides in.
    clusterName :: Prelude.Maybe Prelude.Text,
    -- | If the node group was deployed using a launch template with a custom
    -- AMI, then this is @CUSTOM@. For node groups that weren\'t deployed using
    -- a launch template, this is the AMI type that was specified in the node
    -- group configuration.
    amiType :: Prelude.Maybe AMITypes,
    -- | The Kubernetes taints to be applied to the nodes in the node group when
    -- they are created. Effect is one of @No_Schedule@, @Prefer_No_Schedule@,
    -- or @No_Execute@. Kubernetes taints can be used together with tolerations
    -- to control how workloads are scheduled to your nodes.
    taints :: Prelude.Maybe [Taint],
    -- | If the node group wasn\'t deployed with a launch template, then this is
    -- the instance type that is associated with the node group. If the node
    -- group was deployed with a launch template, then this is @null@.
    instanceTypes :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) associated with the managed node group.
    nodegroupArn :: Prelude.Maybe Prelude.Text
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
-- 'scalingConfig', 'nodegroup_scalingConfig' - The scaling configuration details for the Auto Scaling group that is
-- associated with your node group.
--
-- 'capacityType', 'nodegroup_capacityType' - The capacity type of your managed node group.
--
-- 'status', 'nodegroup_status' - The current status of the managed node group.
--
-- 'modifiedAt', 'nodegroup_modifiedAt' - The Unix epoch timestamp in seconds for when the managed node group was
-- last modified.
--
-- 'releaseVersion', 'nodegroup_releaseVersion' - If the node group was deployed using a launch template with a custom
-- AMI, then this is the AMI ID that was specified in the launch template.
-- For node groups that weren\'t deployed using a launch template, this is
-- the version of the Amazon EKS optimized AMI that the node group was
-- deployed with.
--
-- 'nodegroupName', 'nodegroup_nodegroupName' - The name associated with an Amazon EKS managed node group.
--
-- 'remoteAccess', 'nodegroup_remoteAccess' - If the node group wasn\'t deployed with a launch template, then this is
-- the remote access configuration that is associated with the node group.
-- If the node group was deployed with a launch template, then this is
-- @null@.
--
-- 'diskSize', 'nodegroup_diskSize' - If the node group wasn\'t deployed with a launch template, then this is
-- the disk size in the node group configuration. If the node group was
-- deployed with a launch template, then this is @null@.
--
-- 'createdAt', 'nodegroup_createdAt' - The Unix epoch timestamp in seconds for when the managed node group was
-- created.
--
-- 'labels', 'nodegroup_labels' - The Kubernetes labels applied to the nodes in the node group.
--
-- Only labels that are applied with the Amazon EKS API are shown here.
-- There may be other Kubernetes labels applied to the nodes in this group.
--
-- 'launchTemplate', 'nodegroup_launchTemplate' - If a launch template was used to create the node group, then this is the
-- launch template that was used.
--
-- 'version', 'nodegroup_version' - The Kubernetes version of the managed node group.
--
-- 'nodeRole', 'nodegroup_nodeRole' - The IAM role associated with your node group. The Amazon EKS node
-- @kubelet@ daemon makes calls to Amazon Web Services APIs on your behalf.
-- Nodes receive permissions for these API calls through an IAM instance
-- profile and associated policies.
--
-- 'health', 'nodegroup_health' - The health status of the node group. If there are issues with your node
-- group\'s health, they are listed here.
--
-- 'resources', 'nodegroup_resources' - The resources associated with the node group, such as Auto Scaling
-- groups and security groups for remote access.
--
-- 'tags', 'nodegroup_tags' - The metadata applied to the node group to assist with categorization and
-- organization. Each tag consists of a key and an optional value, both of
-- which you define. Node group tags do not propagate to any other
-- resources associated with the node group, such as the Amazon EC2
-- instances or subnets.
--
-- 'updateConfig', 'nodegroup_updateConfig' - The node group update configuration.
--
-- 'subnets', 'nodegroup_subnets' - The subnets that were specified for the Auto Scaling group that is
-- associated with your node group.
--
-- 'clusterName', 'nodegroup_clusterName' - The name of the cluster that the managed node group resides in.
--
-- 'amiType', 'nodegroup_amiType' - If the node group was deployed using a launch template with a custom
-- AMI, then this is @CUSTOM@. For node groups that weren\'t deployed using
-- a launch template, this is the AMI type that was specified in the node
-- group configuration.
--
-- 'taints', 'nodegroup_taints' - The Kubernetes taints to be applied to the nodes in the node group when
-- they are created. Effect is one of @No_Schedule@, @Prefer_No_Schedule@,
-- or @No_Execute@. Kubernetes taints can be used together with tolerations
-- to control how workloads are scheduled to your nodes.
--
-- 'instanceTypes', 'nodegroup_instanceTypes' - If the node group wasn\'t deployed with a launch template, then this is
-- the instance type that is associated with the node group. If the node
-- group was deployed with a launch template, then this is @null@.
--
-- 'nodegroupArn', 'nodegroup_nodegroupArn' - The Amazon Resource Name (ARN) associated with the managed node group.
newNodegroup ::
  Nodegroup
newNodegroup =
  Nodegroup'
    { scalingConfig = Prelude.Nothing,
      capacityType = Prelude.Nothing,
      status = Prelude.Nothing,
      modifiedAt = Prelude.Nothing,
      releaseVersion = Prelude.Nothing,
      nodegroupName = Prelude.Nothing,
      remoteAccess = Prelude.Nothing,
      diskSize = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      labels = Prelude.Nothing,
      launchTemplate = Prelude.Nothing,
      version = Prelude.Nothing,
      nodeRole = Prelude.Nothing,
      health = Prelude.Nothing,
      resources = Prelude.Nothing,
      tags = Prelude.Nothing,
      updateConfig = Prelude.Nothing,
      subnets = Prelude.Nothing,
      clusterName = Prelude.Nothing,
      amiType = Prelude.Nothing,
      taints = Prelude.Nothing,
      instanceTypes = Prelude.Nothing,
      nodegroupArn = Prelude.Nothing
    }

-- | The scaling configuration details for the Auto Scaling group that is
-- associated with your node group.
nodegroup_scalingConfig :: Lens.Lens' Nodegroup (Prelude.Maybe NodegroupScalingConfig)
nodegroup_scalingConfig = Lens.lens (\Nodegroup' {scalingConfig} -> scalingConfig) (\s@Nodegroup' {} a -> s {scalingConfig = a} :: Nodegroup)

-- | The capacity type of your managed node group.
nodegroup_capacityType :: Lens.Lens' Nodegroup (Prelude.Maybe CapacityTypes)
nodegroup_capacityType = Lens.lens (\Nodegroup' {capacityType} -> capacityType) (\s@Nodegroup' {} a -> s {capacityType = a} :: Nodegroup)

-- | The current status of the managed node group.
nodegroup_status :: Lens.Lens' Nodegroup (Prelude.Maybe NodegroupStatus)
nodegroup_status = Lens.lens (\Nodegroup' {status} -> status) (\s@Nodegroup' {} a -> s {status = a} :: Nodegroup)

-- | The Unix epoch timestamp in seconds for when the managed node group was
-- last modified.
nodegroup_modifiedAt :: Lens.Lens' Nodegroup (Prelude.Maybe Prelude.UTCTime)
nodegroup_modifiedAt = Lens.lens (\Nodegroup' {modifiedAt} -> modifiedAt) (\s@Nodegroup' {} a -> s {modifiedAt = a} :: Nodegroup) Prelude.. Lens.mapping Core._Time

-- | If the node group was deployed using a launch template with a custom
-- AMI, then this is the AMI ID that was specified in the launch template.
-- For node groups that weren\'t deployed using a launch template, this is
-- the version of the Amazon EKS optimized AMI that the node group was
-- deployed with.
nodegroup_releaseVersion :: Lens.Lens' Nodegroup (Prelude.Maybe Prelude.Text)
nodegroup_releaseVersion = Lens.lens (\Nodegroup' {releaseVersion} -> releaseVersion) (\s@Nodegroup' {} a -> s {releaseVersion = a} :: Nodegroup)

-- | The name associated with an Amazon EKS managed node group.
nodegroup_nodegroupName :: Lens.Lens' Nodegroup (Prelude.Maybe Prelude.Text)
nodegroup_nodegroupName = Lens.lens (\Nodegroup' {nodegroupName} -> nodegroupName) (\s@Nodegroup' {} a -> s {nodegroupName = a} :: Nodegroup)

-- | If the node group wasn\'t deployed with a launch template, then this is
-- the remote access configuration that is associated with the node group.
-- If the node group was deployed with a launch template, then this is
-- @null@.
nodegroup_remoteAccess :: Lens.Lens' Nodegroup (Prelude.Maybe RemoteAccessConfig)
nodegroup_remoteAccess = Lens.lens (\Nodegroup' {remoteAccess} -> remoteAccess) (\s@Nodegroup' {} a -> s {remoteAccess = a} :: Nodegroup)

-- | If the node group wasn\'t deployed with a launch template, then this is
-- the disk size in the node group configuration. If the node group was
-- deployed with a launch template, then this is @null@.
nodegroup_diskSize :: Lens.Lens' Nodegroup (Prelude.Maybe Prelude.Int)
nodegroup_diskSize = Lens.lens (\Nodegroup' {diskSize} -> diskSize) (\s@Nodegroup' {} a -> s {diskSize = a} :: Nodegroup)

-- | The Unix epoch timestamp in seconds for when the managed node group was
-- created.
nodegroup_createdAt :: Lens.Lens' Nodegroup (Prelude.Maybe Prelude.UTCTime)
nodegroup_createdAt = Lens.lens (\Nodegroup' {createdAt} -> createdAt) (\s@Nodegroup' {} a -> s {createdAt = a} :: Nodegroup) Prelude.. Lens.mapping Core._Time

-- | The Kubernetes labels applied to the nodes in the node group.
--
-- Only labels that are applied with the Amazon EKS API are shown here.
-- There may be other Kubernetes labels applied to the nodes in this group.
nodegroup_labels :: Lens.Lens' Nodegroup (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
nodegroup_labels = Lens.lens (\Nodegroup' {labels} -> labels) (\s@Nodegroup' {} a -> s {labels = a} :: Nodegroup) Prelude.. Lens.mapping Lens._Coerce

-- | If a launch template was used to create the node group, then this is the
-- launch template that was used.
nodegroup_launchTemplate :: Lens.Lens' Nodegroup (Prelude.Maybe LaunchTemplateSpecification)
nodegroup_launchTemplate = Lens.lens (\Nodegroup' {launchTemplate} -> launchTemplate) (\s@Nodegroup' {} a -> s {launchTemplate = a} :: Nodegroup)

-- | The Kubernetes version of the managed node group.
nodegroup_version :: Lens.Lens' Nodegroup (Prelude.Maybe Prelude.Text)
nodegroup_version = Lens.lens (\Nodegroup' {version} -> version) (\s@Nodegroup' {} a -> s {version = a} :: Nodegroup)

-- | The IAM role associated with your node group. The Amazon EKS node
-- @kubelet@ daemon makes calls to Amazon Web Services APIs on your behalf.
-- Nodes receive permissions for these API calls through an IAM instance
-- profile and associated policies.
nodegroup_nodeRole :: Lens.Lens' Nodegroup (Prelude.Maybe Prelude.Text)
nodegroup_nodeRole = Lens.lens (\Nodegroup' {nodeRole} -> nodeRole) (\s@Nodegroup' {} a -> s {nodeRole = a} :: Nodegroup)

-- | The health status of the node group. If there are issues with your node
-- group\'s health, they are listed here.
nodegroup_health :: Lens.Lens' Nodegroup (Prelude.Maybe NodegroupHealth)
nodegroup_health = Lens.lens (\Nodegroup' {health} -> health) (\s@Nodegroup' {} a -> s {health = a} :: Nodegroup)

-- | The resources associated with the node group, such as Auto Scaling
-- groups and security groups for remote access.
nodegroup_resources :: Lens.Lens' Nodegroup (Prelude.Maybe NodegroupResources)
nodegroup_resources = Lens.lens (\Nodegroup' {resources} -> resources) (\s@Nodegroup' {} a -> s {resources = a} :: Nodegroup)

-- | The metadata applied to the node group to assist with categorization and
-- organization. Each tag consists of a key and an optional value, both of
-- which you define. Node group tags do not propagate to any other
-- resources associated with the node group, such as the Amazon EC2
-- instances or subnets.
nodegroup_tags :: Lens.Lens' Nodegroup (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
nodegroup_tags = Lens.lens (\Nodegroup' {tags} -> tags) (\s@Nodegroup' {} a -> s {tags = a} :: Nodegroup) Prelude.. Lens.mapping Lens._Coerce

-- | The node group update configuration.
nodegroup_updateConfig :: Lens.Lens' Nodegroup (Prelude.Maybe NodegroupUpdateConfig)
nodegroup_updateConfig = Lens.lens (\Nodegroup' {updateConfig} -> updateConfig) (\s@Nodegroup' {} a -> s {updateConfig = a} :: Nodegroup)

-- | The subnets that were specified for the Auto Scaling group that is
-- associated with your node group.
nodegroup_subnets :: Lens.Lens' Nodegroup (Prelude.Maybe [Prelude.Text])
nodegroup_subnets = Lens.lens (\Nodegroup' {subnets} -> subnets) (\s@Nodegroup' {} a -> s {subnets = a} :: Nodegroup) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the cluster that the managed node group resides in.
nodegroup_clusterName :: Lens.Lens' Nodegroup (Prelude.Maybe Prelude.Text)
nodegroup_clusterName = Lens.lens (\Nodegroup' {clusterName} -> clusterName) (\s@Nodegroup' {} a -> s {clusterName = a} :: Nodegroup)

-- | If the node group was deployed using a launch template with a custom
-- AMI, then this is @CUSTOM@. For node groups that weren\'t deployed using
-- a launch template, this is the AMI type that was specified in the node
-- group configuration.
nodegroup_amiType :: Lens.Lens' Nodegroup (Prelude.Maybe AMITypes)
nodegroup_amiType = Lens.lens (\Nodegroup' {amiType} -> amiType) (\s@Nodegroup' {} a -> s {amiType = a} :: Nodegroup)

-- | The Kubernetes taints to be applied to the nodes in the node group when
-- they are created. Effect is one of @No_Schedule@, @Prefer_No_Schedule@,
-- or @No_Execute@. Kubernetes taints can be used together with tolerations
-- to control how workloads are scheduled to your nodes.
nodegroup_taints :: Lens.Lens' Nodegroup (Prelude.Maybe [Taint])
nodegroup_taints = Lens.lens (\Nodegroup' {taints} -> taints) (\s@Nodegroup' {} a -> s {taints = a} :: Nodegroup) Prelude.. Lens.mapping Lens._Coerce

-- | If the node group wasn\'t deployed with a launch template, then this is
-- the instance type that is associated with the node group. If the node
-- group was deployed with a launch template, then this is @null@.
nodegroup_instanceTypes :: Lens.Lens' Nodegroup (Prelude.Maybe [Prelude.Text])
nodegroup_instanceTypes = Lens.lens (\Nodegroup' {instanceTypes} -> instanceTypes) (\s@Nodegroup' {} a -> s {instanceTypes = a} :: Nodegroup) Prelude.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) associated with the managed node group.
nodegroup_nodegroupArn :: Lens.Lens' Nodegroup (Prelude.Maybe Prelude.Text)
nodegroup_nodegroupArn = Lens.lens (\Nodegroup' {nodegroupArn} -> nodegroupArn) (\s@Nodegroup' {} a -> s {nodegroupArn = a} :: Nodegroup)

instance Core.FromJSON Nodegroup where
  parseJSON =
    Core.withObject
      "Nodegroup"
      ( \x ->
          Nodegroup'
            Prelude.<$> (x Core..:? "scalingConfig")
            Prelude.<*> (x Core..:? "capacityType")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "modifiedAt")
            Prelude.<*> (x Core..:? "releaseVersion")
            Prelude.<*> (x Core..:? "nodegroupName")
            Prelude.<*> (x Core..:? "remoteAccess")
            Prelude.<*> (x Core..:? "diskSize")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "labels" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "launchTemplate")
            Prelude.<*> (x Core..:? "version")
            Prelude.<*> (x Core..:? "nodeRole")
            Prelude.<*> (x Core..:? "health")
            Prelude.<*> (x Core..:? "resources")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "updateConfig")
            Prelude.<*> (x Core..:? "subnets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "clusterName")
            Prelude.<*> (x Core..:? "amiType")
            Prelude.<*> (x Core..:? "taints" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "instanceTypes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "nodegroupArn")
      )

instance Prelude.Hashable Nodegroup

instance Prelude.NFData Nodegroup
