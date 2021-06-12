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
import Network.AWS.EKS.Types.RemoteAccessConfig
import qualified Network.AWS.Lens as Lens

-- | An object representing an Amazon EKS managed node group.
--
-- /See:/ 'newNodegroup' smart constructor.
data Nodegroup = Nodegroup'
  { -- | The scaling configuration details for the Auto Scaling group that is
    -- associated with your node group.
    scalingConfig :: Core.Maybe NodegroupScalingConfig,
    -- | The Unix epoch timestamp in seconds for when the managed node group was
    -- last modified.
    modifiedAt :: Core.Maybe Core.POSIX,
    -- | The current status of the managed node group.
    status :: Core.Maybe NodegroupStatus,
    -- | The capacity type of your managed node group.
    capacityType :: Core.Maybe CapacityTypes,
    -- | If the node group was deployed using a launch template with a custom
    -- AMI, then this is the AMI ID that was specified in the launch template.
    -- For node groups that weren\'t deployed using a launch template, this is
    -- the version of the Amazon EKS optimized AMI that the node group was
    -- deployed with.
    releaseVersion :: Core.Maybe Core.Text,
    -- | If the node group wasn\'t deployed with a launch template, then this is
    -- the disk size in the node group configuration. If the node group was
    -- deployed with a launch template, then this is @null@.
    diskSize :: Core.Maybe Core.Int,
    -- | The name associated with an Amazon EKS managed node group.
    nodegroupName :: Core.Maybe Core.Text,
    -- | If the node group wasn\'t deployed with a launch template, then this is
    -- the remote access configuration that is associated with the node group.
    -- If the node group was deployed with a launch template, then this is
    -- @null@.
    remoteAccess :: Core.Maybe RemoteAccessConfig,
    -- | The Unix epoch timestamp in seconds for when the managed node group was
    -- created.
    createdAt :: Core.Maybe Core.POSIX,
    -- | If a launch template was used to create the node group, then this is the
    -- launch template that was used.
    launchTemplate :: Core.Maybe LaunchTemplateSpecification,
    -- | The Kubernetes labels applied to the nodes in the node group.
    --
    -- Only labels that are applied with the Amazon EKS API are shown here.
    -- There may be other Kubernetes labels applied to the nodes in this group.
    labels :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The Kubernetes version of the managed node group.
    version :: Core.Maybe Core.Text,
    -- | The IAM role associated with your node group. The Amazon EKS node
    -- @kubelet@ daemon makes calls to AWS APIs on your behalf. Nodes receive
    -- permissions for these API calls through an IAM instance profile and
    -- associated policies.
    nodeRole :: Core.Maybe Core.Text,
    -- | The health status of the node group. If there are issues with your node
    -- group\'s health, they are listed here.
    health :: Core.Maybe NodegroupHealth,
    -- | The resources associated with the node group, such as Auto Scaling
    -- groups and security groups for remote access.
    resources :: Core.Maybe NodegroupResources,
    -- | The metadata applied to the node group to assist with categorization and
    -- organization. Each tag consists of a key and an optional value, both of
    -- which you define. Node group tags do not propagate to any other
    -- resources associated with the node group, such as the Amazon EC2
    -- instances or subnets.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The subnets that were specified for the Auto Scaling group that is
    -- associated with your node group.
    subnets :: Core.Maybe [Core.Text],
    -- | If the node group was deployed using a launch template with a custom
    -- AMI, then this is @CUSTOM@. For node groups that weren\'t deployed using
    -- a launch template, this is the AMI type that was specified in the node
    -- group configuration.
    amiType :: Core.Maybe AMITypes,
    -- | The name of the cluster that the managed node group resides in.
    clusterName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) associated with the managed node group.
    nodegroupArn :: Core.Maybe Core.Text,
    -- | If the node group wasn\'t deployed with a launch template, then this is
    -- the instance type that is associated with the node group. If the node
    -- group was deployed with a launch template, then this is @null@.
    instanceTypes :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'modifiedAt', 'nodegroup_modifiedAt' - The Unix epoch timestamp in seconds for when the managed node group was
-- last modified.
--
-- 'status', 'nodegroup_status' - The current status of the managed node group.
--
-- 'capacityType', 'nodegroup_capacityType' - The capacity type of your managed node group.
--
-- 'releaseVersion', 'nodegroup_releaseVersion' - If the node group was deployed using a launch template with a custom
-- AMI, then this is the AMI ID that was specified in the launch template.
-- For node groups that weren\'t deployed using a launch template, this is
-- the version of the Amazon EKS optimized AMI that the node group was
-- deployed with.
--
-- 'diskSize', 'nodegroup_diskSize' - If the node group wasn\'t deployed with a launch template, then this is
-- the disk size in the node group configuration. If the node group was
-- deployed with a launch template, then this is @null@.
--
-- 'nodegroupName', 'nodegroup_nodegroupName' - The name associated with an Amazon EKS managed node group.
--
-- 'remoteAccess', 'nodegroup_remoteAccess' - If the node group wasn\'t deployed with a launch template, then this is
-- the remote access configuration that is associated with the node group.
-- If the node group was deployed with a launch template, then this is
-- @null@.
--
-- 'createdAt', 'nodegroup_createdAt' - The Unix epoch timestamp in seconds for when the managed node group was
-- created.
--
-- 'launchTemplate', 'nodegroup_launchTemplate' - If a launch template was used to create the node group, then this is the
-- launch template that was used.
--
-- 'labels', 'nodegroup_labels' - The Kubernetes labels applied to the nodes in the node group.
--
-- Only labels that are applied with the Amazon EKS API are shown here.
-- There may be other Kubernetes labels applied to the nodes in this group.
--
-- 'version', 'nodegroup_version' - The Kubernetes version of the managed node group.
--
-- 'nodeRole', 'nodegroup_nodeRole' - The IAM role associated with your node group. The Amazon EKS node
-- @kubelet@ daemon makes calls to AWS APIs on your behalf. Nodes receive
-- permissions for these API calls through an IAM instance profile and
-- associated policies.
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
-- 'subnets', 'nodegroup_subnets' - The subnets that were specified for the Auto Scaling group that is
-- associated with your node group.
--
-- 'amiType', 'nodegroup_amiType' - If the node group was deployed using a launch template with a custom
-- AMI, then this is @CUSTOM@. For node groups that weren\'t deployed using
-- a launch template, this is the AMI type that was specified in the node
-- group configuration.
--
-- 'clusterName', 'nodegroup_clusterName' - The name of the cluster that the managed node group resides in.
--
-- 'nodegroupArn', 'nodegroup_nodegroupArn' - The Amazon Resource Name (ARN) associated with the managed node group.
--
-- 'instanceTypes', 'nodegroup_instanceTypes' - If the node group wasn\'t deployed with a launch template, then this is
-- the instance type that is associated with the node group. If the node
-- group was deployed with a launch template, then this is @null@.
newNodegroup ::
  Nodegroup
newNodegroup =
  Nodegroup'
    { scalingConfig = Core.Nothing,
      modifiedAt = Core.Nothing,
      status = Core.Nothing,
      capacityType = Core.Nothing,
      releaseVersion = Core.Nothing,
      diskSize = Core.Nothing,
      nodegroupName = Core.Nothing,
      remoteAccess = Core.Nothing,
      createdAt = Core.Nothing,
      launchTemplate = Core.Nothing,
      labels = Core.Nothing,
      version = Core.Nothing,
      nodeRole = Core.Nothing,
      health = Core.Nothing,
      resources = Core.Nothing,
      tags = Core.Nothing,
      subnets = Core.Nothing,
      amiType = Core.Nothing,
      clusterName = Core.Nothing,
      nodegroupArn = Core.Nothing,
      instanceTypes = Core.Nothing
    }

-- | The scaling configuration details for the Auto Scaling group that is
-- associated with your node group.
nodegroup_scalingConfig :: Lens.Lens' Nodegroup (Core.Maybe NodegroupScalingConfig)
nodegroup_scalingConfig = Lens.lens (\Nodegroup' {scalingConfig} -> scalingConfig) (\s@Nodegroup' {} a -> s {scalingConfig = a} :: Nodegroup)

-- | The Unix epoch timestamp in seconds for when the managed node group was
-- last modified.
nodegroup_modifiedAt :: Lens.Lens' Nodegroup (Core.Maybe Core.UTCTime)
nodegroup_modifiedAt = Lens.lens (\Nodegroup' {modifiedAt} -> modifiedAt) (\s@Nodegroup' {} a -> s {modifiedAt = a} :: Nodegroup) Core.. Lens.mapping Core._Time

-- | The current status of the managed node group.
nodegroup_status :: Lens.Lens' Nodegroup (Core.Maybe NodegroupStatus)
nodegroup_status = Lens.lens (\Nodegroup' {status} -> status) (\s@Nodegroup' {} a -> s {status = a} :: Nodegroup)

-- | The capacity type of your managed node group.
nodegroup_capacityType :: Lens.Lens' Nodegroup (Core.Maybe CapacityTypes)
nodegroup_capacityType = Lens.lens (\Nodegroup' {capacityType} -> capacityType) (\s@Nodegroup' {} a -> s {capacityType = a} :: Nodegroup)

-- | If the node group was deployed using a launch template with a custom
-- AMI, then this is the AMI ID that was specified in the launch template.
-- For node groups that weren\'t deployed using a launch template, this is
-- the version of the Amazon EKS optimized AMI that the node group was
-- deployed with.
nodegroup_releaseVersion :: Lens.Lens' Nodegroup (Core.Maybe Core.Text)
nodegroup_releaseVersion = Lens.lens (\Nodegroup' {releaseVersion} -> releaseVersion) (\s@Nodegroup' {} a -> s {releaseVersion = a} :: Nodegroup)

-- | If the node group wasn\'t deployed with a launch template, then this is
-- the disk size in the node group configuration. If the node group was
-- deployed with a launch template, then this is @null@.
nodegroup_diskSize :: Lens.Lens' Nodegroup (Core.Maybe Core.Int)
nodegroup_diskSize = Lens.lens (\Nodegroup' {diskSize} -> diskSize) (\s@Nodegroup' {} a -> s {diskSize = a} :: Nodegroup)

-- | The name associated with an Amazon EKS managed node group.
nodegroup_nodegroupName :: Lens.Lens' Nodegroup (Core.Maybe Core.Text)
nodegroup_nodegroupName = Lens.lens (\Nodegroup' {nodegroupName} -> nodegroupName) (\s@Nodegroup' {} a -> s {nodegroupName = a} :: Nodegroup)

-- | If the node group wasn\'t deployed with a launch template, then this is
-- the remote access configuration that is associated with the node group.
-- If the node group was deployed with a launch template, then this is
-- @null@.
nodegroup_remoteAccess :: Lens.Lens' Nodegroup (Core.Maybe RemoteAccessConfig)
nodegroup_remoteAccess = Lens.lens (\Nodegroup' {remoteAccess} -> remoteAccess) (\s@Nodegroup' {} a -> s {remoteAccess = a} :: Nodegroup)

-- | The Unix epoch timestamp in seconds for when the managed node group was
-- created.
nodegroup_createdAt :: Lens.Lens' Nodegroup (Core.Maybe Core.UTCTime)
nodegroup_createdAt = Lens.lens (\Nodegroup' {createdAt} -> createdAt) (\s@Nodegroup' {} a -> s {createdAt = a} :: Nodegroup) Core.. Lens.mapping Core._Time

-- | If a launch template was used to create the node group, then this is the
-- launch template that was used.
nodegroup_launchTemplate :: Lens.Lens' Nodegroup (Core.Maybe LaunchTemplateSpecification)
nodegroup_launchTemplate = Lens.lens (\Nodegroup' {launchTemplate} -> launchTemplate) (\s@Nodegroup' {} a -> s {launchTemplate = a} :: Nodegroup)

-- | The Kubernetes labels applied to the nodes in the node group.
--
-- Only labels that are applied with the Amazon EKS API are shown here.
-- There may be other Kubernetes labels applied to the nodes in this group.
nodegroup_labels :: Lens.Lens' Nodegroup (Core.Maybe (Core.HashMap Core.Text Core.Text))
nodegroup_labels = Lens.lens (\Nodegroup' {labels} -> labels) (\s@Nodegroup' {} a -> s {labels = a} :: Nodegroup) Core.. Lens.mapping Lens._Coerce

-- | The Kubernetes version of the managed node group.
nodegroup_version :: Lens.Lens' Nodegroup (Core.Maybe Core.Text)
nodegroup_version = Lens.lens (\Nodegroup' {version} -> version) (\s@Nodegroup' {} a -> s {version = a} :: Nodegroup)

-- | The IAM role associated with your node group. The Amazon EKS node
-- @kubelet@ daemon makes calls to AWS APIs on your behalf. Nodes receive
-- permissions for these API calls through an IAM instance profile and
-- associated policies.
nodegroup_nodeRole :: Lens.Lens' Nodegroup (Core.Maybe Core.Text)
nodegroup_nodeRole = Lens.lens (\Nodegroup' {nodeRole} -> nodeRole) (\s@Nodegroup' {} a -> s {nodeRole = a} :: Nodegroup)

-- | The health status of the node group. If there are issues with your node
-- group\'s health, they are listed here.
nodegroup_health :: Lens.Lens' Nodegroup (Core.Maybe NodegroupHealth)
nodegroup_health = Lens.lens (\Nodegroup' {health} -> health) (\s@Nodegroup' {} a -> s {health = a} :: Nodegroup)

-- | The resources associated with the node group, such as Auto Scaling
-- groups and security groups for remote access.
nodegroup_resources :: Lens.Lens' Nodegroup (Core.Maybe NodegroupResources)
nodegroup_resources = Lens.lens (\Nodegroup' {resources} -> resources) (\s@Nodegroup' {} a -> s {resources = a} :: Nodegroup)

-- | The metadata applied to the node group to assist with categorization and
-- organization. Each tag consists of a key and an optional value, both of
-- which you define. Node group tags do not propagate to any other
-- resources associated with the node group, such as the Amazon EC2
-- instances or subnets.
nodegroup_tags :: Lens.Lens' Nodegroup (Core.Maybe (Core.HashMap Core.Text Core.Text))
nodegroup_tags = Lens.lens (\Nodegroup' {tags} -> tags) (\s@Nodegroup' {} a -> s {tags = a} :: Nodegroup) Core.. Lens.mapping Lens._Coerce

-- | The subnets that were specified for the Auto Scaling group that is
-- associated with your node group.
nodegroup_subnets :: Lens.Lens' Nodegroup (Core.Maybe [Core.Text])
nodegroup_subnets = Lens.lens (\Nodegroup' {subnets} -> subnets) (\s@Nodegroup' {} a -> s {subnets = a} :: Nodegroup) Core.. Lens.mapping Lens._Coerce

-- | If the node group was deployed using a launch template with a custom
-- AMI, then this is @CUSTOM@. For node groups that weren\'t deployed using
-- a launch template, this is the AMI type that was specified in the node
-- group configuration.
nodegroup_amiType :: Lens.Lens' Nodegroup (Core.Maybe AMITypes)
nodegroup_amiType = Lens.lens (\Nodegroup' {amiType} -> amiType) (\s@Nodegroup' {} a -> s {amiType = a} :: Nodegroup)

-- | The name of the cluster that the managed node group resides in.
nodegroup_clusterName :: Lens.Lens' Nodegroup (Core.Maybe Core.Text)
nodegroup_clusterName = Lens.lens (\Nodegroup' {clusterName} -> clusterName) (\s@Nodegroup' {} a -> s {clusterName = a} :: Nodegroup)

-- | The Amazon Resource Name (ARN) associated with the managed node group.
nodegroup_nodegroupArn :: Lens.Lens' Nodegroup (Core.Maybe Core.Text)
nodegroup_nodegroupArn = Lens.lens (\Nodegroup' {nodegroupArn} -> nodegroupArn) (\s@Nodegroup' {} a -> s {nodegroupArn = a} :: Nodegroup)

-- | If the node group wasn\'t deployed with a launch template, then this is
-- the instance type that is associated with the node group. If the node
-- group was deployed with a launch template, then this is @null@.
nodegroup_instanceTypes :: Lens.Lens' Nodegroup (Core.Maybe [Core.Text])
nodegroup_instanceTypes = Lens.lens (\Nodegroup' {instanceTypes} -> instanceTypes) (\s@Nodegroup' {} a -> s {instanceTypes = a} :: Nodegroup) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON Nodegroup where
  parseJSON =
    Core.withObject
      "Nodegroup"
      ( \x ->
          Nodegroup'
            Core.<$> (x Core..:? "scalingConfig")
            Core.<*> (x Core..:? "modifiedAt")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "capacityType")
            Core.<*> (x Core..:? "releaseVersion")
            Core.<*> (x Core..:? "diskSize")
            Core.<*> (x Core..:? "nodegroupName")
            Core.<*> (x Core..:? "remoteAccess")
            Core.<*> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "launchTemplate")
            Core.<*> (x Core..:? "labels" Core..!= Core.mempty)
            Core.<*> (x Core..:? "version")
            Core.<*> (x Core..:? "nodeRole")
            Core.<*> (x Core..:? "health")
            Core.<*> (x Core..:? "resources")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "subnets" Core..!= Core.mempty)
            Core.<*> (x Core..:? "amiType")
            Core.<*> (x Core..:? "clusterName")
            Core.<*> (x Core..:? "nodegroupArn")
            Core.<*> (x Core..:? "instanceTypes" Core..!= Core.mempty)
      )

instance Core.Hashable Nodegroup

instance Core.NFData Nodegroup
