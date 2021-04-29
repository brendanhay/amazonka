{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EMR.Types.Cluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.Cluster where

import Network.AWS.EMR.Types.Application
import Network.AWS.EMR.Types.ClusterStatus
import Network.AWS.EMR.Types.Configuration
import Network.AWS.EMR.Types.Ec2InstanceAttributes
import Network.AWS.EMR.Types.InstanceCollectionType
import Network.AWS.EMR.Types.KerberosAttributes
import Network.AWS.EMR.Types.PlacementGroupConfig
import Network.AWS.EMR.Types.RepoUpgradeOnBoot
import Network.AWS.EMR.Types.ScaleDownBehavior
import Network.AWS.EMR.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The detailed description of the cluster.
--
-- /See:/ 'newCluster' smart constructor.
data Cluster = Cluster'
  { -- | The Amazon Resource Name of the cluster.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | Applies only when @CustomAmiID@ is used. Specifies the type of updates
    -- that are applied from the Amazon Linux AMI package repositories when an
    -- instance boots using the AMI.
    repoUpgradeOnBoot :: Prelude.Maybe RepoUpgradeOnBoot,
    -- | The IAM role that will be assumed by the Amazon EMR service to access
    -- AWS resources on your behalf.
    serviceRole :: Prelude.Maybe Prelude.Text,
    -- | The name of the security configuration applied to the cluster.
    securityConfiguration :: Prelude.Maybe Prelude.Text,
    -- | The way that individual Amazon EC2 instances terminate when an automatic
    -- scale-in activity occurs or an instance group is resized.
    -- @TERMINATE_AT_INSTANCE_HOUR@ indicates that Amazon EMR terminates nodes
    -- at the instance-hour boundary, regardless of when the request to
    -- terminate the instance was submitted. This option is only available with
    -- Amazon EMR 5.1.0 and later and is the default for clusters created using
    -- that version. @TERMINATE_AT_TASK_COMPLETION@ indicates that Amazon EMR
    -- adds nodes to a deny list and drains tasks from nodes before terminating
    -- the Amazon EC2 instances, regardless of the instance-hour boundary. With
    -- either behavior, Amazon EMR removes the least active nodes first and
    -- blocks instance termination if it could lead to HDFS corruption.
    -- @TERMINATE_AT_TASK_COMPLETION@ is available only in Amazon EMR version
    -- 4.1.0 and later, and is the default for versions of Amazon EMR earlier
    -- than 5.1.0.
    scaleDownBehavior :: Prelude.Maybe ScaleDownBehavior,
    -- | An IAM role for automatic scaling policies. The default role is
    -- @EMR_AutoScaling_DefaultRole@. The IAM role provides permissions that
    -- the automatic scaling feature requires to launch and terminate EC2
    -- instances in an instance group.
    autoScalingRole :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether Amazon EMR will lock the cluster to prevent the EC2
    -- instances from being terminated by an API call or user intervention, or
    -- in the event of a cluster error.
    terminationProtected :: Prelude.Maybe Prelude.Bool,
    -- | Applies only to Amazon EMR releases 4.x and later. The list of
    -- Configurations supplied to the EMR cluster.
    configurations :: Prelude.Maybe [Configuration],
    -- | The Amazon Resource Name (ARN) of the Outpost where the cluster is
    -- launched.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | The DNS name of the master node. If the cluster is on a private subnet,
    -- this is the private DNS name. On a public subnet, this is the public DNS
    -- name.
    masterPublicDnsName :: Prelude.Maybe Prelude.Text,
    -- | The AMI version running on this cluster.
    runningAmiVersion :: Prelude.Maybe Prelude.Text,
    -- | The AMI version requested for this cluster.
    requestedAmiVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon EMR release label, which determines the version of
    -- open-source application packages installed on the cluster. Release
    -- labels are in the form @emr-x.x.x@, where x.x.x is an Amazon EMR release
    -- version such as @emr-5.14.0@. For more information about Amazon EMR
    -- release versions and included application versions and features, see
    -- <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/>. The release
    -- label applies only to Amazon EMR releases version 4.0 and later. Earlier
    -- versions use @AmiVersion@.
    releaseLabel :: Prelude.Maybe Prelude.Text,
    -- | The size, in GiB, of the Amazon EBS root device volume of the Linux AMI
    -- that is used for each EC2 instance. Available in Amazon EMR version 4.x
    -- and later.
    ebsRootVolumeSize :: Prelude.Maybe Prelude.Int,
    -- | The instance fleet configuration is available only in Amazon EMR
    -- versions 4.8.0 and later, excluding 5.0.x versions.
    --
    -- The instance group configuration of the cluster. A value of
    -- @INSTANCE_GROUP@ indicates a uniform instance group configuration. A
    -- value of @INSTANCE_FLEET@ indicates an instance fleets configuration.
    instanceCollectionType :: Prelude.Maybe InstanceCollectionType,
    -- | The AWS KMS customer master key (CMK) used for encrypting log files.
    -- This attribute is only available with EMR version 5.30.0 and later,
    -- excluding EMR 6.0.0.
    logEncryptionKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | A list of tags associated with a cluster.
    tags :: Prelude.Maybe [Tag],
    -- | The applications installed on this cluster.
    applications :: Prelude.Maybe [Application],
    -- | Specifies the number of steps that can be executed concurrently.
    stepConcurrencyLevel :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether the cluster is visible to all IAM users of the AWS
    -- account associated with the cluster. The default value, @true@,
    -- indicates that all IAM users in the AWS account can perform cluster
    -- actions if they have the proper IAM policy permissions. If this value is
    -- @false@, only the IAM user that created the cluster can perform actions.
    -- This value can be changed on a running cluster by using the
    -- SetVisibleToAllUsers action. You can override the default value of
    -- @true@ when you create a cluster by using the @VisibleToAllUsers@
    -- parameter of the @RunJobFlow@ action.
    visibleToAllUsers :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the cluster should terminate after completing all
    -- steps.
    autoTerminate :: Prelude.Maybe Prelude.Bool,
    -- | An approximation of the cost of the cluster, represented in
    -- m1.small\/hours. This value is incremented one time for every hour an
    -- m1.small instance runs. Larger instances are weighted more, so an EC2
    -- instance that is roughly four times more expensive would result in the
    -- normalized instance hours being incremented by four. This result is only
    -- an approximation and does not reflect the actual billing rate.
    normalizedInstanceHours :: Prelude.Maybe Prelude.Int,
    -- | Available only in Amazon EMR version 5.7.0 and later. The ID of a custom
    -- Amazon EBS-backed Linux AMI if the cluster uses a custom AMI.
    customAmiId :: Prelude.Maybe Prelude.Text,
    -- | Placement group configured for an Amazon EMR cluster.
    placementGroups :: Prelude.Maybe [PlacementGroupConfig],
    -- | Provides information about the EC2 instances in a cluster grouped by
    -- category. For example, key name, subnet ID, IAM instance profile, and so
    -- on.
    ec2InstanceAttributes :: Prelude.Maybe Ec2InstanceAttributes,
    -- | Attributes for Kerberos configuration when Kerberos authentication is
    -- enabled using a security configuration. For more information see
    -- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-kerberos.html Use Kerberos Authentication>
    -- in the /Amazon EMR Management Guide/.
    kerberosAttributes :: Prelude.Maybe KerberosAttributes,
    -- | The path to the Amazon S3 location where logs for this cluster are
    -- stored.
    logUri :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the cluster.
    id :: Prelude.Text,
    -- | The name of the cluster.
    name :: Prelude.Text,
    -- | The current status details about the cluster.
    status :: ClusterStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Cluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'cluster_clusterArn' - The Amazon Resource Name of the cluster.
--
-- 'repoUpgradeOnBoot', 'cluster_repoUpgradeOnBoot' - Applies only when @CustomAmiID@ is used. Specifies the type of updates
-- that are applied from the Amazon Linux AMI package repositories when an
-- instance boots using the AMI.
--
-- 'serviceRole', 'cluster_serviceRole' - The IAM role that will be assumed by the Amazon EMR service to access
-- AWS resources on your behalf.
--
-- 'securityConfiguration', 'cluster_securityConfiguration' - The name of the security configuration applied to the cluster.
--
-- 'scaleDownBehavior', 'cluster_scaleDownBehavior' - The way that individual Amazon EC2 instances terminate when an automatic
-- scale-in activity occurs or an instance group is resized.
-- @TERMINATE_AT_INSTANCE_HOUR@ indicates that Amazon EMR terminates nodes
-- at the instance-hour boundary, regardless of when the request to
-- terminate the instance was submitted. This option is only available with
-- Amazon EMR 5.1.0 and later and is the default for clusters created using
-- that version. @TERMINATE_AT_TASK_COMPLETION@ indicates that Amazon EMR
-- adds nodes to a deny list and drains tasks from nodes before terminating
-- the Amazon EC2 instances, regardless of the instance-hour boundary. With
-- either behavior, Amazon EMR removes the least active nodes first and
-- blocks instance termination if it could lead to HDFS corruption.
-- @TERMINATE_AT_TASK_COMPLETION@ is available only in Amazon EMR version
-- 4.1.0 and later, and is the default for versions of Amazon EMR earlier
-- than 5.1.0.
--
-- 'autoScalingRole', 'cluster_autoScalingRole' - An IAM role for automatic scaling policies. The default role is
-- @EMR_AutoScaling_DefaultRole@. The IAM role provides permissions that
-- the automatic scaling feature requires to launch and terminate EC2
-- instances in an instance group.
--
-- 'terminationProtected', 'cluster_terminationProtected' - Indicates whether Amazon EMR will lock the cluster to prevent the EC2
-- instances from being terminated by an API call or user intervention, or
-- in the event of a cluster error.
--
-- 'configurations', 'cluster_configurations' - Applies only to Amazon EMR releases 4.x and later. The list of
-- Configurations supplied to the EMR cluster.
--
-- 'outpostArn', 'cluster_outpostArn' - The Amazon Resource Name (ARN) of the Outpost where the cluster is
-- launched.
--
-- 'masterPublicDnsName', 'cluster_masterPublicDnsName' - The DNS name of the master node. If the cluster is on a private subnet,
-- this is the private DNS name. On a public subnet, this is the public DNS
-- name.
--
-- 'runningAmiVersion', 'cluster_runningAmiVersion' - The AMI version running on this cluster.
--
-- 'requestedAmiVersion', 'cluster_requestedAmiVersion' - The AMI version requested for this cluster.
--
-- 'releaseLabel', 'cluster_releaseLabel' - The Amazon EMR release label, which determines the version of
-- open-source application packages installed on the cluster. Release
-- labels are in the form @emr-x.x.x@, where x.x.x is an Amazon EMR release
-- version such as @emr-5.14.0@. For more information about Amazon EMR
-- release versions and included application versions and features, see
-- <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/>. The release
-- label applies only to Amazon EMR releases version 4.0 and later. Earlier
-- versions use @AmiVersion@.
--
-- 'ebsRootVolumeSize', 'cluster_ebsRootVolumeSize' - The size, in GiB, of the Amazon EBS root device volume of the Linux AMI
-- that is used for each EC2 instance. Available in Amazon EMR version 4.x
-- and later.
--
-- 'instanceCollectionType', 'cluster_instanceCollectionType' - The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions.
--
-- The instance group configuration of the cluster. A value of
-- @INSTANCE_GROUP@ indicates a uniform instance group configuration. A
-- value of @INSTANCE_FLEET@ indicates an instance fleets configuration.
--
-- 'logEncryptionKmsKeyId', 'cluster_logEncryptionKmsKeyId' - The AWS KMS customer master key (CMK) used for encrypting log files.
-- This attribute is only available with EMR version 5.30.0 and later,
-- excluding EMR 6.0.0.
--
-- 'tags', 'cluster_tags' - A list of tags associated with a cluster.
--
-- 'applications', 'cluster_applications' - The applications installed on this cluster.
--
-- 'stepConcurrencyLevel', 'cluster_stepConcurrencyLevel' - Specifies the number of steps that can be executed concurrently.
--
-- 'visibleToAllUsers', 'cluster_visibleToAllUsers' - Indicates whether the cluster is visible to all IAM users of the AWS
-- account associated with the cluster. The default value, @true@,
-- indicates that all IAM users in the AWS account can perform cluster
-- actions if they have the proper IAM policy permissions. If this value is
-- @false@, only the IAM user that created the cluster can perform actions.
-- This value can be changed on a running cluster by using the
-- SetVisibleToAllUsers action. You can override the default value of
-- @true@ when you create a cluster by using the @VisibleToAllUsers@
-- parameter of the @RunJobFlow@ action.
--
-- 'autoTerminate', 'cluster_autoTerminate' - Specifies whether the cluster should terminate after completing all
-- steps.
--
-- 'normalizedInstanceHours', 'cluster_normalizedInstanceHours' - An approximation of the cost of the cluster, represented in
-- m1.small\/hours. This value is incremented one time for every hour an
-- m1.small instance runs. Larger instances are weighted more, so an EC2
-- instance that is roughly four times more expensive would result in the
-- normalized instance hours being incremented by four. This result is only
-- an approximation and does not reflect the actual billing rate.
--
-- 'customAmiId', 'cluster_customAmiId' - Available only in Amazon EMR version 5.7.0 and later. The ID of a custom
-- Amazon EBS-backed Linux AMI if the cluster uses a custom AMI.
--
-- 'placementGroups', 'cluster_placementGroups' - Placement group configured for an Amazon EMR cluster.
--
-- 'ec2InstanceAttributes', 'cluster_ec2InstanceAttributes' - Provides information about the EC2 instances in a cluster grouped by
-- category. For example, key name, subnet ID, IAM instance profile, and so
-- on.
--
-- 'kerberosAttributes', 'cluster_kerberosAttributes' - Attributes for Kerberos configuration when Kerberos authentication is
-- enabled using a security configuration. For more information see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-kerberos.html Use Kerberos Authentication>
-- in the /Amazon EMR Management Guide/.
--
-- 'logUri', 'cluster_logUri' - The path to the Amazon S3 location where logs for this cluster are
-- stored.
--
-- 'id', 'cluster_id' - The unique identifier for the cluster.
--
-- 'name', 'cluster_name' - The name of the cluster.
--
-- 'status', 'cluster_status' - The current status details about the cluster.
newCluster ::
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'status'
  ClusterStatus ->
  Cluster
newCluster pId_ pName_ pStatus_ =
  Cluster'
    { clusterArn = Prelude.Nothing,
      repoUpgradeOnBoot = Prelude.Nothing,
      serviceRole = Prelude.Nothing,
      securityConfiguration = Prelude.Nothing,
      scaleDownBehavior = Prelude.Nothing,
      autoScalingRole = Prelude.Nothing,
      terminationProtected = Prelude.Nothing,
      configurations = Prelude.Nothing,
      outpostArn = Prelude.Nothing,
      masterPublicDnsName = Prelude.Nothing,
      runningAmiVersion = Prelude.Nothing,
      requestedAmiVersion = Prelude.Nothing,
      releaseLabel = Prelude.Nothing,
      ebsRootVolumeSize = Prelude.Nothing,
      instanceCollectionType = Prelude.Nothing,
      logEncryptionKmsKeyId = Prelude.Nothing,
      tags = Prelude.Nothing,
      applications = Prelude.Nothing,
      stepConcurrencyLevel = Prelude.Nothing,
      visibleToAllUsers = Prelude.Nothing,
      autoTerminate = Prelude.Nothing,
      normalizedInstanceHours = Prelude.Nothing,
      customAmiId = Prelude.Nothing,
      placementGroups = Prelude.Nothing,
      ec2InstanceAttributes = Prelude.Nothing,
      kerberosAttributes = Prelude.Nothing,
      logUri = Prelude.Nothing,
      id = pId_,
      name = pName_,
      status = pStatus_
    }

-- | The Amazon Resource Name of the cluster.
cluster_clusterArn :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_clusterArn = Lens.lens (\Cluster' {clusterArn} -> clusterArn) (\s@Cluster' {} a -> s {clusterArn = a} :: Cluster)

-- | Applies only when @CustomAmiID@ is used. Specifies the type of updates
-- that are applied from the Amazon Linux AMI package repositories when an
-- instance boots using the AMI.
cluster_repoUpgradeOnBoot :: Lens.Lens' Cluster (Prelude.Maybe RepoUpgradeOnBoot)
cluster_repoUpgradeOnBoot = Lens.lens (\Cluster' {repoUpgradeOnBoot} -> repoUpgradeOnBoot) (\s@Cluster' {} a -> s {repoUpgradeOnBoot = a} :: Cluster)

-- | The IAM role that will be assumed by the Amazon EMR service to access
-- AWS resources on your behalf.
cluster_serviceRole :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_serviceRole = Lens.lens (\Cluster' {serviceRole} -> serviceRole) (\s@Cluster' {} a -> s {serviceRole = a} :: Cluster)

-- | The name of the security configuration applied to the cluster.
cluster_securityConfiguration :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_securityConfiguration = Lens.lens (\Cluster' {securityConfiguration} -> securityConfiguration) (\s@Cluster' {} a -> s {securityConfiguration = a} :: Cluster)

-- | The way that individual Amazon EC2 instances terminate when an automatic
-- scale-in activity occurs or an instance group is resized.
-- @TERMINATE_AT_INSTANCE_HOUR@ indicates that Amazon EMR terminates nodes
-- at the instance-hour boundary, regardless of when the request to
-- terminate the instance was submitted. This option is only available with
-- Amazon EMR 5.1.0 and later and is the default for clusters created using
-- that version. @TERMINATE_AT_TASK_COMPLETION@ indicates that Amazon EMR
-- adds nodes to a deny list and drains tasks from nodes before terminating
-- the Amazon EC2 instances, regardless of the instance-hour boundary. With
-- either behavior, Amazon EMR removes the least active nodes first and
-- blocks instance termination if it could lead to HDFS corruption.
-- @TERMINATE_AT_TASK_COMPLETION@ is available only in Amazon EMR version
-- 4.1.0 and later, and is the default for versions of Amazon EMR earlier
-- than 5.1.0.
cluster_scaleDownBehavior :: Lens.Lens' Cluster (Prelude.Maybe ScaleDownBehavior)
cluster_scaleDownBehavior = Lens.lens (\Cluster' {scaleDownBehavior} -> scaleDownBehavior) (\s@Cluster' {} a -> s {scaleDownBehavior = a} :: Cluster)

-- | An IAM role for automatic scaling policies. The default role is
-- @EMR_AutoScaling_DefaultRole@. The IAM role provides permissions that
-- the automatic scaling feature requires to launch and terminate EC2
-- instances in an instance group.
cluster_autoScalingRole :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_autoScalingRole = Lens.lens (\Cluster' {autoScalingRole} -> autoScalingRole) (\s@Cluster' {} a -> s {autoScalingRole = a} :: Cluster)

-- | Indicates whether Amazon EMR will lock the cluster to prevent the EC2
-- instances from being terminated by an API call or user intervention, or
-- in the event of a cluster error.
cluster_terminationProtected :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Bool)
cluster_terminationProtected = Lens.lens (\Cluster' {terminationProtected} -> terminationProtected) (\s@Cluster' {} a -> s {terminationProtected = a} :: Cluster)

-- | Applies only to Amazon EMR releases 4.x and later. The list of
-- Configurations supplied to the EMR cluster.
cluster_configurations :: Lens.Lens' Cluster (Prelude.Maybe [Configuration])
cluster_configurations = Lens.lens (\Cluster' {configurations} -> configurations) (\s@Cluster' {} a -> s {configurations = a} :: Cluster) Prelude.. Lens.mapping Prelude._Coerce

-- | The Amazon Resource Name (ARN) of the Outpost where the cluster is
-- launched.
cluster_outpostArn :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_outpostArn = Lens.lens (\Cluster' {outpostArn} -> outpostArn) (\s@Cluster' {} a -> s {outpostArn = a} :: Cluster)

-- | The DNS name of the master node. If the cluster is on a private subnet,
-- this is the private DNS name. On a public subnet, this is the public DNS
-- name.
cluster_masterPublicDnsName :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_masterPublicDnsName = Lens.lens (\Cluster' {masterPublicDnsName} -> masterPublicDnsName) (\s@Cluster' {} a -> s {masterPublicDnsName = a} :: Cluster)

-- | The AMI version running on this cluster.
cluster_runningAmiVersion :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_runningAmiVersion = Lens.lens (\Cluster' {runningAmiVersion} -> runningAmiVersion) (\s@Cluster' {} a -> s {runningAmiVersion = a} :: Cluster)

-- | The AMI version requested for this cluster.
cluster_requestedAmiVersion :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_requestedAmiVersion = Lens.lens (\Cluster' {requestedAmiVersion} -> requestedAmiVersion) (\s@Cluster' {} a -> s {requestedAmiVersion = a} :: Cluster)

-- | The Amazon EMR release label, which determines the version of
-- open-source application packages installed on the cluster. Release
-- labels are in the form @emr-x.x.x@, where x.x.x is an Amazon EMR release
-- version such as @emr-5.14.0@. For more information about Amazon EMR
-- release versions and included application versions and features, see
-- <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/>. The release
-- label applies only to Amazon EMR releases version 4.0 and later. Earlier
-- versions use @AmiVersion@.
cluster_releaseLabel :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_releaseLabel = Lens.lens (\Cluster' {releaseLabel} -> releaseLabel) (\s@Cluster' {} a -> s {releaseLabel = a} :: Cluster)

-- | The size, in GiB, of the Amazon EBS root device volume of the Linux AMI
-- that is used for each EC2 instance. Available in Amazon EMR version 4.x
-- and later.
cluster_ebsRootVolumeSize :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Int)
cluster_ebsRootVolumeSize = Lens.lens (\Cluster' {ebsRootVolumeSize} -> ebsRootVolumeSize) (\s@Cluster' {} a -> s {ebsRootVolumeSize = a} :: Cluster)

-- | The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions.
--
-- The instance group configuration of the cluster. A value of
-- @INSTANCE_GROUP@ indicates a uniform instance group configuration. A
-- value of @INSTANCE_FLEET@ indicates an instance fleets configuration.
cluster_instanceCollectionType :: Lens.Lens' Cluster (Prelude.Maybe InstanceCollectionType)
cluster_instanceCollectionType = Lens.lens (\Cluster' {instanceCollectionType} -> instanceCollectionType) (\s@Cluster' {} a -> s {instanceCollectionType = a} :: Cluster)

-- | The AWS KMS customer master key (CMK) used for encrypting log files.
-- This attribute is only available with EMR version 5.30.0 and later,
-- excluding EMR 6.0.0.
cluster_logEncryptionKmsKeyId :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_logEncryptionKmsKeyId = Lens.lens (\Cluster' {logEncryptionKmsKeyId} -> logEncryptionKmsKeyId) (\s@Cluster' {} a -> s {logEncryptionKmsKeyId = a} :: Cluster)

-- | A list of tags associated with a cluster.
cluster_tags :: Lens.Lens' Cluster (Prelude.Maybe [Tag])
cluster_tags = Lens.lens (\Cluster' {tags} -> tags) (\s@Cluster' {} a -> s {tags = a} :: Cluster) Prelude.. Lens.mapping Prelude._Coerce

-- | The applications installed on this cluster.
cluster_applications :: Lens.Lens' Cluster (Prelude.Maybe [Application])
cluster_applications = Lens.lens (\Cluster' {applications} -> applications) (\s@Cluster' {} a -> s {applications = a} :: Cluster) Prelude.. Lens.mapping Prelude._Coerce

-- | Specifies the number of steps that can be executed concurrently.
cluster_stepConcurrencyLevel :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Int)
cluster_stepConcurrencyLevel = Lens.lens (\Cluster' {stepConcurrencyLevel} -> stepConcurrencyLevel) (\s@Cluster' {} a -> s {stepConcurrencyLevel = a} :: Cluster)

-- | Indicates whether the cluster is visible to all IAM users of the AWS
-- account associated with the cluster. The default value, @true@,
-- indicates that all IAM users in the AWS account can perform cluster
-- actions if they have the proper IAM policy permissions. If this value is
-- @false@, only the IAM user that created the cluster can perform actions.
-- This value can be changed on a running cluster by using the
-- SetVisibleToAllUsers action. You can override the default value of
-- @true@ when you create a cluster by using the @VisibleToAllUsers@
-- parameter of the @RunJobFlow@ action.
cluster_visibleToAllUsers :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Bool)
cluster_visibleToAllUsers = Lens.lens (\Cluster' {visibleToAllUsers} -> visibleToAllUsers) (\s@Cluster' {} a -> s {visibleToAllUsers = a} :: Cluster)

-- | Specifies whether the cluster should terminate after completing all
-- steps.
cluster_autoTerminate :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Bool)
cluster_autoTerminate = Lens.lens (\Cluster' {autoTerminate} -> autoTerminate) (\s@Cluster' {} a -> s {autoTerminate = a} :: Cluster)

-- | An approximation of the cost of the cluster, represented in
-- m1.small\/hours. This value is incremented one time for every hour an
-- m1.small instance runs. Larger instances are weighted more, so an EC2
-- instance that is roughly four times more expensive would result in the
-- normalized instance hours being incremented by four. This result is only
-- an approximation and does not reflect the actual billing rate.
cluster_normalizedInstanceHours :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Int)
cluster_normalizedInstanceHours = Lens.lens (\Cluster' {normalizedInstanceHours} -> normalizedInstanceHours) (\s@Cluster' {} a -> s {normalizedInstanceHours = a} :: Cluster)

-- | Available only in Amazon EMR version 5.7.0 and later. The ID of a custom
-- Amazon EBS-backed Linux AMI if the cluster uses a custom AMI.
cluster_customAmiId :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_customAmiId = Lens.lens (\Cluster' {customAmiId} -> customAmiId) (\s@Cluster' {} a -> s {customAmiId = a} :: Cluster)

-- | Placement group configured for an Amazon EMR cluster.
cluster_placementGroups :: Lens.Lens' Cluster (Prelude.Maybe [PlacementGroupConfig])
cluster_placementGroups = Lens.lens (\Cluster' {placementGroups} -> placementGroups) (\s@Cluster' {} a -> s {placementGroups = a} :: Cluster) Prelude.. Lens.mapping Prelude._Coerce

-- | Provides information about the EC2 instances in a cluster grouped by
-- category. For example, key name, subnet ID, IAM instance profile, and so
-- on.
cluster_ec2InstanceAttributes :: Lens.Lens' Cluster (Prelude.Maybe Ec2InstanceAttributes)
cluster_ec2InstanceAttributes = Lens.lens (\Cluster' {ec2InstanceAttributes} -> ec2InstanceAttributes) (\s@Cluster' {} a -> s {ec2InstanceAttributes = a} :: Cluster)

-- | Attributes for Kerberos configuration when Kerberos authentication is
-- enabled using a security configuration. For more information see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-kerberos.html Use Kerberos Authentication>
-- in the /Amazon EMR Management Guide/.
cluster_kerberosAttributes :: Lens.Lens' Cluster (Prelude.Maybe KerberosAttributes)
cluster_kerberosAttributes = Lens.lens (\Cluster' {kerberosAttributes} -> kerberosAttributes) (\s@Cluster' {} a -> s {kerberosAttributes = a} :: Cluster)

-- | The path to the Amazon S3 location where logs for this cluster are
-- stored.
cluster_logUri :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_logUri = Lens.lens (\Cluster' {logUri} -> logUri) (\s@Cluster' {} a -> s {logUri = a} :: Cluster)

-- | The unique identifier for the cluster.
cluster_id :: Lens.Lens' Cluster Prelude.Text
cluster_id = Lens.lens (\Cluster' {id} -> id) (\s@Cluster' {} a -> s {id = a} :: Cluster)

-- | The name of the cluster.
cluster_name :: Lens.Lens' Cluster Prelude.Text
cluster_name = Lens.lens (\Cluster' {name} -> name) (\s@Cluster' {} a -> s {name = a} :: Cluster)

-- | The current status details about the cluster.
cluster_status :: Lens.Lens' Cluster ClusterStatus
cluster_status = Lens.lens (\Cluster' {status} -> status) (\s@Cluster' {} a -> s {status = a} :: Cluster)

instance Prelude.FromJSON Cluster where
  parseJSON =
    Prelude.withObject
      "Cluster"
      ( \x ->
          Cluster'
            Prelude.<$> (x Prelude..:? "ClusterArn")
            Prelude.<*> (x Prelude..:? "RepoUpgradeOnBoot")
            Prelude.<*> (x Prelude..:? "ServiceRole")
            Prelude.<*> (x Prelude..:? "SecurityConfiguration")
            Prelude.<*> (x Prelude..:? "ScaleDownBehavior")
            Prelude.<*> (x Prelude..:? "AutoScalingRole")
            Prelude.<*> (x Prelude..:? "TerminationProtected")
            Prelude.<*> ( x Prelude..:? "Configurations"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "OutpostArn")
            Prelude.<*> (x Prelude..:? "MasterPublicDnsName")
            Prelude.<*> (x Prelude..:? "RunningAmiVersion")
            Prelude.<*> (x Prelude..:? "RequestedAmiVersion")
            Prelude.<*> (x Prelude..:? "ReleaseLabel")
            Prelude.<*> (x Prelude..:? "EbsRootVolumeSize")
            Prelude.<*> (x Prelude..:? "InstanceCollectionType")
            Prelude.<*> (x Prelude..:? "LogEncryptionKmsKeyId")
            Prelude.<*> (x Prelude..:? "Tags" Prelude..!= Prelude.mempty)
            Prelude.<*> ( x Prelude..:? "Applications"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "StepConcurrencyLevel")
            Prelude.<*> (x Prelude..:? "VisibleToAllUsers")
            Prelude.<*> (x Prelude..:? "AutoTerminate")
            Prelude.<*> (x Prelude..:? "NormalizedInstanceHours")
            Prelude.<*> (x Prelude..:? "CustomAmiId")
            Prelude.<*> ( x Prelude..:? "PlacementGroups"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "Ec2InstanceAttributes")
            Prelude.<*> (x Prelude..:? "KerberosAttributes")
            Prelude.<*> (x Prelude..:? "LogUri")
            Prelude.<*> (x Prelude..: "Id")
            Prelude.<*> (x Prelude..: "Name")
            Prelude.<*> (x Prelude..: "Status")
      )

instance Prelude.Hashable Cluster

instance Prelude.NFData Cluster
