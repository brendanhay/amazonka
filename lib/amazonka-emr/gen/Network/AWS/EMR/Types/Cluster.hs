{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Cluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.Cluster
  ( Cluster (..),

    -- * Smart constructor
    mkCluster,

    -- * Lenses
    cfApplications,
    cfAutoScalingRole,
    cfAutoTerminate,
    cfClusterArn,
    cfConfigurations,
    cfCustomAmiId,
    cfEbsRootVolumeSize,
    cfEc2InstanceAttributes,
    cfId,
    cfInstanceCollectionType,
    cfKerberosAttributes,
    cfLogEncryptionKmsKeyId,
    cfLogUri,
    cfMasterPublicDnsName,
    cfName,
    cfNormalizedInstanceHours,
    cfOutpostArn,
    cfPlacementGroups,
    cfReleaseLabel,
    cfRepoUpgradeOnBoot,
    cfRequestedAmiVersion,
    cfRunningAmiVersion,
    cfScaleDownBehavior,
    cfSecurityConfiguration,
    cfServiceRole,
    cfStatus,
    cfStepConcurrencyLevel,
    cfTags,
    cfTerminationProtected,
    cfVisibleToAllUsers,
  )
where

import qualified Network.AWS.EMR.Types.Application as Types
import qualified Network.AWS.EMR.Types.ClusterArn as Types
import qualified Network.AWS.EMR.Types.ClusterStatus as Types
import qualified Network.AWS.EMR.Types.Configuration as Types
import qualified Network.AWS.EMR.Types.CustomAmiId as Types
import qualified Network.AWS.EMR.Types.Ec2InstanceAttributes as Types
import qualified Network.AWS.EMR.Types.Id as Types
import qualified Network.AWS.EMR.Types.InstanceCollectionType as Types
import qualified Network.AWS.EMR.Types.KerberosAttributes as Types
import qualified Network.AWS.EMR.Types.LogEncryptionKmsKeyId as Types
import qualified Network.AWS.EMR.Types.LogUri as Types
import qualified Network.AWS.EMR.Types.MasterPublicDnsName as Types
import qualified Network.AWS.EMR.Types.Name as Types
import qualified Network.AWS.EMR.Types.OutpostArn as Types
import qualified Network.AWS.EMR.Types.PlacementGroupConfig as Types
import qualified Network.AWS.EMR.Types.ReleaseLabel as Types
import qualified Network.AWS.EMR.Types.RepoUpgradeOnBoot as Types
import qualified Network.AWS.EMR.Types.RequestedAmiVersion as Types
import qualified Network.AWS.EMR.Types.RunningAmiVersion as Types
import qualified Network.AWS.EMR.Types.ScaleDownBehavior as Types
import qualified Network.AWS.EMR.Types.ServiceRole as Types
import qualified Network.AWS.EMR.Types.Tag as Types
import qualified Network.AWS.EMR.Types.XmlString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The detailed description of the cluster.
--
-- /See:/ 'mkCluster' smart constructor.
data Cluster = Cluster'
  { -- | The applications installed on this cluster.
    applications :: Core.Maybe [Types.Application],
    -- | An IAM role for automatic scaling policies. The default role is @EMR_AutoScaling_DefaultRole@ . The IAM role provides permissions that the automatic scaling feature requires to launch and terminate EC2 instances in an instance group.
    autoScalingRole :: Core.Maybe Types.XmlString,
    -- | Specifies whether the cluster should terminate after completing all steps.
    autoTerminate :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name of the cluster.
    clusterArn :: Core.Maybe Types.ClusterArn,
    -- | Applies only to Amazon EMR releases 4.x and later. The list of Configurations supplied to the EMR cluster.
    configurations :: Core.Maybe [Types.Configuration],
    -- | Available only in Amazon EMR version 5.7.0 and later. The ID of a custom Amazon EBS-backed Linux AMI if the cluster uses a custom AMI.
    customAmiId :: Core.Maybe Types.CustomAmiId,
    -- | The size, in GiB, of the Amazon EBS root device volume of the Linux AMI that is used for each EC2 instance. Available in Amazon EMR version 4.x and later.
    ebsRootVolumeSize :: Core.Maybe Core.Int,
    -- | Provides information about the EC2 instances in a cluster grouped by category. For example, key name, subnet ID, IAM instance profile, and so on.
    ec2InstanceAttributes :: Core.Maybe Types.Ec2InstanceAttributes,
    -- | The unique identifier for the cluster.
    id :: Types.Id,
    -- | The instance group configuration of the cluster. A value of @INSTANCE_GROUP@ indicates a uniform instance group configuration. A value of @INSTANCE_FLEET@ indicates an instance fleets configuration.
    instanceCollectionType :: Core.Maybe Types.InstanceCollectionType,
    -- | Attributes for Kerberos configuration when Kerberos authentication is enabled using a security configuration. For more information see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-kerberos.html Use Kerberos Authentication> in the /Amazon EMR Management Guide/ .
    kerberosAttributes :: Core.Maybe Types.KerberosAttributes,
    -- | The AWS KMS customer master key (CMK) used for encrypting log files. This attribute is only available with EMR version 5.30.0 and later, excluding EMR 6.0.0.
    logEncryptionKmsKeyId :: Core.Maybe Types.LogEncryptionKmsKeyId,
    -- | The path to the Amazon S3 location where logs for this cluster are stored.
    logUri :: Core.Maybe Types.LogUri,
    -- | The DNS name of the master node. If the cluster is on a private subnet, this is the private DNS name. On a public subnet, this is the public DNS name.
    masterPublicDnsName :: Core.Maybe Types.MasterPublicDnsName,
    -- | The name of the cluster.
    name :: Types.Name,
    -- | An approximation of the cost of the cluster, represented in m1.small/hours. This value is incremented one time for every hour an m1.small instance runs. Larger instances are weighted more, so an EC2 instance that is roughly four times more expensive would result in the normalized instance hours being incremented by four. This result is only an approximation and does not reflect the actual billing rate.
    normalizedInstanceHours :: Core.Maybe Core.Int,
    -- | The Amazon Resource Name (ARN) of the Outpost where the cluster is launched.
    outpostArn :: Core.Maybe Types.OutpostArn,
    -- | Placement group configured for an Amazon EMR cluster.
    placementGroups :: Core.Maybe [Types.PlacementGroupConfig],
    -- | The Amazon EMR release label, which determines the version of open-source application packages installed on the cluster. Release labels are in the form @emr-x.x.x@ , where x.x.x is an Amazon EMR release version such as @emr-5.14.0@ . For more information about Amazon EMR release versions and included application versions and features, see <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/ https://docs.aws.amazon.com/emr/latest/ReleaseGuide/> . The release label applies only to Amazon EMR releases version 4.0 and later. Earlier versions use @AmiVersion@ .
    releaseLabel :: Core.Maybe Types.ReleaseLabel,
    -- | Applies only when @CustomAmiID@ is used. Specifies the type of updates that are applied from the Amazon Linux AMI package repositories when an instance boots using the AMI.
    repoUpgradeOnBoot :: Core.Maybe Types.RepoUpgradeOnBoot,
    -- | The AMI version requested for this cluster.
    requestedAmiVersion :: Core.Maybe Types.RequestedAmiVersion,
    -- | The AMI version running on this cluster.
    runningAmiVersion :: Core.Maybe Types.RunningAmiVersion,
    -- | The way that individual Amazon EC2 instances terminate when an automatic scale-in activity occurs or an instance group is resized. @TERMINATE_AT_INSTANCE_HOUR@ indicates that Amazon EMR terminates nodes at the instance-hour boundary, regardless of when the request to terminate the instance was submitted. This option is only available with Amazon EMR 5.1.0 and later and is the default for clusters created using that version. @TERMINATE_AT_TASK_COMPLETION@ indicates that Amazon EMR adds nodes to a deny list and drains tasks from nodes before terminating the Amazon EC2 instances, regardless of the instance-hour boundary. With either behavior, Amazon EMR removes the least active nodes first and blocks instance termination if it could lead to HDFS corruption. @TERMINATE_AT_TASK_COMPLETION@ is available only in Amazon EMR version 4.1.0 and later, and is the default for versions of Amazon EMR earlier than 5.1.0.
    scaleDownBehavior :: Core.Maybe Types.ScaleDownBehavior,
    -- | The name of the security configuration applied to the cluster.
    securityConfiguration :: Core.Maybe Types.XmlString,
    -- | The IAM role that will be assumed by the Amazon EMR service to access AWS resources on your behalf.
    serviceRole :: Core.Maybe Types.ServiceRole,
    -- | The current status details about the cluster.
    status :: Types.ClusterStatus,
    -- | Specifies the number of steps that can be executed concurrently.
    stepConcurrencyLevel :: Core.Maybe Core.Int,
    -- | A list of tags associated with a cluster.
    tags :: Core.Maybe [Types.Tag],
    -- | Indicates whether Amazon EMR will lock the cluster to prevent the EC2 instances from being terminated by an API call or user intervention, or in the event of a cluster error.
    terminationProtected :: Core.Maybe Core.Bool,
    -- | Indicates whether the cluster is visible to all IAM users of the AWS account associated with the cluster. The default value, @true@ , indicates that all IAM users in the AWS account can perform cluster actions if they have the proper IAM policy permissions. If this value is @false@ , only the IAM user that created the cluster can perform actions. This value can be changed on a running cluster by using the 'SetVisibleToAllUsers' action. You can override the default value of @true@ when you create a cluster by using the @VisibleToAllUsers@ parameter of the @RunJobFlow@ action.
    visibleToAllUsers :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Cluster' value with any optional fields omitted.
mkCluster ::
  -- | 'id'
  Types.Id ->
  -- | 'name'
  Types.Name ->
  -- | 'status'
  Types.ClusterStatus ->
  Cluster
mkCluster id name status =
  Cluster'
    { applications = Core.Nothing,
      autoScalingRole = Core.Nothing,
      autoTerminate = Core.Nothing,
      clusterArn = Core.Nothing,
      configurations = Core.Nothing,
      customAmiId = Core.Nothing,
      ebsRootVolumeSize = Core.Nothing,
      ec2InstanceAttributes = Core.Nothing,
      id,
      instanceCollectionType = Core.Nothing,
      kerberosAttributes = Core.Nothing,
      logEncryptionKmsKeyId = Core.Nothing,
      logUri = Core.Nothing,
      masterPublicDnsName = Core.Nothing,
      name,
      normalizedInstanceHours = Core.Nothing,
      outpostArn = Core.Nothing,
      placementGroups = Core.Nothing,
      releaseLabel = Core.Nothing,
      repoUpgradeOnBoot = Core.Nothing,
      requestedAmiVersion = Core.Nothing,
      runningAmiVersion = Core.Nothing,
      scaleDownBehavior = Core.Nothing,
      securityConfiguration = Core.Nothing,
      serviceRole = Core.Nothing,
      status,
      stepConcurrencyLevel = Core.Nothing,
      tags = Core.Nothing,
      terminationProtected = Core.Nothing,
      visibleToAllUsers = Core.Nothing
    }

-- | The applications installed on this cluster.
--
-- /Note:/ Consider using 'applications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfApplications :: Lens.Lens' Cluster (Core.Maybe [Types.Application])
cfApplications = Lens.field @"applications"
{-# DEPRECATED cfApplications "Use generic-lens or generic-optics with 'applications' instead." #-}

-- | An IAM role for automatic scaling policies. The default role is @EMR_AutoScaling_DefaultRole@ . The IAM role provides permissions that the automatic scaling feature requires to launch and terminate EC2 instances in an instance group.
--
-- /Note:/ Consider using 'autoScalingRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfAutoScalingRole :: Lens.Lens' Cluster (Core.Maybe Types.XmlString)
cfAutoScalingRole = Lens.field @"autoScalingRole"
{-# DEPRECATED cfAutoScalingRole "Use generic-lens or generic-optics with 'autoScalingRole' instead." #-}

-- | Specifies whether the cluster should terminate after completing all steps.
--
-- /Note:/ Consider using 'autoTerminate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfAutoTerminate :: Lens.Lens' Cluster (Core.Maybe Core.Bool)
cfAutoTerminate = Lens.field @"autoTerminate"
{-# DEPRECATED cfAutoTerminate "Use generic-lens or generic-optics with 'autoTerminate' instead." #-}

-- | The Amazon Resource Name of the cluster.
--
-- /Note:/ Consider using 'clusterArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfClusterArn :: Lens.Lens' Cluster (Core.Maybe Types.ClusterArn)
cfClusterArn = Lens.field @"clusterArn"
{-# DEPRECATED cfClusterArn "Use generic-lens or generic-optics with 'clusterArn' instead." #-}

-- | Applies only to Amazon EMR releases 4.x and later. The list of Configurations supplied to the EMR cluster.
--
-- /Note:/ Consider using 'configurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfConfigurations :: Lens.Lens' Cluster (Core.Maybe [Types.Configuration])
cfConfigurations = Lens.field @"configurations"
{-# DEPRECATED cfConfigurations "Use generic-lens or generic-optics with 'configurations' instead." #-}

-- | Available only in Amazon EMR version 5.7.0 and later. The ID of a custom Amazon EBS-backed Linux AMI if the cluster uses a custom AMI.
--
-- /Note:/ Consider using 'customAmiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfCustomAmiId :: Lens.Lens' Cluster (Core.Maybe Types.CustomAmiId)
cfCustomAmiId = Lens.field @"customAmiId"
{-# DEPRECATED cfCustomAmiId "Use generic-lens or generic-optics with 'customAmiId' instead." #-}

-- | The size, in GiB, of the Amazon EBS root device volume of the Linux AMI that is used for each EC2 instance. Available in Amazon EMR version 4.x and later.
--
-- /Note:/ Consider using 'ebsRootVolumeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfEbsRootVolumeSize :: Lens.Lens' Cluster (Core.Maybe Core.Int)
cfEbsRootVolumeSize = Lens.field @"ebsRootVolumeSize"
{-# DEPRECATED cfEbsRootVolumeSize "Use generic-lens or generic-optics with 'ebsRootVolumeSize' instead." #-}

-- | Provides information about the EC2 instances in a cluster grouped by category. For example, key name, subnet ID, IAM instance profile, and so on.
--
-- /Note:/ Consider using 'ec2InstanceAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfEc2InstanceAttributes :: Lens.Lens' Cluster (Core.Maybe Types.Ec2InstanceAttributes)
cfEc2InstanceAttributes = Lens.field @"ec2InstanceAttributes"
{-# DEPRECATED cfEc2InstanceAttributes "Use generic-lens or generic-optics with 'ec2InstanceAttributes' instead." #-}

-- | The unique identifier for the cluster.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfId :: Lens.Lens' Cluster Types.Id
cfId = Lens.field @"id"
{-# DEPRECATED cfId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The instance group configuration of the cluster. A value of @INSTANCE_GROUP@ indicates a uniform instance group configuration. A value of @INSTANCE_FLEET@ indicates an instance fleets configuration.
--
-- /Note:/ Consider using 'instanceCollectionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfInstanceCollectionType :: Lens.Lens' Cluster (Core.Maybe Types.InstanceCollectionType)
cfInstanceCollectionType = Lens.field @"instanceCollectionType"
{-# DEPRECATED cfInstanceCollectionType "Use generic-lens or generic-optics with 'instanceCollectionType' instead." #-}

-- | Attributes for Kerberos configuration when Kerberos authentication is enabled using a security configuration. For more information see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-kerberos.html Use Kerberos Authentication> in the /Amazon EMR Management Guide/ .
--
-- /Note:/ Consider using 'kerberosAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfKerberosAttributes :: Lens.Lens' Cluster (Core.Maybe Types.KerberosAttributes)
cfKerberosAttributes = Lens.field @"kerberosAttributes"
{-# DEPRECATED cfKerberosAttributes "Use generic-lens or generic-optics with 'kerberosAttributes' instead." #-}

-- | The AWS KMS customer master key (CMK) used for encrypting log files. This attribute is only available with EMR version 5.30.0 and later, excluding EMR 6.0.0.
--
-- /Note:/ Consider using 'logEncryptionKmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfLogEncryptionKmsKeyId :: Lens.Lens' Cluster (Core.Maybe Types.LogEncryptionKmsKeyId)
cfLogEncryptionKmsKeyId = Lens.field @"logEncryptionKmsKeyId"
{-# DEPRECATED cfLogEncryptionKmsKeyId "Use generic-lens or generic-optics with 'logEncryptionKmsKeyId' instead." #-}

-- | The path to the Amazon S3 location where logs for this cluster are stored.
--
-- /Note:/ Consider using 'logUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfLogUri :: Lens.Lens' Cluster (Core.Maybe Types.LogUri)
cfLogUri = Lens.field @"logUri"
{-# DEPRECATED cfLogUri "Use generic-lens or generic-optics with 'logUri' instead." #-}

-- | The DNS name of the master node. If the cluster is on a private subnet, this is the private DNS name. On a public subnet, this is the public DNS name.
--
-- /Note:/ Consider using 'masterPublicDnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfMasterPublicDnsName :: Lens.Lens' Cluster (Core.Maybe Types.MasterPublicDnsName)
cfMasterPublicDnsName = Lens.field @"masterPublicDnsName"
{-# DEPRECATED cfMasterPublicDnsName "Use generic-lens or generic-optics with 'masterPublicDnsName' instead." #-}

-- | The name of the cluster.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfName :: Lens.Lens' Cluster Types.Name
cfName = Lens.field @"name"
{-# DEPRECATED cfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | An approximation of the cost of the cluster, represented in m1.small/hours. This value is incremented one time for every hour an m1.small instance runs. Larger instances are weighted more, so an EC2 instance that is roughly four times more expensive would result in the normalized instance hours being incremented by four. This result is only an approximation and does not reflect the actual billing rate.
--
-- /Note:/ Consider using 'normalizedInstanceHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfNormalizedInstanceHours :: Lens.Lens' Cluster (Core.Maybe Core.Int)
cfNormalizedInstanceHours = Lens.field @"normalizedInstanceHours"
{-# DEPRECATED cfNormalizedInstanceHours "Use generic-lens or generic-optics with 'normalizedInstanceHours' instead." #-}

-- | The Amazon Resource Name (ARN) of the Outpost where the cluster is launched.
--
-- /Note:/ Consider using 'outpostArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfOutpostArn :: Lens.Lens' Cluster (Core.Maybe Types.OutpostArn)
cfOutpostArn = Lens.field @"outpostArn"
{-# DEPRECATED cfOutpostArn "Use generic-lens or generic-optics with 'outpostArn' instead." #-}

-- | Placement group configured for an Amazon EMR cluster.
--
-- /Note:/ Consider using 'placementGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfPlacementGroups :: Lens.Lens' Cluster (Core.Maybe [Types.PlacementGroupConfig])
cfPlacementGroups = Lens.field @"placementGroups"
{-# DEPRECATED cfPlacementGroups "Use generic-lens or generic-optics with 'placementGroups' instead." #-}

-- | The Amazon EMR release label, which determines the version of open-source application packages installed on the cluster. Release labels are in the form @emr-x.x.x@ , where x.x.x is an Amazon EMR release version such as @emr-5.14.0@ . For more information about Amazon EMR release versions and included application versions and features, see <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/ https://docs.aws.amazon.com/emr/latest/ReleaseGuide/> . The release label applies only to Amazon EMR releases version 4.0 and later. Earlier versions use @AmiVersion@ .
--
-- /Note:/ Consider using 'releaseLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfReleaseLabel :: Lens.Lens' Cluster (Core.Maybe Types.ReleaseLabel)
cfReleaseLabel = Lens.field @"releaseLabel"
{-# DEPRECATED cfReleaseLabel "Use generic-lens or generic-optics with 'releaseLabel' instead." #-}

-- | Applies only when @CustomAmiID@ is used. Specifies the type of updates that are applied from the Amazon Linux AMI package repositories when an instance boots using the AMI.
--
-- /Note:/ Consider using 'repoUpgradeOnBoot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfRepoUpgradeOnBoot :: Lens.Lens' Cluster (Core.Maybe Types.RepoUpgradeOnBoot)
cfRepoUpgradeOnBoot = Lens.field @"repoUpgradeOnBoot"
{-# DEPRECATED cfRepoUpgradeOnBoot "Use generic-lens or generic-optics with 'repoUpgradeOnBoot' instead." #-}

-- | The AMI version requested for this cluster.
--
-- /Note:/ Consider using 'requestedAmiVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfRequestedAmiVersion :: Lens.Lens' Cluster (Core.Maybe Types.RequestedAmiVersion)
cfRequestedAmiVersion = Lens.field @"requestedAmiVersion"
{-# DEPRECATED cfRequestedAmiVersion "Use generic-lens or generic-optics with 'requestedAmiVersion' instead." #-}

-- | The AMI version running on this cluster.
--
-- /Note:/ Consider using 'runningAmiVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfRunningAmiVersion :: Lens.Lens' Cluster (Core.Maybe Types.RunningAmiVersion)
cfRunningAmiVersion = Lens.field @"runningAmiVersion"
{-# DEPRECATED cfRunningAmiVersion "Use generic-lens or generic-optics with 'runningAmiVersion' instead." #-}

-- | The way that individual Amazon EC2 instances terminate when an automatic scale-in activity occurs or an instance group is resized. @TERMINATE_AT_INSTANCE_HOUR@ indicates that Amazon EMR terminates nodes at the instance-hour boundary, regardless of when the request to terminate the instance was submitted. This option is only available with Amazon EMR 5.1.0 and later and is the default for clusters created using that version. @TERMINATE_AT_TASK_COMPLETION@ indicates that Amazon EMR adds nodes to a deny list and drains tasks from nodes before terminating the Amazon EC2 instances, regardless of the instance-hour boundary. With either behavior, Amazon EMR removes the least active nodes first and blocks instance termination if it could lead to HDFS corruption. @TERMINATE_AT_TASK_COMPLETION@ is available only in Amazon EMR version 4.1.0 and later, and is the default for versions of Amazon EMR earlier than 5.1.0.
--
-- /Note:/ Consider using 'scaleDownBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfScaleDownBehavior :: Lens.Lens' Cluster (Core.Maybe Types.ScaleDownBehavior)
cfScaleDownBehavior = Lens.field @"scaleDownBehavior"
{-# DEPRECATED cfScaleDownBehavior "Use generic-lens or generic-optics with 'scaleDownBehavior' instead." #-}

-- | The name of the security configuration applied to the cluster.
--
-- /Note:/ Consider using 'securityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfSecurityConfiguration :: Lens.Lens' Cluster (Core.Maybe Types.XmlString)
cfSecurityConfiguration = Lens.field @"securityConfiguration"
{-# DEPRECATED cfSecurityConfiguration "Use generic-lens or generic-optics with 'securityConfiguration' instead." #-}

-- | The IAM role that will be assumed by the Amazon EMR service to access AWS resources on your behalf.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfServiceRole :: Lens.Lens' Cluster (Core.Maybe Types.ServiceRole)
cfServiceRole = Lens.field @"serviceRole"
{-# DEPRECATED cfServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

-- | The current status details about the cluster.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfStatus :: Lens.Lens' Cluster Types.ClusterStatus
cfStatus = Lens.field @"status"
{-# DEPRECATED cfStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Specifies the number of steps that can be executed concurrently.
--
-- /Note:/ Consider using 'stepConcurrencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfStepConcurrencyLevel :: Lens.Lens' Cluster (Core.Maybe Core.Int)
cfStepConcurrencyLevel = Lens.field @"stepConcurrencyLevel"
{-# DEPRECATED cfStepConcurrencyLevel "Use generic-lens or generic-optics with 'stepConcurrencyLevel' instead." #-}

-- | A list of tags associated with a cluster.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfTags :: Lens.Lens' Cluster (Core.Maybe [Types.Tag])
cfTags = Lens.field @"tags"
{-# DEPRECATED cfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Indicates whether Amazon EMR will lock the cluster to prevent the EC2 instances from being terminated by an API call or user intervention, or in the event of a cluster error.
--
-- /Note:/ Consider using 'terminationProtected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfTerminationProtected :: Lens.Lens' Cluster (Core.Maybe Core.Bool)
cfTerminationProtected = Lens.field @"terminationProtected"
{-# DEPRECATED cfTerminationProtected "Use generic-lens or generic-optics with 'terminationProtected' instead." #-}

-- | Indicates whether the cluster is visible to all IAM users of the AWS account associated with the cluster. The default value, @true@ , indicates that all IAM users in the AWS account can perform cluster actions if they have the proper IAM policy permissions. If this value is @false@ , only the IAM user that created the cluster can perform actions. This value can be changed on a running cluster by using the 'SetVisibleToAllUsers' action. You can override the default value of @true@ when you create a cluster by using the @VisibleToAllUsers@ parameter of the @RunJobFlow@ action.
--
-- /Note:/ Consider using 'visibleToAllUsers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfVisibleToAllUsers :: Lens.Lens' Cluster (Core.Maybe Core.Bool)
cfVisibleToAllUsers = Lens.field @"visibleToAllUsers"
{-# DEPRECATED cfVisibleToAllUsers "Use generic-lens or generic-optics with 'visibleToAllUsers' instead." #-}

instance Core.FromJSON Cluster where
  parseJSON =
    Core.withObject "Cluster" Core.$
      \x ->
        Cluster'
          Core.<$> (x Core..:? "Applications")
          Core.<*> (x Core..:? "AutoScalingRole")
          Core.<*> (x Core..:? "AutoTerminate")
          Core.<*> (x Core..:? "ClusterArn")
          Core.<*> (x Core..:? "Configurations")
          Core.<*> (x Core..:? "CustomAmiId")
          Core.<*> (x Core..:? "EbsRootVolumeSize")
          Core.<*> (x Core..:? "Ec2InstanceAttributes")
          Core.<*> (x Core..: "Id")
          Core.<*> (x Core..:? "InstanceCollectionType")
          Core.<*> (x Core..:? "KerberosAttributes")
          Core.<*> (x Core..:? "LogEncryptionKmsKeyId")
          Core.<*> (x Core..:? "LogUri")
          Core.<*> (x Core..:? "MasterPublicDnsName")
          Core.<*> (x Core..: "Name")
          Core.<*> (x Core..:? "NormalizedInstanceHours")
          Core.<*> (x Core..:? "OutpostArn")
          Core.<*> (x Core..:? "PlacementGroups")
          Core.<*> (x Core..:? "ReleaseLabel")
          Core.<*> (x Core..:? "RepoUpgradeOnBoot")
          Core.<*> (x Core..:? "RequestedAmiVersion")
          Core.<*> (x Core..:? "RunningAmiVersion")
          Core.<*> (x Core..:? "ScaleDownBehavior")
          Core.<*> (x Core..:? "SecurityConfiguration")
          Core.<*> (x Core..:? "ServiceRole")
          Core.<*> (x Core..: "Status")
          Core.<*> (x Core..:? "StepConcurrencyLevel")
          Core.<*> (x Core..:? "Tags")
          Core.<*> (x Core..:? "TerminationProtected")
          Core.<*> (x Core..:? "VisibleToAllUsers")
