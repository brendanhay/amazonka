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
    cfStatus,
    cfLogEncryptionKMSKeyId,
    cfClusterARN,
    cfRequestedAMIVersion,
    cfEBSRootVolumeSize,
    cfEC2InstanceAttributes,
    cfOutpostARN,
    cfNormalizedInstanceHours,
    cfConfigurations,
    cfCustomAMIId,
    cfAutoScalingRole,
    cfSecurityConfiguration,
    cfScaleDownBehavior,
    cfInstanceCollectionType,
    cfReleaseLabel,
    cfName,
    cfRepoUpgradeOnBoot,
    cfLogURI,
    cfKerberosAttributes,
    cfId,
    cfPlacementGroups,
    cfRunningAMIVersion,
    cfMasterPublicDNSName,
    cfTerminationProtected,
    cfVisibleToAllUsers,
    cfAutoTerminate,
    cfStepConcurrencyLevel,
    cfApplications,
    cfTags,
    cfServiceRole,
  )
where

import Network.AWS.EMR.Types.Application
import Network.AWS.EMR.Types.ClusterStatus
import Network.AWS.EMR.Types.Configuration
import Network.AWS.EMR.Types.EC2InstanceAttributes
import Network.AWS.EMR.Types.InstanceCollectionType
import Network.AWS.EMR.Types.KerberosAttributes
import Network.AWS.EMR.Types.PlacementGroupConfig
import Network.AWS.EMR.Types.RepoUpgradeOnBoot
import Network.AWS.EMR.Types.ScaleDownBehavior
import Network.AWS.EMR.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The detailed description of the cluster.
--
-- /See:/ 'mkCluster' smart constructor.
data Cluster = Cluster'
  { -- | The current status details about the cluster.
    status :: ClusterStatus,
    -- | The AWS KMS customer master key (CMK) used for encrypting log files. This attribute is only available with EMR version 5.30.0 and later, excluding EMR 6.0.0.
    logEncryptionKMSKeyId :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name of the cluster.
    clusterARN :: Lude.Maybe Lude.Text,
    -- | The AMI version requested for this cluster.
    requestedAMIVersion :: Lude.Maybe Lude.Text,
    -- | The size, in GiB, of the Amazon EBS root device volume of the Linux AMI that is used for each EC2 instance. Available in Amazon EMR version 4.x and later.
    ebsRootVolumeSize :: Lude.Maybe Lude.Int,
    -- | Provides information about the EC2 instances in a cluster grouped by category. For example, key name, subnet ID, IAM instance profile, and so on.
    ec2InstanceAttributes :: Lude.Maybe EC2InstanceAttributes,
    -- | The Amazon Resource Name (ARN) of the Outpost where the cluster is launched.
    outpostARN :: Lude.Maybe Lude.Text,
    -- | An approximation of the cost of the cluster, represented in m1.small/hours. This value is incremented one time for every hour an m1.small instance runs. Larger instances are weighted more, so an EC2 instance that is roughly four times more expensive would result in the normalized instance hours being incremented by four. This result is only an approximation and does not reflect the actual billing rate.
    normalizedInstanceHours :: Lude.Maybe Lude.Int,
    -- | Applies only to Amazon EMR releases 4.x and later. The list of Configurations supplied to the EMR cluster.
    configurations :: Lude.Maybe [Configuration],
    -- | Available only in Amazon EMR version 5.7.0 and later. The ID of a custom Amazon EBS-backed Linux AMI if the cluster uses a custom AMI.
    customAMIId :: Lude.Maybe Lude.Text,
    -- | An IAM role for automatic scaling policies. The default role is @EMR_AutoScaling_DefaultRole@ . The IAM role provides permissions that the automatic scaling feature requires to launch and terminate EC2 instances in an instance group.
    autoScalingRole :: Lude.Maybe Lude.Text,
    -- | The name of the security configuration applied to the cluster.
    securityConfiguration :: Lude.Maybe Lude.Text,
    -- | The way that individual Amazon EC2 instances terminate when an automatic scale-in activity occurs or an instance group is resized. @TERMINATE_AT_INSTANCE_HOUR@ indicates that Amazon EMR terminates nodes at the instance-hour boundary, regardless of when the request to terminate the instance was submitted. This option is only available with Amazon EMR 5.1.0 and later and is the default for clusters created using that version. @TERMINATE_AT_TASK_COMPLETION@ indicates that Amazon EMR adds nodes to a deny list and drains tasks from nodes before terminating the Amazon EC2 instances, regardless of the instance-hour boundary. With either behavior, Amazon EMR removes the least active nodes first and blocks instance termination if it could lead to HDFS corruption. @TERMINATE_AT_TASK_COMPLETION@ is available only in Amazon EMR version 4.1.0 and later, and is the default for versions of Amazon EMR earlier than 5.1.0.
    scaleDownBehavior :: Lude.Maybe ScaleDownBehavior,
    -- | The instance group configuration of the cluster. A value of @INSTANCE_GROUP@ indicates a uniform instance group configuration. A value of @INSTANCE_FLEET@ indicates an instance fleets configuration.
    instanceCollectionType :: Lude.Maybe InstanceCollectionType,
    -- | The Amazon EMR release label, which determines the version of open-source application packages installed on the cluster. Release labels are in the form @emr-x.x.x@ , where x.x.x is an Amazon EMR release version such as @emr-5.14.0@ . For more information about Amazon EMR release versions and included application versions and features, see <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/ https://docs.aws.amazon.com/emr/latest/ReleaseGuide/> . The release label applies only to Amazon EMR releases version 4.0 and later. Earlier versions use @AmiVersion@ .
    releaseLabel :: Lude.Maybe Lude.Text,
    -- | The name of the cluster.
    name :: Lude.Text,
    -- | Applies only when @CustomAmiID@ is used. Specifies the type of updates that are applied from the Amazon Linux AMI package repositories when an instance boots using the AMI.
    repoUpgradeOnBoot :: Lude.Maybe RepoUpgradeOnBoot,
    -- | The path to the Amazon S3 location where logs for this cluster are stored.
    logURI :: Lude.Maybe Lude.Text,
    -- | Attributes for Kerberos configuration when Kerberos authentication is enabled using a security configuration. For more information see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-kerberos.html Use Kerberos Authentication> in the /Amazon EMR Management Guide/ .
    kerberosAttributes :: Lude.Maybe KerberosAttributes,
    -- | The unique identifier for the cluster.
    id :: Lude.Text,
    -- | Placement group configured for an Amazon EMR cluster.
    placementGroups :: Lude.Maybe [PlacementGroupConfig],
    -- | The AMI version running on this cluster.
    runningAMIVersion :: Lude.Maybe Lude.Text,
    -- | The DNS name of the master node. If the cluster is on a private subnet, this is the private DNS name. On a public subnet, this is the public DNS name.
    masterPublicDNSName :: Lude.Maybe Lude.Text,
    -- | Indicates whether Amazon EMR will lock the cluster to prevent the EC2 instances from being terminated by an API call or user intervention, or in the event of a cluster error.
    terminationProtected :: Lude.Maybe Lude.Bool,
    -- | Indicates whether the cluster is visible to all IAM users of the AWS account associated with the cluster. The default value, @true@ , indicates that all IAM users in the AWS account can perform cluster actions if they have the proper IAM policy permissions. If this value is @false@ , only the IAM user that created the cluster can perform actions. This value can be changed on a running cluster by using the 'SetVisibleToAllUsers' action. You can override the default value of @true@ when you create a cluster by using the @VisibleToAllUsers@ parameter of the @RunJobFlow@ action.
    visibleToAllUsers :: Lude.Maybe Lude.Bool,
    -- | Specifies whether the cluster should terminate after completing all steps.
    autoTerminate :: Lude.Maybe Lude.Bool,
    -- | Specifies the number of steps that can be executed concurrently.
    stepConcurrencyLevel :: Lude.Maybe Lude.Int,
    -- | The applications installed on this cluster.
    applications :: Lude.Maybe [Application],
    -- | A list of tags associated with a cluster.
    tags :: Lude.Maybe [Tag],
    -- | The IAM role that will be assumed by the Amazon EMR service to access AWS resources on your behalf.
    serviceRole :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Cluster' with the minimum fields required to make a request.
--
-- * 'status' - The current status details about the cluster.
-- * 'logEncryptionKMSKeyId' - The AWS KMS customer master key (CMK) used for encrypting log files. This attribute is only available with EMR version 5.30.0 and later, excluding EMR 6.0.0.
-- * 'clusterARN' - The Amazon Resource Name of the cluster.
-- * 'requestedAMIVersion' - The AMI version requested for this cluster.
-- * 'ebsRootVolumeSize' - The size, in GiB, of the Amazon EBS root device volume of the Linux AMI that is used for each EC2 instance. Available in Amazon EMR version 4.x and later.
-- * 'ec2InstanceAttributes' - Provides information about the EC2 instances in a cluster grouped by category. For example, key name, subnet ID, IAM instance profile, and so on.
-- * 'outpostARN' - The Amazon Resource Name (ARN) of the Outpost where the cluster is launched.
-- * 'normalizedInstanceHours' - An approximation of the cost of the cluster, represented in m1.small/hours. This value is incremented one time for every hour an m1.small instance runs. Larger instances are weighted more, so an EC2 instance that is roughly four times more expensive would result in the normalized instance hours being incremented by four. This result is only an approximation and does not reflect the actual billing rate.
-- * 'configurations' - Applies only to Amazon EMR releases 4.x and later. The list of Configurations supplied to the EMR cluster.
-- * 'customAMIId' - Available only in Amazon EMR version 5.7.0 and later. The ID of a custom Amazon EBS-backed Linux AMI if the cluster uses a custom AMI.
-- * 'autoScalingRole' - An IAM role for automatic scaling policies. The default role is @EMR_AutoScaling_DefaultRole@ . The IAM role provides permissions that the automatic scaling feature requires to launch and terminate EC2 instances in an instance group.
-- * 'securityConfiguration' - The name of the security configuration applied to the cluster.
-- * 'scaleDownBehavior' - The way that individual Amazon EC2 instances terminate when an automatic scale-in activity occurs or an instance group is resized. @TERMINATE_AT_INSTANCE_HOUR@ indicates that Amazon EMR terminates nodes at the instance-hour boundary, regardless of when the request to terminate the instance was submitted. This option is only available with Amazon EMR 5.1.0 and later and is the default for clusters created using that version. @TERMINATE_AT_TASK_COMPLETION@ indicates that Amazon EMR adds nodes to a deny list and drains tasks from nodes before terminating the Amazon EC2 instances, regardless of the instance-hour boundary. With either behavior, Amazon EMR removes the least active nodes first and blocks instance termination if it could lead to HDFS corruption. @TERMINATE_AT_TASK_COMPLETION@ is available only in Amazon EMR version 4.1.0 and later, and is the default for versions of Amazon EMR earlier than 5.1.0.
-- * 'instanceCollectionType' - The instance group configuration of the cluster. A value of @INSTANCE_GROUP@ indicates a uniform instance group configuration. A value of @INSTANCE_FLEET@ indicates an instance fleets configuration.
-- * 'releaseLabel' - The Amazon EMR release label, which determines the version of open-source application packages installed on the cluster. Release labels are in the form @emr-x.x.x@ , where x.x.x is an Amazon EMR release version such as @emr-5.14.0@ . For more information about Amazon EMR release versions and included application versions and features, see <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/ https://docs.aws.amazon.com/emr/latest/ReleaseGuide/> . The release label applies only to Amazon EMR releases version 4.0 and later. Earlier versions use @AmiVersion@ .
-- * 'name' - The name of the cluster.
-- * 'repoUpgradeOnBoot' - Applies only when @CustomAmiID@ is used. Specifies the type of updates that are applied from the Amazon Linux AMI package repositories when an instance boots using the AMI.
-- * 'logURI' - The path to the Amazon S3 location where logs for this cluster are stored.
-- * 'kerberosAttributes' - Attributes for Kerberos configuration when Kerberos authentication is enabled using a security configuration. For more information see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-kerberos.html Use Kerberos Authentication> in the /Amazon EMR Management Guide/ .
-- * 'id' - The unique identifier for the cluster.
-- * 'placementGroups' - Placement group configured for an Amazon EMR cluster.
-- * 'runningAMIVersion' - The AMI version running on this cluster.
-- * 'masterPublicDNSName' - The DNS name of the master node. If the cluster is on a private subnet, this is the private DNS name. On a public subnet, this is the public DNS name.
-- * 'terminationProtected' - Indicates whether Amazon EMR will lock the cluster to prevent the EC2 instances from being terminated by an API call or user intervention, or in the event of a cluster error.
-- * 'visibleToAllUsers' - Indicates whether the cluster is visible to all IAM users of the AWS account associated with the cluster. The default value, @true@ , indicates that all IAM users in the AWS account can perform cluster actions if they have the proper IAM policy permissions. If this value is @false@ , only the IAM user that created the cluster can perform actions. This value can be changed on a running cluster by using the 'SetVisibleToAllUsers' action. You can override the default value of @true@ when you create a cluster by using the @VisibleToAllUsers@ parameter of the @RunJobFlow@ action.
-- * 'autoTerminate' - Specifies whether the cluster should terminate after completing all steps.
-- * 'stepConcurrencyLevel' - Specifies the number of steps that can be executed concurrently.
-- * 'applications' - The applications installed on this cluster.
-- * 'tags' - A list of tags associated with a cluster.
-- * 'serviceRole' - The IAM role that will be assumed by the Amazon EMR service to access AWS resources on your behalf.
mkCluster ::
  -- | 'status'
  ClusterStatus ->
  -- | 'name'
  Lude.Text ->
  -- | 'id'
  Lude.Text ->
  Cluster
mkCluster pStatus_ pName_ pId_ =
  Cluster'
    { status = pStatus_,
      logEncryptionKMSKeyId = Lude.Nothing,
      clusterARN = Lude.Nothing,
      requestedAMIVersion = Lude.Nothing,
      ebsRootVolumeSize = Lude.Nothing,
      ec2InstanceAttributes = Lude.Nothing,
      outpostARN = Lude.Nothing,
      normalizedInstanceHours = Lude.Nothing,
      configurations = Lude.Nothing,
      customAMIId = Lude.Nothing,
      autoScalingRole = Lude.Nothing,
      securityConfiguration = Lude.Nothing,
      scaleDownBehavior = Lude.Nothing,
      instanceCollectionType = Lude.Nothing,
      releaseLabel = Lude.Nothing,
      name = pName_,
      repoUpgradeOnBoot = Lude.Nothing,
      logURI = Lude.Nothing,
      kerberosAttributes = Lude.Nothing,
      id = pId_,
      placementGroups = Lude.Nothing,
      runningAMIVersion = Lude.Nothing,
      masterPublicDNSName = Lude.Nothing,
      terminationProtected = Lude.Nothing,
      visibleToAllUsers = Lude.Nothing,
      autoTerminate = Lude.Nothing,
      stepConcurrencyLevel = Lude.Nothing,
      applications = Lude.Nothing,
      tags = Lude.Nothing,
      serviceRole = Lude.Nothing
    }

-- | The current status details about the cluster.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfStatus :: Lens.Lens' Cluster ClusterStatus
cfStatus = Lens.lens (status :: Cluster -> ClusterStatus) (\s a -> s {status = a} :: Cluster)
{-# DEPRECATED cfStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The AWS KMS customer master key (CMK) used for encrypting log files. This attribute is only available with EMR version 5.30.0 and later, excluding EMR 6.0.0.
--
-- /Note:/ Consider using 'logEncryptionKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfLogEncryptionKMSKeyId :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cfLogEncryptionKMSKeyId = Lens.lens (logEncryptionKMSKeyId :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {logEncryptionKMSKeyId = a} :: Cluster)
{-# DEPRECATED cfLogEncryptionKMSKeyId "Use generic-lens or generic-optics with 'logEncryptionKMSKeyId' instead." #-}

-- | The Amazon Resource Name of the cluster.
--
-- /Note:/ Consider using 'clusterARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfClusterARN :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cfClusterARN = Lens.lens (clusterARN :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {clusterARN = a} :: Cluster)
{-# DEPRECATED cfClusterARN "Use generic-lens or generic-optics with 'clusterARN' instead." #-}

-- | The AMI version requested for this cluster.
--
-- /Note:/ Consider using 'requestedAMIVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfRequestedAMIVersion :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cfRequestedAMIVersion = Lens.lens (requestedAMIVersion :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {requestedAMIVersion = a} :: Cluster)
{-# DEPRECATED cfRequestedAMIVersion "Use generic-lens or generic-optics with 'requestedAMIVersion' instead." #-}

-- | The size, in GiB, of the Amazon EBS root device volume of the Linux AMI that is used for each EC2 instance. Available in Amazon EMR version 4.x and later.
--
-- /Note:/ Consider using 'ebsRootVolumeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfEBSRootVolumeSize :: Lens.Lens' Cluster (Lude.Maybe Lude.Int)
cfEBSRootVolumeSize = Lens.lens (ebsRootVolumeSize :: Cluster -> Lude.Maybe Lude.Int) (\s a -> s {ebsRootVolumeSize = a} :: Cluster)
{-# DEPRECATED cfEBSRootVolumeSize "Use generic-lens or generic-optics with 'ebsRootVolumeSize' instead." #-}

-- | Provides information about the EC2 instances in a cluster grouped by category. For example, key name, subnet ID, IAM instance profile, and so on.
--
-- /Note:/ Consider using 'ec2InstanceAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfEC2InstanceAttributes :: Lens.Lens' Cluster (Lude.Maybe EC2InstanceAttributes)
cfEC2InstanceAttributes = Lens.lens (ec2InstanceAttributes :: Cluster -> Lude.Maybe EC2InstanceAttributes) (\s a -> s {ec2InstanceAttributes = a} :: Cluster)
{-# DEPRECATED cfEC2InstanceAttributes "Use generic-lens or generic-optics with 'ec2InstanceAttributes' instead." #-}

-- | The Amazon Resource Name (ARN) of the Outpost where the cluster is launched.
--
-- /Note:/ Consider using 'outpostARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfOutpostARN :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cfOutpostARN = Lens.lens (outpostARN :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {outpostARN = a} :: Cluster)
{-# DEPRECATED cfOutpostARN "Use generic-lens or generic-optics with 'outpostARN' instead." #-}

-- | An approximation of the cost of the cluster, represented in m1.small/hours. This value is incremented one time for every hour an m1.small instance runs. Larger instances are weighted more, so an EC2 instance that is roughly four times more expensive would result in the normalized instance hours being incremented by four. This result is only an approximation and does not reflect the actual billing rate.
--
-- /Note:/ Consider using 'normalizedInstanceHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfNormalizedInstanceHours :: Lens.Lens' Cluster (Lude.Maybe Lude.Int)
cfNormalizedInstanceHours = Lens.lens (normalizedInstanceHours :: Cluster -> Lude.Maybe Lude.Int) (\s a -> s {normalizedInstanceHours = a} :: Cluster)
{-# DEPRECATED cfNormalizedInstanceHours "Use generic-lens or generic-optics with 'normalizedInstanceHours' instead." #-}

-- | Applies only to Amazon EMR releases 4.x and later. The list of Configurations supplied to the EMR cluster.
--
-- /Note:/ Consider using 'configurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfConfigurations :: Lens.Lens' Cluster (Lude.Maybe [Configuration])
cfConfigurations = Lens.lens (configurations :: Cluster -> Lude.Maybe [Configuration]) (\s a -> s {configurations = a} :: Cluster)
{-# DEPRECATED cfConfigurations "Use generic-lens or generic-optics with 'configurations' instead." #-}

-- | Available only in Amazon EMR version 5.7.0 and later. The ID of a custom Amazon EBS-backed Linux AMI if the cluster uses a custom AMI.
--
-- /Note:/ Consider using 'customAMIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfCustomAMIId :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cfCustomAMIId = Lens.lens (customAMIId :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {customAMIId = a} :: Cluster)
{-# DEPRECATED cfCustomAMIId "Use generic-lens or generic-optics with 'customAMIId' instead." #-}

-- | An IAM role for automatic scaling policies. The default role is @EMR_AutoScaling_DefaultRole@ . The IAM role provides permissions that the automatic scaling feature requires to launch and terminate EC2 instances in an instance group.
--
-- /Note:/ Consider using 'autoScalingRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfAutoScalingRole :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cfAutoScalingRole = Lens.lens (autoScalingRole :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {autoScalingRole = a} :: Cluster)
{-# DEPRECATED cfAutoScalingRole "Use generic-lens or generic-optics with 'autoScalingRole' instead." #-}

-- | The name of the security configuration applied to the cluster.
--
-- /Note:/ Consider using 'securityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfSecurityConfiguration :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cfSecurityConfiguration = Lens.lens (securityConfiguration :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {securityConfiguration = a} :: Cluster)
{-# DEPRECATED cfSecurityConfiguration "Use generic-lens or generic-optics with 'securityConfiguration' instead." #-}

-- | The way that individual Amazon EC2 instances terminate when an automatic scale-in activity occurs or an instance group is resized. @TERMINATE_AT_INSTANCE_HOUR@ indicates that Amazon EMR terminates nodes at the instance-hour boundary, regardless of when the request to terminate the instance was submitted. This option is only available with Amazon EMR 5.1.0 and later and is the default for clusters created using that version. @TERMINATE_AT_TASK_COMPLETION@ indicates that Amazon EMR adds nodes to a deny list and drains tasks from nodes before terminating the Amazon EC2 instances, regardless of the instance-hour boundary. With either behavior, Amazon EMR removes the least active nodes first and blocks instance termination if it could lead to HDFS corruption. @TERMINATE_AT_TASK_COMPLETION@ is available only in Amazon EMR version 4.1.0 and later, and is the default for versions of Amazon EMR earlier than 5.1.0.
--
-- /Note:/ Consider using 'scaleDownBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfScaleDownBehavior :: Lens.Lens' Cluster (Lude.Maybe ScaleDownBehavior)
cfScaleDownBehavior = Lens.lens (scaleDownBehavior :: Cluster -> Lude.Maybe ScaleDownBehavior) (\s a -> s {scaleDownBehavior = a} :: Cluster)
{-# DEPRECATED cfScaleDownBehavior "Use generic-lens or generic-optics with 'scaleDownBehavior' instead." #-}

-- | The instance group configuration of the cluster. A value of @INSTANCE_GROUP@ indicates a uniform instance group configuration. A value of @INSTANCE_FLEET@ indicates an instance fleets configuration.
--
-- /Note:/ Consider using 'instanceCollectionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfInstanceCollectionType :: Lens.Lens' Cluster (Lude.Maybe InstanceCollectionType)
cfInstanceCollectionType = Lens.lens (instanceCollectionType :: Cluster -> Lude.Maybe InstanceCollectionType) (\s a -> s {instanceCollectionType = a} :: Cluster)
{-# DEPRECATED cfInstanceCollectionType "Use generic-lens or generic-optics with 'instanceCollectionType' instead." #-}

-- | The Amazon EMR release label, which determines the version of open-source application packages installed on the cluster. Release labels are in the form @emr-x.x.x@ , where x.x.x is an Amazon EMR release version such as @emr-5.14.0@ . For more information about Amazon EMR release versions and included application versions and features, see <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/ https://docs.aws.amazon.com/emr/latest/ReleaseGuide/> . The release label applies only to Amazon EMR releases version 4.0 and later. Earlier versions use @AmiVersion@ .
--
-- /Note:/ Consider using 'releaseLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfReleaseLabel :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cfReleaseLabel = Lens.lens (releaseLabel :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {releaseLabel = a} :: Cluster)
{-# DEPRECATED cfReleaseLabel "Use generic-lens or generic-optics with 'releaseLabel' instead." #-}

-- | The name of the cluster.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfName :: Lens.Lens' Cluster Lude.Text
cfName = Lens.lens (name :: Cluster -> Lude.Text) (\s a -> s {name = a} :: Cluster)
{-# DEPRECATED cfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Applies only when @CustomAmiID@ is used. Specifies the type of updates that are applied from the Amazon Linux AMI package repositories when an instance boots using the AMI.
--
-- /Note:/ Consider using 'repoUpgradeOnBoot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfRepoUpgradeOnBoot :: Lens.Lens' Cluster (Lude.Maybe RepoUpgradeOnBoot)
cfRepoUpgradeOnBoot = Lens.lens (repoUpgradeOnBoot :: Cluster -> Lude.Maybe RepoUpgradeOnBoot) (\s a -> s {repoUpgradeOnBoot = a} :: Cluster)
{-# DEPRECATED cfRepoUpgradeOnBoot "Use generic-lens or generic-optics with 'repoUpgradeOnBoot' instead." #-}

-- | The path to the Amazon S3 location where logs for this cluster are stored.
--
-- /Note:/ Consider using 'logURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfLogURI :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cfLogURI = Lens.lens (logURI :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {logURI = a} :: Cluster)
{-# DEPRECATED cfLogURI "Use generic-lens or generic-optics with 'logURI' instead." #-}

-- | Attributes for Kerberos configuration when Kerberos authentication is enabled using a security configuration. For more information see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-kerberos.html Use Kerberos Authentication> in the /Amazon EMR Management Guide/ .
--
-- /Note:/ Consider using 'kerberosAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfKerberosAttributes :: Lens.Lens' Cluster (Lude.Maybe KerberosAttributes)
cfKerberosAttributes = Lens.lens (kerberosAttributes :: Cluster -> Lude.Maybe KerberosAttributes) (\s a -> s {kerberosAttributes = a} :: Cluster)
{-# DEPRECATED cfKerberosAttributes "Use generic-lens or generic-optics with 'kerberosAttributes' instead." #-}

-- | The unique identifier for the cluster.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfId :: Lens.Lens' Cluster Lude.Text
cfId = Lens.lens (id :: Cluster -> Lude.Text) (\s a -> s {id = a} :: Cluster)
{-# DEPRECATED cfId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Placement group configured for an Amazon EMR cluster.
--
-- /Note:/ Consider using 'placementGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfPlacementGroups :: Lens.Lens' Cluster (Lude.Maybe [PlacementGroupConfig])
cfPlacementGroups = Lens.lens (placementGroups :: Cluster -> Lude.Maybe [PlacementGroupConfig]) (\s a -> s {placementGroups = a} :: Cluster)
{-# DEPRECATED cfPlacementGroups "Use generic-lens or generic-optics with 'placementGroups' instead." #-}

-- | The AMI version running on this cluster.
--
-- /Note:/ Consider using 'runningAMIVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfRunningAMIVersion :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cfRunningAMIVersion = Lens.lens (runningAMIVersion :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {runningAMIVersion = a} :: Cluster)
{-# DEPRECATED cfRunningAMIVersion "Use generic-lens or generic-optics with 'runningAMIVersion' instead." #-}

-- | The DNS name of the master node. If the cluster is on a private subnet, this is the private DNS name. On a public subnet, this is the public DNS name.
--
-- /Note:/ Consider using 'masterPublicDNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfMasterPublicDNSName :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cfMasterPublicDNSName = Lens.lens (masterPublicDNSName :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {masterPublicDNSName = a} :: Cluster)
{-# DEPRECATED cfMasterPublicDNSName "Use generic-lens or generic-optics with 'masterPublicDNSName' instead." #-}

-- | Indicates whether Amazon EMR will lock the cluster to prevent the EC2 instances from being terminated by an API call or user intervention, or in the event of a cluster error.
--
-- /Note:/ Consider using 'terminationProtected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfTerminationProtected :: Lens.Lens' Cluster (Lude.Maybe Lude.Bool)
cfTerminationProtected = Lens.lens (terminationProtected :: Cluster -> Lude.Maybe Lude.Bool) (\s a -> s {terminationProtected = a} :: Cluster)
{-# DEPRECATED cfTerminationProtected "Use generic-lens or generic-optics with 'terminationProtected' instead." #-}

-- | Indicates whether the cluster is visible to all IAM users of the AWS account associated with the cluster. The default value, @true@ , indicates that all IAM users in the AWS account can perform cluster actions if they have the proper IAM policy permissions. If this value is @false@ , only the IAM user that created the cluster can perform actions. This value can be changed on a running cluster by using the 'SetVisibleToAllUsers' action. You can override the default value of @true@ when you create a cluster by using the @VisibleToAllUsers@ parameter of the @RunJobFlow@ action.
--
-- /Note:/ Consider using 'visibleToAllUsers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfVisibleToAllUsers :: Lens.Lens' Cluster (Lude.Maybe Lude.Bool)
cfVisibleToAllUsers = Lens.lens (visibleToAllUsers :: Cluster -> Lude.Maybe Lude.Bool) (\s a -> s {visibleToAllUsers = a} :: Cluster)
{-# DEPRECATED cfVisibleToAllUsers "Use generic-lens or generic-optics with 'visibleToAllUsers' instead." #-}

-- | Specifies whether the cluster should terminate after completing all steps.
--
-- /Note:/ Consider using 'autoTerminate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfAutoTerminate :: Lens.Lens' Cluster (Lude.Maybe Lude.Bool)
cfAutoTerminate = Lens.lens (autoTerminate :: Cluster -> Lude.Maybe Lude.Bool) (\s a -> s {autoTerminate = a} :: Cluster)
{-# DEPRECATED cfAutoTerminate "Use generic-lens or generic-optics with 'autoTerminate' instead." #-}

-- | Specifies the number of steps that can be executed concurrently.
--
-- /Note:/ Consider using 'stepConcurrencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfStepConcurrencyLevel :: Lens.Lens' Cluster (Lude.Maybe Lude.Int)
cfStepConcurrencyLevel = Lens.lens (stepConcurrencyLevel :: Cluster -> Lude.Maybe Lude.Int) (\s a -> s {stepConcurrencyLevel = a} :: Cluster)
{-# DEPRECATED cfStepConcurrencyLevel "Use generic-lens or generic-optics with 'stepConcurrencyLevel' instead." #-}

-- | The applications installed on this cluster.
--
-- /Note:/ Consider using 'applications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfApplications :: Lens.Lens' Cluster (Lude.Maybe [Application])
cfApplications = Lens.lens (applications :: Cluster -> Lude.Maybe [Application]) (\s a -> s {applications = a} :: Cluster)
{-# DEPRECATED cfApplications "Use generic-lens or generic-optics with 'applications' instead." #-}

-- | A list of tags associated with a cluster.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfTags :: Lens.Lens' Cluster (Lude.Maybe [Tag])
cfTags = Lens.lens (tags :: Cluster -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: Cluster)
{-# DEPRECATED cfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The IAM role that will be assumed by the Amazon EMR service to access AWS resources on your behalf.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfServiceRole :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cfServiceRole = Lens.lens (serviceRole :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {serviceRole = a} :: Cluster)
{-# DEPRECATED cfServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

instance Lude.FromJSON Cluster where
  parseJSON =
    Lude.withObject
      "Cluster"
      ( \x ->
          Cluster'
            Lude.<$> (x Lude..: "Status")
            Lude.<*> (x Lude..:? "LogEncryptionKmsKeyId")
            Lude.<*> (x Lude..:? "ClusterArn")
            Lude.<*> (x Lude..:? "RequestedAmiVersion")
            Lude.<*> (x Lude..:? "EbsRootVolumeSize")
            Lude.<*> (x Lude..:? "Ec2InstanceAttributes")
            Lude.<*> (x Lude..:? "OutpostArn")
            Lude.<*> (x Lude..:? "NormalizedInstanceHours")
            Lude.<*> (x Lude..:? "Configurations" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "CustomAmiId")
            Lude.<*> (x Lude..:? "AutoScalingRole")
            Lude.<*> (x Lude..:? "SecurityConfiguration")
            Lude.<*> (x Lude..:? "ScaleDownBehavior")
            Lude.<*> (x Lude..:? "InstanceCollectionType")
            Lude.<*> (x Lude..:? "ReleaseLabel")
            Lude.<*> (x Lude..: "Name")
            Lude.<*> (x Lude..:? "RepoUpgradeOnBoot")
            Lude.<*> (x Lude..:? "LogUri")
            Lude.<*> (x Lude..:? "KerberosAttributes")
            Lude.<*> (x Lude..: "Id")
            Lude.<*> (x Lude..:? "PlacementGroups" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "RunningAmiVersion")
            Lude.<*> (x Lude..:? "MasterPublicDnsName")
            Lude.<*> (x Lude..:? "TerminationProtected")
            Lude.<*> (x Lude..:? "VisibleToAllUsers")
            Lude.<*> (x Lude..:? "AutoTerminate")
            Lude.<*> (x Lude..:? "StepConcurrencyLevel")
            Lude.<*> (x Lude..:? "Applications" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Tags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ServiceRole")
      )
