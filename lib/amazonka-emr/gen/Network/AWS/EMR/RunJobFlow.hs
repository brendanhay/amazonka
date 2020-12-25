{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.RunJobFlow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- RunJobFlow creates and starts running a new cluster (job flow). The cluster runs the steps specified. After the steps complete, the cluster stops and the HDFS partition is lost. To prevent loss of data, configure the last step of the job flow to store results in Amazon S3. If the 'JobFlowInstancesConfig' @KeepJobFlowAliveWhenNoSteps@ parameter is set to @TRUE@ , the cluster transitions to the WAITING state rather than shutting down after the steps have completed.
--
-- For additional protection, you can set the 'JobFlowInstancesConfig' @TerminationProtected@ parameter to @TRUE@ to lock the cluster and prevent it from being terminated by API call, user intervention, or in the event of a job flow error.
-- A maximum of 256 steps are allowed in each job flow.
-- If your cluster is long-running (such as a Hive data warehouse) or complex, you may require more than 256 steps to process your data. You can bypass the 256-step limitation in various ways, including using the SSH shell to connect to the master node and submitting queries directly to the software running on the master node, such as Hive and Hadoop. For more information on how to do this, see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/AddMoreThan256Steps.html Add More than 256 Steps to a Cluster> in the /Amazon EMR Management Guide/ .
-- For long running clusters, we recommend that you periodically store your results.
module Network.AWS.EMR.RunJobFlow
  ( -- * Creating a request
    RunJobFlow (..),
    mkRunJobFlow,

    -- ** Request lenses
    rjfName,
    rjfInstances,
    rjfAdditionalInfo,
    rjfAmiVersion,
    rjfApplications,
    rjfAutoScalingRole,
    rjfBootstrapActions,
    rjfConfigurations,
    rjfCustomAmiId,
    rjfEbsRootVolumeSize,
    rjfJobFlowRole,
    rjfKerberosAttributes,
    rjfLogEncryptionKmsKeyId,
    rjfLogUri,
    rjfManagedScalingPolicy,
    rjfNewSupportedProducts,
    rjfPlacementGroupConfigs,
    rjfReleaseLabel,
    rjfRepoUpgradeOnBoot,
    rjfScaleDownBehavior,
    rjfSecurityConfiguration,
    rjfServiceRole,
    rjfStepConcurrencyLevel,
    rjfSteps,
    rjfSupportedProducts,
    rjfTags,
    rjfVisibleToAllUsers,

    -- * Destructuring the response
    RunJobFlowResponse (..),
    mkRunJobFlowResponse,

    -- ** Response lenses
    rjfrrsClusterArn,
    rjfrrsJobFlowId,
    rjfrrsResponseStatus,
  )
where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the 'RunJobFlow' operation.
--
-- /See:/ 'mkRunJobFlow' smart constructor.
data RunJobFlow = RunJobFlow'
  { -- | The name of the job flow.
    name :: Types.XmlStringMaxLen256,
    -- | A specification of the number and type of Amazon EC2 instances.
    instances :: Types.JobFlowInstancesConfig,
    -- | A JSON string for selecting additional features.
    additionalInfo :: Core.Maybe Types.XmlString,
    -- | Applies only to Amazon EMR AMI versions 3.x and 2.x. For Amazon EMR releases 4.0 and later, @ReleaseLabel@ is used. To specify a custom AMI, use @CustomAmiID@ .
    amiVersion :: Core.Maybe Types.XmlStringMaxLen256,
    -- | Applies to Amazon EMR releases 4.0 and later. A case-insensitive list of applications for Amazon EMR to install and configure when launching the cluster. For a list of applications available for each Amazon EMR release version, see the <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/ Amazon EMR Release Guide> .
    applications :: Core.Maybe [Types.Application],
    -- | An IAM role for automatic scaling policies. The default role is @EMR_AutoScaling_DefaultRole@ . The IAM role provides permissions that the automatic scaling feature requires to launch and terminate EC2 instances in an instance group.
    autoScalingRole :: Core.Maybe Types.XmlString,
    -- | A list of bootstrap actions to run before Hadoop starts on the cluster nodes.
    bootstrapActions :: Core.Maybe [Types.BootstrapActionConfig],
    -- | For Amazon EMR releases 4.0 and later. The list of configurations supplied for the EMR cluster you are creating.
    configurations :: Core.Maybe [Types.Configuration],
    -- | Available only in Amazon EMR version 5.7.0 and later. The ID of a custom Amazon EBS-backed Linux AMI. If specified, Amazon EMR uses this AMI when it launches cluster EC2 instances. For more information about custom AMIs in Amazon EMR, see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-custom-ami.html Using a Custom AMI> in the /Amazon EMR Management Guide/ . If omitted, the cluster uses the base Linux AMI for the @ReleaseLabel@ specified. For Amazon EMR versions 2.x and 3.x, use @AmiVersion@ instead.
    --
    -- For information about creating a custom AMI, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/creating-an-ami-ebs.html Creating an Amazon EBS-Backed Linux AMI> in the /Amazon Elastic Compute Cloud User Guide for Linux Instances/ . For information about finding an AMI ID, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html Finding a Linux AMI> .
    customAmiId :: Core.Maybe Types.XmlStringMaxLen256,
    -- | The size, in GiB, of the Amazon EBS root device volume of the Linux AMI that is used for each EC2 instance. Available in Amazon EMR version 4.x and later.
    ebsRootVolumeSize :: Core.Maybe Core.Int,
    -- | Also called instance profile and EC2 role. An IAM role for an EMR cluster. The EC2 instances of the cluster assume this role. The default role is @EMR_EC2_DefaultRole@ . In order to use the default role, you must have already created it using the CLI or console.
    jobFlowRole :: Core.Maybe Types.XmlString,
    -- | Attributes for Kerberos configuration when Kerberos authentication is enabled using a security configuration. For more information see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-kerberos.html Use Kerberos Authentication> in the /Amazon EMR Management Guide/ .
    kerberosAttributes :: Core.Maybe Types.KerberosAttributes,
    -- | The AWS KMS customer master key (CMK) used for encrypting log files. If a value is not provided, the logs remain encrypted by AES-256. This attribute is only available with Amazon EMR version 5.30.0 and later, excluding Amazon EMR 6.0.0.
    logEncryptionKmsKeyId :: Core.Maybe Types.XmlString,
    -- | The location in Amazon S3 to write the log files of the job flow. If a value is not provided, logs are not created.
    logUri :: Core.Maybe Types.XmlString,
    -- | The specified managed scaling policy for an Amazon EMR cluster.
    managedScalingPolicy :: Core.Maybe Types.ManagedScalingPolicy,
    -- | A list of strings that indicates third-party software to use with the job flow that accepts a user argument list. EMR accepts and forwards the argument list to the corresponding installation script as bootstrap action arguments. For more information, see "Launch a Job Flow on the MapR Distribution for Hadoop" in the <https://docs.aws.amazon.com/emr/latest/DeveloperGuide/emr-dg.pdf Amazon EMR Developer Guide> . Supported values are:
    --
    --
    --     * "mapr-m3" - launch the cluster using MapR M3 Edition.
    --
    --
    --     * "mapr-m5" - launch the cluster using MapR M5 Edition.
    --
    --
    --     * "mapr" with the user arguments specifying "--edition,m3" or "--edition,m5" - launch the job flow using MapR M3 or M5 Edition respectively.
    --
    --
    --     * "mapr-m7" - launch the cluster using MapR M7 Edition.
    --
    --
    --     * "hunk" - launch the cluster with the Hunk Big Data Analtics Platform.
    --
    --
    --     * "hue"- launch the cluster with Hue installed.
    --
    --
    --     * "spark" - launch the cluster with Apache Spark installed.
    --
    --
    --     * "ganglia" - launch the cluster with the Ganglia Monitoring System installed.
    newSupportedProducts :: Core.Maybe [Types.SupportedProductConfig],
    -- | The specified placement group configuration for an Amazon EMR cluster.
    placementGroupConfigs :: Core.Maybe [Types.PlacementGroupConfig],
    -- | The Amazon EMR release label, which determines the version of open-source application packages installed on the cluster. Release labels are in the form @emr-x.x.x@ , where x.x.x is an Amazon EMR release version such as @emr-5.14.0@ . For more information about Amazon EMR release versions and included application versions and features, see <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/ https://docs.aws.amazon.com/emr/latest/ReleaseGuide/> . The release label applies only to Amazon EMR releases version 4.0 and later. Earlier versions use @AmiVersion@ .
    releaseLabel :: Core.Maybe Types.XmlStringMaxLen256,
    -- | Applies only when @CustomAmiID@ is used. Specifies which updates from the Amazon Linux AMI package repositories to apply automatically when the instance boots using the AMI. If omitted, the default is @SECURITY@ , which indicates that only security updates are applied. If @NONE@ is specified, no updates are applied, and all updates must be applied manually.
    repoUpgradeOnBoot :: Core.Maybe Types.RepoUpgradeOnBoot,
    -- | Specifies the way that individual Amazon EC2 instances terminate when an automatic scale-in activity occurs or an instance group is resized. @TERMINATE_AT_INSTANCE_HOUR@ indicates that Amazon EMR terminates nodes at the instance-hour boundary, regardless of when the request to terminate the instance was submitted. This option is only available with Amazon EMR 5.1.0 and later and is the default for clusters created using that version. @TERMINATE_AT_TASK_COMPLETION@ indicates that Amazon EMR adds nodes to a deny list and drains tasks from nodes before terminating the Amazon EC2 instances, regardless of the instance-hour boundary. With either behavior, Amazon EMR removes the least active nodes first and blocks instance termination if it could lead to HDFS corruption. @TERMINATE_AT_TASK_COMPLETION@ available only in Amazon EMR version 4.1.0 and later, and is the default for versions of Amazon EMR earlier than 5.1.0.
    scaleDownBehavior :: Core.Maybe Types.ScaleDownBehavior,
    -- | The name of a security configuration to apply to the cluster.
    securityConfiguration :: Core.Maybe Types.XmlString,
    -- | The IAM role that will be assumed by the Amazon EMR service to access AWS resources on your behalf.
    serviceRole :: Core.Maybe Types.XmlString,
    -- | Specifies the number of steps that can be executed concurrently. The default value is @1@ . The maximum value is @256@ .
    stepConcurrencyLevel :: Core.Maybe Core.Int,
    -- | A list of steps to run.
    steps :: Core.Maybe [Types.StepConfig],
    -- | A list of strings that indicates third-party software to use. For more information, see the <https://docs.aws.amazon.com/emr/latest/DeveloperGuide/emr-dg.pdf Amazon EMR Developer Guide> . Currently supported values are:
    --
    --
    --     * "mapr-m3" - launch the job flow using MapR M3 Edition.
    --
    --
    --     * "mapr-m5" - launch the job flow using MapR M5 Edition.
    supportedProducts :: Core.Maybe [Types.XmlStringMaxLen256],
    -- | A list of tags to associate with a cluster and propagate to Amazon EC2 instances.
    tags :: Core.Maybe [Types.Tag],
    -- | A value of @true@ indicates that all IAM users in the AWS account can perform cluster actions if they have the proper IAM policy permissions. This is the default. A value of @false@ indicates that only the IAM user who created the cluster can perform actions.
    visibleToAllUsers :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RunJobFlow' value with any optional fields omitted.
mkRunJobFlow ::
  -- | 'name'
  Types.XmlStringMaxLen256 ->
  -- | 'instances'
  Types.JobFlowInstancesConfig ->
  RunJobFlow
mkRunJobFlow name instances =
  RunJobFlow'
    { name,
      instances,
      additionalInfo = Core.Nothing,
      amiVersion = Core.Nothing,
      applications = Core.Nothing,
      autoScalingRole = Core.Nothing,
      bootstrapActions = Core.Nothing,
      configurations = Core.Nothing,
      customAmiId = Core.Nothing,
      ebsRootVolumeSize = Core.Nothing,
      jobFlowRole = Core.Nothing,
      kerberosAttributes = Core.Nothing,
      logEncryptionKmsKeyId = Core.Nothing,
      logUri = Core.Nothing,
      managedScalingPolicy = Core.Nothing,
      newSupportedProducts = Core.Nothing,
      placementGroupConfigs = Core.Nothing,
      releaseLabel = Core.Nothing,
      repoUpgradeOnBoot = Core.Nothing,
      scaleDownBehavior = Core.Nothing,
      securityConfiguration = Core.Nothing,
      serviceRole = Core.Nothing,
      stepConcurrencyLevel = Core.Nothing,
      steps = Core.Nothing,
      supportedProducts = Core.Nothing,
      tags = Core.Nothing,
      visibleToAllUsers = Core.Nothing
    }

-- | The name of the job flow.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfName :: Lens.Lens' RunJobFlow Types.XmlStringMaxLen256
rjfName = Lens.field @"name"
{-# DEPRECATED rjfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A specification of the number and type of Amazon EC2 instances.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfInstances :: Lens.Lens' RunJobFlow Types.JobFlowInstancesConfig
rjfInstances = Lens.field @"instances"
{-# DEPRECATED rjfInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

-- | A JSON string for selecting additional features.
--
-- /Note:/ Consider using 'additionalInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfAdditionalInfo :: Lens.Lens' RunJobFlow (Core.Maybe Types.XmlString)
rjfAdditionalInfo = Lens.field @"additionalInfo"
{-# DEPRECATED rjfAdditionalInfo "Use generic-lens or generic-optics with 'additionalInfo' instead." #-}

-- | Applies only to Amazon EMR AMI versions 3.x and 2.x. For Amazon EMR releases 4.0 and later, @ReleaseLabel@ is used. To specify a custom AMI, use @CustomAmiID@ .
--
-- /Note:/ Consider using 'amiVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfAmiVersion :: Lens.Lens' RunJobFlow (Core.Maybe Types.XmlStringMaxLen256)
rjfAmiVersion = Lens.field @"amiVersion"
{-# DEPRECATED rjfAmiVersion "Use generic-lens or generic-optics with 'amiVersion' instead." #-}

-- | Applies to Amazon EMR releases 4.0 and later. A case-insensitive list of applications for Amazon EMR to install and configure when launching the cluster. For a list of applications available for each Amazon EMR release version, see the <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/ Amazon EMR Release Guide> .
--
-- /Note:/ Consider using 'applications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfApplications :: Lens.Lens' RunJobFlow (Core.Maybe [Types.Application])
rjfApplications = Lens.field @"applications"
{-# DEPRECATED rjfApplications "Use generic-lens or generic-optics with 'applications' instead." #-}

-- | An IAM role for automatic scaling policies. The default role is @EMR_AutoScaling_DefaultRole@ . The IAM role provides permissions that the automatic scaling feature requires to launch and terminate EC2 instances in an instance group.
--
-- /Note:/ Consider using 'autoScalingRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfAutoScalingRole :: Lens.Lens' RunJobFlow (Core.Maybe Types.XmlString)
rjfAutoScalingRole = Lens.field @"autoScalingRole"
{-# DEPRECATED rjfAutoScalingRole "Use generic-lens or generic-optics with 'autoScalingRole' instead." #-}

-- | A list of bootstrap actions to run before Hadoop starts on the cluster nodes.
--
-- /Note:/ Consider using 'bootstrapActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfBootstrapActions :: Lens.Lens' RunJobFlow (Core.Maybe [Types.BootstrapActionConfig])
rjfBootstrapActions = Lens.field @"bootstrapActions"
{-# DEPRECATED rjfBootstrapActions "Use generic-lens or generic-optics with 'bootstrapActions' instead." #-}

-- | For Amazon EMR releases 4.0 and later. The list of configurations supplied for the EMR cluster you are creating.
--
-- /Note:/ Consider using 'configurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfConfigurations :: Lens.Lens' RunJobFlow (Core.Maybe [Types.Configuration])
rjfConfigurations = Lens.field @"configurations"
{-# DEPRECATED rjfConfigurations "Use generic-lens or generic-optics with 'configurations' instead." #-}

-- | Available only in Amazon EMR version 5.7.0 and later. The ID of a custom Amazon EBS-backed Linux AMI. If specified, Amazon EMR uses this AMI when it launches cluster EC2 instances. For more information about custom AMIs in Amazon EMR, see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-custom-ami.html Using a Custom AMI> in the /Amazon EMR Management Guide/ . If omitted, the cluster uses the base Linux AMI for the @ReleaseLabel@ specified. For Amazon EMR versions 2.x and 3.x, use @AmiVersion@ instead.
--
-- For information about creating a custom AMI, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/creating-an-ami-ebs.html Creating an Amazon EBS-Backed Linux AMI> in the /Amazon Elastic Compute Cloud User Guide for Linux Instances/ . For information about finding an AMI ID, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html Finding a Linux AMI> .
--
-- /Note:/ Consider using 'customAmiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfCustomAmiId :: Lens.Lens' RunJobFlow (Core.Maybe Types.XmlStringMaxLen256)
rjfCustomAmiId = Lens.field @"customAmiId"
{-# DEPRECATED rjfCustomAmiId "Use generic-lens or generic-optics with 'customAmiId' instead." #-}

-- | The size, in GiB, of the Amazon EBS root device volume of the Linux AMI that is used for each EC2 instance. Available in Amazon EMR version 4.x and later.
--
-- /Note:/ Consider using 'ebsRootVolumeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfEbsRootVolumeSize :: Lens.Lens' RunJobFlow (Core.Maybe Core.Int)
rjfEbsRootVolumeSize = Lens.field @"ebsRootVolumeSize"
{-# DEPRECATED rjfEbsRootVolumeSize "Use generic-lens or generic-optics with 'ebsRootVolumeSize' instead." #-}

-- | Also called instance profile and EC2 role. An IAM role for an EMR cluster. The EC2 instances of the cluster assume this role. The default role is @EMR_EC2_DefaultRole@ . In order to use the default role, you must have already created it using the CLI or console.
--
-- /Note:/ Consider using 'jobFlowRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfJobFlowRole :: Lens.Lens' RunJobFlow (Core.Maybe Types.XmlString)
rjfJobFlowRole = Lens.field @"jobFlowRole"
{-# DEPRECATED rjfJobFlowRole "Use generic-lens or generic-optics with 'jobFlowRole' instead." #-}

-- | Attributes for Kerberos configuration when Kerberos authentication is enabled using a security configuration. For more information see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-kerberos.html Use Kerberos Authentication> in the /Amazon EMR Management Guide/ .
--
-- /Note:/ Consider using 'kerberosAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfKerberosAttributes :: Lens.Lens' RunJobFlow (Core.Maybe Types.KerberosAttributes)
rjfKerberosAttributes = Lens.field @"kerberosAttributes"
{-# DEPRECATED rjfKerberosAttributes "Use generic-lens or generic-optics with 'kerberosAttributes' instead." #-}

-- | The AWS KMS customer master key (CMK) used for encrypting log files. If a value is not provided, the logs remain encrypted by AES-256. This attribute is only available with Amazon EMR version 5.30.0 and later, excluding Amazon EMR 6.0.0.
--
-- /Note:/ Consider using 'logEncryptionKmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfLogEncryptionKmsKeyId :: Lens.Lens' RunJobFlow (Core.Maybe Types.XmlString)
rjfLogEncryptionKmsKeyId = Lens.field @"logEncryptionKmsKeyId"
{-# DEPRECATED rjfLogEncryptionKmsKeyId "Use generic-lens or generic-optics with 'logEncryptionKmsKeyId' instead." #-}

-- | The location in Amazon S3 to write the log files of the job flow. If a value is not provided, logs are not created.
--
-- /Note:/ Consider using 'logUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfLogUri :: Lens.Lens' RunJobFlow (Core.Maybe Types.XmlString)
rjfLogUri = Lens.field @"logUri"
{-# DEPRECATED rjfLogUri "Use generic-lens or generic-optics with 'logUri' instead." #-}

-- | The specified managed scaling policy for an Amazon EMR cluster.
--
-- /Note:/ Consider using 'managedScalingPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfManagedScalingPolicy :: Lens.Lens' RunJobFlow (Core.Maybe Types.ManagedScalingPolicy)
rjfManagedScalingPolicy = Lens.field @"managedScalingPolicy"
{-# DEPRECATED rjfManagedScalingPolicy "Use generic-lens or generic-optics with 'managedScalingPolicy' instead." #-}

-- | A list of strings that indicates third-party software to use with the job flow that accepts a user argument list. EMR accepts and forwards the argument list to the corresponding installation script as bootstrap action arguments. For more information, see "Launch a Job Flow on the MapR Distribution for Hadoop" in the <https://docs.aws.amazon.com/emr/latest/DeveloperGuide/emr-dg.pdf Amazon EMR Developer Guide> . Supported values are:
--
--
--     * "mapr-m3" - launch the cluster using MapR M3 Edition.
--
--
--     * "mapr-m5" - launch the cluster using MapR M5 Edition.
--
--
--     * "mapr" with the user arguments specifying "--edition,m3" or "--edition,m5" - launch the job flow using MapR M3 or M5 Edition respectively.
--
--
--     * "mapr-m7" - launch the cluster using MapR M7 Edition.
--
--
--     * "hunk" - launch the cluster with the Hunk Big Data Analtics Platform.
--
--
--     * "hue"- launch the cluster with Hue installed.
--
--
--     * "spark" - launch the cluster with Apache Spark installed.
--
--
--     * "ganglia" - launch the cluster with the Ganglia Monitoring System installed.
--
--
--
-- /Note:/ Consider using 'newSupportedProducts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfNewSupportedProducts :: Lens.Lens' RunJobFlow (Core.Maybe [Types.SupportedProductConfig])
rjfNewSupportedProducts = Lens.field @"newSupportedProducts"
{-# DEPRECATED rjfNewSupportedProducts "Use generic-lens or generic-optics with 'newSupportedProducts' instead." #-}

-- | The specified placement group configuration for an Amazon EMR cluster.
--
-- /Note:/ Consider using 'placementGroupConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfPlacementGroupConfigs :: Lens.Lens' RunJobFlow (Core.Maybe [Types.PlacementGroupConfig])
rjfPlacementGroupConfigs = Lens.field @"placementGroupConfigs"
{-# DEPRECATED rjfPlacementGroupConfigs "Use generic-lens or generic-optics with 'placementGroupConfigs' instead." #-}

-- | The Amazon EMR release label, which determines the version of open-source application packages installed on the cluster. Release labels are in the form @emr-x.x.x@ , where x.x.x is an Amazon EMR release version such as @emr-5.14.0@ . For more information about Amazon EMR release versions and included application versions and features, see <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/ https://docs.aws.amazon.com/emr/latest/ReleaseGuide/> . The release label applies only to Amazon EMR releases version 4.0 and later. Earlier versions use @AmiVersion@ .
--
-- /Note:/ Consider using 'releaseLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfReleaseLabel :: Lens.Lens' RunJobFlow (Core.Maybe Types.XmlStringMaxLen256)
rjfReleaseLabel = Lens.field @"releaseLabel"
{-# DEPRECATED rjfReleaseLabel "Use generic-lens or generic-optics with 'releaseLabel' instead." #-}

-- | Applies only when @CustomAmiID@ is used. Specifies which updates from the Amazon Linux AMI package repositories to apply automatically when the instance boots using the AMI. If omitted, the default is @SECURITY@ , which indicates that only security updates are applied. If @NONE@ is specified, no updates are applied, and all updates must be applied manually.
--
-- /Note:/ Consider using 'repoUpgradeOnBoot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfRepoUpgradeOnBoot :: Lens.Lens' RunJobFlow (Core.Maybe Types.RepoUpgradeOnBoot)
rjfRepoUpgradeOnBoot = Lens.field @"repoUpgradeOnBoot"
{-# DEPRECATED rjfRepoUpgradeOnBoot "Use generic-lens or generic-optics with 'repoUpgradeOnBoot' instead." #-}

-- | Specifies the way that individual Amazon EC2 instances terminate when an automatic scale-in activity occurs or an instance group is resized. @TERMINATE_AT_INSTANCE_HOUR@ indicates that Amazon EMR terminates nodes at the instance-hour boundary, regardless of when the request to terminate the instance was submitted. This option is only available with Amazon EMR 5.1.0 and later and is the default for clusters created using that version. @TERMINATE_AT_TASK_COMPLETION@ indicates that Amazon EMR adds nodes to a deny list and drains tasks from nodes before terminating the Amazon EC2 instances, regardless of the instance-hour boundary. With either behavior, Amazon EMR removes the least active nodes first and blocks instance termination if it could lead to HDFS corruption. @TERMINATE_AT_TASK_COMPLETION@ available only in Amazon EMR version 4.1.0 and later, and is the default for versions of Amazon EMR earlier than 5.1.0.
--
-- /Note:/ Consider using 'scaleDownBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfScaleDownBehavior :: Lens.Lens' RunJobFlow (Core.Maybe Types.ScaleDownBehavior)
rjfScaleDownBehavior = Lens.field @"scaleDownBehavior"
{-# DEPRECATED rjfScaleDownBehavior "Use generic-lens or generic-optics with 'scaleDownBehavior' instead." #-}

-- | The name of a security configuration to apply to the cluster.
--
-- /Note:/ Consider using 'securityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfSecurityConfiguration :: Lens.Lens' RunJobFlow (Core.Maybe Types.XmlString)
rjfSecurityConfiguration = Lens.field @"securityConfiguration"
{-# DEPRECATED rjfSecurityConfiguration "Use generic-lens or generic-optics with 'securityConfiguration' instead." #-}

-- | The IAM role that will be assumed by the Amazon EMR service to access AWS resources on your behalf.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfServiceRole :: Lens.Lens' RunJobFlow (Core.Maybe Types.XmlString)
rjfServiceRole = Lens.field @"serviceRole"
{-# DEPRECATED rjfServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

-- | Specifies the number of steps that can be executed concurrently. The default value is @1@ . The maximum value is @256@ .
--
-- /Note:/ Consider using 'stepConcurrencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfStepConcurrencyLevel :: Lens.Lens' RunJobFlow (Core.Maybe Core.Int)
rjfStepConcurrencyLevel = Lens.field @"stepConcurrencyLevel"
{-# DEPRECATED rjfStepConcurrencyLevel "Use generic-lens or generic-optics with 'stepConcurrencyLevel' instead." #-}

-- | A list of steps to run.
--
-- /Note:/ Consider using 'steps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfSteps :: Lens.Lens' RunJobFlow (Core.Maybe [Types.StepConfig])
rjfSteps = Lens.field @"steps"
{-# DEPRECATED rjfSteps "Use generic-lens or generic-optics with 'steps' instead." #-}

-- | A list of strings that indicates third-party software to use. For more information, see the <https://docs.aws.amazon.com/emr/latest/DeveloperGuide/emr-dg.pdf Amazon EMR Developer Guide> . Currently supported values are:
--
--
--     * "mapr-m3" - launch the job flow using MapR M3 Edition.
--
--
--     * "mapr-m5" - launch the job flow using MapR M5 Edition.
--
--
--
-- /Note:/ Consider using 'supportedProducts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfSupportedProducts :: Lens.Lens' RunJobFlow (Core.Maybe [Types.XmlStringMaxLen256])
rjfSupportedProducts = Lens.field @"supportedProducts"
{-# DEPRECATED rjfSupportedProducts "Use generic-lens or generic-optics with 'supportedProducts' instead." #-}

-- | A list of tags to associate with a cluster and propagate to Amazon EC2 instances.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfTags :: Lens.Lens' RunJobFlow (Core.Maybe [Types.Tag])
rjfTags = Lens.field @"tags"
{-# DEPRECATED rjfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A value of @true@ indicates that all IAM users in the AWS account can perform cluster actions if they have the proper IAM policy permissions. This is the default. A value of @false@ indicates that only the IAM user who created the cluster can perform actions.
--
-- /Note:/ Consider using 'visibleToAllUsers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfVisibleToAllUsers :: Lens.Lens' RunJobFlow (Core.Maybe Core.Bool)
rjfVisibleToAllUsers = Lens.field @"visibleToAllUsers"
{-# DEPRECATED rjfVisibleToAllUsers "Use generic-lens or generic-optics with 'visibleToAllUsers' instead." #-}

instance Core.FromJSON RunJobFlow where
  toJSON RunJobFlow {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("Instances" Core..= instances),
            ("AdditionalInfo" Core..=) Core.<$> additionalInfo,
            ("AmiVersion" Core..=) Core.<$> amiVersion,
            ("Applications" Core..=) Core.<$> applications,
            ("AutoScalingRole" Core..=) Core.<$> autoScalingRole,
            ("BootstrapActions" Core..=) Core.<$> bootstrapActions,
            ("Configurations" Core..=) Core.<$> configurations,
            ("CustomAmiId" Core..=) Core.<$> customAmiId,
            ("EbsRootVolumeSize" Core..=) Core.<$> ebsRootVolumeSize,
            ("JobFlowRole" Core..=) Core.<$> jobFlowRole,
            ("KerberosAttributes" Core..=) Core.<$> kerberosAttributes,
            ("LogEncryptionKmsKeyId" Core..=) Core.<$> logEncryptionKmsKeyId,
            ("LogUri" Core..=) Core.<$> logUri,
            ("ManagedScalingPolicy" Core..=) Core.<$> managedScalingPolicy,
            ("NewSupportedProducts" Core..=) Core.<$> newSupportedProducts,
            ("PlacementGroupConfigs" Core..=) Core.<$> placementGroupConfigs,
            ("ReleaseLabel" Core..=) Core.<$> releaseLabel,
            ("RepoUpgradeOnBoot" Core..=) Core.<$> repoUpgradeOnBoot,
            ("ScaleDownBehavior" Core..=) Core.<$> scaleDownBehavior,
            ("SecurityConfiguration" Core..=) Core.<$> securityConfiguration,
            ("ServiceRole" Core..=) Core.<$> serviceRole,
            ("StepConcurrencyLevel" Core..=) Core.<$> stepConcurrencyLevel,
            ("Steps" Core..=) Core.<$> steps,
            ("SupportedProducts" Core..=) Core.<$> supportedProducts,
            ("Tags" Core..=) Core.<$> tags,
            ("VisibleToAllUsers" Core..=) Core.<$> visibleToAllUsers
          ]
      )

instance Core.AWSRequest RunJobFlow where
  type Rs RunJobFlow = RunJobFlowResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "ElasticMapReduce.RunJobFlow")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RunJobFlowResponse'
            Core.<$> (x Core..:? "ClusterArn")
            Core.<*> (x Core..:? "JobFlowId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The result of the 'RunJobFlow' operation.
--
-- /See:/ 'mkRunJobFlowResponse' smart constructor.
data RunJobFlowResponse = RunJobFlowResponse'
  { -- | The Amazon Resource Name of the cluster.
    clusterArn :: Core.Maybe Types.ArnType,
    -- | An unique identifier for the job flow.
    jobFlowId :: Core.Maybe Types.XmlStringMaxLen256,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RunJobFlowResponse' value with any optional fields omitted.
mkRunJobFlowResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RunJobFlowResponse
mkRunJobFlowResponse responseStatus =
  RunJobFlowResponse'
    { clusterArn = Core.Nothing,
      jobFlowId = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name of the cluster.
--
-- /Note:/ Consider using 'clusterArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfrrsClusterArn :: Lens.Lens' RunJobFlowResponse (Core.Maybe Types.ArnType)
rjfrrsClusterArn = Lens.field @"clusterArn"
{-# DEPRECATED rjfrrsClusterArn "Use generic-lens or generic-optics with 'clusterArn' instead." #-}

-- | An unique identifier for the job flow.
--
-- /Note:/ Consider using 'jobFlowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfrrsJobFlowId :: Lens.Lens' RunJobFlowResponse (Core.Maybe Types.XmlStringMaxLen256)
rjfrrsJobFlowId = Lens.field @"jobFlowId"
{-# DEPRECATED rjfrrsJobFlowId "Use generic-lens or generic-optics with 'jobFlowId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfrrsResponseStatus :: Lens.Lens' RunJobFlowResponse Core.Int
rjfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rjfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
