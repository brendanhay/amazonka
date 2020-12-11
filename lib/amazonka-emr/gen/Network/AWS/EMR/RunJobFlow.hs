{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    rjfLogEncryptionKMSKeyId,
    rjfAMIVersion,
    rjfEBSRootVolumeSize,
    rjfAdditionalInfo,
    rjfConfigurations,
    rjfCustomAMIId,
    rjfAutoScalingRole,
    rjfSecurityConfiguration,
    rjfScaleDownBehavior,
    rjfSteps,
    rjfJobFlowRole,
    rjfBootstrapActions,
    rjfReleaseLabel,
    rjfRepoUpgradeOnBoot,
    rjfPlacementGroupConfigs,
    rjfLogURI,
    rjfKerberosAttributes,
    rjfNewSupportedProducts,
    rjfManagedScalingPolicy,
    rjfVisibleToAllUsers,
    rjfSupportedProducts,
    rjfStepConcurrencyLevel,
    rjfApplications,
    rjfTags,
    rjfServiceRole,
    rjfName,
    rjfInstances,

    -- * Destructuring the response
    RunJobFlowResponse (..),
    mkRunJobFlowResponse,

    -- ** Response lenses
    rjfrsClusterARN,
    rjfrsJobFlowId,
    rjfrsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Input to the 'RunJobFlow' operation.
--
-- /See:/ 'mkRunJobFlow' smart constructor.
data RunJobFlow = RunJobFlow'
  { logEncryptionKMSKeyId ::
      Lude.Maybe Lude.Text,
    amiVersion :: Lude.Maybe Lude.Text,
    ebsRootVolumeSize :: Lude.Maybe Lude.Int,
    additionalInfo :: Lude.Maybe Lude.Text,
    configurations :: Lude.Maybe [Configuration],
    customAMIId :: Lude.Maybe Lude.Text,
    autoScalingRole :: Lude.Maybe Lude.Text,
    securityConfiguration :: Lude.Maybe Lude.Text,
    scaleDownBehavior :: Lude.Maybe ScaleDownBehavior,
    steps :: Lude.Maybe [StepConfig],
    jobFlowRole :: Lude.Maybe Lude.Text,
    bootstrapActions :: Lude.Maybe [BootstrapActionConfig],
    releaseLabel :: Lude.Maybe Lude.Text,
    repoUpgradeOnBoot :: Lude.Maybe RepoUpgradeOnBoot,
    placementGroupConfigs :: Lude.Maybe [PlacementGroupConfig],
    logURI :: Lude.Maybe Lude.Text,
    kerberosAttributes :: Lude.Maybe KerberosAttributes,
    newSupportedProducts :: Lude.Maybe [SupportedProductConfig],
    managedScalingPolicy :: Lude.Maybe ManagedScalingPolicy,
    visibleToAllUsers :: Lude.Maybe Lude.Bool,
    supportedProducts :: Lude.Maybe [Lude.Text],
    stepConcurrencyLevel :: Lude.Maybe Lude.Int,
    applications :: Lude.Maybe [Application],
    tags :: Lude.Maybe [Tag],
    serviceRole :: Lude.Maybe Lude.Text,
    name :: Lude.Text,
    instances :: JobFlowInstancesConfig
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RunJobFlow' with the minimum fields required to make a request.
--
-- * 'additionalInfo' - A JSON string for selecting additional features.
-- * 'amiVersion' - Applies only to Amazon EMR AMI versions 3.x and 2.x. For Amazon EMR releases 4.0 and later, @ReleaseLabel@ is used. To specify a custom AMI, use @CustomAmiID@ .
-- * 'applications' - Applies to Amazon EMR releases 4.0 and later. A case-insensitive list of applications for Amazon EMR to install and configure when launching the cluster. For a list of applications available for each Amazon EMR release version, see the <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/ Amazon EMR Release Guide> .
-- * 'autoScalingRole' - An IAM role for automatic scaling policies. The default role is @EMR_AutoScaling_DefaultRole@ . The IAM role provides permissions that the automatic scaling feature requires to launch and terminate EC2 instances in an instance group.
-- * 'bootstrapActions' - A list of bootstrap actions to run before Hadoop starts on the cluster nodes.
-- * 'configurations' - For Amazon EMR releases 4.0 and later. The list of configurations supplied for the EMR cluster you are creating.
-- * 'customAMIId' - Available only in Amazon EMR version 5.7.0 and later. The ID of a custom Amazon EBS-backed Linux AMI. If specified, Amazon EMR uses this AMI when it launches cluster EC2 instances. For more information about custom AMIs in Amazon EMR, see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-custom-ami.html Using a Custom AMI> in the /Amazon EMR Management Guide/ . If omitted, the cluster uses the base Linux AMI for the @ReleaseLabel@ specified. For Amazon EMR versions 2.x and 3.x, use @AmiVersion@ instead.
--
-- For information about creating a custom AMI, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/creating-an-ami-ebs.html Creating an Amazon EBS-Backed Linux AMI> in the /Amazon Elastic Compute Cloud User Guide for Linux Instances/ . For information about finding an AMI ID, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html Finding a Linux AMI> .
-- * 'ebsRootVolumeSize' - The size, in GiB, of the Amazon EBS root device volume of the Linux AMI that is used for each EC2 instance. Available in Amazon EMR version 4.x and later.
-- * 'instances' - A specification of the number and type of Amazon EC2 instances.
-- * 'jobFlowRole' - Also called instance profile and EC2 role. An IAM role for an EMR cluster. The EC2 instances of the cluster assume this role. The default role is @EMR_EC2_DefaultRole@ . In order to use the default role, you must have already created it using the CLI or console.
-- * 'kerberosAttributes' - Attributes for Kerberos configuration when Kerberos authentication is enabled using a security configuration. For more information see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-kerberos.html Use Kerberos Authentication> in the /Amazon EMR Management Guide/ .
-- * 'logEncryptionKMSKeyId' - The AWS KMS customer master key (CMK) used for encrypting log files. If a value is not provided, the logs remain encrypted by AES-256. This attribute is only available with Amazon EMR version 5.30.0 and later, excluding Amazon EMR 6.0.0.
-- * 'logURI' - The location in Amazon S3 to write the log files of the job flow. If a value is not provided, logs are not created.
-- * 'managedScalingPolicy' - The specified managed scaling policy for an Amazon EMR cluster.
-- * 'name' - The name of the job flow.
-- * 'newSupportedProducts' - A list of strings that indicates third-party software to use with the job flow that accepts a user argument list. EMR accepts and forwards the argument list to the corresponding installation script as bootstrap action arguments. For more information, see "Launch a Job Flow on the MapR Distribution for Hadoop" in the <https://docs.aws.amazon.com/emr/latest/DeveloperGuide/emr-dg.pdf Amazon EMR Developer Guide> . Supported values are:
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
-- * 'placementGroupConfigs' - The specified placement group configuration for an Amazon EMR cluster.
-- * 'releaseLabel' - The Amazon EMR release label, which determines the version of open-source application packages installed on the cluster. Release labels are in the form @emr-x.x.x@ , where x.x.x is an Amazon EMR release version such as @emr-5.14.0@ . For more information about Amazon EMR release versions and included application versions and features, see <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/ https://docs.aws.amazon.com/emr/latest/ReleaseGuide/> . The release label applies only to Amazon EMR releases version 4.0 and later. Earlier versions use @AmiVersion@ .
-- * 'repoUpgradeOnBoot' - Applies only when @CustomAmiID@ is used. Specifies which updates from the Amazon Linux AMI package repositories to apply automatically when the instance boots using the AMI. If omitted, the default is @SECURITY@ , which indicates that only security updates are applied. If @NONE@ is specified, no updates are applied, and all updates must be applied manually.
-- * 'scaleDownBehavior' - Specifies the way that individual Amazon EC2 instances terminate when an automatic scale-in activity occurs or an instance group is resized. @TERMINATE_AT_INSTANCE_HOUR@ indicates that Amazon EMR terminates nodes at the instance-hour boundary, regardless of when the request to terminate the instance was submitted. This option is only available with Amazon EMR 5.1.0 and later and is the default for clusters created using that version. @TERMINATE_AT_TASK_COMPLETION@ indicates that Amazon EMR adds nodes to a deny list and drains tasks from nodes before terminating the Amazon EC2 instances, regardless of the instance-hour boundary. With either behavior, Amazon EMR removes the least active nodes first and blocks instance termination if it could lead to HDFS corruption. @TERMINATE_AT_TASK_COMPLETION@ available only in Amazon EMR version 4.1.0 and later, and is the default for versions of Amazon EMR earlier than 5.1.0.
-- * 'securityConfiguration' - The name of a security configuration to apply to the cluster.
-- * 'serviceRole' - The IAM role that will be assumed by the Amazon EMR service to access AWS resources on your behalf.
-- * 'stepConcurrencyLevel' - Specifies the number of steps that can be executed concurrently. The default value is @1@ . The maximum value is @256@ .
-- * 'steps' - A list of steps to run.
-- * 'supportedProducts' - A list of strings that indicates third-party software to use. For more information, see the <https://docs.aws.amazon.com/emr/latest/DeveloperGuide/emr-dg.pdf Amazon EMR Developer Guide> . Currently supported values are:
--
--
--     * "mapr-m3" - launch the job flow using MapR M3 Edition.
--
--
--     * "mapr-m5" - launch the job flow using MapR M5 Edition.
--
--
-- * 'tags' - A list of tags to associate with a cluster and propagate to Amazon EC2 instances.
-- * 'visibleToAllUsers' - A value of @true@ indicates that all IAM users in the AWS account can perform cluster actions if they have the proper IAM policy permissions. This is the default. A value of @false@ indicates that only the IAM user who created the cluster can perform actions.
mkRunJobFlow ::
  -- | 'name'
  Lude.Text ->
  -- | 'instances'
  JobFlowInstancesConfig ->
  RunJobFlow
mkRunJobFlow pName_ pInstances_ =
  RunJobFlow'
    { logEncryptionKMSKeyId = Lude.Nothing,
      amiVersion = Lude.Nothing,
      ebsRootVolumeSize = Lude.Nothing,
      additionalInfo = Lude.Nothing,
      configurations = Lude.Nothing,
      customAMIId = Lude.Nothing,
      autoScalingRole = Lude.Nothing,
      securityConfiguration = Lude.Nothing,
      scaleDownBehavior = Lude.Nothing,
      steps = Lude.Nothing,
      jobFlowRole = Lude.Nothing,
      bootstrapActions = Lude.Nothing,
      releaseLabel = Lude.Nothing,
      repoUpgradeOnBoot = Lude.Nothing,
      placementGroupConfigs = Lude.Nothing,
      logURI = Lude.Nothing,
      kerberosAttributes = Lude.Nothing,
      newSupportedProducts = Lude.Nothing,
      managedScalingPolicy = Lude.Nothing,
      visibleToAllUsers = Lude.Nothing,
      supportedProducts = Lude.Nothing,
      stepConcurrencyLevel = Lude.Nothing,
      applications = Lude.Nothing,
      tags = Lude.Nothing,
      serviceRole = Lude.Nothing,
      name = pName_,
      instances = pInstances_
    }

-- | The AWS KMS customer master key (CMK) used for encrypting log files. If a value is not provided, the logs remain encrypted by AES-256. This attribute is only available with Amazon EMR version 5.30.0 and later, excluding Amazon EMR 6.0.0.
--
-- /Note:/ Consider using 'logEncryptionKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfLogEncryptionKMSKeyId :: Lens.Lens' RunJobFlow (Lude.Maybe Lude.Text)
rjfLogEncryptionKMSKeyId = Lens.lens (logEncryptionKMSKeyId :: RunJobFlow -> Lude.Maybe Lude.Text) (\s a -> s {logEncryptionKMSKeyId = a} :: RunJobFlow)
{-# DEPRECATED rjfLogEncryptionKMSKeyId "Use generic-lens or generic-optics with 'logEncryptionKMSKeyId' instead." #-}

-- | Applies only to Amazon EMR AMI versions 3.x and 2.x. For Amazon EMR releases 4.0 and later, @ReleaseLabel@ is used. To specify a custom AMI, use @CustomAmiID@ .
--
-- /Note:/ Consider using 'amiVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfAMIVersion :: Lens.Lens' RunJobFlow (Lude.Maybe Lude.Text)
rjfAMIVersion = Lens.lens (amiVersion :: RunJobFlow -> Lude.Maybe Lude.Text) (\s a -> s {amiVersion = a} :: RunJobFlow)
{-# DEPRECATED rjfAMIVersion "Use generic-lens or generic-optics with 'amiVersion' instead." #-}

-- | The size, in GiB, of the Amazon EBS root device volume of the Linux AMI that is used for each EC2 instance. Available in Amazon EMR version 4.x and later.
--
-- /Note:/ Consider using 'ebsRootVolumeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfEBSRootVolumeSize :: Lens.Lens' RunJobFlow (Lude.Maybe Lude.Int)
rjfEBSRootVolumeSize = Lens.lens (ebsRootVolumeSize :: RunJobFlow -> Lude.Maybe Lude.Int) (\s a -> s {ebsRootVolumeSize = a} :: RunJobFlow)
{-# DEPRECATED rjfEBSRootVolumeSize "Use generic-lens or generic-optics with 'ebsRootVolumeSize' instead." #-}

-- | A JSON string for selecting additional features.
--
-- /Note:/ Consider using 'additionalInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfAdditionalInfo :: Lens.Lens' RunJobFlow (Lude.Maybe Lude.Text)
rjfAdditionalInfo = Lens.lens (additionalInfo :: RunJobFlow -> Lude.Maybe Lude.Text) (\s a -> s {additionalInfo = a} :: RunJobFlow)
{-# DEPRECATED rjfAdditionalInfo "Use generic-lens or generic-optics with 'additionalInfo' instead." #-}

-- | For Amazon EMR releases 4.0 and later. The list of configurations supplied for the EMR cluster you are creating.
--
-- /Note:/ Consider using 'configurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfConfigurations :: Lens.Lens' RunJobFlow (Lude.Maybe [Configuration])
rjfConfigurations = Lens.lens (configurations :: RunJobFlow -> Lude.Maybe [Configuration]) (\s a -> s {configurations = a} :: RunJobFlow)
{-# DEPRECATED rjfConfigurations "Use generic-lens or generic-optics with 'configurations' instead." #-}

-- | Available only in Amazon EMR version 5.7.0 and later. The ID of a custom Amazon EBS-backed Linux AMI. If specified, Amazon EMR uses this AMI when it launches cluster EC2 instances. For more information about custom AMIs in Amazon EMR, see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-custom-ami.html Using a Custom AMI> in the /Amazon EMR Management Guide/ . If omitted, the cluster uses the base Linux AMI for the @ReleaseLabel@ specified. For Amazon EMR versions 2.x and 3.x, use @AmiVersion@ instead.
--
-- For information about creating a custom AMI, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/creating-an-ami-ebs.html Creating an Amazon EBS-Backed Linux AMI> in the /Amazon Elastic Compute Cloud User Guide for Linux Instances/ . For information about finding an AMI ID, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html Finding a Linux AMI> .
--
-- /Note:/ Consider using 'customAMIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfCustomAMIId :: Lens.Lens' RunJobFlow (Lude.Maybe Lude.Text)
rjfCustomAMIId = Lens.lens (customAMIId :: RunJobFlow -> Lude.Maybe Lude.Text) (\s a -> s {customAMIId = a} :: RunJobFlow)
{-# DEPRECATED rjfCustomAMIId "Use generic-lens or generic-optics with 'customAMIId' instead." #-}

-- | An IAM role for automatic scaling policies. The default role is @EMR_AutoScaling_DefaultRole@ . The IAM role provides permissions that the automatic scaling feature requires to launch and terminate EC2 instances in an instance group.
--
-- /Note:/ Consider using 'autoScalingRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfAutoScalingRole :: Lens.Lens' RunJobFlow (Lude.Maybe Lude.Text)
rjfAutoScalingRole = Lens.lens (autoScalingRole :: RunJobFlow -> Lude.Maybe Lude.Text) (\s a -> s {autoScalingRole = a} :: RunJobFlow)
{-# DEPRECATED rjfAutoScalingRole "Use generic-lens or generic-optics with 'autoScalingRole' instead." #-}

-- | The name of a security configuration to apply to the cluster.
--
-- /Note:/ Consider using 'securityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfSecurityConfiguration :: Lens.Lens' RunJobFlow (Lude.Maybe Lude.Text)
rjfSecurityConfiguration = Lens.lens (securityConfiguration :: RunJobFlow -> Lude.Maybe Lude.Text) (\s a -> s {securityConfiguration = a} :: RunJobFlow)
{-# DEPRECATED rjfSecurityConfiguration "Use generic-lens or generic-optics with 'securityConfiguration' instead." #-}

-- | Specifies the way that individual Amazon EC2 instances terminate when an automatic scale-in activity occurs or an instance group is resized. @TERMINATE_AT_INSTANCE_HOUR@ indicates that Amazon EMR terminates nodes at the instance-hour boundary, regardless of when the request to terminate the instance was submitted. This option is only available with Amazon EMR 5.1.0 and later and is the default for clusters created using that version. @TERMINATE_AT_TASK_COMPLETION@ indicates that Amazon EMR adds nodes to a deny list and drains tasks from nodes before terminating the Amazon EC2 instances, regardless of the instance-hour boundary. With either behavior, Amazon EMR removes the least active nodes first and blocks instance termination if it could lead to HDFS corruption. @TERMINATE_AT_TASK_COMPLETION@ available only in Amazon EMR version 4.1.0 and later, and is the default for versions of Amazon EMR earlier than 5.1.0.
--
-- /Note:/ Consider using 'scaleDownBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfScaleDownBehavior :: Lens.Lens' RunJobFlow (Lude.Maybe ScaleDownBehavior)
rjfScaleDownBehavior = Lens.lens (scaleDownBehavior :: RunJobFlow -> Lude.Maybe ScaleDownBehavior) (\s a -> s {scaleDownBehavior = a} :: RunJobFlow)
{-# DEPRECATED rjfScaleDownBehavior "Use generic-lens or generic-optics with 'scaleDownBehavior' instead." #-}

-- | A list of steps to run.
--
-- /Note:/ Consider using 'steps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfSteps :: Lens.Lens' RunJobFlow (Lude.Maybe [StepConfig])
rjfSteps = Lens.lens (steps :: RunJobFlow -> Lude.Maybe [StepConfig]) (\s a -> s {steps = a} :: RunJobFlow)
{-# DEPRECATED rjfSteps "Use generic-lens or generic-optics with 'steps' instead." #-}

-- | Also called instance profile and EC2 role. An IAM role for an EMR cluster. The EC2 instances of the cluster assume this role. The default role is @EMR_EC2_DefaultRole@ . In order to use the default role, you must have already created it using the CLI or console.
--
-- /Note:/ Consider using 'jobFlowRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfJobFlowRole :: Lens.Lens' RunJobFlow (Lude.Maybe Lude.Text)
rjfJobFlowRole = Lens.lens (jobFlowRole :: RunJobFlow -> Lude.Maybe Lude.Text) (\s a -> s {jobFlowRole = a} :: RunJobFlow)
{-# DEPRECATED rjfJobFlowRole "Use generic-lens or generic-optics with 'jobFlowRole' instead." #-}

-- | A list of bootstrap actions to run before Hadoop starts on the cluster nodes.
--
-- /Note:/ Consider using 'bootstrapActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfBootstrapActions :: Lens.Lens' RunJobFlow (Lude.Maybe [BootstrapActionConfig])
rjfBootstrapActions = Lens.lens (bootstrapActions :: RunJobFlow -> Lude.Maybe [BootstrapActionConfig]) (\s a -> s {bootstrapActions = a} :: RunJobFlow)
{-# DEPRECATED rjfBootstrapActions "Use generic-lens or generic-optics with 'bootstrapActions' instead." #-}

-- | The Amazon EMR release label, which determines the version of open-source application packages installed on the cluster. Release labels are in the form @emr-x.x.x@ , where x.x.x is an Amazon EMR release version such as @emr-5.14.0@ . For more information about Amazon EMR release versions and included application versions and features, see <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/ https://docs.aws.amazon.com/emr/latest/ReleaseGuide/> . The release label applies only to Amazon EMR releases version 4.0 and later. Earlier versions use @AmiVersion@ .
--
-- /Note:/ Consider using 'releaseLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfReleaseLabel :: Lens.Lens' RunJobFlow (Lude.Maybe Lude.Text)
rjfReleaseLabel = Lens.lens (releaseLabel :: RunJobFlow -> Lude.Maybe Lude.Text) (\s a -> s {releaseLabel = a} :: RunJobFlow)
{-# DEPRECATED rjfReleaseLabel "Use generic-lens or generic-optics with 'releaseLabel' instead." #-}

-- | Applies only when @CustomAmiID@ is used. Specifies which updates from the Amazon Linux AMI package repositories to apply automatically when the instance boots using the AMI. If omitted, the default is @SECURITY@ , which indicates that only security updates are applied. If @NONE@ is specified, no updates are applied, and all updates must be applied manually.
--
-- /Note:/ Consider using 'repoUpgradeOnBoot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfRepoUpgradeOnBoot :: Lens.Lens' RunJobFlow (Lude.Maybe RepoUpgradeOnBoot)
rjfRepoUpgradeOnBoot = Lens.lens (repoUpgradeOnBoot :: RunJobFlow -> Lude.Maybe RepoUpgradeOnBoot) (\s a -> s {repoUpgradeOnBoot = a} :: RunJobFlow)
{-# DEPRECATED rjfRepoUpgradeOnBoot "Use generic-lens or generic-optics with 'repoUpgradeOnBoot' instead." #-}

-- | The specified placement group configuration for an Amazon EMR cluster.
--
-- /Note:/ Consider using 'placementGroupConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfPlacementGroupConfigs :: Lens.Lens' RunJobFlow (Lude.Maybe [PlacementGroupConfig])
rjfPlacementGroupConfigs = Lens.lens (placementGroupConfigs :: RunJobFlow -> Lude.Maybe [PlacementGroupConfig]) (\s a -> s {placementGroupConfigs = a} :: RunJobFlow)
{-# DEPRECATED rjfPlacementGroupConfigs "Use generic-lens or generic-optics with 'placementGroupConfigs' instead." #-}

-- | The location in Amazon S3 to write the log files of the job flow. If a value is not provided, logs are not created.
--
-- /Note:/ Consider using 'logURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfLogURI :: Lens.Lens' RunJobFlow (Lude.Maybe Lude.Text)
rjfLogURI = Lens.lens (logURI :: RunJobFlow -> Lude.Maybe Lude.Text) (\s a -> s {logURI = a} :: RunJobFlow)
{-# DEPRECATED rjfLogURI "Use generic-lens or generic-optics with 'logURI' instead." #-}

-- | Attributes for Kerberos configuration when Kerberos authentication is enabled using a security configuration. For more information see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-kerberos.html Use Kerberos Authentication> in the /Amazon EMR Management Guide/ .
--
-- /Note:/ Consider using 'kerberosAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfKerberosAttributes :: Lens.Lens' RunJobFlow (Lude.Maybe KerberosAttributes)
rjfKerberosAttributes = Lens.lens (kerberosAttributes :: RunJobFlow -> Lude.Maybe KerberosAttributes) (\s a -> s {kerberosAttributes = a} :: RunJobFlow)
{-# DEPRECATED rjfKerberosAttributes "Use generic-lens or generic-optics with 'kerberosAttributes' instead." #-}

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
rjfNewSupportedProducts :: Lens.Lens' RunJobFlow (Lude.Maybe [SupportedProductConfig])
rjfNewSupportedProducts = Lens.lens (newSupportedProducts :: RunJobFlow -> Lude.Maybe [SupportedProductConfig]) (\s a -> s {newSupportedProducts = a} :: RunJobFlow)
{-# DEPRECATED rjfNewSupportedProducts "Use generic-lens or generic-optics with 'newSupportedProducts' instead." #-}

-- | The specified managed scaling policy for an Amazon EMR cluster.
--
-- /Note:/ Consider using 'managedScalingPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfManagedScalingPolicy :: Lens.Lens' RunJobFlow (Lude.Maybe ManagedScalingPolicy)
rjfManagedScalingPolicy = Lens.lens (managedScalingPolicy :: RunJobFlow -> Lude.Maybe ManagedScalingPolicy) (\s a -> s {managedScalingPolicy = a} :: RunJobFlow)
{-# DEPRECATED rjfManagedScalingPolicy "Use generic-lens or generic-optics with 'managedScalingPolicy' instead." #-}

-- | A value of @true@ indicates that all IAM users in the AWS account can perform cluster actions if they have the proper IAM policy permissions. This is the default. A value of @false@ indicates that only the IAM user who created the cluster can perform actions.
--
-- /Note:/ Consider using 'visibleToAllUsers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfVisibleToAllUsers :: Lens.Lens' RunJobFlow (Lude.Maybe Lude.Bool)
rjfVisibleToAllUsers = Lens.lens (visibleToAllUsers :: RunJobFlow -> Lude.Maybe Lude.Bool) (\s a -> s {visibleToAllUsers = a} :: RunJobFlow)
{-# DEPRECATED rjfVisibleToAllUsers "Use generic-lens or generic-optics with 'visibleToAllUsers' instead." #-}

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
rjfSupportedProducts :: Lens.Lens' RunJobFlow (Lude.Maybe [Lude.Text])
rjfSupportedProducts = Lens.lens (supportedProducts :: RunJobFlow -> Lude.Maybe [Lude.Text]) (\s a -> s {supportedProducts = a} :: RunJobFlow)
{-# DEPRECATED rjfSupportedProducts "Use generic-lens or generic-optics with 'supportedProducts' instead." #-}

-- | Specifies the number of steps that can be executed concurrently. The default value is @1@ . The maximum value is @256@ .
--
-- /Note:/ Consider using 'stepConcurrencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfStepConcurrencyLevel :: Lens.Lens' RunJobFlow (Lude.Maybe Lude.Int)
rjfStepConcurrencyLevel = Lens.lens (stepConcurrencyLevel :: RunJobFlow -> Lude.Maybe Lude.Int) (\s a -> s {stepConcurrencyLevel = a} :: RunJobFlow)
{-# DEPRECATED rjfStepConcurrencyLevel "Use generic-lens or generic-optics with 'stepConcurrencyLevel' instead." #-}

-- | Applies to Amazon EMR releases 4.0 and later. A case-insensitive list of applications for Amazon EMR to install and configure when launching the cluster. For a list of applications available for each Amazon EMR release version, see the <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/ Amazon EMR Release Guide> .
--
-- /Note:/ Consider using 'applications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfApplications :: Lens.Lens' RunJobFlow (Lude.Maybe [Application])
rjfApplications = Lens.lens (applications :: RunJobFlow -> Lude.Maybe [Application]) (\s a -> s {applications = a} :: RunJobFlow)
{-# DEPRECATED rjfApplications "Use generic-lens or generic-optics with 'applications' instead." #-}

-- | A list of tags to associate with a cluster and propagate to Amazon EC2 instances.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfTags :: Lens.Lens' RunJobFlow (Lude.Maybe [Tag])
rjfTags = Lens.lens (tags :: RunJobFlow -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: RunJobFlow)
{-# DEPRECATED rjfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The IAM role that will be assumed by the Amazon EMR service to access AWS resources on your behalf.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfServiceRole :: Lens.Lens' RunJobFlow (Lude.Maybe Lude.Text)
rjfServiceRole = Lens.lens (serviceRole :: RunJobFlow -> Lude.Maybe Lude.Text) (\s a -> s {serviceRole = a} :: RunJobFlow)
{-# DEPRECATED rjfServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

-- | The name of the job flow.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfName :: Lens.Lens' RunJobFlow Lude.Text
rjfName = Lens.lens (name :: RunJobFlow -> Lude.Text) (\s a -> s {name = a} :: RunJobFlow)
{-# DEPRECATED rjfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A specification of the number and type of Amazon EC2 instances.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfInstances :: Lens.Lens' RunJobFlow JobFlowInstancesConfig
rjfInstances = Lens.lens (instances :: RunJobFlow -> JobFlowInstancesConfig) (\s a -> s {instances = a} :: RunJobFlow)
{-# DEPRECATED rjfInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

instance Lude.AWSRequest RunJobFlow where
  type Rs RunJobFlow = RunJobFlowResponse
  request = Req.postJSON emrService
  response =
    Res.receiveJSON
      ( \s h x ->
          RunJobFlowResponse'
            Lude.<$> (x Lude..?> "ClusterArn")
            Lude.<*> (x Lude..?> "JobFlowId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RunJobFlow where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.RunJobFlow" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RunJobFlow where
  toJSON RunJobFlow' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("LogEncryptionKmsKeyId" Lude..=) Lude.<$> logEncryptionKMSKeyId,
            ("AmiVersion" Lude..=) Lude.<$> amiVersion,
            ("EbsRootVolumeSize" Lude..=) Lude.<$> ebsRootVolumeSize,
            ("AdditionalInfo" Lude..=) Lude.<$> additionalInfo,
            ("Configurations" Lude..=) Lude.<$> configurations,
            ("CustomAmiId" Lude..=) Lude.<$> customAMIId,
            ("AutoScalingRole" Lude..=) Lude.<$> autoScalingRole,
            ("SecurityConfiguration" Lude..=) Lude.<$> securityConfiguration,
            ("ScaleDownBehavior" Lude..=) Lude.<$> scaleDownBehavior,
            ("Steps" Lude..=) Lude.<$> steps,
            ("JobFlowRole" Lude..=) Lude.<$> jobFlowRole,
            ("BootstrapActions" Lude..=) Lude.<$> bootstrapActions,
            ("ReleaseLabel" Lude..=) Lude.<$> releaseLabel,
            ("RepoUpgradeOnBoot" Lude..=) Lude.<$> repoUpgradeOnBoot,
            ("PlacementGroupConfigs" Lude..=) Lude.<$> placementGroupConfigs,
            ("LogUri" Lude..=) Lude.<$> logURI,
            ("KerberosAttributes" Lude..=) Lude.<$> kerberosAttributes,
            ("NewSupportedProducts" Lude..=) Lude.<$> newSupportedProducts,
            ("ManagedScalingPolicy" Lude..=) Lude.<$> managedScalingPolicy,
            ("VisibleToAllUsers" Lude..=) Lude.<$> visibleToAllUsers,
            ("SupportedProducts" Lude..=) Lude.<$> supportedProducts,
            ("StepConcurrencyLevel" Lude..=) Lude.<$> stepConcurrencyLevel,
            ("Applications" Lude..=) Lude.<$> applications,
            ("Tags" Lude..=) Lude.<$> tags,
            ("ServiceRole" Lude..=) Lude.<$> serviceRole,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("Instances" Lude..= instances)
          ]
      )

instance Lude.ToPath RunJobFlow where
  toPath = Lude.const "/"

instance Lude.ToQuery RunJobFlow where
  toQuery = Lude.const Lude.mempty

-- | The result of the 'RunJobFlow' operation.
--
-- /See:/ 'mkRunJobFlowResponse' smart constructor.
data RunJobFlowResponse = RunJobFlowResponse'
  { clusterARN ::
      Lude.Maybe Lude.Text,
    jobFlowId :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RunJobFlowResponse' with the minimum fields required to make a request.
--
-- * 'clusterARN' - The Amazon Resource Name of the cluster.
-- * 'jobFlowId' - An unique identifier for the job flow.
-- * 'responseStatus' - The response status code.
mkRunJobFlowResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RunJobFlowResponse
mkRunJobFlowResponse pResponseStatus_ =
  RunJobFlowResponse'
    { clusterARN = Lude.Nothing,
      jobFlowId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name of the cluster.
--
-- /Note:/ Consider using 'clusterARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfrsClusterARN :: Lens.Lens' RunJobFlowResponse (Lude.Maybe Lude.Text)
rjfrsClusterARN = Lens.lens (clusterARN :: RunJobFlowResponse -> Lude.Maybe Lude.Text) (\s a -> s {clusterARN = a} :: RunJobFlowResponse)
{-# DEPRECATED rjfrsClusterARN "Use generic-lens or generic-optics with 'clusterARN' instead." #-}

-- | An unique identifier for the job flow.
--
-- /Note:/ Consider using 'jobFlowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfrsJobFlowId :: Lens.Lens' RunJobFlowResponse (Lude.Maybe Lude.Text)
rjfrsJobFlowId = Lens.lens (jobFlowId :: RunJobFlowResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobFlowId = a} :: RunJobFlowResponse)
{-# DEPRECATED rjfrsJobFlowId "Use generic-lens or generic-optics with 'jobFlowId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjfrsResponseStatus :: Lens.Lens' RunJobFlowResponse Lude.Int
rjfrsResponseStatus = Lens.lens (responseStatus :: RunJobFlowResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RunJobFlowResponse)
{-# DEPRECATED rjfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
