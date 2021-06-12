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
-- Module      : Network.AWS.EMR.RunJobFlow
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- RunJobFlow creates and starts running a new cluster (job flow). The
-- cluster runs the steps specified. After the steps complete, the cluster
-- stops and the HDFS partition is lost. To prevent loss of data, configure
-- the last step of the job flow to store results in Amazon S3. If the
-- JobFlowInstancesConfig @KeepJobFlowAliveWhenNoSteps@ parameter is set to
-- @TRUE@, the cluster transitions to the WAITING state rather than
-- shutting down after the steps have completed.
--
-- For additional protection, you can set the JobFlowInstancesConfig
-- @TerminationProtected@ parameter to @TRUE@ to lock the cluster and
-- prevent it from being terminated by API call, user intervention, or in
-- the event of a job flow error.
--
-- A maximum of 256 steps are allowed in each job flow.
--
-- If your cluster is long-running (such as a Hive data warehouse) or
-- complex, you may require more than 256 steps to process your data. You
-- can bypass the 256-step limitation in various ways, including using the
-- SSH shell to connect to the master node and submitting queries directly
-- to the software running on the master node, such as Hive and Hadoop. For
-- more information on how to do this, see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/AddMoreThan256Steps.html Add More than 256 Steps to a Cluster>
-- in the /Amazon EMR Management Guide/.
--
-- For long running clusters, we recommend that you periodically store your
-- results.
--
-- The instance fleets configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions. The RunJobFlow
-- request can contain InstanceFleets parameters or InstanceGroups
-- parameters, but not both.
module Network.AWS.EMR.RunJobFlow
  ( -- * Creating a Request
    RunJobFlow (..),
    newRunJobFlow,

    -- * Request Lenses
    runJobFlow_amiVersion,
    runJobFlow_additionalInfo,
    runJobFlow_placementGroupConfigs,
    runJobFlow_repoUpgradeOnBoot,
    runJobFlow_serviceRole,
    runJobFlow_securityConfiguration,
    runJobFlow_scaleDownBehavior,
    runJobFlow_autoScalingRole,
    runJobFlow_configurations,
    runJobFlow_releaseLabel,
    runJobFlow_ebsRootVolumeSize,
    runJobFlow_bootstrapActions,
    runJobFlow_logEncryptionKmsKeyId,
    runJobFlow_tags,
    runJobFlow_applications,
    runJobFlow_stepConcurrencyLevel,
    runJobFlow_jobFlowRole,
    runJobFlow_steps,
    runJobFlow_supportedProducts,
    runJobFlow_visibleToAllUsers,
    runJobFlow_customAmiId,
    runJobFlow_managedScalingPolicy,
    runJobFlow_kerberosAttributes,
    runJobFlow_logUri,
    runJobFlow_newSupportedProducts,
    runJobFlow_name,
    runJobFlow_instances,

    -- * Destructuring the Response
    RunJobFlowResponse (..),
    newRunJobFlowResponse,

    -- * Response Lenses
    runJobFlowResponse_clusterArn,
    runJobFlowResponse_jobFlowId,
    runJobFlowResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the RunJobFlow operation.
--
-- /See:/ 'newRunJobFlow' smart constructor.
data RunJobFlow = RunJobFlow'
  { -- | Applies only to Amazon EMR AMI versions 3.x and 2.x. For Amazon EMR
    -- releases 4.0 and later, @ReleaseLabel@ is used. To specify a custom AMI,
    -- use @CustomAmiID@.
    amiVersion :: Core.Maybe Core.Text,
    -- | A JSON string for selecting additional features.
    additionalInfo :: Core.Maybe Core.Text,
    -- | The specified placement group configuration for an Amazon EMR cluster.
    placementGroupConfigs :: Core.Maybe [PlacementGroupConfig],
    -- | Applies only when @CustomAmiID@ is used. Specifies which updates from
    -- the Amazon Linux AMI package repositories to apply automatically when
    -- the instance boots using the AMI. If omitted, the default is @SECURITY@,
    -- which indicates that only security updates are applied. If @NONE@ is
    -- specified, no updates are applied, and all updates must be applied
    -- manually.
    repoUpgradeOnBoot :: Core.Maybe RepoUpgradeOnBoot,
    -- | The IAM role that will be assumed by the Amazon EMR service to access
    -- AWS resources on your behalf.
    serviceRole :: Core.Maybe Core.Text,
    -- | The name of a security configuration to apply to the cluster.
    securityConfiguration :: Core.Maybe Core.Text,
    -- | Specifies the way that individual Amazon EC2 instances terminate when an
    -- automatic scale-in activity occurs or an instance group is resized.
    -- @TERMINATE_AT_INSTANCE_HOUR@ indicates that Amazon EMR terminates nodes
    -- at the instance-hour boundary, regardless of when the request to
    -- terminate the instance was submitted. This option is only available with
    -- Amazon EMR 5.1.0 and later and is the default for clusters created using
    -- that version. @TERMINATE_AT_TASK_COMPLETION@ indicates that Amazon EMR
    -- adds nodes to a deny list and drains tasks from nodes before terminating
    -- the Amazon EC2 instances, regardless of the instance-hour boundary. With
    -- either behavior, Amazon EMR removes the least active nodes first and
    -- blocks instance termination if it could lead to HDFS corruption.
    -- @TERMINATE_AT_TASK_COMPLETION@ available only in Amazon EMR version
    -- 4.1.0 and later, and is the default for versions of Amazon EMR earlier
    -- than 5.1.0.
    scaleDownBehavior :: Core.Maybe ScaleDownBehavior,
    -- | An IAM role for automatic scaling policies. The default role is
    -- @EMR_AutoScaling_DefaultRole@. The IAM role provides permissions that
    -- the automatic scaling feature requires to launch and terminate EC2
    -- instances in an instance group.
    autoScalingRole :: Core.Maybe Core.Text,
    -- | For Amazon EMR releases 4.0 and later. The list of configurations
    -- supplied for the EMR cluster you are creating.
    configurations :: Core.Maybe [Configuration],
    -- | The Amazon EMR release label, which determines the version of
    -- open-source application packages installed on the cluster. Release
    -- labels are in the form @emr-x.x.x@, where x.x.x is an Amazon EMR release
    -- version such as @emr-5.14.0@. For more information about Amazon EMR
    -- release versions and included application versions and features, see
    -- <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/>. The release
    -- label applies only to Amazon EMR releases version 4.0 and later. Earlier
    -- versions use @AmiVersion@.
    releaseLabel :: Core.Maybe Core.Text,
    -- | The size, in GiB, of the Amazon EBS root device volume of the Linux AMI
    -- that is used for each EC2 instance. Available in Amazon EMR version 4.x
    -- and later.
    ebsRootVolumeSize :: Core.Maybe Core.Int,
    -- | A list of bootstrap actions to run before Hadoop starts on the cluster
    -- nodes.
    bootstrapActions :: Core.Maybe [BootstrapActionConfig],
    -- | The AWS KMS customer master key (CMK) used for encrypting log files. If
    -- a value is not provided, the logs remain encrypted by AES-256. This
    -- attribute is only available with Amazon EMR version 5.30.0 and later,
    -- excluding Amazon EMR 6.0.0.
    logEncryptionKmsKeyId :: Core.Maybe Core.Text,
    -- | A list of tags to associate with a cluster and propagate to Amazon EC2
    -- instances.
    tags :: Core.Maybe [Tag],
    -- | Applies to Amazon EMR releases 4.0 and later. A case-insensitive list of
    -- applications for Amazon EMR to install and configure when launching the
    -- cluster. For a list of applications available for each Amazon EMR
    -- release version, see the
    -- <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/ Amazon EMR Release Guide>.
    applications :: Core.Maybe [Application],
    -- | Specifies the number of steps that can be executed concurrently. The
    -- default value is @1@. The maximum value is @256@.
    stepConcurrencyLevel :: Core.Maybe Core.Int,
    -- | Also called instance profile and EC2 role. An IAM role for an EMR
    -- cluster. The EC2 instances of the cluster assume this role. The default
    -- role is @EMR_EC2_DefaultRole@. In order to use the default role, you
    -- must have already created it using the CLI or console.
    jobFlowRole :: Core.Maybe Core.Text,
    -- | A list of steps to run.
    steps :: Core.Maybe [StepConfig],
    -- | For Amazon EMR releases 3.x and 2.x. For Amazon EMR releases 4.x and
    -- later, use Applications.
    --
    -- A list of strings that indicates third-party software to use. For more
    -- information, see the
    -- <https://docs.aws.amazon.com/emr/latest/DeveloperGuide/emr-dg.pdf Amazon EMR Developer Guide>.
    -- Currently supported values are:
    --
    -- -   \"mapr-m3\" - launch the job flow using MapR M3 Edition.
    --
    -- -   \"mapr-m5\" - launch the job flow using MapR M5 Edition.
    supportedProducts :: Core.Maybe [Core.Text],
    -- | A value of @true@ indicates that all IAM users in the AWS account can
    -- perform cluster actions if they have the proper IAM policy permissions.
    -- This is the default. A value of @false@ indicates that only the IAM user
    -- who created the cluster can perform actions.
    visibleToAllUsers :: Core.Maybe Core.Bool,
    -- | Available only in Amazon EMR version 5.7.0 and later. The ID of a custom
    -- Amazon EBS-backed Linux AMI. If specified, Amazon EMR uses this AMI when
    -- it launches cluster EC2 instances. For more information about custom
    -- AMIs in Amazon EMR, see
    -- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-custom-ami.html Using a Custom AMI>
    -- in the /Amazon EMR Management Guide/. If omitted, the cluster uses the
    -- base Linux AMI for the @ReleaseLabel@ specified. For Amazon EMR versions
    -- 2.x and 3.x, use @AmiVersion@ instead.
    --
    -- For information about creating a custom AMI, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/creating-an-ami-ebs.html Creating an Amazon EBS-Backed Linux AMI>
    -- in the /Amazon Elastic Compute Cloud User Guide for Linux Instances/.
    -- For information about finding an AMI ID, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html Finding a Linux AMI>.
    customAmiId :: Core.Maybe Core.Text,
    -- | The specified managed scaling policy for an Amazon EMR cluster.
    managedScalingPolicy :: Core.Maybe ManagedScalingPolicy,
    -- | Attributes for Kerberos configuration when Kerberos authentication is
    -- enabled using a security configuration. For more information see
    -- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-kerberos.html Use Kerberos Authentication>
    -- in the /Amazon EMR Management Guide/.
    kerberosAttributes :: Core.Maybe KerberosAttributes,
    -- | The location in Amazon S3 to write the log files of the job flow. If a
    -- value is not provided, logs are not created.
    logUri :: Core.Maybe Core.Text,
    -- | For Amazon EMR releases 3.x and 2.x. For Amazon EMR releases 4.x and
    -- later, use Applications.
    --
    -- A list of strings that indicates third-party software to use with the
    -- job flow that accepts a user argument list. EMR accepts and forwards the
    -- argument list to the corresponding installation script as bootstrap
    -- action arguments. For more information, see \"Launch a Job Flow on the
    -- MapR Distribution for Hadoop\" in the
    -- <https://docs.aws.amazon.com/emr/latest/DeveloperGuide/emr-dg.pdf Amazon EMR Developer Guide>.
    -- Supported values are:
    --
    -- -   \"mapr-m3\" - launch the cluster using MapR M3 Edition.
    --
    -- -   \"mapr-m5\" - launch the cluster using MapR M5 Edition.
    --
    -- -   \"mapr\" with the user arguments specifying \"--edition,m3\" or
    --     \"--edition,m5\" - launch the job flow using MapR M3 or M5 Edition
    --     respectively.
    --
    -- -   \"mapr-m7\" - launch the cluster using MapR M7 Edition.
    --
    -- -   \"hunk\" - launch the cluster with the Hunk Big Data Analytics
    --     Platform.
    --
    -- -   \"hue\"- launch the cluster with Hue installed.
    --
    -- -   \"spark\" - launch the cluster with Apache Spark installed.
    --
    -- -   \"ganglia\" - launch the cluster with the Ganglia Monitoring System
    --     installed.
    newSupportedProducts' :: Core.Maybe [SupportedProductConfig],
    -- | The name of the job flow.
    name :: Core.Text,
    -- | A specification of the number and type of Amazon EC2 instances.
    instances :: JobFlowInstancesConfig
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RunJobFlow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amiVersion', 'runJobFlow_amiVersion' - Applies only to Amazon EMR AMI versions 3.x and 2.x. For Amazon EMR
-- releases 4.0 and later, @ReleaseLabel@ is used. To specify a custom AMI,
-- use @CustomAmiID@.
--
-- 'additionalInfo', 'runJobFlow_additionalInfo' - A JSON string for selecting additional features.
--
-- 'placementGroupConfigs', 'runJobFlow_placementGroupConfigs' - The specified placement group configuration for an Amazon EMR cluster.
--
-- 'repoUpgradeOnBoot', 'runJobFlow_repoUpgradeOnBoot' - Applies only when @CustomAmiID@ is used. Specifies which updates from
-- the Amazon Linux AMI package repositories to apply automatically when
-- the instance boots using the AMI. If omitted, the default is @SECURITY@,
-- which indicates that only security updates are applied. If @NONE@ is
-- specified, no updates are applied, and all updates must be applied
-- manually.
--
-- 'serviceRole', 'runJobFlow_serviceRole' - The IAM role that will be assumed by the Amazon EMR service to access
-- AWS resources on your behalf.
--
-- 'securityConfiguration', 'runJobFlow_securityConfiguration' - The name of a security configuration to apply to the cluster.
--
-- 'scaleDownBehavior', 'runJobFlow_scaleDownBehavior' - Specifies the way that individual Amazon EC2 instances terminate when an
-- automatic scale-in activity occurs or an instance group is resized.
-- @TERMINATE_AT_INSTANCE_HOUR@ indicates that Amazon EMR terminates nodes
-- at the instance-hour boundary, regardless of when the request to
-- terminate the instance was submitted. This option is only available with
-- Amazon EMR 5.1.0 and later and is the default for clusters created using
-- that version. @TERMINATE_AT_TASK_COMPLETION@ indicates that Amazon EMR
-- adds nodes to a deny list and drains tasks from nodes before terminating
-- the Amazon EC2 instances, regardless of the instance-hour boundary. With
-- either behavior, Amazon EMR removes the least active nodes first and
-- blocks instance termination if it could lead to HDFS corruption.
-- @TERMINATE_AT_TASK_COMPLETION@ available only in Amazon EMR version
-- 4.1.0 and later, and is the default for versions of Amazon EMR earlier
-- than 5.1.0.
--
-- 'autoScalingRole', 'runJobFlow_autoScalingRole' - An IAM role for automatic scaling policies. The default role is
-- @EMR_AutoScaling_DefaultRole@. The IAM role provides permissions that
-- the automatic scaling feature requires to launch and terminate EC2
-- instances in an instance group.
--
-- 'configurations', 'runJobFlow_configurations' - For Amazon EMR releases 4.0 and later. The list of configurations
-- supplied for the EMR cluster you are creating.
--
-- 'releaseLabel', 'runJobFlow_releaseLabel' - The Amazon EMR release label, which determines the version of
-- open-source application packages installed on the cluster. Release
-- labels are in the form @emr-x.x.x@, where x.x.x is an Amazon EMR release
-- version such as @emr-5.14.0@. For more information about Amazon EMR
-- release versions and included application versions and features, see
-- <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/>. The release
-- label applies only to Amazon EMR releases version 4.0 and later. Earlier
-- versions use @AmiVersion@.
--
-- 'ebsRootVolumeSize', 'runJobFlow_ebsRootVolumeSize' - The size, in GiB, of the Amazon EBS root device volume of the Linux AMI
-- that is used for each EC2 instance. Available in Amazon EMR version 4.x
-- and later.
--
-- 'bootstrapActions', 'runJobFlow_bootstrapActions' - A list of bootstrap actions to run before Hadoop starts on the cluster
-- nodes.
--
-- 'logEncryptionKmsKeyId', 'runJobFlow_logEncryptionKmsKeyId' - The AWS KMS customer master key (CMK) used for encrypting log files. If
-- a value is not provided, the logs remain encrypted by AES-256. This
-- attribute is only available with Amazon EMR version 5.30.0 and later,
-- excluding Amazon EMR 6.0.0.
--
-- 'tags', 'runJobFlow_tags' - A list of tags to associate with a cluster and propagate to Amazon EC2
-- instances.
--
-- 'applications', 'runJobFlow_applications' - Applies to Amazon EMR releases 4.0 and later. A case-insensitive list of
-- applications for Amazon EMR to install and configure when launching the
-- cluster. For a list of applications available for each Amazon EMR
-- release version, see the
-- <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/ Amazon EMR Release Guide>.
--
-- 'stepConcurrencyLevel', 'runJobFlow_stepConcurrencyLevel' - Specifies the number of steps that can be executed concurrently. The
-- default value is @1@. The maximum value is @256@.
--
-- 'jobFlowRole', 'runJobFlow_jobFlowRole' - Also called instance profile and EC2 role. An IAM role for an EMR
-- cluster. The EC2 instances of the cluster assume this role. The default
-- role is @EMR_EC2_DefaultRole@. In order to use the default role, you
-- must have already created it using the CLI or console.
--
-- 'steps', 'runJobFlow_steps' - A list of steps to run.
--
-- 'supportedProducts', 'runJobFlow_supportedProducts' - For Amazon EMR releases 3.x and 2.x. For Amazon EMR releases 4.x and
-- later, use Applications.
--
-- A list of strings that indicates third-party software to use. For more
-- information, see the
-- <https://docs.aws.amazon.com/emr/latest/DeveloperGuide/emr-dg.pdf Amazon EMR Developer Guide>.
-- Currently supported values are:
--
-- -   \"mapr-m3\" - launch the job flow using MapR M3 Edition.
--
-- -   \"mapr-m5\" - launch the job flow using MapR M5 Edition.
--
-- 'visibleToAllUsers', 'runJobFlow_visibleToAllUsers' - A value of @true@ indicates that all IAM users in the AWS account can
-- perform cluster actions if they have the proper IAM policy permissions.
-- This is the default. A value of @false@ indicates that only the IAM user
-- who created the cluster can perform actions.
--
-- 'customAmiId', 'runJobFlow_customAmiId' - Available only in Amazon EMR version 5.7.0 and later. The ID of a custom
-- Amazon EBS-backed Linux AMI. If specified, Amazon EMR uses this AMI when
-- it launches cluster EC2 instances. For more information about custom
-- AMIs in Amazon EMR, see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-custom-ami.html Using a Custom AMI>
-- in the /Amazon EMR Management Guide/. If omitted, the cluster uses the
-- base Linux AMI for the @ReleaseLabel@ specified. For Amazon EMR versions
-- 2.x and 3.x, use @AmiVersion@ instead.
--
-- For information about creating a custom AMI, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/creating-an-ami-ebs.html Creating an Amazon EBS-Backed Linux AMI>
-- in the /Amazon Elastic Compute Cloud User Guide for Linux Instances/.
-- For information about finding an AMI ID, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html Finding a Linux AMI>.
--
-- 'managedScalingPolicy', 'runJobFlow_managedScalingPolicy' - The specified managed scaling policy for an Amazon EMR cluster.
--
-- 'kerberosAttributes', 'runJobFlow_kerberosAttributes' - Attributes for Kerberos configuration when Kerberos authentication is
-- enabled using a security configuration. For more information see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-kerberos.html Use Kerberos Authentication>
-- in the /Amazon EMR Management Guide/.
--
-- 'logUri', 'runJobFlow_logUri' - The location in Amazon S3 to write the log files of the job flow. If a
-- value is not provided, logs are not created.
--
-- 'newSupportedProducts'', 'runJobFlow_newSupportedProducts' - For Amazon EMR releases 3.x and 2.x. For Amazon EMR releases 4.x and
-- later, use Applications.
--
-- A list of strings that indicates third-party software to use with the
-- job flow that accepts a user argument list. EMR accepts and forwards the
-- argument list to the corresponding installation script as bootstrap
-- action arguments. For more information, see \"Launch a Job Flow on the
-- MapR Distribution for Hadoop\" in the
-- <https://docs.aws.amazon.com/emr/latest/DeveloperGuide/emr-dg.pdf Amazon EMR Developer Guide>.
-- Supported values are:
--
-- -   \"mapr-m3\" - launch the cluster using MapR M3 Edition.
--
-- -   \"mapr-m5\" - launch the cluster using MapR M5 Edition.
--
-- -   \"mapr\" with the user arguments specifying \"--edition,m3\" or
--     \"--edition,m5\" - launch the job flow using MapR M3 or M5 Edition
--     respectively.
--
-- -   \"mapr-m7\" - launch the cluster using MapR M7 Edition.
--
-- -   \"hunk\" - launch the cluster with the Hunk Big Data Analytics
--     Platform.
--
-- -   \"hue\"- launch the cluster with Hue installed.
--
-- -   \"spark\" - launch the cluster with Apache Spark installed.
--
-- -   \"ganglia\" - launch the cluster with the Ganglia Monitoring System
--     installed.
--
-- 'name', 'runJobFlow_name' - The name of the job flow.
--
-- 'instances', 'runJobFlow_instances' - A specification of the number and type of Amazon EC2 instances.
newRunJobFlow ::
  -- | 'name'
  Core.Text ->
  -- | 'instances'
  JobFlowInstancesConfig ->
  RunJobFlow
newRunJobFlow pName_ pInstances_ =
  RunJobFlow'
    { amiVersion = Core.Nothing,
      additionalInfo = Core.Nothing,
      placementGroupConfigs = Core.Nothing,
      repoUpgradeOnBoot = Core.Nothing,
      serviceRole = Core.Nothing,
      securityConfiguration = Core.Nothing,
      scaleDownBehavior = Core.Nothing,
      autoScalingRole = Core.Nothing,
      configurations = Core.Nothing,
      releaseLabel = Core.Nothing,
      ebsRootVolumeSize = Core.Nothing,
      bootstrapActions = Core.Nothing,
      logEncryptionKmsKeyId = Core.Nothing,
      tags = Core.Nothing,
      applications = Core.Nothing,
      stepConcurrencyLevel = Core.Nothing,
      jobFlowRole = Core.Nothing,
      steps = Core.Nothing,
      supportedProducts = Core.Nothing,
      visibleToAllUsers = Core.Nothing,
      customAmiId = Core.Nothing,
      managedScalingPolicy = Core.Nothing,
      kerberosAttributes = Core.Nothing,
      logUri = Core.Nothing,
      newSupportedProducts' = Core.Nothing,
      name = pName_,
      instances = pInstances_
    }

-- | Applies only to Amazon EMR AMI versions 3.x and 2.x. For Amazon EMR
-- releases 4.0 and later, @ReleaseLabel@ is used. To specify a custom AMI,
-- use @CustomAmiID@.
runJobFlow_amiVersion :: Lens.Lens' RunJobFlow (Core.Maybe Core.Text)
runJobFlow_amiVersion = Lens.lens (\RunJobFlow' {amiVersion} -> amiVersion) (\s@RunJobFlow' {} a -> s {amiVersion = a} :: RunJobFlow)

-- | A JSON string for selecting additional features.
runJobFlow_additionalInfo :: Lens.Lens' RunJobFlow (Core.Maybe Core.Text)
runJobFlow_additionalInfo = Lens.lens (\RunJobFlow' {additionalInfo} -> additionalInfo) (\s@RunJobFlow' {} a -> s {additionalInfo = a} :: RunJobFlow)

-- | The specified placement group configuration for an Amazon EMR cluster.
runJobFlow_placementGroupConfigs :: Lens.Lens' RunJobFlow (Core.Maybe [PlacementGroupConfig])
runJobFlow_placementGroupConfigs = Lens.lens (\RunJobFlow' {placementGroupConfigs} -> placementGroupConfigs) (\s@RunJobFlow' {} a -> s {placementGroupConfigs = a} :: RunJobFlow) Core.. Lens.mapping Lens._Coerce

-- | Applies only when @CustomAmiID@ is used. Specifies which updates from
-- the Amazon Linux AMI package repositories to apply automatically when
-- the instance boots using the AMI. If omitted, the default is @SECURITY@,
-- which indicates that only security updates are applied. If @NONE@ is
-- specified, no updates are applied, and all updates must be applied
-- manually.
runJobFlow_repoUpgradeOnBoot :: Lens.Lens' RunJobFlow (Core.Maybe RepoUpgradeOnBoot)
runJobFlow_repoUpgradeOnBoot = Lens.lens (\RunJobFlow' {repoUpgradeOnBoot} -> repoUpgradeOnBoot) (\s@RunJobFlow' {} a -> s {repoUpgradeOnBoot = a} :: RunJobFlow)

-- | The IAM role that will be assumed by the Amazon EMR service to access
-- AWS resources on your behalf.
runJobFlow_serviceRole :: Lens.Lens' RunJobFlow (Core.Maybe Core.Text)
runJobFlow_serviceRole = Lens.lens (\RunJobFlow' {serviceRole} -> serviceRole) (\s@RunJobFlow' {} a -> s {serviceRole = a} :: RunJobFlow)

-- | The name of a security configuration to apply to the cluster.
runJobFlow_securityConfiguration :: Lens.Lens' RunJobFlow (Core.Maybe Core.Text)
runJobFlow_securityConfiguration = Lens.lens (\RunJobFlow' {securityConfiguration} -> securityConfiguration) (\s@RunJobFlow' {} a -> s {securityConfiguration = a} :: RunJobFlow)

-- | Specifies the way that individual Amazon EC2 instances terminate when an
-- automatic scale-in activity occurs or an instance group is resized.
-- @TERMINATE_AT_INSTANCE_HOUR@ indicates that Amazon EMR terminates nodes
-- at the instance-hour boundary, regardless of when the request to
-- terminate the instance was submitted. This option is only available with
-- Amazon EMR 5.1.0 and later and is the default for clusters created using
-- that version. @TERMINATE_AT_TASK_COMPLETION@ indicates that Amazon EMR
-- adds nodes to a deny list and drains tasks from nodes before terminating
-- the Amazon EC2 instances, regardless of the instance-hour boundary. With
-- either behavior, Amazon EMR removes the least active nodes first and
-- blocks instance termination if it could lead to HDFS corruption.
-- @TERMINATE_AT_TASK_COMPLETION@ available only in Amazon EMR version
-- 4.1.0 and later, and is the default for versions of Amazon EMR earlier
-- than 5.1.0.
runJobFlow_scaleDownBehavior :: Lens.Lens' RunJobFlow (Core.Maybe ScaleDownBehavior)
runJobFlow_scaleDownBehavior = Lens.lens (\RunJobFlow' {scaleDownBehavior} -> scaleDownBehavior) (\s@RunJobFlow' {} a -> s {scaleDownBehavior = a} :: RunJobFlow)

-- | An IAM role for automatic scaling policies. The default role is
-- @EMR_AutoScaling_DefaultRole@. The IAM role provides permissions that
-- the automatic scaling feature requires to launch and terminate EC2
-- instances in an instance group.
runJobFlow_autoScalingRole :: Lens.Lens' RunJobFlow (Core.Maybe Core.Text)
runJobFlow_autoScalingRole = Lens.lens (\RunJobFlow' {autoScalingRole} -> autoScalingRole) (\s@RunJobFlow' {} a -> s {autoScalingRole = a} :: RunJobFlow)

-- | For Amazon EMR releases 4.0 and later. The list of configurations
-- supplied for the EMR cluster you are creating.
runJobFlow_configurations :: Lens.Lens' RunJobFlow (Core.Maybe [Configuration])
runJobFlow_configurations = Lens.lens (\RunJobFlow' {configurations} -> configurations) (\s@RunJobFlow' {} a -> s {configurations = a} :: RunJobFlow) Core.. Lens.mapping Lens._Coerce

-- | The Amazon EMR release label, which determines the version of
-- open-source application packages installed on the cluster. Release
-- labels are in the form @emr-x.x.x@, where x.x.x is an Amazon EMR release
-- version such as @emr-5.14.0@. For more information about Amazon EMR
-- release versions and included application versions and features, see
-- <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/>. The release
-- label applies only to Amazon EMR releases version 4.0 and later. Earlier
-- versions use @AmiVersion@.
runJobFlow_releaseLabel :: Lens.Lens' RunJobFlow (Core.Maybe Core.Text)
runJobFlow_releaseLabel = Lens.lens (\RunJobFlow' {releaseLabel} -> releaseLabel) (\s@RunJobFlow' {} a -> s {releaseLabel = a} :: RunJobFlow)

-- | The size, in GiB, of the Amazon EBS root device volume of the Linux AMI
-- that is used for each EC2 instance. Available in Amazon EMR version 4.x
-- and later.
runJobFlow_ebsRootVolumeSize :: Lens.Lens' RunJobFlow (Core.Maybe Core.Int)
runJobFlow_ebsRootVolumeSize = Lens.lens (\RunJobFlow' {ebsRootVolumeSize} -> ebsRootVolumeSize) (\s@RunJobFlow' {} a -> s {ebsRootVolumeSize = a} :: RunJobFlow)

-- | A list of bootstrap actions to run before Hadoop starts on the cluster
-- nodes.
runJobFlow_bootstrapActions :: Lens.Lens' RunJobFlow (Core.Maybe [BootstrapActionConfig])
runJobFlow_bootstrapActions = Lens.lens (\RunJobFlow' {bootstrapActions} -> bootstrapActions) (\s@RunJobFlow' {} a -> s {bootstrapActions = a} :: RunJobFlow) Core.. Lens.mapping Lens._Coerce

-- | The AWS KMS customer master key (CMK) used for encrypting log files. If
-- a value is not provided, the logs remain encrypted by AES-256. This
-- attribute is only available with Amazon EMR version 5.30.0 and later,
-- excluding Amazon EMR 6.0.0.
runJobFlow_logEncryptionKmsKeyId :: Lens.Lens' RunJobFlow (Core.Maybe Core.Text)
runJobFlow_logEncryptionKmsKeyId = Lens.lens (\RunJobFlow' {logEncryptionKmsKeyId} -> logEncryptionKmsKeyId) (\s@RunJobFlow' {} a -> s {logEncryptionKmsKeyId = a} :: RunJobFlow)

-- | A list of tags to associate with a cluster and propagate to Amazon EC2
-- instances.
runJobFlow_tags :: Lens.Lens' RunJobFlow (Core.Maybe [Tag])
runJobFlow_tags = Lens.lens (\RunJobFlow' {tags} -> tags) (\s@RunJobFlow' {} a -> s {tags = a} :: RunJobFlow) Core.. Lens.mapping Lens._Coerce

-- | Applies to Amazon EMR releases 4.0 and later. A case-insensitive list of
-- applications for Amazon EMR to install and configure when launching the
-- cluster. For a list of applications available for each Amazon EMR
-- release version, see the
-- <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/ Amazon EMR Release Guide>.
runJobFlow_applications :: Lens.Lens' RunJobFlow (Core.Maybe [Application])
runJobFlow_applications = Lens.lens (\RunJobFlow' {applications} -> applications) (\s@RunJobFlow' {} a -> s {applications = a} :: RunJobFlow) Core.. Lens.mapping Lens._Coerce

-- | Specifies the number of steps that can be executed concurrently. The
-- default value is @1@. The maximum value is @256@.
runJobFlow_stepConcurrencyLevel :: Lens.Lens' RunJobFlow (Core.Maybe Core.Int)
runJobFlow_stepConcurrencyLevel = Lens.lens (\RunJobFlow' {stepConcurrencyLevel} -> stepConcurrencyLevel) (\s@RunJobFlow' {} a -> s {stepConcurrencyLevel = a} :: RunJobFlow)

-- | Also called instance profile and EC2 role. An IAM role for an EMR
-- cluster. The EC2 instances of the cluster assume this role. The default
-- role is @EMR_EC2_DefaultRole@. In order to use the default role, you
-- must have already created it using the CLI or console.
runJobFlow_jobFlowRole :: Lens.Lens' RunJobFlow (Core.Maybe Core.Text)
runJobFlow_jobFlowRole = Lens.lens (\RunJobFlow' {jobFlowRole} -> jobFlowRole) (\s@RunJobFlow' {} a -> s {jobFlowRole = a} :: RunJobFlow)

-- | A list of steps to run.
runJobFlow_steps :: Lens.Lens' RunJobFlow (Core.Maybe [StepConfig])
runJobFlow_steps = Lens.lens (\RunJobFlow' {steps} -> steps) (\s@RunJobFlow' {} a -> s {steps = a} :: RunJobFlow) Core.. Lens.mapping Lens._Coerce

-- | For Amazon EMR releases 3.x and 2.x. For Amazon EMR releases 4.x and
-- later, use Applications.
--
-- A list of strings that indicates third-party software to use. For more
-- information, see the
-- <https://docs.aws.amazon.com/emr/latest/DeveloperGuide/emr-dg.pdf Amazon EMR Developer Guide>.
-- Currently supported values are:
--
-- -   \"mapr-m3\" - launch the job flow using MapR M3 Edition.
--
-- -   \"mapr-m5\" - launch the job flow using MapR M5 Edition.
runJobFlow_supportedProducts :: Lens.Lens' RunJobFlow (Core.Maybe [Core.Text])
runJobFlow_supportedProducts = Lens.lens (\RunJobFlow' {supportedProducts} -> supportedProducts) (\s@RunJobFlow' {} a -> s {supportedProducts = a} :: RunJobFlow) Core.. Lens.mapping Lens._Coerce

-- | A value of @true@ indicates that all IAM users in the AWS account can
-- perform cluster actions if they have the proper IAM policy permissions.
-- This is the default. A value of @false@ indicates that only the IAM user
-- who created the cluster can perform actions.
runJobFlow_visibleToAllUsers :: Lens.Lens' RunJobFlow (Core.Maybe Core.Bool)
runJobFlow_visibleToAllUsers = Lens.lens (\RunJobFlow' {visibleToAllUsers} -> visibleToAllUsers) (\s@RunJobFlow' {} a -> s {visibleToAllUsers = a} :: RunJobFlow)

-- | Available only in Amazon EMR version 5.7.0 and later. The ID of a custom
-- Amazon EBS-backed Linux AMI. If specified, Amazon EMR uses this AMI when
-- it launches cluster EC2 instances. For more information about custom
-- AMIs in Amazon EMR, see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-custom-ami.html Using a Custom AMI>
-- in the /Amazon EMR Management Guide/. If omitted, the cluster uses the
-- base Linux AMI for the @ReleaseLabel@ specified. For Amazon EMR versions
-- 2.x and 3.x, use @AmiVersion@ instead.
--
-- For information about creating a custom AMI, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/creating-an-ami-ebs.html Creating an Amazon EBS-Backed Linux AMI>
-- in the /Amazon Elastic Compute Cloud User Guide for Linux Instances/.
-- For information about finding an AMI ID, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html Finding a Linux AMI>.
runJobFlow_customAmiId :: Lens.Lens' RunJobFlow (Core.Maybe Core.Text)
runJobFlow_customAmiId = Lens.lens (\RunJobFlow' {customAmiId} -> customAmiId) (\s@RunJobFlow' {} a -> s {customAmiId = a} :: RunJobFlow)

-- | The specified managed scaling policy for an Amazon EMR cluster.
runJobFlow_managedScalingPolicy :: Lens.Lens' RunJobFlow (Core.Maybe ManagedScalingPolicy)
runJobFlow_managedScalingPolicy = Lens.lens (\RunJobFlow' {managedScalingPolicy} -> managedScalingPolicy) (\s@RunJobFlow' {} a -> s {managedScalingPolicy = a} :: RunJobFlow)

-- | Attributes for Kerberos configuration when Kerberos authentication is
-- enabled using a security configuration. For more information see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-kerberos.html Use Kerberos Authentication>
-- in the /Amazon EMR Management Guide/.
runJobFlow_kerberosAttributes :: Lens.Lens' RunJobFlow (Core.Maybe KerberosAttributes)
runJobFlow_kerberosAttributes = Lens.lens (\RunJobFlow' {kerberosAttributes} -> kerberosAttributes) (\s@RunJobFlow' {} a -> s {kerberosAttributes = a} :: RunJobFlow)

-- | The location in Amazon S3 to write the log files of the job flow. If a
-- value is not provided, logs are not created.
runJobFlow_logUri :: Lens.Lens' RunJobFlow (Core.Maybe Core.Text)
runJobFlow_logUri = Lens.lens (\RunJobFlow' {logUri} -> logUri) (\s@RunJobFlow' {} a -> s {logUri = a} :: RunJobFlow)

-- | For Amazon EMR releases 3.x and 2.x. For Amazon EMR releases 4.x and
-- later, use Applications.
--
-- A list of strings that indicates third-party software to use with the
-- job flow that accepts a user argument list. EMR accepts and forwards the
-- argument list to the corresponding installation script as bootstrap
-- action arguments. For more information, see \"Launch a Job Flow on the
-- MapR Distribution for Hadoop\" in the
-- <https://docs.aws.amazon.com/emr/latest/DeveloperGuide/emr-dg.pdf Amazon EMR Developer Guide>.
-- Supported values are:
--
-- -   \"mapr-m3\" - launch the cluster using MapR M3 Edition.
--
-- -   \"mapr-m5\" - launch the cluster using MapR M5 Edition.
--
-- -   \"mapr\" with the user arguments specifying \"--edition,m3\" or
--     \"--edition,m5\" - launch the job flow using MapR M3 or M5 Edition
--     respectively.
--
-- -   \"mapr-m7\" - launch the cluster using MapR M7 Edition.
--
-- -   \"hunk\" - launch the cluster with the Hunk Big Data Analytics
--     Platform.
--
-- -   \"hue\"- launch the cluster with Hue installed.
--
-- -   \"spark\" - launch the cluster with Apache Spark installed.
--
-- -   \"ganglia\" - launch the cluster with the Ganglia Monitoring System
--     installed.
runJobFlow_newSupportedProducts :: Lens.Lens' RunJobFlow (Core.Maybe [SupportedProductConfig])
runJobFlow_newSupportedProducts = Lens.lens (\RunJobFlow' {newSupportedProducts'} -> newSupportedProducts') (\s@RunJobFlow' {} a -> s {newSupportedProducts' = a} :: RunJobFlow) Core.. Lens.mapping Lens._Coerce

-- | The name of the job flow.
runJobFlow_name :: Lens.Lens' RunJobFlow Core.Text
runJobFlow_name = Lens.lens (\RunJobFlow' {name} -> name) (\s@RunJobFlow' {} a -> s {name = a} :: RunJobFlow)

-- | A specification of the number and type of Amazon EC2 instances.
runJobFlow_instances :: Lens.Lens' RunJobFlow JobFlowInstancesConfig
runJobFlow_instances = Lens.lens (\RunJobFlow' {instances} -> instances) (\s@RunJobFlow' {} a -> s {instances = a} :: RunJobFlow)

instance Core.AWSRequest RunJobFlow where
  type AWSResponse RunJobFlow = RunJobFlowResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RunJobFlowResponse'
            Core.<$> (x Core..?> "ClusterArn")
            Core.<*> (x Core..?> "JobFlowId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RunJobFlow

instance Core.NFData RunJobFlow

instance Core.ToHeaders RunJobFlow where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("ElasticMapReduce.RunJobFlow" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RunJobFlow where
  toJSON RunJobFlow' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AmiVersion" Core..=) Core.<$> amiVersion,
            ("AdditionalInfo" Core..=) Core.<$> additionalInfo,
            ("PlacementGroupConfigs" Core..=)
              Core.<$> placementGroupConfigs,
            ("RepoUpgradeOnBoot" Core..=)
              Core.<$> repoUpgradeOnBoot,
            ("ServiceRole" Core..=) Core.<$> serviceRole,
            ("SecurityConfiguration" Core..=)
              Core.<$> securityConfiguration,
            ("ScaleDownBehavior" Core..=)
              Core.<$> scaleDownBehavior,
            ("AutoScalingRole" Core..=) Core.<$> autoScalingRole,
            ("Configurations" Core..=) Core.<$> configurations,
            ("ReleaseLabel" Core..=) Core.<$> releaseLabel,
            ("EbsRootVolumeSize" Core..=)
              Core.<$> ebsRootVolumeSize,
            ("BootstrapActions" Core..=)
              Core.<$> bootstrapActions,
            ("LogEncryptionKmsKeyId" Core..=)
              Core.<$> logEncryptionKmsKeyId,
            ("Tags" Core..=) Core.<$> tags,
            ("Applications" Core..=) Core.<$> applications,
            ("StepConcurrencyLevel" Core..=)
              Core.<$> stepConcurrencyLevel,
            ("JobFlowRole" Core..=) Core.<$> jobFlowRole,
            ("Steps" Core..=) Core.<$> steps,
            ("SupportedProducts" Core..=)
              Core.<$> supportedProducts,
            ("VisibleToAllUsers" Core..=)
              Core.<$> visibleToAllUsers,
            ("CustomAmiId" Core..=) Core.<$> customAmiId,
            ("ManagedScalingPolicy" Core..=)
              Core.<$> managedScalingPolicy,
            ("KerberosAttributes" Core..=)
              Core.<$> kerberosAttributes,
            ("LogUri" Core..=) Core.<$> logUri,
            ("NewSupportedProducts" Core..=)
              Core.<$> newSupportedProducts',
            Core.Just ("Name" Core..= name),
            Core.Just ("Instances" Core..= instances)
          ]
      )

instance Core.ToPath RunJobFlow where
  toPath = Core.const "/"

instance Core.ToQuery RunJobFlow where
  toQuery = Core.const Core.mempty

-- | The result of the RunJobFlow operation.
--
-- /See:/ 'newRunJobFlowResponse' smart constructor.
data RunJobFlowResponse = RunJobFlowResponse'
  { -- | The Amazon Resource Name (ARN) of the cluster.
    clusterArn :: Core.Maybe Core.Text,
    -- | A unique identifier for the job flow.
    jobFlowId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RunJobFlowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'runJobFlowResponse_clusterArn' - The Amazon Resource Name (ARN) of the cluster.
--
-- 'jobFlowId', 'runJobFlowResponse_jobFlowId' - A unique identifier for the job flow.
--
-- 'httpStatus', 'runJobFlowResponse_httpStatus' - The response's http status code.
newRunJobFlowResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RunJobFlowResponse
newRunJobFlowResponse pHttpStatus_ =
  RunJobFlowResponse'
    { clusterArn = Core.Nothing,
      jobFlowId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the cluster.
runJobFlowResponse_clusterArn :: Lens.Lens' RunJobFlowResponse (Core.Maybe Core.Text)
runJobFlowResponse_clusterArn = Lens.lens (\RunJobFlowResponse' {clusterArn} -> clusterArn) (\s@RunJobFlowResponse' {} a -> s {clusterArn = a} :: RunJobFlowResponse)

-- | A unique identifier for the job flow.
runJobFlowResponse_jobFlowId :: Lens.Lens' RunJobFlowResponse (Core.Maybe Core.Text)
runJobFlowResponse_jobFlowId = Lens.lens (\RunJobFlowResponse' {jobFlowId} -> jobFlowId) (\s@RunJobFlowResponse' {} a -> s {jobFlowId = a} :: RunJobFlowResponse)

-- | The response's http status code.
runJobFlowResponse_httpStatus :: Lens.Lens' RunJobFlowResponse Core.Int
runJobFlowResponse_httpStatus = Lens.lens (\RunJobFlowResponse' {httpStatus} -> httpStatus) (\s@RunJobFlowResponse' {} a -> s {httpStatus = a} :: RunJobFlowResponse)

instance Core.NFData RunJobFlowResponse
