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
-- Module      : Amazonka.EMR.RunJobFlow
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
module Amazonka.EMR.RunJobFlow
  ( -- * Creating a Request
    RunJobFlow (..),
    newRunJobFlow,

    -- * Request Lenses
    runJobFlow_logEncryptionKmsKeyId,
    runJobFlow_amiVersion,
    runJobFlow_ebsRootVolumeSize,
    runJobFlow_additionalInfo,
    runJobFlow_autoTerminationPolicy,
    runJobFlow_configurations,
    runJobFlow_customAmiId,
    runJobFlow_autoScalingRole,
    runJobFlow_securityConfiguration,
    runJobFlow_scaleDownBehavior,
    runJobFlow_steps,
    runJobFlow_jobFlowRole,
    runJobFlow_bootstrapActions,
    runJobFlow_releaseLabel,
    runJobFlow_repoUpgradeOnBoot,
    runJobFlow_placementGroupConfigs,
    runJobFlow_logUri,
    runJobFlow_kerberosAttributes,
    runJobFlow_newSupportedProducts,
    runJobFlow_managedScalingPolicy,
    runJobFlow_visibleToAllUsers,
    runJobFlow_supportedProducts,
    runJobFlow_stepConcurrencyLevel,
    runJobFlow_applications,
    runJobFlow_tags,
    runJobFlow_serviceRole,
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

import qualified Amazonka.Core as Core
import Amazonka.EMR.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Input to the RunJobFlow operation.
--
-- /See:/ 'newRunJobFlow' smart constructor.
data RunJobFlow = RunJobFlow'
  { -- | The KMS key used for encrypting log files. If a value is not provided,
    -- the logs remain encrypted by AES-256. This attribute is only available
    -- with Amazon EMR version 5.30.0 and later, excluding Amazon EMR 6.0.0.
    logEncryptionKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Applies only to Amazon EMR AMI versions 3.x and 2.x. For Amazon EMR
    -- releases 4.0 and later, @ReleaseLabel@ is used. To specify a custom AMI,
    -- use @CustomAmiID@.
    amiVersion :: Prelude.Maybe Prelude.Text,
    -- | The size, in GiB, of the Amazon EBS root device volume of the Linux AMI
    -- that is used for each EC2 instance. Available in Amazon EMR version 4.x
    -- and later.
    ebsRootVolumeSize :: Prelude.Maybe Prelude.Int,
    -- | A JSON string for selecting additional features.
    additionalInfo :: Prelude.Maybe Prelude.Text,
    autoTerminationPolicy :: Prelude.Maybe AutoTerminationPolicy,
    -- | For Amazon EMR releases 4.0 and later. The list of configurations
    -- supplied for the EMR cluster you are creating.
    configurations :: Prelude.Maybe [Configuration],
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
    customAmiId :: Prelude.Maybe Prelude.Text,
    -- | An IAM role for automatic scaling policies. The default role is
    -- @EMR_AutoScaling_DefaultRole@. The IAM role provides permissions that
    -- the automatic scaling feature requires to launch and terminate EC2
    -- instances in an instance group.
    autoScalingRole :: Prelude.Maybe Prelude.Text,
    -- | The name of a security configuration to apply to the cluster.
    securityConfiguration :: Prelude.Maybe Prelude.Text,
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
    scaleDownBehavior :: Prelude.Maybe ScaleDownBehavior,
    -- | A list of steps to run.
    steps :: Prelude.Maybe [StepConfig],
    -- | Also called instance profile and EC2 role. An IAM role for an EMR
    -- cluster. The EC2 instances of the cluster assume this role. The default
    -- role is @EMR_EC2_DefaultRole@. In order to use the default role, you
    -- must have already created it using the CLI or console.
    jobFlowRole :: Prelude.Maybe Prelude.Text,
    -- | A list of bootstrap actions to run before Hadoop starts on the cluster
    -- nodes.
    bootstrapActions :: Prelude.Maybe [BootstrapActionConfig],
    -- | The Amazon EMR release label, which determines the version of
    -- open-source application packages installed on the cluster. Release
    -- labels are in the form @emr-x.x.x@, where x.x.x is an Amazon EMR release
    -- version such as @emr-5.14.0@. For more information about Amazon EMR
    -- release versions and included application versions and features, see
    -- <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/>. The release
    -- label applies only to Amazon EMR releases version 4.0 and later. Earlier
    -- versions use @AmiVersion@.
    releaseLabel :: Prelude.Maybe Prelude.Text,
    -- | Applies only when @CustomAmiID@ is used. Specifies which updates from
    -- the Amazon Linux AMI package repositories to apply automatically when
    -- the instance boots using the AMI. If omitted, the default is @SECURITY@,
    -- which indicates that only security updates are applied. If @NONE@ is
    -- specified, no updates are applied, and all updates must be applied
    -- manually.
    repoUpgradeOnBoot :: Prelude.Maybe RepoUpgradeOnBoot,
    -- | The specified placement group configuration for an Amazon EMR cluster.
    placementGroupConfigs :: Prelude.Maybe [PlacementGroupConfig],
    -- | The location in Amazon S3 to write the log files of the job flow. If a
    -- value is not provided, logs are not created.
    logUri :: Prelude.Maybe Prelude.Text,
    -- | Attributes for Kerberos configuration when Kerberos authentication is
    -- enabled using a security configuration. For more information see
    -- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-kerberos.html Use Kerberos Authentication>
    -- in the /Amazon EMR Management Guide/.
    kerberosAttributes :: Prelude.Maybe KerberosAttributes,
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
    newSupportedProducts' :: Prelude.Maybe [SupportedProductConfig],
    -- | The specified managed scaling policy for an Amazon EMR cluster.
    managedScalingPolicy :: Prelude.Maybe ManagedScalingPolicy,
    -- | Set this value to @true@ so that IAM principals in the Amazon Web
    -- Services account associated with the cluster can perform EMR actions on
    -- the cluster that their IAM policies allow. This value defaults to @true@
    -- for clusters created using the EMR API or the CLI
    -- <https://docs.aws.amazon.com/cli/latest/reference/emr/create-cluster.html create-cluster>
    -- command.
    --
    -- When set to @false@, only the IAM principal that created the cluster and
    -- the Amazon Web Services account root user can perform EMR actions for
    -- the cluster, regardless of the IAM permissions policies attached to
    -- other IAM principals. For more information, see
    -- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/security_iam_emr-with-iam.html#security_set_visible_to_all_users Understanding the EMR Cluster VisibleToAllUsers Setting>
    -- in the /Amazon EMRManagement Guide/.
    visibleToAllUsers :: Prelude.Maybe Prelude.Bool,
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
    supportedProducts :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the number of steps that can be executed concurrently. The
    -- default value is @1@. The maximum value is @256@.
    stepConcurrencyLevel :: Prelude.Maybe Prelude.Int,
    -- | Applies to Amazon EMR releases 4.0 and later. A case-insensitive list of
    -- applications for Amazon EMR to install and configure when launching the
    -- cluster. For a list of applications available for each Amazon EMR
    -- release version, see the
    -- <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/ Amazon EMR Release Guide>.
    applications :: Prelude.Maybe [Application],
    -- | A list of tags to associate with a cluster and propagate to Amazon EC2
    -- instances.
    tags :: Prelude.Maybe [Tag],
    -- | The IAM role that Amazon EMR assumes in order to access Amazon Web
    -- Services resources on your behalf.
    serviceRole :: Prelude.Maybe Prelude.Text,
    -- | The name of the job flow.
    name :: Prelude.Text,
    -- | A specification of the number and type of Amazon EC2 instances.
    instances :: JobFlowInstancesConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RunJobFlow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logEncryptionKmsKeyId', 'runJobFlow_logEncryptionKmsKeyId' - The KMS key used for encrypting log files. If a value is not provided,
-- the logs remain encrypted by AES-256. This attribute is only available
-- with Amazon EMR version 5.30.0 and later, excluding Amazon EMR 6.0.0.
--
-- 'amiVersion', 'runJobFlow_amiVersion' - Applies only to Amazon EMR AMI versions 3.x and 2.x. For Amazon EMR
-- releases 4.0 and later, @ReleaseLabel@ is used. To specify a custom AMI,
-- use @CustomAmiID@.
--
-- 'ebsRootVolumeSize', 'runJobFlow_ebsRootVolumeSize' - The size, in GiB, of the Amazon EBS root device volume of the Linux AMI
-- that is used for each EC2 instance. Available in Amazon EMR version 4.x
-- and later.
--
-- 'additionalInfo', 'runJobFlow_additionalInfo' - A JSON string for selecting additional features.
--
-- 'autoTerminationPolicy', 'runJobFlow_autoTerminationPolicy' - Undocumented member.
--
-- 'configurations', 'runJobFlow_configurations' - For Amazon EMR releases 4.0 and later. The list of configurations
-- supplied for the EMR cluster you are creating.
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
-- 'autoScalingRole', 'runJobFlow_autoScalingRole' - An IAM role for automatic scaling policies. The default role is
-- @EMR_AutoScaling_DefaultRole@. The IAM role provides permissions that
-- the automatic scaling feature requires to launch and terminate EC2
-- instances in an instance group.
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
-- 'steps', 'runJobFlow_steps' - A list of steps to run.
--
-- 'jobFlowRole', 'runJobFlow_jobFlowRole' - Also called instance profile and EC2 role. An IAM role for an EMR
-- cluster. The EC2 instances of the cluster assume this role. The default
-- role is @EMR_EC2_DefaultRole@. In order to use the default role, you
-- must have already created it using the CLI or console.
--
-- 'bootstrapActions', 'runJobFlow_bootstrapActions' - A list of bootstrap actions to run before Hadoop starts on the cluster
-- nodes.
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
-- 'repoUpgradeOnBoot', 'runJobFlow_repoUpgradeOnBoot' - Applies only when @CustomAmiID@ is used. Specifies which updates from
-- the Amazon Linux AMI package repositories to apply automatically when
-- the instance boots using the AMI. If omitted, the default is @SECURITY@,
-- which indicates that only security updates are applied. If @NONE@ is
-- specified, no updates are applied, and all updates must be applied
-- manually.
--
-- 'placementGroupConfigs', 'runJobFlow_placementGroupConfigs' - The specified placement group configuration for an Amazon EMR cluster.
--
-- 'logUri', 'runJobFlow_logUri' - The location in Amazon S3 to write the log files of the job flow. If a
-- value is not provided, logs are not created.
--
-- 'kerberosAttributes', 'runJobFlow_kerberosAttributes' - Attributes for Kerberos configuration when Kerberos authentication is
-- enabled using a security configuration. For more information see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-kerberos.html Use Kerberos Authentication>
-- in the /Amazon EMR Management Guide/.
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
-- 'managedScalingPolicy', 'runJobFlow_managedScalingPolicy' - The specified managed scaling policy for an Amazon EMR cluster.
--
-- 'visibleToAllUsers', 'runJobFlow_visibleToAllUsers' - Set this value to @true@ so that IAM principals in the Amazon Web
-- Services account associated with the cluster can perform EMR actions on
-- the cluster that their IAM policies allow. This value defaults to @true@
-- for clusters created using the EMR API or the CLI
-- <https://docs.aws.amazon.com/cli/latest/reference/emr/create-cluster.html create-cluster>
-- command.
--
-- When set to @false@, only the IAM principal that created the cluster and
-- the Amazon Web Services account root user can perform EMR actions for
-- the cluster, regardless of the IAM permissions policies attached to
-- other IAM principals. For more information, see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/security_iam_emr-with-iam.html#security_set_visible_to_all_users Understanding the EMR Cluster VisibleToAllUsers Setting>
-- in the /Amazon EMRManagement Guide/.
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
-- 'stepConcurrencyLevel', 'runJobFlow_stepConcurrencyLevel' - Specifies the number of steps that can be executed concurrently. The
-- default value is @1@. The maximum value is @256@.
--
-- 'applications', 'runJobFlow_applications' - Applies to Amazon EMR releases 4.0 and later. A case-insensitive list of
-- applications for Amazon EMR to install and configure when launching the
-- cluster. For a list of applications available for each Amazon EMR
-- release version, see the
-- <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/ Amazon EMR Release Guide>.
--
-- 'tags', 'runJobFlow_tags' - A list of tags to associate with a cluster and propagate to Amazon EC2
-- instances.
--
-- 'serviceRole', 'runJobFlow_serviceRole' - The IAM role that Amazon EMR assumes in order to access Amazon Web
-- Services resources on your behalf.
--
-- 'name', 'runJobFlow_name' - The name of the job flow.
--
-- 'instances', 'runJobFlow_instances' - A specification of the number and type of Amazon EC2 instances.
newRunJobFlow ::
  -- | 'name'
  Prelude.Text ->
  -- | 'instances'
  JobFlowInstancesConfig ->
  RunJobFlow
newRunJobFlow pName_ pInstances_ =
  RunJobFlow'
    { logEncryptionKmsKeyId =
        Prelude.Nothing,
      amiVersion = Prelude.Nothing,
      ebsRootVolumeSize = Prelude.Nothing,
      additionalInfo = Prelude.Nothing,
      autoTerminationPolicy = Prelude.Nothing,
      configurations = Prelude.Nothing,
      customAmiId = Prelude.Nothing,
      autoScalingRole = Prelude.Nothing,
      securityConfiguration = Prelude.Nothing,
      scaleDownBehavior = Prelude.Nothing,
      steps = Prelude.Nothing,
      jobFlowRole = Prelude.Nothing,
      bootstrapActions = Prelude.Nothing,
      releaseLabel = Prelude.Nothing,
      repoUpgradeOnBoot = Prelude.Nothing,
      placementGroupConfigs = Prelude.Nothing,
      logUri = Prelude.Nothing,
      kerberosAttributes = Prelude.Nothing,
      newSupportedProducts' = Prelude.Nothing,
      managedScalingPolicy = Prelude.Nothing,
      visibleToAllUsers = Prelude.Nothing,
      supportedProducts = Prelude.Nothing,
      stepConcurrencyLevel = Prelude.Nothing,
      applications = Prelude.Nothing,
      tags = Prelude.Nothing,
      serviceRole = Prelude.Nothing,
      name = pName_,
      instances = pInstances_
    }

-- | The KMS key used for encrypting log files. If a value is not provided,
-- the logs remain encrypted by AES-256. This attribute is only available
-- with Amazon EMR version 5.30.0 and later, excluding Amazon EMR 6.0.0.
runJobFlow_logEncryptionKmsKeyId :: Lens.Lens' RunJobFlow (Prelude.Maybe Prelude.Text)
runJobFlow_logEncryptionKmsKeyId = Lens.lens (\RunJobFlow' {logEncryptionKmsKeyId} -> logEncryptionKmsKeyId) (\s@RunJobFlow' {} a -> s {logEncryptionKmsKeyId = a} :: RunJobFlow)

-- | Applies only to Amazon EMR AMI versions 3.x and 2.x. For Amazon EMR
-- releases 4.0 and later, @ReleaseLabel@ is used. To specify a custom AMI,
-- use @CustomAmiID@.
runJobFlow_amiVersion :: Lens.Lens' RunJobFlow (Prelude.Maybe Prelude.Text)
runJobFlow_amiVersion = Lens.lens (\RunJobFlow' {amiVersion} -> amiVersion) (\s@RunJobFlow' {} a -> s {amiVersion = a} :: RunJobFlow)

-- | The size, in GiB, of the Amazon EBS root device volume of the Linux AMI
-- that is used for each EC2 instance. Available in Amazon EMR version 4.x
-- and later.
runJobFlow_ebsRootVolumeSize :: Lens.Lens' RunJobFlow (Prelude.Maybe Prelude.Int)
runJobFlow_ebsRootVolumeSize = Lens.lens (\RunJobFlow' {ebsRootVolumeSize} -> ebsRootVolumeSize) (\s@RunJobFlow' {} a -> s {ebsRootVolumeSize = a} :: RunJobFlow)

-- | A JSON string for selecting additional features.
runJobFlow_additionalInfo :: Lens.Lens' RunJobFlow (Prelude.Maybe Prelude.Text)
runJobFlow_additionalInfo = Lens.lens (\RunJobFlow' {additionalInfo} -> additionalInfo) (\s@RunJobFlow' {} a -> s {additionalInfo = a} :: RunJobFlow)

-- | Undocumented member.
runJobFlow_autoTerminationPolicy :: Lens.Lens' RunJobFlow (Prelude.Maybe AutoTerminationPolicy)
runJobFlow_autoTerminationPolicy = Lens.lens (\RunJobFlow' {autoTerminationPolicy} -> autoTerminationPolicy) (\s@RunJobFlow' {} a -> s {autoTerminationPolicy = a} :: RunJobFlow)

-- | For Amazon EMR releases 4.0 and later. The list of configurations
-- supplied for the EMR cluster you are creating.
runJobFlow_configurations :: Lens.Lens' RunJobFlow (Prelude.Maybe [Configuration])
runJobFlow_configurations = Lens.lens (\RunJobFlow' {configurations} -> configurations) (\s@RunJobFlow' {} a -> s {configurations = a} :: RunJobFlow) Prelude.. Lens.mapping Lens.coerced

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
runJobFlow_customAmiId :: Lens.Lens' RunJobFlow (Prelude.Maybe Prelude.Text)
runJobFlow_customAmiId = Lens.lens (\RunJobFlow' {customAmiId} -> customAmiId) (\s@RunJobFlow' {} a -> s {customAmiId = a} :: RunJobFlow)

-- | An IAM role for automatic scaling policies. The default role is
-- @EMR_AutoScaling_DefaultRole@. The IAM role provides permissions that
-- the automatic scaling feature requires to launch and terminate EC2
-- instances in an instance group.
runJobFlow_autoScalingRole :: Lens.Lens' RunJobFlow (Prelude.Maybe Prelude.Text)
runJobFlow_autoScalingRole = Lens.lens (\RunJobFlow' {autoScalingRole} -> autoScalingRole) (\s@RunJobFlow' {} a -> s {autoScalingRole = a} :: RunJobFlow)

-- | The name of a security configuration to apply to the cluster.
runJobFlow_securityConfiguration :: Lens.Lens' RunJobFlow (Prelude.Maybe Prelude.Text)
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
runJobFlow_scaleDownBehavior :: Lens.Lens' RunJobFlow (Prelude.Maybe ScaleDownBehavior)
runJobFlow_scaleDownBehavior = Lens.lens (\RunJobFlow' {scaleDownBehavior} -> scaleDownBehavior) (\s@RunJobFlow' {} a -> s {scaleDownBehavior = a} :: RunJobFlow)

-- | A list of steps to run.
runJobFlow_steps :: Lens.Lens' RunJobFlow (Prelude.Maybe [StepConfig])
runJobFlow_steps = Lens.lens (\RunJobFlow' {steps} -> steps) (\s@RunJobFlow' {} a -> s {steps = a} :: RunJobFlow) Prelude.. Lens.mapping Lens.coerced

-- | Also called instance profile and EC2 role. An IAM role for an EMR
-- cluster. The EC2 instances of the cluster assume this role. The default
-- role is @EMR_EC2_DefaultRole@. In order to use the default role, you
-- must have already created it using the CLI or console.
runJobFlow_jobFlowRole :: Lens.Lens' RunJobFlow (Prelude.Maybe Prelude.Text)
runJobFlow_jobFlowRole = Lens.lens (\RunJobFlow' {jobFlowRole} -> jobFlowRole) (\s@RunJobFlow' {} a -> s {jobFlowRole = a} :: RunJobFlow)

-- | A list of bootstrap actions to run before Hadoop starts on the cluster
-- nodes.
runJobFlow_bootstrapActions :: Lens.Lens' RunJobFlow (Prelude.Maybe [BootstrapActionConfig])
runJobFlow_bootstrapActions = Lens.lens (\RunJobFlow' {bootstrapActions} -> bootstrapActions) (\s@RunJobFlow' {} a -> s {bootstrapActions = a} :: RunJobFlow) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon EMR release label, which determines the version of
-- open-source application packages installed on the cluster. Release
-- labels are in the form @emr-x.x.x@, where x.x.x is an Amazon EMR release
-- version such as @emr-5.14.0@. For more information about Amazon EMR
-- release versions and included application versions and features, see
-- <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/>. The release
-- label applies only to Amazon EMR releases version 4.0 and later. Earlier
-- versions use @AmiVersion@.
runJobFlow_releaseLabel :: Lens.Lens' RunJobFlow (Prelude.Maybe Prelude.Text)
runJobFlow_releaseLabel = Lens.lens (\RunJobFlow' {releaseLabel} -> releaseLabel) (\s@RunJobFlow' {} a -> s {releaseLabel = a} :: RunJobFlow)

-- | Applies only when @CustomAmiID@ is used. Specifies which updates from
-- the Amazon Linux AMI package repositories to apply automatically when
-- the instance boots using the AMI. If omitted, the default is @SECURITY@,
-- which indicates that only security updates are applied. If @NONE@ is
-- specified, no updates are applied, and all updates must be applied
-- manually.
runJobFlow_repoUpgradeOnBoot :: Lens.Lens' RunJobFlow (Prelude.Maybe RepoUpgradeOnBoot)
runJobFlow_repoUpgradeOnBoot = Lens.lens (\RunJobFlow' {repoUpgradeOnBoot} -> repoUpgradeOnBoot) (\s@RunJobFlow' {} a -> s {repoUpgradeOnBoot = a} :: RunJobFlow)

-- | The specified placement group configuration for an Amazon EMR cluster.
runJobFlow_placementGroupConfigs :: Lens.Lens' RunJobFlow (Prelude.Maybe [PlacementGroupConfig])
runJobFlow_placementGroupConfigs = Lens.lens (\RunJobFlow' {placementGroupConfigs} -> placementGroupConfigs) (\s@RunJobFlow' {} a -> s {placementGroupConfigs = a} :: RunJobFlow) Prelude.. Lens.mapping Lens.coerced

-- | The location in Amazon S3 to write the log files of the job flow. If a
-- value is not provided, logs are not created.
runJobFlow_logUri :: Lens.Lens' RunJobFlow (Prelude.Maybe Prelude.Text)
runJobFlow_logUri = Lens.lens (\RunJobFlow' {logUri} -> logUri) (\s@RunJobFlow' {} a -> s {logUri = a} :: RunJobFlow)

-- | Attributes for Kerberos configuration when Kerberos authentication is
-- enabled using a security configuration. For more information see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-kerberos.html Use Kerberos Authentication>
-- in the /Amazon EMR Management Guide/.
runJobFlow_kerberosAttributes :: Lens.Lens' RunJobFlow (Prelude.Maybe KerberosAttributes)
runJobFlow_kerberosAttributes = Lens.lens (\RunJobFlow' {kerberosAttributes} -> kerberosAttributes) (\s@RunJobFlow' {} a -> s {kerberosAttributes = a} :: RunJobFlow)

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
runJobFlow_newSupportedProducts :: Lens.Lens' RunJobFlow (Prelude.Maybe [SupportedProductConfig])
runJobFlow_newSupportedProducts = Lens.lens (\RunJobFlow' {newSupportedProducts'} -> newSupportedProducts') (\s@RunJobFlow' {} a -> s {newSupportedProducts' = a} :: RunJobFlow) Prelude.. Lens.mapping Lens.coerced

-- | The specified managed scaling policy for an Amazon EMR cluster.
runJobFlow_managedScalingPolicy :: Lens.Lens' RunJobFlow (Prelude.Maybe ManagedScalingPolicy)
runJobFlow_managedScalingPolicy = Lens.lens (\RunJobFlow' {managedScalingPolicy} -> managedScalingPolicy) (\s@RunJobFlow' {} a -> s {managedScalingPolicy = a} :: RunJobFlow)

-- | Set this value to @true@ so that IAM principals in the Amazon Web
-- Services account associated with the cluster can perform EMR actions on
-- the cluster that their IAM policies allow. This value defaults to @true@
-- for clusters created using the EMR API or the CLI
-- <https://docs.aws.amazon.com/cli/latest/reference/emr/create-cluster.html create-cluster>
-- command.
--
-- When set to @false@, only the IAM principal that created the cluster and
-- the Amazon Web Services account root user can perform EMR actions for
-- the cluster, regardless of the IAM permissions policies attached to
-- other IAM principals. For more information, see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/security_iam_emr-with-iam.html#security_set_visible_to_all_users Understanding the EMR Cluster VisibleToAllUsers Setting>
-- in the /Amazon EMRManagement Guide/.
runJobFlow_visibleToAllUsers :: Lens.Lens' RunJobFlow (Prelude.Maybe Prelude.Bool)
runJobFlow_visibleToAllUsers = Lens.lens (\RunJobFlow' {visibleToAllUsers} -> visibleToAllUsers) (\s@RunJobFlow' {} a -> s {visibleToAllUsers = a} :: RunJobFlow)

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
runJobFlow_supportedProducts :: Lens.Lens' RunJobFlow (Prelude.Maybe [Prelude.Text])
runJobFlow_supportedProducts = Lens.lens (\RunJobFlow' {supportedProducts} -> supportedProducts) (\s@RunJobFlow' {} a -> s {supportedProducts = a} :: RunJobFlow) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the number of steps that can be executed concurrently. The
-- default value is @1@. The maximum value is @256@.
runJobFlow_stepConcurrencyLevel :: Lens.Lens' RunJobFlow (Prelude.Maybe Prelude.Int)
runJobFlow_stepConcurrencyLevel = Lens.lens (\RunJobFlow' {stepConcurrencyLevel} -> stepConcurrencyLevel) (\s@RunJobFlow' {} a -> s {stepConcurrencyLevel = a} :: RunJobFlow)

-- | Applies to Amazon EMR releases 4.0 and later. A case-insensitive list of
-- applications for Amazon EMR to install and configure when launching the
-- cluster. For a list of applications available for each Amazon EMR
-- release version, see the
-- <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/ Amazon EMR Release Guide>.
runJobFlow_applications :: Lens.Lens' RunJobFlow (Prelude.Maybe [Application])
runJobFlow_applications = Lens.lens (\RunJobFlow' {applications} -> applications) (\s@RunJobFlow' {} a -> s {applications = a} :: RunJobFlow) Prelude.. Lens.mapping Lens.coerced

-- | A list of tags to associate with a cluster and propagate to Amazon EC2
-- instances.
runJobFlow_tags :: Lens.Lens' RunJobFlow (Prelude.Maybe [Tag])
runJobFlow_tags = Lens.lens (\RunJobFlow' {tags} -> tags) (\s@RunJobFlow' {} a -> s {tags = a} :: RunJobFlow) Prelude.. Lens.mapping Lens.coerced

-- | The IAM role that Amazon EMR assumes in order to access Amazon Web
-- Services resources on your behalf.
runJobFlow_serviceRole :: Lens.Lens' RunJobFlow (Prelude.Maybe Prelude.Text)
runJobFlow_serviceRole = Lens.lens (\RunJobFlow' {serviceRole} -> serviceRole) (\s@RunJobFlow' {} a -> s {serviceRole = a} :: RunJobFlow)

-- | The name of the job flow.
runJobFlow_name :: Lens.Lens' RunJobFlow Prelude.Text
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
            Prelude.<$> (x Core..?> "ClusterArn")
            Prelude.<*> (x Core..?> "JobFlowId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RunJobFlow

instance Prelude.NFData RunJobFlow

instance Core.ToHeaders RunJobFlow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ElasticMapReduce.RunJobFlow" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RunJobFlow where
  toJSON RunJobFlow' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("LogEncryptionKmsKeyId" Core..=)
              Prelude.<$> logEncryptionKmsKeyId,
            ("AmiVersion" Core..=) Prelude.<$> amiVersion,
            ("EbsRootVolumeSize" Core..=)
              Prelude.<$> ebsRootVolumeSize,
            ("AdditionalInfo" Core..=)
              Prelude.<$> additionalInfo,
            ("AutoTerminationPolicy" Core..=)
              Prelude.<$> autoTerminationPolicy,
            ("Configurations" Core..=)
              Prelude.<$> configurations,
            ("CustomAmiId" Core..=) Prelude.<$> customAmiId,
            ("AutoScalingRole" Core..=)
              Prelude.<$> autoScalingRole,
            ("SecurityConfiguration" Core..=)
              Prelude.<$> securityConfiguration,
            ("ScaleDownBehavior" Core..=)
              Prelude.<$> scaleDownBehavior,
            ("Steps" Core..=) Prelude.<$> steps,
            ("JobFlowRole" Core..=) Prelude.<$> jobFlowRole,
            ("BootstrapActions" Core..=)
              Prelude.<$> bootstrapActions,
            ("ReleaseLabel" Core..=) Prelude.<$> releaseLabel,
            ("RepoUpgradeOnBoot" Core..=)
              Prelude.<$> repoUpgradeOnBoot,
            ("PlacementGroupConfigs" Core..=)
              Prelude.<$> placementGroupConfigs,
            ("LogUri" Core..=) Prelude.<$> logUri,
            ("KerberosAttributes" Core..=)
              Prelude.<$> kerberosAttributes,
            ("NewSupportedProducts" Core..=)
              Prelude.<$> newSupportedProducts',
            ("ManagedScalingPolicy" Core..=)
              Prelude.<$> managedScalingPolicy,
            ("VisibleToAllUsers" Core..=)
              Prelude.<$> visibleToAllUsers,
            ("SupportedProducts" Core..=)
              Prelude.<$> supportedProducts,
            ("StepConcurrencyLevel" Core..=)
              Prelude.<$> stepConcurrencyLevel,
            ("Applications" Core..=) Prelude.<$> applications,
            ("Tags" Core..=) Prelude.<$> tags,
            ("ServiceRole" Core..=) Prelude.<$> serviceRole,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("Instances" Core..= instances)
          ]
      )

instance Core.ToPath RunJobFlow where
  toPath = Prelude.const "/"

instance Core.ToQuery RunJobFlow where
  toQuery = Prelude.const Prelude.mempty

-- | The result of the RunJobFlow operation.
--
-- /See:/ 'newRunJobFlowResponse' smart constructor.
data RunJobFlowResponse = RunJobFlowResponse'
  { -- | The Amazon Resource Name (ARN) of the cluster.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the job flow.
    jobFlowId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  RunJobFlowResponse
newRunJobFlowResponse pHttpStatus_ =
  RunJobFlowResponse'
    { clusterArn = Prelude.Nothing,
      jobFlowId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the cluster.
runJobFlowResponse_clusterArn :: Lens.Lens' RunJobFlowResponse (Prelude.Maybe Prelude.Text)
runJobFlowResponse_clusterArn = Lens.lens (\RunJobFlowResponse' {clusterArn} -> clusterArn) (\s@RunJobFlowResponse' {} a -> s {clusterArn = a} :: RunJobFlowResponse)

-- | A unique identifier for the job flow.
runJobFlowResponse_jobFlowId :: Lens.Lens' RunJobFlowResponse (Prelude.Maybe Prelude.Text)
runJobFlowResponse_jobFlowId = Lens.lens (\RunJobFlowResponse' {jobFlowId} -> jobFlowId) (\s@RunJobFlowResponse' {} a -> s {jobFlowId = a} :: RunJobFlowResponse)

-- | The response's http status code.
runJobFlowResponse_httpStatus :: Lens.Lens' RunJobFlowResponse Prelude.Int
runJobFlowResponse_httpStatus = Lens.lens (\RunJobFlowResponse' {httpStatus} -> httpStatus) (\s@RunJobFlowResponse' {} a -> s {httpStatus = a} :: RunJobFlowResponse)

instance Prelude.NFData RunJobFlowResponse
