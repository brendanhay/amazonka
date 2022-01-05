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
-- Module      : Amazonka.MwAA.CreateEnvironment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Managed Workflows for Apache Airflow (MWAA)
-- environment.
module Amazonka.MwAA.CreateEnvironment
  ( -- * Creating a Request
    CreateEnvironment (..),
    newCreateEnvironment,

    -- * Request Lenses
    createEnvironment_schedulers,
    createEnvironment_minWorkers,
    createEnvironment_pluginsS3Path,
    createEnvironment_webserverAccessMode,
    createEnvironment_airflowVersion,
    createEnvironment_kmsKey,
    createEnvironment_weeklyMaintenanceWindowStart,
    createEnvironment_requirementsS3ObjectVersion,
    createEnvironment_pluginsS3ObjectVersion,
    createEnvironment_airflowConfigurationOptions,
    createEnvironment_loggingConfiguration,
    createEnvironment_environmentClass,
    createEnvironment_tags,
    createEnvironment_requirementsS3Path,
    createEnvironment_maxWorkers,
    createEnvironment_dagS3Path,
    createEnvironment_executionRoleArn,
    createEnvironment_name,
    createEnvironment_networkConfiguration,
    createEnvironment_sourceBucketArn,

    -- * Destructuring the Response
    CreateEnvironmentResponse (..),
    newCreateEnvironmentResponse,

    -- * Response Lenses
    createEnvironmentResponse_arn,
    createEnvironmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MwAA.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | This section contains the Amazon Managed Workflows for Apache Airflow
-- (MWAA) API reference documentation to create an environment. For more
-- information, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/get-started.html Get started with Amazon Managed Workflows for Apache Airflow>.
--
-- /See:/ 'newCreateEnvironment' smart constructor.
data CreateEnvironment = CreateEnvironment'
  { -- | The number of Apache Airflow schedulers to run in your environment.
    schedulers :: Prelude.Maybe Prelude.Int,
    -- | The minimum number of workers that you want to run in your environment.
    -- MWAA scales the number of Apache Airflow workers up to the number you
    -- specify in the @MaxWorkers@ field. When there are no more tasks running,
    -- and no more in the queue, MWAA disposes of the extra workers leaving the
    -- worker count you specify in the @MinWorkers@ field. For example, @2@.
    minWorkers :: Prelude.Maybe Prelude.Natural,
    -- | The relative path to the @plugins.zip@ file on your Amazon S3 bucket.
    -- For example, @plugins.zip@. If specified, then the plugins.zip version
    -- is required. To learn more, see
    -- <https://docs.aws.amazon.com/mwaa/latest/userguide/configuring-dag-import-plugins.html Installing custom plugins>.
    pluginsS3Path :: Prelude.Maybe Prelude.Text,
    -- | The Apache Airflow /Web server/ access mode. To learn more, see
    -- <https://docs.aws.amazon.com/mwaa/latest/userguide/configuring-networking.html Apache Airflow access modes>.
    webserverAccessMode :: Prelude.Maybe WebserverAccessMode,
    -- | The Apache Airflow version for your environment. For example,
    -- @v1.10.12@. If no value is specified, defaults to the latest version.
    -- Valid values: @v1.10.12@.
    airflowVersion :: Prelude.Maybe Prelude.Text,
    -- | The AWS Key Management Service (KMS) key to encrypt the data in your
    -- environment. You can use an AWS owned CMK, or a Customer managed CMK
    -- (advanced). To learn more, see
    -- <https://docs.aws.amazon.com/mwaa/latest/userguide/get-started.html Get started with Amazon Managed Workflows for Apache Airflow>.
    kmsKey :: Prelude.Maybe Prelude.Text,
    -- | The day and time of the week to start weekly maintenance updates of your
    -- environment in the following format: @DAY:HH:MM@. For example:
    -- @TUE:03:30@. You can specify a start time in 30 minute increments only.
    -- Supported input includes the following:
    --
    -- -   MON|TUE|WED|THU|FRI|SAT|SUN:([01]\\\\d|2[0-3]):(00|30)
    weeklyMaintenanceWindowStart :: Prelude.Maybe Prelude.Text,
    -- | The version of the requirements.txt file on your Amazon S3 bucket. A
    -- version must be specified each time a requirements.txt file is updated.
    -- To learn more, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/versioning-workflows.html How S3 Versioning works>.
    requirementsS3ObjectVersion :: Prelude.Maybe Prelude.Text,
    -- | The version of the plugins.zip file on your Amazon S3 bucket. A version
    -- must be specified each time a plugins.zip file is updated. To learn
    -- more, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/versioning-workflows.html How S3 Versioning works>.
    pluginsS3ObjectVersion :: Prelude.Maybe Prelude.Text,
    -- | A list of key-value pairs containing the Apache Airflow configuration
    -- options you want to attach to your environment. To learn more, see
    -- <https://docs.aws.amazon.com/mwaa/latest/userguide/configuring-env-variables.html Apache Airflow configuration options>.
    airflowConfigurationOptions :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Defines the Apache Airflow logs to send to CloudWatch Logs:
    -- @DagProcessingLogs@, @SchedulerLogs@, @TaskLogs@, @WebserverLogs@,
    -- @WorkerLogs@.
    loggingConfiguration :: Prelude.Maybe LoggingConfigurationInput,
    -- | The environment class type. Valid values: @mw1.small@, @mw1.medium@,
    -- @mw1.large@. To learn more, see
    -- <https://docs.aws.amazon.com/mwaa/latest/userguide/environment-class.html Amazon MWAA environment class>.
    environmentClass :: Prelude.Maybe Prelude.Text,
    -- | The key-value tag pairs you want to associate to your environment. For
    -- example, @\"Environment\": \"Staging\"@. To learn more, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources>.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The relative path to the @requirements.txt@ file on your Amazon S3
    -- bucket. For example, @requirements.txt@. If specified, then a file
    -- version is required. To learn more, see
    -- <https://docs.aws.amazon.com/mwaa/latest/userguide/working-dags-dependencies.html Installing Python dependencies>.
    requirementsS3Path :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of workers that you want to run in your environment.
    -- MWAA scales the number of Apache Airflow workers up to the number you
    -- specify in the @MaxWorkers@ field. For example, @20@. When there are no
    -- more tasks running, and no more in the queue, MWAA disposes of the extra
    -- workers leaving the one worker that is included with your environment,
    -- or the number you specify in @MinWorkers@.
    maxWorkers :: Prelude.Maybe Prelude.Natural,
    -- | The relative path to the DAGs folder on your Amazon S3 bucket. For
    -- example, @dags@. To learn more, see
    -- <https://docs.aws.amazon.com/mwaa/latest/userguide/configuring-dag-folder.html Adding or updating DAGs>.
    dagS3Path :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the execution role for your
    -- environment. An execution role is an AWS Identity and Access Management
    -- (IAM) role that grants MWAA permission to access AWS services and
    -- resources used by your environment. For example,
    -- @arn:aws:iam::123456789:role\/my-execution-role@. To learn more, see
    -- <https://docs.aws.amazon.com/mwaa/latest/userguide/mwaa-create-role.html Amazon MWAA Execution role>.
    executionRoleArn :: Prelude.Text,
    -- | The name of the Amazon MWAA environment. For example,
    -- @MyMWAAEnvironment@.
    name :: Prelude.Text,
    -- | The VPC networking components used to secure and enable network traffic
    -- between the AWS resources for your environment. To learn more, see
    -- <https://docs.aws.amazon.com/mwaa/latest/userguide/networking-about.html About networking on Amazon MWAA>.
    networkConfiguration :: NetworkConfiguration,
    -- | The Amazon Resource Name (ARN) of the Amazon S3 bucket where your DAG
    -- code and supporting files are stored. For example,
    -- @arn:aws:s3:::my-airflow-bucket-unique-name@. To learn more, see
    -- <https://docs.aws.amazon.com/mwaa/latest/userguide/mwaa-s3-bucket.html Create an Amazon S3 bucket for Amazon MWAA>.
    sourceBucketArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schedulers', 'createEnvironment_schedulers' - The number of Apache Airflow schedulers to run in your environment.
--
-- 'minWorkers', 'createEnvironment_minWorkers' - The minimum number of workers that you want to run in your environment.
-- MWAA scales the number of Apache Airflow workers up to the number you
-- specify in the @MaxWorkers@ field. When there are no more tasks running,
-- and no more in the queue, MWAA disposes of the extra workers leaving the
-- worker count you specify in the @MinWorkers@ field. For example, @2@.
--
-- 'pluginsS3Path', 'createEnvironment_pluginsS3Path' - The relative path to the @plugins.zip@ file on your Amazon S3 bucket.
-- For example, @plugins.zip@. If specified, then the plugins.zip version
-- is required. To learn more, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/configuring-dag-import-plugins.html Installing custom plugins>.
--
-- 'webserverAccessMode', 'createEnvironment_webserverAccessMode' - The Apache Airflow /Web server/ access mode. To learn more, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/configuring-networking.html Apache Airflow access modes>.
--
-- 'airflowVersion', 'createEnvironment_airflowVersion' - The Apache Airflow version for your environment. For example,
-- @v1.10.12@. If no value is specified, defaults to the latest version.
-- Valid values: @v1.10.12@.
--
-- 'kmsKey', 'createEnvironment_kmsKey' - The AWS Key Management Service (KMS) key to encrypt the data in your
-- environment. You can use an AWS owned CMK, or a Customer managed CMK
-- (advanced). To learn more, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/get-started.html Get started with Amazon Managed Workflows for Apache Airflow>.
--
-- 'weeklyMaintenanceWindowStart', 'createEnvironment_weeklyMaintenanceWindowStart' - The day and time of the week to start weekly maintenance updates of your
-- environment in the following format: @DAY:HH:MM@. For example:
-- @TUE:03:30@. You can specify a start time in 30 minute increments only.
-- Supported input includes the following:
--
-- -   MON|TUE|WED|THU|FRI|SAT|SUN:([01]\\\\d|2[0-3]):(00|30)
--
-- 'requirementsS3ObjectVersion', 'createEnvironment_requirementsS3ObjectVersion' - The version of the requirements.txt file on your Amazon S3 bucket. A
-- version must be specified each time a requirements.txt file is updated.
-- To learn more, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/versioning-workflows.html How S3 Versioning works>.
--
-- 'pluginsS3ObjectVersion', 'createEnvironment_pluginsS3ObjectVersion' - The version of the plugins.zip file on your Amazon S3 bucket. A version
-- must be specified each time a plugins.zip file is updated. To learn
-- more, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/versioning-workflows.html How S3 Versioning works>.
--
-- 'airflowConfigurationOptions', 'createEnvironment_airflowConfigurationOptions' - A list of key-value pairs containing the Apache Airflow configuration
-- options you want to attach to your environment. To learn more, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/configuring-env-variables.html Apache Airflow configuration options>.
--
-- 'loggingConfiguration', 'createEnvironment_loggingConfiguration' - Defines the Apache Airflow logs to send to CloudWatch Logs:
-- @DagProcessingLogs@, @SchedulerLogs@, @TaskLogs@, @WebserverLogs@,
-- @WorkerLogs@.
--
-- 'environmentClass', 'createEnvironment_environmentClass' - The environment class type. Valid values: @mw1.small@, @mw1.medium@,
-- @mw1.large@. To learn more, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/environment-class.html Amazon MWAA environment class>.
--
-- 'tags', 'createEnvironment_tags' - The key-value tag pairs you want to associate to your environment. For
-- example, @\"Environment\": \"Staging\"@. To learn more, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources>.
--
-- 'requirementsS3Path', 'createEnvironment_requirementsS3Path' - The relative path to the @requirements.txt@ file on your Amazon S3
-- bucket. For example, @requirements.txt@. If specified, then a file
-- version is required. To learn more, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/working-dags-dependencies.html Installing Python dependencies>.
--
-- 'maxWorkers', 'createEnvironment_maxWorkers' - The maximum number of workers that you want to run in your environment.
-- MWAA scales the number of Apache Airflow workers up to the number you
-- specify in the @MaxWorkers@ field. For example, @20@. When there are no
-- more tasks running, and no more in the queue, MWAA disposes of the extra
-- workers leaving the one worker that is included with your environment,
-- or the number you specify in @MinWorkers@.
--
-- 'dagS3Path', 'createEnvironment_dagS3Path' - The relative path to the DAGs folder on your Amazon S3 bucket. For
-- example, @dags@. To learn more, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/configuring-dag-folder.html Adding or updating DAGs>.
--
-- 'executionRoleArn', 'createEnvironment_executionRoleArn' - The Amazon Resource Name (ARN) of the execution role for your
-- environment. An execution role is an AWS Identity and Access Management
-- (IAM) role that grants MWAA permission to access AWS services and
-- resources used by your environment. For example,
-- @arn:aws:iam::123456789:role\/my-execution-role@. To learn more, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/mwaa-create-role.html Amazon MWAA Execution role>.
--
-- 'name', 'createEnvironment_name' - The name of the Amazon MWAA environment. For example,
-- @MyMWAAEnvironment@.
--
-- 'networkConfiguration', 'createEnvironment_networkConfiguration' - The VPC networking components used to secure and enable network traffic
-- between the AWS resources for your environment. To learn more, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/networking-about.html About networking on Amazon MWAA>.
--
-- 'sourceBucketArn', 'createEnvironment_sourceBucketArn' - The Amazon Resource Name (ARN) of the Amazon S3 bucket where your DAG
-- code and supporting files are stored. For example,
-- @arn:aws:s3:::my-airflow-bucket-unique-name@. To learn more, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/mwaa-s3-bucket.html Create an Amazon S3 bucket for Amazon MWAA>.
newCreateEnvironment ::
  -- | 'dagS3Path'
  Prelude.Text ->
  -- | 'executionRoleArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'networkConfiguration'
  NetworkConfiguration ->
  -- | 'sourceBucketArn'
  Prelude.Text ->
  CreateEnvironment
newCreateEnvironment
  pDagS3Path_
  pExecutionRoleArn_
  pName_
  pNetworkConfiguration_
  pSourceBucketArn_ =
    CreateEnvironment'
      { schedulers = Prelude.Nothing,
        minWorkers = Prelude.Nothing,
        pluginsS3Path = Prelude.Nothing,
        webserverAccessMode = Prelude.Nothing,
        airflowVersion = Prelude.Nothing,
        kmsKey = Prelude.Nothing,
        weeklyMaintenanceWindowStart = Prelude.Nothing,
        requirementsS3ObjectVersion = Prelude.Nothing,
        pluginsS3ObjectVersion = Prelude.Nothing,
        airflowConfigurationOptions = Prelude.Nothing,
        loggingConfiguration = Prelude.Nothing,
        environmentClass = Prelude.Nothing,
        tags = Prelude.Nothing,
        requirementsS3Path = Prelude.Nothing,
        maxWorkers = Prelude.Nothing,
        dagS3Path = pDagS3Path_,
        executionRoleArn = pExecutionRoleArn_,
        name = pName_,
        networkConfiguration = pNetworkConfiguration_,
        sourceBucketArn = pSourceBucketArn_
      }

-- | The number of Apache Airflow schedulers to run in your environment.
createEnvironment_schedulers :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Int)
createEnvironment_schedulers = Lens.lens (\CreateEnvironment' {schedulers} -> schedulers) (\s@CreateEnvironment' {} a -> s {schedulers = a} :: CreateEnvironment)

-- | The minimum number of workers that you want to run in your environment.
-- MWAA scales the number of Apache Airflow workers up to the number you
-- specify in the @MaxWorkers@ field. When there are no more tasks running,
-- and no more in the queue, MWAA disposes of the extra workers leaving the
-- worker count you specify in the @MinWorkers@ field. For example, @2@.
createEnvironment_minWorkers :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Natural)
createEnvironment_minWorkers = Lens.lens (\CreateEnvironment' {minWorkers} -> minWorkers) (\s@CreateEnvironment' {} a -> s {minWorkers = a} :: CreateEnvironment)

-- | The relative path to the @plugins.zip@ file on your Amazon S3 bucket.
-- For example, @plugins.zip@. If specified, then the plugins.zip version
-- is required. To learn more, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/configuring-dag-import-plugins.html Installing custom plugins>.
createEnvironment_pluginsS3Path :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_pluginsS3Path = Lens.lens (\CreateEnvironment' {pluginsS3Path} -> pluginsS3Path) (\s@CreateEnvironment' {} a -> s {pluginsS3Path = a} :: CreateEnvironment)

-- | The Apache Airflow /Web server/ access mode. To learn more, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/configuring-networking.html Apache Airflow access modes>.
createEnvironment_webserverAccessMode :: Lens.Lens' CreateEnvironment (Prelude.Maybe WebserverAccessMode)
createEnvironment_webserverAccessMode = Lens.lens (\CreateEnvironment' {webserverAccessMode} -> webserverAccessMode) (\s@CreateEnvironment' {} a -> s {webserverAccessMode = a} :: CreateEnvironment)

-- | The Apache Airflow version for your environment. For example,
-- @v1.10.12@. If no value is specified, defaults to the latest version.
-- Valid values: @v1.10.12@.
createEnvironment_airflowVersion :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_airflowVersion = Lens.lens (\CreateEnvironment' {airflowVersion} -> airflowVersion) (\s@CreateEnvironment' {} a -> s {airflowVersion = a} :: CreateEnvironment)

-- | The AWS Key Management Service (KMS) key to encrypt the data in your
-- environment. You can use an AWS owned CMK, or a Customer managed CMK
-- (advanced). To learn more, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/get-started.html Get started with Amazon Managed Workflows for Apache Airflow>.
createEnvironment_kmsKey :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_kmsKey = Lens.lens (\CreateEnvironment' {kmsKey} -> kmsKey) (\s@CreateEnvironment' {} a -> s {kmsKey = a} :: CreateEnvironment)

-- | The day and time of the week to start weekly maintenance updates of your
-- environment in the following format: @DAY:HH:MM@. For example:
-- @TUE:03:30@. You can specify a start time in 30 minute increments only.
-- Supported input includes the following:
--
-- -   MON|TUE|WED|THU|FRI|SAT|SUN:([01]\\\\d|2[0-3]):(00|30)
createEnvironment_weeklyMaintenanceWindowStart :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_weeklyMaintenanceWindowStart = Lens.lens (\CreateEnvironment' {weeklyMaintenanceWindowStart} -> weeklyMaintenanceWindowStart) (\s@CreateEnvironment' {} a -> s {weeklyMaintenanceWindowStart = a} :: CreateEnvironment)

-- | The version of the requirements.txt file on your Amazon S3 bucket. A
-- version must be specified each time a requirements.txt file is updated.
-- To learn more, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/versioning-workflows.html How S3 Versioning works>.
createEnvironment_requirementsS3ObjectVersion :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_requirementsS3ObjectVersion = Lens.lens (\CreateEnvironment' {requirementsS3ObjectVersion} -> requirementsS3ObjectVersion) (\s@CreateEnvironment' {} a -> s {requirementsS3ObjectVersion = a} :: CreateEnvironment)

-- | The version of the plugins.zip file on your Amazon S3 bucket. A version
-- must be specified each time a plugins.zip file is updated. To learn
-- more, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/versioning-workflows.html How S3 Versioning works>.
createEnvironment_pluginsS3ObjectVersion :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_pluginsS3ObjectVersion = Lens.lens (\CreateEnvironment' {pluginsS3ObjectVersion} -> pluginsS3ObjectVersion) (\s@CreateEnvironment' {} a -> s {pluginsS3ObjectVersion = a} :: CreateEnvironment)

-- | A list of key-value pairs containing the Apache Airflow configuration
-- options you want to attach to your environment. To learn more, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/configuring-env-variables.html Apache Airflow configuration options>.
createEnvironment_airflowConfigurationOptions :: Lens.Lens' CreateEnvironment (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createEnvironment_airflowConfigurationOptions = Lens.lens (\CreateEnvironment' {airflowConfigurationOptions} -> airflowConfigurationOptions) (\s@CreateEnvironment' {} a -> s {airflowConfigurationOptions = a} :: CreateEnvironment) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | Defines the Apache Airflow logs to send to CloudWatch Logs:
-- @DagProcessingLogs@, @SchedulerLogs@, @TaskLogs@, @WebserverLogs@,
-- @WorkerLogs@.
createEnvironment_loggingConfiguration :: Lens.Lens' CreateEnvironment (Prelude.Maybe LoggingConfigurationInput)
createEnvironment_loggingConfiguration = Lens.lens (\CreateEnvironment' {loggingConfiguration} -> loggingConfiguration) (\s@CreateEnvironment' {} a -> s {loggingConfiguration = a} :: CreateEnvironment)

-- | The environment class type. Valid values: @mw1.small@, @mw1.medium@,
-- @mw1.large@. To learn more, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/environment-class.html Amazon MWAA environment class>.
createEnvironment_environmentClass :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_environmentClass = Lens.lens (\CreateEnvironment' {environmentClass} -> environmentClass) (\s@CreateEnvironment' {} a -> s {environmentClass = a} :: CreateEnvironment)

-- | The key-value tag pairs you want to associate to your environment. For
-- example, @\"Environment\": \"Staging\"@. To learn more, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources>.
createEnvironment_tags :: Lens.Lens' CreateEnvironment (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createEnvironment_tags = Lens.lens (\CreateEnvironment' {tags} -> tags) (\s@CreateEnvironment' {} a -> s {tags = a} :: CreateEnvironment) Prelude.. Lens.mapping Lens.coerced

-- | The relative path to the @requirements.txt@ file on your Amazon S3
-- bucket. For example, @requirements.txt@. If specified, then a file
-- version is required. To learn more, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/working-dags-dependencies.html Installing Python dependencies>.
createEnvironment_requirementsS3Path :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_requirementsS3Path = Lens.lens (\CreateEnvironment' {requirementsS3Path} -> requirementsS3Path) (\s@CreateEnvironment' {} a -> s {requirementsS3Path = a} :: CreateEnvironment)

-- | The maximum number of workers that you want to run in your environment.
-- MWAA scales the number of Apache Airflow workers up to the number you
-- specify in the @MaxWorkers@ field. For example, @20@. When there are no
-- more tasks running, and no more in the queue, MWAA disposes of the extra
-- workers leaving the one worker that is included with your environment,
-- or the number you specify in @MinWorkers@.
createEnvironment_maxWorkers :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Natural)
createEnvironment_maxWorkers = Lens.lens (\CreateEnvironment' {maxWorkers} -> maxWorkers) (\s@CreateEnvironment' {} a -> s {maxWorkers = a} :: CreateEnvironment)

-- | The relative path to the DAGs folder on your Amazon S3 bucket. For
-- example, @dags@. To learn more, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/configuring-dag-folder.html Adding or updating DAGs>.
createEnvironment_dagS3Path :: Lens.Lens' CreateEnvironment Prelude.Text
createEnvironment_dagS3Path = Lens.lens (\CreateEnvironment' {dagS3Path} -> dagS3Path) (\s@CreateEnvironment' {} a -> s {dagS3Path = a} :: CreateEnvironment)

-- | The Amazon Resource Name (ARN) of the execution role for your
-- environment. An execution role is an AWS Identity and Access Management
-- (IAM) role that grants MWAA permission to access AWS services and
-- resources used by your environment. For example,
-- @arn:aws:iam::123456789:role\/my-execution-role@. To learn more, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/mwaa-create-role.html Amazon MWAA Execution role>.
createEnvironment_executionRoleArn :: Lens.Lens' CreateEnvironment Prelude.Text
createEnvironment_executionRoleArn = Lens.lens (\CreateEnvironment' {executionRoleArn} -> executionRoleArn) (\s@CreateEnvironment' {} a -> s {executionRoleArn = a} :: CreateEnvironment)

-- | The name of the Amazon MWAA environment. For example,
-- @MyMWAAEnvironment@.
createEnvironment_name :: Lens.Lens' CreateEnvironment Prelude.Text
createEnvironment_name = Lens.lens (\CreateEnvironment' {name} -> name) (\s@CreateEnvironment' {} a -> s {name = a} :: CreateEnvironment)

-- | The VPC networking components used to secure and enable network traffic
-- between the AWS resources for your environment. To learn more, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/networking-about.html About networking on Amazon MWAA>.
createEnvironment_networkConfiguration :: Lens.Lens' CreateEnvironment NetworkConfiguration
createEnvironment_networkConfiguration = Lens.lens (\CreateEnvironment' {networkConfiguration} -> networkConfiguration) (\s@CreateEnvironment' {} a -> s {networkConfiguration = a} :: CreateEnvironment)

-- | The Amazon Resource Name (ARN) of the Amazon S3 bucket where your DAG
-- code and supporting files are stored. For example,
-- @arn:aws:s3:::my-airflow-bucket-unique-name@. To learn more, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/mwaa-s3-bucket.html Create an Amazon S3 bucket for Amazon MWAA>.
createEnvironment_sourceBucketArn :: Lens.Lens' CreateEnvironment Prelude.Text
createEnvironment_sourceBucketArn = Lens.lens (\CreateEnvironment' {sourceBucketArn} -> sourceBucketArn) (\s@CreateEnvironment' {} a -> s {sourceBucketArn = a} :: CreateEnvironment)

instance Core.AWSRequest CreateEnvironment where
  type
    AWSResponse CreateEnvironment =
      CreateEnvironmentResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEnvironmentResponse'
            Prelude.<$> (x Core..?> "Arn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateEnvironment where
  hashWithSalt _salt CreateEnvironment' {..} =
    _salt `Prelude.hashWithSalt` schedulers
      `Prelude.hashWithSalt` minWorkers
      `Prelude.hashWithSalt` pluginsS3Path
      `Prelude.hashWithSalt` webserverAccessMode
      `Prelude.hashWithSalt` airflowVersion
      `Prelude.hashWithSalt` kmsKey
      `Prelude.hashWithSalt` weeklyMaintenanceWindowStart
      `Prelude.hashWithSalt` requirementsS3ObjectVersion
      `Prelude.hashWithSalt` pluginsS3ObjectVersion
      `Prelude.hashWithSalt` airflowConfigurationOptions
      `Prelude.hashWithSalt` loggingConfiguration
      `Prelude.hashWithSalt` environmentClass
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` requirementsS3Path
      `Prelude.hashWithSalt` maxWorkers
      `Prelude.hashWithSalt` dagS3Path
      `Prelude.hashWithSalt` executionRoleArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` networkConfiguration
      `Prelude.hashWithSalt` sourceBucketArn

instance Prelude.NFData CreateEnvironment where
  rnf CreateEnvironment' {..} =
    Prelude.rnf schedulers
      `Prelude.seq` Prelude.rnf minWorkers
      `Prelude.seq` Prelude.rnf pluginsS3Path
      `Prelude.seq` Prelude.rnf webserverAccessMode
      `Prelude.seq` Prelude.rnf airflowVersion
      `Prelude.seq` Prelude.rnf kmsKey
      `Prelude.seq` Prelude.rnf weeklyMaintenanceWindowStart
      `Prelude.seq` Prelude.rnf requirementsS3ObjectVersion
      `Prelude.seq` Prelude.rnf pluginsS3ObjectVersion
      `Prelude.seq` Prelude.rnf airflowConfigurationOptions
      `Prelude.seq` Prelude.rnf loggingConfiguration
      `Prelude.seq` Prelude.rnf environmentClass
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf requirementsS3Path
      `Prelude.seq` Prelude.rnf maxWorkers
      `Prelude.seq` Prelude.rnf dagS3Path
      `Prelude.seq` Prelude.rnf executionRoleArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf networkConfiguration
      `Prelude.seq` Prelude.rnf sourceBucketArn

instance Core.ToHeaders CreateEnvironment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateEnvironment where
  toJSON CreateEnvironment' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Schedulers" Core..=) Prelude.<$> schedulers,
            ("MinWorkers" Core..=) Prelude.<$> minWorkers,
            ("PluginsS3Path" Core..=) Prelude.<$> pluginsS3Path,
            ("WebserverAccessMode" Core..=)
              Prelude.<$> webserverAccessMode,
            ("AirflowVersion" Core..=)
              Prelude.<$> airflowVersion,
            ("KmsKey" Core..=) Prelude.<$> kmsKey,
            ("WeeklyMaintenanceWindowStart" Core..=)
              Prelude.<$> weeklyMaintenanceWindowStart,
            ("RequirementsS3ObjectVersion" Core..=)
              Prelude.<$> requirementsS3ObjectVersion,
            ("PluginsS3ObjectVersion" Core..=)
              Prelude.<$> pluginsS3ObjectVersion,
            ("AirflowConfigurationOptions" Core..=)
              Prelude.<$> airflowConfigurationOptions,
            ("LoggingConfiguration" Core..=)
              Prelude.<$> loggingConfiguration,
            ("EnvironmentClass" Core..=)
              Prelude.<$> environmentClass,
            ("Tags" Core..=) Prelude.<$> tags,
            ("RequirementsS3Path" Core..=)
              Prelude.<$> requirementsS3Path,
            ("MaxWorkers" Core..=) Prelude.<$> maxWorkers,
            Prelude.Just ("DagS3Path" Core..= dagS3Path),
            Prelude.Just
              ("ExecutionRoleArn" Core..= executionRoleArn),
            Prelude.Just
              ( "NetworkConfiguration"
                  Core..= networkConfiguration
              ),
            Prelude.Just
              ("SourceBucketArn" Core..= sourceBucketArn)
          ]
      )

instance Core.ToPath CreateEnvironment where
  toPath CreateEnvironment' {..} =
    Prelude.mconcat ["/environments/", Core.toBS name]

instance Core.ToQuery CreateEnvironment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEnvironmentResponse' smart constructor.
data CreateEnvironmentResponse = CreateEnvironmentResponse'
  { -- | The Amazon Resource Name (ARN) returned in the response for the
    -- environment.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEnvironmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createEnvironmentResponse_arn' - The Amazon Resource Name (ARN) returned in the response for the
-- environment.
--
-- 'httpStatus', 'createEnvironmentResponse_httpStatus' - The response's http status code.
newCreateEnvironmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateEnvironmentResponse
newCreateEnvironmentResponse pHttpStatus_ =
  CreateEnvironmentResponse'
    { arn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) returned in the response for the
-- environment.
createEnvironmentResponse_arn :: Lens.Lens' CreateEnvironmentResponse (Prelude.Maybe Prelude.Text)
createEnvironmentResponse_arn = Lens.lens (\CreateEnvironmentResponse' {arn} -> arn) (\s@CreateEnvironmentResponse' {} a -> s {arn = a} :: CreateEnvironmentResponse)

-- | The response's http status code.
createEnvironmentResponse_httpStatus :: Lens.Lens' CreateEnvironmentResponse Prelude.Int
createEnvironmentResponse_httpStatus = Lens.lens (\CreateEnvironmentResponse' {httpStatus} -> httpStatus) (\s@CreateEnvironmentResponse' {} a -> s {httpStatus = a} :: CreateEnvironmentResponse)

instance Prelude.NFData CreateEnvironmentResponse where
  rnf CreateEnvironmentResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf httpStatus
