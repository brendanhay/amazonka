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
-- Module      : Amazonka.Batch.SubmitJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Submits an Batch job from a job definition. Parameters that are
-- specified during SubmitJob override parameters defined in the job
-- definition. vCPU and memory requirements that are specified in the
-- @resourceRequirements@ objects in the job definition are the exception.
-- They can\'t be overridden this way using the @memory@ and @vcpus@
-- parameters. Rather, you must specify updates to job definition
-- parameters in a @resourceRequirements@ object that\'s included in the
-- @containerOverrides@ parameter.
--
-- Job queues with a scheduling policy are limited to 500 active fair share
-- identifiers at a time.
--
-- Jobs that run on Fargate resources can\'t be guaranteed to run for more
-- than 14 days. This is because, after 14 days, Fargate resources might
-- become unavailable and job might be terminated.
module Amazonka.Batch.SubmitJob
  ( -- * Creating a Request
    SubmitJob (..),
    newSubmitJob,

    -- * Request Lenses
    submitJob_arrayProperties,
    submitJob_containerOverrides,
    submitJob_dependsOn,
    submitJob_eksPropertiesOverride,
    submitJob_nodeOverrides,
    submitJob_parameters,
    submitJob_propagateTags,
    submitJob_retryStrategy,
    submitJob_schedulingPriorityOverride,
    submitJob_shareIdentifier,
    submitJob_tags,
    submitJob_timeout,
    submitJob_jobName,
    submitJob_jobQueue,
    submitJob_jobDefinition,

    -- * Destructuring the Response
    SubmitJobResponse (..),
    newSubmitJobResponse,

    -- * Response Lenses
    submitJobResponse_jobArn,
    submitJobResponse_httpStatus,
    submitJobResponse_jobName,
    submitJobResponse_jobId,
  )
where

import Amazonka.Batch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for @SubmitJob@.
--
-- /See:/ 'newSubmitJob' smart constructor.
data SubmitJob = SubmitJob'
  { -- | The array properties for the submitted job, such as the size of the
    -- array. The array size can be between 2 and 10,000. If you specify array
    -- properties for a job, it becomes an array job. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/array_jobs.html Array Jobs>
    -- in the /Batch User Guide/.
    arrayProperties :: Prelude.Maybe ArrayProperties,
    -- | An object with various properties that override the defaults for the job
    -- definition that specify the name of a container in the specified job
    -- definition and the overrides it should receive. You can override the
    -- default command for a container, which is specified in the job
    -- definition or the Docker image, with a @command@ override. You can also
    -- override existing environment variables on a container or add new
    -- environment variables to it with an @environment@ override.
    containerOverrides :: Prelude.Maybe ContainerOverrides,
    -- | A list of dependencies for the job. A job can depend upon a maximum of
    -- 20 jobs. You can specify a @SEQUENTIAL@ type dependency without
    -- specifying a job ID for array jobs so that each child array job
    -- completes sequentially, starting at index 0. You can also specify an
    -- @N_TO_N@ type dependency with a job ID for array jobs. In that case,
    -- each index child of this job must wait for the corresponding index child
    -- of each dependency to complete before it can begin.
    dependsOn :: Prelude.Maybe [JobDependency],
    -- | An object that can only be specified for jobs that are run on Amazon EKS
    -- resources with various properties that override defaults for the job
    -- definition.
    eksPropertiesOverride :: Prelude.Maybe EksPropertiesOverride,
    -- | A list of node overrides in JSON format that specify the node range to
    -- target and the container overrides for that node range.
    --
    -- This parameter isn\'t applicable to jobs that are running on Fargate
    -- resources; use @containerOverrides@ instead.
    nodeOverrides :: Prelude.Maybe NodeOverrides,
    -- | Additional parameters passed to the job that replace parameter
    -- substitution placeholders that are set in the job definition. Parameters
    -- are specified as a key and value pair mapping. Parameters in a
    -- @SubmitJob@ request override any corresponding parameter defaults from
    -- the job definition.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies whether to propagate the tags from the job or job definition
    -- to the corresponding Amazon ECS task. If no value is specified, the tags
    -- aren\'t propagated. Tags can only be propagated to the tasks during task
    -- creation. For tags with the same name, job tags are given priority over
    -- job definitions tags. If the total number of combined tags from the job
    -- and job definition is over 50, the job is moved to the @FAILED@ state.
    -- When specified, this overrides the tag propagation setting in the job
    -- definition.
    propagateTags :: Prelude.Maybe Prelude.Bool,
    -- | The retry strategy to use for failed jobs from this SubmitJob operation.
    -- When a retry strategy is specified here, it overrides the retry strategy
    -- defined in the job definition.
    retryStrategy :: Prelude.Maybe RetryStrategy,
    -- | The scheduling priority for the job. This only affects jobs in job
    -- queues with a fair share policy. Jobs with a higher scheduling priority
    -- are scheduled before jobs with a lower scheduling priority. This
    -- overrides any scheduling priority in the job definition.
    --
    -- The minimum supported value is 0 and the maximum supported value is
    -- 9999.
    schedulingPriorityOverride :: Prelude.Maybe Prelude.Int,
    -- | The share identifier for the job. If the job queue doesn\'t have a
    -- scheduling policy, then this parameter must not be specified. If the job
    -- queue has a scheduling policy, then this parameter must be specified.
    shareIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The tags that you apply to the job request to help you categorize and
    -- organize your resources. Each tag consists of a key and an optional
    -- value. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
    -- in /Amazon Web Services General Reference/.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The timeout configuration for this SubmitJob operation. You can specify
    -- a timeout duration after which Batch terminates your jobs if they
    -- haven\'t finished. If a job is terminated due to a timeout, it isn\'t
    -- retried. The minimum value for the timeout is 60 seconds. This
    -- configuration overrides any timeout configuration specified in the job
    -- definition. For array jobs, child jobs have the same timeout
    -- configuration as the parent job. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/job_timeouts.html Job Timeouts>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    timeout :: Prelude.Maybe JobTimeout,
    -- | The name of the job. It can be up to 128 letters long. The first
    -- character must be alphanumeric, can contain uppercase and lowercase
    -- letters, numbers, hyphens (-), and underscores (_).
    jobName :: Prelude.Text,
    -- | The job queue where the job is submitted. You can specify either the
    -- name or the Amazon Resource Name (ARN) of the queue.
    jobQueue :: Prelude.Text,
    -- | The job definition used by this job. This value can be one of @name@,
    -- @name:revision@, or the Amazon Resource Name (ARN) for the job
    -- definition. If @name@ is specified without a revision then the latest
    -- active revision is used.
    jobDefinition :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SubmitJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arrayProperties', 'submitJob_arrayProperties' - The array properties for the submitted job, such as the size of the
-- array. The array size can be between 2 and 10,000. If you specify array
-- properties for a job, it becomes an array job. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/array_jobs.html Array Jobs>
-- in the /Batch User Guide/.
--
-- 'containerOverrides', 'submitJob_containerOverrides' - An object with various properties that override the defaults for the job
-- definition that specify the name of a container in the specified job
-- definition and the overrides it should receive. You can override the
-- default command for a container, which is specified in the job
-- definition or the Docker image, with a @command@ override. You can also
-- override existing environment variables on a container or add new
-- environment variables to it with an @environment@ override.
--
-- 'dependsOn', 'submitJob_dependsOn' - A list of dependencies for the job. A job can depend upon a maximum of
-- 20 jobs. You can specify a @SEQUENTIAL@ type dependency without
-- specifying a job ID for array jobs so that each child array job
-- completes sequentially, starting at index 0. You can also specify an
-- @N_TO_N@ type dependency with a job ID for array jobs. In that case,
-- each index child of this job must wait for the corresponding index child
-- of each dependency to complete before it can begin.
--
-- 'eksPropertiesOverride', 'submitJob_eksPropertiesOverride' - An object that can only be specified for jobs that are run on Amazon EKS
-- resources with various properties that override defaults for the job
-- definition.
--
-- 'nodeOverrides', 'submitJob_nodeOverrides' - A list of node overrides in JSON format that specify the node range to
-- target and the container overrides for that node range.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources; use @containerOverrides@ instead.
--
-- 'parameters', 'submitJob_parameters' - Additional parameters passed to the job that replace parameter
-- substitution placeholders that are set in the job definition. Parameters
-- are specified as a key and value pair mapping. Parameters in a
-- @SubmitJob@ request override any corresponding parameter defaults from
-- the job definition.
--
-- 'propagateTags', 'submitJob_propagateTags' - Specifies whether to propagate the tags from the job or job definition
-- to the corresponding Amazon ECS task. If no value is specified, the tags
-- aren\'t propagated. Tags can only be propagated to the tasks during task
-- creation. For tags with the same name, job tags are given priority over
-- job definitions tags. If the total number of combined tags from the job
-- and job definition is over 50, the job is moved to the @FAILED@ state.
-- When specified, this overrides the tag propagation setting in the job
-- definition.
--
-- 'retryStrategy', 'submitJob_retryStrategy' - The retry strategy to use for failed jobs from this SubmitJob operation.
-- When a retry strategy is specified here, it overrides the retry strategy
-- defined in the job definition.
--
-- 'schedulingPriorityOverride', 'submitJob_schedulingPriorityOverride' - The scheduling priority for the job. This only affects jobs in job
-- queues with a fair share policy. Jobs with a higher scheduling priority
-- are scheduled before jobs with a lower scheduling priority. This
-- overrides any scheduling priority in the job definition.
--
-- The minimum supported value is 0 and the maximum supported value is
-- 9999.
--
-- 'shareIdentifier', 'submitJob_shareIdentifier' - The share identifier for the job. If the job queue doesn\'t have a
-- scheduling policy, then this parameter must not be specified. If the job
-- queue has a scheduling policy, then this parameter must be specified.
--
-- 'tags', 'submitJob_tags' - The tags that you apply to the job request to help you categorize and
-- organize your resources. Each tag consists of a key and an optional
-- value. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- in /Amazon Web Services General Reference/.
--
-- 'timeout', 'submitJob_timeout' - The timeout configuration for this SubmitJob operation. You can specify
-- a timeout duration after which Batch terminates your jobs if they
-- haven\'t finished. If a job is terminated due to a timeout, it isn\'t
-- retried. The minimum value for the timeout is 60 seconds. This
-- configuration overrides any timeout configuration specified in the job
-- definition. For array jobs, child jobs have the same timeout
-- configuration as the parent job. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/job_timeouts.html Job Timeouts>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'jobName', 'submitJob_jobName' - The name of the job. It can be up to 128 letters long. The first
-- character must be alphanumeric, can contain uppercase and lowercase
-- letters, numbers, hyphens (-), and underscores (_).
--
-- 'jobQueue', 'submitJob_jobQueue' - The job queue where the job is submitted. You can specify either the
-- name or the Amazon Resource Name (ARN) of the queue.
--
-- 'jobDefinition', 'submitJob_jobDefinition' - The job definition used by this job. This value can be one of @name@,
-- @name:revision@, or the Amazon Resource Name (ARN) for the job
-- definition. If @name@ is specified without a revision then the latest
-- active revision is used.
newSubmitJob ::
  -- | 'jobName'
  Prelude.Text ->
  -- | 'jobQueue'
  Prelude.Text ->
  -- | 'jobDefinition'
  Prelude.Text ->
  SubmitJob
newSubmitJob pJobName_ pJobQueue_ pJobDefinition_ =
  SubmitJob'
    { arrayProperties = Prelude.Nothing,
      containerOverrides = Prelude.Nothing,
      dependsOn = Prelude.Nothing,
      eksPropertiesOverride = Prelude.Nothing,
      nodeOverrides = Prelude.Nothing,
      parameters = Prelude.Nothing,
      propagateTags = Prelude.Nothing,
      retryStrategy = Prelude.Nothing,
      schedulingPriorityOverride = Prelude.Nothing,
      shareIdentifier = Prelude.Nothing,
      tags = Prelude.Nothing,
      timeout = Prelude.Nothing,
      jobName = pJobName_,
      jobQueue = pJobQueue_,
      jobDefinition = pJobDefinition_
    }

-- | The array properties for the submitted job, such as the size of the
-- array. The array size can be between 2 and 10,000. If you specify array
-- properties for a job, it becomes an array job. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/array_jobs.html Array Jobs>
-- in the /Batch User Guide/.
submitJob_arrayProperties :: Lens.Lens' SubmitJob (Prelude.Maybe ArrayProperties)
submitJob_arrayProperties = Lens.lens (\SubmitJob' {arrayProperties} -> arrayProperties) (\s@SubmitJob' {} a -> s {arrayProperties = a} :: SubmitJob)

-- | An object with various properties that override the defaults for the job
-- definition that specify the name of a container in the specified job
-- definition and the overrides it should receive. You can override the
-- default command for a container, which is specified in the job
-- definition or the Docker image, with a @command@ override. You can also
-- override existing environment variables on a container or add new
-- environment variables to it with an @environment@ override.
submitJob_containerOverrides :: Lens.Lens' SubmitJob (Prelude.Maybe ContainerOverrides)
submitJob_containerOverrides = Lens.lens (\SubmitJob' {containerOverrides} -> containerOverrides) (\s@SubmitJob' {} a -> s {containerOverrides = a} :: SubmitJob)

-- | A list of dependencies for the job. A job can depend upon a maximum of
-- 20 jobs. You can specify a @SEQUENTIAL@ type dependency without
-- specifying a job ID for array jobs so that each child array job
-- completes sequentially, starting at index 0. You can also specify an
-- @N_TO_N@ type dependency with a job ID for array jobs. In that case,
-- each index child of this job must wait for the corresponding index child
-- of each dependency to complete before it can begin.
submitJob_dependsOn :: Lens.Lens' SubmitJob (Prelude.Maybe [JobDependency])
submitJob_dependsOn = Lens.lens (\SubmitJob' {dependsOn} -> dependsOn) (\s@SubmitJob' {} a -> s {dependsOn = a} :: SubmitJob) Prelude.. Lens.mapping Lens.coerced

-- | An object that can only be specified for jobs that are run on Amazon EKS
-- resources with various properties that override defaults for the job
-- definition.
submitJob_eksPropertiesOverride :: Lens.Lens' SubmitJob (Prelude.Maybe EksPropertiesOverride)
submitJob_eksPropertiesOverride = Lens.lens (\SubmitJob' {eksPropertiesOverride} -> eksPropertiesOverride) (\s@SubmitJob' {} a -> s {eksPropertiesOverride = a} :: SubmitJob)

-- | A list of node overrides in JSON format that specify the node range to
-- target and the container overrides for that node range.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources; use @containerOverrides@ instead.
submitJob_nodeOverrides :: Lens.Lens' SubmitJob (Prelude.Maybe NodeOverrides)
submitJob_nodeOverrides = Lens.lens (\SubmitJob' {nodeOverrides} -> nodeOverrides) (\s@SubmitJob' {} a -> s {nodeOverrides = a} :: SubmitJob)

-- | Additional parameters passed to the job that replace parameter
-- substitution placeholders that are set in the job definition. Parameters
-- are specified as a key and value pair mapping. Parameters in a
-- @SubmitJob@ request override any corresponding parameter defaults from
-- the job definition.
submitJob_parameters :: Lens.Lens' SubmitJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
submitJob_parameters = Lens.lens (\SubmitJob' {parameters} -> parameters) (\s@SubmitJob' {} a -> s {parameters = a} :: SubmitJob) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether to propagate the tags from the job or job definition
-- to the corresponding Amazon ECS task. If no value is specified, the tags
-- aren\'t propagated. Tags can only be propagated to the tasks during task
-- creation. For tags with the same name, job tags are given priority over
-- job definitions tags. If the total number of combined tags from the job
-- and job definition is over 50, the job is moved to the @FAILED@ state.
-- When specified, this overrides the tag propagation setting in the job
-- definition.
submitJob_propagateTags :: Lens.Lens' SubmitJob (Prelude.Maybe Prelude.Bool)
submitJob_propagateTags = Lens.lens (\SubmitJob' {propagateTags} -> propagateTags) (\s@SubmitJob' {} a -> s {propagateTags = a} :: SubmitJob)

-- | The retry strategy to use for failed jobs from this SubmitJob operation.
-- When a retry strategy is specified here, it overrides the retry strategy
-- defined in the job definition.
submitJob_retryStrategy :: Lens.Lens' SubmitJob (Prelude.Maybe RetryStrategy)
submitJob_retryStrategy = Lens.lens (\SubmitJob' {retryStrategy} -> retryStrategy) (\s@SubmitJob' {} a -> s {retryStrategy = a} :: SubmitJob)

-- | The scheduling priority for the job. This only affects jobs in job
-- queues with a fair share policy. Jobs with a higher scheduling priority
-- are scheduled before jobs with a lower scheduling priority. This
-- overrides any scheduling priority in the job definition.
--
-- The minimum supported value is 0 and the maximum supported value is
-- 9999.
submitJob_schedulingPriorityOverride :: Lens.Lens' SubmitJob (Prelude.Maybe Prelude.Int)
submitJob_schedulingPriorityOverride = Lens.lens (\SubmitJob' {schedulingPriorityOverride} -> schedulingPriorityOverride) (\s@SubmitJob' {} a -> s {schedulingPriorityOverride = a} :: SubmitJob)

-- | The share identifier for the job. If the job queue doesn\'t have a
-- scheduling policy, then this parameter must not be specified. If the job
-- queue has a scheduling policy, then this parameter must be specified.
submitJob_shareIdentifier :: Lens.Lens' SubmitJob (Prelude.Maybe Prelude.Text)
submitJob_shareIdentifier = Lens.lens (\SubmitJob' {shareIdentifier} -> shareIdentifier) (\s@SubmitJob' {} a -> s {shareIdentifier = a} :: SubmitJob)

-- | The tags that you apply to the job request to help you categorize and
-- organize your resources. Each tag consists of a key and an optional
-- value. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- in /Amazon Web Services General Reference/.
submitJob_tags :: Lens.Lens' SubmitJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
submitJob_tags = Lens.lens (\SubmitJob' {tags} -> tags) (\s@SubmitJob' {} a -> s {tags = a} :: SubmitJob) Prelude.. Lens.mapping Lens.coerced

-- | The timeout configuration for this SubmitJob operation. You can specify
-- a timeout duration after which Batch terminates your jobs if they
-- haven\'t finished. If a job is terminated due to a timeout, it isn\'t
-- retried. The minimum value for the timeout is 60 seconds. This
-- configuration overrides any timeout configuration specified in the job
-- definition. For array jobs, child jobs have the same timeout
-- configuration as the parent job. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/job_timeouts.html Job Timeouts>
-- in the /Amazon Elastic Container Service Developer Guide/.
submitJob_timeout :: Lens.Lens' SubmitJob (Prelude.Maybe JobTimeout)
submitJob_timeout = Lens.lens (\SubmitJob' {timeout} -> timeout) (\s@SubmitJob' {} a -> s {timeout = a} :: SubmitJob)

-- | The name of the job. It can be up to 128 letters long. The first
-- character must be alphanumeric, can contain uppercase and lowercase
-- letters, numbers, hyphens (-), and underscores (_).
submitJob_jobName :: Lens.Lens' SubmitJob Prelude.Text
submitJob_jobName = Lens.lens (\SubmitJob' {jobName} -> jobName) (\s@SubmitJob' {} a -> s {jobName = a} :: SubmitJob)

-- | The job queue where the job is submitted. You can specify either the
-- name or the Amazon Resource Name (ARN) of the queue.
submitJob_jobQueue :: Lens.Lens' SubmitJob Prelude.Text
submitJob_jobQueue = Lens.lens (\SubmitJob' {jobQueue} -> jobQueue) (\s@SubmitJob' {} a -> s {jobQueue = a} :: SubmitJob)

-- | The job definition used by this job. This value can be one of @name@,
-- @name:revision@, or the Amazon Resource Name (ARN) for the job
-- definition. If @name@ is specified without a revision then the latest
-- active revision is used.
submitJob_jobDefinition :: Lens.Lens' SubmitJob Prelude.Text
submitJob_jobDefinition = Lens.lens (\SubmitJob' {jobDefinition} -> jobDefinition) (\s@SubmitJob' {} a -> s {jobDefinition = a} :: SubmitJob)

instance Core.AWSRequest SubmitJob where
  type AWSResponse SubmitJob = SubmitJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SubmitJobResponse'
            Prelude.<$> (x Data..?> "jobArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "jobName")
            Prelude.<*> (x Data..:> "jobId")
      )

instance Prelude.Hashable SubmitJob where
  hashWithSalt _salt SubmitJob' {..} =
    _salt
      `Prelude.hashWithSalt` arrayProperties
      `Prelude.hashWithSalt` containerOverrides
      `Prelude.hashWithSalt` dependsOn
      `Prelude.hashWithSalt` eksPropertiesOverride
      `Prelude.hashWithSalt` nodeOverrides
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` propagateTags
      `Prelude.hashWithSalt` retryStrategy
      `Prelude.hashWithSalt` schedulingPriorityOverride
      `Prelude.hashWithSalt` shareIdentifier
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` timeout
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` jobQueue
      `Prelude.hashWithSalt` jobDefinition

instance Prelude.NFData SubmitJob where
  rnf SubmitJob' {..} =
    Prelude.rnf arrayProperties
      `Prelude.seq` Prelude.rnf containerOverrides
      `Prelude.seq` Prelude.rnf dependsOn
      `Prelude.seq` Prelude.rnf eksPropertiesOverride
      `Prelude.seq` Prelude.rnf nodeOverrides
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf propagateTags
      `Prelude.seq` Prelude.rnf retryStrategy
      `Prelude.seq` Prelude.rnf schedulingPriorityOverride
      `Prelude.seq` Prelude.rnf shareIdentifier
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf timeout
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobQueue
      `Prelude.seq` Prelude.rnf jobDefinition

instance Data.ToHeaders SubmitJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SubmitJob where
  toJSON SubmitJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("arrayProperties" Data..=)
              Prelude.<$> arrayProperties,
            ("containerOverrides" Data..=)
              Prelude.<$> containerOverrides,
            ("dependsOn" Data..=) Prelude.<$> dependsOn,
            ("eksPropertiesOverride" Data..=)
              Prelude.<$> eksPropertiesOverride,
            ("nodeOverrides" Data..=) Prelude.<$> nodeOverrides,
            ("parameters" Data..=) Prelude.<$> parameters,
            ("propagateTags" Data..=) Prelude.<$> propagateTags,
            ("retryStrategy" Data..=) Prelude.<$> retryStrategy,
            ("schedulingPriorityOverride" Data..=)
              Prelude.<$> schedulingPriorityOverride,
            ("shareIdentifier" Data..=)
              Prelude.<$> shareIdentifier,
            ("tags" Data..=) Prelude.<$> tags,
            ("timeout" Data..=) Prelude.<$> timeout,
            Prelude.Just ("jobName" Data..= jobName),
            Prelude.Just ("jobQueue" Data..= jobQueue),
            Prelude.Just
              ("jobDefinition" Data..= jobDefinition)
          ]
      )

instance Data.ToPath SubmitJob where
  toPath = Prelude.const "/v1/submitjob"

instance Data.ToQuery SubmitJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSubmitJobResponse' smart constructor.
data SubmitJobResponse = SubmitJobResponse'
  { -- | The Amazon Resource Name (ARN) for the job.
    jobArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the job.
    jobName :: Prelude.Text,
    -- | The unique identifier for the job.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SubmitJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobArn', 'submitJobResponse_jobArn' - The Amazon Resource Name (ARN) for the job.
--
-- 'httpStatus', 'submitJobResponse_httpStatus' - The response's http status code.
--
-- 'jobName', 'submitJobResponse_jobName' - The name of the job.
--
-- 'jobId', 'submitJobResponse_jobId' - The unique identifier for the job.
newSubmitJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'jobName'
  Prelude.Text ->
  -- | 'jobId'
  Prelude.Text ->
  SubmitJobResponse
newSubmitJobResponse pHttpStatus_ pJobName_ pJobId_ =
  SubmitJobResponse'
    { jobArn = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      jobName = pJobName_,
      jobId = pJobId_
    }

-- | The Amazon Resource Name (ARN) for the job.
submitJobResponse_jobArn :: Lens.Lens' SubmitJobResponse (Prelude.Maybe Prelude.Text)
submitJobResponse_jobArn = Lens.lens (\SubmitJobResponse' {jobArn} -> jobArn) (\s@SubmitJobResponse' {} a -> s {jobArn = a} :: SubmitJobResponse)

-- | The response's http status code.
submitJobResponse_httpStatus :: Lens.Lens' SubmitJobResponse Prelude.Int
submitJobResponse_httpStatus = Lens.lens (\SubmitJobResponse' {httpStatus} -> httpStatus) (\s@SubmitJobResponse' {} a -> s {httpStatus = a} :: SubmitJobResponse)

-- | The name of the job.
submitJobResponse_jobName :: Lens.Lens' SubmitJobResponse Prelude.Text
submitJobResponse_jobName = Lens.lens (\SubmitJobResponse' {jobName} -> jobName) (\s@SubmitJobResponse' {} a -> s {jobName = a} :: SubmitJobResponse)

-- | The unique identifier for the job.
submitJobResponse_jobId :: Lens.Lens' SubmitJobResponse Prelude.Text
submitJobResponse_jobId = Lens.lens (\SubmitJobResponse' {jobId} -> jobId) (\s@SubmitJobResponse' {} a -> s {jobId = a} :: SubmitJobResponse)

instance Prelude.NFData SubmitJobResponse where
  rnf SubmitJobResponse' {..} =
    Prelude.rnf jobArn
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobId
