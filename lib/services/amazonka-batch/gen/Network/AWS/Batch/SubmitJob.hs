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
-- Module      : Network.AWS.Batch.SubmitJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Submits an Batch job from a job definition. Parameters that are
-- specified during SubmitJob override parameters defined in the job
-- definition. vCPU and memory requirements that are specified in the
-- @ResourceRequirements@ objects in the job definition are the exception.
-- They can\'t be overridden this way using the @memory@ and @vcpus@
-- parameters. Rather, you must specify updates to job definition
-- parameters in a @ResourceRequirements@ object that\'s included in the
-- @containerOverrides@ parameter.
--
-- Jobs that run on Fargate resources can\'t be guaranteed to run for more
-- than 14 days. This is because, after 14 days, Fargate resources might
-- become unavailable and job might be terminated.
module Network.AWS.Batch.SubmitJob
  ( -- * Creating a Request
    SubmitJob (..),
    newSubmitJob,

    -- * Request Lenses
    submitJob_dependsOn,
    submitJob_timeout,
    submitJob_containerOverrides,
    submitJob_arrayProperties,
    submitJob_nodeOverrides,
    submitJob_tags,
    submitJob_retryStrategy,
    submitJob_parameters,
    submitJob_propagateTags,
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

import Network.AWS.Batch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for @SubmitJob@.
--
-- /See:/ 'newSubmitJob' smart constructor.
data SubmitJob = SubmitJob'
  { -- | A list of dependencies for the job. A job can depend upon a maximum of
    -- 20 jobs. You can specify a @SEQUENTIAL@ type dependency without
    -- specifying a job ID for array jobs so that each child array job
    -- completes sequentially, starting at index 0. You can also specify an
    -- @N_TO_N@ type dependency with a job ID for array jobs. In that case,
    -- each index child of this job must wait for the corresponding index child
    -- of each dependency to complete before it can begin.
    dependsOn :: Prelude.Maybe [JobDependency],
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
    -- | A list of container overrides in the JSON format that specify the name
    -- of a container in the specified job definition and the overrides it
    -- should receive. You can override the default command for a container,
    -- which is specified in the job definition or the Docker image, with a
    -- @command@ override. You can also override existing environment variables
    -- on a container or add new environment variables to it with an
    -- @environment@ override.
    containerOverrides :: Prelude.Maybe ContainerOverrides,
    -- | The array properties for the submitted job, such as the size of the
    -- array. The array size can be between 2 and 10,000. If you specify array
    -- properties for a job, it becomes an array job. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/array_jobs.html Array Jobs>
    -- in the /Batch User Guide/.
    arrayProperties :: Prelude.Maybe ArrayProperties,
    -- | A list of node overrides in JSON format that specify the node range to
    -- target and the container overrides for that node range.
    --
    -- This parameter isn\'t applicable to jobs that are running on Fargate
    -- resources; use @containerOverrides@ instead.
    nodeOverrides :: Prelude.Maybe NodeOverrides,
    -- | The tags that you apply to the job request to help you categorize and
    -- organize your resources. Each tag consists of a key and an optional
    -- value. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
    -- in /Amazon Web Services General Reference/.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The retry strategy to use for failed jobs from this SubmitJob operation.
    -- When a retry strategy is specified here, it overrides the retry strategy
    -- defined in the job definition.
    retryStrategy :: Prelude.Maybe RetryStrategy,
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
    -- | The name of the job. The first character must be alphanumeric, and up to
    -- 128 letters (uppercase and lowercase), numbers, hyphens, and underscores
    -- are allowed.
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
-- 'dependsOn', 'submitJob_dependsOn' - A list of dependencies for the job. A job can depend upon a maximum of
-- 20 jobs. You can specify a @SEQUENTIAL@ type dependency without
-- specifying a job ID for array jobs so that each child array job
-- completes sequentially, starting at index 0. You can also specify an
-- @N_TO_N@ type dependency with a job ID for array jobs. In that case,
-- each index child of this job must wait for the corresponding index child
-- of each dependency to complete before it can begin.
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
-- 'containerOverrides', 'submitJob_containerOverrides' - A list of container overrides in the JSON format that specify the name
-- of a container in the specified job definition and the overrides it
-- should receive. You can override the default command for a container,
-- which is specified in the job definition or the Docker image, with a
-- @command@ override. You can also override existing environment variables
-- on a container or add new environment variables to it with an
-- @environment@ override.
--
-- 'arrayProperties', 'submitJob_arrayProperties' - The array properties for the submitted job, such as the size of the
-- array. The array size can be between 2 and 10,000. If you specify array
-- properties for a job, it becomes an array job. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/array_jobs.html Array Jobs>
-- in the /Batch User Guide/.
--
-- 'nodeOverrides', 'submitJob_nodeOverrides' - A list of node overrides in JSON format that specify the node range to
-- target and the container overrides for that node range.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources; use @containerOverrides@ instead.
--
-- 'tags', 'submitJob_tags' - The tags that you apply to the job request to help you categorize and
-- organize your resources. Each tag consists of a key and an optional
-- value. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- in /Amazon Web Services General Reference/.
--
-- 'retryStrategy', 'submitJob_retryStrategy' - The retry strategy to use for failed jobs from this SubmitJob operation.
-- When a retry strategy is specified here, it overrides the retry strategy
-- defined in the job definition.
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
-- 'jobName', 'submitJob_jobName' - The name of the job. The first character must be alphanumeric, and up to
-- 128 letters (uppercase and lowercase), numbers, hyphens, and underscores
-- are allowed.
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
    { dependsOn = Prelude.Nothing,
      timeout = Prelude.Nothing,
      containerOverrides = Prelude.Nothing,
      arrayProperties = Prelude.Nothing,
      nodeOverrides = Prelude.Nothing,
      tags = Prelude.Nothing,
      retryStrategy = Prelude.Nothing,
      parameters = Prelude.Nothing,
      propagateTags = Prelude.Nothing,
      jobName = pJobName_,
      jobQueue = pJobQueue_,
      jobDefinition = pJobDefinition_
    }

-- | A list of dependencies for the job. A job can depend upon a maximum of
-- 20 jobs. You can specify a @SEQUENTIAL@ type dependency without
-- specifying a job ID for array jobs so that each child array job
-- completes sequentially, starting at index 0. You can also specify an
-- @N_TO_N@ type dependency with a job ID for array jobs. In that case,
-- each index child of this job must wait for the corresponding index child
-- of each dependency to complete before it can begin.
submitJob_dependsOn :: Lens.Lens' SubmitJob (Prelude.Maybe [JobDependency])
submitJob_dependsOn = Lens.lens (\SubmitJob' {dependsOn} -> dependsOn) (\s@SubmitJob' {} a -> s {dependsOn = a} :: SubmitJob) Prelude.. Lens.mapping Lens._Coerce

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

-- | A list of container overrides in the JSON format that specify the name
-- of a container in the specified job definition and the overrides it
-- should receive. You can override the default command for a container,
-- which is specified in the job definition or the Docker image, with a
-- @command@ override. You can also override existing environment variables
-- on a container or add new environment variables to it with an
-- @environment@ override.
submitJob_containerOverrides :: Lens.Lens' SubmitJob (Prelude.Maybe ContainerOverrides)
submitJob_containerOverrides = Lens.lens (\SubmitJob' {containerOverrides} -> containerOverrides) (\s@SubmitJob' {} a -> s {containerOverrides = a} :: SubmitJob)

-- | The array properties for the submitted job, such as the size of the
-- array. The array size can be between 2 and 10,000. If you specify array
-- properties for a job, it becomes an array job. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/array_jobs.html Array Jobs>
-- in the /Batch User Guide/.
submitJob_arrayProperties :: Lens.Lens' SubmitJob (Prelude.Maybe ArrayProperties)
submitJob_arrayProperties = Lens.lens (\SubmitJob' {arrayProperties} -> arrayProperties) (\s@SubmitJob' {} a -> s {arrayProperties = a} :: SubmitJob)

-- | A list of node overrides in JSON format that specify the node range to
-- target and the container overrides for that node range.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources; use @containerOverrides@ instead.
submitJob_nodeOverrides :: Lens.Lens' SubmitJob (Prelude.Maybe NodeOverrides)
submitJob_nodeOverrides = Lens.lens (\SubmitJob' {nodeOverrides} -> nodeOverrides) (\s@SubmitJob' {} a -> s {nodeOverrides = a} :: SubmitJob)

-- | The tags that you apply to the job request to help you categorize and
-- organize your resources. Each tag consists of a key and an optional
-- value. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- in /Amazon Web Services General Reference/.
submitJob_tags :: Lens.Lens' SubmitJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
submitJob_tags = Lens.lens (\SubmitJob' {tags} -> tags) (\s@SubmitJob' {} a -> s {tags = a} :: SubmitJob) Prelude.. Lens.mapping Lens._Coerce

-- | The retry strategy to use for failed jobs from this SubmitJob operation.
-- When a retry strategy is specified here, it overrides the retry strategy
-- defined in the job definition.
submitJob_retryStrategy :: Lens.Lens' SubmitJob (Prelude.Maybe RetryStrategy)
submitJob_retryStrategy = Lens.lens (\SubmitJob' {retryStrategy} -> retryStrategy) (\s@SubmitJob' {} a -> s {retryStrategy = a} :: SubmitJob)

-- | Additional parameters passed to the job that replace parameter
-- substitution placeholders that are set in the job definition. Parameters
-- are specified as a key and value pair mapping. Parameters in a
-- @SubmitJob@ request override any corresponding parameter defaults from
-- the job definition.
submitJob_parameters :: Lens.Lens' SubmitJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
submitJob_parameters = Lens.lens (\SubmitJob' {parameters} -> parameters) (\s@SubmitJob' {} a -> s {parameters = a} :: SubmitJob) Prelude.. Lens.mapping Lens._Coerce

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

-- | The name of the job. The first character must be alphanumeric, and up to
-- 128 letters (uppercase and lowercase), numbers, hyphens, and underscores
-- are allowed.
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SubmitJobResponse'
            Prelude.<$> (x Core..?> "jobArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "jobName")
            Prelude.<*> (x Core..:> "jobId")
      )

instance Prelude.Hashable SubmitJob

instance Prelude.NFData SubmitJob

instance Core.ToHeaders SubmitJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SubmitJob where
  toJSON SubmitJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("dependsOn" Core..=) Prelude.<$> dependsOn,
            ("timeout" Core..=) Prelude.<$> timeout,
            ("containerOverrides" Core..=)
              Prelude.<$> containerOverrides,
            ("arrayProperties" Core..=)
              Prelude.<$> arrayProperties,
            ("nodeOverrides" Core..=) Prelude.<$> nodeOverrides,
            ("tags" Core..=) Prelude.<$> tags,
            ("retryStrategy" Core..=) Prelude.<$> retryStrategy,
            ("parameters" Core..=) Prelude.<$> parameters,
            ("propagateTags" Core..=) Prelude.<$> propagateTags,
            Prelude.Just ("jobName" Core..= jobName),
            Prelude.Just ("jobQueue" Core..= jobQueue),
            Prelude.Just
              ("jobDefinition" Core..= jobDefinition)
          ]
      )

instance Core.ToPath SubmitJob where
  toPath = Prelude.const "/v1/submitjob"

instance Core.ToQuery SubmitJob where
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

instance Prelude.NFData SubmitJobResponse
