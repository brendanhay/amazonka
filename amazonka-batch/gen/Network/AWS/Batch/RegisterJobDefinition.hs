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
-- Module      : Network.AWS.Batch.RegisterJobDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an AWS Batch job definition.
module Network.AWS.Batch.RegisterJobDefinition
  ( -- * Creating a Request
    RegisterJobDefinition (..),
    newRegisterJobDefinition,

    -- * Request Lenses
    registerJobDefinition_platformCapabilities,
    registerJobDefinition_timeout,
    registerJobDefinition_nodeProperties,
    registerJobDefinition_tags,
    registerJobDefinition_containerProperties,
    registerJobDefinition_retryStrategy,
    registerJobDefinition_parameters,
    registerJobDefinition_propagateTags,
    registerJobDefinition_jobDefinitionName,
    registerJobDefinition_type,

    -- * Destructuring the Response
    RegisterJobDefinitionResponse (..),
    newRegisterJobDefinitionResponse,

    -- * Response Lenses
    registerJobDefinitionResponse_httpStatus,
    registerJobDefinitionResponse_jobDefinitionName,
    registerJobDefinitionResponse_jobDefinitionArn,
    registerJobDefinitionResponse_revision,
  )
where

import Network.AWS.Batch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for @RegisterJobDefinition@.
--
-- /See:/ 'newRegisterJobDefinition' smart constructor.
data RegisterJobDefinition = RegisterJobDefinition'
  { -- | The platform capabilities required by the job definition. If no value is
    -- specified, it defaults to @EC2@. To run the job on Fargate resources,
    -- specify @FARGATE@.
    platformCapabilities :: Core.Maybe [PlatformCapability],
    -- | The timeout configuration for jobs that are submitted with this job
    -- definition, after which AWS Batch terminates your jobs if they have not
    -- finished. If a job is terminated due to a timeout, it isn\'t retried.
    -- The minimum value for the timeout is 60 seconds. Any timeout
    -- configuration that\'s specified during a SubmitJob operation overrides
    -- the timeout configuration defined here. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/job_timeouts.html Job Timeouts>
    -- in the /AWS Batch User Guide/.
    timeout :: Core.Maybe JobTimeout,
    -- | An object with various properties specific to multi-node parallel jobs.
    -- If you specify node properties for a job, it becomes a multi-node
    -- parallel job. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/multi-node-parallel-jobs.html Multi-node Parallel Jobs>
    -- in the /AWS Batch User Guide/. If the job definition\'s @type@ parameter
    -- is @container@, then you must specify either @containerProperties@ or
    -- @nodeProperties@.
    --
    -- If the job runs on Fargate resources, then you must not specify
    -- @nodeProperties@; use @containerProperties@ instead.
    nodeProperties :: Core.Maybe NodeProperties,
    -- | The tags that you apply to the job definition to help you categorize and
    -- organize your resources. Each tag consists of a key and an optional
    -- value. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/using-tags.html Tagging AWS Resources>
    -- in /AWS Batch User Guide/.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | An object with various properties specific to single-node
    -- container-based jobs. If the job definition\'s @type@ parameter is
    -- @container@, then you must specify either @containerProperties@ or
    -- @nodeProperties@.
    --
    -- If the job runs on Fargate resources, then you must not specify
    -- @nodeProperties@; use only @containerProperties@.
    containerProperties :: Core.Maybe ContainerProperties,
    -- | The retry strategy to use for failed jobs that are submitted with this
    -- job definition. Any retry strategy that\'s specified during a SubmitJob
    -- operation overrides the retry strategy defined here. If a job is
    -- terminated due to a timeout, it isn\'t retried.
    retryStrategy :: Core.Maybe RetryStrategy,
    -- | Default parameter substitution placeholders to set in the job
    -- definition. Parameters are specified as a key-value pair mapping.
    -- Parameters in a @SubmitJob@ request override any corresponding parameter
    -- defaults from the job definition.
    parameters :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Specifies whether to propagate the tags from the job or job definition
    -- to the corresponding Amazon ECS task. If no value is specified, the tags
    -- are not propagated. Tags can only be propagated to the tasks during task
    -- creation. For tags with the same name, job tags are given priority over
    -- job definitions tags. If the total number of combined tags from the job
    -- and job definition is over 50, the job is moved to the @FAILED@ state.
    propagateTags :: Core.Maybe Core.Bool,
    -- | The name of the job definition to register. Up to 128 letters (uppercase
    -- and lowercase), numbers, hyphens, and underscores are allowed.
    jobDefinitionName :: Core.Text,
    -- | The type of job definition. For more information about multi-node
    -- parallel jobs, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/multi-node-job-def.html Creating a multi-node parallel job definition>
    -- in the /AWS Batch User Guide/.
    --
    -- If the job is run on Fargate resources, then @multinode@ isn\'t
    -- supported.
    type' :: JobDefinitionType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterJobDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platformCapabilities', 'registerJobDefinition_platformCapabilities' - The platform capabilities required by the job definition. If no value is
-- specified, it defaults to @EC2@. To run the job on Fargate resources,
-- specify @FARGATE@.
--
-- 'timeout', 'registerJobDefinition_timeout' - The timeout configuration for jobs that are submitted with this job
-- definition, after which AWS Batch terminates your jobs if they have not
-- finished. If a job is terminated due to a timeout, it isn\'t retried.
-- The minimum value for the timeout is 60 seconds. Any timeout
-- configuration that\'s specified during a SubmitJob operation overrides
-- the timeout configuration defined here. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/job_timeouts.html Job Timeouts>
-- in the /AWS Batch User Guide/.
--
-- 'nodeProperties', 'registerJobDefinition_nodeProperties' - An object with various properties specific to multi-node parallel jobs.
-- If you specify node properties for a job, it becomes a multi-node
-- parallel job. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/multi-node-parallel-jobs.html Multi-node Parallel Jobs>
-- in the /AWS Batch User Guide/. If the job definition\'s @type@ parameter
-- is @container@, then you must specify either @containerProperties@ or
-- @nodeProperties@.
--
-- If the job runs on Fargate resources, then you must not specify
-- @nodeProperties@; use @containerProperties@ instead.
--
-- 'tags', 'registerJobDefinition_tags' - The tags that you apply to the job definition to help you categorize and
-- organize your resources. Each tag consists of a key and an optional
-- value. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/using-tags.html Tagging AWS Resources>
-- in /AWS Batch User Guide/.
--
-- 'containerProperties', 'registerJobDefinition_containerProperties' - An object with various properties specific to single-node
-- container-based jobs. If the job definition\'s @type@ parameter is
-- @container@, then you must specify either @containerProperties@ or
-- @nodeProperties@.
--
-- If the job runs on Fargate resources, then you must not specify
-- @nodeProperties@; use only @containerProperties@.
--
-- 'retryStrategy', 'registerJobDefinition_retryStrategy' - The retry strategy to use for failed jobs that are submitted with this
-- job definition. Any retry strategy that\'s specified during a SubmitJob
-- operation overrides the retry strategy defined here. If a job is
-- terminated due to a timeout, it isn\'t retried.
--
-- 'parameters', 'registerJobDefinition_parameters' - Default parameter substitution placeholders to set in the job
-- definition. Parameters are specified as a key-value pair mapping.
-- Parameters in a @SubmitJob@ request override any corresponding parameter
-- defaults from the job definition.
--
-- 'propagateTags', 'registerJobDefinition_propagateTags' - Specifies whether to propagate the tags from the job or job definition
-- to the corresponding Amazon ECS task. If no value is specified, the tags
-- are not propagated. Tags can only be propagated to the tasks during task
-- creation. For tags with the same name, job tags are given priority over
-- job definitions tags. If the total number of combined tags from the job
-- and job definition is over 50, the job is moved to the @FAILED@ state.
--
-- 'jobDefinitionName', 'registerJobDefinition_jobDefinitionName' - The name of the job definition to register. Up to 128 letters (uppercase
-- and lowercase), numbers, hyphens, and underscores are allowed.
--
-- 'type'', 'registerJobDefinition_type' - The type of job definition. For more information about multi-node
-- parallel jobs, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/multi-node-job-def.html Creating a multi-node parallel job definition>
-- in the /AWS Batch User Guide/.
--
-- If the job is run on Fargate resources, then @multinode@ isn\'t
-- supported.
newRegisterJobDefinition ::
  -- | 'jobDefinitionName'
  Core.Text ->
  -- | 'type''
  JobDefinitionType ->
  RegisterJobDefinition
newRegisterJobDefinition pJobDefinitionName_ pType_ =
  RegisterJobDefinition'
    { platformCapabilities =
        Core.Nothing,
      timeout = Core.Nothing,
      nodeProperties = Core.Nothing,
      tags = Core.Nothing,
      containerProperties = Core.Nothing,
      retryStrategy = Core.Nothing,
      parameters = Core.Nothing,
      propagateTags = Core.Nothing,
      jobDefinitionName = pJobDefinitionName_,
      type' = pType_
    }

-- | The platform capabilities required by the job definition. If no value is
-- specified, it defaults to @EC2@. To run the job on Fargate resources,
-- specify @FARGATE@.
registerJobDefinition_platformCapabilities :: Lens.Lens' RegisterJobDefinition (Core.Maybe [PlatformCapability])
registerJobDefinition_platformCapabilities = Lens.lens (\RegisterJobDefinition' {platformCapabilities} -> platformCapabilities) (\s@RegisterJobDefinition' {} a -> s {platformCapabilities = a} :: RegisterJobDefinition) Core.. Lens.mapping Lens._Coerce

-- | The timeout configuration for jobs that are submitted with this job
-- definition, after which AWS Batch terminates your jobs if they have not
-- finished. If a job is terminated due to a timeout, it isn\'t retried.
-- The minimum value for the timeout is 60 seconds. Any timeout
-- configuration that\'s specified during a SubmitJob operation overrides
-- the timeout configuration defined here. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/job_timeouts.html Job Timeouts>
-- in the /AWS Batch User Guide/.
registerJobDefinition_timeout :: Lens.Lens' RegisterJobDefinition (Core.Maybe JobTimeout)
registerJobDefinition_timeout = Lens.lens (\RegisterJobDefinition' {timeout} -> timeout) (\s@RegisterJobDefinition' {} a -> s {timeout = a} :: RegisterJobDefinition)

-- | An object with various properties specific to multi-node parallel jobs.
-- If you specify node properties for a job, it becomes a multi-node
-- parallel job. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/multi-node-parallel-jobs.html Multi-node Parallel Jobs>
-- in the /AWS Batch User Guide/. If the job definition\'s @type@ parameter
-- is @container@, then you must specify either @containerProperties@ or
-- @nodeProperties@.
--
-- If the job runs on Fargate resources, then you must not specify
-- @nodeProperties@; use @containerProperties@ instead.
registerJobDefinition_nodeProperties :: Lens.Lens' RegisterJobDefinition (Core.Maybe NodeProperties)
registerJobDefinition_nodeProperties = Lens.lens (\RegisterJobDefinition' {nodeProperties} -> nodeProperties) (\s@RegisterJobDefinition' {} a -> s {nodeProperties = a} :: RegisterJobDefinition)

-- | The tags that you apply to the job definition to help you categorize and
-- organize your resources. Each tag consists of a key and an optional
-- value. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/using-tags.html Tagging AWS Resources>
-- in /AWS Batch User Guide/.
registerJobDefinition_tags :: Lens.Lens' RegisterJobDefinition (Core.Maybe (Core.HashMap Core.Text Core.Text))
registerJobDefinition_tags = Lens.lens (\RegisterJobDefinition' {tags} -> tags) (\s@RegisterJobDefinition' {} a -> s {tags = a} :: RegisterJobDefinition) Core.. Lens.mapping Lens._Coerce

-- | An object with various properties specific to single-node
-- container-based jobs. If the job definition\'s @type@ parameter is
-- @container@, then you must specify either @containerProperties@ or
-- @nodeProperties@.
--
-- If the job runs on Fargate resources, then you must not specify
-- @nodeProperties@; use only @containerProperties@.
registerJobDefinition_containerProperties :: Lens.Lens' RegisterJobDefinition (Core.Maybe ContainerProperties)
registerJobDefinition_containerProperties = Lens.lens (\RegisterJobDefinition' {containerProperties} -> containerProperties) (\s@RegisterJobDefinition' {} a -> s {containerProperties = a} :: RegisterJobDefinition)

-- | The retry strategy to use for failed jobs that are submitted with this
-- job definition. Any retry strategy that\'s specified during a SubmitJob
-- operation overrides the retry strategy defined here. If a job is
-- terminated due to a timeout, it isn\'t retried.
registerJobDefinition_retryStrategy :: Lens.Lens' RegisterJobDefinition (Core.Maybe RetryStrategy)
registerJobDefinition_retryStrategy = Lens.lens (\RegisterJobDefinition' {retryStrategy} -> retryStrategy) (\s@RegisterJobDefinition' {} a -> s {retryStrategy = a} :: RegisterJobDefinition)

-- | Default parameter substitution placeholders to set in the job
-- definition. Parameters are specified as a key-value pair mapping.
-- Parameters in a @SubmitJob@ request override any corresponding parameter
-- defaults from the job definition.
registerJobDefinition_parameters :: Lens.Lens' RegisterJobDefinition (Core.Maybe (Core.HashMap Core.Text Core.Text))
registerJobDefinition_parameters = Lens.lens (\RegisterJobDefinition' {parameters} -> parameters) (\s@RegisterJobDefinition' {} a -> s {parameters = a} :: RegisterJobDefinition) Core.. Lens.mapping Lens._Coerce

-- | Specifies whether to propagate the tags from the job or job definition
-- to the corresponding Amazon ECS task. If no value is specified, the tags
-- are not propagated. Tags can only be propagated to the tasks during task
-- creation. For tags with the same name, job tags are given priority over
-- job definitions tags. If the total number of combined tags from the job
-- and job definition is over 50, the job is moved to the @FAILED@ state.
registerJobDefinition_propagateTags :: Lens.Lens' RegisterJobDefinition (Core.Maybe Core.Bool)
registerJobDefinition_propagateTags = Lens.lens (\RegisterJobDefinition' {propagateTags} -> propagateTags) (\s@RegisterJobDefinition' {} a -> s {propagateTags = a} :: RegisterJobDefinition)

-- | The name of the job definition to register. Up to 128 letters (uppercase
-- and lowercase), numbers, hyphens, and underscores are allowed.
registerJobDefinition_jobDefinitionName :: Lens.Lens' RegisterJobDefinition Core.Text
registerJobDefinition_jobDefinitionName = Lens.lens (\RegisterJobDefinition' {jobDefinitionName} -> jobDefinitionName) (\s@RegisterJobDefinition' {} a -> s {jobDefinitionName = a} :: RegisterJobDefinition)

-- | The type of job definition. For more information about multi-node
-- parallel jobs, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/multi-node-job-def.html Creating a multi-node parallel job definition>
-- in the /AWS Batch User Guide/.
--
-- If the job is run on Fargate resources, then @multinode@ isn\'t
-- supported.
registerJobDefinition_type :: Lens.Lens' RegisterJobDefinition JobDefinitionType
registerJobDefinition_type = Lens.lens (\RegisterJobDefinition' {type'} -> type') (\s@RegisterJobDefinition' {} a -> s {type' = a} :: RegisterJobDefinition)

instance Core.AWSRequest RegisterJobDefinition where
  type
    AWSResponse RegisterJobDefinition =
      RegisterJobDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterJobDefinitionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "jobDefinitionName")
            Core.<*> (x Core..:> "jobDefinitionArn")
            Core.<*> (x Core..:> "revision")
      )

instance Core.Hashable RegisterJobDefinition

instance Core.NFData RegisterJobDefinition

instance Core.ToHeaders RegisterJobDefinition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RegisterJobDefinition where
  toJSON RegisterJobDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("platformCapabilities" Core..=)
              Core.<$> platformCapabilities,
            ("timeout" Core..=) Core.<$> timeout,
            ("nodeProperties" Core..=) Core.<$> nodeProperties,
            ("tags" Core..=) Core.<$> tags,
            ("containerProperties" Core..=)
              Core.<$> containerProperties,
            ("retryStrategy" Core..=) Core.<$> retryStrategy,
            ("parameters" Core..=) Core.<$> parameters,
            ("propagateTags" Core..=) Core.<$> propagateTags,
            Core.Just
              ("jobDefinitionName" Core..= jobDefinitionName),
            Core.Just ("type" Core..= type')
          ]
      )

instance Core.ToPath RegisterJobDefinition where
  toPath = Core.const "/v1/registerjobdefinition"

instance Core.ToQuery RegisterJobDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRegisterJobDefinitionResponse' smart constructor.
data RegisterJobDefinitionResponse = RegisterJobDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The name of the job definition.
    jobDefinitionName :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the job definition.
    jobDefinitionArn :: Core.Text,
    -- | The revision of the job definition.
    revision :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterJobDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'registerJobDefinitionResponse_httpStatus' - The response's http status code.
--
-- 'jobDefinitionName', 'registerJobDefinitionResponse_jobDefinitionName' - The name of the job definition.
--
-- 'jobDefinitionArn', 'registerJobDefinitionResponse_jobDefinitionArn' - The Amazon Resource Name (ARN) of the job definition.
--
-- 'revision', 'registerJobDefinitionResponse_revision' - The revision of the job definition.
newRegisterJobDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'jobDefinitionName'
  Core.Text ->
  -- | 'jobDefinitionArn'
  Core.Text ->
  -- | 'revision'
  Core.Int ->
  RegisterJobDefinitionResponse
newRegisterJobDefinitionResponse
  pHttpStatus_
  pJobDefinitionName_
  pJobDefinitionArn_
  pRevision_ =
    RegisterJobDefinitionResponse'
      { httpStatus =
          pHttpStatus_,
        jobDefinitionName = pJobDefinitionName_,
        jobDefinitionArn = pJobDefinitionArn_,
        revision = pRevision_
      }

-- | The response's http status code.
registerJobDefinitionResponse_httpStatus :: Lens.Lens' RegisterJobDefinitionResponse Core.Int
registerJobDefinitionResponse_httpStatus = Lens.lens (\RegisterJobDefinitionResponse' {httpStatus} -> httpStatus) (\s@RegisterJobDefinitionResponse' {} a -> s {httpStatus = a} :: RegisterJobDefinitionResponse)

-- | The name of the job definition.
registerJobDefinitionResponse_jobDefinitionName :: Lens.Lens' RegisterJobDefinitionResponse Core.Text
registerJobDefinitionResponse_jobDefinitionName = Lens.lens (\RegisterJobDefinitionResponse' {jobDefinitionName} -> jobDefinitionName) (\s@RegisterJobDefinitionResponse' {} a -> s {jobDefinitionName = a} :: RegisterJobDefinitionResponse)

-- | The Amazon Resource Name (ARN) of the job definition.
registerJobDefinitionResponse_jobDefinitionArn :: Lens.Lens' RegisterJobDefinitionResponse Core.Text
registerJobDefinitionResponse_jobDefinitionArn = Lens.lens (\RegisterJobDefinitionResponse' {jobDefinitionArn} -> jobDefinitionArn) (\s@RegisterJobDefinitionResponse' {} a -> s {jobDefinitionArn = a} :: RegisterJobDefinitionResponse)

-- | The revision of the job definition.
registerJobDefinitionResponse_revision :: Lens.Lens' RegisterJobDefinitionResponse Core.Int
registerJobDefinitionResponse_revision = Lens.lens (\RegisterJobDefinitionResponse' {revision} -> revision) (\s@RegisterJobDefinitionResponse' {} a -> s {revision = a} :: RegisterJobDefinitionResponse)

instance Core.NFData RegisterJobDefinitionResponse
