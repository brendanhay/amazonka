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
-- Module      : Amazonka.Batch.RegisterJobDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an Batch job definition.
module Amazonka.Batch.RegisterJobDefinition
  ( -- * Creating a Request
    RegisterJobDefinition (..),
    newRegisterJobDefinition,

    -- * Request Lenses
    registerJobDefinition_containerProperties,
    registerJobDefinition_eksProperties,
    registerJobDefinition_nodeProperties,
    registerJobDefinition_parameters,
    registerJobDefinition_platformCapabilities,
    registerJobDefinition_propagateTags,
    registerJobDefinition_retryStrategy,
    registerJobDefinition_schedulingPriority,
    registerJobDefinition_tags,
    registerJobDefinition_timeout,
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

import Amazonka.Batch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for @RegisterJobDefinition@.
--
-- /See:/ 'newRegisterJobDefinition' smart constructor.
data RegisterJobDefinition = RegisterJobDefinition'
  { -- | An object with various properties specific to Amazon ECS based
    -- single-node container-based jobs. If the job definition\'s @type@
    -- parameter is @container@, then you must specify either
    -- @containerProperties@ or @nodeProperties@. This must not be specified
    -- for Amazon EKS based job definitions.
    --
    -- If the job runs on Fargate resources, then you must not specify
    -- @nodeProperties@; use only @containerProperties@.
    containerProperties :: Prelude.Maybe ContainerProperties,
    -- | An object with various properties that are specific to Amazon EKS based
    -- jobs. This must not be specified for Amazon ECS based job definitions.
    eksProperties :: Prelude.Maybe EksProperties,
    -- | An object with various properties specific to multi-node parallel jobs.
    -- If you specify node properties for a job, it becomes a multi-node
    -- parallel job. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/multi-node-parallel-jobs.html Multi-node Parallel Jobs>
    -- in the /Batch User Guide/. If the job definition\'s @type@ parameter is
    -- @container@, then you must specify either @containerProperties@ or
    -- @nodeProperties@.
    --
    -- If the job runs on Fargate resources, then you must not specify
    -- @nodeProperties@; use @containerProperties@ instead.
    --
    -- If the job runs on Amazon EKS resources, then you must not specify
    -- @nodeProperties@.
    nodeProperties :: Prelude.Maybe NodeProperties,
    -- | Default parameter substitution placeholders to set in the job
    -- definition. Parameters are specified as a key-value pair mapping.
    -- Parameters in a @SubmitJob@ request override any corresponding parameter
    -- defaults from the job definition.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The platform capabilities required by the job definition. If no value is
    -- specified, it defaults to @EC2@. To run the job on Fargate resources,
    -- specify @FARGATE@.
    --
    -- If the job runs on Amazon EKS resources, then you must not specify
    -- @platformCapabilities@.
    platformCapabilities :: Prelude.Maybe [PlatformCapability],
    -- | Specifies whether to propagate the tags from the job or job definition
    -- to the corresponding Amazon ECS task. If no value is specified, the tags
    -- are not propagated. Tags can only be propagated to the tasks during task
    -- creation. For tags with the same name, job tags are given priority over
    -- job definitions tags. If the total number of combined tags from the job
    -- and job definition is over 50, the job is moved to the @FAILED@ state.
    --
    -- If the job runs on Amazon EKS resources, then you must not specify
    -- @propagateTags@.
    propagateTags :: Prelude.Maybe Prelude.Bool,
    -- | The retry strategy to use for failed jobs that are submitted with this
    -- job definition. Any retry strategy that\'s specified during a SubmitJob
    -- operation overrides the retry strategy defined here. If a job is
    -- terminated due to a timeout, it isn\'t retried.
    retryStrategy :: Prelude.Maybe RetryStrategy,
    -- | The scheduling priority for jobs that are submitted with this job
    -- definition. This only affects jobs in job queues with a fair share
    -- policy. Jobs with a higher scheduling priority are scheduled before jobs
    -- with a lower scheduling priority.
    --
    -- The minimum supported value is 0 and the maximum supported value is
    -- 9999.
    schedulingPriority :: Prelude.Maybe Prelude.Int,
    -- | The tags that you apply to the job definition to help you categorize and
    -- organize your resources. Each tag consists of a key and an optional
    -- value. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/using-tags.html Tagging Amazon Web Services Resources>
    -- in /Batch User Guide/.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The timeout configuration for jobs that are submitted with this job
    -- definition, after which Batch terminates your jobs if they have not
    -- finished. If a job is terminated due to a timeout, it isn\'t retried.
    -- The minimum value for the timeout is 60 seconds. Any timeout
    -- configuration that\'s specified during a SubmitJob operation overrides
    -- the timeout configuration defined here. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/job_timeouts.html Job Timeouts>
    -- in the /Batch User Guide/.
    timeout :: Prelude.Maybe JobTimeout,
    -- | The name of the job definition to register. It can be up to 128 letters
    -- long. It can contain uppercase and lowercase letters, numbers, hyphens
    -- (-), and underscores (_).
    jobDefinitionName :: Prelude.Text,
    -- | The type of job definition. For more information about multi-node
    -- parallel jobs, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/multi-node-job-def.html Creating a multi-node parallel job definition>
    -- in the /Batch User Guide/.
    --
    -- If the job is run on Fargate resources, then @multinode@ isn\'t
    -- supported.
    type' :: JobDefinitionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterJobDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerProperties', 'registerJobDefinition_containerProperties' - An object with various properties specific to Amazon ECS based
-- single-node container-based jobs. If the job definition\'s @type@
-- parameter is @container@, then you must specify either
-- @containerProperties@ or @nodeProperties@. This must not be specified
-- for Amazon EKS based job definitions.
--
-- If the job runs on Fargate resources, then you must not specify
-- @nodeProperties@; use only @containerProperties@.
--
-- 'eksProperties', 'registerJobDefinition_eksProperties' - An object with various properties that are specific to Amazon EKS based
-- jobs. This must not be specified for Amazon ECS based job definitions.
--
-- 'nodeProperties', 'registerJobDefinition_nodeProperties' - An object with various properties specific to multi-node parallel jobs.
-- If you specify node properties for a job, it becomes a multi-node
-- parallel job. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/multi-node-parallel-jobs.html Multi-node Parallel Jobs>
-- in the /Batch User Guide/. If the job definition\'s @type@ parameter is
-- @container@, then you must specify either @containerProperties@ or
-- @nodeProperties@.
--
-- If the job runs on Fargate resources, then you must not specify
-- @nodeProperties@; use @containerProperties@ instead.
--
-- If the job runs on Amazon EKS resources, then you must not specify
-- @nodeProperties@.
--
-- 'parameters', 'registerJobDefinition_parameters' - Default parameter substitution placeholders to set in the job
-- definition. Parameters are specified as a key-value pair mapping.
-- Parameters in a @SubmitJob@ request override any corresponding parameter
-- defaults from the job definition.
--
-- 'platformCapabilities', 'registerJobDefinition_platformCapabilities' - The platform capabilities required by the job definition. If no value is
-- specified, it defaults to @EC2@. To run the job on Fargate resources,
-- specify @FARGATE@.
--
-- If the job runs on Amazon EKS resources, then you must not specify
-- @platformCapabilities@.
--
-- 'propagateTags', 'registerJobDefinition_propagateTags' - Specifies whether to propagate the tags from the job or job definition
-- to the corresponding Amazon ECS task. If no value is specified, the tags
-- are not propagated. Tags can only be propagated to the tasks during task
-- creation. For tags with the same name, job tags are given priority over
-- job definitions tags. If the total number of combined tags from the job
-- and job definition is over 50, the job is moved to the @FAILED@ state.
--
-- If the job runs on Amazon EKS resources, then you must not specify
-- @propagateTags@.
--
-- 'retryStrategy', 'registerJobDefinition_retryStrategy' - The retry strategy to use for failed jobs that are submitted with this
-- job definition. Any retry strategy that\'s specified during a SubmitJob
-- operation overrides the retry strategy defined here. If a job is
-- terminated due to a timeout, it isn\'t retried.
--
-- 'schedulingPriority', 'registerJobDefinition_schedulingPriority' - The scheduling priority for jobs that are submitted with this job
-- definition. This only affects jobs in job queues with a fair share
-- policy. Jobs with a higher scheduling priority are scheduled before jobs
-- with a lower scheduling priority.
--
-- The minimum supported value is 0 and the maximum supported value is
-- 9999.
--
-- 'tags', 'registerJobDefinition_tags' - The tags that you apply to the job definition to help you categorize and
-- organize your resources. Each tag consists of a key and an optional
-- value. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/using-tags.html Tagging Amazon Web Services Resources>
-- in /Batch User Guide/.
--
-- 'timeout', 'registerJobDefinition_timeout' - The timeout configuration for jobs that are submitted with this job
-- definition, after which Batch terminates your jobs if they have not
-- finished. If a job is terminated due to a timeout, it isn\'t retried.
-- The minimum value for the timeout is 60 seconds. Any timeout
-- configuration that\'s specified during a SubmitJob operation overrides
-- the timeout configuration defined here. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/job_timeouts.html Job Timeouts>
-- in the /Batch User Guide/.
--
-- 'jobDefinitionName', 'registerJobDefinition_jobDefinitionName' - The name of the job definition to register. It can be up to 128 letters
-- long. It can contain uppercase and lowercase letters, numbers, hyphens
-- (-), and underscores (_).
--
-- 'type'', 'registerJobDefinition_type' - The type of job definition. For more information about multi-node
-- parallel jobs, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/multi-node-job-def.html Creating a multi-node parallel job definition>
-- in the /Batch User Guide/.
--
-- If the job is run on Fargate resources, then @multinode@ isn\'t
-- supported.
newRegisterJobDefinition ::
  -- | 'jobDefinitionName'
  Prelude.Text ->
  -- | 'type''
  JobDefinitionType ->
  RegisterJobDefinition
newRegisterJobDefinition pJobDefinitionName_ pType_ =
  RegisterJobDefinition'
    { containerProperties =
        Prelude.Nothing,
      eksProperties = Prelude.Nothing,
      nodeProperties = Prelude.Nothing,
      parameters = Prelude.Nothing,
      platformCapabilities = Prelude.Nothing,
      propagateTags = Prelude.Nothing,
      retryStrategy = Prelude.Nothing,
      schedulingPriority = Prelude.Nothing,
      tags = Prelude.Nothing,
      timeout = Prelude.Nothing,
      jobDefinitionName = pJobDefinitionName_,
      type' = pType_
    }

-- | An object with various properties specific to Amazon ECS based
-- single-node container-based jobs. If the job definition\'s @type@
-- parameter is @container@, then you must specify either
-- @containerProperties@ or @nodeProperties@. This must not be specified
-- for Amazon EKS based job definitions.
--
-- If the job runs on Fargate resources, then you must not specify
-- @nodeProperties@; use only @containerProperties@.
registerJobDefinition_containerProperties :: Lens.Lens' RegisterJobDefinition (Prelude.Maybe ContainerProperties)
registerJobDefinition_containerProperties = Lens.lens (\RegisterJobDefinition' {containerProperties} -> containerProperties) (\s@RegisterJobDefinition' {} a -> s {containerProperties = a} :: RegisterJobDefinition)

-- | An object with various properties that are specific to Amazon EKS based
-- jobs. This must not be specified for Amazon ECS based job definitions.
registerJobDefinition_eksProperties :: Lens.Lens' RegisterJobDefinition (Prelude.Maybe EksProperties)
registerJobDefinition_eksProperties = Lens.lens (\RegisterJobDefinition' {eksProperties} -> eksProperties) (\s@RegisterJobDefinition' {} a -> s {eksProperties = a} :: RegisterJobDefinition)

-- | An object with various properties specific to multi-node parallel jobs.
-- If you specify node properties for a job, it becomes a multi-node
-- parallel job. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/multi-node-parallel-jobs.html Multi-node Parallel Jobs>
-- in the /Batch User Guide/. If the job definition\'s @type@ parameter is
-- @container@, then you must specify either @containerProperties@ or
-- @nodeProperties@.
--
-- If the job runs on Fargate resources, then you must not specify
-- @nodeProperties@; use @containerProperties@ instead.
--
-- If the job runs on Amazon EKS resources, then you must not specify
-- @nodeProperties@.
registerJobDefinition_nodeProperties :: Lens.Lens' RegisterJobDefinition (Prelude.Maybe NodeProperties)
registerJobDefinition_nodeProperties = Lens.lens (\RegisterJobDefinition' {nodeProperties} -> nodeProperties) (\s@RegisterJobDefinition' {} a -> s {nodeProperties = a} :: RegisterJobDefinition)

-- | Default parameter substitution placeholders to set in the job
-- definition. Parameters are specified as a key-value pair mapping.
-- Parameters in a @SubmitJob@ request override any corresponding parameter
-- defaults from the job definition.
registerJobDefinition_parameters :: Lens.Lens' RegisterJobDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
registerJobDefinition_parameters = Lens.lens (\RegisterJobDefinition' {parameters} -> parameters) (\s@RegisterJobDefinition' {} a -> s {parameters = a} :: RegisterJobDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The platform capabilities required by the job definition. If no value is
-- specified, it defaults to @EC2@. To run the job on Fargate resources,
-- specify @FARGATE@.
--
-- If the job runs on Amazon EKS resources, then you must not specify
-- @platformCapabilities@.
registerJobDefinition_platformCapabilities :: Lens.Lens' RegisterJobDefinition (Prelude.Maybe [PlatformCapability])
registerJobDefinition_platformCapabilities = Lens.lens (\RegisterJobDefinition' {platformCapabilities} -> platformCapabilities) (\s@RegisterJobDefinition' {} a -> s {platformCapabilities = a} :: RegisterJobDefinition) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether to propagate the tags from the job or job definition
-- to the corresponding Amazon ECS task. If no value is specified, the tags
-- are not propagated. Tags can only be propagated to the tasks during task
-- creation. For tags with the same name, job tags are given priority over
-- job definitions tags. If the total number of combined tags from the job
-- and job definition is over 50, the job is moved to the @FAILED@ state.
--
-- If the job runs on Amazon EKS resources, then you must not specify
-- @propagateTags@.
registerJobDefinition_propagateTags :: Lens.Lens' RegisterJobDefinition (Prelude.Maybe Prelude.Bool)
registerJobDefinition_propagateTags = Lens.lens (\RegisterJobDefinition' {propagateTags} -> propagateTags) (\s@RegisterJobDefinition' {} a -> s {propagateTags = a} :: RegisterJobDefinition)

-- | The retry strategy to use for failed jobs that are submitted with this
-- job definition. Any retry strategy that\'s specified during a SubmitJob
-- operation overrides the retry strategy defined here. If a job is
-- terminated due to a timeout, it isn\'t retried.
registerJobDefinition_retryStrategy :: Lens.Lens' RegisterJobDefinition (Prelude.Maybe RetryStrategy)
registerJobDefinition_retryStrategy = Lens.lens (\RegisterJobDefinition' {retryStrategy} -> retryStrategy) (\s@RegisterJobDefinition' {} a -> s {retryStrategy = a} :: RegisterJobDefinition)

-- | The scheduling priority for jobs that are submitted with this job
-- definition. This only affects jobs in job queues with a fair share
-- policy. Jobs with a higher scheduling priority are scheduled before jobs
-- with a lower scheduling priority.
--
-- The minimum supported value is 0 and the maximum supported value is
-- 9999.
registerJobDefinition_schedulingPriority :: Lens.Lens' RegisterJobDefinition (Prelude.Maybe Prelude.Int)
registerJobDefinition_schedulingPriority = Lens.lens (\RegisterJobDefinition' {schedulingPriority} -> schedulingPriority) (\s@RegisterJobDefinition' {} a -> s {schedulingPriority = a} :: RegisterJobDefinition)

-- | The tags that you apply to the job definition to help you categorize and
-- organize your resources. Each tag consists of a key and an optional
-- value. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/using-tags.html Tagging Amazon Web Services Resources>
-- in /Batch User Guide/.
registerJobDefinition_tags :: Lens.Lens' RegisterJobDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
registerJobDefinition_tags = Lens.lens (\RegisterJobDefinition' {tags} -> tags) (\s@RegisterJobDefinition' {} a -> s {tags = a} :: RegisterJobDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The timeout configuration for jobs that are submitted with this job
-- definition, after which Batch terminates your jobs if they have not
-- finished. If a job is terminated due to a timeout, it isn\'t retried.
-- The minimum value for the timeout is 60 seconds. Any timeout
-- configuration that\'s specified during a SubmitJob operation overrides
-- the timeout configuration defined here. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/job_timeouts.html Job Timeouts>
-- in the /Batch User Guide/.
registerJobDefinition_timeout :: Lens.Lens' RegisterJobDefinition (Prelude.Maybe JobTimeout)
registerJobDefinition_timeout = Lens.lens (\RegisterJobDefinition' {timeout} -> timeout) (\s@RegisterJobDefinition' {} a -> s {timeout = a} :: RegisterJobDefinition)

-- | The name of the job definition to register. It can be up to 128 letters
-- long. It can contain uppercase and lowercase letters, numbers, hyphens
-- (-), and underscores (_).
registerJobDefinition_jobDefinitionName :: Lens.Lens' RegisterJobDefinition Prelude.Text
registerJobDefinition_jobDefinitionName = Lens.lens (\RegisterJobDefinition' {jobDefinitionName} -> jobDefinitionName) (\s@RegisterJobDefinition' {} a -> s {jobDefinitionName = a} :: RegisterJobDefinition)

-- | The type of job definition. For more information about multi-node
-- parallel jobs, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/multi-node-job-def.html Creating a multi-node parallel job definition>
-- in the /Batch User Guide/.
--
-- If the job is run on Fargate resources, then @multinode@ isn\'t
-- supported.
registerJobDefinition_type :: Lens.Lens' RegisterJobDefinition JobDefinitionType
registerJobDefinition_type = Lens.lens (\RegisterJobDefinition' {type'} -> type') (\s@RegisterJobDefinition' {} a -> s {type' = a} :: RegisterJobDefinition)

instance Core.AWSRequest RegisterJobDefinition where
  type
    AWSResponse RegisterJobDefinition =
      RegisterJobDefinitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterJobDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "jobDefinitionName")
            Prelude.<*> (x Data..:> "jobDefinitionArn")
            Prelude.<*> (x Data..:> "revision")
      )

instance Prelude.Hashable RegisterJobDefinition where
  hashWithSalt _salt RegisterJobDefinition' {..} =
    _salt `Prelude.hashWithSalt` containerProperties
      `Prelude.hashWithSalt` eksProperties
      `Prelude.hashWithSalt` nodeProperties
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` platformCapabilities
      `Prelude.hashWithSalt` propagateTags
      `Prelude.hashWithSalt` retryStrategy
      `Prelude.hashWithSalt` schedulingPriority
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` timeout
      `Prelude.hashWithSalt` jobDefinitionName
      `Prelude.hashWithSalt` type'

instance Prelude.NFData RegisterJobDefinition where
  rnf RegisterJobDefinition' {..} =
    Prelude.rnf containerProperties
      `Prelude.seq` Prelude.rnf eksProperties
      `Prelude.seq` Prelude.rnf nodeProperties
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf platformCapabilities
      `Prelude.seq` Prelude.rnf propagateTags
      `Prelude.seq` Prelude.rnf retryStrategy
      `Prelude.seq` Prelude.rnf schedulingPriority
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf timeout
      `Prelude.seq` Prelude.rnf jobDefinitionName
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders RegisterJobDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RegisterJobDefinition where
  toJSON RegisterJobDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("containerProperties" Data..=)
              Prelude.<$> containerProperties,
            ("eksProperties" Data..=) Prelude.<$> eksProperties,
            ("nodeProperties" Data..=)
              Prelude.<$> nodeProperties,
            ("parameters" Data..=) Prelude.<$> parameters,
            ("platformCapabilities" Data..=)
              Prelude.<$> platformCapabilities,
            ("propagateTags" Data..=) Prelude.<$> propagateTags,
            ("retryStrategy" Data..=) Prelude.<$> retryStrategy,
            ("schedulingPriority" Data..=)
              Prelude.<$> schedulingPriority,
            ("tags" Data..=) Prelude.<$> tags,
            ("timeout" Data..=) Prelude.<$> timeout,
            Prelude.Just
              ("jobDefinitionName" Data..= jobDefinitionName),
            Prelude.Just ("type" Data..= type')
          ]
      )

instance Data.ToPath RegisterJobDefinition where
  toPath = Prelude.const "/v1/registerjobdefinition"

instance Data.ToQuery RegisterJobDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterJobDefinitionResponse' smart constructor.
data RegisterJobDefinitionResponse = RegisterJobDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the job definition.
    jobDefinitionName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the job definition.
    jobDefinitionArn :: Prelude.Text,
    -- | The revision of the job definition.
    revision :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'jobDefinitionName'
  Prelude.Text ->
  -- | 'jobDefinitionArn'
  Prelude.Text ->
  -- | 'revision'
  Prelude.Int ->
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
registerJobDefinitionResponse_httpStatus :: Lens.Lens' RegisterJobDefinitionResponse Prelude.Int
registerJobDefinitionResponse_httpStatus = Lens.lens (\RegisterJobDefinitionResponse' {httpStatus} -> httpStatus) (\s@RegisterJobDefinitionResponse' {} a -> s {httpStatus = a} :: RegisterJobDefinitionResponse)

-- | The name of the job definition.
registerJobDefinitionResponse_jobDefinitionName :: Lens.Lens' RegisterJobDefinitionResponse Prelude.Text
registerJobDefinitionResponse_jobDefinitionName = Lens.lens (\RegisterJobDefinitionResponse' {jobDefinitionName} -> jobDefinitionName) (\s@RegisterJobDefinitionResponse' {} a -> s {jobDefinitionName = a} :: RegisterJobDefinitionResponse)

-- | The Amazon Resource Name (ARN) of the job definition.
registerJobDefinitionResponse_jobDefinitionArn :: Lens.Lens' RegisterJobDefinitionResponse Prelude.Text
registerJobDefinitionResponse_jobDefinitionArn = Lens.lens (\RegisterJobDefinitionResponse' {jobDefinitionArn} -> jobDefinitionArn) (\s@RegisterJobDefinitionResponse' {} a -> s {jobDefinitionArn = a} :: RegisterJobDefinitionResponse)

-- | The revision of the job definition.
registerJobDefinitionResponse_revision :: Lens.Lens' RegisterJobDefinitionResponse Prelude.Int
registerJobDefinitionResponse_revision = Lens.lens (\RegisterJobDefinitionResponse' {revision} -> revision) (\s@RegisterJobDefinitionResponse' {} a -> s {revision = a} :: RegisterJobDefinitionResponse)

instance Prelude.NFData RegisterJobDefinitionResponse where
  rnf RegisterJobDefinitionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf jobDefinitionName
      `Prelude.seq` Prelude.rnf jobDefinitionArn
      `Prelude.seq` Prelude.rnf revision
