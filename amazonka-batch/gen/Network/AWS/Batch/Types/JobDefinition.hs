{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.JobDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.JobDefinition where

import Network.AWS.Batch.Types.ContainerProperties
import Network.AWS.Batch.Types.JobTimeout
import Network.AWS.Batch.Types.NodeProperties
import Network.AWS.Batch.Types.PlatformCapability
import Network.AWS.Batch.Types.RetryStrategy
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An object representing an AWS Batch job definition.
--
-- /See:/ 'newJobDefinition' smart constructor.
data JobDefinition = JobDefinition'
  { -- | The status of the job definition.
    status :: Core.Maybe Core.Text,
    -- | The platform capabilities required by the job definition. If no value is
    -- specified, it defaults to @EC2@. Jobs run on Fargate resources specify
    -- @FARGATE@.
    platformCapabilities :: Core.Maybe [PlatformCapability],
    -- | The timeout configuration for jobs that are submitted with this job
    -- definition. You can specify a timeout duration after which AWS Batch
    -- terminates your jobs if they haven\'t finished.
    timeout :: Core.Maybe JobTimeout,
    -- | An object with various properties specific to multi-node parallel jobs.
    --
    -- If the job runs on Fargate resources, then you must not specify
    -- @nodeProperties@; use @containerProperties@ instead.
    nodeProperties :: Core.Maybe NodeProperties,
    -- | The tags applied to the job definition.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | An object with various properties specific to container-based jobs.
    containerProperties :: Core.Maybe ContainerProperties,
    -- | The retry strategy to use for failed jobs that are submitted with this
    -- job definition.
    retryStrategy :: Core.Maybe RetryStrategy,
    -- | Default parameters or parameter substitution placeholders that are set
    -- in the job definition. Parameters are specified as a key-value pair
    -- mapping. Parameters in a @SubmitJob@ request override any corresponding
    -- parameter defaults from the job definition. For more information about
    -- specifying parameters, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/job_definition_parameters.html Job Definition Parameters>
    -- in the /AWS Batch User Guide/.
    parameters :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Specifies whether to propagate the tags from the job or job definition
    -- to the corresponding Amazon ECS task. If no value is specified, the tags
    -- aren\'t propagated. Tags can only be propagated to the tasks during task
    -- creation. For tags with the same name, job tags are given priority over
    -- job definitions tags. If the total number of combined tags from the job
    -- and job definition is over 50, the job is moved to the @FAILED@ state.
    propagateTags :: Core.Maybe Core.Bool,
    -- | The name of the job definition.
    jobDefinitionName :: Core.Text,
    -- | The Amazon Resource Name (ARN) for the job definition.
    jobDefinitionArn :: Core.Text,
    -- | The revision of the job definition.
    revision :: Core.Int,
    -- | The type of job definition. If the job is run on Fargate resources, then
    -- @multinode@ isn\'t supported. For more information about multi-node
    -- parallel jobs, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/multi-node-job-def.html Creating a multi-node parallel job definition>
    -- in the /AWS Batch User Guide/.
    type' :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'JobDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'jobDefinition_status' - The status of the job definition.
--
-- 'platformCapabilities', 'jobDefinition_platformCapabilities' - The platform capabilities required by the job definition. If no value is
-- specified, it defaults to @EC2@. Jobs run on Fargate resources specify
-- @FARGATE@.
--
-- 'timeout', 'jobDefinition_timeout' - The timeout configuration for jobs that are submitted with this job
-- definition. You can specify a timeout duration after which AWS Batch
-- terminates your jobs if they haven\'t finished.
--
-- 'nodeProperties', 'jobDefinition_nodeProperties' - An object with various properties specific to multi-node parallel jobs.
--
-- If the job runs on Fargate resources, then you must not specify
-- @nodeProperties@; use @containerProperties@ instead.
--
-- 'tags', 'jobDefinition_tags' - The tags applied to the job definition.
--
-- 'containerProperties', 'jobDefinition_containerProperties' - An object with various properties specific to container-based jobs.
--
-- 'retryStrategy', 'jobDefinition_retryStrategy' - The retry strategy to use for failed jobs that are submitted with this
-- job definition.
--
-- 'parameters', 'jobDefinition_parameters' - Default parameters or parameter substitution placeholders that are set
-- in the job definition. Parameters are specified as a key-value pair
-- mapping. Parameters in a @SubmitJob@ request override any corresponding
-- parameter defaults from the job definition. For more information about
-- specifying parameters, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/job_definition_parameters.html Job Definition Parameters>
-- in the /AWS Batch User Guide/.
--
-- 'propagateTags', 'jobDefinition_propagateTags' - Specifies whether to propagate the tags from the job or job definition
-- to the corresponding Amazon ECS task. If no value is specified, the tags
-- aren\'t propagated. Tags can only be propagated to the tasks during task
-- creation. For tags with the same name, job tags are given priority over
-- job definitions tags. If the total number of combined tags from the job
-- and job definition is over 50, the job is moved to the @FAILED@ state.
--
-- 'jobDefinitionName', 'jobDefinition_jobDefinitionName' - The name of the job definition.
--
-- 'jobDefinitionArn', 'jobDefinition_jobDefinitionArn' - The Amazon Resource Name (ARN) for the job definition.
--
-- 'revision', 'jobDefinition_revision' - The revision of the job definition.
--
-- 'type'', 'jobDefinition_type' - The type of job definition. If the job is run on Fargate resources, then
-- @multinode@ isn\'t supported. For more information about multi-node
-- parallel jobs, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/multi-node-job-def.html Creating a multi-node parallel job definition>
-- in the /AWS Batch User Guide/.
newJobDefinition ::
  -- | 'jobDefinitionName'
  Core.Text ->
  -- | 'jobDefinitionArn'
  Core.Text ->
  -- | 'revision'
  Core.Int ->
  -- | 'type''
  Core.Text ->
  JobDefinition
newJobDefinition
  pJobDefinitionName_
  pJobDefinitionArn_
  pRevision_
  pType_ =
    JobDefinition'
      { status = Core.Nothing,
        platformCapabilities = Core.Nothing,
        timeout = Core.Nothing,
        nodeProperties = Core.Nothing,
        tags = Core.Nothing,
        containerProperties = Core.Nothing,
        retryStrategy = Core.Nothing,
        parameters = Core.Nothing,
        propagateTags = Core.Nothing,
        jobDefinitionName = pJobDefinitionName_,
        jobDefinitionArn = pJobDefinitionArn_,
        revision = pRevision_,
        type' = pType_
      }

-- | The status of the job definition.
jobDefinition_status :: Lens.Lens' JobDefinition (Core.Maybe Core.Text)
jobDefinition_status = Lens.lens (\JobDefinition' {status} -> status) (\s@JobDefinition' {} a -> s {status = a} :: JobDefinition)

-- | The platform capabilities required by the job definition. If no value is
-- specified, it defaults to @EC2@. Jobs run on Fargate resources specify
-- @FARGATE@.
jobDefinition_platformCapabilities :: Lens.Lens' JobDefinition (Core.Maybe [PlatformCapability])
jobDefinition_platformCapabilities = Lens.lens (\JobDefinition' {platformCapabilities} -> platformCapabilities) (\s@JobDefinition' {} a -> s {platformCapabilities = a} :: JobDefinition) Core.. Lens.mapping Lens._Coerce

-- | The timeout configuration for jobs that are submitted with this job
-- definition. You can specify a timeout duration after which AWS Batch
-- terminates your jobs if they haven\'t finished.
jobDefinition_timeout :: Lens.Lens' JobDefinition (Core.Maybe JobTimeout)
jobDefinition_timeout = Lens.lens (\JobDefinition' {timeout} -> timeout) (\s@JobDefinition' {} a -> s {timeout = a} :: JobDefinition)

-- | An object with various properties specific to multi-node parallel jobs.
--
-- If the job runs on Fargate resources, then you must not specify
-- @nodeProperties@; use @containerProperties@ instead.
jobDefinition_nodeProperties :: Lens.Lens' JobDefinition (Core.Maybe NodeProperties)
jobDefinition_nodeProperties = Lens.lens (\JobDefinition' {nodeProperties} -> nodeProperties) (\s@JobDefinition' {} a -> s {nodeProperties = a} :: JobDefinition)

-- | The tags applied to the job definition.
jobDefinition_tags :: Lens.Lens' JobDefinition (Core.Maybe (Core.HashMap Core.Text Core.Text))
jobDefinition_tags = Lens.lens (\JobDefinition' {tags} -> tags) (\s@JobDefinition' {} a -> s {tags = a} :: JobDefinition) Core.. Lens.mapping Lens._Coerce

-- | An object with various properties specific to container-based jobs.
jobDefinition_containerProperties :: Lens.Lens' JobDefinition (Core.Maybe ContainerProperties)
jobDefinition_containerProperties = Lens.lens (\JobDefinition' {containerProperties} -> containerProperties) (\s@JobDefinition' {} a -> s {containerProperties = a} :: JobDefinition)

-- | The retry strategy to use for failed jobs that are submitted with this
-- job definition.
jobDefinition_retryStrategy :: Lens.Lens' JobDefinition (Core.Maybe RetryStrategy)
jobDefinition_retryStrategy = Lens.lens (\JobDefinition' {retryStrategy} -> retryStrategy) (\s@JobDefinition' {} a -> s {retryStrategy = a} :: JobDefinition)

-- | Default parameters or parameter substitution placeholders that are set
-- in the job definition. Parameters are specified as a key-value pair
-- mapping. Parameters in a @SubmitJob@ request override any corresponding
-- parameter defaults from the job definition. For more information about
-- specifying parameters, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/job_definition_parameters.html Job Definition Parameters>
-- in the /AWS Batch User Guide/.
jobDefinition_parameters :: Lens.Lens' JobDefinition (Core.Maybe (Core.HashMap Core.Text Core.Text))
jobDefinition_parameters = Lens.lens (\JobDefinition' {parameters} -> parameters) (\s@JobDefinition' {} a -> s {parameters = a} :: JobDefinition) Core.. Lens.mapping Lens._Coerce

-- | Specifies whether to propagate the tags from the job or job definition
-- to the corresponding Amazon ECS task. If no value is specified, the tags
-- aren\'t propagated. Tags can only be propagated to the tasks during task
-- creation. For tags with the same name, job tags are given priority over
-- job definitions tags. If the total number of combined tags from the job
-- and job definition is over 50, the job is moved to the @FAILED@ state.
jobDefinition_propagateTags :: Lens.Lens' JobDefinition (Core.Maybe Core.Bool)
jobDefinition_propagateTags = Lens.lens (\JobDefinition' {propagateTags} -> propagateTags) (\s@JobDefinition' {} a -> s {propagateTags = a} :: JobDefinition)

-- | The name of the job definition.
jobDefinition_jobDefinitionName :: Lens.Lens' JobDefinition Core.Text
jobDefinition_jobDefinitionName = Lens.lens (\JobDefinition' {jobDefinitionName} -> jobDefinitionName) (\s@JobDefinition' {} a -> s {jobDefinitionName = a} :: JobDefinition)

-- | The Amazon Resource Name (ARN) for the job definition.
jobDefinition_jobDefinitionArn :: Lens.Lens' JobDefinition Core.Text
jobDefinition_jobDefinitionArn = Lens.lens (\JobDefinition' {jobDefinitionArn} -> jobDefinitionArn) (\s@JobDefinition' {} a -> s {jobDefinitionArn = a} :: JobDefinition)

-- | The revision of the job definition.
jobDefinition_revision :: Lens.Lens' JobDefinition Core.Int
jobDefinition_revision = Lens.lens (\JobDefinition' {revision} -> revision) (\s@JobDefinition' {} a -> s {revision = a} :: JobDefinition)

-- | The type of job definition. If the job is run on Fargate resources, then
-- @multinode@ isn\'t supported. For more information about multi-node
-- parallel jobs, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/multi-node-job-def.html Creating a multi-node parallel job definition>
-- in the /AWS Batch User Guide/.
jobDefinition_type :: Lens.Lens' JobDefinition Core.Text
jobDefinition_type = Lens.lens (\JobDefinition' {type'} -> type') (\s@JobDefinition' {} a -> s {type' = a} :: JobDefinition)

instance Core.FromJSON JobDefinition where
  parseJSON =
    Core.withObject
      "JobDefinition"
      ( \x ->
          JobDefinition'
            Core.<$> (x Core..:? "status")
            Core.<*> ( x Core..:? "platformCapabilities"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "timeout")
            Core.<*> (x Core..:? "nodeProperties")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "containerProperties")
            Core.<*> (x Core..:? "retryStrategy")
            Core.<*> (x Core..:? "parameters" Core..!= Core.mempty)
            Core.<*> (x Core..:? "propagateTags")
            Core.<*> (x Core..: "jobDefinitionName")
            Core.<*> (x Core..: "jobDefinitionArn")
            Core.<*> (x Core..: "revision")
            Core.<*> (x Core..: "type")
      )

instance Core.Hashable JobDefinition

instance Core.NFData JobDefinition
