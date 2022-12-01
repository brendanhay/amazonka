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
-- Module      : Amazonka.Batch.Types.JobDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.JobDefinition where

import Amazonka.Batch.Types.ContainerProperties
import Amazonka.Batch.Types.EksProperties
import Amazonka.Batch.Types.JobTimeout
import Amazonka.Batch.Types.NodeProperties
import Amazonka.Batch.Types.OrchestrationType
import Amazonka.Batch.Types.PlatformCapability
import Amazonka.Batch.Types.RetryStrategy
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents an Batch job definition.
--
-- /See:/ 'newJobDefinition' smart constructor.
data JobDefinition = JobDefinition'
  { -- | The tags that are applied to the job definition.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The timeout time for jobs that are submitted with this job definition.
    -- After the amount of time you specify passes, Batch terminates your jobs
    -- if they aren\'t finished.
    timeout :: Prelude.Maybe JobTimeout,
    -- | An object with various properties specific to Amazon ECS based jobs.
    -- Valid values are @containerProperties@, @eksProperties@, and
    -- @nodeProperties@. Only one can be specified.
    containerProperties :: Prelude.Maybe ContainerProperties,
    -- | The retry strategy to use for failed jobs that are submitted with this
    -- job definition.
    retryStrategy :: Prelude.Maybe RetryStrategy,
    -- | The platform capabilities required by the job definition. If no value is
    -- specified, it defaults to @EC2@. Jobs run on Fargate resources specify
    -- @FARGATE@.
    platformCapabilities :: Prelude.Maybe [PlatformCapability],
    -- | The orchestration type of the compute environment. The valid values are
    -- @ECS@ (default) or @EKS@.
    containerOrchestrationType :: Prelude.Maybe OrchestrationType,
    -- | The status of the job definition.
    status :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether to propagate the tags from the job or job definition
    -- to the corresponding Amazon ECS task. If no value is specified, the tags
    -- aren\'t propagated. Tags can only be propagated to the tasks when the
    -- tasks are created. For tags with the same name, job tags are given
    -- priority over job definitions tags. If the total number of combined tags
    -- from the job and job definition is over 50, the job is moved to the
    -- @FAILED@ state.
    propagateTags :: Prelude.Maybe Prelude.Bool,
    -- | An object with various properties that are specific to multi-node
    -- parallel jobs. Valid values are @containerProperties@, @eksProperties@,
    -- and @nodeProperties@. Only one can be specified.
    --
    -- If the job runs on Fargate resources, don\'t specify @nodeProperties@.
    -- Use @containerProperties@ instead.
    nodeProperties :: Prelude.Maybe NodeProperties,
    -- | The scheduling priority of the job definition. This only affects jobs in
    -- job queues with a fair share policy. Jobs with a higher scheduling
    -- priority are scheduled before jobs with a lower scheduling priority.
    schedulingPriority :: Prelude.Maybe Prelude.Int,
    -- | An object with various properties that are specific to Amazon EKS based
    -- jobs. Valid values are @containerProperties@, @eksProperties@, and
    -- @nodeProperties@. Only one can be specified.
    eksProperties :: Prelude.Maybe EksProperties,
    -- | Default parameters or parameter substitution placeholders that are set
    -- in the job definition. Parameters are specified as a key-value pair
    -- mapping. Parameters in a @SubmitJob@ request override any corresponding
    -- parameter defaults from the job definition. For more information about
    -- specifying parameters, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/job_definition_parameters.html Job definition parameters>
    -- in the /Batch User Guide/.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the job definition.
    jobDefinitionName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the job definition.
    jobDefinitionArn :: Prelude.Text,
    -- | The revision of the job definition.
    revision :: Prelude.Int,
    -- | The type of job definition. It\'s either @container@ or @multinode@. If
    -- the job is run on Fargate resources, then @multinode@ isn\'t supported.
    -- For more information about multi-node parallel jobs, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/multi-node-job-def.html Creating a multi-node parallel job definition>
    -- in the /Batch User Guide/.
    type' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'jobDefinition_tags' - The tags that are applied to the job definition.
--
-- 'timeout', 'jobDefinition_timeout' - The timeout time for jobs that are submitted with this job definition.
-- After the amount of time you specify passes, Batch terminates your jobs
-- if they aren\'t finished.
--
-- 'containerProperties', 'jobDefinition_containerProperties' - An object with various properties specific to Amazon ECS based jobs.
-- Valid values are @containerProperties@, @eksProperties@, and
-- @nodeProperties@. Only one can be specified.
--
-- 'retryStrategy', 'jobDefinition_retryStrategy' - The retry strategy to use for failed jobs that are submitted with this
-- job definition.
--
-- 'platformCapabilities', 'jobDefinition_platformCapabilities' - The platform capabilities required by the job definition. If no value is
-- specified, it defaults to @EC2@. Jobs run on Fargate resources specify
-- @FARGATE@.
--
-- 'containerOrchestrationType', 'jobDefinition_containerOrchestrationType' - The orchestration type of the compute environment. The valid values are
-- @ECS@ (default) or @EKS@.
--
-- 'status', 'jobDefinition_status' - The status of the job definition.
--
-- 'propagateTags', 'jobDefinition_propagateTags' - Specifies whether to propagate the tags from the job or job definition
-- to the corresponding Amazon ECS task. If no value is specified, the tags
-- aren\'t propagated. Tags can only be propagated to the tasks when the
-- tasks are created. For tags with the same name, job tags are given
-- priority over job definitions tags. If the total number of combined tags
-- from the job and job definition is over 50, the job is moved to the
-- @FAILED@ state.
--
-- 'nodeProperties', 'jobDefinition_nodeProperties' - An object with various properties that are specific to multi-node
-- parallel jobs. Valid values are @containerProperties@, @eksProperties@,
-- and @nodeProperties@. Only one can be specified.
--
-- If the job runs on Fargate resources, don\'t specify @nodeProperties@.
-- Use @containerProperties@ instead.
--
-- 'schedulingPriority', 'jobDefinition_schedulingPriority' - The scheduling priority of the job definition. This only affects jobs in
-- job queues with a fair share policy. Jobs with a higher scheduling
-- priority are scheduled before jobs with a lower scheduling priority.
--
-- 'eksProperties', 'jobDefinition_eksProperties' - An object with various properties that are specific to Amazon EKS based
-- jobs. Valid values are @containerProperties@, @eksProperties@, and
-- @nodeProperties@. Only one can be specified.
--
-- 'parameters', 'jobDefinition_parameters' - Default parameters or parameter substitution placeholders that are set
-- in the job definition. Parameters are specified as a key-value pair
-- mapping. Parameters in a @SubmitJob@ request override any corresponding
-- parameter defaults from the job definition. For more information about
-- specifying parameters, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/job_definition_parameters.html Job definition parameters>
-- in the /Batch User Guide/.
--
-- 'jobDefinitionName', 'jobDefinition_jobDefinitionName' - The name of the job definition.
--
-- 'jobDefinitionArn', 'jobDefinition_jobDefinitionArn' - The Amazon Resource Name (ARN) for the job definition.
--
-- 'revision', 'jobDefinition_revision' - The revision of the job definition.
--
-- 'type'', 'jobDefinition_type' - The type of job definition. It\'s either @container@ or @multinode@. If
-- the job is run on Fargate resources, then @multinode@ isn\'t supported.
-- For more information about multi-node parallel jobs, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/multi-node-job-def.html Creating a multi-node parallel job definition>
-- in the /Batch User Guide/.
newJobDefinition ::
  -- | 'jobDefinitionName'
  Prelude.Text ->
  -- | 'jobDefinitionArn'
  Prelude.Text ->
  -- | 'revision'
  Prelude.Int ->
  -- | 'type''
  Prelude.Text ->
  JobDefinition
newJobDefinition
  pJobDefinitionName_
  pJobDefinitionArn_
  pRevision_
  pType_ =
    JobDefinition'
      { tags = Prelude.Nothing,
        timeout = Prelude.Nothing,
        containerProperties = Prelude.Nothing,
        retryStrategy = Prelude.Nothing,
        platformCapabilities = Prelude.Nothing,
        containerOrchestrationType = Prelude.Nothing,
        status = Prelude.Nothing,
        propagateTags = Prelude.Nothing,
        nodeProperties = Prelude.Nothing,
        schedulingPriority = Prelude.Nothing,
        eksProperties = Prelude.Nothing,
        parameters = Prelude.Nothing,
        jobDefinitionName = pJobDefinitionName_,
        jobDefinitionArn = pJobDefinitionArn_,
        revision = pRevision_,
        type' = pType_
      }

-- | The tags that are applied to the job definition.
jobDefinition_tags :: Lens.Lens' JobDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
jobDefinition_tags = Lens.lens (\JobDefinition' {tags} -> tags) (\s@JobDefinition' {} a -> s {tags = a} :: JobDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The timeout time for jobs that are submitted with this job definition.
-- After the amount of time you specify passes, Batch terminates your jobs
-- if they aren\'t finished.
jobDefinition_timeout :: Lens.Lens' JobDefinition (Prelude.Maybe JobTimeout)
jobDefinition_timeout = Lens.lens (\JobDefinition' {timeout} -> timeout) (\s@JobDefinition' {} a -> s {timeout = a} :: JobDefinition)

-- | An object with various properties specific to Amazon ECS based jobs.
-- Valid values are @containerProperties@, @eksProperties@, and
-- @nodeProperties@. Only one can be specified.
jobDefinition_containerProperties :: Lens.Lens' JobDefinition (Prelude.Maybe ContainerProperties)
jobDefinition_containerProperties = Lens.lens (\JobDefinition' {containerProperties} -> containerProperties) (\s@JobDefinition' {} a -> s {containerProperties = a} :: JobDefinition)

-- | The retry strategy to use for failed jobs that are submitted with this
-- job definition.
jobDefinition_retryStrategy :: Lens.Lens' JobDefinition (Prelude.Maybe RetryStrategy)
jobDefinition_retryStrategy = Lens.lens (\JobDefinition' {retryStrategy} -> retryStrategy) (\s@JobDefinition' {} a -> s {retryStrategy = a} :: JobDefinition)

-- | The platform capabilities required by the job definition. If no value is
-- specified, it defaults to @EC2@. Jobs run on Fargate resources specify
-- @FARGATE@.
jobDefinition_platformCapabilities :: Lens.Lens' JobDefinition (Prelude.Maybe [PlatformCapability])
jobDefinition_platformCapabilities = Lens.lens (\JobDefinition' {platformCapabilities} -> platformCapabilities) (\s@JobDefinition' {} a -> s {platformCapabilities = a} :: JobDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The orchestration type of the compute environment. The valid values are
-- @ECS@ (default) or @EKS@.
jobDefinition_containerOrchestrationType :: Lens.Lens' JobDefinition (Prelude.Maybe OrchestrationType)
jobDefinition_containerOrchestrationType = Lens.lens (\JobDefinition' {containerOrchestrationType} -> containerOrchestrationType) (\s@JobDefinition' {} a -> s {containerOrchestrationType = a} :: JobDefinition)

-- | The status of the job definition.
jobDefinition_status :: Lens.Lens' JobDefinition (Prelude.Maybe Prelude.Text)
jobDefinition_status = Lens.lens (\JobDefinition' {status} -> status) (\s@JobDefinition' {} a -> s {status = a} :: JobDefinition)

-- | Specifies whether to propagate the tags from the job or job definition
-- to the corresponding Amazon ECS task. If no value is specified, the tags
-- aren\'t propagated. Tags can only be propagated to the tasks when the
-- tasks are created. For tags with the same name, job tags are given
-- priority over job definitions tags. If the total number of combined tags
-- from the job and job definition is over 50, the job is moved to the
-- @FAILED@ state.
jobDefinition_propagateTags :: Lens.Lens' JobDefinition (Prelude.Maybe Prelude.Bool)
jobDefinition_propagateTags = Lens.lens (\JobDefinition' {propagateTags} -> propagateTags) (\s@JobDefinition' {} a -> s {propagateTags = a} :: JobDefinition)

-- | An object with various properties that are specific to multi-node
-- parallel jobs. Valid values are @containerProperties@, @eksProperties@,
-- and @nodeProperties@. Only one can be specified.
--
-- If the job runs on Fargate resources, don\'t specify @nodeProperties@.
-- Use @containerProperties@ instead.
jobDefinition_nodeProperties :: Lens.Lens' JobDefinition (Prelude.Maybe NodeProperties)
jobDefinition_nodeProperties = Lens.lens (\JobDefinition' {nodeProperties} -> nodeProperties) (\s@JobDefinition' {} a -> s {nodeProperties = a} :: JobDefinition)

-- | The scheduling priority of the job definition. This only affects jobs in
-- job queues with a fair share policy. Jobs with a higher scheduling
-- priority are scheduled before jobs with a lower scheduling priority.
jobDefinition_schedulingPriority :: Lens.Lens' JobDefinition (Prelude.Maybe Prelude.Int)
jobDefinition_schedulingPriority = Lens.lens (\JobDefinition' {schedulingPriority} -> schedulingPriority) (\s@JobDefinition' {} a -> s {schedulingPriority = a} :: JobDefinition)

-- | An object with various properties that are specific to Amazon EKS based
-- jobs. Valid values are @containerProperties@, @eksProperties@, and
-- @nodeProperties@. Only one can be specified.
jobDefinition_eksProperties :: Lens.Lens' JobDefinition (Prelude.Maybe EksProperties)
jobDefinition_eksProperties = Lens.lens (\JobDefinition' {eksProperties} -> eksProperties) (\s@JobDefinition' {} a -> s {eksProperties = a} :: JobDefinition)

-- | Default parameters or parameter substitution placeholders that are set
-- in the job definition. Parameters are specified as a key-value pair
-- mapping. Parameters in a @SubmitJob@ request override any corresponding
-- parameter defaults from the job definition. For more information about
-- specifying parameters, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/job_definition_parameters.html Job definition parameters>
-- in the /Batch User Guide/.
jobDefinition_parameters :: Lens.Lens' JobDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
jobDefinition_parameters = Lens.lens (\JobDefinition' {parameters} -> parameters) (\s@JobDefinition' {} a -> s {parameters = a} :: JobDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The name of the job definition.
jobDefinition_jobDefinitionName :: Lens.Lens' JobDefinition Prelude.Text
jobDefinition_jobDefinitionName = Lens.lens (\JobDefinition' {jobDefinitionName} -> jobDefinitionName) (\s@JobDefinition' {} a -> s {jobDefinitionName = a} :: JobDefinition)

-- | The Amazon Resource Name (ARN) for the job definition.
jobDefinition_jobDefinitionArn :: Lens.Lens' JobDefinition Prelude.Text
jobDefinition_jobDefinitionArn = Lens.lens (\JobDefinition' {jobDefinitionArn} -> jobDefinitionArn) (\s@JobDefinition' {} a -> s {jobDefinitionArn = a} :: JobDefinition)

-- | The revision of the job definition.
jobDefinition_revision :: Lens.Lens' JobDefinition Prelude.Int
jobDefinition_revision = Lens.lens (\JobDefinition' {revision} -> revision) (\s@JobDefinition' {} a -> s {revision = a} :: JobDefinition)

-- | The type of job definition. It\'s either @container@ or @multinode@. If
-- the job is run on Fargate resources, then @multinode@ isn\'t supported.
-- For more information about multi-node parallel jobs, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/multi-node-job-def.html Creating a multi-node parallel job definition>
-- in the /Batch User Guide/.
jobDefinition_type :: Lens.Lens' JobDefinition Prelude.Text
jobDefinition_type = Lens.lens (\JobDefinition' {type'} -> type') (\s@JobDefinition' {} a -> s {type' = a} :: JobDefinition)

instance Core.FromJSON JobDefinition where
  parseJSON =
    Core.withObject
      "JobDefinition"
      ( \x ->
          JobDefinition'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "timeout")
            Prelude.<*> (x Core..:? "containerProperties")
            Prelude.<*> (x Core..:? "retryStrategy")
            Prelude.<*> ( x Core..:? "platformCapabilities"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "containerOrchestrationType")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "propagateTags")
            Prelude.<*> (x Core..:? "nodeProperties")
            Prelude.<*> (x Core..:? "schedulingPriority")
            Prelude.<*> (x Core..:? "eksProperties")
            Prelude.<*> (x Core..:? "parameters" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "jobDefinitionName")
            Prelude.<*> (x Core..: "jobDefinitionArn")
            Prelude.<*> (x Core..: "revision")
            Prelude.<*> (x Core..: "type")
      )

instance Prelude.Hashable JobDefinition where
  hashWithSalt _salt JobDefinition' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` timeout
      `Prelude.hashWithSalt` containerProperties
      `Prelude.hashWithSalt` retryStrategy
      `Prelude.hashWithSalt` platformCapabilities
      `Prelude.hashWithSalt` containerOrchestrationType
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` propagateTags
      `Prelude.hashWithSalt` nodeProperties
      `Prelude.hashWithSalt` schedulingPriority
      `Prelude.hashWithSalt` eksProperties
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` jobDefinitionName
      `Prelude.hashWithSalt` jobDefinitionArn
      `Prelude.hashWithSalt` revision
      `Prelude.hashWithSalt` type'

instance Prelude.NFData JobDefinition where
  rnf JobDefinition' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf timeout
      `Prelude.seq` Prelude.rnf containerProperties
      `Prelude.seq` Prelude.rnf retryStrategy
      `Prelude.seq` Prelude.rnf platformCapabilities
      `Prelude.seq` Prelude.rnf containerOrchestrationType
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf propagateTags
      `Prelude.seq` Prelude.rnf nodeProperties
      `Prelude.seq` Prelude.rnf schedulingPriority
      `Prelude.seq` Prelude.rnf eksProperties
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf jobDefinitionName
      `Prelude.seq` Prelude.rnf jobDefinitionArn
      `Prelude.seq` Prelude.rnf revision
      `Prelude.seq` Prelude.rnf type'
