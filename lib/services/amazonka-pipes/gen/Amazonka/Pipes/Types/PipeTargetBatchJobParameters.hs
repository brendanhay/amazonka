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
-- Module      : Amazonka.Pipes.Types.PipeTargetBatchJobParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.PipeTargetBatchJobParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types.BatchArrayProperties
import Amazonka.Pipes.Types.BatchContainerOverrides
import Amazonka.Pipes.Types.BatchJobDependency
import Amazonka.Pipes.Types.BatchRetryStrategy
import qualified Amazonka.Prelude as Prelude

-- | The parameters for using an Batch job as a target.
--
-- /See:/ 'newPipeTargetBatchJobParameters' smart constructor.
data PipeTargetBatchJobParameters = PipeTargetBatchJobParameters'
  { -- | The array properties for the submitted job, such as the size of the
    -- array. The array size can be between 2 and 10,000. If you specify array
    -- properties for a job, it becomes an array job. This parameter is used
    -- only if the target is an Batch job.
    arrayProperties :: Prelude.Maybe BatchArrayProperties,
    -- | The overrides that are sent to a container.
    containerOverrides :: Prelude.Maybe BatchContainerOverrides,
    -- | A list of dependencies for the job. A job can depend upon a maximum of
    -- 20 jobs. You can specify a @SEQUENTIAL@ type dependency without
    -- specifying a job ID for array jobs so that each child array job
    -- completes sequentially, starting at index 0. You can also specify an
    -- @N_TO_N@ type dependency with a job ID for array jobs. In that case,
    -- each index child of this job must wait for the corresponding index child
    -- of each dependency to complete before it can begin.
    dependsOn :: Prelude.Maybe [BatchJobDependency],
    -- | Additional parameters passed to the job that replace parameter
    -- substitution placeholders that are set in the job definition. Parameters
    -- are specified as a key and value pair mapping. Parameters included here
    -- override any corresponding parameter defaults from the job definition.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The retry strategy to use for failed jobs. When a retry strategy is
    -- specified here, it overrides the retry strategy defined in the job
    -- definition.
    retryStrategy :: Prelude.Maybe BatchRetryStrategy,
    -- | The job definition used by this job. This value can be one of @name@,
    -- @name:revision@, or the Amazon Resource Name (ARN) for the job
    -- definition. If name is specified without a revision then the latest
    -- active revision is used.
    jobDefinition :: Prelude.Text,
    -- | The name of the job. It can be up to 128 letters long. The first
    -- character must be alphanumeric, can contain uppercase and lowercase
    -- letters, numbers, hyphens (-), and underscores (_).
    jobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipeTargetBatchJobParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arrayProperties', 'pipeTargetBatchJobParameters_arrayProperties' - The array properties for the submitted job, such as the size of the
-- array. The array size can be between 2 and 10,000. If you specify array
-- properties for a job, it becomes an array job. This parameter is used
-- only if the target is an Batch job.
--
-- 'containerOverrides', 'pipeTargetBatchJobParameters_containerOverrides' - The overrides that are sent to a container.
--
-- 'dependsOn', 'pipeTargetBatchJobParameters_dependsOn' - A list of dependencies for the job. A job can depend upon a maximum of
-- 20 jobs. You can specify a @SEQUENTIAL@ type dependency without
-- specifying a job ID for array jobs so that each child array job
-- completes sequentially, starting at index 0. You can also specify an
-- @N_TO_N@ type dependency with a job ID for array jobs. In that case,
-- each index child of this job must wait for the corresponding index child
-- of each dependency to complete before it can begin.
--
-- 'parameters', 'pipeTargetBatchJobParameters_parameters' - Additional parameters passed to the job that replace parameter
-- substitution placeholders that are set in the job definition. Parameters
-- are specified as a key and value pair mapping. Parameters included here
-- override any corresponding parameter defaults from the job definition.
--
-- 'retryStrategy', 'pipeTargetBatchJobParameters_retryStrategy' - The retry strategy to use for failed jobs. When a retry strategy is
-- specified here, it overrides the retry strategy defined in the job
-- definition.
--
-- 'jobDefinition', 'pipeTargetBatchJobParameters_jobDefinition' - The job definition used by this job. This value can be one of @name@,
-- @name:revision@, or the Amazon Resource Name (ARN) for the job
-- definition. If name is specified without a revision then the latest
-- active revision is used.
--
-- 'jobName', 'pipeTargetBatchJobParameters_jobName' - The name of the job. It can be up to 128 letters long. The first
-- character must be alphanumeric, can contain uppercase and lowercase
-- letters, numbers, hyphens (-), and underscores (_).
newPipeTargetBatchJobParameters ::
  -- | 'jobDefinition'
  Prelude.Text ->
  -- | 'jobName'
  Prelude.Text ->
  PipeTargetBatchJobParameters
newPipeTargetBatchJobParameters
  pJobDefinition_
  pJobName_ =
    PipeTargetBatchJobParameters'
      { arrayProperties =
          Prelude.Nothing,
        containerOverrides = Prelude.Nothing,
        dependsOn = Prelude.Nothing,
        parameters = Prelude.Nothing,
        retryStrategy = Prelude.Nothing,
        jobDefinition = pJobDefinition_,
        jobName = pJobName_
      }

-- | The array properties for the submitted job, such as the size of the
-- array. The array size can be between 2 and 10,000. If you specify array
-- properties for a job, it becomes an array job. This parameter is used
-- only if the target is an Batch job.
pipeTargetBatchJobParameters_arrayProperties :: Lens.Lens' PipeTargetBatchJobParameters (Prelude.Maybe BatchArrayProperties)
pipeTargetBatchJobParameters_arrayProperties = Lens.lens (\PipeTargetBatchJobParameters' {arrayProperties} -> arrayProperties) (\s@PipeTargetBatchJobParameters' {} a -> s {arrayProperties = a} :: PipeTargetBatchJobParameters)

-- | The overrides that are sent to a container.
pipeTargetBatchJobParameters_containerOverrides :: Lens.Lens' PipeTargetBatchJobParameters (Prelude.Maybe BatchContainerOverrides)
pipeTargetBatchJobParameters_containerOverrides = Lens.lens (\PipeTargetBatchJobParameters' {containerOverrides} -> containerOverrides) (\s@PipeTargetBatchJobParameters' {} a -> s {containerOverrides = a} :: PipeTargetBatchJobParameters)

-- | A list of dependencies for the job. A job can depend upon a maximum of
-- 20 jobs. You can specify a @SEQUENTIAL@ type dependency without
-- specifying a job ID for array jobs so that each child array job
-- completes sequentially, starting at index 0. You can also specify an
-- @N_TO_N@ type dependency with a job ID for array jobs. In that case,
-- each index child of this job must wait for the corresponding index child
-- of each dependency to complete before it can begin.
pipeTargetBatchJobParameters_dependsOn :: Lens.Lens' PipeTargetBatchJobParameters (Prelude.Maybe [BatchJobDependency])
pipeTargetBatchJobParameters_dependsOn = Lens.lens (\PipeTargetBatchJobParameters' {dependsOn} -> dependsOn) (\s@PipeTargetBatchJobParameters' {} a -> s {dependsOn = a} :: PipeTargetBatchJobParameters) Prelude.. Lens.mapping Lens.coerced

-- | Additional parameters passed to the job that replace parameter
-- substitution placeholders that are set in the job definition. Parameters
-- are specified as a key and value pair mapping. Parameters included here
-- override any corresponding parameter defaults from the job definition.
pipeTargetBatchJobParameters_parameters :: Lens.Lens' PipeTargetBatchJobParameters (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
pipeTargetBatchJobParameters_parameters = Lens.lens (\PipeTargetBatchJobParameters' {parameters} -> parameters) (\s@PipeTargetBatchJobParameters' {} a -> s {parameters = a} :: PipeTargetBatchJobParameters) Prelude.. Lens.mapping Lens.coerced

-- | The retry strategy to use for failed jobs. When a retry strategy is
-- specified here, it overrides the retry strategy defined in the job
-- definition.
pipeTargetBatchJobParameters_retryStrategy :: Lens.Lens' PipeTargetBatchJobParameters (Prelude.Maybe BatchRetryStrategy)
pipeTargetBatchJobParameters_retryStrategy = Lens.lens (\PipeTargetBatchJobParameters' {retryStrategy} -> retryStrategy) (\s@PipeTargetBatchJobParameters' {} a -> s {retryStrategy = a} :: PipeTargetBatchJobParameters)

-- | The job definition used by this job. This value can be one of @name@,
-- @name:revision@, or the Amazon Resource Name (ARN) for the job
-- definition. If name is specified without a revision then the latest
-- active revision is used.
pipeTargetBatchJobParameters_jobDefinition :: Lens.Lens' PipeTargetBatchJobParameters Prelude.Text
pipeTargetBatchJobParameters_jobDefinition = Lens.lens (\PipeTargetBatchJobParameters' {jobDefinition} -> jobDefinition) (\s@PipeTargetBatchJobParameters' {} a -> s {jobDefinition = a} :: PipeTargetBatchJobParameters)

-- | The name of the job. It can be up to 128 letters long. The first
-- character must be alphanumeric, can contain uppercase and lowercase
-- letters, numbers, hyphens (-), and underscores (_).
pipeTargetBatchJobParameters_jobName :: Lens.Lens' PipeTargetBatchJobParameters Prelude.Text
pipeTargetBatchJobParameters_jobName = Lens.lens (\PipeTargetBatchJobParameters' {jobName} -> jobName) (\s@PipeTargetBatchJobParameters' {} a -> s {jobName = a} :: PipeTargetBatchJobParameters)

instance Data.FromJSON PipeTargetBatchJobParameters where
  parseJSON =
    Data.withObject
      "PipeTargetBatchJobParameters"
      ( \x ->
          PipeTargetBatchJobParameters'
            Prelude.<$> (x Data..:? "ArrayProperties")
            Prelude.<*> (x Data..:? "ContainerOverrides")
            Prelude.<*> (x Data..:? "DependsOn" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Parameters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "RetryStrategy")
            Prelude.<*> (x Data..: "JobDefinition")
            Prelude.<*> (x Data..: "JobName")
      )

instance
  Prelude.Hashable
    PipeTargetBatchJobParameters
  where
  hashWithSalt _salt PipeTargetBatchJobParameters' {..} =
    _salt
      `Prelude.hashWithSalt` arrayProperties
      `Prelude.hashWithSalt` containerOverrides
      `Prelude.hashWithSalt` dependsOn
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` retryStrategy
      `Prelude.hashWithSalt` jobDefinition
      `Prelude.hashWithSalt` jobName

instance Prelude.NFData PipeTargetBatchJobParameters where
  rnf PipeTargetBatchJobParameters' {..} =
    Prelude.rnf arrayProperties
      `Prelude.seq` Prelude.rnf containerOverrides
      `Prelude.seq` Prelude.rnf dependsOn
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf retryStrategy
      `Prelude.seq` Prelude.rnf jobDefinition
      `Prelude.seq` Prelude.rnf jobName

instance Data.ToJSON PipeTargetBatchJobParameters where
  toJSON PipeTargetBatchJobParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ArrayProperties" Data..=)
              Prelude.<$> arrayProperties,
            ("ContainerOverrides" Data..=)
              Prelude.<$> containerOverrides,
            ("DependsOn" Data..=) Prelude.<$> dependsOn,
            ("Parameters" Data..=) Prelude.<$> parameters,
            ("RetryStrategy" Data..=) Prelude.<$> retryStrategy,
            Prelude.Just ("JobDefinition" Data..= jobDefinition),
            Prelude.Just ("JobName" Data..= jobName)
          ]
      )
