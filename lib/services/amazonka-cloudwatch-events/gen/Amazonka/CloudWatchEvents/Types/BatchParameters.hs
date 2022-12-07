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
-- Module      : Amazonka.CloudWatchEvents.Types.BatchParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.BatchParameters where

import Amazonka.CloudWatchEvents.Types.BatchArrayProperties
import Amazonka.CloudWatchEvents.Types.BatchRetryStrategy
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The custom parameters to be used when the target is an Batch job.
--
-- /See:/ 'newBatchParameters' smart constructor.
data BatchParameters = BatchParameters'
  { -- | The retry strategy to use for failed jobs, if the target is an Batch
    -- job. The retry strategy is the number of times to retry the failed job
    -- execution. Valid values are 1–10. When you specify a retry strategy
    -- here, it overrides the retry strategy defined in the job definition.
    retryStrategy :: Prelude.Maybe BatchRetryStrategy,
    -- | The array properties for the submitted job, such as the size of the
    -- array. The array size can be between 2 and 10,000. If you specify array
    -- properties for a job, it becomes an array job. This parameter is used
    -- only if the target is an Batch job.
    arrayProperties :: Prelude.Maybe BatchArrayProperties,
    -- | The ARN or name of the job definition to use if the event target is an
    -- Batch job. This job definition must already exist.
    jobDefinition :: Prelude.Text,
    -- | The name to use for this execution of the job, if the target is an Batch
    -- job.
    jobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'retryStrategy', 'batchParameters_retryStrategy' - The retry strategy to use for failed jobs, if the target is an Batch
-- job. The retry strategy is the number of times to retry the failed job
-- execution. Valid values are 1–10. When you specify a retry strategy
-- here, it overrides the retry strategy defined in the job definition.
--
-- 'arrayProperties', 'batchParameters_arrayProperties' - The array properties for the submitted job, such as the size of the
-- array. The array size can be between 2 and 10,000. If you specify array
-- properties for a job, it becomes an array job. This parameter is used
-- only if the target is an Batch job.
--
-- 'jobDefinition', 'batchParameters_jobDefinition' - The ARN or name of the job definition to use if the event target is an
-- Batch job. This job definition must already exist.
--
-- 'jobName', 'batchParameters_jobName' - The name to use for this execution of the job, if the target is an Batch
-- job.
newBatchParameters ::
  -- | 'jobDefinition'
  Prelude.Text ->
  -- | 'jobName'
  Prelude.Text ->
  BatchParameters
newBatchParameters pJobDefinition_ pJobName_ =
  BatchParameters'
    { retryStrategy = Prelude.Nothing,
      arrayProperties = Prelude.Nothing,
      jobDefinition = pJobDefinition_,
      jobName = pJobName_
    }

-- | The retry strategy to use for failed jobs, if the target is an Batch
-- job. The retry strategy is the number of times to retry the failed job
-- execution. Valid values are 1–10. When you specify a retry strategy
-- here, it overrides the retry strategy defined in the job definition.
batchParameters_retryStrategy :: Lens.Lens' BatchParameters (Prelude.Maybe BatchRetryStrategy)
batchParameters_retryStrategy = Lens.lens (\BatchParameters' {retryStrategy} -> retryStrategy) (\s@BatchParameters' {} a -> s {retryStrategy = a} :: BatchParameters)

-- | The array properties for the submitted job, such as the size of the
-- array. The array size can be between 2 and 10,000. If you specify array
-- properties for a job, it becomes an array job. This parameter is used
-- only if the target is an Batch job.
batchParameters_arrayProperties :: Lens.Lens' BatchParameters (Prelude.Maybe BatchArrayProperties)
batchParameters_arrayProperties = Lens.lens (\BatchParameters' {arrayProperties} -> arrayProperties) (\s@BatchParameters' {} a -> s {arrayProperties = a} :: BatchParameters)

-- | The ARN or name of the job definition to use if the event target is an
-- Batch job. This job definition must already exist.
batchParameters_jobDefinition :: Lens.Lens' BatchParameters Prelude.Text
batchParameters_jobDefinition = Lens.lens (\BatchParameters' {jobDefinition} -> jobDefinition) (\s@BatchParameters' {} a -> s {jobDefinition = a} :: BatchParameters)

-- | The name to use for this execution of the job, if the target is an Batch
-- job.
batchParameters_jobName :: Lens.Lens' BatchParameters Prelude.Text
batchParameters_jobName = Lens.lens (\BatchParameters' {jobName} -> jobName) (\s@BatchParameters' {} a -> s {jobName = a} :: BatchParameters)

instance Data.FromJSON BatchParameters where
  parseJSON =
    Data.withObject
      "BatchParameters"
      ( \x ->
          BatchParameters'
            Prelude.<$> (x Data..:? "RetryStrategy")
            Prelude.<*> (x Data..:? "ArrayProperties")
            Prelude.<*> (x Data..: "JobDefinition")
            Prelude.<*> (x Data..: "JobName")
      )

instance Prelude.Hashable BatchParameters where
  hashWithSalt _salt BatchParameters' {..} =
    _salt `Prelude.hashWithSalt` retryStrategy
      `Prelude.hashWithSalt` arrayProperties
      `Prelude.hashWithSalt` jobDefinition
      `Prelude.hashWithSalt` jobName

instance Prelude.NFData BatchParameters where
  rnf BatchParameters' {..} =
    Prelude.rnf retryStrategy
      `Prelude.seq` Prelude.rnf arrayProperties
      `Prelude.seq` Prelude.rnf jobDefinition
      `Prelude.seq` Prelude.rnf jobName

instance Data.ToJSON BatchParameters where
  toJSON BatchParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RetryStrategy" Data..=) Prelude.<$> retryStrategy,
            ("ArrayProperties" Data..=)
              Prelude.<$> arrayProperties,
            Prelude.Just ("JobDefinition" Data..= jobDefinition),
            Prelude.Just ("JobName" Data..= jobName)
          ]
      )
