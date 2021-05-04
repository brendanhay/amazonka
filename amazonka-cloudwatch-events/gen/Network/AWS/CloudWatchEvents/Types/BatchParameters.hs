{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudWatchEvents.Types.BatchParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.BatchParameters where

import Network.AWS.CloudWatchEvents.Types.BatchArrayProperties
import Network.AWS.CloudWatchEvents.Types.BatchRetryStrategy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The custom parameters to be used when the target is an AWS Batch job.
--
-- /See:/ 'newBatchParameters' smart constructor.
data BatchParameters = BatchParameters'
  { -- | The array properties for the submitted job, such as the size of the
    -- array. The array size can be between 2 and 10,000. If you specify array
    -- properties for a job, it becomes an array job. This parameter is used
    -- only if the target is an AWS Batch job.
    arrayProperties :: Prelude.Maybe BatchArrayProperties,
    -- | The retry strategy to use for failed jobs, if the target is an AWS Batch
    -- job. The retry strategy is the number of times to retry the failed job
    -- execution. Valid values are 1–10. When you specify a retry strategy
    -- here, it overrides the retry strategy defined in the job definition.
    retryStrategy :: Prelude.Maybe BatchRetryStrategy,
    -- | The ARN or name of the job definition to use if the event target is an
    -- AWS Batch job. This job definition must already exist.
    jobDefinition :: Prelude.Text,
    -- | The name to use for this execution of the job, if the target is an AWS
    -- Batch job.
    jobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arrayProperties', 'batchParameters_arrayProperties' - The array properties for the submitted job, such as the size of the
-- array. The array size can be between 2 and 10,000. If you specify array
-- properties for a job, it becomes an array job. This parameter is used
-- only if the target is an AWS Batch job.
--
-- 'retryStrategy', 'batchParameters_retryStrategy' - The retry strategy to use for failed jobs, if the target is an AWS Batch
-- job. The retry strategy is the number of times to retry the failed job
-- execution. Valid values are 1–10. When you specify a retry strategy
-- here, it overrides the retry strategy defined in the job definition.
--
-- 'jobDefinition', 'batchParameters_jobDefinition' - The ARN or name of the job definition to use if the event target is an
-- AWS Batch job. This job definition must already exist.
--
-- 'jobName', 'batchParameters_jobName' - The name to use for this execution of the job, if the target is an AWS
-- Batch job.
newBatchParameters ::
  -- | 'jobDefinition'
  Prelude.Text ->
  -- | 'jobName'
  Prelude.Text ->
  BatchParameters
newBatchParameters pJobDefinition_ pJobName_ =
  BatchParameters'
    { arrayProperties = Prelude.Nothing,
      retryStrategy = Prelude.Nothing,
      jobDefinition = pJobDefinition_,
      jobName = pJobName_
    }

-- | The array properties for the submitted job, such as the size of the
-- array. The array size can be between 2 and 10,000. If you specify array
-- properties for a job, it becomes an array job. This parameter is used
-- only if the target is an AWS Batch job.
batchParameters_arrayProperties :: Lens.Lens' BatchParameters (Prelude.Maybe BatchArrayProperties)
batchParameters_arrayProperties = Lens.lens (\BatchParameters' {arrayProperties} -> arrayProperties) (\s@BatchParameters' {} a -> s {arrayProperties = a} :: BatchParameters)

-- | The retry strategy to use for failed jobs, if the target is an AWS Batch
-- job. The retry strategy is the number of times to retry the failed job
-- execution. Valid values are 1–10. When you specify a retry strategy
-- here, it overrides the retry strategy defined in the job definition.
batchParameters_retryStrategy :: Lens.Lens' BatchParameters (Prelude.Maybe BatchRetryStrategy)
batchParameters_retryStrategy = Lens.lens (\BatchParameters' {retryStrategy} -> retryStrategy) (\s@BatchParameters' {} a -> s {retryStrategy = a} :: BatchParameters)

-- | The ARN or name of the job definition to use if the event target is an
-- AWS Batch job. This job definition must already exist.
batchParameters_jobDefinition :: Lens.Lens' BatchParameters Prelude.Text
batchParameters_jobDefinition = Lens.lens (\BatchParameters' {jobDefinition} -> jobDefinition) (\s@BatchParameters' {} a -> s {jobDefinition = a} :: BatchParameters)

-- | The name to use for this execution of the job, if the target is an AWS
-- Batch job.
batchParameters_jobName :: Lens.Lens' BatchParameters Prelude.Text
batchParameters_jobName = Lens.lens (\BatchParameters' {jobName} -> jobName) (\s@BatchParameters' {} a -> s {jobName = a} :: BatchParameters)

instance Prelude.FromJSON BatchParameters where
  parseJSON =
    Prelude.withObject
      "BatchParameters"
      ( \x ->
          BatchParameters'
            Prelude.<$> (x Prelude..:? "ArrayProperties")
            Prelude.<*> (x Prelude..:? "RetryStrategy")
            Prelude.<*> (x Prelude..: "JobDefinition")
            Prelude.<*> (x Prelude..: "JobName")
      )

instance Prelude.Hashable BatchParameters

instance Prelude.NFData BatchParameters

instance Prelude.ToJSON BatchParameters where
  toJSON BatchParameters' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ArrayProperties" Prelude..=)
              Prelude.<$> arrayProperties,
            ("RetryStrategy" Prelude..=)
              Prelude.<$> retryStrategy,
            Prelude.Just
              ("JobDefinition" Prelude..= jobDefinition),
            Prelude.Just ("JobName" Prelude..= jobName)
          ]
      )
