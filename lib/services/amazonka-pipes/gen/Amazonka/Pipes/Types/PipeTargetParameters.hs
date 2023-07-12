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
-- Module      : Amazonka.Pipes.Types.PipeTargetParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.PipeTargetParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types.PipeTargetBatchJobParameters
import Amazonka.Pipes.Types.PipeTargetCloudWatchLogsParameters
import Amazonka.Pipes.Types.PipeTargetEcsTaskParameters
import Amazonka.Pipes.Types.PipeTargetEventBridgeEventBusParameters
import Amazonka.Pipes.Types.PipeTargetHttpParameters
import Amazonka.Pipes.Types.PipeTargetKinesisStreamParameters
import Amazonka.Pipes.Types.PipeTargetLambdaFunctionParameters
import Amazonka.Pipes.Types.PipeTargetRedshiftDataParameters
import Amazonka.Pipes.Types.PipeTargetSageMakerPipelineParameters
import Amazonka.Pipes.Types.PipeTargetSqsQueueParameters
import Amazonka.Pipes.Types.PipeTargetStateMachineParameters
import qualified Amazonka.Prelude as Prelude

-- | The parameters required to set up a target for your pipe.
--
-- /See:/ 'newPipeTargetParameters' smart constructor.
data PipeTargetParameters = PipeTargetParameters'
  { -- | The parameters for using an Batch job as a target.
    batchJobParameters :: Prelude.Maybe PipeTargetBatchJobParameters,
    -- | The parameters for using an CloudWatch Logs log stream as a target.
    cloudWatchLogsParameters :: Prelude.Maybe PipeTargetCloudWatchLogsParameters,
    -- | The parameters for using an Amazon ECS task as a target.
    ecsTaskParameters :: Prelude.Maybe PipeTargetEcsTaskParameters,
    -- | The parameters for using an EventBridge event bus as a target.
    eventBridgeEventBusParameters :: Prelude.Maybe PipeTargetEventBridgeEventBusParameters,
    -- | These are custom parameter to be used when the target is an API Gateway
    -- REST APIs or EventBridge ApiDestinations.
    httpParameters :: Prelude.Maybe PipeTargetHttpParameters,
    -- | Valid JSON text passed to the target. In this case, nothing from the
    -- event itself is passed to the target. For more information, see
    -- <http://www.rfc-editor.org/rfc/rfc7159.txt The JavaScript Object Notation (JSON) Data Interchange Format>.
    inputTemplate :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The parameters for using a Kinesis stream as a source.
    kinesisStreamParameters :: Prelude.Maybe PipeTargetKinesisStreamParameters,
    -- | The parameters for using a Lambda function as a target.
    lambdaFunctionParameters :: Prelude.Maybe PipeTargetLambdaFunctionParameters,
    -- | These are custom parameters to be used when the target is a Amazon
    -- Redshift cluster to invoke the Amazon Redshift Data API
    -- ExecuteStatement.
    redshiftDataParameters :: Prelude.Maybe PipeTargetRedshiftDataParameters,
    -- | The parameters for using a SageMaker pipeline as a target.
    sageMakerPipelineParameters :: Prelude.Maybe PipeTargetSageMakerPipelineParameters,
    -- | The parameters for using a Amazon SQS stream as a source.
    sqsQueueParameters :: Prelude.Maybe PipeTargetSqsQueueParameters,
    -- | The parameters for using a Step Functions state machine as a target.
    stepFunctionStateMachineParameters :: Prelude.Maybe PipeTargetStateMachineParameters
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipeTargetParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchJobParameters', 'pipeTargetParameters_batchJobParameters' - The parameters for using an Batch job as a target.
--
-- 'cloudWatchLogsParameters', 'pipeTargetParameters_cloudWatchLogsParameters' - The parameters for using an CloudWatch Logs log stream as a target.
--
-- 'ecsTaskParameters', 'pipeTargetParameters_ecsTaskParameters' - The parameters for using an Amazon ECS task as a target.
--
-- 'eventBridgeEventBusParameters', 'pipeTargetParameters_eventBridgeEventBusParameters' - The parameters for using an EventBridge event bus as a target.
--
-- 'httpParameters', 'pipeTargetParameters_httpParameters' - These are custom parameter to be used when the target is an API Gateway
-- REST APIs or EventBridge ApiDestinations.
--
-- 'inputTemplate', 'pipeTargetParameters_inputTemplate' - Valid JSON text passed to the target. In this case, nothing from the
-- event itself is passed to the target. For more information, see
-- <http://www.rfc-editor.org/rfc/rfc7159.txt The JavaScript Object Notation (JSON) Data Interchange Format>.
--
-- 'kinesisStreamParameters', 'pipeTargetParameters_kinesisStreamParameters' - The parameters for using a Kinesis stream as a source.
--
-- 'lambdaFunctionParameters', 'pipeTargetParameters_lambdaFunctionParameters' - The parameters for using a Lambda function as a target.
--
-- 'redshiftDataParameters', 'pipeTargetParameters_redshiftDataParameters' - These are custom parameters to be used when the target is a Amazon
-- Redshift cluster to invoke the Amazon Redshift Data API
-- ExecuteStatement.
--
-- 'sageMakerPipelineParameters', 'pipeTargetParameters_sageMakerPipelineParameters' - The parameters for using a SageMaker pipeline as a target.
--
-- 'sqsQueueParameters', 'pipeTargetParameters_sqsQueueParameters' - The parameters for using a Amazon SQS stream as a source.
--
-- 'stepFunctionStateMachineParameters', 'pipeTargetParameters_stepFunctionStateMachineParameters' - The parameters for using a Step Functions state machine as a target.
newPipeTargetParameters ::
  PipeTargetParameters
newPipeTargetParameters =
  PipeTargetParameters'
    { batchJobParameters =
        Prelude.Nothing,
      cloudWatchLogsParameters = Prelude.Nothing,
      ecsTaskParameters = Prelude.Nothing,
      eventBridgeEventBusParameters = Prelude.Nothing,
      httpParameters = Prelude.Nothing,
      inputTemplate = Prelude.Nothing,
      kinesisStreamParameters = Prelude.Nothing,
      lambdaFunctionParameters = Prelude.Nothing,
      redshiftDataParameters = Prelude.Nothing,
      sageMakerPipelineParameters = Prelude.Nothing,
      sqsQueueParameters = Prelude.Nothing,
      stepFunctionStateMachineParameters = Prelude.Nothing
    }

-- | The parameters for using an Batch job as a target.
pipeTargetParameters_batchJobParameters :: Lens.Lens' PipeTargetParameters (Prelude.Maybe PipeTargetBatchJobParameters)
pipeTargetParameters_batchJobParameters = Lens.lens (\PipeTargetParameters' {batchJobParameters} -> batchJobParameters) (\s@PipeTargetParameters' {} a -> s {batchJobParameters = a} :: PipeTargetParameters)

-- | The parameters for using an CloudWatch Logs log stream as a target.
pipeTargetParameters_cloudWatchLogsParameters :: Lens.Lens' PipeTargetParameters (Prelude.Maybe PipeTargetCloudWatchLogsParameters)
pipeTargetParameters_cloudWatchLogsParameters = Lens.lens (\PipeTargetParameters' {cloudWatchLogsParameters} -> cloudWatchLogsParameters) (\s@PipeTargetParameters' {} a -> s {cloudWatchLogsParameters = a} :: PipeTargetParameters)

-- | The parameters for using an Amazon ECS task as a target.
pipeTargetParameters_ecsTaskParameters :: Lens.Lens' PipeTargetParameters (Prelude.Maybe PipeTargetEcsTaskParameters)
pipeTargetParameters_ecsTaskParameters = Lens.lens (\PipeTargetParameters' {ecsTaskParameters} -> ecsTaskParameters) (\s@PipeTargetParameters' {} a -> s {ecsTaskParameters = a} :: PipeTargetParameters)

-- | The parameters for using an EventBridge event bus as a target.
pipeTargetParameters_eventBridgeEventBusParameters :: Lens.Lens' PipeTargetParameters (Prelude.Maybe PipeTargetEventBridgeEventBusParameters)
pipeTargetParameters_eventBridgeEventBusParameters = Lens.lens (\PipeTargetParameters' {eventBridgeEventBusParameters} -> eventBridgeEventBusParameters) (\s@PipeTargetParameters' {} a -> s {eventBridgeEventBusParameters = a} :: PipeTargetParameters)

-- | These are custom parameter to be used when the target is an API Gateway
-- REST APIs or EventBridge ApiDestinations.
pipeTargetParameters_httpParameters :: Lens.Lens' PipeTargetParameters (Prelude.Maybe PipeTargetHttpParameters)
pipeTargetParameters_httpParameters = Lens.lens (\PipeTargetParameters' {httpParameters} -> httpParameters) (\s@PipeTargetParameters' {} a -> s {httpParameters = a} :: PipeTargetParameters)

-- | Valid JSON text passed to the target. In this case, nothing from the
-- event itself is passed to the target. For more information, see
-- <http://www.rfc-editor.org/rfc/rfc7159.txt The JavaScript Object Notation (JSON) Data Interchange Format>.
pipeTargetParameters_inputTemplate :: Lens.Lens' PipeTargetParameters (Prelude.Maybe Prelude.Text)
pipeTargetParameters_inputTemplate = Lens.lens (\PipeTargetParameters' {inputTemplate} -> inputTemplate) (\s@PipeTargetParameters' {} a -> s {inputTemplate = a} :: PipeTargetParameters) Prelude.. Lens.mapping Data._Sensitive

-- | The parameters for using a Kinesis stream as a source.
pipeTargetParameters_kinesisStreamParameters :: Lens.Lens' PipeTargetParameters (Prelude.Maybe PipeTargetKinesisStreamParameters)
pipeTargetParameters_kinesisStreamParameters = Lens.lens (\PipeTargetParameters' {kinesisStreamParameters} -> kinesisStreamParameters) (\s@PipeTargetParameters' {} a -> s {kinesisStreamParameters = a} :: PipeTargetParameters)

-- | The parameters for using a Lambda function as a target.
pipeTargetParameters_lambdaFunctionParameters :: Lens.Lens' PipeTargetParameters (Prelude.Maybe PipeTargetLambdaFunctionParameters)
pipeTargetParameters_lambdaFunctionParameters = Lens.lens (\PipeTargetParameters' {lambdaFunctionParameters} -> lambdaFunctionParameters) (\s@PipeTargetParameters' {} a -> s {lambdaFunctionParameters = a} :: PipeTargetParameters)

-- | These are custom parameters to be used when the target is a Amazon
-- Redshift cluster to invoke the Amazon Redshift Data API
-- ExecuteStatement.
pipeTargetParameters_redshiftDataParameters :: Lens.Lens' PipeTargetParameters (Prelude.Maybe PipeTargetRedshiftDataParameters)
pipeTargetParameters_redshiftDataParameters = Lens.lens (\PipeTargetParameters' {redshiftDataParameters} -> redshiftDataParameters) (\s@PipeTargetParameters' {} a -> s {redshiftDataParameters = a} :: PipeTargetParameters)

-- | The parameters for using a SageMaker pipeline as a target.
pipeTargetParameters_sageMakerPipelineParameters :: Lens.Lens' PipeTargetParameters (Prelude.Maybe PipeTargetSageMakerPipelineParameters)
pipeTargetParameters_sageMakerPipelineParameters = Lens.lens (\PipeTargetParameters' {sageMakerPipelineParameters} -> sageMakerPipelineParameters) (\s@PipeTargetParameters' {} a -> s {sageMakerPipelineParameters = a} :: PipeTargetParameters)

-- | The parameters for using a Amazon SQS stream as a source.
pipeTargetParameters_sqsQueueParameters :: Lens.Lens' PipeTargetParameters (Prelude.Maybe PipeTargetSqsQueueParameters)
pipeTargetParameters_sqsQueueParameters = Lens.lens (\PipeTargetParameters' {sqsQueueParameters} -> sqsQueueParameters) (\s@PipeTargetParameters' {} a -> s {sqsQueueParameters = a} :: PipeTargetParameters)

-- | The parameters for using a Step Functions state machine as a target.
pipeTargetParameters_stepFunctionStateMachineParameters :: Lens.Lens' PipeTargetParameters (Prelude.Maybe PipeTargetStateMachineParameters)
pipeTargetParameters_stepFunctionStateMachineParameters = Lens.lens (\PipeTargetParameters' {stepFunctionStateMachineParameters} -> stepFunctionStateMachineParameters) (\s@PipeTargetParameters' {} a -> s {stepFunctionStateMachineParameters = a} :: PipeTargetParameters)

instance Data.FromJSON PipeTargetParameters where
  parseJSON =
    Data.withObject
      "PipeTargetParameters"
      ( \x ->
          PipeTargetParameters'
            Prelude.<$> (x Data..:? "BatchJobParameters")
            Prelude.<*> (x Data..:? "CloudWatchLogsParameters")
            Prelude.<*> (x Data..:? "EcsTaskParameters")
            Prelude.<*> (x Data..:? "EventBridgeEventBusParameters")
            Prelude.<*> (x Data..:? "HttpParameters")
            Prelude.<*> (x Data..:? "InputTemplate")
            Prelude.<*> (x Data..:? "KinesisStreamParameters")
            Prelude.<*> (x Data..:? "LambdaFunctionParameters")
            Prelude.<*> (x Data..:? "RedshiftDataParameters")
            Prelude.<*> (x Data..:? "SageMakerPipelineParameters")
            Prelude.<*> (x Data..:? "SqsQueueParameters")
            Prelude.<*> (x Data..:? "StepFunctionStateMachineParameters")
      )

instance Prelude.Hashable PipeTargetParameters where
  hashWithSalt _salt PipeTargetParameters' {..} =
    _salt
      `Prelude.hashWithSalt` batchJobParameters
      `Prelude.hashWithSalt` cloudWatchLogsParameters
      `Prelude.hashWithSalt` ecsTaskParameters
      `Prelude.hashWithSalt` eventBridgeEventBusParameters
      `Prelude.hashWithSalt` httpParameters
      `Prelude.hashWithSalt` inputTemplate
      `Prelude.hashWithSalt` kinesisStreamParameters
      `Prelude.hashWithSalt` lambdaFunctionParameters
      `Prelude.hashWithSalt` redshiftDataParameters
      `Prelude.hashWithSalt` sageMakerPipelineParameters
      `Prelude.hashWithSalt` sqsQueueParameters
      `Prelude.hashWithSalt` stepFunctionStateMachineParameters

instance Prelude.NFData PipeTargetParameters where
  rnf PipeTargetParameters' {..} =
    Prelude.rnf batchJobParameters
      `Prelude.seq` Prelude.rnf cloudWatchLogsParameters
      `Prelude.seq` Prelude.rnf ecsTaskParameters
      `Prelude.seq` Prelude.rnf eventBridgeEventBusParameters
      `Prelude.seq` Prelude.rnf httpParameters
      `Prelude.seq` Prelude.rnf inputTemplate
      `Prelude.seq` Prelude.rnf kinesisStreamParameters
      `Prelude.seq` Prelude.rnf lambdaFunctionParameters
      `Prelude.seq` Prelude.rnf redshiftDataParameters
      `Prelude.seq` Prelude.rnf sageMakerPipelineParameters
      `Prelude.seq` Prelude.rnf sqsQueueParameters
      `Prelude.seq` Prelude.rnf stepFunctionStateMachineParameters

instance Data.ToJSON PipeTargetParameters where
  toJSON PipeTargetParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BatchJobParameters" Data..=)
              Prelude.<$> batchJobParameters,
            ("CloudWatchLogsParameters" Data..=)
              Prelude.<$> cloudWatchLogsParameters,
            ("EcsTaskParameters" Data..=)
              Prelude.<$> ecsTaskParameters,
            ("EventBridgeEventBusParameters" Data..=)
              Prelude.<$> eventBridgeEventBusParameters,
            ("HttpParameters" Data..=)
              Prelude.<$> httpParameters,
            ("InputTemplate" Data..=) Prelude.<$> inputTemplate,
            ("KinesisStreamParameters" Data..=)
              Prelude.<$> kinesisStreamParameters,
            ("LambdaFunctionParameters" Data..=)
              Prelude.<$> lambdaFunctionParameters,
            ("RedshiftDataParameters" Data..=)
              Prelude.<$> redshiftDataParameters,
            ("SageMakerPipelineParameters" Data..=)
              Prelude.<$> sageMakerPipelineParameters,
            ("SqsQueueParameters" Data..=)
              Prelude.<$> sqsQueueParameters,
            ("StepFunctionStateMachineParameters" Data..=)
              Prelude.<$> stepFunctionStateMachineParameters
          ]
      )
