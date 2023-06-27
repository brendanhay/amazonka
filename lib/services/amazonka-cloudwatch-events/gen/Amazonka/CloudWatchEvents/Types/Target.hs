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
-- Module      : Amazonka.CloudWatchEvents.Types.Target
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.Target where

import Amazonka.CloudWatchEvents.Types.BatchParameters
import Amazonka.CloudWatchEvents.Types.DeadLetterConfig
import Amazonka.CloudWatchEvents.Types.EcsParameters
import Amazonka.CloudWatchEvents.Types.HttpParameters
import Amazonka.CloudWatchEvents.Types.InputTransformer
import Amazonka.CloudWatchEvents.Types.KinesisParameters
import Amazonka.CloudWatchEvents.Types.RedshiftDataParameters
import Amazonka.CloudWatchEvents.Types.RetryPolicy
import Amazonka.CloudWatchEvents.Types.RunCommandParameters
import Amazonka.CloudWatchEvents.Types.SageMakerPipelineParameters
import Amazonka.CloudWatchEvents.Types.SqsParameters
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Targets are the resources to be invoked when a rule is triggered. For a
-- complete list of services and resources that can be set as a target, see
-- <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_PutTargets.html PutTargets>.
--
-- If you are setting the event bus of another account as the target, and
-- that account granted permission to your account through an organization
-- instead of directly by the account ID, then you must specify a @RoleArn@
-- with proper permissions in the @Target@ structure. For more information,
-- see
-- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-cross-account-event-delivery.html Sending and Receiving Events Between Amazon Web Services Accounts>
-- in the /Amazon EventBridge User Guide/.
--
-- /See:/ 'newTarget' smart constructor.
data Target = Target'
  { -- | If the event target is an Batch job, this contains the job definition,
    -- job name, and other parameters. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/jobs.html Jobs> in
    -- the /Batch User Guide/.
    batchParameters :: Prelude.Maybe BatchParameters,
    -- | The @DeadLetterConfig@ that defines the target queue to send dead-letter
    -- queue events to.
    deadLetterConfig :: Prelude.Maybe DeadLetterConfig,
    -- | Contains the Amazon ECS task definition and task count to be used, if
    -- the event target is an Amazon ECS task. For more information about
    -- Amazon ECS tasks, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Task Definitions>
    -- in the /Amazon EC2 Container Service Developer Guide/.
    ecsParameters :: Prelude.Maybe EcsParameters,
    -- | Contains the HTTP parameters to use when the target is a API Gateway
    -- endpoint or EventBridge ApiDestination.
    --
    -- If you specify an API Gateway API or EventBridge ApiDestination as a
    -- target, you can use this parameter to specify headers, path parameters,
    -- and query string keys\/values as part of your target invoking request.
    -- If you\'re using ApiDestinations, the corresponding Connection can also
    -- have these values configured. In case of any conflicting keys, values
    -- from the Connection take precedence.
    httpParameters :: Prelude.Maybe HttpParameters,
    -- | Valid JSON text passed to the target. In this case, nothing from the
    -- event itself is passed to the target. For more information, see
    -- <http://www.rfc-editor.org/rfc/rfc7159.txt The JavaScript Object Notation (JSON) Data Interchange Format>.
    input :: Prelude.Maybe Prelude.Text,
    -- | The value of the JSONPath that is used for extracting part of the
    -- matched event when passing it to the target. You may use JSON dot
    -- notation or bracket notation. For more information about JSON paths, see
    -- <http://goessner.net/articles/JsonPath/ JSONPath>.
    inputPath :: Prelude.Maybe Prelude.Text,
    -- | Settings to enable you to provide custom input to a target based on
    -- certain event data. You can extract one or more key-value pairs from the
    -- event and then use that data to send customized input to the target.
    inputTransformer :: Prelude.Maybe InputTransformer,
    -- | The custom parameter you can use to control the shard assignment, when
    -- the target is a Kinesis data stream. If you do not include this
    -- parameter, the default is to use the @eventId@ as the partition key.
    kinesisParameters :: Prelude.Maybe KinesisParameters,
    -- | Contains the Amazon Redshift Data API parameters to use when the target
    -- is a Amazon Redshift cluster.
    --
    -- If you specify a Amazon Redshift Cluster as a Target, you can use this
    -- to specify parameters to invoke the Amazon Redshift Data API
    -- ExecuteStatement based on EventBridge events.
    redshiftDataParameters :: Prelude.Maybe RedshiftDataParameters,
    -- | The @RetryPolicy@ object that contains the retry policy configuration to
    -- use for the dead-letter queue.
    retryPolicy :: Prelude.Maybe RetryPolicy,
    -- | The Amazon Resource Name (ARN) of the IAM role to be used for this
    -- target when the rule is triggered. If one rule triggers multiple
    -- targets, you can use a different IAM role for each target.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Parameters used when you are using the rule to invoke Amazon EC2 Run
    -- Command.
    runCommandParameters :: Prelude.Maybe RunCommandParameters,
    -- | Contains the SageMaker Model Building Pipeline parameters to start
    -- execution of a SageMaker Model Building Pipeline.
    --
    -- If you specify a SageMaker Model Building Pipeline as a target, you can
    -- use this to specify parameters to start a pipeline execution based on
    -- EventBridge events.
    sageMakerPipelineParameters :: Prelude.Maybe SageMakerPipelineParameters,
    -- | Contains the message group ID to use when the target is a FIFO queue.
    --
    -- If you specify an SQS FIFO queue as a target, the queue must have
    -- content-based deduplication enabled.
    sqsParameters :: Prelude.Maybe SqsParameters,
    -- | The ID of the target within the specified rule. Use this ID to reference
    -- the target when updating the rule. We recommend using a memorable and
    -- unique string.
    id :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the target.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Target' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchParameters', 'target_batchParameters' - If the event target is an Batch job, this contains the job definition,
-- job name, and other parameters. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/jobs.html Jobs> in
-- the /Batch User Guide/.
--
-- 'deadLetterConfig', 'target_deadLetterConfig' - The @DeadLetterConfig@ that defines the target queue to send dead-letter
-- queue events to.
--
-- 'ecsParameters', 'target_ecsParameters' - Contains the Amazon ECS task definition and task count to be used, if
-- the event target is an Amazon ECS task. For more information about
-- Amazon ECS tasks, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Task Definitions>
-- in the /Amazon EC2 Container Service Developer Guide/.
--
-- 'httpParameters', 'target_httpParameters' - Contains the HTTP parameters to use when the target is a API Gateway
-- endpoint or EventBridge ApiDestination.
--
-- If you specify an API Gateway API or EventBridge ApiDestination as a
-- target, you can use this parameter to specify headers, path parameters,
-- and query string keys\/values as part of your target invoking request.
-- If you\'re using ApiDestinations, the corresponding Connection can also
-- have these values configured. In case of any conflicting keys, values
-- from the Connection take precedence.
--
-- 'input', 'target_input' - Valid JSON text passed to the target. In this case, nothing from the
-- event itself is passed to the target. For more information, see
-- <http://www.rfc-editor.org/rfc/rfc7159.txt The JavaScript Object Notation (JSON) Data Interchange Format>.
--
-- 'inputPath', 'target_inputPath' - The value of the JSONPath that is used for extracting part of the
-- matched event when passing it to the target. You may use JSON dot
-- notation or bracket notation. For more information about JSON paths, see
-- <http://goessner.net/articles/JsonPath/ JSONPath>.
--
-- 'inputTransformer', 'target_inputTransformer' - Settings to enable you to provide custom input to a target based on
-- certain event data. You can extract one or more key-value pairs from the
-- event and then use that data to send customized input to the target.
--
-- 'kinesisParameters', 'target_kinesisParameters' - The custom parameter you can use to control the shard assignment, when
-- the target is a Kinesis data stream. If you do not include this
-- parameter, the default is to use the @eventId@ as the partition key.
--
-- 'redshiftDataParameters', 'target_redshiftDataParameters' - Contains the Amazon Redshift Data API parameters to use when the target
-- is a Amazon Redshift cluster.
--
-- If you specify a Amazon Redshift Cluster as a Target, you can use this
-- to specify parameters to invoke the Amazon Redshift Data API
-- ExecuteStatement based on EventBridge events.
--
-- 'retryPolicy', 'target_retryPolicy' - The @RetryPolicy@ object that contains the retry policy configuration to
-- use for the dead-letter queue.
--
-- 'roleArn', 'target_roleArn' - The Amazon Resource Name (ARN) of the IAM role to be used for this
-- target when the rule is triggered. If one rule triggers multiple
-- targets, you can use a different IAM role for each target.
--
-- 'runCommandParameters', 'target_runCommandParameters' - Parameters used when you are using the rule to invoke Amazon EC2 Run
-- Command.
--
-- 'sageMakerPipelineParameters', 'target_sageMakerPipelineParameters' - Contains the SageMaker Model Building Pipeline parameters to start
-- execution of a SageMaker Model Building Pipeline.
--
-- If you specify a SageMaker Model Building Pipeline as a target, you can
-- use this to specify parameters to start a pipeline execution based on
-- EventBridge events.
--
-- 'sqsParameters', 'target_sqsParameters' - Contains the message group ID to use when the target is a FIFO queue.
--
-- If you specify an SQS FIFO queue as a target, the queue must have
-- content-based deduplication enabled.
--
-- 'id', 'target_id' - The ID of the target within the specified rule. Use this ID to reference
-- the target when updating the rule. We recommend using a memorable and
-- unique string.
--
-- 'arn', 'target_arn' - The Amazon Resource Name (ARN) of the target.
newTarget ::
  -- | 'id'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  Target
newTarget pId_ pArn_ =
  Target'
    { batchParameters = Prelude.Nothing,
      deadLetterConfig = Prelude.Nothing,
      ecsParameters = Prelude.Nothing,
      httpParameters = Prelude.Nothing,
      input = Prelude.Nothing,
      inputPath = Prelude.Nothing,
      inputTransformer = Prelude.Nothing,
      kinesisParameters = Prelude.Nothing,
      redshiftDataParameters = Prelude.Nothing,
      retryPolicy = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      runCommandParameters = Prelude.Nothing,
      sageMakerPipelineParameters = Prelude.Nothing,
      sqsParameters = Prelude.Nothing,
      id = pId_,
      arn = pArn_
    }

-- | If the event target is an Batch job, this contains the job definition,
-- job name, and other parameters. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/jobs.html Jobs> in
-- the /Batch User Guide/.
target_batchParameters :: Lens.Lens' Target (Prelude.Maybe BatchParameters)
target_batchParameters = Lens.lens (\Target' {batchParameters} -> batchParameters) (\s@Target' {} a -> s {batchParameters = a} :: Target)

-- | The @DeadLetterConfig@ that defines the target queue to send dead-letter
-- queue events to.
target_deadLetterConfig :: Lens.Lens' Target (Prelude.Maybe DeadLetterConfig)
target_deadLetterConfig = Lens.lens (\Target' {deadLetterConfig} -> deadLetterConfig) (\s@Target' {} a -> s {deadLetterConfig = a} :: Target)

-- | Contains the Amazon ECS task definition and task count to be used, if
-- the event target is an Amazon ECS task. For more information about
-- Amazon ECS tasks, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Task Definitions>
-- in the /Amazon EC2 Container Service Developer Guide/.
target_ecsParameters :: Lens.Lens' Target (Prelude.Maybe EcsParameters)
target_ecsParameters = Lens.lens (\Target' {ecsParameters} -> ecsParameters) (\s@Target' {} a -> s {ecsParameters = a} :: Target)

-- | Contains the HTTP parameters to use when the target is a API Gateway
-- endpoint or EventBridge ApiDestination.
--
-- If you specify an API Gateway API or EventBridge ApiDestination as a
-- target, you can use this parameter to specify headers, path parameters,
-- and query string keys\/values as part of your target invoking request.
-- If you\'re using ApiDestinations, the corresponding Connection can also
-- have these values configured. In case of any conflicting keys, values
-- from the Connection take precedence.
target_httpParameters :: Lens.Lens' Target (Prelude.Maybe HttpParameters)
target_httpParameters = Lens.lens (\Target' {httpParameters} -> httpParameters) (\s@Target' {} a -> s {httpParameters = a} :: Target)

-- | Valid JSON text passed to the target. In this case, nothing from the
-- event itself is passed to the target. For more information, see
-- <http://www.rfc-editor.org/rfc/rfc7159.txt The JavaScript Object Notation (JSON) Data Interchange Format>.
target_input :: Lens.Lens' Target (Prelude.Maybe Prelude.Text)
target_input = Lens.lens (\Target' {input} -> input) (\s@Target' {} a -> s {input = a} :: Target)

-- | The value of the JSONPath that is used for extracting part of the
-- matched event when passing it to the target. You may use JSON dot
-- notation or bracket notation. For more information about JSON paths, see
-- <http://goessner.net/articles/JsonPath/ JSONPath>.
target_inputPath :: Lens.Lens' Target (Prelude.Maybe Prelude.Text)
target_inputPath = Lens.lens (\Target' {inputPath} -> inputPath) (\s@Target' {} a -> s {inputPath = a} :: Target)

-- | Settings to enable you to provide custom input to a target based on
-- certain event data. You can extract one or more key-value pairs from the
-- event and then use that data to send customized input to the target.
target_inputTransformer :: Lens.Lens' Target (Prelude.Maybe InputTransformer)
target_inputTransformer = Lens.lens (\Target' {inputTransformer} -> inputTransformer) (\s@Target' {} a -> s {inputTransformer = a} :: Target)

-- | The custom parameter you can use to control the shard assignment, when
-- the target is a Kinesis data stream. If you do not include this
-- parameter, the default is to use the @eventId@ as the partition key.
target_kinesisParameters :: Lens.Lens' Target (Prelude.Maybe KinesisParameters)
target_kinesisParameters = Lens.lens (\Target' {kinesisParameters} -> kinesisParameters) (\s@Target' {} a -> s {kinesisParameters = a} :: Target)

-- | Contains the Amazon Redshift Data API parameters to use when the target
-- is a Amazon Redshift cluster.
--
-- If you specify a Amazon Redshift Cluster as a Target, you can use this
-- to specify parameters to invoke the Amazon Redshift Data API
-- ExecuteStatement based on EventBridge events.
target_redshiftDataParameters :: Lens.Lens' Target (Prelude.Maybe RedshiftDataParameters)
target_redshiftDataParameters = Lens.lens (\Target' {redshiftDataParameters} -> redshiftDataParameters) (\s@Target' {} a -> s {redshiftDataParameters = a} :: Target)

-- | The @RetryPolicy@ object that contains the retry policy configuration to
-- use for the dead-letter queue.
target_retryPolicy :: Lens.Lens' Target (Prelude.Maybe RetryPolicy)
target_retryPolicy = Lens.lens (\Target' {retryPolicy} -> retryPolicy) (\s@Target' {} a -> s {retryPolicy = a} :: Target)

-- | The Amazon Resource Name (ARN) of the IAM role to be used for this
-- target when the rule is triggered. If one rule triggers multiple
-- targets, you can use a different IAM role for each target.
target_roleArn :: Lens.Lens' Target (Prelude.Maybe Prelude.Text)
target_roleArn = Lens.lens (\Target' {roleArn} -> roleArn) (\s@Target' {} a -> s {roleArn = a} :: Target)

-- | Parameters used when you are using the rule to invoke Amazon EC2 Run
-- Command.
target_runCommandParameters :: Lens.Lens' Target (Prelude.Maybe RunCommandParameters)
target_runCommandParameters = Lens.lens (\Target' {runCommandParameters} -> runCommandParameters) (\s@Target' {} a -> s {runCommandParameters = a} :: Target)

-- | Contains the SageMaker Model Building Pipeline parameters to start
-- execution of a SageMaker Model Building Pipeline.
--
-- If you specify a SageMaker Model Building Pipeline as a target, you can
-- use this to specify parameters to start a pipeline execution based on
-- EventBridge events.
target_sageMakerPipelineParameters :: Lens.Lens' Target (Prelude.Maybe SageMakerPipelineParameters)
target_sageMakerPipelineParameters = Lens.lens (\Target' {sageMakerPipelineParameters} -> sageMakerPipelineParameters) (\s@Target' {} a -> s {sageMakerPipelineParameters = a} :: Target)

-- | Contains the message group ID to use when the target is a FIFO queue.
--
-- If you specify an SQS FIFO queue as a target, the queue must have
-- content-based deduplication enabled.
target_sqsParameters :: Lens.Lens' Target (Prelude.Maybe SqsParameters)
target_sqsParameters = Lens.lens (\Target' {sqsParameters} -> sqsParameters) (\s@Target' {} a -> s {sqsParameters = a} :: Target)

-- | The ID of the target within the specified rule. Use this ID to reference
-- the target when updating the rule. We recommend using a memorable and
-- unique string.
target_id :: Lens.Lens' Target Prelude.Text
target_id = Lens.lens (\Target' {id} -> id) (\s@Target' {} a -> s {id = a} :: Target)

-- | The Amazon Resource Name (ARN) of the target.
target_arn :: Lens.Lens' Target Prelude.Text
target_arn = Lens.lens (\Target' {arn} -> arn) (\s@Target' {} a -> s {arn = a} :: Target)

instance Data.FromJSON Target where
  parseJSON =
    Data.withObject
      "Target"
      ( \x ->
          Target'
            Prelude.<$> (x Data..:? "BatchParameters")
            Prelude.<*> (x Data..:? "DeadLetterConfig")
            Prelude.<*> (x Data..:? "EcsParameters")
            Prelude.<*> (x Data..:? "HttpParameters")
            Prelude.<*> (x Data..:? "Input")
            Prelude.<*> (x Data..:? "InputPath")
            Prelude.<*> (x Data..:? "InputTransformer")
            Prelude.<*> (x Data..:? "KinesisParameters")
            Prelude.<*> (x Data..:? "RedshiftDataParameters")
            Prelude.<*> (x Data..:? "RetryPolicy")
            Prelude.<*> (x Data..:? "RoleArn")
            Prelude.<*> (x Data..:? "RunCommandParameters")
            Prelude.<*> (x Data..:? "SageMakerPipelineParameters")
            Prelude.<*> (x Data..:? "SqsParameters")
            Prelude.<*> (x Data..: "Id")
            Prelude.<*> (x Data..: "Arn")
      )

instance Prelude.Hashable Target where
  hashWithSalt _salt Target' {..} =
    _salt
      `Prelude.hashWithSalt` batchParameters
      `Prelude.hashWithSalt` deadLetterConfig
      `Prelude.hashWithSalt` ecsParameters
      `Prelude.hashWithSalt` httpParameters
      `Prelude.hashWithSalt` input
      `Prelude.hashWithSalt` inputPath
      `Prelude.hashWithSalt` inputTransformer
      `Prelude.hashWithSalt` kinesisParameters
      `Prelude.hashWithSalt` redshiftDataParameters
      `Prelude.hashWithSalt` retryPolicy
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` runCommandParameters
      `Prelude.hashWithSalt` sageMakerPipelineParameters
      `Prelude.hashWithSalt` sqsParameters
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` arn

instance Prelude.NFData Target where
  rnf Target' {..} =
    Prelude.rnf batchParameters
      `Prelude.seq` Prelude.rnf deadLetterConfig
      `Prelude.seq` Prelude.rnf ecsParameters
      `Prelude.seq` Prelude.rnf httpParameters
      `Prelude.seq` Prelude.rnf input
      `Prelude.seq` Prelude.rnf inputPath
      `Prelude.seq` Prelude.rnf inputTransformer
      `Prelude.seq` Prelude.rnf kinesisParameters
      `Prelude.seq` Prelude.rnf redshiftDataParameters
      `Prelude.seq` Prelude.rnf retryPolicy
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf runCommandParameters
      `Prelude.seq` Prelude.rnf sageMakerPipelineParameters
      `Prelude.seq` Prelude.rnf sqsParameters
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf arn

instance Data.ToJSON Target where
  toJSON Target' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BatchParameters" Data..=)
              Prelude.<$> batchParameters,
            ("DeadLetterConfig" Data..=)
              Prelude.<$> deadLetterConfig,
            ("EcsParameters" Data..=) Prelude.<$> ecsParameters,
            ("HttpParameters" Data..=)
              Prelude.<$> httpParameters,
            ("Input" Data..=) Prelude.<$> input,
            ("InputPath" Data..=) Prelude.<$> inputPath,
            ("InputTransformer" Data..=)
              Prelude.<$> inputTransformer,
            ("KinesisParameters" Data..=)
              Prelude.<$> kinesisParameters,
            ("RedshiftDataParameters" Data..=)
              Prelude.<$> redshiftDataParameters,
            ("RetryPolicy" Data..=) Prelude.<$> retryPolicy,
            ("RoleArn" Data..=) Prelude.<$> roleArn,
            ("RunCommandParameters" Data..=)
              Prelude.<$> runCommandParameters,
            ("SageMakerPipelineParameters" Data..=)
              Prelude.<$> sageMakerPipelineParameters,
            ("SqsParameters" Data..=) Prelude.<$> sqsParameters,
            Prelude.Just ("Id" Data..= id),
            Prelude.Just ("Arn" Data..= arn)
          ]
      )
