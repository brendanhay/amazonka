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
-- Module      : Network.AWS.CloudWatchEvents.Types.Target
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.Target where

import Network.AWS.CloudWatchEvents.Types.BatchParameters
import Network.AWS.CloudWatchEvents.Types.DeadLetterConfig
import Network.AWS.CloudWatchEvents.Types.EcsParameters
import Network.AWS.CloudWatchEvents.Types.HttpParameters
import Network.AWS.CloudWatchEvents.Types.InputTransformer
import Network.AWS.CloudWatchEvents.Types.KinesisParameters
import Network.AWS.CloudWatchEvents.Types.RedshiftDataParameters
import Network.AWS.CloudWatchEvents.Types.RetryPolicy
import Network.AWS.CloudWatchEvents.Types.RunCommandParameters
import Network.AWS.CloudWatchEvents.Types.SqsParameters
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Targets are the resources to be invoked when a rule is triggered. For a
-- complete list of services and resources that can be set as a target, see
-- PutTargets.
--
-- If you are setting the event bus of another account as the target, and
-- that account granted permission to your account through an organization
-- instead of directly by the account ID, then you must specify a @RoleArn@
-- with proper permissions in the @Target@ structure. For more information,
-- see
-- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-cross-account-event-delivery.html Sending and Receiving Events Between AWS Accounts>
-- in the /Amazon EventBridge User Guide/.
--
-- /See:/ 'newTarget' smart constructor.
data Target = Target'
  { -- | Contains the Amazon ECS task definition and task count to be used, if
    -- the event target is an Amazon ECS task. For more information about
    -- Amazon ECS tasks, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Task Definitions>
    -- in the /Amazon EC2 Container Service Developer Guide/.
    ecsParameters :: Core.Maybe EcsParameters,
    -- | Parameters used when you are using the rule to invoke Amazon EC2 Run
    -- Command.
    runCommandParameters :: Core.Maybe RunCommandParameters,
    -- | The Amazon Resource Name (ARN) of the IAM role to be used for this
    -- target when the rule is triggered. If one rule triggers multiple
    -- targets, you can use a different IAM role for each target.
    roleArn :: Core.Maybe Core.Text,
    -- | Contains the Redshift Data API parameters to use when the target is a
    -- Redshift cluster.
    --
    -- If you specify a Redshift Cluster as a Target, you can use this to
    -- specify parameters to invoke the Redshift Data API ExecuteStatement
    -- based on EventBridge events.
    redshiftDataParameters :: Core.Maybe RedshiftDataParameters,
    -- | If the event target is an AWS Batch job, this contains the job
    -- definition, job name, and other parameters. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/jobs.html Jobs> in
    -- the /AWS Batch User Guide/.
    batchParameters :: Core.Maybe BatchParameters,
    -- | Valid JSON text passed to the target. In this case, nothing from the
    -- event itself is passed to the target. For more information, see
    -- <http://www.rfc-editor.org/rfc/rfc7159.txt The JavaScript Object Notation (JSON) Data Interchange Format>.
    input :: Core.Maybe Core.Text,
    -- | The value of the JSONPath that is used for extracting part of the
    -- matched event when passing it to the target. You must use JSON dot
    -- notation, not bracket notation. For more information about JSON paths,
    -- see <http://goessner.net/articles/JsonPath/ JSONPath>.
    inputPath :: Core.Maybe Core.Text,
    -- | The @DeadLetterConfig@ that defines the target queue to send dead-letter
    -- queue events to.
    deadLetterConfig :: Core.Maybe DeadLetterConfig,
    -- | The @RetryPolicy@ object that contains the retry policy configuration to
    -- use for the dead-letter queue.
    retryPolicy :: Core.Maybe RetryPolicy,
    -- | Contains the HTTP parameters to use when the target is a API Gateway
    -- REST endpoint or EventBridge ApiDestination.
    --
    -- If you specify an API Gateway REST API or EventBridge ApiDestination as
    -- a target, you can use this parameter to specify headers, path
    -- parameters, and query string keys\/values as part of your target
    -- invoking request. If you\'re using ApiDestinations, the corresponding
    -- Connection can also have these values configured. In case of any
    -- conflicting keys, values from the Connection take precedence.
    httpParameters :: Core.Maybe HttpParameters,
    -- | Contains the message group ID to use when the target is a FIFO queue.
    --
    -- If you specify an SQS FIFO queue as a target, the queue must have
    -- content-based deduplication enabled.
    sqsParameters :: Core.Maybe SqsParameters,
    -- | Settings to enable you to provide custom input to a target based on
    -- certain event data. You can extract one or more key-value pairs from the
    -- event and then use that data to send customized input to the target.
    inputTransformer :: Core.Maybe InputTransformer,
    -- | The custom parameter you can use to control the shard assignment, when
    -- the target is a Kinesis data stream. If you do not include this
    -- parameter, the default is to use the @eventId@ as the partition key.
    kinesisParameters :: Core.Maybe KinesisParameters,
    -- | The ID of the target.
    id :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the target.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Target' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ecsParameters', 'target_ecsParameters' - Contains the Amazon ECS task definition and task count to be used, if
-- the event target is an Amazon ECS task. For more information about
-- Amazon ECS tasks, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Task Definitions>
-- in the /Amazon EC2 Container Service Developer Guide/.
--
-- 'runCommandParameters', 'target_runCommandParameters' - Parameters used when you are using the rule to invoke Amazon EC2 Run
-- Command.
--
-- 'roleArn', 'target_roleArn' - The Amazon Resource Name (ARN) of the IAM role to be used for this
-- target when the rule is triggered. If one rule triggers multiple
-- targets, you can use a different IAM role for each target.
--
-- 'redshiftDataParameters', 'target_redshiftDataParameters' - Contains the Redshift Data API parameters to use when the target is a
-- Redshift cluster.
--
-- If you specify a Redshift Cluster as a Target, you can use this to
-- specify parameters to invoke the Redshift Data API ExecuteStatement
-- based on EventBridge events.
--
-- 'batchParameters', 'target_batchParameters' - If the event target is an AWS Batch job, this contains the job
-- definition, job name, and other parameters. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/jobs.html Jobs> in
-- the /AWS Batch User Guide/.
--
-- 'input', 'target_input' - Valid JSON text passed to the target. In this case, nothing from the
-- event itself is passed to the target. For more information, see
-- <http://www.rfc-editor.org/rfc/rfc7159.txt The JavaScript Object Notation (JSON) Data Interchange Format>.
--
-- 'inputPath', 'target_inputPath' - The value of the JSONPath that is used for extracting part of the
-- matched event when passing it to the target. You must use JSON dot
-- notation, not bracket notation. For more information about JSON paths,
-- see <http://goessner.net/articles/JsonPath/ JSONPath>.
--
-- 'deadLetterConfig', 'target_deadLetterConfig' - The @DeadLetterConfig@ that defines the target queue to send dead-letter
-- queue events to.
--
-- 'retryPolicy', 'target_retryPolicy' - The @RetryPolicy@ object that contains the retry policy configuration to
-- use for the dead-letter queue.
--
-- 'httpParameters', 'target_httpParameters' - Contains the HTTP parameters to use when the target is a API Gateway
-- REST endpoint or EventBridge ApiDestination.
--
-- If you specify an API Gateway REST API or EventBridge ApiDestination as
-- a target, you can use this parameter to specify headers, path
-- parameters, and query string keys\/values as part of your target
-- invoking request. If you\'re using ApiDestinations, the corresponding
-- Connection can also have these values configured. In case of any
-- conflicting keys, values from the Connection take precedence.
--
-- 'sqsParameters', 'target_sqsParameters' - Contains the message group ID to use when the target is a FIFO queue.
--
-- If you specify an SQS FIFO queue as a target, the queue must have
-- content-based deduplication enabled.
--
-- 'inputTransformer', 'target_inputTransformer' - Settings to enable you to provide custom input to a target based on
-- certain event data. You can extract one or more key-value pairs from the
-- event and then use that data to send customized input to the target.
--
-- 'kinesisParameters', 'target_kinesisParameters' - The custom parameter you can use to control the shard assignment, when
-- the target is a Kinesis data stream. If you do not include this
-- parameter, the default is to use the @eventId@ as the partition key.
--
-- 'id', 'target_id' - The ID of the target.
--
-- 'arn', 'target_arn' - The Amazon Resource Name (ARN) of the target.
newTarget ::
  -- | 'id'
  Core.Text ->
  -- | 'arn'
  Core.Text ->
  Target
newTarget pId_ pArn_ =
  Target'
    { ecsParameters = Core.Nothing,
      runCommandParameters = Core.Nothing,
      roleArn = Core.Nothing,
      redshiftDataParameters = Core.Nothing,
      batchParameters = Core.Nothing,
      input = Core.Nothing,
      inputPath = Core.Nothing,
      deadLetterConfig = Core.Nothing,
      retryPolicy = Core.Nothing,
      httpParameters = Core.Nothing,
      sqsParameters = Core.Nothing,
      inputTransformer = Core.Nothing,
      kinesisParameters = Core.Nothing,
      id = pId_,
      arn = pArn_
    }

-- | Contains the Amazon ECS task definition and task count to be used, if
-- the event target is an Amazon ECS task. For more information about
-- Amazon ECS tasks, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Task Definitions>
-- in the /Amazon EC2 Container Service Developer Guide/.
target_ecsParameters :: Lens.Lens' Target (Core.Maybe EcsParameters)
target_ecsParameters = Lens.lens (\Target' {ecsParameters} -> ecsParameters) (\s@Target' {} a -> s {ecsParameters = a} :: Target)

-- | Parameters used when you are using the rule to invoke Amazon EC2 Run
-- Command.
target_runCommandParameters :: Lens.Lens' Target (Core.Maybe RunCommandParameters)
target_runCommandParameters = Lens.lens (\Target' {runCommandParameters} -> runCommandParameters) (\s@Target' {} a -> s {runCommandParameters = a} :: Target)

-- | The Amazon Resource Name (ARN) of the IAM role to be used for this
-- target when the rule is triggered. If one rule triggers multiple
-- targets, you can use a different IAM role for each target.
target_roleArn :: Lens.Lens' Target (Core.Maybe Core.Text)
target_roleArn = Lens.lens (\Target' {roleArn} -> roleArn) (\s@Target' {} a -> s {roleArn = a} :: Target)

-- | Contains the Redshift Data API parameters to use when the target is a
-- Redshift cluster.
--
-- If you specify a Redshift Cluster as a Target, you can use this to
-- specify parameters to invoke the Redshift Data API ExecuteStatement
-- based on EventBridge events.
target_redshiftDataParameters :: Lens.Lens' Target (Core.Maybe RedshiftDataParameters)
target_redshiftDataParameters = Lens.lens (\Target' {redshiftDataParameters} -> redshiftDataParameters) (\s@Target' {} a -> s {redshiftDataParameters = a} :: Target)

-- | If the event target is an AWS Batch job, this contains the job
-- definition, job name, and other parameters. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/jobs.html Jobs> in
-- the /AWS Batch User Guide/.
target_batchParameters :: Lens.Lens' Target (Core.Maybe BatchParameters)
target_batchParameters = Lens.lens (\Target' {batchParameters} -> batchParameters) (\s@Target' {} a -> s {batchParameters = a} :: Target)

-- | Valid JSON text passed to the target. In this case, nothing from the
-- event itself is passed to the target. For more information, see
-- <http://www.rfc-editor.org/rfc/rfc7159.txt The JavaScript Object Notation (JSON) Data Interchange Format>.
target_input :: Lens.Lens' Target (Core.Maybe Core.Text)
target_input = Lens.lens (\Target' {input} -> input) (\s@Target' {} a -> s {input = a} :: Target)

-- | The value of the JSONPath that is used for extracting part of the
-- matched event when passing it to the target. You must use JSON dot
-- notation, not bracket notation. For more information about JSON paths,
-- see <http://goessner.net/articles/JsonPath/ JSONPath>.
target_inputPath :: Lens.Lens' Target (Core.Maybe Core.Text)
target_inputPath = Lens.lens (\Target' {inputPath} -> inputPath) (\s@Target' {} a -> s {inputPath = a} :: Target)

-- | The @DeadLetterConfig@ that defines the target queue to send dead-letter
-- queue events to.
target_deadLetterConfig :: Lens.Lens' Target (Core.Maybe DeadLetterConfig)
target_deadLetterConfig = Lens.lens (\Target' {deadLetterConfig} -> deadLetterConfig) (\s@Target' {} a -> s {deadLetterConfig = a} :: Target)

-- | The @RetryPolicy@ object that contains the retry policy configuration to
-- use for the dead-letter queue.
target_retryPolicy :: Lens.Lens' Target (Core.Maybe RetryPolicy)
target_retryPolicy = Lens.lens (\Target' {retryPolicy} -> retryPolicy) (\s@Target' {} a -> s {retryPolicy = a} :: Target)

-- | Contains the HTTP parameters to use when the target is a API Gateway
-- REST endpoint or EventBridge ApiDestination.
--
-- If you specify an API Gateway REST API or EventBridge ApiDestination as
-- a target, you can use this parameter to specify headers, path
-- parameters, and query string keys\/values as part of your target
-- invoking request. If you\'re using ApiDestinations, the corresponding
-- Connection can also have these values configured. In case of any
-- conflicting keys, values from the Connection take precedence.
target_httpParameters :: Lens.Lens' Target (Core.Maybe HttpParameters)
target_httpParameters = Lens.lens (\Target' {httpParameters} -> httpParameters) (\s@Target' {} a -> s {httpParameters = a} :: Target)

-- | Contains the message group ID to use when the target is a FIFO queue.
--
-- If you specify an SQS FIFO queue as a target, the queue must have
-- content-based deduplication enabled.
target_sqsParameters :: Lens.Lens' Target (Core.Maybe SqsParameters)
target_sqsParameters = Lens.lens (\Target' {sqsParameters} -> sqsParameters) (\s@Target' {} a -> s {sqsParameters = a} :: Target)

-- | Settings to enable you to provide custom input to a target based on
-- certain event data. You can extract one or more key-value pairs from the
-- event and then use that data to send customized input to the target.
target_inputTransformer :: Lens.Lens' Target (Core.Maybe InputTransformer)
target_inputTransformer = Lens.lens (\Target' {inputTransformer} -> inputTransformer) (\s@Target' {} a -> s {inputTransformer = a} :: Target)

-- | The custom parameter you can use to control the shard assignment, when
-- the target is a Kinesis data stream. If you do not include this
-- parameter, the default is to use the @eventId@ as the partition key.
target_kinesisParameters :: Lens.Lens' Target (Core.Maybe KinesisParameters)
target_kinesisParameters = Lens.lens (\Target' {kinesisParameters} -> kinesisParameters) (\s@Target' {} a -> s {kinesisParameters = a} :: Target)

-- | The ID of the target.
target_id :: Lens.Lens' Target Core.Text
target_id = Lens.lens (\Target' {id} -> id) (\s@Target' {} a -> s {id = a} :: Target)

-- | The Amazon Resource Name (ARN) of the target.
target_arn :: Lens.Lens' Target Core.Text
target_arn = Lens.lens (\Target' {arn} -> arn) (\s@Target' {} a -> s {arn = a} :: Target)

instance Core.FromJSON Target where
  parseJSON =
    Core.withObject
      "Target"
      ( \x ->
          Target'
            Core.<$> (x Core..:? "EcsParameters")
            Core.<*> (x Core..:? "RunCommandParameters")
            Core.<*> (x Core..:? "RoleArn")
            Core.<*> (x Core..:? "RedshiftDataParameters")
            Core.<*> (x Core..:? "BatchParameters")
            Core.<*> (x Core..:? "Input")
            Core.<*> (x Core..:? "InputPath")
            Core.<*> (x Core..:? "DeadLetterConfig")
            Core.<*> (x Core..:? "RetryPolicy")
            Core.<*> (x Core..:? "HttpParameters")
            Core.<*> (x Core..:? "SqsParameters")
            Core.<*> (x Core..:? "InputTransformer")
            Core.<*> (x Core..:? "KinesisParameters")
            Core.<*> (x Core..: "Id")
            Core.<*> (x Core..: "Arn")
      )

instance Core.Hashable Target

instance Core.NFData Target

instance Core.ToJSON Target where
  toJSON Target' {..} =
    Core.object
      ( Core.catMaybes
          [ ("EcsParameters" Core..=) Core.<$> ecsParameters,
            ("RunCommandParameters" Core..=)
              Core.<$> runCommandParameters,
            ("RoleArn" Core..=) Core.<$> roleArn,
            ("RedshiftDataParameters" Core..=)
              Core.<$> redshiftDataParameters,
            ("BatchParameters" Core..=) Core.<$> batchParameters,
            ("Input" Core..=) Core.<$> input,
            ("InputPath" Core..=) Core.<$> inputPath,
            ("DeadLetterConfig" Core..=)
              Core.<$> deadLetterConfig,
            ("RetryPolicy" Core..=) Core.<$> retryPolicy,
            ("HttpParameters" Core..=) Core.<$> httpParameters,
            ("SqsParameters" Core..=) Core.<$> sqsParameters,
            ("InputTransformer" Core..=)
              Core.<$> inputTransformer,
            ("KinesisParameters" Core..=)
              Core.<$> kinesisParameters,
            Core.Just ("Id" Core..= id),
            Core.Just ("Arn" Core..= arn)
          ]
      )
