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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    ecsParameters :: Prelude.Maybe EcsParameters,
    -- | Parameters used when you are using the rule to invoke Amazon EC2 Run
    -- Command.
    runCommandParameters :: Prelude.Maybe RunCommandParameters,
    -- | The Amazon Resource Name (ARN) of the IAM role to be used for this
    -- target when the rule is triggered. If one rule triggers multiple
    -- targets, you can use a different IAM role for each target.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Contains the Redshift Data API parameters to use when the target is a
    -- Redshift cluster.
    --
    -- If you specify a Redshift Cluster as a Target, you can use this to
    -- specify parameters to invoke the Redshift Data API ExecuteStatement
    -- based on EventBridge events.
    redshiftDataParameters :: Prelude.Maybe RedshiftDataParameters,
    -- | If the event target is an AWS Batch job, this contains the job
    -- definition, job name, and other parameters. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/jobs.html Jobs> in
    -- the /AWS Batch User Guide/.
    batchParameters :: Prelude.Maybe BatchParameters,
    -- | Valid JSON text passed to the target. In this case, nothing from the
    -- event itself is passed to the target. For more information, see
    -- <http://www.rfc-editor.org/rfc/rfc7159.txt The JavaScript Object Notation (JSON) Data Interchange Format>.
    input :: Prelude.Maybe Prelude.Text,
    -- | The value of the JSONPath that is used for extracting part of the
    -- matched event when passing it to the target. You must use JSON dot
    -- notation, not bracket notation. For more information about JSON paths,
    -- see <http://goessner.net/articles/JsonPath/ JSONPath>.
    inputPath :: Prelude.Maybe Prelude.Text,
    -- | The @DeadLetterConfig@ that defines the target queue to send dead-letter
    -- queue events to.
    deadLetterConfig :: Prelude.Maybe DeadLetterConfig,
    -- | The @RetryPolicy@ object that contains the retry policy configuration to
    -- use for the dead-letter queue.
    retryPolicy :: Prelude.Maybe RetryPolicy,
    -- | Contains the HTTP parameters to use when the target is a API Gateway
    -- REST endpoint or EventBridge ApiDestination.
    --
    -- If you specify an API Gateway REST API or EventBridge ApiDestination as
    -- a target, you can use this parameter to specify headers, path
    -- parameters, and query string keys\/values as part of your target
    -- invoking request. If you\'re using ApiDestinations, the corresponding
    -- Connection can also have these values configured. In case of any
    -- conflicting keys, values from the Connection take precedence.
    httpParameters :: Prelude.Maybe HttpParameters,
    -- | Contains the message group ID to use when the target is a FIFO queue.
    --
    -- If you specify an SQS FIFO queue as a target, the queue must have
    -- content-based deduplication enabled.
    sqsParameters :: Prelude.Maybe SqsParameters,
    -- | Settings to enable you to provide custom input to a target based on
    -- certain event data. You can extract one or more key-value pairs from the
    -- event and then use that data to send customized input to the target.
    inputTransformer :: Prelude.Maybe InputTransformer,
    -- | The custom parameter you can use to control the shard assignment, when
    -- the target is a Kinesis data stream. If you do not include this
    -- parameter, the default is to use the @eventId@ as the partition key.
    kinesisParameters :: Prelude.Maybe KinesisParameters,
    -- | The ID of the target.
    id :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the target.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  Target
newTarget pId_ pArn_ =
  Target'
    { ecsParameters = Prelude.Nothing,
      runCommandParameters = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      redshiftDataParameters = Prelude.Nothing,
      batchParameters = Prelude.Nothing,
      input = Prelude.Nothing,
      inputPath = Prelude.Nothing,
      deadLetterConfig = Prelude.Nothing,
      retryPolicy = Prelude.Nothing,
      httpParameters = Prelude.Nothing,
      sqsParameters = Prelude.Nothing,
      inputTransformer = Prelude.Nothing,
      kinesisParameters = Prelude.Nothing,
      id = pId_,
      arn = pArn_
    }

-- | Contains the Amazon ECS task definition and task count to be used, if
-- the event target is an Amazon ECS task. For more information about
-- Amazon ECS tasks, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Task Definitions>
-- in the /Amazon EC2 Container Service Developer Guide/.
target_ecsParameters :: Lens.Lens' Target (Prelude.Maybe EcsParameters)
target_ecsParameters = Lens.lens (\Target' {ecsParameters} -> ecsParameters) (\s@Target' {} a -> s {ecsParameters = a} :: Target)

-- | Parameters used when you are using the rule to invoke Amazon EC2 Run
-- Command.
target_runCommandParameters :: Lens.Lens' Target (Prelude.Maybe RunCommandParameters)
target_runCommandParameters = Lens.lens (\Target' {runCommandParameters} -> runCommandParameters) (\s@Target' {} a -> s {runCommandParameters = a} :: Target)

-- | The Amazon Resource Name (ARN) of the IAM role to be used for this
-- target when the rule is triggered. If one rule triggers multiple
-- targets, you can use a different IAM role for each target.
target_roleArn :: Lens.Lens' Target (Prelude.Maybe Prelude.Text)
target_roleArn = Lens.lens (\Target' {roleArn} -> roleArn) (\s@Target' {} a -> s {roleArn = a} :: Target)

-- | Contains the Redshift Data API parameters to use when the target is a
-- Redshift cluster.
--
-- If you specify a Redshift Cluster as a Target, you can use this to
-- specify parameters to invoke the Redshift Data API ExecuteStatement
-- based on EventBridge events.
target_redshiftDataParameters :: Lens.Lens' Target (Prelude.Maybe RedshiftDataParameters)
target_redshiftDataParameters = Lens.lens (\Target' {redshiftDataParameters} -> redshiftDataParameters) (\s@Target' {} a -> s {redshiftDataParameters = a} :: Target)

-- | If the event target is an AWS Batch job, this contains the job
-- definition, job name, and other parameters. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/jobs.html Jobs> in
-- the /AWS Batch User Guide/.
target_batchParameters :: Lens.Lens' Target (Prelude.Maybe BatchParameters)
target_batchParameters = Lens.lens (\Target' {batchParameters} -> batchParameters) (\s@Target' {} a -> s {batchParameters = a} :: Target)

-- | Valid JSON text passed to the target. In this case, nothing from the
-- event itself is passed to the target. For more information, see
-- <http://www.rfc-editor.org/rfc/rfc7159.txt The JavaScript Object Notation (JSON) Data Interchange Format>.
target_input :: Lens.Lens' Target (Prelude.Maybe Prelude.Text)
target_input = Lens.lens (\Target' {input} -> input) (\s@Target' {} a -> s {input = a} :: Target)

-- | The value of the JSONPath that is used for extracting part of the
-- matched event when passing it to the target. You must use JSON dot
-- notation, not bracket notation. For more information about JSON paths,
-- see <http://goessner.net/articles/JsonPath/ JSONPath>.
target_inputPath :: Lens.Lens' Target (Prelude.Maybe Prelude.Text)
target_inputPath = Lens.lens (\Target' {inputPath} -> inputPath) (\s@Target' {} a -> s {inputPath = a} :: Target)

-- | The @DeadLetterConfig@ that defines the target queue to send dead-letter
-- queue events to.
target_deadLetterConfig :: Lens.Lens' Target (Prelude.Maybe DeadLetterConfig)
target_deadLetterConfig = Lens.lens (\Target' {deadLetterConfig} -> deadLetterConfig) (\s@Target' {} a -> s {deadLetterConfig = a} :: Target)

-- | The @RetryPolicy@ object that contains the retry policy configuration to
-- use for the dead-letter queue.
target_retryPolicy :: Lens.Lens' Target (Prelude.Maybe RetryPolicy)
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
target_httpParameters :: Lens.Lens' Target (Prelude.Maybe HttpParameters)
target_httpParameters = Lens.lens (\Target' {httpParameters} -> httpParameters) (\s@Target' {} a -> s {httpParameters = a} :: Target)

-- | Contains the message group ID to use when the target is a FIFO queue.
--
-- If you specify an SQS FIFO queue as a target, the queue must have
-- content-based deduplication enabled.
target_sqsParameters :: Lens.Lens' Target (Prelude.Maybe SqsParameters)
target_sqsParameters = Lens.lens (\Target' {sqsParameters} -> sqsParameters) (\s@Target' {} a -> s {sqsParameters = a} :: Target)

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

-- | The ID of the target.
target_id :: Lens.Lens' Target Prelude.Text
target_id = Lens.lens (\Target' {id} -> id) (\s@Target' {} a -> s {id = a} :: Target)

-- | The Amazon Resource Name (ARN) of the target.
target_arn :: Lens.Lens' Target Prelude.Text
target_arn = Lens.lens (\Target' {arn} -> arn) (\s@Target' {} a -> s {arn = a} :: Target)

instance Prelude.FromJSON Target where
  parseJSON =
    Prelude.withObject
      "Target"
      ( \x ->
          Target'
            Prelude.<$> (x Prelude..:? "EcsParameters")
            Prelude.<*> (x Prelude..:? "RunCommandParameters")
            Prelude.<*> (x Prelude..:? "RoleArn")
            Prelude.<*> (x Prelude..:? "RedshiftDataParameters")
            Prelude.<*> (x Prelude..:? "BatchParameters")
            Prelude.<*> (x Prelude..:? "Input")
            Prelude.<*> (x Prelude..:? "InputPath")
            Prelude.<*> (x Prelude..:? "DeadLetterConfig")
            Prelude.<*> (x Prelude..:? "RetryPolicy")
            Prelude.<*> (x Prelude..:? "HttpParameters")
            Prelude.<*> (x Prelude..:? "SqsParameters")
            Prelude.<*> (x Prelude..:? "InputTransformer")
            Prelude.<*> (x Prelude..:? "KinesisParameters")
            Prelude.<*> (x Prelude..: "Id")
            Prelude.<*> (x Prelude..: "Arn")
      )

instance Prelude.Hashable Target

instance Prelude.NFData Target

instance Prelude.ToJSON Target where
  toJSON Target' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("EcsParameters" Prelude..=)
              Prelude.<$> ecsParameters,
            ("RunCommandParameters" Prelude..=)
              Prelude.<$> runCommandParameters,
            ("RoleArn" Prelude..=) Prelude.<$> roleArn,
            ("RedshiftDataParameters" Prelude..=)
              Prelude.<$> redshiftDataParameters,
            ("BatchParameters" Prelude..=)
              Prelude.<$> batchParameters,
            ("Input" Prelude..=) Prelude.<$> input,
            ("InputPath" Prelude..=) Prelude.<$> inputPath,
            ("DeadLetterConfig" Prelude..=)
              Prelude.<$> deadLetterConfig,
            ("RetryPolicy" Prelude..=) Prelude.<$> retryPolicy,
            ("HttpParameters" Prelude..=)
              Prelude.<$> httpParameters,
            ("SqsParameters" Prelude..=)
              Prelude.<$> sqsParameters,
            ("InputTransformer" Prelude..=)
              Prelude.<$> inputTransformer,
            ("KinesisParameters" Prelude..=)
              Prelude.<$> kinesisParameters,
            Prelude.Just ("Id" Prelude..= id),
            Prelude.Just ("Arn" Prelude..= arn)
          ]
      )
