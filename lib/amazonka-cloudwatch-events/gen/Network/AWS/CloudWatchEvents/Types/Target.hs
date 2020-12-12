{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.Target
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.Target
  ( Target (..),

    -- * Smart constructor
    mkTarget,

    -- * Lenses
    tRunCommandParameters,
    tHTTPParameters,
    tKinesisParameters,
    tInputTransformer,
    tDeadLetterConfig,
    tSqsParameters,
    tInput,
    tBatchParameters,
    tRedshiftDataParameters,
    tEcsParameters,
    tRetryPolicy,
    tInputPath,
    tRoleARN,
    tId,
    tARN,
  )
where

import Network.AWS.CloudWatchEvents.Types.BatchParameters
import Network.AWS.CloudWatchEvents.Types.DeadLetterConfig
import Network.AWS.CloudWatchEvents.Types.EcsParameters
import Network.AWS.CloudWatchEvents.Types.HTTPParameters
import Network.AWS.CloudWatchEvents.Types.InputTransformer
import Network.AWS.CloudWatchEvents.Types.KinesisParameters
import Network.AWS.CloudWatchEvents.Types.RedshiftDataParameters
import Network.AWS.CloudWatchEvents.Types.RetryPolicy
import Network.AWS.CloudWatchEvents.Types.RunCommandParameters
import Network.AWS.CloudWatchEvents.Types.SqsParameters
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Targets are the resources to be invoked when a rule is triggered. For a complete list of services and resources that can be set as a target, see 'PutTargets' .
--
-- If you are setting the event bus of another account as the target, and that account granted permission to your account through an organization instead of directly by the account ID, then you must specify a @RoleArn@ with proper permissions in the @Target@ structure. For more information, see <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-cross-account-event-delivery.html Sending and Receiving Events Between AWS Accounts> in the /Amazon EventBridge User Guide/ .
--
-- /See:/ 'mkTarget' smart constructor.
data Target = Target'
  { runCommandParameters ::
      Lude.Maybe RunCommandParameters,
    hTTPParameters :: Lude.Maybe HTTPParameters,
    kinesisParameters :: Lude.Maybe KinesisParameters,
    inputTransformer :: Lude.Maybe InputTransformer,
    deadLetterConfig :: Lude.Maybe DeadLetterConfig,
    sqsParameters :: Lude.Maybe SqsParameters,
    input :: Lude.Maybe Lude.Text,
    batchParameters :: Lude.Maybe BatchParameters,
    redshiftDataParameters :: Lude.Maybe RedshiftDataParameters,
    ecsParameters :: Lude.Maybe EcsParameters,
    retryPolicy :: Lude.Maybe RetryPolicy,
    inputPath :: Lude.Maybe Lude.Text,
    roleARN :: Lude.Maybe Lude.Text,
    id :: Lude.Text,
    arn :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Target' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the target.
-- * 'batchParameters' - If the event target is an AWS Batch job, this contains the job definition, job name, and other parameters. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/jobs.html Jobs> in the /AWS Batch User Guide/ .
-- * 'deadLetterConfig' - The @DeadLetterConfig@ that defines the target queue to send dead-letter queue events to.
-- * 'ecsParameters' - Contains the Amazon ECS task definition and task count to be used, if the event target is an Amazon ECS task. For more information about Amazon ECS tasks, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Task Definitions > in the /Amazon EC2 Container Service Developer Guide/ .
-- * 'hTTPParameters' - Contains the HTTP parameters to use when the target is a API Gateway REST endpoint.
--
-- If you specify an API Gateway REST API as a target, you can use this parameter to specify headers, path parameter, query string keys/values as part of your target invoking request.
-- * 'id' - The ID of the target.
-- * 'input' - Valid JSON text passed to the target. In this case, nothing from the event itself is passed to the target. For more information, see <http://www.rfc-editor.org/rfc/rfc7159.txt The JavaScript Object Notation (JSON) Data Interchange Format> .
-- * 'inputPath' - The value of the JSONPath that is used for extracting part of the matched event when passing it to the target. You must use JSON dot notation, not bracket notation. For more information about JSON paths, see <http://goessner.net/articles/JsonPath/ JSONPath> .
-- * 'inputTransformer' - Settings to enable you to provide custom input to a target based on certain event data. You can extract one or more key-value pairs from the event and then use that data to send customized input to the target.
-- * 'kinesisParameters' - The custom parameter you can use to control the shard assignment, when the target is a Kinesis data stream. If you do not include this parameter, the default is to use the @eventId@ as the partition key.
-- * 'redshiftDataParameters' - Contains the Redshift Data API parameters to use when the target is a Redshift cluster.
--
-- If you specify a Redshift Cluster as a Target, you can use this to specify parameters to invoke the Redshift Data API ExecuteStatement based on EventBridge events.
-- * 'retryPolicy' - The @RetryPolicy@ object that contains the retry policy configuration to use for the dead-letter queue.
-- * 'roleARN' - The Amazon Resource Name (ARN) of the IAM role to be used for this target when the rule is triggered. If one rule triggers multiple targets, you can use a different IAM role for each target.
-- * 'runCommandParameters' - Parameters used when you are using the rule to invoke Amazon EC2 Run Command.
-- * 'sqsParameters' - Contains the message group ID to use when the target is a FIFO queue.
--
-- If you specify an SQS FIFO queue as a target, the queue must have content-based deduplication enabled.
mkTarget ::
  -- | 'id'
  Lude.Text ->
  -- | 'arn'
  Lude.Text ->
  Target
mkTarget pId_ pARN_ =
  Target'
    { runCommandParameters = Lude.Nothing,
      hTTPParameters = Lude.Nothing,
      kinesisParameters = Lude.Nothing,
      inputTransformer = Lude.Nothing,
      deadLetterConfig = Lude.Nothing,
      sqsParameters = Lude.Nothing,
      input = Lude.Nothing,
      batchParameters = Lude.Nothing,
      redshiftDataParameters = Lude.Nothing,
      ecsParameters = Lude.Nothing,
      retryPolicy = Lude.Nothing,
      inputPath = Lude.Nothing,
      roleARN = Lude.Nothing,
      id = pId_,
      arn = pARN_
    }

-- | Parameters used when you are using the rule to invoke Amazon EC2 Run Command.
--
-- /Note:/ Consider using 'runCommandParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tRunCommandParameters :: Lens.Lens' Target (Lude.Maybe RunCommandParameters)
tRunCommandParameters = Lens.lens (runCommandParameters :: Target -> Lude.Maybe RunCommandParameters) (\s a -> s {runCommandParameters = a} :: Target)
{-# DEPRECATED tRunCommandParameters "Use generic-lens or generic-optics with 'runCommandParameters' instead." #-}

-- | Contains the HTTP parameters to use when the target is a API Gateway REST endpoint.
--
-- If you specify an API Gateway REST API as a target, you can use this parameter to specify headers, path parameter, query string keys/values as part of your target invoking request.
--
-- /Note:/ Consider using 'hTTPParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tHTTPParameters :: Lens.Lens' Target (Lude.Maybe HTTPParameters)
tHTTPParameters = Lens.lens (hTTPParameters :: Target -> Lude.Maybe HTTPParameters) (\s a -> s {hTTPParameters = a} :: Target)
{-# DEPRECATED tHTTPParameters "Use generic-lens or generic-optics with 'hTTPParameters' instead." #-}

-- | The custom parameter you can use to control the shard assignment, when the target is a Kinesis data stream. If you do not include this parameter, the default is to use the @eventId@ as the partition key.
--
-- /Note:/ Consider using 'kinesisParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tKinesisParameters :: Lens.Lens' Target (Lude.Maybe KinesisParameters)
tKinesisParameters = Lens.lens (kinesisParameters :: Target -> Lude.Maybe KinesisParameters) (\s a -> s {kinesisParameters = a} :: Target)
{-# DEPRECATED tKinesisParameters "Use generic-lens or generic-optics with 'kinesisParameters' instead." #-}

-- | Settings to enable you to provide custom input to a target based on certain event data. You can extract one or more key-value pairs from the event and then use that data to send customized input to the target.
--
-- /Note:/ Consider using 'inputTransformer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tInputTransformer :: Lens.Lens' Target (Lude.Maybe InputTransformer)
tInputTransformer = Lens.lens (inputTransformer :: Target -> Lude.Maybe InputTransformer) (\s a -> s {inputTransformer = a} :: Target)
{-# DEPRECATED tInputTransformer "Use generic-lens or generic-optics with 'inputTransformer' instead." #-}

-- | The @DeadLetterConfig@ that defines the target queue to send dead-letter queue events to.
--
-- /Note:/ Consider using 'deadLetterConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tDeadLetterConfig :: Lens.Lens' Target (Lude.Maybe DeadLetterConfig)
tDeadLetterConfig = Lens.lens (deadLetterConfig :: Target -> Lude.Maybe DeadLetterConfig) (\s a -> s {deadLetterConfig = a} :: Target)
{-# DEPRECATED tDeadLetterConfig "Use generic-lens or generic-optics with 'deadLetterConfig' instead." #-}

-- | Contains the message group ID to use when the target is a FIFO queue.
--
-- If you specify an SQS FIFO queue as a target, the queue must have content-based deduplication enabled.
--
-- /Note:/ Consider using 'sqsParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tSqsParameters :: Lens.Lens' Target (Lude.Maybe SqsParameters)
tSqsParameters = Lens.lens (sqsParameters :: Target -> Lude.Maybe SqsParameters) (\s a -> s {sqsParameters = a} :: Target)
{-# DEPRECATED tSqsParameters "Use generic-lens or generic-optics with 'sqsParameters' instead." #-}

-- | Valid JSON text passed to the target. In this case, nothing from the event itself is passed to the target. For more information, see <http://www.rfc-editor.org/rfc/rfc7159.txt The JavaScript Object Notation (JSON) Data Interchange Format> .
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tInput :: Lens.Lens' Target (Lude.Maybe Lude.Text)
tInput = Lens.lens (input :: Target -> Lude.Maybe Lude.Text) (\s a -> s {input = a} :: Target)
{-# DEPRECATED tInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | If the event target is an AWS Batch job, this contains the job definition, job name, and other parameters. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/jobs.html Jobs> in the /AWS Batch User Guide/ .
--
-- /Note:/ Consider using 'batchParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tBatchParameters :: Lens.Lens' Target (Lude.Maybe BatchParameters)
tBatchParameters = Lens.lens (batchParameters :: Target -> Lude.Maybe BatchParameters) (\s a -> s {batchParameters = a} :: Target)
{-# DEPRECATED tBatchParameters "Use generic-lens or generic-optics with 'batchParameters' instead." #-}

-- | Contains the Redshift Data API parameters to use when the target is a Redshift cluster.
--
-- If you specify a Redshift Cluster as a Target, you can use this to specify parameters to invoke the Redshift Data API ExecuteStatement based on EventBridge events.
--
-- /Note:/ Consider using 'redshiftDataParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tRedshiftDataParameters :: Lens.Lens' Target (Lude.Maybe RedshiftDataParameters)
tRedshiftDataParameters = Lens.lens (redshiftDataParameters :: Target -> Lude.Maybe RedshiftDataParameters) (\s a -> s {redshiftDataParameters = a} :: Target)
{-# DEPRECATED tRedshiftDataParameters "Use generic-lens or generic-optics with 'redshiftDataParameters' instead." #-}

-- | Contains the Amazon ECS task definition and task count to be used, if the event target is an Amazon ECS task. For more information about Amazon ECS tasks, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Task Definitions > in the /Amazon EC2 Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'ecsParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tEcsParameters :: Lens.Lens' Target (Lude.Maybe EcsParameters)
tEcsParameters = Lens.lens (ecsParameters :: Target -> Lude.Maybe EcsParameters) (\s a -> s {ecsParameters = a} :: Target)
{-# DEPRECATED tEcsParameters "Use generic-lens or generic-optics with 'ecsParameters' instead." #-}

-- | The @RetryPolicy@ object that contains the retry policy configuration to use for the dead-letter queue.
--
-- /Note:/ Consider using 'retryPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tRetryPolicy :: Lens.Lens' Target (Lude.Maybe RetryPolicy)
tRetryPolicy = Lens.lens (retryPolicy :: Target -> Lude.Maybe RetryPolicy) (\s a -> s {retryPolicy = a} :: Target)
{-# DEPRECATED tRetryPolicy "Use generic-lens or generic-optics with 'retryPolicy' instead." #-}

-- | The value of the JSONPath that is used for extracting part of the matched event when passing it to the target. You must use JSON dot notation, not bracket notation. For more information about JSON paths, see <http://goessner.net/articles/JsonPath/ JSONPath> .
--
-- /Note:/ Consider using 'inputPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tInputPath :: Lens.Lens' Target (Lude.Maybe Lude.Text)
tInputPath = Lens.lens (inputPath :: Target -> Lude.Maybe Lude.Text) (\s a -> s {inputPath = a} :: Target)
{-# DEPRECATED tInputPath "Use generic-lens or generic-optics with 'inputPath' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role to be used for this target when the rule is triggered. If one rule triggers multiple targets, you can use a different IAM role for each target.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tRoleARN :: Lens.Lens' Target (Lude.Maybe Lude.Text)
tRoleARN = Lens.lens (roleARN :: Target -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: Target)
{-# DEPRECATED tRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The ID of the target.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tId :: Lens.Lens' Target Lude.Text
tId = Lens.lens (id :: Target -> Lude.Text) (\s a -> s {id = a} :: Target)
{-# DEPRECATED tId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The Amazon Resource Name (ARN) of the target.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tARN :: Lens.Lens' Target Lude.Text
tARN = Lens.lens (arn :: Target -> Lude.Text) (\s a -> s {arn = a} :: Target)
{-# DEPRECATED tARN "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.FromJSON Target where
  parseJSON =
    Lude.withObject
      "Target"
      ( \x ->
          Target'
            Lude.<$> (x Lude..:? "RunCommandParameters")
            Lude.<*> (x Lude..:? "HttpParameters")
            Lude.<*> (x Lude..:? "KinesisParameters")
            Lude.<*> (x Lude..:? "InputTransformer")
            Lude.<*> (x Lude..:? "DeadLetterConfig")
            Lude.<*> (x Lude..:? "SqsParameters")
            Lude.<*> (x Lude..:? "Input")
            Lude.<*> (x Lude..:? "BatchParameters")
            Lude.<*> (x Lude..:? "RedshiftDataParameters")
            Lude.<*> (x Lude..:? "EcsParameters")
            Lude.<*> (x Lude..:? "RetryPolicy")
            Lude.<*> (x Lude..:? "InputPath")
            Lude.<*> (x Lude..:? "RoleArn")
            Lude.<*> (x Lude..: "Id")
            Lude.<*> (x Lude..: "Arn")
      )

instance Lude.ToJSON Target where
  toJSON Target' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RunCommandParameters" Lude..=) Lude.<$> runCommandParameters,
            ("HttpParameters" Lude..=) Lude.<$> hTTPParameters,
            ("KinesisParameters" Lude..=) Lude.<$> kinesisParameters,
            ("InputTransformer" Lude..=) Lude.<$> inputTransformer,
            ("DeadLetterConfig" Lude..=) Lude.<$> deadLetterConfig,
            ("SqsParameters" Lude..=) Lude.<$> sqsParameters,
            ("Input" Lude..=) Lude.<$> input,
            ("BatchParameters" Lude..=) Lude.<$> batchParameters,
            ("RedshiftDataParameters" Lude..=) Lude.<$> redshiftDataParameters,
            ("EcsParameters" Lude..=) Lude.<$> ecsParameters,
            ("RetryPolicy" Lude..=) Lude.<$> retryPolicy,
            ("InputPath" Lude..=) Lude.<$> inputPath,
            ("RoleArn" Lude..=) Lude.<$> roleARN,
            Lude.Just ("Id" Lude..= id),
            Lude.Just ("Arn" Lude..= arn)
          ]
      )
