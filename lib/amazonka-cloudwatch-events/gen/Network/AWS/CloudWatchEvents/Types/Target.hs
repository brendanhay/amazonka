{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.Target
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchEvents.Types.Target
  ( Target (..)
  -- * Smart constructor
  , mkTarget
  -- * Lenses
  , tId
  , tArn
  , tBatchParameters
  , tDeadLetterConfig
  , tEcsParameters
  , tHttpParameters
  , tInput
  , tInputPath
  , tInputTransformer
  , tKinesisParameters
  , tRedshiftDataParameters
  , tRetryPolicy
  , tRoleArn
  , tRunCommandParameters
  , tSqsParameters
  ) where

import qualified Network.AWS.CloudWatchEvents.Types.BatchParameters as Types
import qualified Network.AWS.CloudWatchEvents.Types.DeadLetterConfig as Types
import qualified Network.AWS.CloudWatchEvents.Types.EcsParameters as Types
import qualified Network.AWS.CloudWatchEvents.Types.HttpParameters as Types
import qualified Network.AWS.CloudWatchEvents.Types.InputTransformer as Types
import qualified Network.AWS.CloudWatchEvents.Types.KinesisParameters as Types
import qualified Network.AWS.CloudWatchEvents.Types.RedshiftDataParameters as Types
import qualified Network.AWS.CloudWatchEvents.Types.RetryPolicy as Types
import qualified Network.AWS.CloudWatchEvents.Types.RoleArn as Types
import qualified Network.AWS.CloudWatchEvents.Types.RunCommandParameters as Types
import qualified Network.AWS.CloudWatchEvents.Types.SqsParameters as Types
import qualified Network.AWS.CloudWatchEvents.Types.TargetArn as Types
import qualified Network.AWS.CloudWatchEvents.Types.TargetId as Types
import qualified Network.AWS.CloudWatchEvents.Types.TargetInput as Types
import qualified Network.AWS.CloudWatchEvents.Types.TargetInputPath as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Targets are the resources to be invoked when a rule is triggered. For a complete list of services and resources that can be set as a target, see 'PutTargets' .
--
-- If you are setting the event bus of another account as the target, and that account granted permission to your account through an organization instead of directly by the account ID, then you must specify a @RoleArn@ with proper permissions in the @Target@ structure. For more information, see <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-cross-account-event-delivery.html Sending and Receiving Events Between AWS Accounts> in the /Amazon EventBridge User Guide/ .
--
-- /See:/ 'mkTarget' smart constructor.
data Target = Target'
  { id :: Types.TargetId
    -- ^ The ID of the target.
  , arn :: Types.TargetArn
    -- ^ The Amazon Resource Name (ARN) of the target.
  , batchParameters :: Core.Maybe Types.BatchParameters
    -- ^ If the event target is an AWS Batch job, this contains the job definition, job name, and other parameters. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/jobs.html Jobs> in the /AWS Batch User Guide/ .
  , deadLetterConfig :: Core.Maybe Types.DeadLetterConfig
    -- ^ The @DeadLetterConfig@ that defines the target queue to send dead-letter queue events to.
  , ecsParameters :: Core.Maybe Types.EcsParameters
    -- ^ Contains the Amazon ECS task definition and task count to be used, if the event target is an Amazon ECS task. For more information about Amazon ECS tasks, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Task Definitions > in the /Amazon EC2 Container Service Developer Guide/ .
  , httpParameters :: Core.Maybe Types.HttpParameters
    -- ^ Contains the HTTP parameters to use when the target is a API Gateway REST endpoint.
--
-- If you specify an API Gateway REST API as a target, you can use this parameter to specify headers, path parameter, query string keys/values as part of your target invoking request.
  , input :: Core.Maybe Types.TargetInput
    -- ^ Valid JSON text passed to the target. In this case, nothing from the event itself is passed to the target. For more information, see <http://www.rfc-editor.org/rfc/rfc7159.txt The JavaScript Object Notation (JSON) Data Interchange Format> .
  , inputPath :: Core.Maybe Types.TargetInputPath
    -- ^ The value of the JSONPath that is used for extracting part of the matched event when passing it to the target. You must use JSON dot notation, not bracket notation. For more information about JSON paths, see <http://goessner.net/articles/JsonPath/ JSONPath> .
  , inputTransformer :: Core.Maybe Types.InputTransformer
    -- ^ Settings to enable you to provide custom input to a target based on certain event data. You can extract one or more key-value pairs from the event and then use that data to send customized input to the target.
  , kinesisParameters :: Core.Maybe Types.KinesisParameters
    -- ^ The custom parameter you can use to control the shard assignment, when the target is a Kinesis data stream. If you do not include this parameter, the default is to use the @eventId@ as the partition key.
  , redshiftDataParameters :: Core.Maybe Types.RedshiftDataParameters
    -- ^ Contains the Redshift Data API parameters to use when the target is a Redshift cluster.
--
-- If you specify a Redshift Cluster as a Target, you can use this to specify parameters to invoke the Redshift Data API ExecuteStatement based on EventBridge events.
  , retryPolicy :: Core.Maybe Types.RetryPolicy
    -- ^ The @RetryPolicy@ object that contains the retry policy configuration to use for the dead-letter queue.
  , roleArn :: Core.Maybe Types.RoleArn
    -- ^ The Amazon Resource Name (ARN) of the IAM role to be used for this target when the rule is triggered. If one rule triggers multiple targets, you can use a different IAM role for each target.
  , runCommandParameters :: Core.Maybe Types.RunCommandParameters
    -- ^ Parameters used when you are using the rule to invoke Amazon EC2 Run Command.
  , sqsParameters :: Core.Maybe Types.SqsParameters
    -- ^ Contains the message group ID to use when the target is a FIFO queue.
--
-- If you specify an SQS FIFO queue as a target, the queue must have content-based deduplication enabled.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Target' value with any optional fields omitted.
mkTarget
    :: Types.TargetId -- ^ 'id'
    -> Types.TargetArn -- ^ 'arn'
    -> Target
mkTarget id arn
  = Target'{id, arn, batchParameters = Core.Nothing,
            deadLetterConfig = Core.Nothing, ecsParameters = Core.Nothing,
            httpParameters = Core.Nothing, input = Core.Nothing,
            inputPath = Core.Nothing, inputTransformer = Core.Nothing,
            kinesisParameters = Core.Nothing,
            redshiftDataParameters = Core.Nothing, retryPolicy = Core.Nothing,
            roleArn = Core.Nothing, runCommandParameters = Core.Nothing,
            sqsParameters = Core.Nothing}

-- | The ID of the target.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tId :: Lens.Lens' Target Types.TargetId
tId = Lens.field @"id"
{-# INLINEABLE tId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The Amazon Resource Name (ARN) of the target.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tArn :: Lens.Lens' Target Types.TargetArn
tArn = Lens.field @"arn"
{-# INLINEABLE tArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | If the event target is an AWS Batch job, this contains the job definition, job name, and other parameters. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/jobs.html Jobs> in the /AWS Batch User Guide/ .
--
-- /Note:/ Consider using 'batchParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tBatchParameters :: Lens.Lens' Target (Core.Maybe Types.BatchParameters)
tBatchParameters = Lens.field @"batchParameters"
{-# INLINEABLE tBatchParameters #-}
{-# DEPRECATED batchParameters "Use generic-lens or generic-optics with 'batchParameters' instead"  #-}

-- | The @DeadLetterConfig@ that defines the target queue to send dead-letter queue events to.
--
-- /Note:/ Consider using 'deadLetterConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tDeadLetterConfig :: Lens.Lens' Target (Core.Maybe Types.DeadLetterConfig)
tDeadLetterConfig = Lens.field @"deadLetterConfig"
{-# INLINEABLE tDeadLetterConfig #-}
{-# DEPRECATED deadLetterConfig "Use generic-lens or generic-optics with 'deadLetterConfig' instead"  #-}

-- | Contains the Amazon ECS task definition and task count to be used, if the event target is an Amazon ECS task. For more information about Amazon ECS tasks, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Task Definitions > in the /Amazon EC2 Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'ecsParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tEcsParameters :: Lens.Lens' Target (Core.Maybe Types.EcsParameters)
tEcsParameters = Lens.field @"ecsParameters"
{-# INLINEABLE tEcsParameters #-}
{-# DEPRECATED ecsParameters "Use generic-lens or generic-optics with 'ecsParameters' instead"  #-}

-- | Contains the HTTP parameters to use when the target is a API Gateway REST endpoint.
--
-- If you specify an API Gateway REST API as a target, you can use this parameter to specify headers, path parameter, query string keys/values as part of your target invoking request.
--
-- /Note:/ Consider using 'httpParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tHttpParameters :: Lens.Lens' Target (Core.Maybe Types.HttpParameters)
tHttpParameters = Lens.field @"httpParameters"
{-# INLINEABLE tHttpParameters #-}
{-# DEPRECATED httpParameters "Use generic-lens or generic-optics with 'httpParameters' instead"  #-}

-- | Valid JSON text passed to the target. In this case, nothing from the event itself is passed to the target. For more information, see <http://www.rfc-editor.org/rfc/rfc7159.txt The JavaScript Object Notation (JSON) Data Interchange Format> .
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tInput :: Lens.Lens' Target (Core.Maybe Types.TargetInput)
tInput = Lens.field @"input"
{-# INLINEABLE tInput #-}
{-# DEPRECATED input "Use generic-lens or generic-optics with 'input' instead"  #-}

-- | The value of the JSONPath that is used for extracting part of the matched event when passing it to the target. You must use JSON dot notation, not bracket notation. For more information about JSON paths, see <http://goessner.net/articles/JsonPath/ JSONPath> .
--
-- /Note:/ Consider using 'inputPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tInputPath :: Lens.Lens' Target (Core.Maybe Types.TargetInputPath)
tInputPath = Lens.field @"inputPath"
{-# INLINEABLE tInputPath #-}
{-# DEPRECATED inputPath "Use generic-lens or generic-optics with 'inputPath' instead"  #-}

-- | Settings to enable you to provide custom input to a target based on certain event data. You can extract one or more key-value pairs from the event and then use that data to send customized input to the target.
--
-- /Note:/ Consider using 'inputTransformer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tInputTransformer :: Lens.Lens' Target (Core.Maybe Types.InputTransformer)
tInputTransformer = Lens.field @"inputTransformer"
{-# INLINEABLE tInputTransformer #-}
{-# DEPRECATED inputTransformer "Use generic-lens or generic-optics with 'inputTransformer' instead"  #-}

-- | The custom parameter you can use to control the shard assignment, when the target is a Kinesis data stream. If you do not include this parameter, the default is to use the @eventId@ as the partition key.
--
-- /Note:/ Consider using 'kinesisParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tKinesisParameters :: Lens.Lens' Target (Core.Maybe Types.KinesisParameters)
tKinesisParameters = Lens.field @"kinesisParameters"
{-# INLINEABLE tKinesisParameters #-}
{-# DEPRECATED kinesisParameters "Use generic-lens or generic-optics with 'kinesisParameters' instead"  #-}

-- | Contains the Redshift Data API parameters to use when the target is a Redshift cluster.
--
-- If you specify a Redshift Cluster as a Target, you can use this to specify parameters to invoke the Redshift Data API ExecuteStatement based on EventBridge events.
--
-- /Note:/ Consider using 'redshiftDataParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tRedshiftDataParameters :: Lens.Lens' Target (Core.Maybe Types.RedshiftDataParameters)
tRedshiftDataParameters = Lens.field @"redshiftDataParameters"
{-# INLINEABLE tRedshiftDataParameters #-}
{-# DEPRECATED redshiftDataParameters "Use generic-lens or generic-optics with 'redshiftDataParameters' instead"  #-}

-- | The @RetryPolicy@ object that contains the retry policy configuration to use for the dead-letter queue.
--
-- /Note:/ Consider using 'retryPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tRetryPolicy :: Lens.Lens' Target (Core.Maybe Types.RetryPolicy)
tRetryPolicy = Lens.field @"retryPolicy"
{-# INLINEABLE tRetryPolicy #-}
{-# DEPRECATED retryPolicy "Use generic-lens or generic-optics with 'retryPolicy' instead"  #-}

-- | The Amazon Resource Name (ARN) of the IAM role to be used for this target when the rule is triggered. If one rule triggers multiple targets, you can use a different IAM role for each target.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tRoleArn :: Lens.Lens' Target (Core.Maybe Types.RoleArn)
tRoleArn = Lens.field @"roleArn"
{-# INLINEABLE tRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | Parameters used when you are using the rule to invoke Amazon EC2 Run Command.
--
-- /Note:/ Consider using 'runCommandParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tRunCommandParameters :: Lens.Lens' Target (Core.Maybe Types.RunCommandParameters)
tRunCommandParameters = Lens.field @"runCommandParameters"
{-# INLINEABLE tRunCommandParameters #-}
{-# DEPRECATED runCommandParameters "Use generic-lens or generic-optics with 'runCommandParameters' instead"  #-}

-- | Contains the message group ID to use when the target is a FIFO queue.
--
-- If you specify an SQS FIFO queue as a target, the queue must have content-based deduplication enabled.
--
-- /Note:/ Consider using 'sqsParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tSqsParameters :: Lens.Lens' Target (Core.Maybe Types.SqsParameters)
tSqsParameters = Lens.field @"sqsParameters"
{-# INLINEABLE tSqsParameters #-}
{-# DEPRECATED sqsParameters "Use generic-lens or generic-optics with 'sqsParameters' instead"  #-}

instance Core.FromJSON Target where
        toJSON Target{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Id" Core..= id), Core.Just ("Arn" Core..= arn),
                  ("BatchParameters" Core..=) Core.<$> batchParameters,
                  ("DeadLetterConfig" Core..=) Core.<$> deadLetterConfig,
                  ("EcsParameters" Core..=) Core.<$> ecsParameters,
                  ("HttpParameters" Core..=) Core.<$> httpParameters,
                  ("Input" Core..=) Core.<$> input,
                  ("InputPath" Core..=) Core.<$> inputPath,
                  ("InputTransformer" Core..=) Core.<$> inputTransformer,
                  ("KinesisParameters" Core..=) Core.<$> kinesisParameters,
                  ("RedshiftDataParameters" Core..=) Core.<$> redshiftDataParameters,
                  ("RetryPolicy" Core..=) Core.<$> retryPolicy,
                  ("RoleArn" Core..=) Core.<$> roleArn,
                  ("RunCommandParameters" Core..=) Core.<$> runCommandParameters,
                  ("SqsParameters" Core..=) Core.<$> sqsParameters])

instance Core.FromJSON Target where
        parseJSON
          = Core.withObject "Target" Core.$
              \ x ->
                Target' Core.<$>
                  (x Core..: "Id") Core.<*> x Core..: "Arn" Core.<*>
                    x Core..:? "BatchParameters"
                    Core.<*> x Core..:? "DeadLetterConfig"
                    Core.<*> x Core..:? "EcsParameters"
                    Core.<*> x Core..:? "HttpParameters"
                    Core.<*> x Core..:? "Input"
                    Core.<*> x Core..:? "InputPath"
                    Core.<*> x Core..:? "InputTransformer"
                    Core.<*> x Core..:? "KinesisParameters"
                    Core.<*> x Core..:? "RedshiftDataParameters"
                    Core.<*> x Core..:? "RetryPolicy"
                    Core.<*> x Core..:? "RoleArn"
                    Core.<*> x Core..:? "RunCommandParameters"
                    Core.<*> x Core..:? "SqsParameters"
