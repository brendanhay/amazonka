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
-- Module      : Amazonka.Scheduler.Types.Target
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Scheduler.Types.Target where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Scheduler.Types.DeadLetterConfig
import Amazonka.Scheduler.Types.EcsParameters
import Amazonka.Scheduler.Types.EventBridgeParameters
import Amazonka.Scheduler.Types.KinesisParameters
import Amazonka.Scheduler.Types.RetryPolicy
import Amazonka.Scheduler.Types.SageMakerPipelineParameters
import Amazonka.Scheduler.Types.SqsParameters

-- | The schedule\'s target. EventBridge Scheduler supports templated target
-- that invoke common API operations, as well as universal targets that you
-- can customize to invoke over 6,000 API operations across more than 270
-- services. You can only specify one templated or universal target for a
-- schedule.
--
-- /See:/ 'newTarget' smart constructor.
data Target = Target'
  { -- | An object that contains information about an Amazon SQS queue that
    -- EventBridge Scheduler uses as a dead-letter queue for your schedule. If
    -- specified, EventBridge Scheduler delivers failed events that could not
    -- be successfully delivered to a target to the queue.
    deadLetterConfig :: Prelude.Maybe DeadLetterConfig,
    -- | The templated target type for the Amazon ECS
    -- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_RunTask.html RunTask>
    -- API operation.
    ecsParameters :: Prelude.Maybe EcsParameters,
    -- | The templated target type for the EventBridge
    -- <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_PutEvents.html PutEvents>
    -- API operation.
    eventBridgeParameters :: Prelude.Maybe EventBridgeParameters,
    -- | The text, or well-formed JSON, passed to the target. If you are
    -- configuring a templated Lambda, AWS Step Functions, or Amazon
    -- EventBridge target, the input must be a well-formed JSON. For all other
    -- target types, a JSON is not required. If you do not specify anything for
    -- this field, EventBridge Scheduler delivers a default notification to the
    -- target.
    input :: Prelude.Maybe Prelude.Text,
    -- | The templated target type for the Amazon Kinesis
    -- <kinesis/latest/APIReference/API_PutRecord.html PutRecord> API
    -- operation.
    kinesisParameters :: Prelude.Maybe KinesisParameters,
    -- | A @RetryPolicy@ object that includes information about the retry policy
    -- settings, including the maximum age of an event, and the maximum number
    -- of times EventBridge Scheduler will try to deliver the event to a
    -- target.
    retryPolicy :: Prelude.Maybe RetryPolicy,
    -- | The templated target type for the Amazon SageMaker
    -- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_StartPipelineExecution.html StartPipelineExecution>
    -- API operation.
    sageMakerPipelineParameters :: Prelude.Maybe SageMakerPipelineParameters,
    -- | The templated target type for the Amazon SQS
    -- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_SendMessage.html SendMessage>
    -- API operation. Contains the message group ID to use when the target is a
    -- FIFO queue. If you specify an Amazon SQS FIFO queue as a target, the
    -- queue must have content-based deduplication enabled. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/using-messagededuplicationid-property.html Using the Amazon SQS message deduplication ID>
    -- in the /Amazon SQS Developer Guide/.
    sqsParameters :: Prelude.Maybe SqsParameters,
    -- | The Amazon Resource Name (ARN) of the target.
    arn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role that EventBridge
    -- Scheduler will use for this target when the schedule is invoked.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Target' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deadLetterConfig', 'target_deadLetterConfig' - An object that contains information about an Amazon SQS queue that
-- EventBridge Scheduler uses as a dead-letter queue for your schedule. If
-- specified, EventBridge Scheduler delivers failed events that could not
-- be successfully delivered to a target to the queue.
--
-- 'ecsParameters', 'target_ecsParameters' - The templated target type for the Amazon ECS
-- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_RunTask.html RunTask>
-- API operation.
--
-- 'eventBridgeParameters', 'target_eventBridgeParameters' - The templated target type for the EventBridge
-- <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_PutEvents.html PutEvents>
-- API operation.
--
-- 'input', 'target_input' - The text, or well-formed JSON, passed to the target. If you are
-- configuring a templated Lambda, AWS Step Functions, or Amazon
-- EventBridge target, the input must be a well-formed JSON. For all other
-- target types, a JSON is not required. If you do not specify anything for
-- this field, EventBridge Scheduler delivers a default notification to the
-- target.
--
-- 'kinesisParameters', 'target_kinesisParameters' - The templated target type for the Amazon Kinesis
-- <kinesis/latest/APIReference/API_PutRecord.html PutRecord> API
-- operation.
--
-- 'retryPolicy', 'target_retryPolicy' - A @RetryPolicy@ object that includes information about the retry policy
-- settings, including the maximum age of an event, and the maximum number
-- of times EventBridge Scheduler will try to deliver the event to a
-- target.
--
-- 'sageMakerPipelineParameters', 'target_sageMakerPipelineParameters' - The templated target type for the Amazon SageMaker
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_StartPipelineExecution.html StartPipelineExecution>
-- API operation.
--
-- 'sqsParameters', 'target_sqsParameters' - The templated target type for the Amazon SQS
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_SendMessage.html SendMessage>
-- API operation. Contains the message group ID to use when the target is a
-- FIFO queue. If you specify an Amazon SQS FIFO queue as a target, the
-- queue must have content-based deduplication enabled. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/using-messagededuplicationid-property.html Using the Amazon SQS message deduplication ID>
-- in the /Amazon SQS Developer Guide/.
--
-- 'arn', 'target_arn' - The Amazon Resource Name (ARN) of the target.
--
-- 'roleArn', 'target_roleArn' - The Amazon Resource Name (ARN) of the IAM role that EventBridge
-- Scheduler will use for this target when the schedule is invoked.
newTarget ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  Target
newTarget pArn_ pRoleArn_ =
  Target'
    { deadLetterConfig = Prelude.Nothing,
      ecsParameters = Prelude.Nothing,
      eventBridgeParameters = Prelude.Nothing,
      input = Prelude.Nothing,
      kinesisParameters = Prelude.Nothing,
      retryPolicy = Prelude.Nothing,
      sageMakerPipelineParameters = Prelude.Nothing,
      sqsParameters = Prelude.Nothing,
      arn = pArn_,
      roleArn = pRoleArn_
    }

-- | An object that contains information about an Amazon SQS queue that
-- EventBridge Scheduler uses as a dead-letter queue for your schedule. If
-- specified, EventBridge Scheduler delivers failed events that could not
-- be successfully delivered to a target to the queue.
target_deadLetterConfig :: Lens.Lens' Target (Prelude.Maybe DeadLetterConfig)
target_deadLetterConfig = Lens.lens (\Target' {deadLetterConfig} -> deadLetterConfig) (\s@Target' {} a -> s {deadLetterConfig = a} :: Target)

-- | The templated target type for the Amazon ECS
-- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_RunTask.html RunTask>
-- API operation.
target_ecsParameters :: Lens.Lens' Target (Prelude.Maybe EcsParameters)
target_ecsParameters = Lens.lens (\Target' {ecsParameters} -> ecsParameters) (\s@Target' {} a -> s {ecsParameters = a} :: Target)

-- | The templated target type for the EventBridge
-- <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_PutEvents.html PutEvents>
-- API operation.
target_eventBridgeParameters :: Lens.Lens' Target (Prelude.Maybe EventBridgeParameters)
target_eventBridgeParameters = Lens.lens (\Target' {eventBridgeParameters} -> eventBridgeParameters) (\s@Target' {} a -> s {eventBridgeParameters = a} :: Target)

-- | The text, or well-formed JSON, passed to the target. If you are
-- configuring a templated Lambda, AWS Step Functions, or Amazon
-- EventBridge target, the input must be a well-formed JSON. For all other
-- target types, a JSON is not required. If you do not specify anything for
-- this field, EventBridge Scheduler delivers a default notification to the
-- target.
target_input :: Lens.Lens' Target (Prelude.Maybe Prelude.Text)
target_input = Lens.lens (\Target' {input} -> input) (\s@Target' {} a -> s {input = a} :: Target)

-- | The templated target type for the Amazon Kinesis
-- <kinesis/latest/APIReference/API_PutRecord.html PutRecord> API
-- operation.
target_kinesisParameters :: Lens.Lens' Target (Prelude.Maybe KinesisParameters)
target_kinesisParameters = Lens.lens (\Target' {kinesisParameters} -> kinesisParameters) (\s@Target' {} a -> s {kinesisParameters = a} :: Target)

-- | A @RetryPolicy@ object that includes information about the retry policy
-- settings, including the maximum age of an event, and the maximum number
-- of times EventBridge Scheduler will try to deliver the event to a
-- target.
target_retryPolicy :: Lens.Lens' Target (Prelude.Maybe RetryPolicy)
target_retryPolicy = Lens.lens (\Target' {retryPolicy} -> retryPolicy) (\s@Target' {} a -> s {retryPolicy = a} :: Target)

-- | The templated target type for the Amazon SageMaker
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_StartPipelineExecution.html StartPipelineExecution>
-- API operation.
target_sageMakerPipelineParameters :: Lens.Lens' Target (Prelude.Maybe SageMakerPipelineParameters)
target_sageMakerPipelineParameters = Lens.lens (\Target' {sageMakerPipelineParameters} -> sageMakerPipelineParameters) (\s@Target' {} a -> s {sageMakerPipelineParameters = a} :: Target)

-- | The templated target type for the Amazon SQS
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_SendMessage.html SendMessage>
-- API operation. Contains the message group ID to use when the target is a
-- FIFO queue. If you specify an Amazon SQS FIFO queue as a target, the
-- queue must have content-based deduplication enabled. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/using-messagededuplicationid-property.html Using the Amazon SQS message deduplication ID>
-- in the /Amazon SQS Developer Guide/.
target_sqsParameters :: Lens.Lens' Target (Prelude.Maybe SqsParameters)
target_sqsParameters = Lens.lens (\Target' {sqsParameters} -> sqsParameters) (\s@Target' {} a -> s {sqsParameters = a} :: Target)

-- | The Amazon Resource Name (ARN) of the target.
target_arn :: Lens.Lens' Target Prelude.Text
target_arn = Lens.lens (\Target' {arn} -> arn) (\s@Target' {} a -> s {arn = a} :: Target)

-- | The Amazon Resource Name (ARN) of the IAM role that EventBridge
-- Scheduler will use for this target when the schedule is invoked.
target_roleArn :: Lens.Lens' Target Prelude.Text
target_roleArn = Lens.lens (\Target' {roleArn} -> roleArn) (\s@Target' {} a -> s {roleArn = a} :: Target)

instance Data.FromJSON Target where
  parseJSON =
    Data.withObject
      "Target"
      ( \x ->
          Target'
            Prelude.<$> (x Data..:? "DeadLetterConfig")
            Prelude.<*> (x Data..:? "EcsParameters")
            Prelude.<*> (x Data..:? "EventBridgeParameters")
            Prelude.<*> (x Data..:? "Input")
            Prelude.<*> (x Data..:? "KinesisParameters")
            Prelude.<*> (x Data..:? "RetryPolicy")
            Prelude.<*> (x Data..:? "SageMakerPipelineParameters")
            Prelude.<*> (x Data..:? "SqsParameters")
            Prelude.<*> (x Data..: "Arn")
            Prelude.<*> (x Data..: "RoleArn")
      )

instance Prelude.Hashable Target where
  hashWithSalt _salt Target' {..} =
    _salt
      `Prelude.hashWithSalt` deadLetterConfig
      `Prelude.hashWithSalt` ecsParameters
      `Prelude.hashWithSalt` eventBridgeParameters
      `Prelude.hashWithSalt` input
      `Prelude.hashWithSalt` kinesisParameters
      `Prelude.hashWithSalt` retryPolicy
      `Prelude.hashWithSalt` sageMakerPipelineParameters
      `Prelude.hashWithSalt` sqsParameters
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData Target where
  rnf Target' {..} =
    Prelude.rnf deadLetterConfig `Prelude.seq`
      Prelude.rnf ecsParameters `Prelude.seq`
        Prelude.rnf eventBridgeParameters `Prelude.seq`
          Prelude.rnf input `Prelude.seq`
            Prelude.rnf kinesisParameters `Prelude.seq`
              Prelude.rnf retryPolicy `Prelude.seq`
                Prelude.rnf sageMakerPipelineParameters `Prelude.seq`
                  Prelude.rnf sqsParameters `Prelude.seq`
                    Prelude.rnf arn `Prelude.seq`
                      Prelude.rnf roleArn

instance Data.ToJSON Target where
  toJSON Target' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeadLetterConfig" Data..=)
              Prelude.<$> deadLetterConfig,
            ("EcsParameters" Data..=) Prelude.<$> ecsParameters,
            ("EventBridgeParameters" Data..=)
              Prelude.<$> eventBridgeParameters,
            ("Input" Data..=) Prelude.<$> input,
            ("KinesisParameters" Data..=)
              Prelude.<$> kinesisParameters,
            ("RetryPolicy" Data..=) Prelude.<$> retryPolicy,
            ("SageMakerPipelineParameters" Data..=)
              Prelude.<$> sageMakerPipelineParameters,
            ("SqsParameters" Data..=) Prelude.<$> sqsParameters,
            Prelude.Just ("Arn" Data..= arn),
            Prelude.Just ("RoleArn" Data..= roleArn)
          ]
      )
