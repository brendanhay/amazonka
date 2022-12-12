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
-- Module      : Amazonka.Pipes.Types.PipeSourceParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.PipeSourceParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types.FilterCriteria
import Amazonka.Pipes.Types.PipeSourceActiveMQBrokerParameters
import Amazonka.Pipes.Types.PipeSourceDynamoDBStreamParameters
import Amazonka.Pipes.Types.PipeSourceKinesisStreamParameters
import Amazonka.Pipes.Types.PipeSourceManagedStreamingKafkaParameters
import Amazonka.Pipes.Types.PipeSourceRabbitMQBrokerParameters
import Amazonka.Pipes.Types.PipeSourceSelfManagedKafkaParameters
import Amazonka.Pipes.Types.PipeSourceSqsQueueParameters
import qualified Amazonka.Prelude as Prelude

-- | The parameters required to set up a source for your pipe.
--
-- /See:/ 'newPipeSourceParameters' smart constructor.
data PipeSourceParameters = PipeSourceParameters'
  { -- | The parameters for using an Active MQ broker as a source.
    activeMQBrokerParameters :: Prelude.Maybe PipeSourceActiveMQBrokerParameters,
    -- | The parameters for using a DynamoDB stream as a source.
    dynamoDBStreamParameters :: Prelude.Maybe PipeSourceDynamoDBStreamParameters,
    -- | The collection of event patterns used to filter events. For more
    -- information, see
    -- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns>
    -- in the /Amazon EventBridge User Guide/.
    filterCriteria :: Prelude.Maybe FilterCriteria,
    -- | The parameters for using a Kinesis stream as a source.
    kinesisStreamParameters :: Prelude.Maybe PipeSourceKinesisStreamParameters,
    -- | The parameters for using an MSK stream as a source.
    managedStreamingKafkaParameters :: Prelude.Maybe PipeSourceManagedStreamingKafkaParameters,
    -- | The parameters for using a Rabbit MQ broker as a source.
    rabbitMQBrokerParameters :: Prelude.Maybe PipeSourceRabbitMQBrokerParameters,
    -- | The parameters for using a self-managed Apache Kafka stream as a source.
    selfManagedKafkaParameters :: Prelude.Maybe PipeSourceSelfManagedKafkaParameters,
    -- | The parameters for using a Amazon SQS stream as a source.
    sqsQueueParameters :: Prelude.Maybe PipeSourceSqsQueueParameters
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipeSourceParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeMQBrokerParameters', 'pipeSourceParameters_activeMQBrokerParameters' - The parameters for using an Active MQ broker as a source.
--
-- 'dynamoDBStreamParameters', 'pipeSourceParameters_dynamoDBStreamParameters' - The parameters for using a DynamoDB stream as a source.
--
-- 'filterCriteria', 'pipeSourceParameters_filterCriteria' - The collection of event patterns used to filter events. For more
-- information, see
-- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns>
-- in the /Amazon EventBridge User Guide/.
--
-- 'kinesisStreamParameters', 'pipeSourceParameters_kinesisStreamParameters' - The parameters for using a Kinesis stream as a source.
--
-- 'managedStreamingKafkaParameters', 'pipeSourceParameters_managedStreamingKafkaParameters' - The parameters for using an MSK stream as a source.
--
-- 'rabbitMQBrokerParameters', 'pipeSourceParameters_rabbitMQBrokerParameters' - The parameters for using a Rabbit MQ broker as a source.
--
-- 'selfManagedKafkaParameters', 'pipeSourceParameters_selfManagedKafkaParameters' - The parameters for using a self-managed Apache Kafka stream as a source.
--
-- 'sqsQueueParameters', 'pipeSourceParameters_sqsQueueParameters' - The parameters for using a Amazon SQS stream as a source.
newPipeSourceParameters ::
  PipeSourceParameters
newPipeSourceParameters =
  PipeSourceParameters'
    { activeMQBrokerParameters =
        Prelude.Nothing,
      dynamoDBStreamParameters = Prelude.Nothing,
      filterCriteria = Prelude.Nothing,
      kinesisStreamParameters = Prelude.Nothing,
      managedStreamingKafkaParameters = Prelude.Nothing,
      rabbitMQBrokerParameters = Prelude.Nothing,
      selfManagedKafkaParameters = Prelude.Nothing,
      sqsQueueParameters = Prelude.Nothing
    }

-- | The parameters for using an Active MQ broker as a source.
pipeSourceParameters_activeMQBrokerParameters :: Lens.Lens' PipeSourceParameters (Prelude.Maybe PipeSourceActiveMQBrokerParameters)
pipeSourceParameters_activeMQBrokerParameters = Lens.lens (\PipeSourceParameters' {activeMQBrokerParameters} -> activeMQBrokerParameters) (\s@PipeSourceParameters' {} a -> s {activeMQBrokerParameters = a} :: PipeSourceParameters)

-- | The parameters for using a DynamoDB stream as a source.
pipeSourceParameters_dynamoDBStreamParameters :: Lens.Lens' PipeSourceParameters (Prelude.Maybe PipeSourceDynamoDBStreamParameters)
pipeSourceParameters_dynamoDBStreamParameters = Lens.lens (\PipeSourceParameters' {dynamoDBStreamParameters} -> dynamoDBStreamParameters) (\s@PipeSourceParameters' {} a -> s {dynamoDBStreamParameters = a} :: PipeSourceParameters)

-- | The collection of event patterns used to filter events. For more
-- information, see
-- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns>
-- in the /Amazon EventBridge User Guide/.
pipeSourceParameters_filterCriteria :: Lens.Lens' PipeSourceParameters (Prelude.Maybe FilterCriteria)
pipeSourceParameters_filterCriteria = Lens.lens (\PipeSourceParameters' {filterCriteria} -> filterCriteria) (\s@PipeSourceParameters' {} a -> s {filterCriteria = a} :: PipeSourceParameters)

-- | The parameters for using a Kinesis stream as a source.
pipeSourceParameters_kinesisStreamParameters :: Lens.Lens' PipeSourceParameters (Prelude.Maybe PipeSourceKinesisStreamParameters)
pipeSourceParameters_kinesisStreamParameters = Lens.lens (\PipeSourceParameters' {kinesisStreamParameters} -> kinesisStreamParameters) (\s@PipeSourceParameters' {} a -> s {kinesisStreamParameters = a} :: PipeSourceParameters)

-- | The parameters for using an MSK stream as a source.
pipeSourceParameters_managedStreamingKafkaParameters :: Lens.Lens' PipeSourceParameters (Prelude.Maybe PipeSourceManagedStreamingKafkaParameters)
pipeSourceParameters_managedStreamingKafkaParameters = Lens.lens (\PipeSourceParameters' {managedStreamingKafkaParameters} -> managedStreamingKafkaParameters) (\s@PipeSourceParameters' {} a -> s {managedStreamingKafkaParameters = a} :: PipeSourceParameters)

-- | The parameters for using a Rabbit MQ broker as a source.
pipeSourceParameters_rabbitMQBrokerParameters :: Lens.Lens' PipeSourceParameters (Prelude.Maybe PipeSourceRabbitMQBrokerParameters)
pipeSourceParameters_rabbitMQBrokerParameters = Lens.lens (\PipeSourceParameters' {rabbitMQBrokerParameters} -> rabbitMQBrokerParameters) (\s@PipeSourceParameters' {} a -> s {rabbitMQBrokerParameters = a} :: PipeSourceParameters)

-- | The parameters for using a self-managed Apache Kafka stream as a source.
pipeSourceParameters_selfManagedKafkaParameters :: Lens.Lens' PipeSourceParameters (Prelude.Maybe PipeSourceSelfManagedKafkaParameters)
pipeSourceParameters_selfManagedKafkaParameters = Lens.lens (\PipeSourceParameters' {selfManagedKafkaParameters} -> selfManagedKafkaParameters) (\s@PipeSourceParameters' {} a -> s {selfManagedKafkaParameters = a} :: PipeSourceParameters)

-- | The parameters for using a Amazon SQS stream as a source.
pipeSourceParameters_sqsQueueParameters :: Lens.Lens' PipeSourceParameters (Prelude.Maybe PipeSourceSqsQueueParameters)
pipeSourceParameters_sqsQueueParameters = Lens.lens (\PipeSourceParameters' {sqsQueueParameters} -> sqsQueueParameters) (\s@PipeSourceParameters' {} a -> s {sqsQueueParameters = a} :: PipeSourceParameters)

instance Data.FromJSON PipeSourceParameters where
  parseJSON =
    Data.withObject
      "PipeSourceParameters"
      ( \x ->
          PipeSourceParameters'
            Prelude.<$> (x Data..:? "ActiveMQBrokerParameters")
            Prelude.<*> (x Data..:? "DynamoDBStreamParameters")
            Prelude.<*> (x Data..:? "FilterCriteria")
            Prelude.<*> (x Data..:? "KinesisStreamParameters")
            Prelude.<*> (x Data..:? "ManagedStreamingKafkaParameters")
            Prelude.<*> (x Data..:? "RabbitMQBrokerParameters")
            Prelude.<*> (x Data..:? "SelfManagedKafkaParameters")
            Prelude.<*> (x Data..:? "SqsQueueParameters")
      )

instance Prelude.Hashable PipeSourceParameters where
  hashWithSalt _salt PipeSourceParameters' {..} =
    _salt
      `Prelude.hashWithSalt` activeMQBrokerParameters
      `Prelude.hashWithSalt` dynamoDBStreamParameters
      `Prelude.hashWithSalt` filterCriteria
      `Prelude.hashWithSalt` kinesisStreamParameters
      `Prelude.hashWithSalt` managedStreamingKafkaParameters
      `Prelude.hashWithSalt` rabbitMQBrokerParameters
      `Prelude.hashWithSalt` selfManagedKafkaParameters
      `Prelude.hashWithSalt` sqsQueueParameters

instance Prelude.NFData PipeSourceParameters where
  rnf PipeSourceParameters' {..} =
    Prelude.rnf activeMQBrokerParameters
      `Prelude.seq` Prelude.rnf dynamoDBStreamParameters
      `Prelude.seq` Prelude.rnf filterCriteria
      `Prelude.seq` Prelude.rnf kinesisStreamParameters
      `Prelude.seq` Prelude.rnf managedStreamingKafkaParameters
      `Prelude.seq` Prelude.rnf rabbitMQBrokerParameters
      `Prelude.seq` Prelude.rnf selfManagedKafkaParameters
      `Prelude.seq` Prelude.rnf sqsQueueParameters

instance Data.ToJSON PipeSourceParameters where
  toJSON PipeSourceParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ActiveMQBrokerParameters" Data..=)
              Prelude.<$> activeMQBrokerParameters,
            ("DynamoDBStreamParameters" Data..=)
              Prelude.<$> dynamoDBStreamParameters,
            ("FilterCriteria" Data..=)
              Prelude.<$> filterCriteria,
            ("KinesisStreamParameters" Data..=)
              Prelude.<$> kinesisStreamParameters,
            ("ManagedStreamingKafkaParameters" Data..=)
              Prelude.<$> managedStreamingKafkaParameters,
            ("RabbitMQBrokerParameters" Data..=)
              Prelude.<$> rabbitMQBrokerParameters,
            ("SelfManagedKafkaParameters" Data..=)
              Prelude.<$> selfManagedKafkaParameters,
            ("SqsQueueParameters" Data..=)
              Prelude.<$> sqsQueueParameters
          ]
      )
