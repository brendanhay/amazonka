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
-- Module      : Amazonka.Pipes.Types.UpdatePipeSourceParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.UpdatePipeSourceParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types.FilterCriteria
import Amazonka.Pipes.Types.UpdatePipeSourceActiveMQBrokerParameters
import Amazonka.Pipes.Types.UpdatePipeSourceDynamoDBStreamParameters
import Amazonka.Pipes.Types.UpdatePipeSourceKinesisStreamParameters
import Amazonka.Pipes.Types.UpdatePipeSourceManagedStreamingKafkaParameters
import Amazonka.Pipes.Types.UpdatePipeSourceRabbitMQBrokerParameters
import Amazonka.Pipes.Types.UpdatePipeSourceSelfManagedKafkaParameters
import Amazonka.Pipes.Types.UpdatePipeSourceSqsQueueParameters
import qualified Amazonka.Prelude as Prelude

-- | The parameters required to set up a source for your pipe.
--
-- /See:/ 'newUpdatePipeSourceParameters' smart constructor.
data UpdatePipeSourceParameters = UpdatePipeSourceParameters'
  { -- | The parameters for using an Active MQ broker as a source.
    activeMQBrokerParameters :: Prelude.Maybe UpdatePipeSourceActiveMQBrokerParameters,
    -- | The parameters for using a DynamoDB stream as a source.
    dynamoDBStreamParameters :: Prelude.Maybe UpdatePipeSourceDynamoDBStreamParameters,
    -- | The collection of event patterns used to filter events. For more
    -- information, see
    -- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns>
    -- in the /Amazon EventBridge User Guide/.
    filterCriteria :: Prelude.Maybe FilterCriteria,
    -- | The parameters for using a Kinesis stream as a source.
    kinesisStreamParameters :: Prelude.Maybe UpdatePipeSourceKinesisStreamParameters,
    -- | The parameters for using an MSK stream as a source.
    managedStreamingKafkaParameters :: Prelude.Maybe UpdatePipeSourceManagedStreamingKafkaParameters,
    -- | The parameters for using a Rabbit MQ broker as a source.
    rabbitMQBrokerParameters :: Prelude.Maybe UpdatePipeSourceRabbitMQBrokerParameters,
    -- | The parameters for using a self-managed Apache Kafka stream as a source.
    selfManagedKafkaParameters :: Prelude.Maybe UpdatePipeSourceSelfManagedKafkaParameters,
    -- | The parameters for using a Amazon SQS stream as a source.
    sqsQueueParameters :: Prelude.Maybe UpdatePipeSourceSqsQueueParameters
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePipeSourceParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeMQBrokerParameters', 'updatePipeSourceParameters_activeMQBrokerParameters' - The parameters for using an Active MQ broker as a source.
--
-- 'dynamoDBStreamParameters', 'updatePipeSourceParameters_dynamoDBStreamParameters' - The parameters for using a DynamoDB stream as a source.
--
-- 'filterCriteria', 'updatePipeSourceParameters_filterCriteria' - The collection of event patterns used to filter events. For more
-- information, see
-- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns>
-- in the /Amazon EventBridge User Guide/.
--
-- 'kinesisStreamParameters', 'updatePipeSourceParameters_kinesisStreamParameters' - The parameters for using a Kinesis stream as a source.
--
-- 'managedStreamingKafkaParameters', 'updatePipeSourceParameters_managedStreamingKafkaParameters' - The parameters for using an MSK stream as a source.
--
-- 'rabbitMQBrokerParameters', 'updatePipeSourceParameters_rabbitMQBrokerParameters' - The parameters for using a Rabbit MQ broker as a source.
--
-- 'selfManagedKafkaParameters', 'updatePipeSourceParameters_selfManagedKafkaParameters' - The parameters for using a self-managed Apache Kafka stream as a source.
--
-- 'sqsQueueParameters', 'updatePipeSourceParameters_sqsQueueParameters' - The parameters for using a Amazon SQS stream as a source.
newUpdatePipeSourceParameters ::
  UpdatePipeSourceParameters
newUpdatePipeSourceParameters =
  UpdatePipeSourceParameters'
    { activeMQBrokerParameters =
        Prelude.Nothing,
      dynamoDBStreamParameters = Prelude.Nothing,
      filterCriteria = Prelude.Nothing,
      kinesisStreamParameters = Prelude.Nothing,
      managedStreamingKafkaParameters =
        Prelude.Nothing,
      rabbitMQBrokerParameters = Prelude.Nothing,
      selfManagedKafkaParameters = Prelude.Nothing,
      sqsQueueParameters = Prelude.Nothing
    }

-- | The parameters for using an Active MQ broker as a source.
updatePipeSourceParameters_activeMQBrokerParameters :: Lens.Lens' UpdatePipeSourceParameters (Prelude.Maybe UpdatePipeSourceActiveMQBrokerParameters)
updatePipeSourceParameters_activeMQBrokerParameters = Lens.lens (\UpdatePipeSourceParameters' {activeMQBrokerParameters} -> activeMQBrokerParameters) (\s@UpdatePipeSourceParameters' {} a -> s {activeMQBrokerParameters = a} :: UpdatePipeSourceParameters)

-- | The parameters for using a DynamoDB stream as a source.
updatePipeSourceParameters_dynamoDBStreamParameters :: Lens.Lens' UpdatePipeSourceParameters (Prelude.Maybe UpdatePipeSourceDynamoDBStreamParameters)
updatePipeSourceParameters_dynamoDBStreamParameters = Lens.lens (\UpdatePipeSourceParameters' {dynamoDBStreamParameters} -> dynamoDBStreamParameters) (\s@UpdatePipeSourceParameters' {} a -> s {dynamoDBStreamParameters = a} :: UpdatePipeSourceParameters)

-- | The collection of event patterns used to filter events. For more
-- information, see
-- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns>
-- in the /Amazon EventBridge User Guide/.
updatePipeSourceParameters_filterCriteria :: Lens.Lens' UpdatePipeSourceParameters (Prelude.Maybe FilterCriteria)
updatePipeSourceParameters_filterCriteria = Lens.lens (\UpdatePipeSourceParameters' {filterCriteria} -> filterCriteria) (\s@UpdatePipeSourceParameters' {} a -> s {filterCriteria = a} :: UpdatePipeSourceParameters)

-- | The parameters for using a Kinesis stream as a source.
updatePipeSourceParameters_kinesisStreamParameters :: Lens.Lens' UpdatePipeSourceParameters (Prelude.Maybe UpdatePipeSourceKinesisStreamParameters)
updatePipeSourceParameters_kinesisStreamParameters = Lens.lens (\UpdatePipeSourceParameters' {kinesisStreamParameters} -> kinesisStreamParameters) (\s@UpdatePipeSourceParameters' {} a -> s {kinesisStreamParameters = a} :: UpdatePipeSourceParameters)

-- | The parameters for using an MSK stream as a source.
updatePipeSourceParameters_managedStreamingKafkaParameters :: Lens.Lens' UpdatePipeSourceParameters (Prelude.Maybe UpdatePipeSourceManagedStreamingKafkaParameters)
updatePipeSourceParameters_managedStreamingKafkaParameters = Lens.lens (\UpdatePipeSourceParameters' {managedStreamingKafkaParameters} -> managedStreamingKafkaParameters) (\s@UpdatePipeSourceParameters' {} a -> s {managedStreamingKafkaParameters = a} :: UpdatePipeSourceParameters)

-- | The parameters for using a Rabbit MQ broker as a source.
updatePipeSourceParameters_rabbitMQBrokerParameters :: Lens.Lens' UpdatePipeSourceParameters (Prelude.Maybe UpdatePipeSourceRabbitMQBrokerParameters)
updatePipeSourceParameters_rabbitMQBrokerParameters = Lens.lens (\UpdatePipeSourceParameters' {rabbitMQBrokerParameters} -> rabbitMQBrokerParameters) (\s@UpdatePipeSourceParameters' {} a -> s {rabbitMQBrokerParameters = a} :: UpdatePipeSourceParameters)

-- | The parameters for using a self-managed Apache Kafka stream as a source.
updatePipeSourceParameters_selfManagedKafkaParameters :: Lens.Lens' UpdatePipeSourceParameters (Prelude.Maybe UpdatePipeSourceSelfManagedKafkaParameters)
updatePipeSourceParameters_selfManagedKafkaParameters = Lens.lens (\UpdatePipeSourceParameters' {selfManagedKafkaParameters} -> selfManagedKafkaParameters) (\s@UpdatePipeSourceParameters' {} a -> s {selfManagedKafkaParameters = a} :: UpdatePipeSourceParameters)

-- | The parameters for using a Amazon SQS stream as a source.
updatePipeSourceParameters_sqsQueueParameters :: Lens.Lens' UpdatePipeSourceParameters (Prelude.Maybe UpdatePipeSourceSqsQueueParameters)
updatePipeSourceParameters_sqsQueueParameters = Lens.lens (\UpdatePipeSourceParameters' {sqsQueueParameters} -> sqsQueueParameters) (\s@UpdatePipeSourceParameters' {} a -> s {sqsQueueParameters = a} :: UpdatePipeSourceParameters)

instance Prelude.Hashable UpdatePipeSourceParameters where
  hashWithSalt _salt UpdatePipeSourceParameters' {..} =
    _salt
      `Prelude.hashWithSalt` activeMQBrokerParameters
      `Prelude.hashWithSalt` dynamoDBStreamParameters
      `Prelude.hashWithSalt` filterCriteria
      `Prelude.hashWithSalt` kinesisStreamParameters
      `Prelude.hashWithSalt` managedStreamingKafkaParameters
      `Prelude.hashWithSalt` rabbitMQBrokerParameters
      `Prelude.hashWithSalt` selfManagedKafkaParameters
      `Prelude.hashWithSalt` sqsQueueParameters

instance Prelude.NFData UpdatePipeSourceParameters where
  rnf UpdatePipeSourceParameters' {..} =
    Prelude.rnf activeMQBrokerParameters
      `Prelude.seq` Prelude.rnf dynamoDBStreamParameters
      `Prelude.seq` Prelude.rnf filterCriteria
      `Prelude.seq` Prelude.rnf kinesisStreamParameters
      `Prelude.seq` Prelude.rnf managedStreamingKafkaParameters
      `Prelude.seq` Prelude.rnf rabbitMQBrokerParameters
      `Prelude.seq` Prelude.rnf selfManagedKafkaParameters
      `Prelude.seq` Prelude.rnf sqsQueueParameters

instance Data.ToJSON UpdatePipeSourceParameters where
  toJSON UpdatePipeSourceParameters' {..} =
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
