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
-- Module      : Network.AWS.IoT.Types.KafkaAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.KafkaAction where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Send messages to an Amazon Managed Streaming for Apache Kafka (Amazon
-- MSK) or self-managed Apache Kafka cluster.
--
-- /See:/ 'newKafkaAction' smart constructor.
data KafkaAction = KafkaAction'
  { -- | The Kafka message key.
    key :: Core.Maybe Core.Text,
    -- | The Kafka message partition.
    partition :: Core.Maybe Core.Text,
    -- | The ARN of Kafka action\'s VPC @TopicRuleDestination@.
    destinationArn :: Core.Text,
    -- | The Kafka topic for messages to be sent to the Kafka broker.
    topic :: Core.Text,
    -- | Properties of the Apache Kafka producer client.
    clientProperties :: Core.HashMap Core.Text Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'KafkaAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'kafkaAction_key' - The Kafka message key.
--
-- 'partition', 'kafkaAction_partition' - The Kafka message partition.
--
-- 'destinationArn', 'kafkaAction_destinationArn' - The ARN of Kafka action\'s VPC @TopicRuleDestination@.
--
-- 'topic', 'kafkaAction_topic' - The Kafka topic for messages to be sent to the Kafka broker.
--
-- 'clientProperties', 'kafkaAction_clientProperties' - Properties of the Apache Kafka producer client.
newKafkaAction ::
  -- | 'destinationArn'
  Core.Text ->
  -- | 'topic'
  Core.Text ->
  KafkaAction
newKafkaAction pDestinationArn_ pTopic_ =
  KafkaAction'
    { key = Core.Nothing,
      partition = Core.Nothing,
      destinationArn = pDestinationArn_,
      topic = pTopic_,
      clientProperties = Core.mempty
    }

-- | The Kafka message key.
kafkaAction_key :: Lens.Lens' KafkaAction (Core.Maybe Core.Text)
kafkaAction_key = Lens.lens (\KafkaAction' {key} -> key) (\s@KafkaAction' {} a -> s {key = a} :: KafkaAction)

-- | The Kafka message partition.
kafkaAction_partition :: Lens.Lens' KafkaAction (Core.Maybe Core.Text)
kafkaAction_partition = Lens.lens (\KafkaAction' {partition} -> partition) (\s@KafkaAction' {} a -> s {partition = a} :: KafkaAction)

-- | The ARN of Kafka action\'s VPC @TopicRuleDestination@.
kafkaAction_destinationArn :: Lens.Lens' KafkaAction Core.Text
kafkaAction_destinationArn = Lens.lens (\KafkaAction' {destinationArn} -> destinationArn) (\s@KafkaAction' {} a -> s {destinationArn = a} :: KafkaAction)

-- | The Kafka topic for messages to be sent to the Kafka broker.
kafkaAction_topic :: Lens.Lens' KafkaAction Core.Text
kafkaAction_topic = Lens.lens (\KafkaAction' {topic} -> topic) (\s@KafkaAction' {} a -> s {topic = a} :: KafkaAction)

-- | Properties of the Apache Kafka producer client.
kafkaAction_clientProperties :: Lens.Lens' KafkaAction (Core.HashMap Core.Text Core.Text)
kafkaAction_clientProperties = Lens.lens (\KafkaAction' {clientProperties} -> clientProperties) (\s@KafkaAction' {} a -> s {clientProperties = a} :: KafkaAction) Core.. Lens._Coerce

instance Core.FromJSON KafkaAction where
  parseJSON =
    Core.withObject
      "KafkaAction"
      ( \x ->
          KafkaAction'
            Core.<$> (x Core..:? "key")
            Core.<*> (x Core..:? "partition")
            Core.<*> (x Core..: "destinationArn")
            Core.<*> (x Core..: "topic")
            Core.<*> (x Core..:? "clientProperties" Core..!= Core.mempty)
      )

instance Core.Hashable KafkaAction

instance Core.NFData KafkaAction

instance Core.ToJSON KafkaAction where
  toJSON KafkaAction' {..} =
    Core.object
      ( Core.catMaybes
          [ ("key" Core..=) Core.<$> key,
            ("partition" Core..=) Core.<$> partition,
            Core.Just ("destinationArn" Core..= destinationArn),
            Core.Just ("topic" Core..= topic),
            Core.Just
              ("clientProperties" Core..= clientProperties)
          ]
      )
