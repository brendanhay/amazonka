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
-- Module      : Amazonka.IoT.Types.KafkaAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.KafkaAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Send messages to an Amazon Managed Streaming for Apache Kafka (Amazon
-- MSK) or self-managed Apache Kafka cluster.
--
-- /See:/ 'newKafkaAction' smart constructor.
data KafkaAction = KafkaAction'
  { -- | The Kafka message key.
    key :: Prelude.Maybe Prelude.Text,
    -- | The Kafka message partition.
    partition :: Prelude.Maybe Prelude.Text,
    -- | The ARN of Kafka action\'s VPC @TopicRuleDestination@.
    destinationArn :: Prelude.Text,
    -- | The Kafka topic for messages to be sent to the Kafka broker.
    topic :: Prelude.Text,
    -- | Properties of the Apache Kafka producer client.
    clientProperties :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'topic'
  Prelude.Text ->
  KafkaAction
newKafkaAction pDestinationArn_ pTopic_ =
  KafkaAction'
    { key = Prelude.Nothing,
      partition = Prelude.Nothing,
      destinationArn = pDestinationArn_,
      topic = pTopic_,
      clientProperties = Prelude.mempty
    }

-- | The Kafka message key.
kafkaAction_key :: Lens.Lens' KafkaAction (Prelude.Maybe Prelude.Text)
kafkaAction_key = Lens.lens (\KafkaAction' {key} -> key) (\s@KafkaAction' {} a -> s {key = a} :: KafkaAction)

-- | The Kafka message partition.
kafkaAction_partition :: Lens.Lens' KafkaAction (Prelude.Maybe Prelude.Text)
kafkaAction_partition = Lens.lens (\KafkaAction' {partition} -> partition) (\s@KafkaAction' {} a -> s {partition = a} :: KafkaAction)

-- | The ARN of Kafka action\'s VPC @TopicRuleDestination@.
kafkaAction_destinationArn :: Lens.Lens' KafkaAction Prelude.Text
kafkaAction_destinationArn = Lens.lens (\KafkaAction' {destinationArn} -> destinationArn) (\s@KafkaAction' {} a -> s {destinationArn = a} :: KafkaAction)

-- | The Kafka topic for messages to be sent to the Kafka broker.
kafkaAction_topic :: Lens.Lens' KafkaAction Prelude.Text
kafkaAction_topic = Lens.lens (\KafkaAction' {topic} -> topic) (\s@KafkaAction' {} a -> s {topic = a} :: KafkaAction)

-- | Properties of the Apache Kafka producer client.
kafkaAction_clientProperties :: Lens.Lens' KafkaAction (Prelude.HashMap Prelude.Text Prelude.Text)
kafkaAction_clientProperties = Lens.lens (\KafkaAction' {clientProperties} -> clientProperties) (\s@KafkaAction' {} a -> s {clientProperties = a} :: KafkaAction) Prelude.. Lens.coerced

instance Data.FromJSON KafkaAction where
  parseJSON =
    Data.withObject
      "KafkaAction"
      ( \x ->
          KafkaAction'
            Prelude.<$> (x Data..:? "key")
            Prelude.<*> (x Data..:? "partition")
            Prelude.<*> (x Data..: "destinationArn")
            Prelude.<*> (x Data..: "topic")
            Prelude.<*> ( x
                            Data..:? "clientProperties"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable KafkaAction where
  hashWithSalt _salt KafkaAction' {..} =
    _salt
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` partition
      `Prelude.hashWithSalt` destinationArn
      `Prelude.hashWithSalt` topic
      `Prelude.hashWithSalt` clientProperties

instance Prelude.NFData KafkaAction where
  rnf KafkaAction' {..} =
    Prelude.rnf key
      `Prelude.seq` Prelude.rnf partition
      `Prelude.seq` Prelude.rnf destinationArn
      `Prelude.seq` Prelude.rnf topic
      `Prelude.seq` Prelude.rnf clientProperties

instance Data.ToJSON KafkaAction where
  toJSON KafkaAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("key" Data..=) Prelude.<$> key,
            ("partition" Data..=) Prelude.<$> partition,
            Prelude.Just
              ("destinationArn" Data..= destinationArn),
            Prelude.Just ("topic" Data..= topic),
            Prelude.Just
              ("clientProperties" Data..= clientProperties)
          ]
      )
