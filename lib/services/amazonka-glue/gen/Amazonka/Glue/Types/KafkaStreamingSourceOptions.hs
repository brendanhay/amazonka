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
-- Module      : Amazonka.Glue.Types.KafkaStreamingSourceOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.KafkaStreamingSourceOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Additional options for streaming.
--
-- /See:/ 'newKafkaStreamingSourceOptions' smart constructor.
data KafkaStreamingSourceOptions = KafkaStreamingSourceOptions'
  { -- | The rate limit on the maximum number of offsets that are processed per
    -- trigger interval. The specified total number of offsets is
    -- proportionally split across @topicPartitions@ of different volumes. The
    -- default value is null, which means that the consumer reads all offsets
    -- until the known latest offset.
    maxOffsetsPerTrigger :: Prelude.Maybe Prelude.Natural,
    -- | The number of times to retry before failing to fetch Kafka offsets. The
    -- default value is @3@.
    numRetries :: Prelude.Maybe Prelude.Natural,
    -- | A Java regex string that identifies the topic list to subscribe to. You
    -- must specify at least one of @\"topicName\"@, @\"assign\"@ or
    -- @\"subscribePattern\"@.
    subscribePattern :: Prelude.Maybe Prelude.Text,
    -- | The time in milliseconds to wait before retrying to fetch Kafka offsets.
    -- The default value is @10@.
    retryIntervalMs :: Prelude.Maybe Prelude.Natural,
    -- | The end point when a batch query is ended. Possible values are either
    -- @\"latest\"@ or a JSON string that specifies an ending offset for each
    -- @TopicPartition@.
    endingOffsets :: Prelude.Maybe Prelude.Text,
    -- | The desired minimum number of partitions to read from Kafka. The default
    -- value is null, which means that the number of spark partitions is equal
    -- to the number of Kafka partitions.
    minPartitions :: Prelude.Maybe Prelude.Natural,
    -- | The starting position in the Kafka topic to read data from. The possible
    -- values are @\"earliest\"@ or @\"latest\"@. The default value is
    -- @\"latest\"@.
    startingOffsets :: Prelude.Maybe Prelude.Text,
    -- | Specifies the delimiter character.
    delimiter :: Prelude.Maybe Prelude.Text,
    -- | The topic name as specified in Apache Kafka. You must specify at least
    -- one of @\"topicName\"@, @\"assign\"@ or @\"subscribePattern\"@.
    topicName :: Prelude.Maybe Prelude.Text,
    -- | The protocol used to communicate with brokers. The possible values are
    -- @\"SSL\"@ or @\"PLAINTEXT\"@.
    securityProtocol :: Prelude.Maybe Prelude.Text,
    -- | A list of bootstrap server URLs, for example, as
    -- @b-1.vpc-test-2.o4q88o.c6.kafka.us-east-1.amazonaws.com:9094@. This
    -- option must be specified in the API call or defined in the table
    -- metadata in the Data Catalog.
    bootstrapServers :: Prelude.Maybe Prelude.Text,
    -- | The timeout in milliseconds to poll data from Kafka in Spark job
    -- executors. The default value is @512@.
    pollTimeoutMs :: Prelude.Maybe Prelude.Natural,
    -- | The name of the connection.
    connectionName :: Prelude.Maybe Prelude.Text,
    -- | The specific @TopicPartitions@ to consume. You must specify at least one
    -- of @\"topicName\"@, @\"assign\"@ or @\"subscribePattern\"@.
    assign :: Prelude.Maybe Prelude.Text,
    -- | An optional classification.
    classification :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KafkaStreamingSourceOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxOffsetsPerTrigger', 'kafkaStreamingSourceOptions_maxOffsetsPerTrigger' - The rate limit on the maximum number of offsets that are processed per
-- trigger interval. The specified total number of offsets is
-- proportionally split across @topicPartitions@ of different volumes. The
-- default value is null, which means that the consumer reads all offsets
-- until the known latest offset.
--
-- 'numRetries', 'kafkaStreamingSourceOptions_numRetries' - The number of times to retry before failing to fetch Kafka offsets. The
-- default value is @3@.
--
-- 'subscribePattern', 'kafkaStreamingSourceOptions_subscribePattern' - A Java regex string that identifies the topic list to subscribe to. You
-- must specify at least one of @\"topicName\"@, @\"assign\"@ or
-- @\"subscribePattern\"@.
--
-- 'retryIntervalMs', 'kafkaStreamingSourceOptions_retryIntervalMs' - The time in milliseconds to wait before retrying to fetch Kafka offsets.
-- The default value is @10@.
--
-- 'endingOffsets', 'kafkaStreamingSourceOptions_endingOffsets' - The end point when a batch query is ended. Possible values are either
-- @\"latest\"@ or a JSON string that specifies an ending offset for each
-- @TopicPartition@.
--
-- 'minPartitions', 'kafkaStreamingSourceOptions_minPartitions' - The desired minimum number of partitions to read from Kafka. The default
-- value is null, which means that the number of spark partitions is equal
-- to the number of Kafka partitions.
--
-- 'startingOffsets', 'kafkaStreamingSourceOptions_startingOffsets' - The starting position in the Kafka topic to read data from. The possible
-- values are @\"earliest\"@ or @\"latest\"@. The default value is
-- @\"latest\"@.
--
-- 'delimiter', 'kafkaStreamingSourceOptions_delimiter' - Specifies the delimiter character.
--
-- 'topicName', 'kafkaStreamingSourceOptions_topicName' - The topic name as specified in Apache Kafka. You must specify at least
-- one of @\"topicName\"@, @\"assign\"@ or @\"subscribePattern\"@.
--
-- 'securityProtocol', 'kafkaStreamingSourceOptions_securityProtocol' - The protocol used to communicate with brokers. The possible values are
-- @\"SSL\"@ or @\"PLAINTEXT\"@.
--
-- 'bootstrapServers', 'kafkaStreamingSourceOptions_bootstrapServers' - A list of bootstrap server URLs, for example, as
-- @b-1.vpc-test-2.o4q88o.c6.kafka.us-east-1.amazonaws.com:9094@. This
-- option must be specified in the API call or defined in the table
-- metadata in the Data Catalog.
--
-- 'pollTimeoutMs', 'kafkaStreamingSourceOptions_pollTimeoutMs' - The timeout in milliseconds to poll data from Kafka in Spark job
-- executors. The default value is @512@.
--
-- 'connectionName', 'kafkaStreamingSourceOptions_connectionName' - The name of the connection.
--
-- 'assign', 'kafkaStreamingSourceOptions_assign' - The specific @TopicPartitions@ to consume. You must specify at least one
-- of @\"topicName\"@, @\"assign\"@ or @\"subscribePattern\"@.
--
-- 'classification', 'kafkaStreamingSourceOptions_classification' - An optional classification.
newKafkaStreamingSourceOptions ::
  KafkaStreamingSourceOptions
newKafkaStreamingSourceOptions =
  KafkaStreamingSourceOptions'
    { maxOffsetsPerTrigger =
        Prelude.Nothing,
      numRetries = Prelude.Nothing,
      subscribePattern = Prelude.Nothing,
      retryIntervalMs = Prelude.Nothing,
      endingOffsets = Prelude.Nothing,
      minPartitions = Prelude.Nothing,
      startingOffsets = Prelude.Nothing,
      delimiter = Prelude.Nothing,
      topicName = Prelude.Nothing,
      securityProtocol = Prelude.Nothing,
      bootstrapServers = Prelude.Nothing,
      pollTimeoutMs = Prelude.Nothing,
      connectionName = Prelude.Nothing,
      assign = Prelude.Nothing,
      classification = Prelude.Nothing
    }

-- | The rate limit on the maximum number of offsets that are processed per
-- trigger interval. The specified total number of offsets is
-- proportionally split across @topicPartitions@ of different volumes. The
-- default value is null, which means that the consumer reads all offsets
-- until the known latest offset.
kafkaStreamingSourceOptions_maxOffsetsPerTrigger :: Lens.Lens' KafkaStreamingSourceOptions (Prelude.Maybe Prelude.Natural)
kafkaStreamingSourceOptions_maxOffsetsPerTrigger = Lens.lens (\KafkaStreamingSourceOptions' {maxOffsetsPerTrigger} -> maxOffsetsPerTrigger) (\s@KafkaStreamingSourceOptions' {} a -> s {maxOffsetsPerTrigger = a} :: KafkaStreamingSourceOptions)

-- | The number of times to retry before failing to fetch Kafka offsets. The
-- default value is @3@.
kafkaStreamingSourceOptions_numRetries :: Lens.Lens' KafkaStreamingSourceOptions (Prelude.Maybe Prelude.Natural)
kafkaStreamingSourceOptions_numRetries = Lens.lens (\KafkaStreamingSourceOptions' {numRetries} -> numRetries) (\s@KafkaStreamingSourceOptions' {} a -> s {numRetries = a} :: KafkaStreamingSourceOptions)

-- | A Java regex string that identifies the topic list to subscribe to. You
-- must specify at least one of @\"topicName\"@, @\"assign\"@ or
-- @\"subscribePattern\"@.
kafkaStreamingSourceOptions_subscribePattern :: Lens.Lens' KafkaStreamingSourceOptions (Prelude.Maybe Prelude.Text)
kafkaStreamingSourceOptions_subscribePattern = Lens.lens (\KafkaStreamingSourceOptions' {subscribePattern} -> subscribePattern) (\s@KafkaStreamingSourceOptions' {} a -> s {subscribePattern = a} :: KafkaStreamingSourceOptions)

-- | The time in milliseconds to wait before retrying to fetch Kafka offsets.
-- The default value is @10@.
kafkaStreamingSourceOptions_retryIntervalMs :: Lens.Lens' KafkaStreamingSourceOptions (Prelude.Maybe Prelude.Natural)
kafkaStreamingSourceOptions_retryIntervalMs = Lens.lens (\KafkaStreamingSourceOptions' {retryIntervalMs} -> retryIntervalMs) (\s@KafkaStreamingSourceOptions' {} a -> s {retryIntervalMs = a} :: KafkaStreamingSourceOptions)

-- | The end point when a batch query is ended. Possible values are either
-- @\"latest\"@ or a JSON string that specifies an ending offset for each
-- @TopicPartition@.
kafkaStreamingSourceOptions_endingOffsets :: Lens.Lens' KafkaStreamingSourceOptions (Prelude.Maybe Prelude.Text)
kafkaStreamingSourceOptions_endingOffsets = Lens.lens (\KafkaStreamingSourceOptions' {endingOffsets} -> endingOffsets) (\s@KafkaStreamingSourceOptions' {} a -> s {endingOffsets = a} :: KafkaStreamingSourceOptions)

-- | The desired minimum number of partitions to read from Kafka. The default
-- value is null, which means that the number of spark partitions is equal
-- to the number of Kafka partitions.
kafkaStreamingSourceOptions_minPartitions :: Lens.Lens' KafkaStreamingSourceOptions (Prelude.Maybe Prelude.Natural)
kafkaStreamingSourceOptions_minPartitions = Lens.lens (\KafkaStreamingSourceOptions' {minPartitions} -> minPartitions) (\s@KafkaStreamingSourceOptions' {} a -> s {minPartitions = a} :: KafkaStreamingSourceOptions)

-- | The starting position in the Kafka topic to read data from. The possible
-- values are @\"earliest\"@ or @\"latest\"@. The default value is
-- @\"latest\"@.
kafkaStreamingSourceOptions_startingOffsets :: Lens.Lens' KafkaStreamingSourceOptions (Prelude.Maybe Prelude.Text)
kafkaStreamingSourceOptions_startingOffsets = Lens.lens (\KafkaStreamingSourceOptions' {startingOffsets} -> startingOffsets) (\s@KafkaStreamingSourceOptions' {} a -> s {startingOffsets = a} :: KafkaStreamingSourceOptions)

-- | Specifies the delimiter character.
kafkaStreamingSourceOptions_delimiter :: Lens.Lens' KafkaStreamingSourceOptions (Prelude.Maybe Prelude.Text)
kafkaStreamingSourceOptions_delimiter = Lens.lens (\KafkaStreamingSourceOptions' {delimiter} -> delimiter) (\s@KafkaStreamingSourceOptions' {} a -> s {delimiter = a} :: KafkaStreamingSourceOptions)

-- | The topic name as specified in Apache Kafka. You must specify at least
-- one of @\"topicName\"@, @\"assign\"@ or @\"subscribePattern\"@.
kafkaStreamingSourceOptions_topicName :: Lens.Lens' KafkaStreamingSourceOptions (Prelude.Maybe Prelude.Text)
kafkaStreamingSourceOptions_topicName = Lens.lens (\KafkaStreamingSourceOptions' {topicName} -> topicName) (\s@KafkaStreamingSourceOptions' {} a -> s {topicName = a} :: KafkaStreamingSourceOptions)

-- | The protocol used to communicate with brokers. The possible values are
-- @\"SSL\"@ or @\"PLAINTEXT\"@.
kafkaStreamingSourceOptions_securityProtocol :: Lens.Lens' KafkaStreamingSourceOptions (Prelude.Maybe Prelude.Text)
kafkaStreamingSourceOptions_securityProtocol = Lens.lens (\KafkaStreamingSourceOptions' {securityProtocol} -> securityProtocol) (\s@KafkaStreamingSourceOptions' {} a -> s {securityProtocol = a} :: KafkaStreamingSourceOptions)

-- | A list of bootstrap server URLs, for example, as
-- @b-1.vpc-test-2.o4q88o.c6.kafka.us-east-1.amazonaws.com:9094@. This
-- option must be specified in the API call or defined in the table
-- metadata in the Data Catalog.
kafkaStreamingSourceOptions_bootstrapServers :: Lens.Lens' KafkaStreamingSourceOptions (Prelude.Maybe Prelude.Text)
kafkaStreamingSourceOptions_bootstrapServers = Lens.lens (\KafkaStreamingSourceOptions' {bootstrapServers} -> bootstrapServers) (\s@KafkaStreamingSourceOptions' {} a -> s {bootstrapServers = a} :: KafkaStreamingSourceOptions)

-- | The timeout in milliseconds to poll data from Kafka in Spark job
-- executors. The default value is @512@.
kafkaStreamingSourceOptions_pollTimeoutMs :: Lens.Lens' KafkaStreamingSourceOptions (Prelude.Maybe Prelude.Natural)
kafkaStreamingSourceOptions_pollTimeoutMs = Lens.lens (\KafkaStreamingSourceOptions' {pollTimeoutMs} -> pollTimeoutMs) (\s@KafkaStreamingSourceOptions' {} a -> s {pollTimeoutMs = a} :: KafkaStreamingSourceOptions)

-- | The name of the connection.
kafkaStreamingSourceOptions_connectionName :: Lens.Lens' KafkaStreamingSourceOptions (Prelude.Maybe Prelude.Text)
kafkaStreamingSourceOptions_connectionName = Lens.lens (\KafkaStreamingSourceOptions' {connectionName} -> connectionName) (\s@KafkaStreamingSourceOptions' {} a -> s {connectionName = a} :: KafkaStreamingSourceOptions)

-- | The specific @TopicPartitions@ to consume. You must specify at least one
-- of @\"topicName\"@, @\"assign\"@ or @\"subscribePattern\"@.
kafkaStreamingSourceOptions_assign :: Lens.Lens' KafkaStreamingSourceOptions (Prelude.Maybe Prelude.Text)
kafkaStreamingSourceOptions_assign = Lens.lens (\KafkaStreamingSourceOptions' {assign} -> assign) (\s@KafkaStreamingSourceOptions' {} a -> s {assign = a} :: KafkaStreamingSourceOptions)

-- | An optional classification.
kafkaStreamingSourceOptions_classification :: Lens.Lens' KafkaStreamingSourceOptions (Prelude.Maybe Prelude.Text)
kafkaStreamingSourceOptions_classification = Lens.lens (\KafkaStreamingSourceOptions' {classification} -> classification) (\s@KafkaStreamingSourceOptions' {} a -> s {classification = a} :: KafkaStreamingSourceOptions)

instance Core.FromJSON KafkaStreamingSourceOptions where
  parseJSON =
    Core.withObject
      "KafkaStreamingSourceOptions"
      ( \x ->
          KafkaStreamingSourceOptions'
            Prelude.<$> (x Core..:? "MaxOffsetsPerTrigger")
            Prelude.<*> (x Core..:? "NumRetries")
            Prelude.<*> (x Core..:? "SubscribePattern")
            Prelude.<*> (x Core..:? "RetryIntervalMs")
            Prelude.<*> (x Core..:? "EndingOffsets")
            Prelude.<*> (x Core..:? "MinPartitions")
            Prelude.<*> (x Core..:? "StartingOffsets")
            Prelude.<*> (x Core..:? "Delimiter")
            Prelude.<*> (x Core..:? "TopicName")
            Prelude.<*> (x Core..:? "SecurityProtocol")
            Prelude.<*> (x Core..:? "BootstrapServers")
            Prelude.<*> (x Core..:? "PollTimeoutMs")
            Prelude.<*> (x Core..:? "ConnectionName")
            Prelude.<*> (x Core..:? "Assign")
            Prelude.<*> (x Core..:? "Classification")
      )

instance Prelude.Hashable KafkaStreamingSourceOptions where
  hashWithSalt _salt KafkaStreamingSourceOptions' {..} =
    _salt `Prelude.hashWithSalt` maxOffsetsPerTrigger
      `Prelude.hashWithSalt` numRetries
      `Prelude.hashWithSalt` subscribePattern
      `Prelude.hashWithSalt` retryIntervalMs
      `Prelude.hashWithSalt` endingOffsets
      `Prelude.hashWithSalt` minPartitions
      `Prelude.hashWithSalt` startingOffsets
      `Prelude.hashWithSalt` delimiter
      `Prelude.hashWithSalt` topicName
      `Prelude.hashWithSalt` securityProtocol
      `Prelude.hashWithSalt` bootstrapServers
      `Prelude.hashWithSalt` pollTimeoutMs
      `Prelude.hashWithSalt` connectionName
      `Prelude.hashWithSalt` assign
      `Prelude.hashWithSalt` classification

instance Prelude.NFData KafkaStreamingSourceOptions where
  rnf KafkaStreamingSourceOptions' {..} =
    Prelude.rnf maxOffsetsPerTrigger
      `Prelude.seq` Prelude.rnf numRetries
      `Prelude.seq` Prelude.rnf subscribePattern
      `Prelude.seq` Prelude.rnf retryIntervalMs
      `Prelude.seq` Prelude.rnf endingOffsets
      `Prelude.seq` Prelude.rnf minPartitions
      `Prelude.seq` Prelude.rnf startingOffsets
      `Prelude.seq` Prelude.rnf delimiter
      `Prelude.seq` Prelude.rnf topicName
      `Prelude.seq` Prelude.rnf securityProtocol
      `Prelude.seq` Prelude.rnf bootstrapServers
      `Prelude.seq` Prelude.rnf pollTimeoutMs
      `Prelude.seq` Prelude.rnf connectionName
      `Prelude.seq` Prelude.rnf assign
      `Prelude.seq` Prelude.rnf classification

instance Core.ToJSON KafkaStreamingSourceOptions where
  toJSON KafkaStreamingSourceOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MaxOffsetsPerTrigger" Core..=)
              Prelude.<$> maxOffsetsPerTrigger,
            ("NumRetries" Core..=) Prelude.<$> numRetries,
            ("SubscribePattern" Core..=)
              Prelude.<$> subscribePattern,
            ("RetryIntervalMs" Core..=)
              Prelude.<$> retryIntervalMs,
            ("EndingOffsets" Core..=) Prelude.<$> endingOffsets,
            ("MinPartitions" Core..=) Prelude.<$> minPartitions,
            ("StartingOffsets" Core..=)
              Prelude.<$> startingOffsets,
            ("Delimiter" Core..=) Prelude.<$> delimiter,
            ("TopicName" Core..=) Prelude.<$> topicName,
            ("SecurityProtocol" Core..=)
              Prelude.<$> securityProtocol,
            ("BootstrapServers" Core..=)
              Prelude.<$> bootstrapServers,
            ("PollTimeoutMs" Core..=) Prelude.<$> pollTimeoutMs,
            ("ConnectionName" Core..=)
              Prelude.<$> connectionName,
            ("Assign" Core..=) Prelude.<$> assign,
            ("Classification" Core..=)
              Prelude.<$> classification
          ]
      )
