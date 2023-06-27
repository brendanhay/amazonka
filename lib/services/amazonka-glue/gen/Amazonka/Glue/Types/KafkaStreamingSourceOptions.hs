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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.KafkaStreamingSourceOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Additional options for streaming.
--
-- /See:/ 'newKafkaStreamingSourceOptions' smart constructor.
data KafkaStreamingSourceOptions = KafkaStreamingSourceOptions'
  { -- | When this option is set to \'true\', the data output will contain an
    -- additional column named \"__src_timestamp\" that indicates the time when
    -- the corresponding record received by the topic. The default value is
    -- \'false\'. This option is supported in Glue version 4.0 or later.
    addRecordTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The specific @TopicPartitions@ to consume. You must specify at least one
    -- of @\"topicName\"@, @\"assign\"@ or @\"subscribePattern\"@.
    assign :: Prelude.Maybe Prelude.Text,
    -- | A list of bootstrap server URLs, for example, as
    -- @b-1.vpc-test-2.o4q88o.c6.kafka.us-east-1.amazonaws.com:9094@. This
    -- option must be specified in the API call or defined in the table
    -- metadata in the Data Catalog.
    bootstrapServers :: Prelude.Maybe Prelude.Text,
    -- | An optional classification.
    classification :: Prelude.Maybe Prelude.Text,
    -- | The name of the connection.
    connectionName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the delimiter character.
    delimiter :: Prelude.Maybe Prelude.Text,
    -- | When this option is set to \'true\', for each batch, it will emit the
    -- metrics for the duration between the oldest record received by the topic
    -- and the time it arrives in Glue to CloudWatch. The metric\'s name is
    -- \"glue.driver.streaming.maxConsumerLagInMs\". The default value is
    -- \'false\'. This option is supported in Glue version 4.0 or later.
    emitConsumerLagMetrics :: Prelude.Maybe Prelude.Text,
    -- | The end point when a batch query is ended. Possible values are either
    -- @\"latest\"@ or a JSON string that specifies an ending offset for each
    -- @TopicPartition@.
    endingOffsets :: Prelude.Maybe Prelude.Text,
    -- | Whether to include the Kafka headers. When the option is set to
    -- \"true\", the data output will contain an additional column named
    -- \"glue_streaming_kafka_headers\" with type
    -- @Array[Struct(key: String, value: String)]@. The default value is
    -- \"false\". This option is available in Glue version 3.0 or later only.
    includeHeaders :: Prelude.Maybe Prelude.Bool,
    -- | The rate limit on the maximum number of offsets that are processed per
    -- trigger interval. The specified total number of offsets is
    -- proportionally split across @topicPartitions@ of different volumes. The
    -- default value is null, which means that the consumer reads all offsets
    -- until the known latest offset.
    maxOffsetsPerTrigger :: Prelude.Maybe Prelude.Natural,
    -- | The desired minimum number of partitions to read from Kafka. The default
    -- value is null, which means that the number of spark partitions is equal
    -- to the number of Kafka partitions.
    minPartitions :: Prelude.Maybe Prelude.Natural,
    -- | The number of times to retry before failing to fetch Kafka offsets. The
    -- default value is @3@.
    numRetries :: Prelude.Maybe Prelude.Natural,
    -- | The timeout in milliseconds to poll data from Kafka in Spark job
    -- executors. The default value is @512@.
    pollTimeoutMs :: Prelude.Maybe Prelude.Natural,
    -- | The time in milliseconds to wait before retrying to fetch Kafka offsets.
    -- The default value is @10@.
    retryIntervalMs :: Prelude.Maybe Prelude.Natural,
    -- | The protocol used to communicate with brokers. The possible values are
    -- @\"SSL\"@ or @\"PLAINTEXT\"@.
    securityProtocol :: Prelude.Maybe Prelude.Text,
    -- | The starting position in the Kafka topic to read data from. The possible
    -- values are @\"earliest\"@ or @\"latest\"@. The default value is
    -- @\"latest\"@.
    startingOffsets :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of the record in the Kafka topic to start reading data
    -- from. The possible values are a timestamp string in UTC format of the
    -- pattern @yyyy-mm-ddTHH:MM:SSZ@ (where Z represents a UTC timezone offset
    -- with a +\/-. For example: \"2023-04-04T08:00:00+08:00\").
    --
    -- Only one of @StartingTimestamp@ or @StartingOffsets@ must be set.
    startingTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | A Java regex string that identifies the topic list to subscribe to. You
    -- must specify at least one of @\"topicName\"@, @\"assign\"@ or
    -- @\"subscribePattern\"@.
    subscribePattern :: Prelude.Maybe Prelude.Text,
    -- | The topic name as specified in Apache Kafka. You must specify at least
    -- one of @\"topicName\"@, @\"assign\"@ or @\"subscribePattern\"@.
    topicName :: Prelude.Maybe Prelude.Text
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
-- 'addRecordTimestamp', 'kafkaStreamingSourceOptions_addRecordTimestamp' - When this option is set to \'true\', the data output will contain an
-- additional column named \"__src_timestamp\" that indicates the time when
-- the corresponding record received by the topic. The default value is
-- \'false\'. This option is supported in Glue version 4.0 or later.
--
-- 'assign', 'kafkaStreamingSourceOptions_assign' - The specific @TopicPartitions@ to consume. You must specify at least one
-- of @\"topicName\"@, @\"assign\"@ or @\"subscribePattern\"@.
--
-- 'bootstrapServers', 'kafkaStreamingSourceOptions_bootstrapServers' - A list of bootstrap server URLs, for example, as
-- @b-1.vpc-test-2.o4q88o.c6.kafka.us-east-1.amazonaws.com:9094@. This
-- option must be specified in the API call or defined in the table
-- metadata in the Data Catalog.
--
-- 'classification', 'kafkaStreamingSourceOptions_classification' - An optional classification.
--
-- 'connectionName', 'kafkaStreamingSourceOptions_connectionName' - The name of the connection.
--
-- 'delimiter', 'kafkaStreamingSourceOptions_delimiter' - Specifies the delimiter character.
--
-- 'emitConsumerLagMetrics', 'kafkaStreamingSourceOptions_emitConsumerLagMetrics' - When this option is set to \'true\', for each batch, it will emit the
-- metrics for the duration between the oldest record received by the topic
-- and the time it arrives in Glue to CloudWatch. The metric\'s name is
-- \"glue.driver.streaming.maxConsumerLagInMs\". The default value is
-- \'false\'. This option is supported in Glue version 4.0 or later.
--
-- 'endingOffsets', 'kafkaStreamingSourceOptions_endingOffsets' - The end point when a batch query is ended. Possible values are either
-- @\"latest\"@ or a JSON string that specifies an ending offset for each
-- @TopicPartition@.
--
-- 'includeHeaders', 'kafkaStreamingSourceOptions_includeHeaders' - Whether to include the Kafka headers. When the option is set to
-- \"true\", the data output will contain an additional column named
-- \"glue_streaming_kafka_headers\" with type
-- @Array[Struct(key: String, value: String)]@. The default value is
-- \"false\". This option is available in Glue version 3.0 or later only.
--
-- 'maxOffsetsPerTrigger', 'kafkaStreamingSourceOptions_maxOffsetsPerTrigger' - The rate limit on the maximum number of offsets that are processed per
-- trigger interval. The specified total number of offsets is
-- proportionally split across @topicPartitions@ of different volumes. The
-- default value is null, which means that the consumer reads all offsets
-- until the known latest offset.
--
-- 'minPartitions', 'kafkaStreamingSourceOptions_minPartitions' - The desired minimum number of partitions to read from Kafka. The default
-- value is null, which means that the number of spark partitions is equal
-- to the number of Kafka partitions.
--
-- 'numRetries', 'kafkaStreamingSourceOptions_numRetries' - The number of times to retry before failing to fetch Kafka offsets. The
-- default value is @3@.
--
-- 'pollTimeoutMs', 'kafkaStreamingSourceOptions_pollTimeoutMs' - The timeout in milliseconds to poll data from Kafka in Spark job
-- executors. The default value is @512@.
--
-- 'retryIntervalMs', 'kafkaStreamingSourceOptions_retryIntervalMs' - The time in milliseconds to wait before retrying to fetch Kafka offsets.
-- The default value is @10@.
--
-- 'securityProtocol', 'kafkaStreamingSourceOptions_securityProtocol' - The protocol used to communicate with brokers. The possible values are
-- @\"SSL\"@ or @\"PLAINTEXT\"@.
--
-- 'startingOffsets', 'kafkaStreamingSourceOptions_startingOffsets' - The starting position in the Kafka topic to read data from. The possible
-- values are @\"earliest\"@ or @\"latest\"@. The default value is
-- @\"latest\"@.
--
-- 'startingTimestamp', 'kafkaStreamingSourceOptions_startingTimestamp' - The timestamp of the record in the Kafka topic to start reading data
-- from. The possible values are a timestamp string in UTC format of the
-- pattern @yyyy-mm-ddTHH:MM:SSZ@ (where Z represents a UTC timezone offset
-- with a +\/-. For example: \"2023-04-04T08:00:00+08:00\").
--
-- Only one of @StartingTimestamp@ or @StartingOffsets@ must be set.
--
-- 'subscribePattern', 'kafkaStreamingSourceOptions_subscribePattern' - A Java regex string that identifies the topic list to subscribe to. You
-- must specify at least one of @\"topicName\"@, @\"assign\"@ or
-- @\"subscribePattern\"@.
--
-- 'topicName', 'kafkaStreamingSourceOptions_topicName' - The topic name as specified in Apache Kafka. You must specify at least
-- one of @\"topicName\"@, @\"assign\"@ or @\"subscribePattern\"@.
newKafkaStreamingSourceOptions ::
  KafkaStreamingSourceOptions
newKafkaStreamingSourceOptions =
  KafkaStreamingSourceOptions'
    { addRecordTimestamp =
        Prelude.Nothing,
      assign = Prelude.Nothing,
      bootstrapServers = Prelude.Nothing,
      classification = Prelude.Nothing,
      connectionName = Prelude.Nothing,
      delimiter = Prelude.Nothing,
      emitConsumerLagMetrics = Prelude.Nothing,
      endingOffsets = Prelude.Nothing,
      includeHeaders = Prelude.Nothing,
      maxOffsetsPerTrigger = Prelude.Nothing,
      minPartitions = Prelude.Nothing,
      numRetries = Prelude.Nothing,
      pollTimeoutMs = Prelude.Nothing,
      retryIntervalMs = Prelude.Nothing,
      securityProtocol = Prelude.Nothing,
      startingOffsets = Prelude.Nothing,
      startingTimestamp = Prelude.Nothing,
      subscribePattern = Prelude.Nothing,
      topicName = Prelude.Nothing
    }

-- | When this option is set to \'true\', the data output will contain an
-- additional column named \"__src_timestamp\" that indicates the time when
-- the corresponding record received by the topic. The default value is
-- \'false\'. This option is supported in Glue version 4.0 or later.
kafkaStreamingSourceOptions_addRecordTimestamp :: Lens.Lens' KafkaStreamingSourceOptions (Prelude.Maybe Prelude.Text)
kafkaStreamingSourceOptions_addRecordTimestamp = Lens.lens (\KafkaStreamingSourceOptions' {addRecordTimestamp} -> addRecordTimestamp) (\s@KafkaStreamingSourceOptions' {} a -> s {addRecordTimestamp = a} :: KafkaStreamingSourceOptions)

-- | The specific @TopicPartitions@ to consume. You must specify at least one
-- of @\"topicName\"@, @\"assign\"@ or @\"subscribePattern\"@.
kafkaStreamingSourceOptions_assign :: Lens.Lens' KafkaStreamingSourceOptions (Prelude.Maybe Prelude.Text)
kafkaStreamingSourceOptions_assign = Lens.lens (\KafkaStreamingSourceOptions' {assign} -> assign) (\s@KafkaStreamingSourceOptions' {} a -> s {assign = a} :: KafkaStreamingSourceOptions)

-- | A list of bootstrap server URLs, for example, as
-- @b-1.vpc-test-2.o4q88o.c6.kafka.us-east-1.amazonaws.com:9094@. This
-- option must be specified in the API call or defined in the table
-- metadata in the Data Catalog.
kafkaStreamingSourceOptions_bootstrapServers :: Lens.Lens' KafkaStreamingSourceOptions (Prelude.Maybe Prelude.Text)
kafkaStreamingSourceOptions_bootstrapServers = Lens.lens (\KafkaStreamingSourceOptions' {bootstrapServers} -> bootstrapServers) (\s@KafkaStreamingSourceOptions' {} a -> s {bootstrapServers = a} :: KafkaStreamingSourceOptions)

-- | An optional classification.
kafkaStreamingSourceOptions_classification :: Lens.Lens' KafkaStreamingSourceOptions (Prelude.Maybe Prelude.Text)
kafkaStreamingSourceOptions_classification = Lens.lens (\KafkaStreamingSourceOptions' {classification} -> classification) (\s@KafkaStreamingSourceOptions' {} a -> s {classification = a} :: KafkaStreamingSourceOptions)

-- | The name of the connection.
kafkaStreamingSourceOptions_connectionName :: Lens.Lens' KafkaStreamingSourceOptions (Prelude.Maybe Prelude.Text)
kafkaStreamingSourceOptions_connectionName = Lens.lens (\KafkaStreamingSourceOptions' {connectionName} -> connectionName) (\s@KafkaStreamingSourceOptions' {} a -> s {connectionName = a} :: KafkaStreamingSourceOptions)

-- | Specifies the delimiter character.
kafkaStreamingSourceOptions_delimiter :: Lens.Lens' KafkaStreamingSourceOptions (Prelude.Maybe Prelude.Text)
kafkaStreamingSourceOptions_delimiter = Lens.lens (\KafkaStreamingSourceOptions' {delimiter} -> delimiter) (\s@KafkaStreamingSourceOptions' {} a -> s {delimiter = a} :: KafkaStreamingSourceOptions)

-- | When this option is set to \'true\', for each batch, it will emit the
-- metrics for the duration between the oldest record received by the topic
-- and the time it arrives in Glue to CloudWatch. The metric\'s name is
-- \"glue.driver.streaming.maxConsumerLagInMs\". The default value is
-- \'false\'. This option is supported in Glue version 4.0 or later.
kafkaStreamingSourceOptions_emitConsumerLagMetrics :: Lens.Lens' KafkaStreamingSourceOptions (Prelude.Maybe Prelude.Text)
kafkaStreamingSourceOptions_emitConsumerLagMetrics = Lens.lens (\KafkaStreamingSourceOptions' {emitConsumerLagMetrics} -> emitConsumerLagMetrics) (\s@KafkaStreamingSourceOptions' {} a -> s {emitConsumerLagMetrics = a} :: KafkaStreamingSourceOptions)

-- | The end point when a batch query is ended. Possible values are either
-- @\"latest\"@ or a JSON string that specifies an ending offset for each
-- @TopicPartition@.
kafkaStreamingSourceOptions_endingOffsets :: Lens.Lens' KafkaStreamingSourceOptions (Prelude.Maybe Prelude.Text)
kafkaStreamingSourceOptions_endingOffsets = Lens.lens (\KafkaStreamingSourceOptions' {endingOffsets} -> endingOffsets) (\s@KafkaStreamingSourceOptions' {} a -> s {endingOffsets = a} :: KafkaStreamingSourceOptions)

-- | Whether to include the Kafka headers. When the option is set to
-- \"true\", the data output will contain an additional column named
-- \"glue_streaming_kafka_headers\" with type
-- @Array[Struct(key: String, value: String)]@. The default value is
-- \"false\". This option is available in Glue version 3.0 or later only.
kafkaStreamingSourceOptions_includeHeaders :: Lens.Lens' KafkaStreamingSourceOptions (Prelude.Maybe Prelude.Bool)
kafkaStreamingSourceOptions_includeHeaders = Lens.lens (\KafkaStreamingSourceOptions' {includeHeaders} -> includeHeaders) (\s@KafkaStreamingSourceOptions' {} a -> s {includeHeaders = a} :: KafkaStreamingSourceOptions)

-- | The rate limit on the maximum number of offsets that are processed per
-- trigger interval. The specified total number of offsets is
-- proportionally split across @topicPartitions@ of different volumes. The
-- default value is null, which means that the consumer reads all offsets
-- until the known latest offset.
kafkaStreamingSourceOptions_maxOffsetsPerTrigger :: Lens.Lens' KafkaStreamingSourceOptions (Prelude.Maybe Prelude.Natural)
kafkaStreamingSourceOptions_maxOffsetsPerTrigger = Lens.lens (\KafkaStreamingSourceOptions' {maxOffsetsPerTrigger} -> maxOffsetsPerTrigger) (\s@KafkaStreamingSourceOptions' {} a -> s {maxOffsetsPerTrigger = a} :: KafkaStreamingSourceOptions)

-- | The desired minimum number of partitions to read from Kafka. The default
-- value is null, which means that the number of spark partitions is equal
-- to the number of Kafka partitions.
kafkaStreamingSourceOptions_minPartitions :: Lens.Lens' KafkaStreamingSourceOptions (Prelude.Maybe Prelude.Natural)
kafkaStreamingSourceOptions_minPartitions = Lens.lens (\KafkaStreamingSourceOptions' {minPartitions} -> minPartitions) (\s@KafkaStreamingSourceOptions' {} a -> s {minPartitions = a} :: KafkaStreamingSourceOptions)

-- | The number of times to retry before failing to fetch Kafka offsets. The
-- default value is @3@.
kafkaStreamingSourceOptions_numRetries :: Lens.Lens' KafkaStreamingSourceOptions (Prelude.Maybe Prelude.Natural)
kafkaStreamingSourceOptions_numRetries = Lens.lens (\KafkaStreamingSourceOptions' {numRetries} -> numRetries) (\s@KafkaStreamingSourceOptions' {} a -> s {numRetries = a} :: KafkaStreamingSourceOptions)

-- | The timeout in milliseconds to poll data from Kafka in Spark job
-- executors. The default value is @512@.
kafkaStreamingSourceOptions_pollTimeoutMs :: Lens.Lens' KafkaStreamingSourceOptions (Prelude.Maybe Prelude.Natural)
kafkaStreamingSourceOptions_pollTimeoutMs = Lens.lens (\KafkaStreamingSourceOptions' {pollTimeoutMs} -> pollTimeoutMs) (\s@KafkaStreamingSourceOptions' {} a -> s {pollTimeoutMs = a} :: KafkaStreamingSourceOptions)

-- | The time in milliseconds to wait before retrying to fetch Kafka offsets.
-- The default value is @10@.
kafkaStreamingSourceOptions_retryIntervalMs :: Lens.Lens' KafkaStreamingSourceOptions (Prelude.Maybe Prelude.Natural)
kafkaStreamingSourceOptions_retryIntervalMs = Lens.lens (\KafkaStreamingSourceOptions' {retryIntervalMs} -> retryIntervalMs) (\s@KafkaStreamingSourceOptions' {} a -> s {retryIntervalMs = a} :: KafkaStreamingSourceOptions)

-- | The protocol used to communicate with brokers. The possible values are
-- @\"SSL\"@ or @\"PLAINTEXT\"@.
kafkaStreamingSourceOptions_securityProtocol :: Lens.Lens' KafkaStreamingSourceOptions (Prelude.Maybe Prelude.Text)
kafkaStreamingSourceOptions_securityProtocol = Lens.lens (\KafkaStreamingSourceOptions' {securityProtocol} -> securityProtocol) (\s@KafkaStreamingSourceOptions' {} a -> s {securityProtocol = a} :: KafkaStreamingSourceOptions)

-- | The starting position in the Kafka topic to read data from. The possible
-- values are @\"earliest\"@ or @\"latest\"@. The default value is
-- @\"latest\"@.
kafkaStreamingSourceOptions_startingOffsets :: Lens.Lens' KafkaStreamingSourceOptions (Prelude.Maybe Prelude.Text)
kafkaStreamingSourceOptions_startingOffsets = Lens.lens (\KafkaStreamingSourceOptions' {startingOffsets} -> startingOffsets) (\s@KafkaStreamingSourceOptions' {} a -> s {startingOffsets = a} :: KafkaStreamingSourceOptions)

-- | The timestamp of the record in the Kafka topic to start reading data
-- from. The possible values are a timestamp string in UTC format of the
-- pattern @yyyy-mm-ddTHH:MM:SSZ@ (where Z represents a UTC timezone offset
-- with a +\/-. For example: \"2023-04-04T08:00:00+08:00\").
--
-- Only one of @StartingTimestamp@ or @StartingOffsets@ must be set.
kafkaStreamingSourceOptions_startingTimestamp :: Lens.Lens' KafkaStreamingSourceOptions (Prelude.Maybe Prelude.UTCTime)
kafkaStreamingSourceOptions_startingTimestamp = Lens.lens (\KafkaStreamingSourceOptions' {startingTimestamp} -> startingTimestamp) (\s@KafkaStreamingSourceOptions' {} a -> s {startingTimestamp = a} :: KafkaStreamingSourceOptions) Prelude.. Lens.mapping Data._Time

-- | A Java regex string that identifies the topic list to subscribe to. You
-- must specify at least one of @\"topicName\"@, @\"assign\"@ or
-- @\"subscribePattern\"@.
kafkaStreamingSourceOptions_subscribePattern :: Lens.Lens' KafkaStreamingSourceOptions (Prelude.Maybe Prelude.Text)
kafkaStreamingSourceOptions_subscribePattern = Lens.lens (\KafkaStreamingSourceOptions' {subscribePattern} -> subscribePattern) (\s@KafkaStreamingSourceOptions' {} a -> s {subscribePattern = a} :: KafkaStreamingSourceOptions)

-- | The topic name as specified in Apache Kafka. You must specify at least
-- one of @\"topicName\"@, @\"assign\"@ or @\"subscribePattern\"@.
kafkaStreamingSourceOptions_topicName :: Lens.Lens' KafkaStreamingSourceOptions (Prelude.Maybe Prelude.Text)
kafkaStreamingSourceOptions_topicName = Lens.lens (\KafkaStreamingSourceOptions' {topicName} -> topicName) (\s@KafkaStreamingSourceOptions' {} a -> s {topicName = a} :: KafkaStreamingSourceOptions)

instance Data.FromJSON KafkaStreamingSourceOptions where
  parseJSON =
    Data.withObject
      "KafkaStreamingSourceOptions"
      ( \x ->
          KafkaStreamingSourceOptions'
            Prelude.<$> (x Data..:? "AddRecordTimestamp")
            Prelude.<*> (x Data..:? "Assign")
            Prelude.<*> (x Data..:? "BootstrapServers")
            Prelude.<*> (x Data..:? "Classification")
            Prelude.<*> (x Data..:? "ConnectionName")
            Prelude.<*> (x Data..:? "Delimiter")
            Prelude.<*> (x Data..:? "EmitConsumerLagMetrics")
            Prelude.<*> (x Data..:? "EndingOffsets")
            Prelude.<*> (x Data..:? "IncludeHeaders")
            Prelude.<*> (x Data..:? "MaxOffsetsPerTrigger")
            Prelude.<*> (x Data..:? "MinPartitions")
            Prelude.<*> (x Data..:? "NumRetries")
            Prelude.<*> (x Data..:? "PollTimeoutMs")
            Prelude.<*> (x Data..:? "RetryIntervalMs")
            Prelude.<*> (x Data..:? "SecurityProtocol")
            Prelude.<*> (x Data..:? "StartingOffsets")
            Prelude.<*> (x Data..:? "StartingTimestamp")
            Prelude.<*> (x Data..:? "SubscribePattern")
            Prelude.<*> (x Data..:? "TopicName")
      )

instance Prelude.Hashable KafkaStreamingSourceOptions where
  hashWithSalt _salt KafkaStreamingSourceOptions' {..} =
    _salt
      `Prelude.hashWithSalt` addRecordTimestamp
      `Prelude.hashWithSalt` assign
      `Prelude.hashWithSalt` bootstrapServers
      `Prelude.hashWithSalt` classification
      `Prelude.hashWithSalt` connectionName
      `Prelude.hashWithSalt` delimiter
      `Prelude.hashWithSalt` emitConsumerLagMetrics
      `Prelude.hashWithSalt` endingOffsets
      `Prelude.hashWithSalt` includeHeaders
      `Prelude.hashWithSalt` maxOffsetsPerTrigger
      `Prelude.hashWithSalt` minPartitions
      `Prelude.hashWithSalt` numRetries
      `Prelude.hashWithSalt` pollTimeoutMs
      `Prelude.hashWithSalt` retryIntervalMs
      `Prelude.hashWithSalt` securityProtocol
      `Prelude.hashWithSalt` startingOffsets
      `Prelude.hashWithSalt` startingTimestamp
      `Prelude.hashWithSalt` subscribePattern
      `Prelude.hashWithSalt` topicName

instance Prelude.NFData KafkaStreamingSourceOptions where
  rnf KafkaStreamingSourceOptions' {..} =
    Prelude.rnf addRecordTimestamp
      `Prelude.seq` Prelude.rnf assign
      `Prelude.seq` Prelude.rnf bootstrapServers
      `Prelude.seq` Prelude.rnf classification
      `Prelude.seq` Prelude.rnf connectionName
      `Prelude.seq` Prelude.rnf delimiter
      `Prelude.seq` Prelude.rnf emitConsumerLagMetrics
      `Prelude.seq` Prelude.rnf endingOffsets
      `Prelude.seq` Prelude.rnf includeHeaders
      `Prelude.seq` Prelude.rnf maxOffsetsPerTrigger
      `Prelude.seq` Prelude.rnf minPartitions
      `Prelude.seq` Prelude.rnf numRetries
      `Prelude.seq` Prelude.rnf pollTimeoutMs
      `Prelude.seq` Prelude.rnf retryIntervalMs
      `Prelude.seq` Prelude.rnf securityProtocol
      `Prelude.seq` Prelude.rnf startingOffsets
      `Prelude.seq` Prelude.rnf startingTimestamp
      `Prelude.seq` Prelude.rnf subscribePattern
      `Prelude.seq` Prelude.rnf topicName

instance Data.ToJSON KafkaStreamingSourceOptions where
  toJSON KafkaStreamingSourceOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AddRecordTimestamp" Data..=)
              Prelude.<$> addRecordTimestamp,
            ("Assign" Data..=) Prelude.<$> assign,
            ("BootstrapServers" Data..=)
              Prelude.<$> bootstrapServers,
            ("Classification" Data..=)
              Prelude.<$> classification,
            ("ConnectionName" Data..=)
              Prelude.<$> connectionName,
            ("Delimiter" Data..=) Prelude.<$> delimiter,
            ("EmitConsumerLagMetrics" Data..=)
              Prelude.<$> emitConsumerLagMetrics,
            ("EndingOffsets" Data..=) Prelude.<$> endingOffsets,
            ("IncludeHeaders" Data..=)
              Prelude.<$> includeHeaders,
            ("MaxOffsetsPerTrigger" Data..=)
              Prelude.<$> maxOffsetsPerTrigger,
            ("MinPartitions" Data..=) Prelude.<$> minPartitions,
            ("NumRetries" Data..=) Prelude.<$> numRetries,
            ("PollTimeoutMs" Data..=) Prelude.<$> pollTimeoutMs,
            ("RetryIntervalMs" Data..=)
              Prelude.<$> retryIntervalMs,
            ("SecurityProtocol" Data..=)
              Prelude.<$> securityProtocol,
            ("StartingOffsets" Data..=)
              Prelude.<$> startingOffsets,
            ("StartingTimestamp" Data..=)
              Prelude.<$> startingTimestamp,
            ("SubscribePattern" Data..=)
              Prelude.<$> subscribePattern,
            ("TopicName" Data..=) Prelude.<$> topicName
          ]
      )
