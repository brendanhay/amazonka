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
-- Module      : Amazonka.Glue.Types.KinesisStreamingSourceOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.KinesisStreamingSourceOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.StartingPosition
import qualified Amazonka.Prelude as Prelude

-- | Additional options for the Amazon Kinesis streaming data source.
--
-- /See:/ 'newKinesisStreamingSourceOptions' smart constructor.
data KinesisStreamingSourceOptions = KinesisStreamingSourceOptions'
  { -- | Adds a time delay between two consecutive getRecords operations. The
    -- default value is @\"False\"@. This option is only configurable for Glue
    -- version 2.0 and above.
    addIdleTimeBetweenReads :: Prelude.Maybe Prelude.Bool,
    -- | When this option is set to \'true\', the data output will contain an
    -- additional column named \"__src_timestamp\" that indicates the time when
    -- the corresponding record received by the stream. The default value is
    -- \'false\'. This option is supported in Glue version 4.0 or later.
    addRecordTimestamp :: Prelude.Maybe Prelude.Text,
    -- | Avoids creating an empty microbatch job by checking for unread data in
    -- the Kinesis data stream before the batch is started. The default value
    -- is @\"False\"@.
    avoidEmptyBatches :: Prelude.Maybe Prelude.Bool,
    -- | An optional classification.
    classification :: Prelude.Maybe Prelude.Text,
    -- | Specifies the delimiter character.
    delimiter :: Prelude.Maybe Prelude.Text,
    -- | The minimum time interval between two ListShards API calls for your
    -- script to consider resharding. The default value is @1s@.
    describeShardInterval :: Prelude.Maybe Prelude.Natural,
    -- | When this option is set to \'true\', for each batch, it will emit the
    -- metrics for the duration between the oldest record received by the
    -- stream and the time it arrives in Glue to CloudWatch. The metric\'s name
    -- is \"glue.driver.streaming.maxConsumerLagInMs\". The default value is
    -- \'false\'. This option is supported in Glue version 4.0 or later.
    emitConsumerLagMetrics :: Prelude.Maybe Prelude.Text,
    -- | The URL of the Kinesis endpoint.
    endpointUrl :: Prelude.Maybe Prelude.Text,
    -- | The minimum time delay between two consecutive getRecords operations,
    -- specified in ms. The default value is @1000@. This option is only
    -- configurable for Glue version 2.0 and above.
    idleTimeBetweenReadsInMs :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of records to fetch per shard in the Kinesis data
    -- stream. The default value is @100000@.
    maxFetchRecordsPerShard :: Prelude.Maybe Prelude.Natural,
    -- | The maximum time spent in the job executor to fetch a record from the
    -- Kinesis data stream per shard, specified in milliseconds (ms). The
    -- default value is @1000@.
    maxFetchTimeInMs :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of records to fetch from the Kinesis data stream in
    -- each getRecords operation. The default value is @10000@.
    maxRecordPerRead :: Prelude.Maybe Prelude.Natural,
    -- | The maximum cool-off time period (specified in ms) between two retries
    -- of a Kinesis Data Streams API call. The default value is @10000@.
    maxRetryIntervalMs :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of retries for Kinesis Data Streams API requests. The
    -- default value is @3@.
    numRetries :: Prelude.Maybe Prelude.Natural,
    -- | The cool-off time period (specified in ms) before retrying the Kinesis
    -- Data Streams API call. The default value is @1000@.
    retryIntervalMs :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the role to assume using AWS Security
    -- Token Service (AWS STS). This role must have permissions for describe or
    -- read record operations for the Kinesis data stream. You must use this
    -- parameter when accessing a data stream in a different account. Used in
    -- conjunction with @\"awsSTSSessionName\"@.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | An identifier for the session assuming the role using AWS STS. You must
    -- use this parameter when accessing a data stream in a different account.
    -- Used in conjunction with @\"awsSTSRoleARN\"@.
    roleSessionName :: Prelude.Maybe Prelude.Text,
    -- | The starting position in the Kinesis data stream to read data from. The
    -- possible values are @\"latest\"@, @\"trim_horizon\"@, @\"earliest\"@, or
    -- a timestamp string in UTC format in the pattern @yyyy-mm-ddTHH:MM:SSZ@
    -- (where @Z@ represents a UTC timezone offset with a +\/-. For example:
    -- \"2023-04-04T08:00:00-04:00\"). The default value is @\"latest\"@.
    --
    -- Note: Using a value that is a timestamp string in UTC format for
    -- \"startingPosition\" is supported only for Glue version 4.0 or later.
    startingPosition :: Prelude.Maybe StartingPosition,
    -- | The timestamp of the record in the Kinesis data stream to start reading
    -- data from. The possible values are a timestamp string in UTC format of
    -- the pattern @yyyy-mm-ddTHH:MM:SSZ@ (where Z represents a UTC timezone
    -- offset with a +\/-. For example: \"2023-04-04T08:00:00+08:00\").
    startingTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | The Amazon Resource Name (ARN) of the Kinesis data stream.
    streamArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the Kinesis data stream.
    streamName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KinesisStreamingSourceOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addIdleTimeBetweenReads', 'kinesisStreamingSourceOptions_addIdleTimeBetweenReads' - Adds a time delay between two consecutive getRecords operations. The
-- default value is @\"False\"@. This option is only configurable for Glue
-- version 2.0 and above.
--
-- 'addRecordTimestamp', 'kinesisStreamingSourceOptions_addRecordTimestamp' - When this option is set to \'true\', the data output will contain an
-- additional column named \"__src_timestamp\" that indicates the time when
-- the corresponding record received by the stream. The default value is
-- \'false\'. This option is supported in Glue version 4.0 or later.
--
-- 'avoidEmptyBatches', 'kinesisStreamingSourceOptions_avoidEmptyBatches' - Avoids creating an empty microbatch job by checking for unread data in
-- the Kinesis data stream before the batch is started. The default value
-- is @\"False\"@.
--
-- 'classification', 'kinesisStreamingSourceOptions_classification' - An optional classification.
--
-- 'delimiter', 'kinesisStreamingSourceOptions_delimiter' - Specifies the delimiter character.
--
-- 'describeShardInterval', 'kinesisStreamingSourceOptions_describeShardInterval' - The minimum time interval between two ListShards API calls for your
-- script to consider resharding. The default value is @1s@.
--
-- 'emitConsumerLagMetrics', 'kinesisStreamingSourceOptions_emitConsumerLagMetrics' - When this option is set to \'true\', for each batch, it will emit the
-- metrics for the duration between the oldest record received by the
-- stream and the time it arrives in Glue to CloudWatch. The metric\'s name
-- is \"glue.driver.streaming.maxConsumerLagInMs\". The default value is
-- \'false\'. This option is supported in Glue version 4.0 or later.
--
-- 'endpointUrl', 'kinesisStreamingSourceOptions_endpointUrl' - The URL of the Kinesis endpoint.
--
-- 'idleTimeBetweenReadsInMs', 'kinesisStreamingSourceOptions_idleTimeBetweenReadsInMs' - The minimum time delay between two consecutive getRecords operations,
-- specified in ms. The default value is @1000@. This option is only
-- configurable for Glue version 2.0 and above.
--
-- 'maxFetchRecordsPerShard', 'kinesisStreamingSourceOptions_maxFetchRecordsPerShard' - The maximum number of records to fetch per shard in the Kinesis data
-- stream. The default value is @100000@.
--
-- 'maxFetchTimeInMs', 'kinesisStreamingSourceOptions_maxFetchTimeInMs' - The maximum time spent in the job executor to fetch a record from the
-- Kinesis data stream per shard, specified in milliseconds (ms). The
-- default value is @1000@.
--
-- 'maxRecordPerRead', 'kinesisStreamingSourceOptions_maxRecordPerRead' - The maximum number of records to fetch from the Kinesis data stream in
-- each getRecords operation. The default value is @10000@.
--
-- 'maxRetryIntervalMs', 'kinesisStreamingSourceOptions_maxRetryIntervalMs' - The maximum cool-off time period (specified in ms) between two retries
-- of a Kinesis Data Streams API call. The default value is @10000@.
--
-- 'numRetries', 'kinesisStreamingSourceOptions_numRetries' - The maximum number of retries for Kinesis Data Streams API requests. The
-- default value is @3@.
--
-- 'retryIntervalMs', 'kinesisStreamingSourceOptions_retryIntervalMs' - The cool-off time period (specified in ms) before retrying the Kinesis
-- Data Streams API call. The default value is @1000@.
--
-- 'roleArn', 'kinesisStreamingSourceOptions_roleArn' - The Amazon Resource Name (ARN) of the role to assume using AWS Security
-- Token Service (AWS STS). This role must have permissions for describe or
-- read record operations for the Kinesis data stream. You must use this
-- parameter when accessing a data stream in a different account. Used in
-- conjunction with @\"awsSTSSessionName\"@.
--
-- 'roleSessionName', 'kinesisStreamingSourceOptions_roleSessionName' - An identifier for the session assuming the role using AWS STS. You must
-- use this parameter when accessing a data stream in a different account.
-- Used in conjunction with @\"awsSTSRoleARN\"@.
--
-- 'startingPosition', 'kinesisStreamingSourceOptions_startingPosition' - The starting position in the Kinesis data stream to read data from. The
-- possible values are @\"latest\"@, @\"trim_horizon\"@, @\"earliest\"@, or
-- a timestamp string in UTC format in the pattern @yyyy-mm-ddTHH:MM:SSZ@
-- (where @Z@ represents a UTC timezone offset with a +\/-. For example:
-- \"2023-04-04T08:00:00-04:00\"). The default value is @\"latest\"@.
--
-- Note: Using a value that is a timestamp string in UTC format for
-- \"startingPosition\" is supported only for Glue version 4.0 or later.
--
-- 'startingTimestamp', 'kinesisStreamingSourceOptions_startingTimestamp' - The timestamp of the record in the Kinesis data stream to start reading
-- data from. The possible values are a timestamp string in UTC format of
-- the pattern @yyyy-mm-ddTHH:MM:SSZ@ (where Z represents a UTC timezone
-- offset with a +\/-. For example: \"2023-04-04T08:00:00+08:00\").
--
-- 'streamArn', 'kinesisStreamingSourceOptions_streamArn' - The Amazon Resource Name (ARN) of the Kinesis data stream.
--
-- 'streamName', 'kinesisStreamingSourceOptions_streamName' - The name of the Kinesis data stream.
newKinesisStreamingSourceOptions ::
  KinesisStreamingSourceOptions
newKinesisStreamingSourceOptions =
  KinesisStreamingSourceOptions'
    { addIdleTimeBetweenReads =
        Prelude.Nothing,
      addRecordTimestamp = Prelude.Nothing,
      avoidEmptyBatches = Prelude.Nothing,
      classification = Prelude.Nothing,
      delimiter = Prelude.Nothing,
      describeShardInterval = Prelude.Nothing,
      emitConsumerLagMetrics = Prelude.Nothing,
      endpointUrl = Prelude.Nothing,
      idleTimeBetweenReadsInMs = Prelude.Nothing,
      maxFetchRecordsPerShard = Prelude.Nothing,
      maxFetchTimeInMs = Prelude.Nothing,
      maxRecordPerRead = Prelude.Nothing,
      maxRetryIntervalMs = Prelude.Nothing,
      numRetries = Prelude.Nothing,
      retryIntervalMs = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      roleSessionName = Prelude.Nothing,
      startingPosition = Prelude.Nothing,
      startingTimestamp = Prelude.Nothing,
      streamArn = Prelude.Nothing,
      streamName = Prelude.Nothing
    }

-- | Adds a time delay between two consecutive getRecords operations. The
-- default value is @\"False\"@. This option is only configurable for Glue
-- version 2.0 and above.
kinesisStreamingSourceOptions_addIdleTimeBetweenReads :: Lens.Lens' KinesisStreamingSourceOptions (Prelude.Maybe Prelude.Bool)
kinesisStreamingSourceOptions_addIdleTimeBetweenReads = Lens.lens (\KinesisStreamingSourceOptions' {addIdleTimeBetweenReads} -> addIdleTimeBetweenReads) (\s@KinesisStreamingSourceOptions' {} a -> s {addIdleTimeBetweenReads = a} :: KinesisStreamingSourceOptions)

-- | When this option is set to \'true\', the data output will contain an
-- additional column named \"__src_timestamp\" that indicates the time when
-- the corresponding record received by the stream. The default value is
-- \'false\'. This option is supported in Glue version 4.0 or later.
kinesisStreamingSourceOptions_addRecordTimestamp :: Lens.Lens' KinesisStreamingSourceOptions (Prelude.Maybe Prelude.Text)
kinesisStreamingSourceOptions_addRecordTimestamp = Lens.lens (\KinesisStreamingSourceOptions' {addRecordTimestamp} -> addRecordTimestamp) (\s@KinesisStreamingSourceOptions' {} a -> s {addRecordTimestamp = a} :: KinesisStreamingSourceOptions)

-- | Avoids creating an empty microbatch job by checking for unread data in
-- the Kinesis data stream before the batch is started. The default value
-- is @\"False\"@.
kinesisStreamingSourceOptions_avoidEmptyBatches :: Lens.Lens' KinesisStreamingSourceOptions (Prelude.Maybe Prelude.Bool)
kinesisStreamingSourceOptions_avoidEmptyBatches = Lens.lens (\KinesisStreamingSourceOptions' {avoidEmptyBatches} -> avoidEmptyBatches) (\s@KinesisStreamingSourceOptions' {} a -> s {avoidEmptyBatches = a} :: KinesisStreamingSourceOptions)

-- | An optional classification.
kinesisStreamingSourceOptions_classification :: Lens.Lens' KinesisStreamingSourceOptions (Prelude.Maybe Prelude.Text)
kinesisStreamingSourceOptions_classification = Lens.lens (\KinesisStreamingSourceOptions' {classification} -> classification) (\s@KinesisStreamingSourceOptions' {} a -> s {classification = a} :: KinesisStreamingSourceOptions)

-- | Specifies the delimiter character.
kinesisStreamingSourceOptions_delimiter :: Lens.Lens' KinesisStreamingSourceOptions (Prelude.Maybe Prelude.Text)
kinesisStreamingSourceOptions_delimiter = Lens.lens (\KinesisStreamingSourceOptions' {delimiter} -> delimiter) (\s@KinesisStreamingSourceOptions' {} a -> s {delimiter = a} :: KinesisStreamingSourceOptions)

-- | The minimum time interval between two ListShards API calls for your
-- script to consider resharding. The default value is @1s@.
kinesisStreamingSourceOptions_describeShardInterval :: Lens.Lens' KinesisStreamingSourceOptions (Prelude.Maybe Prelude.Natural)
kinesisStreamingSourceOptions_describeShardInterval = Lens.lens (\KinesisStreamingSourceOptions' {describeShardInterval} -> describeShardInterval) (\s@KinesisStreamingSourceOptions' {} a -> s {describeShardInterval = a} :: KinesisStreamingSourceOptions)

-- | When this option is set to \'true\', for each batch, it will emit the
-- metrics for the duration between the oldest record received by the
-- stream and the time it arrives in Glue to CloudWatch. The metric\'s name
-- is \"glue.driver.streaming.maxConsumerLagInMs\". The default value is
-- \'false\'. This option is supported in Glue version 4.0 or later.
kinesisStreamingSourceOptions_emitConsumerLagMetrics :: Lens.Lens' KinesisStreamingSourceOptions (Prelude.Maybe Prelude.Text)
kinesisStreamingSourceOptions_emitConsumerLagMetrics = Lens.lens (\KinesisStreamingSourceOptions' {emitConsumerLagMetrics} -> emitConsumerLagMetrics) (\s@KinesisStreamingSourceOptions' {} a -> s {emitConsumerLagMetrics = a} :: KinesisStreamingSourceOptions)

-- | The URL of the Kinesis endpoint.
kinesisStreamingSourceOptions_endpointUrl :: Lens.Lens' KinesisStreamingSourceOptions (Prelude.Maybe Prelude.Text)
kinesisStreamingSourceOptions_endpointUrl = Lens.lens (\KinesisStreamingSourceOptions' {endpointUrl} -> endpointUrl) (\s@KinesisStreamingSourceOptions' {} a -> s {endpointUrl = a} :: KinesisStreamingSourceOptions)

-- | The minimum time delay between two consecutive getRecords operations,
-- specified in ms. The default value is @1000@. This option is only
-- configurable for Glue version 2.0 and above.
kinesisStreamingSourceOptions_idleTimeBetweenReadsInMs :: Lens.Lens' KinesisStreamingSourceOptions (Prelude.Maybe Prelude.Natural)
kinesisStreamingSourceOptions_idleTimeBetweenReadsInMs = Lens.lens (\KinesisStreamingSourceOptions' {idleTimeBetweenReadsInMs} -> idleTimeBetweenReadsInMs) (\s@KinesisStreamingSourceOptions' {} a -> s {idleTimeBetweenReadsInMs = a} :: KinesisStreamingSourceOptions)

-- | The maximum number of records to fetch per shard in the Kinesis data
-- stream. The default value is @100000@.
kinesisStreamingSourceOptions_maxFetchRecordsPerShard :: Lens.Lens' KinesisStreamingSourceOptions (Prelude.Maybe Prelude.Natural)
kinesisStreamingSourceOptions_maxFetchRecordsPerShard = Lens.lens (\KinesisStreamingSourceOptions' {maxFetchRecordsPerShard} -> maxFetchRecordsPerShard) (\s@KinesisStreamingSourceOptions' {} a -> s {maxFetchRecordsPerShard = a} :: KinesisStreamingSourceOptions)

-- | The maximum time spent in the job executor to fetch a record from the
-- Kinesis data stream per shard, specified in milliseconds (ms). The
-- default value is @1000@.
kinesisStreamingSourceOptions_maxFetchTimeInMs :: Lens.Lens' KinesisStreamingSourceOptions (Prelude.Maybe Prelude.Natural)
kinesisStreamingSourceOptions_maxFetchTimeInMs = Lens.lens (\KinesisStreamingSourceOptions' {maxFetchTimeInMs} -> maxFetchTimeInMs) (\s@KinesisStreamingSourceOptions' {} a -> s {maxFetchTimeInMs = a} :: KinesisStreamingSourceOptions)

-- | The maximum number of records to fetch from the Kinesis data stream in
-- each getRecords operation. The default value is @10000@.
kinesisStreamingSourceOptions_maxRecordPerRead :: Lens.Lens' KinesisStreamingSourceOptions (Prelude.Maybe Prelude.Natural)
kinesisStreamingSourceOptions_maxRecordPerRead = Lens.lens (\KinesisStreamingSourceOptions' {maxRecordPerRead} -> maxRecordPerRead) (\s@KinesisStreamingSourceOptions' {} a -> s {maxRecordPerRead = a} :: KinesisStreamingSourceOptions)

-- | The maximum cool-off time period (specified in ms) between two retries
-- of a Kinesis Data Streams API call. The default value is @10000@.
kinesisStreamingSourceOptions_maxRetryIntervalMs :: Lens.Lens' KinesisStreamingSourceOptions (Prelude.Maybe Prelude.Natural)
kinesisStreamingSourceOptions_maxRetryIntervalMs = Lens.lens (\KinesisStreamingSourceOptions' {maxRetryIntervalMs} -> maxRetryIntervalMs) (\s@KinesisStreamingSourceOptions' {} a -> s {maxRetryIntervalMs = a} :: KinesisStreamingSourceOptions)

-- | The maximum number of retries for Kinesis Data Streams API requests. The
-- default value is @3@.
kinesisStreamingSourceOptions_numRetries :: Lens.Lens' KinesisStreamingSourceOptions (Prelude.Maybe Prelude.Natural)
kinesisStreamingSourceOptions_numRetries = Lens.lens (\KinesisStreamingSourceOptions' {numRetries} -> numRetries) (\s@KinesisStreamingSourceOptions' {} a -> s {numRetries = a} :: KinesisStreamingSourceOptions)

-- | The cool-off time period (specified in ms) before retrying the Kinesis
-- Data Streams API call. The default value is @1000@.
kinesisStreamingSourceOptions_retryIntervalMs :: Lens.Lens' KinesisStreamingSourceOptions (Prelude.Maybe Prelude.Natural)
kinesisStreamingSourceOptions_retryIntervalMs = Lens.lens (\KinesisStreamingSourceOptions' {retryIntervalMs} -> retryIntervalMs) (\s@KinesisStreamingSourceOptions' {} a -> s {retryIntervalMs = a} :: KinesisStreamingSourceOptions)

-- | The Amazon Resource Name (ARN) of the role to assume using AWS Security
-- Token Service (AWS STS). This role must have permissions for describe or
-- read record operations for the Kinesis data stream. You must use this
-- parameter when accessing a data stream in a different account. Used in
-- conjunction with @\"awsSTSSessionName\"@.
kinesisStreamingSourceOptions_roleArn :: Lens.Lens' KinesisStreamingSourceOptions (Prelude.Maybe Prelude.Text)
kinesisStreamingSourceOptions_roleArn = Lens.lens (\KinesisStreamingSourceOptions' {roleArn} -> roleArn) (\s@KinesisStreamingSourceOptions' {} a -> s {roleArn = a} :: KinesisStreamingSourceOptions)

-- | An identifier for the session assuming the role using AWS STS. You must
-- use this parameter when accessing a data stream in a different account.
-- Used in conjunction with @\"awsSTSRoleARN\"@.
kinesisStreamingSourceOptions_roleSessionName :: Lens.Lens' KinesisStreamingSourceOptions (Prelude.Maybe Prelude.Text)
kinesisStreamingSourceOptions_roleSessionName = Lens.lens (\KinesisStreamingSourceOptions' {roleSessionName} -> roleSessionName) (\s@KinesisStreamingSourceOptions' {} a -> s {roleSessionName = a} :: KinesisStreamingSourceOptions)

-- | The starting position in the Kinesis data stream to read data from. The
-- possible values are @\"latest\"@, @\"trim_horizon\"@, @\"earliest\"@, or
-- a timestamp string in UTC format in the pattern @yyyy-mm-ddTHH:MM:SSZ@
-- (where @Z@ represents a UTC timezone offset with a +\/-. For example:
-- \"2023-04-04T08:00:00-04:00\"). The default value is @\"latest\"@.
--
-- Note: Using a value that is a timestamp string in UTC format for
-- \"startingPosition\" is supported only for Glue version 4.0 or later.
kinesisStreamingSourceOptions_startingPosition :: Lens.Lens' KinesisStreamingSourceOptions (Prelude.Maybe StartingPosition)
kinesisStreamingSourceOptions_startingPosition = Lens.lens (\KinesisStreamingSourceOptions' {startingPosition} -> startingPosition) (\s@KinesisStreamingSourceOptions' {} a -> s {startingPosition = a} :: KinesisStreamingSourceOptions)

-- | The timestamp of the record in the Kinesis data stream to start reading
-- data from. The possible values are a timestamp string in UTC format of
-- the pattern @yyyy-mm-ddTHH:MM:SSZ@ (where Z represents a UTC timezone
-- offset with a +\/-. For example: \"2023-04-04T08:00:00+08:00\").
kinesisStreamingSourceOptions_startingTimestamp :: Lens.Lens' KinesisStreamingSourceOptions (Prelude.Maybe Prelude.UTCTime)
kinesisStreamingSourceOptions_startingTimestamp = Lens.lens (\KinesisStreamingSourceOptions' {startingTimestamp} -> startingTimestamp) (\s@KinesisStreamingSourceOptions' {} a -> s {startingTimestamp = a} :: KinesisStreamingSourceOptions) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the Kinesis data stream.
kinesisStreamingSourceOptions_streamArn :: Lens.Lens' KinesisStreamingSourceOptions (Prelude.Maybe Prelude.Text)
kinesisStreamingSourceOptions_streamArn = Lens.lens (\KinesisStreamingSourceOptions' {streamArn} -> streamArn) (\s@KinesisStreamingSourceOptions' {} a -> s {streamArn = a} :: KinesisStreamingSourceOptions)

-- | The name of the Kinesis data stream.
kinesisStreamingSourceOptions_streamName :: Lens.Lens' KinesisStreamingSourceOptions (Prelude.Maybe Prelude.Text)
kinesisStreamingSourceOptions_streamName = Lens.lens (\KinesisStreamingSourceOptions' {streamName} -> streamName) (\s@KinesisStreamingSourceOptions' {} a -> s {streamName = a} :: KinesisStreamingSourceOptions)

instance Data.FromJSON KinesisStreamingSourceOptions where
  parseJSON =
    Data.withObject
      "KinesisStreamingSourceOptions"
      ( \x ->
          KinesisStreamingSourceOptions'
            Prelude.<$> (x Data..:? "AddIdleTimeBetweenReads")
            Prelude.<*> (x Data..:? "AddRecordTimestamp")
            Prelude.<*> (x Data..:? "AvoidEmptyBatches")
            Prelude.<*> (x Data..:? "Classification")
            Prelude.<*> (x Data..:? "Delimiter")
            Prelude.<*> (x Data..:? "DescribeShardInterval")
            Prelude.<*> (x Data..:? "EmitConsumerLagMetrics")
            Prelude.<*> (x Data..:? "EndpointUrl")
            Prelude.<*> (x Data..:? "IdleTimeBetweenReadsInMs")
            Prelude.<*> (x Data..:? "MaxFetchRecordsPerShard")
            Prelude.<*> (x Data..:? "MaxFetchTimeInMs")
            Prelude.<*> (x Data..:? "MaxRecordPerRead")
            Prelude.<*> (x Data..:? "MaxRetryIntervalMs")
            Prelude.<*> (x Data..:? "NumRetries")
            Prelude.<*> (x Data..:? "RetryIntervalMs")
            Prelude.<*> (x Data..:? "RoleArn")
            Prelude.<*> (x Data..:? "RoleSessionName")
            Prelude.<*> (x Data..:? "StartingPosition")
            Prelude.<*> (x Data..:? "StartingTimestamp")
            Prelude.<*> (x Data..:? "StreamArn")
            Prelude.<*> (x Data..:? "StreamName")
      )

instance
  Prelude.Hashable
    KinesisStreamingSourceOptions
  where
  hashWithSalt _salt KinesisStreamingSourceOptions' {..} =
    _salt
      `Prelude.hashWithSalt` addIdleTimeBetweenReads
      `Prelude.hashWithSalt` addRecordTimestamp
      `Prelude.hashWithSalt` avoidEmptyBatches
      `Prelude.hashWithSalt` classification
      `Prelude.hashWithSalt` delimiter
      `Prelude.hashWithSalt` describeShardInterval
      `Prelude.hashWithSalt` emitConsumerLagMetrics
      `Prelude.hashWithSalt` endpointUrl
      `Prelude.hashWithSalt` idleTimeBetweenReadsInMs
      `Prelude.hashWithSalt` maxFetchRecordsPerShard
      `Prelude.hashWithSalt` maxFetchTimeInMs
      `Prelude.hashWithSalt` maxRecordPerRead
      `Prelude.hashWithSalt` maxRetryIntervalMs
      `Prelude.hashWithSalt` numRetries
      `Prelude.hashWithSalt` retryIntervalMs
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` roleSessionName
      `Prelude.hashWithSalt` startingPosition
      `Prelude.hashWithSalt` startingTimestamp
      `Prelude.hashWithSalt` streamArn
      `Prelude.hashWithSalt` streamName

instance Prelude.NFData KinesisStreamingSourceOptions where
  rnf KinesisStreamingSourceOptions' {..} =
    Prelude.rnf addIdleTimeBetweenReads
      `Prelude.seq` Prelude.rnf addRecordTimestamp
      `Prelude.seq` Prelude.rnf avoidEmptyBatches
      `Prelude.seq` Prelude.rnf classification
      `Prelude.seq` Prelude.rnf delimiter
      `Prelude.seq` Prelude.rnf describeShardInterval
      `Prelude.seq` Prelude.rnf emitConsumerLagMetrics
      `Prelude.seq` Prelude.rnf endpointUrl
      `Prelude.seq` Prelude.rnf idleTimeBetweenReadsInMs
      `Prelude.seq` Prelude.rnf maxFetchRecordsPerShard
      `Prelude.seq` Prelude.rnf maxFetchTimeInMs
      `Prelude.seq` Prelude.rnf maxRecordPerRead
      `Prelude.seq` Prelude.rnf maxRetryIntervalMs
      `Prelude.seq` Prelude.rnf numRetries
      `Prelude.seq` Prelude.rnf retryIntervalMs
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf roleSessionName
      `Prelude.seq` Prelude.rnf startingPosition
      `Prelude.seq` Prelude.rnf startingTimestamp
      `Prelude.seq` Prelude.rnf streamArn
      `Prelude.seq` Prelude.rnf streamName

instance Data.ToJSON KinesisStreamingSourceOptions where
  toJSON KinesisStreamingSourceOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AddIdleTimeBetweenReads" Data..=)
              Prelude.<$> addIdleTimeBetweenReads,
            ("AddRecordTimestamp" Data..=)
              Prelude.<$> addRecordTimestamp,
            ("AvoidEmptyBatches" Data..=)
              Prelude.<$> avoidEmptyBatches,
            ("Classification" Data..=)
              Prelude.<$> classification,
            ("Delimiter" Data..=) Prelude.<$> delimiter,
            ("DescribeShardInterval" Data..=)
              Prelude.<$> describeShardInterval,
            ("EmitConsumerLagMetrics" Data..=)
              Prelude.<$> emitConsumerLagMetrics,
            ("EndpointUrl" Data..=) Prelude.<$> endpointUrl,
            ("IdleTimeBetweenReadsInMs" Data..=)
              Prelude.<$> idleTimeBetweenReadsInMs,
            ("MaxFetchRecordsPerShard" Data..=)
              Prelude.<$> maxFetchRecordsPerShard,
            ("MaxFetchTimeInMs" Data..=)
              Prelude.<$> maxFetchTimeInMs,
            ("MaxRecordPerRead" Data..=)
              Prelude.<$> maxRecordPerRead,
            ("MaxRetryIntervalMs" Data..=)
              Prelude.<$> maxRetryIntervalMs,
            ("NumRetries" Data..=) Prelude.<$> numRetries,
            ("RetryIntervalMs" Data..=)
              Prelude.<$> retryIntervalMs,
            ("RoleArn" Data..=) Prelude.<$> roleArn,
            ("RoleSessionName" Data..=)
              Prelude.<$> roleSessionName,
            ("StartingPosition" Data..=)
              Prelude.<$> startingPosition,
            ("StartingTimestamp" Data..=)
              Prelude.<$> startingTimestamp,
            ("StreamArn" Data..=) Prelude.<$> streamArn,
            ("StreamName" Data..=) Prelude.<$> streamName
          ]
      )
