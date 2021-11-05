{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Pi.DescribeDimensionKeys
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For a specific time period, retrieve the top @N@ dimension keys for a
-- metric.
--
-- Each response element returns a maximum of 500 bytes. For larger
-- elements, such as SQL statements, only the first 500 bytes are returned.
module Amazonka.Pi.DescribeDimensionKeys
  ( -- * Creating a Request
    DescribeDimensionKeys (..),
    newDescribeDimensionKeys,

    -- * Request Lenses
    describeDimensionKeys_periodInSeconds,
    describeDimensionKeys_nextToken,
    describeDimensionKeys_filter,
    describeDimensionKeys_maxResults,
    describeDimensionKeys_partitionBy,
    describeDimensionKeys_serviceType,
    describeDimensionKeys_identifier,
    describeDimensionKeys_startTime,
    describeDimensionKeys_endTime,
    describeDimensionKeys_metric,
    describeDimensionKeys_groupBy,

    -- * Destructuring the Response
    DescribeDimensionKeysResponse (..),
    newDescribeDimensionKeysResponse,

    -- * Response Lenses
    describeDimensionKeysResponse_alignedEndTime,
    describeDimensionKeysResponse_alignedStartTime,
    describeDimensionKeysResponse_keys,
    describeDimensionKeysResponse_nextToken,
    describeDimensionKeysResponse_partitionKeys,
    describeDimensionKeysResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pi.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDimensionKeys' smart constructor.
data DescribeDimensionKeys = DescribeDimensionKeys'
  { -- | The granularity, in seconds, of the data points returned from
    -- Performance Insights. A period can be as short as one second, or as long
    -- as one day (86400 seconds). Valid values are:
    --
    -- -   @1@ (one second)
    --
    -- -   @60@ (one minute)
    --
    -- -   @300@ (five minutes)
    --
    -- -   @3600@ (one hour)
    --
    -- -   @86400@ (twenty-four hours)
    --
    -- If you don\'t specify @PeriodInSeconds@, then Performance Insights
    -- chooses a value for you, with a goal of returning roughly 100-200 data
    -- points in the response.
    periodInSeconds :: Prelude.Maybe Prelude.Int,
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- token, up to the value specified by @MaxRecords@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One or more filters to apply in the request. Restrictions:
    --
    -- -   Any number of filters by the same dimension, as specified in the
    --     @GroupBy@ or @Partition@ parameters.
    --
    -- -   A single filter for any other dimension in this dimension group.
    filter' :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The maximum number of items to return in the response. If more items
    -- exist than the specified @MaxRecords@ value, a pagination token is
    -- included in the response so that the remaining results can be retrieved.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | For each dimension specified in @GroupBy@, specify a secondary dimension
    -- to further subdivide the partition keys in the response.
    partitionBy :: Prelude.Maybe DimensionGroup,
    -- | The AWS service for which Performance Insights will return metrics. The
    -- only valid value for /ServiceType/ is @RDS@.
    serviceType :: ServiceType,
    -- | An immutable, AWS Region-unique identifier for a data source.
    -- Performance Insights gathers metrics from this data source.
    --
    -- To use an Amazon RDS instance as a data source, you specify its
    -- @DbiResourceId@ value. For example, specify
    -- @db-FAIHNTYBKTGAUSUZQYPDS2GW4A@
    identifier :: Prelude.Text,
    -- | The date and time specifying the beginning of the requested time series
    -- data. You must specify a @StartTime@ within the past 7 days. The value
    -- specified is /inclusive/, which means that data points equal to or
    -- greater than @StartTime@ are returned.
    --
    -- The value for @StartTime@ must be earlier than the value for @EndTime@.
    startTime :: Core.POSIX,
    -- | The date and time specifying the end of the requested time series data.
    -- The value specified is /exclusive/, which means that data points less
    -- than (but not equal to) @EndTime@ are returned.
    --
    -- The value for @EndTime@ must be later than the value for @StartTime@.
    endTime :: Core.POSIX,
    -- | The name of a Performance Insights metric to be measured.
    --
    -- Valid values for @Metric@ are:
    --
    -- -   @db.load.avg@ - a scaled representation of the number of active
    --     sessions for the database engine.
    --
    -- -   @db.sampledload.avg@ - the raw number of active sessions for the
    --     database engine.
    --
    -- If the number of active sessions is less than an internal Performance
    -- Insights threshold, @db.load.avg@ and @db.sampledload.avg@ are the same
    -- value. If the number of active sessions is greater than the internal
    -- threshold, Performance Insights samples the active sessions, with
    -- @db.load.avg@ showing the scaled values, @db.sampledload.avg@ showing
    -- the raw values, and @db.sampledload.avg@ less than @db.load.avg@. For
    -- most use cases, you can query @db.load.avg@ only.
    metric :: Prelude.Text,
    -- | A specification for how to aggregate the data points from a query
    -- result. You must specify a valid dimension group. Performance Insights
    -- returns all dimensions within this group, unless you provide the names
    -- of specific dimensions within this group. You can also request that
    -- Performance Insights return a limited number of values for a dimension.
    groupBy :: DimensionGroup
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDimensionKeys' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'periodInSeconds', 'describeDimensionKeys_periodInSeconds' - The granularity, in seconds, of the data points returned from
-- Performance Insights. A period can be as short as one second, or as long
-- as one day (86400 seconds). Valid values are:
--
-- -   @1@ (one second)
--
-- -   @60@ (one minute)
--
-- -   @300@ (five minutes)
--
-- -   @3600@ (one hour)
--
-- -   @86400@ (twenty-four hours)
--
-- If you don\'t specify @PeriodInSeconds@, then Performance Insights
-- chooses a value for you, with a goal of returning roughly 100-200 data
-- points in the response.
--
-- 'nextToken', 'describeDimensionKeys_nextToken' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- token, up to the value specified by @MaxRecords@.
--
-- 'filter'', 'describeDimensionKeys_filter' - One or more filters to apply in the request. Restrictions:
--
-- -   Any number of filters by the same dimension, as specified in the
--     @GroupBy@ or @Partition@ parameters.
--
-- -   A single filter for any other dimension in this dimension group.
--
-- 'maxResults', 'describeDimensionKeys_maxResults' - The maximum number of items to return in the response. If more items
-- exist than the specified @MaxRecords@ value, a pagination token is
-- included in the response so that the remaining results can be retrieved.
--
-- 'partitionBy', 'describeDimensionKeys_partitionBy' - For each dimension specified in @GroupBy@, specify a secondary dimension
-- to further subdivide the partition keys in the response.
--
-- 'serviceType', 'describeDimensionKeys_serviceType' - The AWS service for which Performance Insights will return metrics. The
-- only valid value for /ServiceType/ is @RDS@.
--
-- 'identifier', 'describeDimensionKeys_identifier' - An immutable, AWS Region-unique identifier for a data source.
-- Performance Insights gathers metrics from this data source.
--
-- To use an Amazon RDS instance as a data source, you specify its
-- @DbiResourceId@ value. For example, specify
-- @db-FAIHNTYBKTGAUSUZQYPDS2GW4A@
--
-- 'startTime', 'describeDimensionKeys_startTime' - The date and time specifying the beginning of the requested time series
-- data. You must specify a @StartTime@ within the past 7 days. The value
-- specified is /inclusive/, which means that data points equal to or
-- greater than @StartTime@ are returned.
--
-- The value for @StartTime@ must be earlier than the value for @EndTime@.
--
-- 'endTime', 'describeDimensionKeys_endTime' - The date and time specifying the end of the requested time series data.
-- The value specified is /exclusive/, which means that data points less
-- than (but not equal to) @EndTime@ are returned.
--
-- The value for @EndTime@ must be later than the value for @StartTime@.
--
-- 'metric', 'describeDimensionKeys_metric' - The name of a Performance Insights metric to be measured.
--
-- Valid values for @Metric@ are:
--
-- -   @db.load.avg@ - a scaled representation of the number of active
--     sessions for the database engine.
--
-- -   @db.sampledload.avg@ - the raw number of active sessions for the
--     database engine.
--
-- If the number of active sessions is less than an internal Performance
-- Insights threshold, @db.load.avg@ and @db.sampledload.avg@ are the same
-- value. If the number of active sessions is greater than the internal
-- threshold, Performance Insights samples the active sessions, with
-- @db.load.avg@ showing the scaled values, @db.sampledload.avg@ showing
-- the raw values, and @db.sampledload.avg@ less than @db.load.avg@. For
-- most use cases, you can query @db.load.avg@ only.
--
-- 'groupBy', 'describeDimensionKeys_groupBy' - A specification for how to aggregate the data points from a query
-- result. You must specify a valid dimension group. Performance Insights
-- returns all dimensions within this group, unless you provide the names
-- of specific dimensions within this group. You can also request that
-- Performance Insights return a limited number of values for a dimension.
newDescribeDimensionKeys ::
  -- | 'serviceType'
  ServiceType ->
  -- | 'identifier'
  Prelude.Text ->
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'endTime'
  Prelude.UTCTime ->
  -- | 'metric'
  Prelude.Text ->
  -- | 'groupBy'
  DimensionGroup ->
  DescribeDimensionKeys
newDescribeDimensionKeys
  pServiceType_
  pIdentifier_
  pStartTime_
  pEndTime_
  pMetric_
  pGroupBy_ =
    DescribeDimensionKeys'
      { periodInSeconds =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        filter' = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        partitionBy = Prelude.Nothing,
        serviceType = pServiceType_,
        identifier = pIdentifier_,
        startTime = Core._Time Lens.# pStartTime_,
        endTime = Core._Time Lens.# pEndTime_,
        metric = pMetric_,
        groupBy = pGroupBy_
      }

-- | The granularity, in seconds, of the data points returned from
-- Performance Insights. A period can be as short as one second, or as long
-- as one day (86400 seconds). Valid values are:
--
-- -   @1@ (one second)
--
-- -   @60@ (one minute)
--
-- -   @300@ (five minutes)
--
-- -   @3600@ (one hour)
--
-- -   @86400@ (twenty-four hours)
--
-- If you don\'t specify @PeriodInSeconds@, then Performance Insights
-- chooses a value for you, with a goal of returning roughly 100-200 data
-- points in the response.
describeDimensionKeys_periodInSeconds :: Lens.Lens' DescribeDimensionKeys (Prelude.Maybe Prelude.Int)
describeDimensionKeys_periodInSeconds = Lens.lens (\DescribeDimensionKeys' {periodInSeconds} -> periodInSeconds) (\s@DescribeDimensionKeys' {} a -> s {periodInSeconds = a} :: DescribeDimensionKeys)

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- token, up to the value specified by @MaxRecords@.
describeDimensionKeys_nextToken :: Lens.Lens' DescribeDimensionKeys (Prelude.Maybe Prelude.Text)
describeDimensionKeys_nextToken = Lens.lens (\DescribeDimensionKeys' {nextToken} -> nextToken) (\s@DescribeDimensionKeys' {} a -> s {nextToken = a} :: DescribeDimensionKeys)

-- | One or more filters to apply in the request. Restrictions:
--
-- -   Any number of filters by the same dimension, as specified in the
--     @GroupBy@ or @Partition@ parameters.
--
-- -   A single filter for any other dimension in this dimension group.
describeDimensionKeys_filter :: Lens.Lens' DescribeDimensionKeys (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeDimensionKeys_filter = Lens.lens (\DescribeDimensionKeys' {filter'} -> filter') (\s@DescribeDimensionKeys' {} a -> s {filter' = a} :: DescribeDimensionKeys) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return in the response. If more items
-- exist than the specified @MaxRecords@ value, a pagination token is
-- included in the response so that the remaining results can be retrieved.
describeDimensionKeys_maxResults :: Lens.Lens' DescribeDimensionKeys (Prelude.Maybe Prelude.Natural)
describeDimensionKeys_maxResults = Lens.lens (\DescribeDimensionKeys' {maxResults} -> maxResults) (\s@DescribeDimensionKeys' {} a -> s {maxResults = a} :: DescribeDimensionKeys)

-- | For each dimension specified in @GroupBy@, specify a secondary dimension
-- to further subdivide the partition keys in the response.
describeDimensionKeys_partitionBy :: Lens.Lens' DescribeDimensionKeys (Prelude.Maybe DimensionGroup)
describeDimensionKeys_partitionBy = Lens.lens (\DescribeDimensionKeys' {partitionBy} -> partitionBy) (\s@DescribeDimensionKeys' {} a -> s {partitionBy = a} :: DescribeDimensionKeys)

-- | The AWS service for which Performance Insights will return metrics. The
-- only valid value for /ServiceType/ is @RDS@.
describeDimensionKeys_serviceType :: Lens.Lens' DescribeDimensionKeys ServiceType
describeDimensionKeys_serviceType = Lens.lens (\DescribeDimensionKeys' {serviceType} -> serviceType) (\s@DescribeDimensionKeys' {} a -> s {serviceType = a} :: DescribeDimensionKeys)

-- | An immutable, AWS Region-unique identifier for a data source.
-- Performance Insights gathers metrics from this data source.
--
-- To use an Amazon RDS instance as a data source, you specify its
-- @DbiResourceId@ value. For example, specify
-- @db-FAIHNTYBKTGAUSUZQYPDS2GW4A@
describeDimensionKeys_identifier :: Lens.Lens' DescribeDimensionKeys Prelude.Text
describeDimensionKeys_identifier = Lens.lens (\DescribeDimensionKeys' {identifier} -> identifier) (\s@DescribeDimensionKeys' {} a -> s {identifier = a} :: DescribeDimensionKeys)

-- | The date and time specifying the beginning of the requested time series
-- data. You must specify a @StartTime@ within the past 7 days. The value
-- specified is /inclusive/, which means that data points equal to or
-- greater than @StartTime@ are returned.
--
-- The value for @StartTime@ must be earlier than the value for @EndTime@.
describeDimensionKeys_startTime :: Lens.Lens' DescribeDimensionKeys Prelude.UTCTime
describeDimensionKeys_startTime = Lens.lens (\DescribeDimensionKeys' {startTime} -> startTime) (\s@DescribeDimensionKeys' {} a -> s {startTime = a} :: DescribeDimensionKeys) Prelude.. Core._Time

-- | The date and time specifying the end of the requested time series data.
-- The value specified is /exclusive/, which means that data points less
-- than (but not equal to) @EndTime@ are returned.
--
-- The value for @EndTime@ must be later than the value for @StartTime@.
describeDimensionKeys_endTime :: Lens.Lens' DescribeDimensionKeys Prelude.UTCTime
describeDimensionKeys_endTime = Lens.lens (\DescribeDimensionKeys' {endTime} -> endTime) (\s@DescribeDimensionKeys' {} a -> s {endTime = a} :: DescribeDimensionKeys) Prelude.. Core._Time

-- | The name of a Performance Insights metric to be measured.
--
-- Valid values for @Metric@ are:
--
-- -   @db.load.avg@ - a scaled representation of the number of active
--     sessions for the database engine.
--
-- -   @db.sampledload.avg@ - the raw number of active sessions for the
--     database engine.
--
-- If the number of active sessions is less than an internal Performance
-- Insights threshold, @db.load.avg@ and @db.sampledload.avg@ are the same
-- value. If the number of active sessions is greater than the internal
-- threshold, Performance Insights samples the active sessions, with
-- @db.load.avg@ showing the scaled values, @db.sampledload.avg@ showing
-- the raw values, and @db.sampledload.avg@ less than @db.load.avg@. For
-- most use cases, you can query @db.load.avg@ only.
describeDimensionKeys_metric :: Lens.Lens' DescribeDimensionKeys Prelude.Text
describeDimensionKeys_metric = Lens.lens (\DescribeDimensionKeys' {metric} -> metric) (\s@DescribeDimensionKeys' {} a -> s {metric = a} :: DescribeDimensionKeys)

-- | A specification for how to aggregate the data points from a query
-- result. You must specify a valid dimension group. Performance Insights
-- returns all dimensions within this group, unless you provide the names
-- of specific dimensions within this group. You can also request that
-- Performance Insights return a limited number of values for a dimension.
describeDimensionKeys_groupBy :: Lens.Lens' DescribeDimensionKeys DimensionGroup
describeDimensionKeys_groupBy = Lens.lens (\DescribeDimensionKeys' {groupBy} -> groupBy) (\s@DescribeDimensionKeys' {} a -> s {groupBy = a} :: DescribeDimensionKeys)

instance Core.AWSRequest DescribeDimensionKeys where
  type
    AWSResponse DescribeDimensionKeys =
      DescribeDimensionKeysResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDimensionKeysResponse'
            Prelude.<$> (x Core..?> "AlignedEndTime")
            Prelude.<*> (x Core..?> "AlignedStartTime")
            Prelude.<*> (x Core..?> "Keys" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "PartitionKeys" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDimensionKeys

instance Prelude.NFData DescribeDimensionKeys

instance Core.ToHeaders DescribeDimensionKeys where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PerformanceInsightsv20180227.DescribeDimensionKeys" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeDimensionKeys where
  toJSON DescribeDimensionKeys' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PeriodInSeconds" Core..=)
              Prelude.<$> periodInSeconds,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Filter" Core..=) Prelude.<$> filter',
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("PartitionBy" Core..=) Prelude.<$> partitionBy,
            Prelude.Just ("ServiceType" Core..= serviceType),
            Prelude.Just ("Identifier" Core..= identifier),
            Prelude.Just ("StartTime" Core..= startTime),
            Prelude.Just ("EndTime" Core..= endTime),
            Prelude.Just ("Metric" Core..= metric),
            Prelude.Just ("GroupBy" Core..= groupBy)
          ]
      )

instance Core.ToPath DescribeDimensionKeys where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeDimensionKeys where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDimensionKeysResponse' smart constructor.
data DescribeDimensionKeysResponse = DescribeDimensionKeysResponse'
  { -- | The end time for the returned dimension keys, after alignment to a
    -- granular boundary (as specified by @PeriodInSeconds@). @AlignedEndTime@
    -- will be greater than or equal to the value of the user-specified
    -- @Endtime@.
    alignedEndTime :: Prelude.Maybe Core.POSIX,
    -- | The start time for the returned dimension keys, after alignment to a
    -- granular boundary (as specified by @PeriodInSeconds@).
    -- @AlignedStartTime@ will be less than or equal to the value of the
    -- user-specified @StartTime@.
    alignedStartTime :: Prelude.Maybe Core.POSIX,
    -- | The dimension keys that were requested.
    keys :: Prelude.Maybe [DimensionKeyDescription],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- token, up to the value specified by @MaxRecords@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | If @PartitionBy@ was present in the request, @PartitionKeys@ contains
    -- the breakdown of dimension keys by the specified partitions.
    partitionKeys :: Prelude.Maybe [ResponsePartitionKey],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDimensionKeysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alignedEndTime', 'describeDimensionKeysResponse_alignedEndTime' - The end time for the returned dimension keys, after alignment to a
-- granular boundary (as specified by @PeriodInSeconds@). @AlignedEndTime@
-- will be greater than or equal to the value of the user-specified
-- @Endtime@.
--
-- 'alignedStartTime', 'describeDimensionKeysResponse_alignedStartTime' - The start time for the returned dimension keys, after alignment to a
-- granular boundary (as specified by @PeriodInSeconds@).
-- @AlignedStartTime@ will be less than or equal to the value of the
-- user-specified @StartTime@.
--
-- 'keys', 'describeDimensionKeysResponse_keys' - The dimension keys that were requested.
--
-- 'nextToken', 'describeDimensionKeysResponse_nextToken' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- token, up to the value specified by @MaxRecords@.
--
-- 'partitionKeys', 'describeDimensionKeysResponse_partitionKeys' - If @PartitionBy@ was present in the request, @PartitionKeys@ contains
-- the breakdown of dimension keys by the specified partitions.
--
-- 'httpStatus', 'describeDimensionKeysResponse_httpStatus' - The response's http status code.
newDescribeDimensionKeysResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDimensionKeysResponse
newDescribeDimensionKeysResponse pHttpStatus_ =
  DescribeDimensionKeysResponse'
    { alignedEndTime =
        Prelude.Nothing,
      alignedStartTime = Prelude.Nothing,
      keys = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      partitionKeys = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The end time for the returned dimension keys, after alignment to a
-- granular boundary (as specified by @PeriodInSeconds@). @AlignedEndTime@
-- will be greater than or equal to the value of the user-specified
-- @Endtime@.
describeDimensionKeysResponse_alignedEndTime :: Lens.Lens' DescribeDimensionKeysResponse (Prelude.Maybe Prelude.UTCTime)
describeDimensionKeysResponse_alignedEndTime = Lens.lens (\DescribeDimensionKeysResponse' {alignedEndTime} -> alignedEndTime) (\s@DescribeDimensionKeysResponse' {} a -> s {alignedEndTime = a} :: DescribeDimensionKeysResponse) Prelude.. Lens.mapping Core._Time

-- | The start time for the returned dimension keys, after alignment to a
-- granular boundary (as specified by @PeriodInSeconds@).
-- @AlignedStartTime@ will be less than or equal to the value of the
-- user-specified @StartTime@.
describeDimensionKeysResponse_alignedStartTime :: Lens.Lens' DescribeDimensionKeysResponse (Prelude.Maybe Prelude.UTCTime)
describeDimensionKeysResponse_alignedStartTime = Lens.lens (\DescribeDimensionKeysResponse' {alignedStartTime} -> alignedStartTime) (\s@DescribeDimensionKeysResponse' {} a -> s {alignedStartTime = a} :: DescribeDimensionKeysResponse) Prelude.. Lens.mapping Core._Time

-- | The dimension keys that were requested.
describeDimensionKeysResponse_keys :: Lens.Lens' DescribeDimensionKeysResponse (Prelude.Maybe [DimensionKeyDescription])
describeDimensionKeysResponse_keys = Lens.lens (\DescribeDimensionKeysResponse' {keys} -> keys) (\s@DescribeDimensionKeysResponse' {} a -> s {keys = a} :: DescribeDimensionKeysResponse) Prelude.. Lens.mapping Lens.coerced

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- token, up to the value specified by @MaxRecords@.
describeDimensionKeysResponse_nextToken :: Lens.Lens' DescribeDimensionKeysResponse (Prelude.Maybe Prelude.Text)
describeDimensionKeysResponse_nextToken = Lens.lens (\DescribeDimensionKeysResponse' {nextToken} -> nextToken) (\s@DescribeDimensionKeysResponse' {} a -> s {nextToken = a} :: DescribeDimensionKeysResponse)

-- | If @PartitionBy@ was present in the request, @PartitionKeys@ contains
-- the breakdown of dimension keys by the specified partitions.
describeDimensionKeysResponse_partitionKeys :: Lens.Lens' DescribeDimensionKeysResponse (Prelude.Maybe [ResponsePartitionKey])
describeDimensionKeysResponse_partitionKeys = Lens.lens (\DescribeDimensionKeysResponse' {partitionKeys} -> partitionKeys) (\s@DescribeDimensionKeysResponse' {} a -> s {partitionKeys = a} :: DescribeDimensionKeysResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeDimensionKeysResponse_httpStatus :: Lens.Lens' DescribeDimensionKeysResponse Prelude.Int
describeDimensionKeysResponse_httpStatus = Lens.lens (\DescribeDimensionKeysResponse' {httpStatus} -> httpStatus) (\s@DescribeDimensionKeysResponse' {} a -> s {httpStatus = a} :: DescribeDimensionKeysResponse)

instance Prelude.NFData DescribeDimensionKeysResponse
