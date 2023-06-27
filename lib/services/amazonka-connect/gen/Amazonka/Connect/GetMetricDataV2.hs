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
-- Module      : Amazonka.Connect.GetMetricDataV2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets metric data from the specified Amazon Connect instance.
--
-- @GetMetricDataV2@ offers more features than
-- <https://docs.aws.amazon.com/connect/latest/APIReference/API_GetMetricData.html GetMetricData>,
-- the previous version of this API. It has new metrics, offers filtering
-- at a metric level, and offers the ability to filter and group data by
-- channels, queues, routing profiles, agents, and agent hierarchy levels.
-- It can retrieve historical data for the last 35 days, in 24-hour
-- intervals.
--
-- For a description of the historical metrics that are supported by
-- @GetMetricDataV2@ and @GetMetricData@, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/historical-metrics-definitions.html Historical metrics definitions>
-- in the /Amazon Connect Administrator\'s Guide/.
module Amazonka.Connect.GetMetricDataV2
  ( -- * Creating a Request
    GetMetricDataV2 (..),
    newGetMetricDataV2,

    -- * Request Lenses
    getMetricDataV2_groupings,
    getMetricDataV2_maxResults,
    getMetricDataV2_nextToken,
    getMetricDataV2_resourceArn,
    getMetricDataV2_startTime,
    getMetricDataV2_endTime,
    getMetricDataV2_filters,
    getMetricDataV2_metrics,

    -- * Destructuring the Response
    GetMetricDataV2Response (..),
    newGetMetricDataV2Response,

    -- * Response Lenses
    getMetricDataV2Response_metricResults,
    getMetricDataV2Response_nextToken,
    getMetricDataV2Response_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetMetricDataV2' smart constructor.
data GetMetricDataV2 = GetMetricDataV2'
  { -- | The grouping applied to the metrics that are returned. For example, when
    -- results are grouped by queue, the metrics returned are grouped by queue.
    -- The values that are returned apply to the metrics for each queue. They
    -- are not aggregated for all queues.
    --
    -- If no grouping is specified, a summary of all metrics is returned.
    --
    -- Valid grouping keys: @QUEUE@ | @ROUTING_PROFILE@ | @AGENT@ | @CHANNEL@ |
    -- @AGENT_HIERARCHY_LEVEL_ONE@ | @AGENT_HIERARCHY_LEVEL_TWO@ |
    -- @AGENT_HIERARCHY_LEVEL_THREE@ | @AGENT_HIERARCHY_LEVEL_FOUR@ |
    -- @AGENT_HIERARCHY_LEVEL_FIVE@
    groupings :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the resource. This includes the
    -- @instanceId@ an Amazon Connect instance.
    resourceArn :: Prelude.Text,
    -- | The timestamp, in UNIX Epoch time format, at which to start the
    -- reporting interval for the retrieval of historical metrics data. The
    -- time must be before the end time timestamp. The time range between the
    -- start and end time must be less than 24 hours. The start time cannot be
    -- earlier than 35 days before the time of the request. Historical metrics
    -- are available for 35 days.
    startTime :: Data.POSIX,
    -- | The timestamp, in UNIX Epoch time format, at which to end the reporting
    -- interval for the retrieval of historical metrics data. The time must be
    -- later than the start time timestamp. It cannot be later than the current
    -- timestamp.
    --
    -- The time range between the start and end time must be less than 24
    -- hours.
    endTime :: Data.POSIX,
    -- | The filters to apply to returned metrics. You can filter on the
    -- following resources:
    --
    -- -   Queues
    --
    -- -   Routing profiles
    --
    -- -   Agents
    --
    -- -   Channels
    --
    -- -   User hierarchy groups
    --
    -- At least one filter must be passed from queues, routing profiles,
    -- agents, or user hierarchy groups.
    --
    -- To filter by phone number, see
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/create-historical-metrics-report.html Create a historical metrics report>
    -- in the /Amazon Connect Administrator\'s Guide/.
    --
    -- Note the following limits:
    --
    -- -   __Filter keys__: A maximum of 5 filter keys are supported in a
    --     single request. Valid filter keys: @QUEUE@ | @ROUTING_PROFILE@ |
    --     @AGENT@ | @CHANNEL@ | @AGENT_HIERARCHY_LEVEL_ONE@ |
    --     @AGENT_HIERARCHY_LEVEL_TWO@ | @AGENT_HIERARCHY_LEVEL_THREE@ |
    --     @AGENT_HIERARCHY_LEVEL_FOUR@ | @AGENT_HIERARCHY_LEVEL_FIVE@
    --
    -- -   __Filter values__: A maximum of 100 filter values are supported in a
    --     single request. For example, a @GetMetricDataV2@ request can filter
    --     by 50 queues, 35 agents, and 15 routing profiles for a total of 100
    --     filter values. @VOICE@, @CHAT@, and @TASK@ are valid @filterValue@
    --     for the @CHANNEL@ filter key.
    filters :: Prelude.NonEmpty FilterV2,
    -- | The metrics to retrieve. Specify the name, groupings, and filters for
    -- each metric. The following historical metrics are available. For a
    -- description of each metric, see
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/historical-metrics-definitions.html Historical metrics definitions>
    -- in the /Amazon Connect Administrator\'s Guide/.
    --
    -- [AGENT_ADHERENT_TIME]
    --     This metric is available only in Amazon Web Services Regions where
    --     <https://docs.aws.amazon.com/connect/latest/adminguide/regions.html#optimization_region Forecasting, capacity planning, and scheduling>
    --     is available.
    --
    --     Unit: Seconds
    --
    --     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
    --     Agent Hierarchy
    --
    -- [AGENT_NON_RESPONSE]
    --     Unit: Count
    --
    --     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
    --     Agent Hierarchy
    --
    -- [AGENT_OCCUPANCY]
    --     Unit: Percentage
    --
    --     Valid groupings and filters: Routing Profile, Agent, Agent Hierarchy
    --
    -- [AGENT_SCHEDULE_ADHERENCE]
    --     This metric is available only in Amazon Web Services Regions where
    --     <https://docs.aws.amazon.com/connect/latest/adminguide/regions.html#optimization_region Forecasting, capacity planning, and scheduling>
    --     is available.
    --
    --     Unit: Percent
    --
    --     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
    --     Agent Hierarchy
    --
    -- [AGENT_SCHEDULED_TIME]
    --     This metric is available only in Amazon Web Services Regions where
    --     <https://docs.aws.amazon.com/connect/latest/adminguide/regions.html#optimization_region Forecasting, capacity planning, and scheduling>
    --     is available.
    --
    --     Unit: Seconds
    --
    --     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
    --     Agent Hierarchy
    --
    -- [AVG_ABANDON_TIME]
    --     Unit: Seconds
    --
    --     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
    --     Agent Hierarchy
    --
    -- [AVG_AFTER_CONTACT_WORK_TIME]
    --     Unit: Seconds
    --
    --     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
    --     Agent Hierarchy
    --
    -- [AVG_AGENT_CONNECTING_TIME]
    --     Unit: Seconds
    --
    --     Valid metric filter key: @INITIATION_METHOD@. For now, this metric
    --     only supports the following as @INITIATION_METHOD@: @INBOUND@ |
    --     @OUTBOUND@ | @CALLBACK@ | @API@
    --
    --     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
    --     Agent Hierarchy
    --
    -- [AVG_HANDLE_TIME]
    --     Unit: Seconds
    --
    --     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
    --     Agent Hierarchy
    --
    -- [AVG_HOLD_TIME]
    --     Unit: Seconds
    --
    --     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
    --     Agent Hierarchy
    --
    -- [AVG_INTERACTION_AND_HOLD_TIME]
    --     Unit: Seconds
    --
    --     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
    --     Agent Hierarchy
    --
    -- [AVG_INTERACTION_TIME]
    --     Unit: Seconds
    --
    --     Valid groupings and filters: Queue, Channel, Routing Profile
    --
    -- [AVG_QUEUE_ANSWER_TIME]
    --     Unit: Seconds
    --
    --     Valid groupings and filters: Queue, Channel, Routing Profile
    --
    -- [CONTACTS_ABANDONED]
    --     Unit: Count
    --
    --     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
    --     Agent Hierarchy
    --
    -- [CONTACTS_CREATED]
    --     Unit: Count
    --
    --     Valid metric filter key: @INITIATION_METHOD@
    --
    --     Valid groupings and filters: Queue, Channel, Routing Profile
    --
    -- [CONTACTS_HANDLED]
    --     Unit: Count
    --
    --     Valid metric filter key: @INITIATION_METHOD@, @DISCONNECT_REASON@
    --
    --     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
    --     Agent Hierarchy
    --
    -- [CONTACTS_HOLD_ABANDONS]
    --     Unit: Count
    --
    --     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
    --     Agent Hierarchy
    --
    -- [CONTACTS_QUEUED]
    --     Unit: Count
    --
    --     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
    --     Agent Hierarchy
    --
    -- [CONTACTS_TRANSFERRED_OUT]
    --     Unit: Count
    --
    --     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
    --     Agent Hierarchy
    --
    -- [CONTACTS_TRANSFERRED_OUT_BY_AGENT]
    --     Unit: Count
    --
    --     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
    --     Agent Hierarchy
    --
    -- [CONTACTS_TRANSFERRED_OUT_FROM_QUEUE]
    --     Unit: Count
    --
    --     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
    --     Agent Hierarchy
    --
    -- [MAX_QUEUED_TIME]
    --     Unit: Seconds
    --
    --     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
    --     Agent Hierarchy
    --
    -- [SERVICE_LEVEL]
    --     You can include up to 20 SERVICE_LEVEL metrics in a request.
    --
    --     Unit: Percent
    --
    --     Valid groupings and filters: Queue, Channel, Routing Profile
    --
    --     Threshold: For @ThresholdValue@, enter any whole number from 1 to
    --     604800 (inclusive), in seconds. For @Comparison@, you must enter
    --     @LT@ (for \"Less than\").
    --
    -- [SUM_CONTACTS_ANSWERED_IN_X]
    --     Unit: Count
    --
    --     Valid groupings and filters: Queue, Channel, Routing Profile
    --
    --     Threshold: For @ThresholdValue@, enter any whole number from 1 to
    --     604800 (inclusive), in seconds. For @Comparison@, you must enter
    --     @LT@ (for \"Less than\").
    --
    -- [SUM_CONTACTS_ABANDONED_IN_X]
    --     Unit: Count
    --
    --     Valid groupings and filters: Queue, Channel, Routing Profile
    --
    --     Threshold: For @ThresholdValue@, enter any whole number from 1 to
    --     604800 (inclusive), in seconds. For @Comparison@, you must enter
    --     @LT@ (for \"Less than\").
    --
    -- [SUM_CONTACTS_DISCONNECTED]
    --     Valid metric filter key: @DISCONNECT_REASON@
    --
    --     Unit: Count
    --
    --     Valid groupings and filters: Queue, Channel, Routing Profile
    --
    -- [SUM_RETRY_CALLBACK_ATTEMPTS]
    --     Unit: Count
    --
    --     Valid groupings and filters: Queue, Channel, Routing Profile
    metrics :: [MetricV2]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMetricDataV2' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupings', 'getMetricDataV2_groupings' - The grouping applied to the metrics that are returned. For example, when
-- results are grouped by queue, the metrics returned are grouped by queue.
-- The values that are returned apply to the metrics for each queue. They
-- are not aggregated for all queues.
--
-- If no grouping is specified, a summary of all metrics is returned.
--
-- Valid grouping keys: @QUEUE@ | @ROUTING_PROFILE@ | @AGENT@ | @CHANNEL@ |
-- @AGENT_HIERARCHY_LEVEL_ONE@ | @AGENT_HIERARCHY_LEVEL_TWO@ |
-- @AGENT_HIERARCHY_LEVEL_THREE@ | @AGENT_HIERARCHY_LEVEL_FOUR@ |
-- @AGENT_HIERARCHY_LEVEL_FIVE@
--
-- 'maxResults', 'getMetricDataV2_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'getMetricDataV2_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'resourceArn', 'getMetricDataV2_resourceArn' - The Amazon Resource Name (ARN) of the resource. This includes the
-- @instanceId@ an Amazon Connect instance.
--
-- 'startTime', 'getMetricDataV2_startTime' - The timestamp, in UNIX Epoch time format, at which to start the
-- reporting interval for the retrieval of historical metrics data. The
-- time must be before the end time timestamp. The time range between the
-- start and end time must be less than 24 hours. The start time cannot be
-- earlier than 35 days before the time of the request. Historical metrics
-- are available for 35 days.
--
-- 'endTime', 'getMetricDataV2_endTime' - The timestamp, in UNIX Epoch time format, at which to end the reporting
-- interval for the retrieval of historical metrics data. The time must be
-- later than the start time timestamp. It cannot be later than the current
-- timestamp.
--
-- The time range between the start and end time must be less than 24
-- hours.
--
-- 'filters', 'getMetricDataV2_filters' - The filters to apply to returned metrics. You can filter on the
-- following resources:
--
-- -   Queues
--
-- -   Routing profiles
--
-- -   Agents
--
-- -   Channels
--
-- -   User hierarchy groups
--
-- At least one filter must be passed from queues, routing profiles,
-- agents, or user hierarchy groups.
--
-- To filter by phone number, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/create-historical-metrics-report.html Create a historical metrics report>
-- in the /Amazon Connect Administrator\'s Guide/.
--
-- Note the following limits:
--
-- -   __Filter keys__: A maximum of 5 filter keys are supported in a
--     single request. Valid filter keys: @QUEUE@ | @ROUTING_PROFILE@ |
--     @AGENT@ | @CHANNEL@ | @AGENT_HIERARCHY_LEVEL_ONE@ |
--     @AGENT_HIERARCHY_LEVEL_TWO@ | @AGENT_HIERARCHY_LEVEL_THREE@ |
--     @AGENT_HIERARCHY_LEVEL_FOUR@ | @AGENT_HIERARCHY_LEVEL_FIVE@
--
-- -   __Filter values__: A maximum of 100 filter values are supported in a
--     single request. For example, a @GetMetricDataV2@ request can filter
--     by 50 queues, 35 agents, and 15 routing profiles for a total of 100
--     filter values. @VOICE@, @CHAT@, and @TASK@ are valid @filterValue@
--     for the @CHANNEL@ filter key.
--
-- 'metrics', 'getMetricDataV2_metrics' - The metrics to retrieve. Specify the name, groupings, and filters for
-- each metric. The following historical metrics are available. For a
-- description of each metric, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/historical-metrics-definitions.html Historical metrics definitions>
-- in the /Amazon Connect Administrator\'s Guide/.
--
-- [AGENT_ADHERENT_TIME]
--     This metric is available only in Amazon Web Services Regions where
--     <https://docs.aws.amazon.com/connect/latest/adminguide/regions.html#optimization_region Forecasting, capacity planning, and scheduling>
--     is available.
--
--     Unit: Seconds
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [AGENT_NON_RESPONSE]
--     Unit: Count
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [AGENT_OCCUPANCY]
--     Unit: Percentage
--
--     Valid groupings and filters: Routing Profile, Agent, Agent Hierarchy
--
-- [AGENT_SCHEDULE_ADHERENCE]
--     This metric is available only in Amazon Web Services Regions where
--     <https://docs.aws.amazon.com/connect/latest/adminguide/regions.html#optimization_region Forecasting, capacity planning, and scheduling>
--     is available.
--
--     Unit: Percent
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [AGENT_SCHEDULED_TIME]
--     This metric is available only in Amazon Web Services Regions where
--     <https://docs.aws.amazon.com/connect/latest/adminguide/regions.html#optimization_region Forecasting, capacity planning, and scheduling>
--     is available.
--
--     Unit: Seconds
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [AVG_ABANDON_TIME]
--     Unit: Seconds
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [AVG_AFTER_CONTACT_WORK_TIME]
--     Unit: Seconds
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [AVG_AGENT_CONNECTING_TIME]
--     Unit: Seconds
--
--     Valid metric filter key: @INITIATION_METHOD@. For now, this metric
--     only supports the following as @INITIATION_METHOD@: @INBOUND@ |
--     @OUTBOUND@ | @CALLBACK@ | @API@
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [AVG_HANDLE_TIME]
--     Unit: Seconds
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [AVG_HOLD_TIME]
--     Unit: Seconds
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [AVG_INTERACTION_AND_HOLD_TIME]
--     Unit: Seconds
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [AVG_INTERACTION_TIME]
--     Unit: Seconds
--
--     Valid groupings and filters: Queue, Channel, Routing Profile
--
-- [AVG_QUEUE_ANSWER_TIME]
--     Unit: Seconds
--
--     Valid groupings and filters: Queue, Channel, Routing Profile
--
-- [CONTACTS_ABANDONED]
--     Unit: Count
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [CONTACTS_CREATED]
--     Unit: Count
--
--     Valid metric filter key: @INITIATION_METHOD@
--
--     Valid groupings and filters: Queue, Channel, Routing Profile
--
-- [CONTACTS_HANDLED]
--     Unit: Count
--
--     Valid metric filter key: @INITIATION_METHOD@, @DISCONNECT_REASON@
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [CONTACTS_HOLD_ABANDONS]
--     Unit: Count
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [CONTACTS_QUEUED]
--     Unit: Count
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [CONTACTS_TRANSFERRED_OUT]
--     Unit: Count
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [CONTACTS_TRANSFERRED_OUT_BY_AGENT]
--     Unit: Count
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [CONTACTS_TRANSFERRED_OUT_FROM_QUEUE]
--     Unit: Count
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [MAX_QUEUED_TIME]
--     Unit: Seconds
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [SERVICE_LEVEL]
--     You can include up to 20 SERVICE_LEVEL metrics in a request.
--
--     Unit: Percent
--
--     Valid groupings and filters: Queue, Channel, Routing Profile
--
--     Threshold: For @ThresholdValue@, enter any whole number from 1 to
--     604800 (inclusive), in seconds. For @Comparison@, you must enter
--     @LT@ (for \"Less than\").
--
-- [SUM_CONTACTS_ANSWERED_IN_X]
--     Unit: Count
--
--     Valid groupings and filters: Queue, Channel, Routing Profile
--
--     Threshold: For @ThresholdValue@, enter any whole number from 1 to
--     604800 (inclusive), in seconds. For @Comparison@, you must enter
--     @LT@ (for \"Less than\").
--
-- [SUM_CONTACTS_ABANDONED_IN_X]
--     Unit: Count
--
--     Valid groupings and filters: Queue, Channel, Routing Profile
--
--     Threshold: For @ThresholdValue@, enter any whole number from 1 to
--     604800 (inclusive), in seconds. For @Comparison@, you must enter
--     @LT@ (for \"Less than\").
--
-- [SUM_CONTACTS_DISCONNECTED]
--     Valid metric filter key: @DISCONNECT_REASON@
--
--     Unit: Count
--
--     Valid groupings and filters: Queue, Channel, Routing Profile
--
-- [SUM_RETRY_CALLBACK_ATTEMPTS]
--     Unit: Count
--
--     Valid groupings and filters: Queue, Channel, Routing Profile
newGetMetricDataV2 ::
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'endTime'
  Prelude.UTCTime ->
  -- | 'filters'
  Prelude.NonEmpty FilterV2 ->
  GetMetricDataV2
newGetMetricDataV2
  pResourceArn_
  pStartTime_
  pEndTime_
  pFilters_ =
    GetMetricDataV2'
      { groupings = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        resourceArn = pResourceArn_,
        startTime = Data._Time Lens.# pStartTime_,
        endTime = Data._Time Lens.# pEndTime_,
        filters = Lens.coerced Lens.# pFilters_,
        metrics = Prelude.mempty
      }

-- | The grouping applied to the metrics that are returned. For example, when
-- results are grouped by queue, the metrics returned are grouped by queue.
-- The values that are returned apply to the metrics for each queue. They
-- are not aggregated for all queues.
--
-- If no grouping is specified, a summary of all metrics is returned.
--
-- Valid grouping keys: @QUEUE@ | @ROUTING_PROFILE@ | @AGENT@ | @CHANNEL@ |
-- @AGENT_HIERARCHY_LEVEL_ONE@ | @AGENT_HIERARCHY_LEVEL_TWO@ |
-- @AGENT_HIERARCHY_LEVEL_THREE@ | @AGENT_HIERARCHY_LEVEL_FOUR@ |
-- @AGENT_HIERARCHY_LEVEL_FIVE@
getMetricDataV2_groupings :: Lens.Lens' GetMetricDataV2 (Prelude.Maybe [Prelude.Text])
getMetricDataV2_groupings = Lens.lens (\GetMetricDataV2' {groupings} -> groupings) (\s@GetMetricDataV2' {} a -> s {groupings = a} :: GetMetricDataV2) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return per page.
getMetricDataV2_maxResults :: Lens.Lens' GetMetricDataV2 (Prelude.Maybe Prelude.Natural)
getMetricDataV2_maxResults = Lens.lens (\GetMetricDataV2' {maxResults} -> maxResults) (\s@GetMetricDataV2' {} a -> s {maxResults = a} :: GetMetricDataV2)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
getMetricDataV2_nextToken :: Lens.Lens' GetMetricDataV2 (Prelude.Maybe Prelude.Text)
getMetricDataV2_nextToken = Lens.lens (\GetMetricDataV2' {nextToken} -> nextToken) (\s@GetMetricDataV2' {} a -> s {nextToken = a} :: GetMetricDataV2)

-- | The Amazon Resource Name (ARN) of the resource. This includes the
-- @instanceId@ an Amazon Connect instance.
getMetricDataV2_resourceArn :: Lens.Lens' GetMetricDataV2 Prelude.Text
getMetricDataV2_resourceArn = Lens.lens (\GetMetricDataV2' {resourceArn} -> resourceArn) (\s@GetMetricDataV2' {} a -> s {resourceArn = a} :: GetMetricDataV2)

-- | The timestamp, in UNIX Epoch time format, at which to start the
-- reporting interval for the retrieval of historical metrics data. The
-- time must be before the end time timestamp. The time range between the
-- start and end time must be less than 24 hours. The start time cannot be
-- earlier than 35 days before the time of the request. Historical metrics
-- are available for 35 days.
getMetricDataV2_startTime :: Lens.Lens' GetMetricDataV2 Prelude.UTCTime
getMetricDataV2_startTime = Lens.lens (\GetMetricDataV2' {startTime} -> startTime) (\s@GetMetricDataV2' {} a -> s {startTime = a} :: GetMetricDataV2) Prelude.. Data._Time

-- | The timestamp, in UNIX Epoch time format, at which to end the reporting
-- interval for the retrieval of historical metrics data. The time must be
-- later than the start time timestamp. It cannot be later than the current
-- timestamp.
--
-- The time range between the start and end time must be less than 24
-- hours.
getMetricDataV2_endTime :: Lens.Lens' GetMetricDataV2 Prelude.UTCTime
getMetricDataV2_endTime = Lens.lens (\GetMetricDataV2' {endTime} -> endTime) (\s@GetMetricDataV2' {} a -> s {endTime = a} :: GetMetricDataV2) Prelude.. Data._Time

-- | The filters to apply to returned metrics. You can filter on the
-- following resources:
--
-- -   Queues
--
-- -   Routing profiles
--
-- -   Agents
--
-- -   Channels
--
-- -   User hierarchy groups
--
-- At least one filter must be passed from queues, routing profiles,
-- agents, or user hierarchy groups.
--
-- To filter by phone number, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/create-historical-metrics-report.html Create a historical metrics report>
-- in the /Amazon Connect Administrator\'s Guide/.
--
-- Note the following limits:
--
-- -   __Filter keys__: A maximum of 5 filter keys are supported in a
--     single request. Valid filter keys: @QUEUE@ | @ROUTING_PROFILE@ |
--     @AGENT@ | @CHANNEL@ | @AGENT_HIERARCHY_LEVEL_ONE@ |
--     @AGENT_HIERARCHY_LEVEL_TWO@ | @AGENT_HIERARCHY_LEVEL_THREE@ |
--     @AGENT_HIERARCHY_LEVEL_FOUR@ | @AGENT_HIERARCHY_LEVEL_FIVE@
--
-- -   __Filter values__: A maximum of 100 filter values are supported in a
--     single request. For example, a @GetMetricDataV2@ request can filter
--     by 50 queues, 35 agents, and 15 routing profiles for a total of 100
--     filter values. @VOICE@, @CHAT@, and @TASK@ are valid @filterValue@
--     for the @CHANNEL@ filter key.
getMetricDataV2_filters :: Lens.Lens' GetMetricDataV2 (Prelude.NonEmpty FilterV2)
getMetricDataV2_filters = Lens.lens (\GetMetricDataV2' {filters} -> filters) (\s@GetMetricDataV2' {} a -> s {filters = a} :: GetMetricDataV2) Prelude.. Lens.coerced

-- | The metrics to retrieve. Specify the name, groupings, and filters for
-- each metric. The following historical metrics are available. For a
-- description of each metric, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/historical-metrics-definitions.html Historical metrics definitions>
-- in the /Amazon Connect Administrator\'s Guide/.
--
-- [AGENT_ADHERENT_TIME]
--     This metric is available only in Amazon Web Services Regions where
--     <https://docs.aws.amazon.com/connect/latest/adminguide/regions.html#optimization_region Forecasting, capacity planning, and scheduling>
--     is available.
--
--     Unit: Seconds
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [AGENT_NON_RESPONSE]
--     Unit: Count
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [AGENT_OCCUPANCY]
--     Unit: Percentage
--
--     Valid groupings and filters: Routing Profile, Agent, Agent Hierarchy
--
-- [AGENT_SCHEDULE_ADHERENCE]
--     This metric is available only in Amazon Web Services Regions where
--     <https://docs.aws.amazon.com/connect/latest/adminguide/regions.html#optimization_region Forecasting, capacity planning, and scheduling>
--     is available.
--
--     Unit: Percent
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [AGENT_SCHEDULED_TIME]
--     This metric is available only in Amazon Web Services Regions where
--     <https://docs.aws.amazon.com/connect/latest/adminguide/regions.html#optimization_region Forecasting, capacity planning, and scheduling>
--     is available.
--
--     Unit: Seconds
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [AVG_ABANDON_TIME]
--     Unit: Seconds
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [AVG_AFTER_CONTACT_WORK_TIME]
--     Unit: Seconds
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [AVG_AGENT_CONNECTING_TIME]
--     Unit: Seconds
--
--     Valid metric filter key: @INITIATION_METHOD@. For now, this metric
--     only supports the following as @INITIATION_METHOD@: @INBOUND@ |
--     @OUTBOUND@ | @CALLBACK@ | @API@
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [AVG_HANDLE_TIME]
--     Unit: Seconds
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [AVG_HOLD_TIME]
--     Unit: Seconds
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [AVG_INTERACTION_AND_HOLD_TIME]
--     Unit: Seconds
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [AVG_INTERACTION_TIME]
--     Unit: Seconds
--
--     Valid groupings and filters: Queue, Channel, Routing Profile
--
-- [AVG_QUEUE_ANSWER_TIME]
--     Unit: Seconds
--
--     Valid groupings and filters: Queue, Channel, Routing Profile
--
-- [CONTACTS_ABANDONED]
--     Unit: Count
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [CONTACTS_CREATED]
--     Unit: Count
--
--     Valid metric filter key: @INITIATION_METHOD@
--
--     Valid groupings and filters: Queue, Channel, Routing Profile
--
-- [CONTACTS_HANDLED]
--     Unit: Count
--
--     Valid metric filter key: @INITIATION_METHOD@, @DISCONNECT_REASON@
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [CONTACTS_HOLD_ABANDONS]
--     Unit: Count
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [CONTACTS_QUEUED]
--     Unit: Count
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [CONTACTS_TRANSFERRED_OUT]
--     Unit: Count
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [CONTACTS_TRANSFERRED_OUT_BY_AGENT]
--     Unit: Count
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [CONTACTS_TRANSFERRED_OUT_FROM_QUEUE]
--     Unit: Count
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [MAX_QUEUED_TIME]
--     Unit: Seconds
--
--     Valid groupings and filters: Queue, Channel, Routing Profile, Agent,
--     Agent Hierarchy
--
-- [SERVICE_LEVEL]
--     You can include up to 20 SERVICE_LEVEL metrics in a request.
--
--     Unit: Percent
--
--     Valid groupings and filters: Queue, Channel, Routing Profile
--
--     Threshold: For @ThresholdValue@, enter any whole number from 1 to
--     604800 (inclusive), in seconds. For @Comparison@, you must enter
--     @LT@ (for \"Less than\").
--
-- [SUM_CONTACTS_ANSWERED_IN_X]
--     Unit: Count
--
--     Valid groupings and filters: Queue, Channel, Routing Profile
--
--     Threshold: For @ThresholdValue@, enter any whole number from 1 to
--     604800 (inclusive), in seconds. For @Comparison@, you must enter
--     @LT@ (for \"Less than\").
--
-- [SUM_CONTACTS_ABANDONED_IN_X]
--     Unit: Count
--
--     Valid groupings and filters: Queue, Channel, Routing Profile
--
--     Threshold: For @ThresholdValue@, enter any whole number from 1 to
--     604800 (inclusive), in seconds. For @Comparison@, you must enter
--     @LT@ (for \"Less than\").
--
-- [SUM_CONTACTS_DISCONNECTED]
--     Valid metric filter key: @DISCONNECT_REASON@
--
--     Unit: Count
--
--     Valid groupings and filters: Queue, Channel, Routing Profile
--
-- [SUM_RETRY_CALLBACK_ATTEMPTS]
--     Unit: Count
--
--     Valid groupings and filters: Queue, Channel, Routing Profile
getMetricDataV2_metrics :: Lens.Lens' GetMetricDataV2 [MetricV2]
getMetricDataV2_metrics = Lens.lens (\GetMetricDataV2' {metrics} -> metrics) (\s@GetMetricDataV2' {} a -> s {metrics = a} :: GetMetricDataV2) Prelude.. Lens.coerced

instance Core.AWSRequest GetMetricDataV2 where
  type
    AWSResponse GetMetricDataV2 =
      GetMetricDataV2Response
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMetricDataV2Response'
            Prelude.<$> (x Data..?> "MetricResults" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMetricDataV2 where
  hashWithSalt _salt GetMetricDataV2' {..} =
    _salt
      `Prelude.hashWithSalt` groupings
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` metrics

instance Prelude.NFData GetMetricDataV2 where
  rnf GetMetricDataV2' {..} =
    Prelude.rnf groupings
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf metrics

instance Data.ToHeaders GetMetricDataV2 where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetMetricDataV2 where
  toJSON GetMetricDataV2' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Groupings" Data..=) Prelude.<$> groupings,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("ResourceArn" Data..= resourceArn),
            Prelude.Just ("StartTime" Data..= startTime),
            Prelude.Just ("EndTime" Data..= endTime),
            Prelude.Just ("Filters" Data..= filters),
            Prelude.Just ("Metrics" Data..= metrics)
          ]
      )

instance Data.ToPath GetMetricDataV2 where
  toPath = Prelude.const "/metrics/data"

instance Data.ToQuery GetMetricDataV2 where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMetricDataV2Response' smart constructor.
data GetMetricDataV2Response = GetMetricDataV2Response'
  { -- | Information about the metrics requested in the API request If no
    -- grouping is specified, a summary of metric data is returned.
    metricResults :: Prelude.Maybe [MetricResultV2],
    -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMetricDataV2Response' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricResults', 'getMetricDataV2Response_metricResults' - Information about the metrics requested in the API request If no
-- grouping is specified, a summary of metric data is returned.
--
-- 'nextToken', 'getMetricDataV2Response_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'httpStatus', 'getMetricDataV2Response_httpStatus' - The response's http status code.
newGetMetricDataV2Response ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMetricDataV2Response
newGetMetricDataV2Response pHttpStatus_ =
  GetMetricDataV2Response'
    { metricResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the metrics requested in the API request If no
-- grouping is specified, a summary of metric data is returned.
getMetricDataV2Response_metricResults :: Lens.Lens' GetMetricDataV2Response (Prelude.Maybe [MetricResultV2])
getMetricDataV2Response_metricResults = Lens.lens (\GetMetricDataV2Response' {metricResults} -> metricResults) (\s@GetMetricDataV2Response' {} a -> s {metricResults = a} :: GetMetricDataV2Response) Prelude.. Lens.mapping Lens.coerced

-- | If there are additional results, this is the token for the next set of
-- results.
getMetricDataV2Response_nextToken :: Lens.Lens' GetMetricDataV2Response (Prelude.Maybe Prelude.Text)
getMetricDataV2Response_nextToken = Lens.lens (\GetMetricDataV2Response' {nextToken} -> nextToken) (\s@GetMetricDataV2Response' {} a -> s {nextToken = a} :: GetMetricDataV2Response)

-- | The response's http status code.
getMetricDataV2Response_httpStatus :: Lens.Lens' GetMetricDataV2Response Prelude.Int
getMetricDataV2Response_httpStatus = Lens.lens (\GetMetricDataV2Response' {httpStatus} -> httpStatus) (\s@GetMetricDataV2Response' {} a -> s {httpStatus = a} :: GetMetricDataV2Response)

instance Prelude.NFData GetMetricDataV2Response where
  rnf GetMetricDataV2Response' {..} =
    Prelude.rnf metricResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
