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
-- Module      : Amazonka.SESV2.Types.BatchGetMetricDataQuery
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.BatchGetMetricDataQuery where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.Metric
import Amazonka.SESV2.Types.MetricDimensionName
import Amazonka.SESV2.Types.MetricNamespace

-- | Represents a single metric data query to include in a batch.
--
-- /See:/ 'newBatchGetMetricDataQuery' smart constructor.
data BatchGetMetricDataQuery = BatchGetMetricDataQuery'
  { -- | An object that contains mapping between @MetricDimensionName@ and
    -- @MetricDimensionValue@ to filter metrics by.
    dimensions :: Prelude.Maybe (Prelude.HashMap MetricDimensionName Prelude.Text),
    -- | The query identifier.
    id :: Prelude.Text,
    -- | The query namespace - e.g. @VDM@
    namespace :: MetricNamespace,
    -- | The queried metric. This can be one of the following:
    --
    -- -   @SEND@ – Emails sent eligible for tracking in the VDM dashboard.
    --     This excludes emails sent to the mailbox simulator and emails
    --     addressed to more than one recipient.
    --
    -- -   @COMPLAINT@ – Complaints received for your account. This excludes
    --     complaints from the mailbox simulator, those originating from your
    --     account-level suppression list (if enabled), and those for emails
    --     addressed to more than one recipient
    --
    -- -   @PERMANENT_BOUNCE@ – Permanent bounces - i.e. feedback received for
    --     emails sent to non-existent mailboxes. Excludes bounces from the
    --     mailbox simulator, those originating from your account-level
    --     suppression list (if enabled), and those for emails addressed to
    --     more than one recipient.
    --
    -- -   @TRANSIENT_BOUNCE@ – Transient bounces - i.e. feedback received for
    --     delivery failures excluding issues with non-existent mailboxes.
    --     Excludes bounces from the mailbox simulator, and those for emails
    --     addressed to more than one recipient.
    --
    -- -   @OPEN@ – Unique open events for emails including open trackers.
    --     Excludes opens for emails addressed to more than one recipient.
    --
    -- -   @CLICK@ – Unique click events for emails including wrapped links.
    --     Excludes clicks for emails addressed to more than one recipient.
    --
    -- -   @DELIVERY@ – Successful deliveries for email sending attempts.
    --     Excludes deliveries to the mailbox simulator and for emails
    --     addressed to more than one recipient.
    --
    -- -   @DELIVERY_OPEN@ – Successful deliveries for email sending attempts.
    --     Excludes deliveries to the mailbox simulator, for emails addressed
    --     to more than one recipient, and emails without open trackers.
    --
    -- -   @DELIVERY_CLICK@ – Successful deliveries for email sending attempts.
    --     Excludes deliveries to the mailbox simulator, for emails addressed
    --     to more than one recipient, and emails without click trackers.
    --
    -- -   @DELIVERY_COMPLAINT@ – Successful deliveries for email sending
    --     attempts. Excludes deliveries to the mailbox simulator, for emails
    --     addressed to more than one recipient, and emails addressed to
    --     recipients hosted by ISPs with which Amazon SES does not have a
    --     feedback loop agreement.
    metric :: Metric,
    -- | Represents the start date for the query interval.
    startDate :: Data.POSIX,
    -- | Represents the end date for the query interval.
    endDate :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetMetricDataQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensions', 'batchGetMetricDataQuery_dimensions' - An object that contains mapping between @MetricDimensionName@ and
-- @MetricDimensionValue@ to filter metrics by.
--
-- 'id', 'batchGetMetricDataQuery_id' - The query identifier.
--
-- 'namespace', 'batchGetMetricDataQuery_namespace' - The query namespace - e.g. @VDM@
--
-- 'metric', 'batchGetMetricDataQuery_metric' - The queried metric. This can be one of the following:
--
-- -   @SEND@ – Emails sent eligible for tracking in the VDM dashboard.
--     This excludes emails sent to the mailbox simulator and emails
--     addressed to more than one recipient.
--
-- -   @COMPLAINT@ – Complaints received for your account. This excludes
--     complaints from the mailbox simulator, those originating from your
--     account-level suppression list (if enabled), and those for emails
--     addressed to more than one recipient
--
-- -   @PERMANENT_BOUNCE@ – Permanent bounces - i.e. feedback received for
--     emails sent to non-existent mailboxes. Excludes bounces from the
--     mailbox simulator, those originating from your account-level
--     suppression list (if enabled), and those for emails addressed to
--     more than one recipient.
--
-- -   @TRANSIENT_BOUNCE@ – Transient bounces - i.e. feedback received for
--     delivery failures excluding issues with non-existent mailboxes.
--     Excludes bounces from the mailbox simulator, and those for emails
--     addressed to more than one recipient.
--
-- -   @OPEN@ – Unique open events for emails including open trackers.
--     Excludes opens for emails addressed to more than one recipient.
--
-- -   @CLICK@ – Unique click events for emails including wrapped links.
--     Excludes clicks for emails addressed to more than one recipient.
--
-- -   @DELIVERY@ – Successful deliveries for email sending attempts.
--     Excludes deliveries to the mailbox simulator and for emails
--     addressed to more than one recipient.
--
-- -   @DELIVERY_OPEN@ – Successful deliveries for email sending attempts.
--     Excludes deliveries to the mailbox simulator, for emails addressed
--     to more than one recipient, and emails without open trackers.
--
-- -   @DELIVERY_CLICK@ – Successful deliveries for email sending attempts.
--     Excludes deliveries to the mailbox simulator, for emails addressed
--     to more than one recipient, and emails without click trackers.
--
-- -   @DELIVERY_COMPLAINT@ – Successful deliveries for email sending
--     attempts. Excludes deliveries to the mailbox simulator, for emails
--     addressed to more than one recipient, and emails addressed to
--     recipients hosted by ISPs with which Amazon SES does not have a
--     feedback loop agreement.
--
-- 'startDate', 'batchGetMetricDataQuery_startDate' - Represents the start date for the query interval.
--
-- 'endDate', 'batchGetMetricDataQuery_endDate' - Represents the end date for the query interval.
newBatchGetMetricDataQuery ::
  -- | 'id'
  Prelude.Text ->
  -- | 'namespace'
  MetricNamespace ->
  -- | 'metric'
  Metric ->
  -- | 'startDate'
  Prelude.UTCTime ->
  -- | 'endDate'
  Prelude.UTCTime ->
  BatchGetMetricDataQuery
newBatchGetMetricDataQuery
  pId_
  pNamespace_
  pMetric_
  pStartDate_
  pEndDate_ =
    BatchGetMetricDataQuery'
      { dimensions =
          Prelude.Nothing,
        id = pId_,
        namespace = pNamespace_,
        metric = pMetric_,
        startDate = Data._Time Lens.# pStartDate_,
        endDate = Data._Time Lens.# pEndDate_
      }

-- | An object that contains mapping between @MetricDimensionName@ and
-- @MetricDimensionValue@ to filter metrics by.
batchGetMetricDataQuery_dimensions :: Lens.Lens' BatchGetMetricDataQuery (Prelude.Maybe (Prelude.HashMap MetricDimensionName Prelude.Text))
batchGetMetricDataQuery_dimensions = Lens.lens (\BatchGetMetricDataQuery' {dimensions} -> dimensions) (\s@BatchGetMetricDataQuery' {} a -> s {dimensions = a} :: BatchGetMetricDataQuery) Prelude.. Lens.mapping Lens.coerced

-- | The query identifier.
batchGetMetricDataQuery_id :: Lens.Lens' BatchGetMetricDataQuery Prelude.Text
batchGetMetricDataQuery_id = Lens.lens (\BatchGetMetricDataQuery' {id} -> id) (\s@BatchGetMetricDataQuery' {} a -> s {id = a} :: BatchGetMetricDataQuery)

-- | The query namespace - e.g. @VDM@
batchGetMetricDataQuery_namespace :: Lens.Lens' BatchGetMetricDataQuery MetricNamespace
batchGetMetricDataQuery_namespace = Lens.lens (\BatchGetMetricDataQuery' {namespace} -> namespace) (\s@BatchGetMetricDataQuery' {} a -> s {namespace = a} :: BatchGetMetricDataQuery)

-- | The queried metric. This can be one of the following:
--
-- -   @SEND@ – Emails sent eligible for tracking in the VDM dashboard.
--     This excludes emails sent to the mailbox simulator and emails
--     addressed to more than one recipient.
--
-- -   @COMPLAINT@ – Complaints received for your account. This excludes
--     complaints from the mailbox simulator, those originating from your
--     account-level suppression list (if enabled), and those for emails
--     addressed to more than one recipient
--
-- -   @PERMANENT_BOUNCE@ – Permanent bounces - i.e. feedback received for
--     emails sent to non-existent mailboxes. Excludes bounces from the
--     mailbox simulator, those originating from your account-level
--     suppression list (if enabled), and those for emails addressed to
--     more than one recipient.
--
-- -   @TRANSIENT_BOUNCE@ – Transient bounces - i.e. feedback received for
--     delivery failures excluding issues with non-existent mailboxes.
--     Excludes bounces from the mailbox simulator, and those for emails
--     addressed to more than one recipient.
--
-- -   @OPEN@ – Unique open events for emails including open trackers.
--     Excludes opens for emails addressed to more than one recipient.
--
-- -   @CLICK@ – Unique click events for emails including wrapped links.
--     Excludes clicks for emails addressed to more than one recipient.
--
-- -   @DELIVERY@ – Successful deliveries for email sending attempts.
--     Excludes deliveries to the mailbox simulator and for emails
--     addressed to more than one recipient.
--
-- -   @DELIVERY_OPEN@ – Successful deliveries for email sending attempts.
--     Excludes deliveries to the mailbox simulator, for emails addressed
--     to more than one recipient, and emails without open trackers.
--
-- -   @DELIVERY_CLICK@ – Successful deliveries for email sending attempts.
--     Excludes deliveries to the mailbox simulator, for emails addressed
--     to more than one recipient, and emails without click trackers.
--
-- -   @DELIVERY_COMPLAINT@ – Successful deliveries for email sending
--     attempts. Excludes deliveries to the mailbox simulator, for emails
--     addressed to more than one recipient, and emails addressed to
--     recipients hosted by ISPs with which Amazon SES does not have a
--     feedback loop agreement.
batchGetMetricDataQuery_metric :: Lens.Lens' BatchGetMetricDataQuery Metric
batchGetMetricDataQuery_metric = Lens.lens (\BatchGetMetricDataQuery' {metric} -> metric) (\s@BatchGetMetricDataQuery' {} a -> s {metric = a} :: BatchGetMetricDataQuery)

-- | Represents the start date for the query interval.
batchGetMetricDataQuery_startDate :: Lens.Lens' BatchGetMetricDataQuery Prelude.UTCTime
batchGetMetricDataQuery_startDate = Lens.lens (\BatchGetMetricDataQuery' {startDate} -> startDate) (\s@BatchGetMetricDataQuery' {} a -> s {startDate = a} :: BatchGetMetricDataQuery) Prelude.. Data._Time

-- | Represents the end date for the query interval.
batchGetMetricDataQuery_endDate :: Lens.Lens' BatchGetMetricDataQuery Prelude.UTCTime
batchGetMetricDataQuery_endDate = Lens.lens (\BatchGetMetricDataQuery' {endDate} -> endDate) (\s@BatchGetMetricDataQuery' {} a -> s {endDate = a} :: BatchGetMetricDataQuery) Prelude.. Data._Time

instance Prelude.Hashable BatchGetMetricDataQuery where
  hashWithSalt _salt BatchGetMetricDataQuery' {..} =
    _salt `Prelude.hashWithSalt` dimensions
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` metric
      `Prelude.hashWithSalt` startDate
      `Prelude.hashWithSalt` endDate

instance Prelude.NFData BatchGetMetricDataQuery where
  rnf BatchGetMetricDataQuery' {..} =
    Prelude.rnf dimensions
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf metric
      `Prelude.seq` Prelude.rnf startDate
      `Prelude.seq` Prelude.rnf endDate

instance Data.ToJSON BatchGetMetricDataQuery where
  toJSON BatchGetMetricDataQuery' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Dimensions" Data..=) Prelude.<$> dimensions,
            Prelude.Just ("Id" Data..= id),
            Prelude.Just ("Namespace" Data..= namespace),
            Prelude.Just ("Metric" Data..= metric),
            Prelude.Just ("StartDate" Data..= startDate),
            Prelude.Just ("EndDate" Data..= endDate)
          ]
      )
