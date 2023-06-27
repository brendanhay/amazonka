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
-- Module      : Amazonka.Pi.ListAvailableResourceMetrics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve metrics of the specified types that can be queried for a
-- specified DB instance.
module Amazonka.Pi.ListAvailableResourceMetrics
  ( -- * Creating a Request
    ListAvailableResourceMetrics (..),
    newListAvailableResourceMetrics,

    -- * Request Lenses
    listAvailableResourceMetrics_maxResults,
    listAvailableResourceMetrics_nextToken,
    listAvailableResourceMetrics_serviceType,
    listAvailableResourceMetrics_identifier,
    listAvailableResourceMetrics_metricTypes,

    -- * Destructuring the Response
    ListAvailableResourceMetricsResponse (..),
    newListAvailableResourceMetricsResponse,

    -- * Response Lenses
    listAvailableResourceMetricsResponse_metrics,
    listAvailableResourceMetricsResponse_nextToken,
    listAvailableResourceMetricsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pi.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAvailableResourceMetrics' smart constructor.
data ListAvailableResourceMetrics = ListAvailableResourceMetrics'
  { -- | The maximum number of items to return. If the @MaxRecords@ value is less
    -- than the number of existing items, the response includes a pagination
    -- token.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- token, up to the value specified by @MaxRecords@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services service for which Performance Insights returns
    -- metrics.
    serviceType :: ServiceType,
    -- | An immutable identifier for a data source that is unique within an
    -- Amazon Web Services Region. Performance Insights gathers metrics from
    -- this data source. To use an Amazon RDS DB instance as a data source,
    -- specify its @DbiResourceId@ value. For example, specify
    -- @db-ABCDEFGHIJKLMNOPQRSTU1VWZ@.
    identifier :: Prelude.Text,
    -- | The types of metrics to return in the response. Valid values in the
    -- array include the following:
    --
    -- -   @os@ (OS counter metrics) - All engines
    --
    -- -   @db@ (DB load metrics) - All engines except for Amazon DocumentDB
    --
    -- -   @db.sql.stats@ (per-SQL metrics) - All engines except for Amazon
    --     DocumentDB
    --
    -- -   @db.sql_tokenized.stats@ (per-SQL digest metrics) - All engines
    --     except for Amazon DocumentDB
    metricTypes :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAvailableResourceMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAvailableResourceMetrics_maxResults' - The maximum number of items to return. If the @MaxRecords@ value is less
-- than the number of existing items, the response includes a pagination
-- token.
--
-- 'nextToken', 'listAvailableResourceMetrics_nextToken' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- token, up to the value specified by @MaxRecords@.
--
-- 'serviceType', 'listAvailableResourceMetrics_serviceType' - The Amazon Web Services service for which Performance Insights returns
-- metrics.
--
-- 'identifier', 'listAvailableResourceMetrics_identifier' - An immutable identifier for a data source that is unique within an
-- Amazon Web Services Region. Performance Insights gathers metrics from
-- this data source. To use an Amazon RDS DB instance as a data source,
-- specify its @DbiResourceId@ value. For example, specify
-- @db-ABCDEFGHIJKLMNOPQRSTU1VWZ@.
--
-- 'metricTypes', 'listAvailableResourceMetrics_metricTypes' - The types of metrics to return in the response. Valid values in the
-- array include the following:
--
-- -   @os@ (OS counter metrics) - All engines
--
-- -   @db@ (DB load metrics) - All engines except for Amazon DocumentDB
--
-- -   @db.sql.stats@ (per-SQL metrics) - All engines except for Amazon
--     DocumentDB
--
-- -   @db.sql_tokenized.stats@ (per-SQL digest metrics) - All engines
--     except for Amazon DocumentDB
newListAvailableResourceMetrics ::
  -- | 'serviceType'
  ServiceType ->
  -- | 'identifier'
  Prelude.Text ->
  ListAvailableResourceMetrics
newListAvailableResourceMetrics
  pServiceType_
  pIdentifier_ =
    ListAvailableResourceMetrics'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        serviceType = pServiceType_,
        identifier = pIdentifier_,
        metricTypes = Prelude.mempty
      }

-- | The maximum number of items to return. If the @MaxRecords@ value is less
-- than the number of existing items, the response includes a pagination
-- token.
listAvailableResourceMetrics_maxResults :: Lens.Lens' ListAvailableResourceMetrics (Prelude.Maybe Prelude.Natural)
listAvailableResourceMetrics_maxResults = Lens.lens (\ListAvailableResourceMetrics' {maxResults} -> maxResults) (\s@ListAvailableResourceMetrics' {} a -> s {maxResults = a} :: ListAvailableResourceMetrics)

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- token, up to the value specified by @MaxRecords@.
listAvailableResourceMetrics_nextToken :: Lens.Lens' ListAvailableResourceMetrics (Prelude.Maybe Prelude.Text)
listAvailableResourceMetrics_nextToken = Lens.lens (\ListAvailableResourceMetrics' {nextToken} -> nextToken) (\s@ListAvailableResourceMetrics' {} a -> s {nextToken = a} :: ListAvailableResourceMetrics)

-- | The Amazon Web Services service for which Performance Insights returns
-- metrics.
listAvailableResourceMetrics_serviceType :: Lens.Lens' ListAvailableResourceMetrics ServiceType
listAvailableResourceMetrics_serviceType = Lens.lens (\ListAvailableResourceMetrics' {serviceType} -> serviceType) (\s@ListAvailableResourceMetrics' {} a -> s {serviceType = a} :: ListAvailableResourceMetrics)

-- | An immutable identifier for a data source that is unique within an
-- Amazon Web Services Region. Performance Insights gathers metrics from
-- this data source. To use an Amazon RDS DB instance as a data source,
-- specify its @DbiResourceId@ value. For example, specify
-- @db-ABCDEFGHIJKLMNOPQRSTU1VWZ@.
listAvailableResourceMetrics_identifier :: Lens.Lens' ListAvailableResourceMetrics Prelude.Text
listAvailableResourceMetrics_identifier = Lens.lens (\ListAvailableResourceMetrics' {identifier} -> identifier) (\s@ListAvailableResourceMetrics' {} a -> s {identifier = a} :: ListAvailableResourceMetrics)

-- | The types of metrics to return in the response. Valid values in the
-- array include the following:
--
-- -   @os@ (OS counter metrics) - All engines
--
-- -   @db@ (DB load metrics) - All engines except for Amazon DocumentDB
--
-- -   @db.sql.stats@ (per-SQL metrics) - All engines except for Amazon
--     DocumentDB
--
-- -   @db.sql_tokenized.stats@ (per-SQL digest metrics) - All engines
--     except for Amazon DocumentDB
listAvailableResourceMetrics_metricTypes :: Lens.Lens' ListAvailableResourceMetrics [Prelude.Text]
listAvailableResourceMetrics_metricTypes = Lens.lens (\ListAvailableResourceMetrics' {metricTypes} -> metricTypes) (\s@ListAvailableResourceMetrics' {} a -> s {metricTypes = a} :: ListAvailableResourceMetrics) Prelude.. Lens.coerced

instance Core.AWSRequest ListAvailableResourceMetrics where
  type
    AWSResponse ListAvailableResourceMetrics =
      ListAvailableResourceMetricsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAvailableResourceMetricsResponse'
            Prelude.<$> (x Data..?> "Metrics" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListAvailableResourceMetrics
  where
  hashWithSalt _salt ListAvailableResourceMetrics' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` serviceType
      `Prelude.hashWithSalt` identifier
      `Prelude.hashWithSalt` metricTypes

instance Prelude.NFData ListAvailableResourceMetrics where
  rnf ListAvailableResourceMetrics' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf serviceType
      `Prelude.seq` Prelude.rnf identifier
      `Prelude.seq` Prelude.rnf metricTypes

instance Data.ToHeaders ListAvailableResourceMetrics where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PerformanceInsightsv20180227.ListAvailableResourceMetrics" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAvailableResourceMetrics where
  toJSON ListAvailableResourceMetrics' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("ServiceType" Data..= serviceType),
            Prelude.Just ("Identifier" Data..= identifier),
            Prelude.Just ("MetricTypes" Data..= metricTypes)
          ]
      )

instance Data.ToPath ListAvailableResourceMetrics where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAvailableResourceMetrics where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAvailableResourceMetricsResponse' smart constructor.
data ListAvailableResourceMetricsResponse = ListAvailableResourceMetricsResponse'
  { -- | An array of metrics available to query. Each array element contains the
    -- full name, description, and unit of the metric.
    metrics :: Prelude.Maybe [ResponseResourceMetric],
    -- | A pagination token that indicates the response didn’t return all
    -- available records because @MaxRecords@ was specified in the previous
    -- request. To get the remaining records, specify @NextToken@ in a separate
    -- request with this value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAvailableResourceMetricsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metrics', 'listAvailableResourceMetricsResponse_metrics' - An array of metrics available to query. Each array element contains the
-- full name, description, and unit of the metric.
--
-- 'nextToken', 'listAvailableResourceMetricsResponse_nextToken' - A pagination token that indicates the response didn’t return all
-- available records because @MaxRecords@ was specified in the previous
-- request. To get the remaining records, specify @NextToken@ in a separate
-- request with this value.
--
-- 'httpStatus', 'listAvailableResourceMetricsResponse_httpStatus' - The response's http status code.
newListAvailableResourceMetricsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAvailableResourceMetricsResponse
newListAvailableResourceMetricsResponse pHttpStatus_ =
  ListAvailableResourceMetricsResponse'
    { metrics =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of metrics available to query. Each array element contains the
-- full name, description, and unit of the metric.
listAvailableResourceMetricsResponse_metrics :: Lens.Lens' ListAvailableResourceMetricsResponse (Prelude.Maybe [ResponseResourceMetric])
listAvailableResourceMetricsResponse_metrics = Lens.lens (\ListAvailableResourceMetricsResponse' {metrics} -> metrics) (\s@ListAvailableResourceMetricsResponse' {} a -> s {metrics = a} :: ListAvailableResourceMetricsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A pagination token that indicates the response didn’t return all
-- available records because @MaxRecords@ was specified in the previous
-- request. To get the remaining records, specify @NextToken@ in a separate
-- request with this value.
listAvailableResourceMetricsResponse_nextToken :: Lens.Lens' ListAvailableResourceMetricsResponse (Prelude.Maybe Prelude.Text)
listAvailableResourceMetricsResponse_nextToken = Lens.lens (\ListAvailableResourceMetricsResponse' {nextToken} -> nextToken) (\s@ListAvailableResourceMetricsResponse' {} a -> s {nextToken = a} :: ListAvailableResourceMetricsResponse)

-- | The response's http status code.
listAvailableResourceMetricsResponse_httpStatus :: Lens.Lens' ListAvailableResourceMetricsResponse Prelude.Int
listAvailableResourceMetricsResponse_httpStatus = Lens.lens (\ListAvailableResourceMetricsResponse' {httpStatus} -> httpStatus) (\s@ListAvailableResourceMetricsResponse' {} a -> s {httpStatus = a} :: ListAvailableResourceMetricsResponse)

instance
  Prelude.NFData
    ListAvailableResourceMetricsResponse
  where
  rnf ListAvailableResourceMetricsResponse' {..} =
    Prelude.rnf metrics
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
