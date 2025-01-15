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
-- Module      : Amazonka.IoT.GetStatistics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the count, average, sum, minimum, maximum, sum of squares,
-- variance, and standard deviation for the specified aggregated field. If
-- the aggregation field is of type @String@, only the count statistic is
-- returned.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions GetStatistics>
-- action.
module Amazonka.IoT.GetStatistics
  ( -- * Creating a Request
    GetStatistics (..),
    newGetStatistics,

    -- * Request Lenses
    getStatistics_aggregationField,
    getStatistics_indexName,
    getStatistics_queryVersion,
    getStatistics_queryString,

    -- * Destructuring the Response
    GetStatisticsResponse (..),
    newGetStatisticsResponse,

    -- * Response Lenses
    getStatisticsResponse_statistics,
    getStatisticsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetStatistics' smart constructor.
data GetStatistics = GetStatistics'
  { -- | The aggregation field name.
    aggregationField :: Prelude.Maybe Prelude.Text,
    -- | The name of the index to search. The default value is @AWS_Things@.
    indexName :: Prelude.Maybe Prelude.Text,
    -- | The version of the query used to search.
    queryVersion :: Prelude.Maybe Prelude.Text,
    -- | The query used to search. You can specify \"*\" for the query string to
    -- get the count of all indexed things in your Amazon Web Services account.
    queryString :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregationField', 'getStatistics_aggregationField' - The aggregation field name.
--
-- 'indexName', 'getStatistics_indexName' - The name of the index to search. The default value is @AWS_Things@.
--
-- 'queryVersion', 'getStatistics_queryVersion' - The version of the query used to search.
--
-- 'queryString', 'getStatistics_queryString' - The query used to search. You can specify \"*\" for the query string to
-- get the count of all indexed things in your Amazon Web Services account.
newGetStatistics ::
  -- | 'queryString'
  Prelude.Text ->
  GetStatistics
newGetStatistics pQueryString_ =
  GetStatistics'
    { aggregationField = Prelude.Nothing,
      indexName = Prelude.Nothing,
      queryVersion = Prelude.Nothing,
      queryString = pQueryString_
    }

-- | The aggregation field name.
getStatistics_aggregationField :: Lens.Lens' GetStatistics (Prelude.Maybe Prelude.Text)
getStatistics_aggregationField = Lens.lens (\GetStatistics' {aggregationField} -> aggregationField) (\s@GetStatistics' {} a -> s {aggregationField = a} :: GetStatistics)

-- | The name of the index to search. The default value is @AWS_Things@.
getStatistics_indexName :: Lens.Lens' GetStatistics (Prelude.Maybe Prelude.Text)
getStatistics_indexName = Lens.lens (\GetStatistics' {indexName} -> indexName) (\s@GetStatistics' {} a -> s {indexName = a} :: GetStatistics)

-- | The version of the query used to search.
getStatistics_queryVersion :: Lens.Lens' GetStatistics (Prelude.Maybe Prelude.Text)
getStatistics_queryVersion = Lens.lens (\GetStatistics' {queryVersion} -> queryVersion) (\s@GetStatistics' {} a -> s {queryVersion = a} :: GetStatistics)

-- | The query used to search. You can specify \"*\" for the query string to
-- get the count of all indexed things in your Amazon Web Services account.
getStatistics_queryString :: Lens.Lens' GetStatistics Prelude.Text
getStatistics_queryString = Lens.lens (\GetStatistics' {queryString} -> queryString) (\s@GetStatistics' {} a -> s {queryString = a} :: GetStatistics)

instance Core.AWSRequest GetStatistics where
  type
    AWSResponse GetStatistics =
      GetStatisticsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStatisticsResponse'
            Prelude.<$> (x Data..?> "statistics")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetStatistics where
  hashWithSalt _salt GetStatistics' {..} =
    _salt
      `Prelude.hashWithSalt` aggregationField
      `Prelude.hashWithSalt` indexName
      `Prelude.hashWithSalt` queryVersion
      `Prelude.hashWithSalt` queryString

instance Prelude.NFData GetStatistics where
  rnf GetStatistics' {..} =
    Prelude.rnf aggregationField `Prelude.seq`
      Prelude.rnf indexName `Prelude.seq`
        Prelude.rnf queryVersion `Prelude.seq`
          Prelude.rnf queryString

instance Data.ToHeaders GetStatistics where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON GetStatistics where
  toJSON GetStatistics' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("aggregationField" Data..=)
              Prelude.<$> aggregationField,
            ("indexName" Data..=) Prelude.<$> indexName,
            ("queryVersion" Data..=) Prelude.<$> queryVersion,
            Prelude.Just ("queryString" Data..= queryString)
          ]
      )

instance Data.ToPath GetStatistics where
  toPath = Prelude.const "/indices/statistics"

instance Data.ToQuery GetStatistics where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetStatisticsResponse' smart constructor.
data GetStatisticsResponse = GetStatisticsResponse'
  { -- | The statistics returned by the Fleet Indexing service based on the query
    -- and aggregation field.
    statistics :: Prelude.Maybe Statistics,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStatisticsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statistics', 'getStatisticsResponse_statistics' - The statistics returned by the Fleet Indexing service based on the query
-- and aggregation field.
--
-- 'httpStatus', 'getStatisticsResponse_httpStatus' - The response's http status code.
newGetStatisticsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetStatisticsResponse
newGetStatisticsResponse pHttpStatus_ =
  GetStatisticsResponse'
    { statistics =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The statistics returned by the Fleet Indexing service based on the query
-- and aggregation field.
getStatisticsResponse_statistics :: Lens.Lens' GetStatisticsResponse (Prelude.Maybe Statistics)
getStatisticsResponse_statistics = Lens.lens (\GetStatisticsResponse' {statistics} -> statistics) (\s@GetStatisticsResponse' {} a -> s {statistics = a} :: GetStatisticsResponse)

-- | The response's http status code.
getStatisticsResponse_httpStatus :: Lens.Lens' GetStatisticsResponse Prelude.Int
getStatisticsResponse_httpStatus = Lens.lens (\GetStatisticsResponse' {httpStatus} -> httpStatus) (\s@GetStatisticsResponse' {} a -> s {httpStatus = a} :: GetStatisticsResponse)

instance Prelude.NFData GetStatisticsResponse where
  rnf GetStatisticsResponse' {..} =
    Prelude.rnf statistics `Prelude.seq`
      Prelude.rnf httpStatus
