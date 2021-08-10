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
-- Module      : Network.AWS.IoT.GetStatistics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the count, average, sum, minimum, maximum, sum of squares,
-- variance, and standard deviation for the specified aggregated field. If
-- the aggregation field is of type @String@, only the count statistic is
-- returned.
module Network.AWS.IoT.GetStatistics
  ( -- * Creating a Request
    GetStatistics (..),
    newGetStatistics,

    -- * Request Lenses
    getStatistics_indexName,
    getStatistics_queryVersion,
    getStatistics_aggregationField,
    getStatistics_queryString,

    -- * Destructuring the Response
    GetStatisticsResponse (..),
    newGetStatisticsResponse,

    -- * Response Lenses
    getStatisticsResponse_statistics,
    getStatisticsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetStatistics' smart constructor.
data GetStatistics = GetStatistics'
  { -- | The name of the index to search. The default value is @AWS_Things@.
    indexName :: Prelude.Maybe Prelude.Text,
    -- | The version of the query used to search.
    queryVersion :: Prelude.Maybe Prelude.Text,
    -- | The aggregation field name.
    aggregationField :: Prelude.Maybe Prelude.Text,
    -- | The query used to search. You can specify \"*\" for the query string to
    -- get the count of all indexed things in your AWS account.
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
-- 'indexName', 'getStatistics_indexName' - The name of the index to search. The default value is @AWS_Things@.
--
-- 'queryVersion', 'getStatistics_queryVersion' - The version of the query used to search.
--
-- 'aggregationField', 'getStatistics_aggregationField' - The aggregation field name.
--
-- 'queryString', 'getStatistics_queryString' - The query used to search. You can specify \"*\" for the query string to
-- get the count of all indexed things in your AWS account.
newGetStatistics ::
  -- | 'queryString'
  Prelude.Text ->
  GetStatistics
newGetStatistics pQueryString_ =
  GetStatistics'
    { indexName = Prelude.Nothing,
      queryVersion = Prelude.Nothing,
      aggregationField = Prelude.Nothing,
      queryString = pQueryString_
    }

-- | The name of the index to search. The default value is @AWS_Things@.
getStatistics_indexName :: Lens.Lens' GetStatistics (Prelude.Maybe Prelude.Text)
getStatistics_indexName = Lens.lens (\GetStatistics' {indexName} -> indexName) (\s@GetStatistics' {} a -> s {indexName = a} :: GetStatistics)

-- | The version of the query used to search.
getStatistics_queryVersion :: Lens.Lens' GetStatistics (Prelude.Maybe Prelude.Text)
getStatistics_queryVersion = Lens.lens (\GetStatistics' {queryVersion} -> queryVersion) (\s@GetStatistics' {} a -> s {queryVersion = a} :: GetStatistics)

-- | The aggregation field name.
getStatistics_aggregationField :: Lens.Lens' GetStatistics (Prelude.Maybe Prelude.Text)
getStatistics_aggregationField = Lens.lens (\GetStatistics' {aggregationField} -> aggregationField) (\s@GetStatistics' {} a -> s {aggregationField = a} :: GetStatistics)

-- | The query used to search. You can specify \"*\" for the query string to
-- get the count of all indexed things in your AWS account.
getStatistics_queryString :: Lens.Lens' GetStatistics Prelude.Text
getStatistics_queryString = Lens.lens (\GetStatistics' {queryString} -> queryString) (\s@GetStatistics' {} a -> s {queryString = a} :: GetStatistics)

instance Core.AWSRequest GetStatistics where
  type
    AWSResponse GetStatistics =
      GetStatisticsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStatisticsResponse'
            Prelude.<$> (x Core..?> "statistics")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetStatistics

instance Prelude.NFData GetStatistics

instance Core.ToHeaders GetStatistics where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON GetStatistics where
  toJSON GetStatistics' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("indexName" Core..=) Prelude.<$> indexName,
            ("queryVersion" Core..=) Prelude.<$> queryVersion,
            ("aggregationField" Core..=)
              Prelude.<$> aggregationField,
            Prelude.Just ("queryString" Core..= queryString)
          ]
      )

instance Core.ToPath GetStatistics where
  toPath = Prelude.const "/indices/statistics"

instance Core.ToQuery GetStatistics where
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

instance Prelude.NFData GetStatisticsResponse
