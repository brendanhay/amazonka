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
-- Module      : Amazonka.GuardDuty.GetCoverageStatistics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves aggregated statistics for your account. If you are a GuardDuty
-- administrator, you can retrieve the statistics for all the resources
-- associated with the active member accounts in your organization who have
-- enabled EKS Runtime Monitoring and have the GuardDuty agent running on
-- their EKS nodes.
module Amazonka.GuardDuty.GetCoverageStatistics
  ( -- * Creating a Request
    GetCoverageStatistics (..),
    newGetCoverageStatistics,

    -- * Request Lenses
    getCoverageStatistics_filterCriteria,
    getCoverageStatistics_detectorId,
    getCoverageStatistics_statisticsType,

    -- * Destructuring the Response
    GetCoverageStatisticsResponse (..),
    newGetCoverageStatisticsResponse,

    -- * Response Lenses
    getCoverageStatisticsResponse_coverageStatistics,
    getCoverageStatisticsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCoverageStatistics' smart constructor.
data GetCoverageStatistics = GetCoverageStatistics'
  { -- | Represents the criteria used to filter the coverage statistics
    filterCriteria :: Prelude.Maybe CoverageFilterCriteria,
    -- | The unique ID of the GuardDuty detector associated to the coverage
    -- statistics.
    detectorId :: Prelude.Text,
    -- | Represents the statistics type used to aggregate the coverage details.
    statisticsType :: [CoverageStatisticsType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCoverageStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterCriteria', 'getCoverageStatistics_filterCriteria' - Represents the criteria used to filter the coverage statistics
--
-- 'detectorId', 'getCoverageStatistics_detectorId' - The unique ID of the GuardDuty detector associated to the coverage
-- statistics.
--
-- 'statisticsType', 'getCoverageStatistics_statisticsType' - Represents the statistics type used to aggregate the coverage details.
newGetCoverageStatistics ::
  -- | 'detectorId'
  Prelude.Text ->
  GetCoverageStatistics
newGetCoverageStatistics pDetectorId_ =
  GetCoverageStatistics'
    { filterCriteria =
        Prelude.Nothing,
      detectorId = pDetectorId_,
      statisticsType = Prelude.mempty
    }

-- | Represents the criteria used to filter the coverage statistics
getCoverageStatistics_filterCriteria :: Lens.Lens' GetCoverageStatistics (Prelude.Maybe CoverageFilterCriteria)
getCoverageStatistics_filterCriteria = Lens.lens (\GetCoverageStatistics' {filterCriteria} -> filterCriteria) (\s@GetCoverageStatistics' {} a -> s {filterCriteria = a} :: GetCoverageStatistics)

-- | The unique ID of the GuardDuty detector associated to the coverage
-- statistics.
getCoverageStatistics_detectorId :: Lens.Lens' GetCoverageStatistics Prelude.Text
getCoverageStatistics_detectorId = Lens.lens (\GetCoverageStatistics' {detectorId} -> detectorId) (\s@GetCoverageStatistics' {} a -> s {detectorId = a} :: GetCoverageStatistics)

-- | Represents the statistics type used to aggregate the coverage details.
getCoverageStatistics_statisticsType :: Lens.Lens' GetCoverageStatistics [CoverageStatisticsType]
getCoverageStatistics_statisticsType = Lens.lens (\GetCoverageStatistics' {statisticsType} -> statisticsType) (\s@GetCoverageStatistics' {} a -> s {statisticsType = a} :: GetCoverageStatistics) Prelude.. Lens.coerced

instance Core.AWSRequest GetCoverageStatistics where
  type
    AWSResponse GetCoverageStatistics =
      GetCoverageStatisticsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCoverageStatisticsResponse'
            Prelude.<$> (x Data..?> "coverageStatistics")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCoverageStatistics where
  hashWithSalt _salt GetCoverageStatistics' {..} =
    _salt
      `Prelude.hashWithSalt` filterCriteria
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` statisticsType

instance Prelude.NFData GetCoverageStatistics where
  rnf GetCoverageStatistics' {..} =
    Prelude.rnf filterCriteria
      `Prelude.seq` Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf statisticsType

instance Data.ToHeaders GetCoverageStatistics where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetCoverageStatistics where
  toJSON GetCoverageStatistics' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filterCriteria" Data..=)
              Prelude.<$> filterCriteria,
            Prelude.Just
              ("statisticsType" Data..= statisticsType)
          ]
      )

instance Data.ToPath GetCoverageStatistics where
  toPath GetCoverageStatistics' {..} =
    Prelude.mconcat
      [ "/detector/",
        Data.toBS detectorId,
        "/coverage/statistics"
      ]

instance Data.ToQuery GetCoverageStatistics where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCoverageStatisticsResponse' smart constructor.
data GetCoverageStatisticsResponse = GetCoverageStatisticsResponse'
  { -- | Represents the count aggregated by the @statusCode@ and @resourceType@.
    coverageStatistics :: Prelude.Maybe CoverageStatistics,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCoverageStatisticsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coverageStatistics', 'getCoverageStatisticsResponse_coverageStatistics' - Represents the count aggregated by the @statusCode@ and @resourceType@.
--
-- 'httpStatus', 'getCoverageStatisticsResponse_httpStatus' - The response's http status code.
newGetCoverageStatisticsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCoverageStatisticsResponse
newGetCoverageStatisticsResponse pHttpStatus_ =
  GetCoverageStatisticsResponse'
    { coverageStatistics =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents the count aggregated by the @statusCode@ and @resourceType@.
getCoverageStatisticsResponse_coverageStatistics :: Lens.Lens' GetCoverageStatisticsResponse (Prelude.Maybe CoverageStatistics)
getCoverageStatisticsResponse_coverageStatistics = Lens.lens (\GetCoverageStatisticsResponse' {coverageStatistics} -> coverageStatistics) (\s@GetCoverageStatisticsResponse' {} a -> s {coverageStatistics = a} :: GetCoverageStatisticsResponse)

-- | The response's http status code.
getCoverageStatisticsResponse_httpStatus :: Lens.Lens' GetCoverageStatisticsResponse Prelude.Int
getCoverageStatisticsResponse_httpStatus = Lens.lens (\GetCoverageStatisticsResponse' {httpStatus} -> httpStatus) (\s@GetCoverageStatisticsResponse' {} a -> s {httpStatus = a} :: GetCoverageStatisticsResponse)

instance Prelude.NFData GetCoverageStatisticsResponse where
  rnf GetCoverageStatisticsResponse' {..} =
    Prelude.rnf coverageStatistics
      `Prelude.seq` Prelude.rnf httpStatus
