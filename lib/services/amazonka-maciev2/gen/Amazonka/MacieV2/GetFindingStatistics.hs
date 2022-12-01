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
-- Module      : Amazonka.MacieV2.GetFindingStatistics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves (queries) aggregated statistical data about findings.
module Amazonka.MacieV2.GetFindingStatistics
  ( -- * Creating a Request
    GetFindingStatistics (..),
    newGetFindingStatistics,

    -- * Request Lenses
    getFindingStatistics_sortCriteria,
    getFindingStatistics_findingCriteria,
    getFindingStatistics_size,
    getFindingStatistics_groupBy,

    -- * Destructuring the Response
    GetFindingStatisticsResponse (..),
    newGetFindingStatisticsResponse,

    -- * Response Lenses
    getFindingStatisticsResponse_countsByGroup,
    getFindingStatisticsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetFindingStatistics' smart constructor.
data GetFindingStatistics = GetFindingStatistics'
  { -- | The criteria to use to sort the query results.
    sortCriteria :: Prelude.Maybe FindingStatisticsSortCriteria,
    -- | The criteria to use to filter the query results.
    findingCriteria :: Prelude.Maybe FindingCriteria,
    -- | The maximum number of items to include in each page of the response.
    size :: Prelude.Maybe Prelude.Int,
    -- | The finding property to use to group the query results. Valid values
    -- are:
    --
    -- -   classificationDetails.jobId - The unique identifier for the
    --     classification job that produced the finding.
    --
    -- -   resourcesAffected.s3Bucket.name - The name of the S3 bucket that the
    --     finding applies to.
    --
    -- -   severity.description - The severity level of the finding, such as
    --     High or Medium.
    --
    -- -   type - The type of finding, such as Policy:IAMUser\/S3BucketPublic
    --     and SensitiveData:S3Object\/Personal.
    groupBy :: GroupBy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFindingStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortCriteria', 'getFindingStatistics_sortCriteria' - The criteria to use to sort the query results.
--
-- 'findingCriteria', 'getFindingStatistics_findingCriteria' - The criteria to use to filter the query results.
--
-- 'size', 'getFindingStatistics_size' - The maximum number of items to include in each page of the response.
--
-- 'groupBy', 'getFindingStatistics_groupBy' - The finding property to use to group the query results. Valid values
-- are:
--
-- -   classificationDetails.jobId - The unique identifier for the
--     classification job that produced the finding.
--
-- -   resourcesAffected.s3Bucket.name - The name of the S3 bucket that the
--     finding applies to.
--
-- -   severity.description - The severity level of the finding, such as
--     High or Medium.
--
-- -   type - The type of finding, such as Policy:IAMUser\/S3BucketPublic
--     and SensitiveData:S3Object\/Personal.
newGetFindingStatistics ::
  -- | 'groupBy'
  GroupBy ->
  GetFindingStatistics
newGetFindingStatistics pGroupBy_ =
  GetFindingStatistics'
    { sortCriteria =
        Prelude.Nothing,
      findingCriteria = Prelude.Nothing,
      size = Prelude.Nothing,
      groupBy = pGroupBy_
    }

-- | The criteria to use to sort the query results.
getFindingStatistics_sortCriteria :: Lens.Lens' GetFindingStatistics (Prelude.Maybe FindingStatisticsSortCriteria)
getFindingStatistics_sortCriteria = Lens.lens (\GetFindingStatistics' {sortCriteria} -> sortCriteria) (\s@GetFindingStatistics' {} a -> s {sortCriteria = a} :: GetFindingStatistics)

-- | The criteria to use to filter the query results.
getFindingStatistics_findingCriteria :: Lens.Lens' GetFindingStatistics (Prelude.Maybe FindingCriteria)
getFindingStatistics_findingCriteria = Lens.lens (\GetFindingStatistics' {findingCriteria} -> findingCriteria) (\s@GetFindingStatistics' {} a -> s {findingCriteria = a} :: GetFindingStatistics)

-- | The maximum number of items to include in each page of the response.
getFindingStatistics_size :: Lens.Lens' GetFindingStatistics (Prelude.Maybe Prelude.Int)
getFindingStatistics_size = Lens.lens (\GetFindingStatistics' {size} -> size) (\s@GetFindingStatistics' {} a -> s {size = a} :: GetFindingStatistics)

-- | The finding property to use to group the query results. Valid values
-- are:
--
-- -   classificationDetails.jobId - The unique identifier for the
--     classification job that produced the finding.
--
-- -   resourcesAffected.s3Bucket.name - The name of the S3 bucket that the
--     finding applies to.
--
-- -   severity.description - The severity level of the finding, such as
--     High or Medium.
--
-- -   type - The type of finding, such as Policy:IAMUser\/S3BucketPublic
--     and SensitiveData:S3Object\/Personal.
getFindingStatistics_groupBy :: Lens.Lens' GetFindingStatistics GroupBy
getFindingStatistics_groupBy = Lens.lens (\GetFindingStatistics' {groupBy} -> groupBy) (\s@GetFindingStatistics' {} a -> s {groupBy = a} :: GetFindingStatistics)

instance Core.AWSRequest GetFindingStatistics where
  type
    AWSResponse GetFindingStatistics =
      GetFindingStatisticsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFindingStatisticsResponse'
            Prelude.<$> (x Core..?> "countsByGroup" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetFindingStatistics where
  hashWithSalt _salt GetFindingStatistics' {..} =
    _salt `Prelude.hashWithSalt` sortCriteria
      `Prelude.hashWithSalt` findingCriteria
      `Prelude.hashWithSalt` size
      `Prelude.hashWithSalt` groupBy

instance Prelude.NFData GetFindingStatistics where
  rnf GetFindingStatistics' {..} =
    Prelude.rnf sortCriteria
      `Prelude.seq` Prelude.rnf findingCriteria
      `Prelude.seq` Prelude.rnf size
      `Prelude.seq` Prelude.rnf groupBy

instance Core.ToHeaders GetFindingStatistics where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetFindingStatistics where
  toJSON GetFindingStatistics' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("sortCriteria" Core..=) Prelude.<$> sortCriteria,
            ("findingCriteria" Core..=)
              Prelude.<$> findingCriteria,
            ("size" Core..=) Prelude.<$> size,
            Prelude.Just ("groupBy" Core..= groupBy)
          ]
      )

instance Core.ToPath GetFindingStatistics where
  toPath = Prelude.const "/findings/statistics"

instance Core.ToQuery GetFindingStatistics where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFindingStatisticsResponse' smart constructor.
data GetFindingStatisticsResponse = GetFindingStatisticsResponse'
  { -- | An array of objects, one for each group of findings that meet the filter
    -- criteria specified in the request.
    countsByGroup :: Prelude.Maybe [GroupCount],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFindingStatisticsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'countsByGroup', 'getFindingStatisticsResponse_countsByGroup' - An array of objects, one for each group of findings that meet the filter
-- criteria specified in the request.
--
-- 'httpStatus', 'getFindingStatisticsResponse_httpStatus' - The response's http status code.
newGetFindingStatisticsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFindingStatisticsResponse
newGetFindingStatisticsResponse pHttpStatus_ =
  GetFindingStatisticsResponse'
    { countsByGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects, one for each group of findings that meet the filter
-- criteria specified in the request.
getFindingStatisticsResponse_countsByGroup :: Lens.Lens' GetFindingStatisticsResponse (Prelude.Maybe [GroupCount])
getFindingStatisticsResponse_countsByGroup = Lens.lens (\GetFindingStatisticsResponse' {countsByGroup} -> countsByGroup) (\s@GetFindingStatisticsResponse' {} a -> s {countsByGroup = a} :: GetFindingStatisticsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getFindingStatisticsResponse_httpStatus :: Lens.Lens' GetFindingStatisticsResponse Prelude.Int
getFindingStatisticsResponse_httpStatus = Lens.lens (\GetFindingStatisticsResponse' {httpStatus} -> httpStatus) (\s@GetFindingStatisticsResponse' {} a -> s {httpStatus = a} :: GetFindingStatisticsResponse)

instance Prelude.NFData GetFindingStatisticsResponse where
  rnf GetFindingStatisticsResponse' {..} =
    Prelude.rnf countsByGroup
      `Prelude.seq` Prelude.rnf httpStatus
