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
-- Module      : Amazonka.CodeBuild.DescribeCodeCoverages
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves one or more code coverage reports.
--
-- This operation returns paginated results.
module Amazonka.CodeBuild.DescribeCodeCoverages
  ( -- * Creating a Request
    DescribeCodeCoverages (..),
    newDescribeCodeCoverages,

    -- * Request Lenses
    describeCodeCoverages_minLineCoveragePercentage,
    describeCodeCoverages_sortOrder,
    describeCodeCoverages_maxLineCoveragePercentage,
    describeCodeCoverages_nextToken,
    describeCodeCoverages_maxResults,
    describeCodeCoverages_sortBy,
    describeCodeCoverages_reportArn,

    -- * Destructuring the Response
    DescribeCodeCoveragesResponse (..),
    newDescribeCodeCoveragesResponse,

    -- * Response Lenses
    describeCodeCoveragesResponse_codeCoverages,
    describeCodeCoveragesResponse_nextToken,
    describeCodeCoveragesResponse_httpStatus,
  )
where

import Amazonka.CodeBuild.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeCodeCoverages' smart constructor.
data DescribeCodeCoverages = DescribeCodeCoverages'
  { -- | The minimum line coverage percentage to report.
    minLineCoveragePercentage :: Prelude.Maybe Prelude.Double,
    -- | Specifies if the results are sorted in ascending or descending order.
    sortOrder :: Prelude.Maybe SortOrderType,
    -- | The maximum line coverage percentage to report.
    maxLineCoveragePercentage :: Prelude.Maybe Prelude.Double,
    -- | The @nextToken@ value returned from a previous call to
    -- @DescribeCodeCoverages@. This specifies the next item to return. To
    -- return the beginning of the list, exclude this parameter.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specifies how the results are sorted. Possible values are:
    --
    -- [FILE_PATH]
    --     The results are sorted by file path.
    --
    -- [LINE_COVERAGE_PERCENTAGE]
    --     The results are sorted by the percentage of lines that are covered.
    sortBy :: Prelude.Maybe ReportCodeCoverageSortByType,
    -- | The ARN of the report for which test cases are returned.
    reportArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCodeCoverages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minLineCoveragePercentage', 'describeCodeCoverages_minLineCoveragePercentage' - The minimum line coverage percentage to report.
--
-- 'sortOrder', 'describeCodeCoverages_sortOrder' - Specifies if the results are sorted in ascending or descending order.
--
-- 'maxLineCoveragePercentage', 'describeCodeCoverages_maxLineCoveragePercentage' - The maximum line coverage percentage to report.
--
-- 'nextToken', 'describeCodeCoverages_nextToken' - The @nextToken@ value returned from a previous call to
-- @DescribeCodeCoverages@. This specifies the next item to return. To
-- return the beginning of the list, exclude this parameter.
--
-- 'maxResults', 'describeCodeCoverages_maxResults' - The maximum number of results to return.
--
-- 'sortBy', 'describeCodeCoverages_sortBy' - Specifies how the results are sorted. Possible values are:
--
-- [FILE_PATH]
--     The results are sorted by file path.
--
-- [LINE_COVERAGE_PERCENTAGE]
--     The results are sorted by the percentage of lines that are covered.
--
-- 'reportArn', 'describeCodeCoverages_reportArn' - The ARN of the report for which test cases are returned.
newDescribeCodeCoverages ::
  -- | 'reportArn'
  Prelude.Text ->
  DescribeCodeCoverages
newDescribeCodeCoverages pReportArn_ =
  DescribeCodeCoverages'
    { minLineCoveragePercentage =
        Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      maxLineCoveragePercentage = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      reportArn = pReportArn_
    }

-- | The minimum line coverage percentage to report.
describeCodeCoverages_minLineCoveragePercentage :: Lens.Lens' DescribeCodeCoverages (Prelude.Maybe Prelude.Double)
describeCodeCoverages_minLineCoveragePercentage = Lens.lens (\DescribeCodeCoverages' {minLineCoveragePercentage} -> minLineCoveragePercentage) (\s@DescribeCodeCoverages' {} a -> s {minLineCoveragePercentage = a} :: DescribeCodeCoverages)

-- | Specifies if the results are sorted in ascending or descending order.
describeCodeCoverages_sortOrder :: Lens.Lens' DescribeCodeCoverages (Prelude.Maybe SortOrderType)
describeCodeCoverages_sortOrder = Lens.lens (\DescribeCodeCoverages' {sortOrder} -> sortOrder) (\s@DescribeCodeCoverages' {} a -> s {sortOrder = a} :: DescribeCodeCoverages)

-- | The maximum line coverage percentage to report.
describeCodeCoverages_maxLineCoveragePercentage :: Lens.Lens' DescribeCodeCoverages (Prelude.Maybe Prelude.Double)
describeCodeCoverages_maxLineCoveragePercentage = Lens.lens (\DescribeCodeCoverages' {maxLineCoveragePercentage} -> maxLineCoveragePercentage) (\s@DescribeCodeCoverages' {} a -> s {maxLineCoveragePercentage = a} :: DescribeCodeCoverages)

-- | The @nextToken@ value returned from a previous call to
-- @DescribeCodeCoverages@. This specifies the next item to return. To
-- return the beginning of the list, exclude this parameter.
describeCodeCoverages_nextToken :: Lens.Lens' DescribeCodeCoverages (Prelude.Maybe Prelude.Text)
describeCodeCoverages_nextToken = Lens.lens (\DescribeCodeCoverages' {nextToken} -> nextToken) (\s@DescribeCodeCoverages' {} a -> s {nextToken = a} :: DescribeCodeCoverages)

-- | The maximum number of results to return.
describeCodeCoverages_maxResults :: Lens.Lens' DescribeCodeCoverages (Prelude.Maybe Prelude.Natural)
describeCodeCoverages_maxResults = Lens.lens (\DescribeCodeCoverages' {maxResults} -> maxResults) (\s@DescribeCodeCoverages' {} a -> s {maxResults = a} :: DescribeCodeCoverages)

-- | Specifies how the results are sorted. Possible values are:
--
-- [FILE_PATH]
--     The results are sorted by file path.
--
-- [LINE_COVERAGE_PERCENTAGE]
--     The results are sorted by the percentage of lines that are covered.
describeCodeCoverages_sortBy :: Lens.Lens' DescribeCodeCoverages (Prelude.Maybe ReportCodeCoverageSortByType)
describeCodeCoverages_sortBy = Lens.lens (\DescribeCodeCoverages' {sortBy} -> sortBy) (\s@DescribeCodeCoverages' {} a -> s {sortBy = a} :: DescribeCodeCoverages)

-- | The ARN of the report for which test cases are returned.
describeCodeCoverages_reportArn :: Lens.Lens' DescribeCodeCoverages Prelude.Text
describeCodeCoverages_reportArn = Lens.lens (\DescribeCodeCoverages' {reportArn} -> reportArn) (\s@DescribeCodeCoverages' {} a -> s {reportArn = a} :: DescribeCodeCoverages)

instance Core.AWSPager DescribeCodeCoverages where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeCodeCoveragesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeCodeCoveragesResponse_codeCoverages
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeCodeCoverages_nextToken
          Lens..~ rs
          Lens.^? describeCodeCoveragesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeCodeCoverages where
  type
    AWSResponse DescribeCodeCoverages =
      DescribeCodeCoveragesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCodeCoveragesResponse'
            Prelude.<$> (x Core..?> "codeCoverages" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCodeCoverages

instance Prelude.NFData DescribeCodeCoverages

instance Core.ToHeaders DescribeCodeCoverages where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.DescribeCodeCoverages" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeCodeCoverages where
  toJSON DescribeCodeCoverages' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("minLineCoveragePercentage" Core..=)
              Prelude.<$> minLineCoveragePercentage,
            ("sortOrder" Core..=) Prelude.<$> sortOrder,
            ("maxLineCoveragePercentage" Core..=)
              Prelude.<$> maxLineCoveragePercentage,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("sortBy" Core..=) Prelude.<$> sortBy,
            Prelude.Just ("reportArn" Core..= reportArn)
          ]
      )

instance Core.ToPath DescribeCodeCoverages where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeCodeCoverages where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeCodeCoveragesResponse' smart constructor.
data DescribeCodeCoveragesResponse = DescribeCodeCoveragesResponse'
  { -- | An array of @CodeCoverage@ objects that contain the results.
    codeCoverages :: Prelude.Maybe [CodeCoverage],
    -- | If there are more items to return, this contains a token that is passed
    -- to a subsequent call to @DescribeCodeCoverages@ to retrieve the next set
    -- of items.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCodeCoveragesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeCoverages', 'describeCodeCoveragesResponse_codeCoverages' - An array of @CodeCoverage@ objects that contain the results.
--
-- 'nextToken', 'describeCodeCoveragesResponse_nextToken' - If there are more items to return, this contains a token that is passed
-- to a subsequent call to @DescribeCodeCoverages@ to retrieve the next set
-- of items.
--
-- 'httpStatus', 'describeCodeCoveragesResponse_httpStatus' - The response's http status code.
newDescribeCodeCoveragesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCodeCoveragesResponse
newDescribeCodeCoveragesResponse pHttpStatus_ =
  DescribeCodeCoveragesResponse'
    { codeCoverages =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @CodeCoverage@ objects that contain the results.
describeCodeCoveragesResponse_codeCoverages :: Lens.Lens' DescribeCodeCoveragesResponse (Prelude.Maybe [CodeCoverage])
describeCodeCoveragesResponse_codeCoverages = Lens.lens (\DescribeCodeCoveragesResponse' {codeCoverages} -> codeCoverages) (\s@DescribeCodeCoveragesResponse' {} a -> s {codeCoverages = a} :: DescribeCodeCoveragesResponse) Prelude.. Lens.mapping Lens.coerced

-- | If there are more items to return, this contains a token that is passed
-- to a subsequent call to @DescribeCodeCoverages@ to retrieve the next set
-- of items.
describeCodeCoveragesResponse_nextToken :: Lens.Lens' DescribeCodeCoveragesResponse (Prelude.Maybe Prelude.Text)
describeCodeCoveragesResponse_nextToken = Lens.lens (\DescribeCodeCoveragesResponse' {nextToken} -> nextToken) (\s@DescribeCodeCoveragesResponse' {} a -> s {nextToken = a} :: DescribeCodeCoveragesResponse)

-- | The response's http status code.
describeCodeCoveragesResponse_httpStatus :: Lens.Lens' DescribeCodeCoveragesResponse Prelude.Int
describeCodeCoveragesResponse_httpStatus = Lens.lens (\DescribeCodeCoveragesResponse' {httpStatus} -> httpStatus) (\s@DescribeCodeCoveragesResponse' {} a -> s {httpStatus = a} :: DescribeCodeCoveragesResponse)

instance Prelude.NFData DescribeCodeCoveragesResponse
