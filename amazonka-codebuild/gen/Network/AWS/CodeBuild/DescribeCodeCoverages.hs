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
-- Module      : Network.AWS.CodeBuild.DescribeCodeCoverages
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves one or more code coverage reports.
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.DescribeCodeCoverages
  ( -- * Creating a Request
    DescribeCodeCoverages (..),
    newDescribeCodeCoverages,

    -- * Request Lenses
    describeCodeCoverages_sortOrder,
    describeCodeCoverages_nextToken,
    describeCodeCoverages_maxLineCoveragePercentage,
    describeCodeCoverages_maxResults,
    describeCodeCoverages_sortBy,
    describeCodeCoverages_minLineCoveragePercentage,
    describeCodeCoverages_reportArn,

    -- * Destructuring the Response
    DescribeCodeCoveragesResponse (..),
    newDescribeCodeCoveragesResponse,

    -- * Response Lenses
    describeCodeCoveragesResponse_nextToken,
    describeCodeCoveragesResponse_codeCoverages,
    describeCodeCoveragesResponse_httpStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeCodeCoverages' smart constructor.
data DescribeCodeCoverages = DescribeCodeCoverages'
  { -- | Specifies if the results are sorted in ascending or descending order.
    sortOrder :: Core.Maybe SortOrderType,
    -- | The @nextToken@ value returned from a previous call to
    -- @DescribeCodeCoverages@. This specifies the next item to return. To
    -- return the beginning of the list, exclude this parameter.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum line coverage percentage to report.
    maxLineCoveragePercentage :: Core.Maybe Core.Double,
    -- | The maximum number of results to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | Specifies how the results are sorted. Possible values are:
    --
    -- [FILE_PATH]
    --     The results are sorted by file path.
    --
    -- [LINE_COVERAGE_PERCENTAGE]
    --     The results are sorted by the percentage of lines that are covered.
    sortBy :: Core.Maybe ReportCodeCoverageSortByType,
    -- | The minimum line coverage percentage to report.
    minLineCoveragePercentage :: Core.Maybe Core.Double,
    -- | The ARN of the report for which test cases are returned.
    reportArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeCodeCoverages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'describeCodeCoverages_sortOrder' - Specifies if the results are sorted in ascending or descending order.
--
-- 'nextToken', 'describeCodeCoverages_nextToken' - The @nextToken@ value returned from a previous call to
-- @DescribeCodeCoverages@. This specifies the next item to return. To
-- return the beginning of the list, exclude this parameter.
--
-- 'maxLineCoveragePercentage', 'describeCodeCoverages_maxLineCoveragePercentage' - The maximum line coverage percentage to report.
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
-- 'minLineCoveragePercentage', 'describeCodeCoverages_minLineCoveragePercentage' - The minimum line coverage percentage to report.
--
-- 'reportArn', 'describeCodeCoverages_reportArn' - The ARN of the report for which test cases are returned.
newDescribeCodeCoverages ::
  -- | 'reportArn'
  Core.Text ->
  DescribeCodeCoverages
newDescribeCodeCoverages pReportArn_ =
  DescribeCodeCoverages'
    { sortOrder = Core.Nothing,
      nextToken = Core.Nothing,
      maxLineCoveragePercentage = Core.Nothing,
      maxResults = Core.Nothing,
      sortBy = Core.Nothing,
      minLineCoveragePercentage = Core.Nothing,
      reportArn = pReportArn_
    }

-- | Specifies if the results are sorted in ascending or descending order.
describeCodeCoverages_sortOrder :: Lens.Lens' DescribeCodeCoverages (Core.Maybe SortOrderType)
describeCodeCoverages_sortOrder = Lens.lens (\DescribeCodeCoverages' {sortOrder} -> sortOrder) (\s@DescribeCodeCoverages' {} a -> s {sortOrder = a} :: DescribeCodeCoverages)

-- | The @nextToken@ value returned from a previous call to
-- @DescribeCodeCoverages@. This specifies the next item to return. To
-- return the beginning of the list, exclude this parameter.
describeCodeCoverages_nextToken :: Lens.Lens' DescribeCodeCoverages (Core.Maybe Core.Text)
describeCodeCoverages_nextToken = Lens.lens (\DescribeCodeCoverages' {nextToken} -> nextToken) (\s@DescribeCodeCoverages' {} a -> s {nextToken = a} :: DescribeCodeCoverages)

-- | The maximum line coverage percentage to report.
describeCodeCoverages_maxLineCoveragePercentage :: Lens.Lens' DescribeCodeCoverages (Core.Maybe Core.Double)
describeCodeCoverages_maxLineCoveragePercentage = Lens.lens (\DescribeCodeCoverages' {maxLineCoveragePercentage} -> maxLineCoveragePercentage) (\s@DescribeCodeCoverages' {} a -> s {maxLineCoveragePercentage = a} :: DescribeCodeCoverages)

-- | The maximum number of results to return.
describeCodeCoverages_maxResults :: Lens.Lens' DescribeCodeCoverages (Core.Maybe Core.Natural)
describeCodeCoverages_maxResults = Lens.lens (\DescribeCodeCoverages' {maxResults} -> maxResults) (\s@DescribeCodeCoverages' {} a -> s {maxResults = a} :: DescribeCodeCoverages)

-- | Specifies how the results are sorted. Possible values are:
--
-- [FILE_PATH]
--     The results are sorted by file path.
--
-- [LINE_COVERAGE_PERCENTAGE]
--     The results are sorted by the percentage of lines that are covered.
describeCodeCoverages_sortBy :: Lens.Lens' DescribeCodeCoverages (Core.Maybe ReportCodeCoverageSortByType)
describeCodeCoverages_sortBy = Lens.lens (\DescribeCodeCoverages' {sortBy} -> sortBy) (\s@DescribeCodeCoverages' {} a -> s {sortBy = a} :: DescribeCodeCoverages)

-- | The minimum line coverage percentage to report.
describeCodeCoverages_minLineCoveragePercentage :: Lens.Lens' DescribeCodeCoverages (Core.Maybe Core.Double)
describeCodeCoverages_minLineCoveragePercentage = Lens.lens (\DescribeCodeCoverages' {minLineCoveragePercentage} -> minLineCoveragePercentage) (\s@DescribeCodeCoverages' {} a -> s {minLineCoveragePercentage = a} :: DescribeCodeCoverages)

-- | The ARN of the report for which test cases are returned.
describeCodeCoverages_reportArn :: Lens.Lens' DescribeCodeCoverages Core.Text
describeCodeCoverages_reportArn = Lens.lens (\DescribeCodeCoverages' {reportArn} -> reportArn) (\s@DescribeCodeCoverages' {} a -> s {reportArn = a} :: DescribeCodeCoverages)

instance Core.AWSPager DescribeCodeCoverages where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeCodeCoveragesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeCodeCoveragesResponse_codeCoverages
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeCodeCoverages_nextToken
          Lens..~ rs
          Lens.^? describeCodeCoveragesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeCodeCoverages where
  type
    AWSResponse DescribeCodeCoverages =
      DescribeCodeCoveragesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCodeCoveragesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "codeCoverages" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeCodeCoverages

instance Core.NFData DescribeCodeCoverages

instance Core.ToHeaders DescribeCodeCoverages where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.DescribeCodeCoverages" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeCodeCoverages where
  toJSON DescribeCodeCoverages' {..} =
    Core.object
      ( Core.catMaybes
          [ ("sortOrder" Core..=) Core.<$> sortOrder,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("maxLineCoveragePercentage" Core..=)
              Core.<$> maxLineCoveragePercentage,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("sortBy" Core..=) Core.<$> sortBy,
            ("minLineCoveragePercentage" Core..=)
              Core.<$> minLineCoveragePercentage,
            Core.Just ("reportArn" Core..= reportArn)
          ]
      )

instance Core.ToPath DescribeCodeCoverages where
  toPath = Core.const "/"

instance Core.ToQuery DescribeCodeCoverages where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeCodeCoveragesResponse' smart constructor.
data DescribeCodeCoveragesResponse = DescribeCodeCoveragesResponse'
  { -- | If there are more items to return, this contains a token that is passed
    -- to a subsequent call to @DescribeCodeCoverages@ to retrieve the next set
    -- of items.
    nextToken :: Core.Maybe Core.Text,
    -- | An array of @CodeCoverage@ objects that contain the results.
    codeCoverages :: Core.Maybe [CodeCoverage],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeCodeCoveragesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeCodeCoveragesResponse_nextToken' - If there are more items to return, this contains a token that is passed
-- to a subsequent call to @DescribeCodeCoverages@ to retrieve the next set
-- of items.
--
-- 'codeCoverages', 'describeCodeCoveragesResponse_codeCoverages' - An array of @CodeCoverage@ objects that contain the results.
--
-- 'httpStatus', 'describeCodeCoveragesResponse_httpStatus' - The response's http status code.
newDescribeCodeCoveragesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeCodeCoveragesResponse
newDescribeCodeCoveragesResponse pHttpStatus_ =
  DescribeCodeCoveragesResponse'
    { nextToken =
        Core.Nothing,
      codeCoverages = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are more items to return, this contains a token that is passed
-- to a subsequent call to @DescribeCodeCoverages@ to retrieve the next set
-- of items.
describeCodeCoveragesResponse_nextToken :: Lens.Lens' DescribeCodeCoveragesResponse (Core.Maybe Core.Text)
describeCodeCoveragesResponse_nextToken = Lens.lens (\DescribeCodeCoveragesResponse' {nextToken} -> nextToken) (\s@DescribeCodeCoveragesResponse' {} a -> s {nextToken = a} :: DescribeCodeCoveragesResponse)

-- | An array of @CodeCoverage@ objects that contain the results.
describeCodeCoveragesResponse_codeCoverages :: Lens.Lens' DescribeCodeCoveragesResponse (Core.Maybe [CodeCoverage])
describeCodeCoveragesResponse_codeCoverages = Lens.lens (\DescribeCodeCoveragesResponse' {codeCoverages} -> codeCoverages) (\s@DescribeCodeCoveragesResponse' {} a -> s {codeCoverages = a} :: DescribeCodeCoveragesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeCodeCoveragesResponse_httpStatus :: Lens.Lens' DescribeCodeCoveragesResponse Core.Int
describeCodeCoveragesResponse_httpStatus = Lens.lens (\DescribeCodeCoveragesResponse' {httpStatus} -> httpStatus) (\s@DescribeCodeCoveragesResponse' {} a -> s {httpStatus = a} :: DescribeCodeCoveragesResponse)

instance Core.NFData DescribeCodeCoveragesResponse
