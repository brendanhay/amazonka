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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    describeCodeCoverages_sortOrder,
    describeCodeCoverages_nextToken,
    describeCodeCoverages_minLineCoveragePercentage,
    describeCodeCoverages_sortBy,
    describeCodeCoverages_maxLineCoveragePercentage,
    describeCodeCoverages_maxResults,
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

import Amazonka.CodeBuild.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeCodeCoverages' smart constructor.
data DescribeCodeCoverages = DescribeCodeCoverages'
  { -- | Specifies if the results are sorted in ascending or descending order.
    sortOrder :: Prelude.Maybe SortOrderType,
    -- | The @nextToken@ value returned from a previous call to
    -- @DescribeCodeCoverages@. This specifies the next item to return. To
    -- return the beginning of the list, exclude this parameter.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The minimum line coverage percentage to report.
    minLineCoveragePercentage :: Prelude.Maybe Prelude.Double,
    -- | Specifies how the results are sorted. Possible values are:
    --
    -- [FILE_PATH]
    --     The results are sorted by file path.
    --
    -- [LINE_COVERAGE_PERCENTAGE]
    --     The results are sorted by the percentage of lines that are covered.
    sortBy :: Prelude.Maybe ReportCodeCoverageSortByType,
    -- | The maximum line coverage percentage to report.
    maxLineCoveragePercentage :: Prelude.Maybe Prelude.Double,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
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
-- 'sortOrder', 'describeCodeCoverages_sortOrder' - Specifies if the results are sorted in ascending or descending order.
--
-- 'nextToken', 'describeCodeCoverages_nextToken' - The @nextToken@ value returned from a previous call to
-- @DescribeCodeCoverages@. This specifies the next item to return. To
-- return the beginning of the list, exclude this parameter.
--
-- 'minLineCoveragePercentage', 'describeCodeCoverages_minLineCoveragePercentage' - The minimum line coverage percentage to report.
--
-- 'sortBy', 'describeCodeCoverages_sortBy' - Specifies how the results are sorted. Possible values are:
--
-- [FILE_PATH]
--     The results are sorted by file path.
--
-- [LINE_COVERAGE_PERCENTAGE]
--     The results are sorted by the percentage of lines that are covered.
--
-- 'maxLineCoveragePercentage', 'describeCodeCoverages_maxLineCoveragePercentage' - The maximum line coverage percentage to report.
--
-- 'maxResults', 'describeCodeCoverages_maxResults' - The maximum number of results to return.
--
-- 'reportArn', 'describeCodeCoverages_reportArn' - The ARN of the report for which test cases are returned.
newDescribeCodeCoverages ::
  -- | 'reportArn'
  Prelude.Text ->
  DescribeCodeCoverages
newDescribeCodeCoverages pReportArn_ =
  DescribeCodeCoverages'
    { sortOrder = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      minLineCoveragePercentage = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      maxLineCoveragePercentage = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      reportArn = pReportArn_
    }

-- | Specifies if the results are sorted in ascending or descending order.
describeCodeCoverages_sortOrder :: Lens.Lens' DescribeCodeCoverages (Prelude.Maybe SortOrderType)
describeCodeCoverages_sortOrder = Lens.lens (\DescribeCodeCoverages' {sortOrder} -> sortOrder) (\s@DescribeCodeCoverages' {} a -> s {sortOrder = a} :: DescribeCodeCoverages)

-- | The @nextToken@ value returned from a previous call to
-- @DescribeCodeCoverages@. This specifies the next item to return. To
-- return the beginning of the list, exclude this parameter.
describeCodeCoverages_nextToken :: Lens.Lens' DescribeCodeCoverages (Prelude.Maybe Prelude.Text)
describeCodeCoverages_nextToken = Lens.lens (\DescribeCodeCoverages' {nextToken} -> nextToken) (\s@DescribeCodeCoverages' {} a -> s {nextToken = a} :: DescribeCodeCoverages)

-- | The minimum line coverage percentage to report.
describeCodeCoverages_minLineCoveragePercentage :: Lens.Lens' DescribeCodeCoverages (Prelude.Maybe Prelude.Double)
describeCodeCoverages_minLineCoveragePercentage = Lens.lens (\DescribeCodeCoverages' {minLineCoveragePercentage} -> minLineCoveragePercentage) (\s@DescribeCodeCoverages' {} a -> s {minLineCoveragePercentage = a} :: DescribeCodeCoverages)

-- | Specifies how the results are sorted. Possible values are:
--
-- [FILE_PATH]
--     The results are sorted by file path.
--
-- [LINE_COVERAGE_PERCENTAGE]
--     The results are sorted by the percentage of lines that are covered.
describeCodeCoverages_sortBy :: Lens.Lens' DescribeCodeCoverages (Prelude.Maybe ReportCodeCoverageSortByType)
describeCodeCoverages_sortBy = Lens.lens (\DescribeCodeCoverages' {sortBy} -> sortBy) (\s@DescribeCodeCoverages' {} a -> s {sortBy = a} :: DescribeCodeCoverages)

-- | The maximum line coverage percentage to report.
describeCodeCoverages_maxLineCoveragePercentage :: Lens.Lens' DescribeCodeCoverages (Prelude.Maybe Prelude.Double)
describeCodeCoverages_maxLineCoveragePercentage = Lens.lens (\DescribeCodeCoverages' {maxLineCoveragePercentage} -> maxLineCoveragePercentage) (\s@DescribeCodeCoverages' {} a -> s {maxLineCoveragePercentage = a} :: DescribeCodeCoverages)

-- | The maximum number of results to return.
describeCodeCoverages_maxResults :: Lens.Lens' DescribeCodeCoverages (Prelude.Maybe Prelude.Natural)
describeCodeCoverages_maxResults = Lens.lens (\DescribeCodeCoverages' {maxResults} -> maxResults) (\s@DescribeCodeCoverages' {} a -> s {maxResults = a} :: DescribeCodeCoverages)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCodeCoveragesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "codeCoverages" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCodeCoverages where
  hashWithSalt _salt DescribeCodeCoverages' {..} =
    _salt `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` minLineCoveragePercentage
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` maxLineCoveragePercentage
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` reportArn

instance Prelude.NFData DescribeCodeCoverages where
  rnf DescribeCodeCoverages' {..} =
    Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf minLineCoveragePercentage
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf maxLineCoveragePercentage
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf reportArn

instance Data.ToHeaders DescribeCodeCoverages where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeBuild_20161006.DescribeCodeCoverages" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeCodeCoverages where
  toJSON DescribeCodeCoverages' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("sortOrder" Data..=) Prelude.<$> sortOrder,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("minLineCoveragePercentage" Data..=)
              Prelude.<$> minLineCoveragePercentage,
            ("sortBy" Data..=) Prelude.<$> sortBy,
            ("maxLineCoveragePercentage" Data..=)
              Prelude.<$> maxLineCoveragePercentage,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just ("reportArn" Data..= reportArn)
          ]
      )

instance Data.ToPath DescribeCodeCoverages where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeCodeCoverages where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeCodeCoveragesResponse' smart constructor.
data DescribeCodeCoveragesResponse = DescribeCodeCoveragesResponse'
  { -- | If there are more items to return, this contains a token that is passed
    -- to a subsequent call to @DescribeCodeCoverages@ to retrieve the next set
    -- of items.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of @CodeCoverage@ objects that contain the results.
    codeCoverages :: Prelude.Maybe [CodeCoverage],
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
-- 'nextToken', 'describeCodeCoveragesResponse_nextToken' - If there are more items to return, this contains a token that is passed
-- to a subsequent call to @DescribeCodeCoverages@ to retrieve the next set
-- of items.
--
-- 'codeCoverages', 'describeCodeCoveragesResponse_codeCoverages' - An array of @CodeCoverage@ objects that contain the results.
--
-- 'httpStatus', 'describeCodeCoveragesResponse_httpStatus' - The response's http status code.
newDescribeCodeCoveragesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCodeCoveragesResponse
newDescribeCodeCoveragesResponse pHttpStatus_ =
  DescribeCodeCoveragesResponse'
    { nextToken =
        Prelude.Nothing,
      codeCoverages = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are more items to return, this contains a token that is passed
-- to a subsequent call to @DescribeCodeCoverages@ to retrieve the next set
-- of items.
describeCodeCoveragesResponse_nextToken :: Lens.Lens' DescribeCodeCoveragesResponse (Prelude.Maybe Prelude.Text)
describeCodeCoveragesResponse_nextToken = Lens.lens (\DescribeCodeCoveragesResponse' {nextToken} -> nextToken) (\s@DescribeCodeCoveragesResponse' {} a -> s {nextToken = a} :: DescribeCodeCoveragesResponse)

-- | An array of @CodeCoverage@ objects that contain the results.
describeCodeCoveragesResponse_codeCoverages :: Lens.Lens' DescribeCodeCoveragesResponse (Prelude.Maybe [CodeCoverage])
describeCodeCoveragesResponse_codeCoverages = Lens.lens (\DescribeCodeCoveragesResponse' {codeCoverages} -> codeCoverages) (\s@DescribeCodeCoveragesResponse' {} a -> s {codeCoverages = a} :: DescribeCodeCoveragesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeCodeCoveragesResponse_httpStatus :: Lens.Lens' DescribeCodeCoveragesResponse Prelude.Int
describeCodeCoveragesResponse_httpStatus = Lens.lens (\DescribeCodeCoveragesResponse' {httpStatus} -> httpStatus) (\s@DescribeCodeCoveragesResponse' {} a -> s {httpStatus = a} :: DescribeCodeCoveragesResponse)

instance Prelude.NFData DescribeCodeCoveragesResponse where
  rnf DescribeCodeCoveragesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf codeCoverages
      `Prelude.seq` Prelude.rnf httpStatus
