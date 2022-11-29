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
-- Module      : Amazonka.CodeBuild.DescribeTestCases
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of details about test cases for a report.
--
-- This operation returns paginated results.
module Amazonka.CodeBuild.DescribeTestCases
  ( -- * Creating a Request
    DescribeTestCases (..),
    newDescribeTestCases,

    -- * Request Lenses
    describeTestCases_nextToken,
    describeTestCases_filter,
    describeTestCases_maxResults,
    describeTestCases_reportArn,

    -- * Destructuring the Response
    DescribeTestCasesResponse (..),
    newDescribeTestCasesResponse,

    -- * Response Lenses
    describeTestCasesResponse_nextToken,
    describeTestCasesResponse_testCases,
    describeTestCasesResponse_httpStatus,
  )
where

import Amazonka.CodeBuild.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTestCases' smart constructor.
data DescribeTestCases = DescribeTestCases'
  { -- | During a previous call, the maximum number of items that can be returned
    -- is the value specified in @maxResults@. If there more items in the list,
    -- then a unique string called a /nextToken/ is returned. To get the next
    -- batch of items in the list, call this operation again, adding the next
    -- token to the call. To get all of the items in the list, keep calling
    -- this operation with each subsequent next token that is returned, until
    -- no more next tokens are returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A @TestCaseFilter@ object used to filter the returned reports.
    filter' :: Prelude.Maybe TestCaseFilter,
    -- | The maximum number of paginated test cases returned per response. Use
    -- @nextToken@ to iterate pages in the list of returned @TestCase@ objects.
    -- The default value is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the report for which test cases are returned.
    reportArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTestCases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeTestCases_nextToken' - During a previous call, the maximum number of items that can be returned
-- is the value specified in @maxResults@. If there more items in the list,
-- then a unique string called a /nextToken/ is returned. To get the next
-- batch of items in the list, call this operation again, adding the next
-- token to the call. To get all of the items in the list, keep calling
-- this operation with each subsequent next token that is returned, until
-- no more next tokens are returned.
--
-- 'filter'', 'describeTestCases_filter' - A @TestCaseFilter@ object used to filter the returned reports.
--
-- 'maxResults', 'describeTestCases_maxResults' - The maximum number of paginated test cases returned per response. Use
-- @nextToken@ to iterate pages in the list of returned @TestCase@ objects.
-- The default value is 100.
--
-- 'reportArn', 'describeTestCases_reportArn' - The ARN of the report for which test cases are returned.
newDescribeTestCases ::
  -- | 'reportArn'
  Prelude.Text ->
  DescribeTestCases
newDescribeTestCases pReportArn_ =
  DescribeTestCases'
    { nextToken = Prelude.Nothing,
      filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      reportArn = pReportArn_
    }

-- | During a previous call, the maximum number of items that can be returned
-- is the value specified in @maxResults@. If there more items in the list,
-- then a unique string called a /nextToken/ is returned. To get the next
-- batch of items in the list, call this operation again, adding the next
-- token to the call. To get all of the items in the list, keep calling
-- this operation with each subsequent next token that is returned, until
-- no more next tokens are returned.
describeTestCases_nextToken :: Lens.Lens' DescribeTestCases (Prelude.Maybe Prelude.Text)
describeTestCases_nextToken = Lens.lens (\DescribeTestCases' {nextToken} -> nextToken) (\s@DescribeTestCases' {} a -> s {nextToken = a} :: DescribeTestCases)

-- | A @TestCaseFilter@ object used to filter the returned reports.
describeTestCases_filter :: Lens.Lens' DescribeTestCases (Prelude.Maybe TestCaseFilter)
describeTestCases_filter = Lens.lens (\DescribeTestCases' {filter'} -> filter') (\s@DescribeTestCases' {} a -> s {filter' = a} :: DescribeTestCases)

-- | The maximum number of paginated test cases returned per response. Use
-- @nextToken@ to iterate pages in the list of returned @TestCase@ objects.
-- The default value is 100.
describeTestCases_maxResults :: Lens.Lens' DescribeTestCases (Prelude.Maybe Prelude.Natural)
describeTestCases_maxResults = Lens.lens (\DescribeTestCases' {maxResults} -> maxResults) (\s@DescribeTestCases' {} a -> s {maxResults = a} :: DescribeTestCases)

-- | The ARN of the report for which test cases are returned.
describeTestCases_reportArn :: Lens.Lens' DescribeTestCases Prelude.Text
describeTestCases_reportArn = Lens.lens (\DescribeTestCases' {reportArn} -> reportArn) (\s@DescribeTestCases' {} a -> s {reportArn = a} :: DescribeTestCases)

instance Core.AWSPager DescribeTestCases where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTestCasesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTestCasesResponse_testCases
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeTestCases_nextToken
          Lens..~ rs
          Lens.^? describeTestCasesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeTestCases where
  type
    AWSResponse DescribeTestCases =
      DescribeTestCasesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTestCasesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "testCases" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTestCases where
  hashWithSalt _salt DescribeTestCases' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` reportArn

instance Prelude.NFData DescribeTestCases where
  rnf DescribeTestCases' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf reportArn

instance Core.ToHeaders DescribeTestCases where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.DescribeTestCases" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeTestCases where
  toJSON DescribeTestCases' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("filter" Core..=) Prelude.<$> filter',
            ("maxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("reportArn" Core..= reportArn)
          ]
      )

instance Core.ToPath DescribeTestCases where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeTestCases where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTestCasesResponse' smart constructor.
data DescribeTestCasesResponse = DescribeTestCasesResponse'
  { -- | During a previous call, the maximum number of items that can be returned
    -- is the value specified in @maxResults@. If there more items in the list,
    -- then a unique string called a /nextToken/ is returned. To get the next
    -- batch of items in the list, call this operation again, adding the next
    -- token to the call. To get all of the items in the list, keep calling
    -- this operation with each subsequent next token that is returned, until
    -- no more next tokens are returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The returned list of test cases.
    testCases :: Prelude.Maybe [TestCase],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTestCasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeTestCasesResponse_nextToken' - During a previous call, the maximum number of items that can be returned
-- is the value specified in @maxResults@. If there more items in the list,
-- then a unique string called a /nextToken/ is returned. To get the next
-- batch of items in the list, call this operation again, adding the next
-- token to the call. To get all of the items in the list, keep calling
-- this operation with each subsequent next token that is returned, until
-- no more next tokens are returned.
--
-- 'testCases', 'describeTestCasesResponse_testCases' - The returned list of test cases.
--
-- 'httpStatus', 'describeTestCasesResponse_httpStatus' - The response's http status code.
newDescribeTestCasesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTestCasesResponse
newDescribeTestCasesResponse pHttpStatus_ =
  DescribeTestCasesResponse'
    { nextToken =
        Prelude.Nothing,
      testCases = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | During a previous call, the maximum number of items that can be returned
-- is the value specified in @maxResults@. If there more items in the list,
-- then a unique string called a /nextToken/ is returned. To get the next
-- batch of items in the list, call this operation again, adding the next
-- token to the call. To get all of the items in the list, keep calling
-- this operation with each subsequent next token that is returned, until
-- no more next tokens are returned.
describeTestCasesResponse_nextToken :: Lens.Lens' DescribeTestCasesResponse (Prelude.Maybe Prelude.Text)
describeTestCasesResponse_nextToken = Lens.lens (\DescribeTestCasesResponse' {nextToken} -> nextToken) (\s@DescribeTestCasesResponse' {} a -> s {nextToken = a} :: DescribeTestCasesResponse)

-- | The returned list of test cases.
describeTestCasesResponse_testCases :: Lens.Lens' DescribeTestCasesResponse (Prelude.Maybe [TestCase])
describeTestCasesResponse_testCases = Lens.lens (\DescribeTestCasesResponse' {testCases} -> testCases) (\s@DescribeTestCasesResponse' {} a -> s {testCases = a} :: DescribeTestCasesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeTestCasesResponse_httpStatus :: Lens.Lens' DescribeTestCasesResponse Prelude.Int
describeTestCasesResponse_httpStatus = Lens.lens (\DescribeTestCasesResponse' {httpStatus} -> httpStatus) (\s@DescribeTestCasesResponse' {} a -> s {httpStatus = a} :: DescribeTestCasesResponse)

instance Prelude.NFData DescribeTestCasesResponse where
  rnf DescribeTestCasesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf testCases
      `Prelude.seq` Prelude.rnf httpStatus
