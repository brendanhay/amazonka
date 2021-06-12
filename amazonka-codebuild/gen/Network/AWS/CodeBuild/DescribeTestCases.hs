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
-- Module      : Network.AWS.CodeBuild.DescribeTestCases
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of details about test cases for a report.
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.DescribeTestCases
  ( -- * Creating a Request
    DescribeTestCases (..),
    newDescribeTestCases,

    -- * Request Lenses
    describeTestCases_nextToken,
    describeTestCases_maxResults,
    describeTestCases_filter,
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

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeTestCases' smart constructor.
data DescribeTestCases = DescribeTestCases'
  { -- | During a previous call, the maximum number of items that can be returned
    -- is the value specified in @maxResults@. If there more items in the list,
    -- then a unique string called a /nextToken/ is returned. To get the next
    -- batch of items in the list, call this operation again, adding the next
    -- token to the call. To get all of the items in the list, keep calling
    -- this operation with each subsequent next token that is returned, until
    -- no more next tokens are returned.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of paginated test cases returned per response. Use
    -- @nextToken@ to iterate pages in the list of returned @TestCase@ objects.
    -- The default value is 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | A @TestCaseFilter@ object used to filter the returned reports.
    filter' :: Core.Maybe TestCaseFilter,
    -- | The ARN of the report for which test cases are returned.
    reportArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'maxResults', 'describeTestCases_maxResults' - The maximum number of paginated test cases returned per response. Use
-- @nextToken@ to iterate pages in the list of returned @TestCase@ objects.
-- The default value is 100.
--
-- 'filter'', 'describeTestCases_filter' - A @TestCaseFilter@ object used to filter the returned reports.
--
-- 'reportArn', 'describeTestCases_reportArn' - The ARN of the report for which test cases are returned.
newDescribeTestCases ::
  -- | 'reportArn'
  Core.Text ->
  DescribeTestCases
newDescribeTestCases pReportArn_ =
  DescribeTestCases'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      filter' = Core.Nothing,
      reportArn = pReportArn_
    }

-- | During a previous call, the maximum number of items that can be returned
-- is the value specified in @maxResults@. If there more items in the list,
-- then a unique string called a /nextToken/ is returned. To get the next
-- batch of items in the list, call this operation again, adding the next
-- token to the call. To get all of the items in the list, keep calling
-- this operation with each subsequent next token that is returned, until
-- no more next tokens are returned.
describeTestCases_nextToken :: Lens.Lens' DescribeTestCases (Core.Maybe Core.Text)
describeTestCases_nextToken = Lens.lens (\DescribeTestCases' {nextToken} -> nextToken) (\s@DescribeTestCases' {} a -> s {nextToken = a} :: DescribeTestCases)

-- | The maximum number of paginated test cases returned per response. Use
-- @nextToken@ to iterate pages in the list of returned @TestCase@ objects.
-- The default value is 100.
describeTestCases_maxResults :: Lens.Lens' DescribeTestCases (Core.Maybe Core.Natural)
describeTestCases_maxResults = Lens.lens (\DescribeTestCases' {maxResults} -> maxResults) (\s@DescribeTestCases' {} a -> s {maxResults = a} :: DescribeTestCases)

-- | A @TestCaseFilter@ object used to filter the returned reports.
describeTestCases_filter :: Lens.Lens' DescribeTestCases (Core.Maybe TestCaseFilter)
describeTestCases_filter = Lens.lens (\DescribeTestCases' {filter'} -> filter') (\s@DescribeTestCases' {} a -> s {filter' = a} :: DescribeTestCases)

-- | The ARN of the report for which test cases are returned.
describeTestCases_reportArn :: Lens.Lens' DescribeTestCases Core.Text
describeTestCases_reportArn = Lens.lens (\DescribeTestCases' {reportArn} -> reportArn) (\s@DescribeTestCases' {} a -> s {reportArn = a} :: DescribeTestCases)

instance Core.AWSPager DescribeTestCases where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTestCasesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTestCasesResponse_testCases
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeTestCases_nextToken
          Lens..~ rs
          Lens.^? describeTestCasesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest DescribeTestCases where
  type
    AWSResponse DescribeTestCases =
      DescribeTestCasesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTestCasesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "testCases" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeTestCases

instance Core.NFData DescribeTestCases

instance Core.ToHeaders DescribeTestCases where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.DescribeTestCases" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeTestCases where
  toJSON DescribeTestCases' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("filter" Core..=) Core.<$> filter',
            Core.Just ("reportArn" Core..= reportArn)
          ]
      )

instance Core.ToPath DescribeTestCases where
  toPath = Core.const "/"

instance Core.ToQuery DescribeTestCases where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeTestCasesResponse' smart constructor.
data DescribeTestCasesResponse = DescribeTestCasesResponse'
  { -- | During a previous call, the maximum number of items that can be returned
    -- is the value specified in @maxResults@. If there more items in the list,
    -- then a unique string called a /nextToken/ is returned. To get the next
    -- batch of items in the list, call this operation again, adding the next
    -- token to the call. To get all of the items in the list, keep calling
    -- this operation with each subsequent next token that is returned, until
    -- no more next tokens are returned.
    nextToken :: Core.Maybe Core.Text,
    -- | The returned list of test cases.
    testCases :: Core.Maybe [TestCase],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeTestCasesResponse
newDescribeTestCasesResponse pHttpStatus_ =
  DescribeTestCasesResponse'
    { nextToken =
        Core.Nothing,
      testCases = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | During a previous call, the maximum number of items that can be returned
-- is the value specified in @maxResults@. If there more items in the list,
-- then a unique string called a /nextToken/ is returned. To get the next
-- batch of items in the list, call this operation again, adding the next
-- token to the call. To get all of the items in the list, keep calling
-- this operation with each subsequent next token that is returned, until
-- no more next tokens are returned.
describeTestCasesResponse_nextToken :: Lens.Lens' DescribeTestCasesResponse (Core.Maybe Core.Text)
describeTestCasesResponse_nextToken = Lens.lens (\DescribeTestCasesResponse' {nextToken} -> nextToken) (\s@DescribeTestCasesResponse' {} a -> s {nextToken = a} :: DescribeTestCasesResponse)

-- | The returned list of test cases.
describeTestCasesResponse_testCases :: Lens.Lens' DescribeTestCasesResponse (Core.Maybe [TestCase])
describeTestCasesResponse_testCases = Lens.lens (\DescribeTestCasesResponse' {testCases} -> testCases) (\s@DescribeTestCasesResponse' {} a -> s {testCases = a} :: DescribeTestCasesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeTestCasesResponse_httpStatus :: Lens.Lens' DescribeTestCasesResponse Core.Int
describeTestCasesResponse_httpStatus = Lens.lens (\DescribeTestCasesResponse' {httpStatus} -> httpStatus) (\s@DescribeTestCasesResponse' {} a -> s {httpStatus = a} :: DescribeTestCasesResponse)

instance Core.NFData DescribeTestCasesResponse
