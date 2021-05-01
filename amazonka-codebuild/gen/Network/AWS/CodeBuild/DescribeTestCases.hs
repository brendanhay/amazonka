{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
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
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of paginated test cases returned per response. Use
    -- @nextToken@ to iterate pages in the list of returned @TestCase@ objects.
    -- The default value is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A @TestCaseFilter@ object used to filter the returned reports.
    filter' :: Prelude.Maybe TestCaseFilter,
    -- | The ARN of the report for which test cases are returned.
    reportArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DescribeTestCases
newDescribeTestCases pReportArn_ =
  DescribeTestCases'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filter' = Prelude.Nothing,
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

-- | The maximum number of paginated test cases returned per response. Use
-- @nextToken@ to iterate pages in the list of returned @TestCase@ objects.
-- The default value is 100.
describeTestCases_maxResults :: Lens.Lens' DescribeTestCases (Prelude.Maybe Prelude.Natural)
describeTestCases_maxResults = Lens.lens (\DescribeTestCases' {maxResults} -> maxResults) (\s@DescribeTestCases' {} a -> s {maxResults = a} :: DescribeTestCases)

-- | A @TestCaseFilter@ object used to filter the returned reports.
describeTestCases_filter :: Lens.Lens' DescribeTestCases (Prelude.Maybe TestCaseFilter)
describeTestCases_filter = Lens.lens (\DescribeTestCases' {filter'} -> filter') (\s@DescribeTestCases' {} a -> s {filter' = a} :: DescribeTestCases)

-- | The ARN of the report for which test cases are returned.
describeTestCases_reportArn :: Lens.Lens' DescribeTestCases Prelude.Text
describeTestCases_reportArn = Lens.lens (\DescribeTestCases' {reportArn} -> reportArn) (\s@DescribeTestCases' {} a -> s {reportArn = a} :: DescribeTestCases)

instance Pager.AWSPager DescribeTestCases where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeTestCasesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeTestCasesResponse_testCases
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeTestCases_nextToken
          Lens..~ rs
          Lens.^? describeTestCasesResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeTestCases where
  type Rs DescribeTestCases = DescribeTestCasesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTestCasesResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> ( x Prelude..?> "testCases"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTestCases

instance Prelude.NFData DescribeTestCases

instance Prelude.ToHeaders DescribeTestCases where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeBuild_20161006.DescribeTestCases" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeTestCases where
  toJSON DescribeTestCases' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("nextToken" Prelude..=) Prelude.<$> nextToken,
            ("maxResults" Prelude..=) Prelude.<$> maxResults,
            ("filter" Prelude..=) Prelude.<$> filter',
            Prelude.Just ("reportArn" Prelude..= reportArn)
          ]
      )

instance Prelude.ToPath DescribeTestCases where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeTestCases where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
describeTestCasesResponse_testCases = Lens.lens (\DescribeTestCasesResponse' {testCases} -> testCases) (\s@DescribeTestCasesResponse' {} a -> s {testCases = a} :: DescribeTestCasesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeTestCasesResponse_httpStatus :: Lens.Lens' DescribeTestCasesResponse Prelude.Int
describeTestCasesResponse_httpStatus = Lens.lens (\DescribeTestCasesResponse' {httpStatus} -> httpStatus) (\s@DescribeTestCasesResponse' {} a -> s {httpStatus = a} :: DescribeTestCasesResponse)

instance Prelude.NFData DescribeTestCasesResponse
