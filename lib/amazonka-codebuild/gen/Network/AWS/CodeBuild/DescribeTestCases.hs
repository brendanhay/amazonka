{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.DescribeTestCases
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of details about test cases for a report.
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.DescribeTestCases
  ( -- * Creating a request
    DescribeTestCases (..),
    mkDescribeTestCases,

    -- ** Request lenses
    dtcNextToken,
    dtcFilter,
    dtcReportARN,
    dtcMaxResults,

    -- * Destructuring the response
    DescribeTestCasesResponse (..),
    mkDescribeTestCasesResponse,

    -- ** Response lenses
    dtcrsNextToken,
    dtcrsTestCases,
    dtcrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeTestCases' smart constructor.
data DescribeTestCases = DescribeTestCases'
  { -- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A @TestCaseFilter@ object used to filter the returned reports.
    filter :: Lude.Maybe TestCaseFilter,
    -- | The ARN of the report for which test cases are returned.
    reportARN :: Lude.Text,
    -- | The maximum number of paginated test cases returned per response. Use @nextToken@ to iterate pages in the list of returned @TestCase@ objects. The default value is 100.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTestCases' with the minimum fields required to make a request.
--
-- * 'nextToken' - During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
-- * 'filter' - A @TestCaseFilter@ object used to filter the returned reports.
-- * 'reportARN' - The ARN of the report for which test cases are returned.
-- * 'maxResults' - The maximum number of paginated test cases returned per response. Use @nextToken@ to iterate pages in the list of returned @TestCase@ objects. The default value is 100.
mkDescribeTestCases ::
  -- | 'reportARN'
  Lude.Text ->
  DescribeTestCases
mkDescribeTestCases pReportARN_ =
  DescribeTestCases'
    { nextToken = Lude.Nothing,
      filter = Lude.Nothing,
      reportARN = pReportARN_,
      maxResults = Lude.Nothing
    }

-- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcNextToken :: Lens.Lens' DescribeTestCases (Lude.Maybe Lude.Text)
dtcNextToken = Lens.lens (nextToken :: DescribeTestCases -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeTestCases)
{-# DEPRECATED dtcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A @TestCaseFilter@ object used to filter the returned reports.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcFilter :: Lens.Lens' DescribeTestCases (Lude.Maybe TestCaseFilter)
dtcFilter = Lens.lens (filter :: DescribeTestCases -> Lude.Maybe TestCaseFilter) (\s a -> s {filter = a} :: DescribeTestCases)
{-# DEPRECATED dtcFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The ARN of the report for which test cases are returned.
--
-- /Note:/ Consider using 'reportARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcReportARN :: Lens.Lens' DescribeTestCases Lude.Text
dtcReportARN = Lens.lens (reportARN :: DescribeTestCases -> Lude.Text) (\s a -> s {reportARN = a} :: DescribeTestCases)
{-# DEPRECATED dtcReportARN "Use generic-lens or generic-optics with 'reportARN' instead." #-}

-- | The maximum number of paginated test cases returned per response. Use @nextToken@ to iterate pages in the list of returned @TestCase@ objects. The default value is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcMaxResults :: Lens.Lens' DescribeTestCases (Lude.Maybe Lude.Natural)
dtcMaxResults = Lens.lens (maxResults :: DescribeTestCases -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeTestCases)
{-# DEPRECATED dtcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeTestCases where
  page rq rs
    | Page.stop (rs Lens.^. dtcrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dtcrsTestCases) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dtcNextToken Lens..~ rs Lens.^. dtcrsNextToken

instance Lude.AWSRequest DescribeTestCases where
  type Rs DescribeTestCases = DescribeTestCasesResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeTestCasesResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "testCases" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTestCases where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.DescribeTestCases" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeTestCases where
  toJSON DescribeTestCases' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("filter" Lude..=) Lude.<$> filter,
            Lude.Just ("reportArn" Lude..= reportARN),
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeTestCases where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTestCases where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeTestCasesResponse' smart constructor.
data DescribeTestCasesResponse = DescribeTestCasesResponse'
  { -- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The returned list of test cases.
    testCases :: Lude.Maybe [TestCase],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTestCasesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
-- * 'testCases' - The returned list of test cases.
-- * 'responseStatus' - The response status code.
mkDescribeTestCasesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTestCasesResponse
mkDescribeTestCasesResponse pResponseStatus_ =
  DescribeTestCasesResponse'
    { nextToken = Lude.Nothing,
      testCases = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcrsNextToken :: Lens.Lens' DescribeTestCasesResponse (Lude.Maybe Lude.Text)
dtcrsNextToken = Lens.lens (nextToken :: DescribeTestCasesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeTestCasesResponse)
{-# DEPRECATED dtcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The returned list of test cases.
--
-- /Note:/ Consider using 'testCases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcrsTestCases :: Lens.Lens' DescribeTestCasesResponse (Lude.Maybe [TestCase])
dtcrsTestCases = Lens.lens (testCases :: DescribeTestCasesResponse -> Lude.Maybe [TestCase]) (\s a -> s {testCases = a} :: DescribeTestCasesResponse)
{-# DEPRECATED dtcrsTestCases "Use generic-lens or generic-optics with 'testCases' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcrsResponseStatus :: Lens.Lens' DescribeTestCasesResponse Lude.Int
dtcrsResponseStatus = Lens.lens (responseStatus :: DescribeTestCasesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTestCasesResponse)
{-# DEPRECATED dtcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
