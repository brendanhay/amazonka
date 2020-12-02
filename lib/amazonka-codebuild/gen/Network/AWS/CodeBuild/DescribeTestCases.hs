{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
--
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.DescribeTestCases
  ( -- * Creating a Request
    describeTestCases,
    DescribeTestCases,

    -- * Request Lenses
    dtcNextToken,
    dtcFilter,
    dtcMaxResults,
    dtcReportARN,

    -- * Destructuring the Response
    describeTestCasesResponse,
    DescribeTestCasesResponse,

    -- * Response Lenses
    dtcrsNextToken,
    dtcrsTestCases,
    dtcrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeTestCases' smart constructor.
data DescribeTestCases = DescribeTestCases'
  { _dtcNextToken ::
      !(Maybe Text),
    _dtcFilter :: !(Maybe TestCaseFilter),
    _dtcMaxResults :: !(Maybe Nat),
    _dtcReportARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeTestCases' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtcNextToken' - During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- * 'dtcFilter' - A @TestCaseFilter@ object used to filter the returned reports.
--
-- * 'dtcMaxResults' - The maximum number of paginated test cases returned per response. Use @nextToken@ to iterate pages in the list of returned @TestCase@ objects. The default value is 100.
--
-- * 'dtcReportARN' - The ARN of the report for which test cases are returned.
describeTestCases ::
  -- | 'dtcReportARN'
  Text ->
  DescribeTestCases
describeTestCases pReportARN_ =
  DescribeTestCases'
    { _dtcNextToken = Nothing,
      _dtcFilter = Nothing,
      _dtcMaxResults = Nothing,
      _dtcReportARN = pReportARN_
    }

-- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
dtcNextToken :: Lens' DescribeTestCases (Maybe Text)
dtcNextToken = lens _dtcNextToken (\s a -> s {_dtcNextToken = a})

-- | A @TestCaseFilter@ object used to filter the returned reports.
dtcFilter :: Lens' DescribeTestCases (Maybe TestCaseFilter)
dtcFilter = lens _dtcFilter (\s a -> s {_dtcFilter = a})

-- | The maximum number of paginated test cases returned per response. Use @nextToken@ to iterate pages in the list of returned @TestCase@ objects. The default value is 100.
dtcMaxResults :: Lens' DescribeTestCases (Maybe Natural)
dtcMaxResults = lens _dtcMaxResults (\s a -> s {_dtcMaxResults = a}) . mapping _Nat

-- | The ARN of the report for which test cases are returned.
dtcReportARN :: Lens' DescribeTestCases Text
dtcReportARN = lens _dtcReportARN (\s a -> s {_dtcReportARN = a})

instance AWSPager DescribeTestCases where
  page rq rs
    | stop (rs ^. dtcrsNextToken) = Nothing
    | stop (rs ^. dtcrsTestCases) = Nothing
    | otherwise = Just $ rq & dtcNextToken .~ rs ^. dtcrsNextToken

instance AWSRequest DescribeTestCases where
  type Rs DescribeTestCases = DescribeTestCasesResponse
  request = postJSON codeBuild
  response =
    receiveJSON
      ( \s h x ->
          DescribeTestCasesResponse'
            <$> (x .?> "nextToken")
            <*> (x .?> "testCases" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeTestCases

instance NFData DescribeTestCases

instance ToHeaders DescribeTestCases where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeBuild_20161006.DescribeTestCases" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeTestCases where
  toJSON DescribeTestCases' {..} =
    object
      ( catMaybes
          [ ("nextToken" .=) <$> _dtcNextToken,
            ("filter" .=) <$> _dtcFilter,
            ("maxResults" .=) <$> _dtcMaxResults,
            Just ("reportArn" .= _dtcReportARN)
          ]
      )

instance ToPath DescribeTestCases where
  toPath = const "/"

instance ToQuery DescribeTestCases where
  toQuery = const mempty

-- | /See:/ 'describeTestCasesResponse' smart constructor.
data DescribeTestCasesResponse = DescribeTestCasesResponse'
  { _dtcrsNextToken ::
      !(Maybe Text),
    _dtcrsTestCases :: !(Maybe [TestCase]),
    _dtcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeTestCasesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtcrsNextToken' - During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- * 'dtcrsTestCases' - The returned list of test cases.
--
-- * 'dtcrsResponseStatus' - -- | The response status code.
describeTestCasesResponse ::
  -- | 'dtcrsResponseStatus'
  Int ->
  DescribeTestCasesResponse
describeTestCasesResponse pResponseStatus_ =
  DescribeTestCasesResponse'
    { _dtcrsNextToken = Nothing,
      _dtcrsTestCases = Nothing,
      _dtcrsResponseStatus = pResponseStatus_
    }

-- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
dtcrsNextToken :: Lens' DescribeTestCasesResponse (Maybe Text)
dtcrsNextToken = lens _dtcrsNextToken (\s a -> s {_dtcrsNextToken = a})

-- | The returned list of test cases.
dtcrsTestCases :: Lens' DescribeTestCasesResponse [TestCase]
dtcrsTestCases = lens _dtcrsTestCases (\s a -> s {_dtcrsTestCases = a}) . _Default . _Coerce

-- | -- | The response status code.
dtcrsResponseStatus :: Lens' DescribeTestCasesResponse Int
dtcrsResponseStatus = lens _dtcrsResponseStatus (\s a -> s {_dtcrsResponseStatus = a})

instance NFData DescribeTestCasesResponse
