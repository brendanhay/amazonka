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
-- Module      : Network.AWS.SSM.DescribeOpsItems
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Query a set of OpsItems. You must have permission in AWS Identity and Access Management (IAM) to query a list of OpsItems. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-getting-started.html Getting started with OpsCenter> in the /AWS Systems Manager User Guide/ .
--
--
-- Operations engineers and IT professionals use OpsCenter to view, investigate, and remediate operational issues impacting the performance and health of their AWS resources. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter.html AWS Systems Manager OpsCenter> in the /AWS Systems Manager User Guide/ .
--
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeOpsItems
  ( -- * Creating a Request
    describeOpsItems,
    DescribeOpsItems,

    -- * Request Lenses
    doiOpsItemFilters,
    doiNextToken,
    doiMaxResults,

    -- * Destructuring the Response
    describeOpsItemsResponse,
    DescribeOpsItemsResponse,

    -- * Response Lenses
    doirsNextToken,
    doirsOpsItemSummaries,
    doirsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types

-- | /See:/ 'describeOpsItems' smart constructor.
data DescribeOpsItems = DescribeOpsItems'
  { _doiOpsItemFilters ::
      !(Maybe [OpsItemFilter]),
    _doiNextToken :: !(Maybe Text),
    _doiMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeOpsItems' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'doiOpsItemFilters' - One or more filters to limit the response.     * Key: CreatedTime Operations: GreaterThan, LessThan     * Key: LastModifiedBy Operations: Contains, Equals     * Key: LastModifiedTime Operations: GreaterThan, LessThan     * Key: Priority Operations: Equals     * Key: Source Operations: Contains, Equals     * Key: Status Operations: Equals     * Key: Title Operations: Contains     * Key: OperationalData* Operations: Equals     * Key: OperationalDataKey Operations: Equals     * Key: OperationalDataValue Operations: Equals, Contains     * Key: OpsItemId Operations: Equals     * Key: ResourceId Operations: Contains     * Key: AutomationId Operations: Equals *If you filter the response by using the OperationalData operator, specify a key-value pair by using the following JSON format: {"key":"key_name","value":"a_value"}
--
-- * 'doiNextToken' - A token to start the list. Use this token to get the next set of results.
--
-- * 'doiMaxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
describeOpsItems ::
  DescribeOpsItems
describeOpsItems =
  DescribeOpsItems'
    { _doiOpsItemFilters = Nothing,
      _doiNextToken = Nothing,
      _doiMaxResults = Nothing
    }

-- | One or more filters to limit the response.     * Key: CreatedTime Operations: GreaterThan, LessThan     * Key: LastModifiedBy Operations: Contains, Equals     * Key: LastModifiedTime Operations: GreaterThan, LessThan     * Key: Priority Operations: Equals     * Key: Source Operations: Contains, Equals     * Key: Status Operations: Equals     * Key: Title Operations: Contains     * Key: OperationalData* Operations: Equals     * Key: OperationalDataKey Operations: Equals     * Key: OperationalDataValue Operations: Equals, Contains     * Key: OpsItemId Operations: Equals     * Key: ResourceId Operations: Contains     * Key: AutomationId Operations: Equals *If you filter the response by using the OperationalData operator, specify a key-value pair by using the following JSON format: {"key":"key_name","value":"a_value"}
doiOpsItemFilters :: Lens' DescribeOpsItems [OpsItemFilter]
doiOpsItemFilters = lens _doiOpsItemFilters (\s a -> s {_doiOpsItemFilters = a}) . _Default . _Coerce

-- | A token to start the list. Use this token to get the next set of results.
doiNextToken :: Lens' DescribeOpsItems (Maybe Text)
doiNextToken = lens _doiNextToken (\s a -> s {_doiNextToken = a})

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
doiMaxResults :: Lens' DescribeOpsItems (Maybe Natural)
doiMaxResults = lens _doiMaxResults (\s a -> s {_doiMaxResults = a}) . mapping _Nat

instance AWSPager DescribeOpsItems where
  page rq rs
    | stop (rs ^. doirsNextToken) = Nothing
    | stop (rs ^. doirsOpsItemSummaries) = Nothing
    | otherwise = Just $ rq & doiNextToken .~ rs ^. doirsNextToken

instance AWSRequest DescribeOpsItems where
  type Rs DescribeOpsItems = DescribeOpsItemsResponse
  request = postJSON ssm
  response =
    receiveJSON
      ( \s h x ->
          DescribeOpsItemsResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "OpsItemSummaries" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeOpsItems

instance NFData DescribeOpsItems

instance ToHeaders DescribeOpsItems where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AmazonSSM.DescribeOpsItems" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeOpsItems where
  toJSON DescribeOpsItems' {..} =
    object
      ( catMaybes
          [ ("OpsItemFilters" .=) <$> _doiOpsItemFilters,
            ("NextToken" .=) <$> _doiNextToken,
            ("MaxResults" .=) <$> _doiMaxResults
          ]
      )

instance ToPath DescribeOpsItems where
  toPath = const "/"

instance ToQuery DescribeOpsItems where
  toQuery = const mempty

-- | /See:/ 'describeOpsItemsResponse' smart constructor.
data DescribeOpsItemsResponse = DescribeOpsItemsResponse'
  { _doirsNextToken ::
      !(Maybe Text),
    _doirsOpsItemSummaries ::
      !(Maybe [OpsItemSummary]),
    _doirsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeOpsItemsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'doirsNextToken' - The token for the next set of items to return. Use this token to get the next set of results.
--
-- * 'doirsOpsItemSummaries' - A list of OpsItems.
--
-- * 'doirsResponseStatus' - -- | The response status code.
describeOpsItemsResponse ::
  -- | 'doirsResponseStatus'
  Int ->
  DescribeOpsItemsResponse
describeOpsItemsResponse pResponseStatus_ =
  DescribeOpsItemsResponse'
    { _doirsNextToken = Nothing,
      _doirsOpsItemSummaries = Nothing,
      _doirsResponseStatus = pResponseStatus_
    }

-- | The token for the next set of items to return. Use this token to get the next set of results.
doirsNextToken :: Lens' DescribeOpsItemsResponse (Maybe Text)
doirsNextToken = lens _doirsNextToken (\s a -> s {_doirsNextToken = a})

-- | A list of OpsItems.
doirsOpsItemSummaries :: Lens' DescribeOpsItemsResponse [OpsItemSummary]
doirsOpsItemSummaries = lens _doirsOpsItemSummaries (\s a -> s {_doirsOpsItemSummaries = a}) . _Default . _Coerce

-- | -- | The response status code.
doirsResponseStatus :: Lens' DescribeOpsItemsResponse Int
doirsResponseStatus = lens _doirsResponseStatus (\s a -> s {_doirsResponseStatus = a})

instance NFData DescribeOpsItemsResponse
