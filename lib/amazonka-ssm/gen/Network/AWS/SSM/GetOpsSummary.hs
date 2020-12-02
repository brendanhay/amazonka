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
-- Module      : Network.AWS.SSM.GetOpsSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- View a summary of OpsItems based on specified filters and aggregators.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SSM.GetOpsSummary
  ( -- * Creating a Request
    getOpsSummary,
    GetOpsSummary,

    -- * Request Lenses
    gosAggregators,
    gosSyncName,
    gosFilters,
    gosResultAttributes,
    gosNextToken,
    gosMaxResults,

    -- * Destructuring the Response
    getOpsSummaryResponse,
    GetOpsSummaryResponse,

    -- * Response Lenses
    gosrsEntities,
    gosrsNextToken,
    gosrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types

-- | /See:/ 'getOpsSummary' smart constructor.
data GetOpsSummary = GetOpsSummary'
  { _gosAggregators ::
      !(Maybe (List1 OpsAggregator)),
    _gosSyncName :: !(Maybe Text),
    _gosFilters :: !(Maybe (List1 OpsFilter)),
    _gosResultAttributes :: !(Maybe (List1 OpsResultAttribute)),
    _gosNextToken :: !(Maybe Text),
    _gosMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetOpsSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gosAggregators' - Optional aggregators that return counts of OpsItems based on one or more expressions.
--
-- * 'gosSyncName' - Specify the name of a resource data sync to get.
--
-- * 'gosFilters' - Optional filters used to scope down the returned OpsItems.
--
-- * 'gosResultAttributes' - The OpsItem data type to return.
--
-- * 'gosNextToken' - A token to start the list. Use this token to get the next set of results.
--
-- * 'gosMaxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
getOpsSummary ::
  GetOpsSummary
getOpsSummary =
  GetOpsSummary'
    { _gosAggregators = Nothing,
      _gosSyncName = Nothing,
      _gosFilters = Nothing,
      _gosResultAttributes = Nothing,
      _gosNextToken = Nothing,
      _gosMaxResults = Nothing
    }

-- | Optional aggregators that return counts of OpsItems based on one or more expressions.
gosAggregators :: Lens' GetOpsSummary (Maybe (NonEmpty OpsAggregator))
gosAggregators = lens _gosAggregators (\s a -> s {_gosAggregators = a}) . mapping _List1

-- | Specify the name of a resource data sync to get.
gosSyncName :: Lens' GetOpsSummary (Maybe Text)
gosSyncName = lens _gosSyncName (\s a -> s {_gosSyncName = a})

-- | Optional filters used to scope down the returned OpsItems.
gosFilters :: Lens' GetOpsSummary (Maybe (NonEmpty OpsFilter))
gosFilters = lens _gosFilters (\s a -> s {_gosFilters = a}) . mapping _List1

-- | The OpsItem data type to return.
gosResultAttributes :: Lens' GetOpsSummary (Maybe (NonEmpty OpsResultAttribute))
gosResultAttributes = lens _gosResultAttributes (\s a -> s {_gosResultAttributes = a}) . mapping _List1

-- | A token to start the list. Use this token to get the next set of results.
gosNextToken :: Lens' GetOpsSummary (Maybe Text)
gosNextToken = lens _gosNextToken (\s a -> s {_gosNextToken = a})

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
gosMaxResults :: Lens' GetOpsSummary (Maybe Natural)
gosMaxResults = lens _gosMaxResults (\s a -> s {_gosMaxResults = a}) . mapping _Nat

instance AWSPager GetOpsSummary where
  page rq rs
    | stop (rs ^. gosrsNextToken) = Nothing
    | stop (rs ^. gosrsEntities) = Nothing
    | otherwise = Just $ rq & gosNextToken .~ rs ^. gosrsNextToken

instance AWSRequest GetOpsSummary where
  type Rs GetOpsSummary = GetOpsSummaryResponse
  request = postJSON ssm
  response =
    receiveJSON
      ( \s h x ->
          GetOpsSummaryResponse'
            <$> (x .?> "Entities" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable GetOpsSummary

instance NFData GetOpsSummary

instance ToHeaders GetOpsSummary where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AmazonSSM.GetOpsSummary" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetOpsSummary where
  toJSON GetOpsSummary' {..} =
    object
      ( catMaybes
          [ ("Aggregators" .=) <$> _gosAggregators,
            ("SyncName" .=) <$> _gosSyncName,
            ("Filters" .=) <$> _gosFilters,
            ("ResultAttributes" .=) <$> _gosResultAttributes,
            ("NextToken" .=) <$> _gosNextToken,
            ("MaxResults" .=) <$> _gosMaxResults
          ]
      )

instance ToPath GetOpsSummary where
  toPath = const "/"

instance ToQuery GetOpsSummary where
  toQuery = const mempty

-- | /See:/ 'getOpsSummaryResponse' smart constructor.
data GetOpsSummaryResponse = GetOpsSummaryResponse'
  { _gosrsEntities ::
      !(Maybe [OpsEntity]),
    _gosrsNextToken :: !(Maybe Text),
    _gosrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetOpsSummaryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gosrsEntities' - The list of aggregated and filtered OpsItems.
--
-- * 'gosrsNextToken' - The token for the next set of items to return. Use this token to get the next set of results.
--
-- * 'gosrsResponseStatus' - -- | The response status code.
getOpsSummaryResponse ::
  -- | 'gosrsResponseStatus'
  Int ->
  GetOpsSummaryResponse
getOpsSummaryResponse pResponseStatus_ =
  GetOpsSummaryResponse'
    { _gosrsEntities = Nothing,
      _gosrsNextToken = Nothing,
      _gosrsResponseStatus = pResponseStatus_
    }

-- | The list of aggregated and filtered OpsItems.
gosrsEntities :: Lens' GetOpsSummaryResponse [OpsEntity]
gosrsEntities = lens _gosrsEntities (\s a -> s {_gosrsEntities = a}) . _Default . _Coerce

-- | The token for the next set of items to return. Use this token to get the next set of results.
gosrsNextToken :: Lens' GetOpsSummaryResponse (Maybe Text)
gosrsNextToken = lens _gosrsNextToken (\s a -> s {_gosrsNextToken = a})

-- | -- | The response status code.
gosrsResponseStatus :: Lens' GetOpsSummaryResponse Int
gosrsResponseStatus = lens _gosrsResponseStatus (\s a -> s {_gosrsResponseStatus = a})

instance NFData GetOpsSummaryResponse
