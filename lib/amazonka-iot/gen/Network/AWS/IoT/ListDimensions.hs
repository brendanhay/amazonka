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
-- Module      : Network.AWS.IoT.ListDimensions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the set of dimensions that are defined for your AWS account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListDimensions
  ( -- * Creating a Request
    listDimensions,
    ListDimensions,

    -- * Request Lenses
    ldNextToken,
    ldMaxResults,

    -- * Destructuring the Response
    listDimensionsResponse,
    ListDimensionsResponse,

    -- * Response Lenses
    ldrsNextToken,
    ldrsDimensionNames,
    ldrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listDimensions' smart constructor.
data ListDimensions = ListDimensions'
  { _ldNextToken ::
      !(Maybe Text),
    _ldMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListDimensions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldNextToken' - The token for the next set of results.
--
-- * 'ldMaxResults' - The maximum number of results to retrieve at one time.
listDimensions ::
  ListDimensions
listDimensions =
  ListDimensions' {_ldNextToken = Nothing, _ldMaxResults = Nothing}

-- | The token for the next set of results.
ldNextToken :: Lens' ListDimensions (Maybe Text)
ldNextToken = lens _ldNextToken (\s a -> s {_ldNextToken = a})

-- | The maximum number of results to retrieve at one time.
ldMaxResults :: Lens' ListDimensions (Maybe Natural)
ldMaxResults = lens _ldMaxResults (\s a -> s {_ldMaxResults = a}) . mapping _Nat

instance AWSPager ListDimensions where
  page rq rs
    | stop (rs ^. ldrsNextToken) = Nothing
    | stop (rs ^. ldrsDimensionNames) = Nothing
    | otherwise = Just $ rq & ldNextToken .~ rs ^. ldrsNextToken

instance AWSRequest ListDimensions where
  type Rs ListDimensions = ListDimensionsResponse
  request = get ioT
  response =
    receiveJSON
      ( \s h x ->
          ListDimensionsResponse'
            <$> (x .?> "nextToken")
            <*> (x .?> "dimensionNames" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListDimensions

instance NFData ListDimensions

instance ToHeaders ListDimensions where
  toHeaders = const mempty

instance ToPath ListDimensions where
  toPath = const "/dimensions"

instance ToQuery ListDimensions where
  toQuery ListDimensions' {..} =
    mconcat
      ["nextToken" =: _ldNextToken, "maxResults" =: _ldMaxResults]

-- | /See:/ 'listDimensionsResponse' smart constructor.
data ListDimensionsResponse = ListDimensionsResponse'
  { _ldrsNextToken ::
      !(Maybe Text),
    _ldrsDimensionNames :: !(Maybe [Text]),
    _ldrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListDimensionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldrsNextToken' - A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
--
-- * 'ldrsDimensionNames' - A list of the names of the defined dimensions. Use @DescribeDimension@ to get details for a dimension.
--
-- * 'ldrsResponseStatus' - -- | The response status code.
listDimensionsResponse ::
  -- | 'ldrsResponseStatus'
  Int ->
  ListDimensionsResponse
listDimensionsResponse pResponseStatus_ =
  ListDimensionsResponse'
    { _ldrsNextToken = Nothing,
      _ldrsDimensionNames = Nothing,
      _ldrsResponseStatus = pResponseStatus_
    }

-- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
ldrsNextToken :: Lens' ListDimensionsResponse (Maybe Text)
ldrsNextToken = lens _ldrsNextToken (\s a -> s {_ldrsNextToken = a})

-- | A list of the names of the defined dimensions. Use @DescribeDimension@ to get details for a dimension.
ldrsDimensionNames :: Lens' ListDimensionsResponse [Text]
ldrsDimensionNames = lens _ldrsDimensionNames (\s a -> s {_ldrsDimensionNames = a}) . _Default . _Coerce

-- | -- | The response status code.
ldrsResponseStatus :: Lens' ListDimensionsResponse Int
ldrsResponseStatus = lens _ldrsResponseStatus (\s a -> s {_ldrsResponseStatus = a})

instance NFData ListDimensionsResponse
