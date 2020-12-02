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
-- Module      : Network.AWS.StorageGateway.ListTapePools
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists custom tape pools. You specify custom tape pools to list by specifying one or more custom tape pool Amazon Resource Names (ARNs). If you don't specify a custom tape pool ARN, the operation lists all custom tape pools.
--
--
-- This operation supports pagination. You can optionally specify the @Limit@ parameter in the body to limit the number of tape pools in the response. If the number of tape pools returned in the response is truncated, the response includes a @Marker@ element that you can use in your subsequent request to retrieve the next set of tape pools.
--
--
-- This operation returns paginated results.
module Network.AWS.StorageGateway.ListTapePools
  ( -- * Creating a Request
    listTapePools,
    ListTapePools,

    -- * Request Lenses
    ltpPoolARNs,
    ltpMarker,
    ltpLimit,

    -- * Destructuring the Response
    listTapePoolsResponse,
    ListTapePoolsResponse,

    -- * Response Lenses
    ltprsPoolInfos,
    ltprsMarker,
    ltprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'listTapePools' smart constructor.
data ListTapePools = ListTapePools'
  { _ltpPoolARNs ::
      !(Maybe [Text]),
    _ltpMarker :: !(Maybe Text),
    _ltpLimit :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTapePools' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltpPoolARNs' - The Amazon Resource Name (ARN) of each of the custom tape pools you want to list. If you don't specify a custom tape pool ARN, the response lists all custom tape pools.
--
-- * 'ltpMarker' - A string that indicates the position at which to begin the returned list of tape pools.
--
-- * 'ltpLimit' - An optional number limit for the tape pools in the list returned by this call.
listTapePools ::
  ListTapePools
listTapePools =
  ListTapePools'
    { _ltpPoolARNs = Nothing,
      _ltpMarker = Nothing,
      _ltpLimit = Nothing
    }

-- | The Amazon Resource Name (ARN) of each of the custom tape pools you want to list. If you don't specify a custom tape pool ARN, the response lists all custom tape pools.
ltpPoolARNs :: Lens' ListTapePools [Text]
ltpPoolARNs = lens _ltpPoolARNs (\s a -> s {_ltpPoolARNs = a}) . _Default . _Coerce

-- | A string that indicates the position at which to begin the returned list of tape pools.
ltpMarker :: Lens' ListTapePools (Maybe Text)
ltpMarker = lens _ltpMarker (\s a -> s {_ltpMarker = a})

-- | An optional number limit for the tape pools in the list returned by this call.
ltpLimit :: Lens' ListTapePools (Maybe Natural)
ltpLimit = lens _ltpLimit (\s a -> s {_ltpLimit = a}) . mapping _Nat

instance AWSPager ListTapePools where
  page rq rs
    | stop (rs ^. ltprsMarker) = Nothing
    | stop (rs ^. ltprsPoolInfos) = Nothing
    | otherwise = Just $ rq & ltpMarker .~ rs ^. ltprsMarker

instance AWSRequest ListTapePools where
  type Rs ListTapePools = ListTapePoolsResponse
  request = postJSON storageGateway
  response =
    receiveJSON
      ( \s h x ->
          ListTapePoolsResponse'
            <$> (x .?> "PoolInfos" .!@ mempty)
            <*> (x .?> "Marker")
            <*> (pure (fromEnum s))
      )

instance Hashable ListTapePools

instance NFData ListTapePools

instance ToHeaders ListTapePools where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("StorageGateway_20130630.ListTapePools" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListTapePools where
  toJSON ListTapePools' {..} =
    object
      ( catMaybes
          [ ("PoolARNs" .=) <$> _ltpPoolARNs,
            ("Marker" .=) <$> _ltpMarker,
            ("Limit" .=) <$> _ltpLimit
          ]
      )

instance ToPath ListTapePools where
  toPath = const "/"

instance ToQuery ListTapePools where
  toQuery = const mempty

-- | /See:/ 'listTapePoolsResponse' smart constructor.
data ListTapePoolsResponse = ListTapePoolsResponse'
  { _ltprsPoolInfos ::
      !(Maybe [PoolInfo]),
    _ltprsMarker :: !(Maybe Text),
    _ltprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTapePoolsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltprsPoolInfos' - An array of @PoolInfo@ objects, where each object describes a single custom tape pool. If there are no custom tape pools, the @PoolInfos@ is an empty array.
--
-- * 'ltprsMarker' - A string that indicates the position at which to begin the returned list of tape pools. Use the marker in your next request to continue pagination of tape pools. If there are no more tape pools to list, this element does not appear in the response body.
--
-- * 'ltprsResponseStatus' - -- | The response status code.
listTapePoolsResponse ::
  -- | 'ltprsResponseStatus'
  Int ->
  ListTapePoolsResponse
listTapePoolsResponse pResponseStatus_ =
  ListTapePoolsResponse'
    { _ltprsPoolInfos = Nothing,
      _ltprsMarker = Nothing,
      _ltprsResponseStatus = pResponseStatus_
    }

-- | An array of @PoolInfo@ objects, where each object describes a single custom tape pool. If there are no custom tape pools, the @PoolInfos@ is an empty array.
ltprsPoolInfos :: Lens' ListTapePoolsResponse [PoolInfo]
ltprsPoolInfos = lens _ltprsPoolInfos (\s a -> s {_ltprsPoolInfos = a}) . _Default . _Coerce

-- | A string that indicates the position at which to begin the returned list of tape pools. Use the marker in your next request to continue pagination of tape pools. If there are no more tape pools to list, this element does not appear in the response body.
ltprsMarker :: Lens' ListTapePoolsResponse (Maybe Text)
ltprsMarker = lens _ltprsMarker (\s a -> s {_ltprsMarker = a})

-- | -- | The response status code.
ltprsResponseStatus :: Lens' ListTapePoolsResponse Int
ltprsResponseStatus = lens _ltprsResponseStatus (\s a -> s {_ltprsResponseStatus = a})

instance NFData ListTapePoolsResponse
