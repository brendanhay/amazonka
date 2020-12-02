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
-- Module      : Network.AWS.Lambda.ListFunctionEventInvokeConfigs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of configurations for asynchronous invocation for a function.
--
--
-- To configure options for asynchronous invocation, use 'PutFunctionEventInvokeConfig' .
--
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListFunctionEventInvokeConfigs
  ( -- * Creating a Request
    listFunctionEventInvokeConfigs,
    ListFunctionEventInvokeConfigs,

    -- * Request Lenses
    lfeicMarker,
    lfeicMaxItems,
    lfeicFunctionName,

    -- * Destructuring the Response
    listFunctionEventInvokeConfigsResponse,
    ListFunctionEventInvokeConfigsResponse,

    -- * Response Lenses
    lfeicrsFunctionEventInvokeConfigs,
    lfeicrsNextMarker,
    lfeicrsResponseStatus,
  )
where

import Network.AWS.Lambda.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listFunctionEventInvokeConfigs' smart constructor.
data ListFunctionEventInvokeConfigs = ListFunctionEventInvokeConfigs'
  { _lfeicMarker ::
      !(Maybe Text),
    _lfeicMaxItems ::
      !(Maybe Nat),
    _lfeicFunctionName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListFunctionEventInvokeConfigs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfeicMarker' - Specify the pagination token that's returned by a previous request to retrieve the next page of results.
--
-- * 'lfeicMaxItems' - The maximum number of configurations to return.
--
-- * 'lfeicFunctionName' - The name of the Lambda function. __Name formats__      * __Function name__ - @my-function@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
listFunctionEventInvokeConfigs ::
  -- | 'lfeicFunctionName'
  Text ->
  ListFunctionEventInvokeConfigs
listFunctionEventInvokeConfigs pFunctionName_ =
  ListFunctionEventInvokeConfigs'
    { _lfeicMarker = Nothing,
      _lfeicMaxItems = Nothing,
      _lfeicFunctionName = pFunctionName_
    }

-- | Specify the pagination token that's returned by a previous request to retrieve the next page of results.
lfeicMarker :: Lens' ListFunctionEventInvokeConfigs (Maybe Text)
lfeicMarker = lens _lfeicMarker (\s a -> s {_lfeicMarker = a})

-- | The maximum number of configurations to return.
lfeicMaxItems :: Lens' ListFunctionEventInvokeConfigs (Maybe Natural)
lfeicMaxItems = lens _lfeicMaxItems (\s a -> s {_lfeicMaxItems = a}) . mapping _Nat

-- | The name of the Lambda function. __Name formats__      * __Function name__ - @my-function@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
lfeicFunctionName :: Lens' ListFunctionEventInvokeConfigs Text
lfeicFunctionName = lens _lfeicFunctionName (\s a -> s {_lfeicFunctionName = a})

instance AWSPager ListFunctionEventInvokeConfigs where
  page rq rs
    | stop (rs ^. lfeicrsNextMarker) = Nothing
    | stop (rs ^. lfeicrsFunctionEventInvokeConfigs) = Nothing
    | otherwise = Just $ rq & lfeicMarker .~ rs ^. lfeicrsNextMarker

instance AWSRequest ListFunctionEventInvokeConfigs where
  type
    Rs ListFunctionEventInvokeConfigs =
      ListFunctionEventInvokeConfigsResponse
  request = get lambda
  response =
    receiveJSON
      ( \s h x ->
          ListFunctionEventInvokeConfigsResponse'
            <$> (x .?> "FunctionEventInvokeConfigs" .!@ mempty)
            <*> (x .?> "NextMarker")
            <*> (pure (fromEnum s))
      )

instance Hashable ListFunctionEventInvokeConfigs

instance NFData ListFunctionEventInvokeConfigs

instance ToHeaders ListFunctionEventInvokeConfigs where
  toHeaders = const mempty

instance ToPath ListFunctionEventInvokeConfigs where
  toPath ListFunctionEventInvokeConfigs' {..} =
    mconcat
      [ "/2019-09-25/functions/",
        toBS _lfeicFunctionName,
        "/event-invoke-config/list"
      ]

instance ToQuery ListFunctionEventInvokeConfigs where
  toQuery ListFunctionEventInvokeConfigs' {..} =
    mconcat ["Marker" =: _lfeicMarker, "MaxItems" =: _lfeicMaxItems]

-- | /See:/ 'listFunctionEventInvokeConfigsResponse' smart constructor.
data ListFunctionEventInvokeConfigsResponse = ListFunctionEventInvokeConfigsResponse'
  { _lfeicrsFunctionEventInvokeConfigs ::
      !( Maybe
           [FunctionEventInvokeConfig]
       ),
    _lfeicrsNextMarker ::
      !(Maybe Text),
    _lfeicrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListFunctionEventInvokeConfigsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfeicrsFunctionEventInvokeConfigs' - A list of configurations.
--
-- * 'lfeicrsNextMarker' - The pagination token that's included if more results are available.
--
-- * 'lfeicrsResponseStatus' - -- | The response status code.
listFunctionEventInvokeConfigsResponse ::
  -- | 'lfeicrsResponseStatus'
  Int ->
  ListFunctionEventInvokeConfigsResponse
listFunctionEventInvokeConfigsResponse pResponseStatus_ =
  ListFunctionEventInvokeConfigsResponse'
    { _lfeicrsFunctionEventInvokeConfigs =
        Nothing,
      _lfeicrsNextMarker = Nothing,
      _lfeicrsResponseStatus = pResponseStatus_
    }

-- | A list of configurations.
lfeicrsFunctionEventInvokeConfigs :: Lens' ListFunctionEventInvokeConfigsResponse [FunctionEventInvokeConfig]
lfeicrsFunctionEventInvokeConfigs = lens _lfeicrsFunctionEventInvokeConfigs (\s a -> s {_lfeicrsFunctionEventInvokeConfigs = a}) . _Default . _Coerce

-- | The pagination token that's included if more results are available.
lfeicrsNextMarker :: Lens' ListFunctionEventInvokeConfigsResponse (Maybe Text)
lfeicrsNextMarker = lens _lfeicrsNextMarker (\s a -> s {_lfeicrsNextMarker = a})

-- | -- | The response status code.
lfeicrsResponseStatus :: Lens' ListFunctionEventInvokeConfigsResponse Int
lfeicrsResponseStatus = lens _lfeicrsResponseStatus (\s a -> s {_lfeicrsResponseStatus = a})

instance NFData ListFunctionEventInvokeConfigsResponse
