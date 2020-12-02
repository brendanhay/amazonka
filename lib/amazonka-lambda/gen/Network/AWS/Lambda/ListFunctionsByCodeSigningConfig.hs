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
-- Module      : Network.AWS.Lambda.ListFunctionsByCodeSigningConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the functions that use the specified code signing configuration. You can use this method prior to deleting a code signing configuration, to verify that no functions are using it.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListFunctionsByCodeSigningConfig
  ( -- * Creating a Request
    listFunctionsByCodeSigningConfig,
    ListFunctionsByCodeSigningConfig,

    -- * Request Lenses
    lfbcscMarker,
    lfbcscMaxItems,
    lfbcscCodeSigningConfigARN,

    -- * Destructuring the Response
    listFunctionsByCodeSigningConfigResponse,
    ListFunctionsByCodeSigningConfigResponse,

    -- * Response Lenses
    lfbcscrsFunctionARNs,
    lfbcscrsNextMarker,
    lfbcscrsResponseStatus,
  )
where

import Network.AWS.Lambda.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listFunctionsByCodeSigningConfig' smart constructor.
data ListFunctionsByCodeSigningConfig = ListFunctionsByCodeSigningConfig'
  { _lfbcscMarker ::
      !(Maybe Text),
    _lfbcscMaxItems ::
      !(Maybe Nat),
    _lfbcscCodeSigningConfigARN ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListFunctionsByCodeSigningConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfbcscMarker' - Specify the pagination token that's returned by a previous request to retrieve the next page of results.
--
-- * 'lfbcscMaxItems' - Maximum number of items to return.
--
-- * 'lfbcscCodeSigningConfigARN' - The The Amazon Resource Name (ARN) of the code signing configuration.
listFunctionsByCodeSigningConfig ::
  -- | 'lfbcscCodeSigningConfigARN'
  Text ->
  ListFunctionsByCodeSigningConfig
listFunctionsByCodeSigningConfig pCodeSigningConfigARN_ =
  ListFunctionsByCodeSigningConfig'
    { _lfbcscMarker = Nothing,
      _lfbcscMaxItems = Nothing,
      _lfbcscCodeSigningConfigARN = pCodeSigningConfigARN_
    }

-- | Specify the pagination token that's returned by a previous request to retrieve the next page of results.
lfbcscMarker :: Lens' ListFunctionsByCodeSigningConfig (Maybe Text)
lfbcscMarker = lens _lfbcscMarker (\s a -> s {_lfbcscMarker = a})

-- | Maximum number of items to return.
lfbcscMaxItems :: Lens' ListFunctionsByCodeSigningConfig (Maybe Natural)
lfbcscMaxItems = lens _lfbcscMaxItems (\s a -> s {_lfbcscMaxItems = a}) . mapping _Nat

-- | The The Amazon Resource Name (ARN) of the code signing configuration.
lfbcscCodeSigningConfigARN :: Lens' ListFunctionsByCodeSigningConfig Text
lfbcscCodeSigningConfigARN = lens _lfbcscCodeSigningConfigARN (\s a -> s {_lfbcscCodeSigningConfigARN = a})

instance AWSPager ListFunctionsByCodeSigningConfig where
  page rq rs
    | stop (rs ^. lfbcscrsNextMarker) = Nothing
    | stop (rs ^. lfbcscrsFunctionARNs) = Nothing
    | otherwise = Just $ rq & lfbcscMarker .~ rs ^. lfbcscrsNextMarker

instance AWSRequest ListFunctionsByCodeSigningConfig where
  type
    Rs ListFunctionsByCodeSigningConfig =
      ListFunctionsByCodeSigningConfigResponse
  request = get lambda
  response =
    receiveJSON
      ( \s h x ->
          ListFunctionsByCodeSigningConfigResponse'
            <$> (x .?> "FunctionArns" .!@ mempty)
            <*> (x .?> "NextMarker")
            <*> (pure (fromEnum s))
      )

instance Hashable ListFunctionsByCodeSigningConfig

instance NFData ListFunctionsByCodeSigningConfig

instance ToHeaders ListFunctionsByCodeSigningConfig where
  toHeaders = const mempty

instance ToPath ListFunctionsByCodeSigningConfig where
  toPath ListFunctionsByCodeSigningConfig' {..} =
    mconcat
      [ "/2020-04-22/code-signing-configs/",
        toBS _lfbcscCodeSigningConfigARN,
        "/functions"
      ]

instance ToQuery ListFunctionsByCodeSigningConfig where
  toQuery ListFunctionsByCodeSigningConfig' {..} =
    mconcat
      ["Marker" =: _lfbcscMarker, "MaxItems" =: _lfbcscMaxItems]

-- | /See:/ 'listFunctionsByCodeSigningConfigResponse' smart constructor.
data ListFunctionsByCodeSigningConfigResponse = ListFunctionsByCodeSigningConfigResponse'
  { _lfbcscrsFunctionARNs ::
      !( Maybe
           [Text]
       ),
    _lfbcscrsNextMarker ::
      !( Maybe
           Text
       ),
    _lfbcscrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListFunctionsByCodeSigningConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfbcscrsFunctionARNs' - The function ARNs.
--
-- * 'lfbcscrsNextMarker' - The pagination token that's included if more results are available.
--
-- * 'lfbcscrsResponseStatus' - -- | The response status code.
listFunctionsByCodeSigningConfigResponse ::
  -- | 'lfbcscrsResponseStatus'
  Int ->
  ListFunctionsByCodeSigningConfigResponse
listFunctionsByCodeSigningConfigResponse pResponseStatus_ =
  ListFunctionsByCodeSigningConfigResponse'
    { _lfbcscrsFunctionARNs =
        Nothing,
      _lfbcscrsNextMarker = Nothing,
      _lfbcscrsResponseStatus = pResponseStatus_
    }

-- | The function ARNs.
lfbcscrsFunctionARNs :: Lens' ListFunctionsByCodeSigningConfigResponse [Text]
lfbcscrsFunctionARNs = lens _lfbcscrsFunctionARNs (\s a -> s {_lfbcscrsFunctionARNs = a}) . _Default . _Coerce

-- | The pagination token that's included if more results are available.
lfbcscrsNextMarker :: Lens' ListFunctionsByCodeSigningConfigResponse (Maybe Text)
lfbcscrsNextMarker = lens _lfbcscrsNextMarker (\s a -> s {_lfbcscrsNextMarker = a})

-- | -- | The response status code.
lfbcscrsResponseStatus :: Lens' ListFunctionsByCodeSigningConfigResponse Int
lfbcscrsResponseStatus = lens _lfbcscrsResponseStatus (\s a -> s {_lfbcscrsResponseStatus = a})

instance NFData ListFunctionsByCodeSigningConfigResponse
