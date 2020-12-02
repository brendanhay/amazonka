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
-- Module      : Network.AWS.Lambda.ListProvisionedConcurrencyConfigs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of provisioned concurrency configurations for a function.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListProvisionedConcurrencyConfigs
  ( -- * Creating a Request
    listProvisionedConcurrencyConfigs,
    ListProvisionedConcurrencyConfigs,

    -- * Request Lenses
    lpccMarker,
    lpccMaxItems,
    lpccFunctionName,

    -- * Destructuring the Response
    listProvisionedConcurrencyConfigsResponse,
    ListProvisionedConcurrencyConfigsResponse,

    -- * Response Lenses
    lpccrsProvisionedConcurrencyConfigs,
    lpccrsNextMarker,
    lpccrsResponseStatus,
  )
where

import Network.AWS.Lambda.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listProvisionedConcurrencyConfigs' smart constructor.
data ListProvisionedConcurrencyConfigs = ListProvisionedConcurrencyConfigs'
  { _lpccMarker ::
      !(Maybe Text),
    _lpccMaxItems ::
      !(Maybe Nat),
    _lpccFunctionName ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListProvisionedConcurrencyConfigs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpccMarker' - Specify the pagination token that's returned by a previous request to retrieve the next page of results.
--
-- * 'lpccMaxItems' - Specify a number to limit the number of configurations returned.
--
-- * 'lpccFunctionName' - The name of the Lambda function. __Name formats__      * __Function name__ - @my-function@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
listProvisionedConcurrencyConfigs ::
  -- | 'lpccFunctionName'
  Text ->
  ListProvisionedConcurrencyConfigs
listProvisionedConcurrencyConfigs pFunctionName_ =
  ListProvisionedConcurrencyConfigs'
    { _lpccMarker = Nothing,
      _lpccMaxItems = Nothing,
      _lpccFunctionName = pFunctionName_
    }

-- | Specify the pagination token that's returned by a previous request to retrieve the next page of results.
lpccMarker :: Lens' ListProvisionedConcurrencyConfigs (Maybe Text)
lpccMarker = lens _lpccMarker (\s a -> s {_lpccMarker = a})

-- | Specify a number to limit the number of configurations returned.
lpccMaxItems :: Lens' ListProvisionedConcurrencyConfigs (Maybe Natural)
lpccMaxItems = lens _lpccMaxItems (\s a -> s {_lpccMaxItems = a}) . mapping _Nat

-- | The name of the Lambda function. __Name formats__      * __Function name__ - @my-function@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
lpccFunctionName :: Lens' ListProvisionedConcurrencyConfigs Text
lpccFunctionName = lens _lpccFunctionName (\s a -> s {_lpccFunctionName = a})

instance AWSPager ListProvisionedConcurrencyConfigs where
  page rq rs
    | stop (rs ^. lpccrsNextMarker) = Nothing
    | stop (rs ^. lpccrsProvisionedConcurrencyConfigs) = Nothing
    | otherwise = Just $ rq & lpccMarker .~ rs ^. lpccrsNextMarker

instance AWSRequest ListProvisionedConcurrencyConfigs where
  type
    Rs ListProvisionedConcurrencyConfigs =
      ListProvisionedConcurrencyConfigsResponse
  request = get lambda
  response =
    receiveJSON
      ( \s h x ->
          ListProvisionedConcurrencyConfigsResponse'
            <$> (x .?> "ProvisionedConcurrencyConfigs" .!@ mempty)
            <*> (x .?> "NextMarker")
            <*> (pure (fromEnum s))
      )

instance Hashable ListProvisionedConcurrencyConfigs

instance NFData ListProvisionedConcurrencyConfigs

instance ToHeaders ListProvisionedConcurrencyConfigs where
  toHeaders = const mempty

instance ToPath ListProvisionedConcurrencyConfigs where
  toPath ListProvisionedConcurrencyConfigs' {..} =
    mconcat
      [ "/2019-09-30/functions/",
        toBS _lpccFunctionName,
        "/provisioned-concurrency"
      ]

instance ToQuery ListProvisionedConcurrencyConfigs where
  toQuery ListProvisionedConcurrencyConfigs' {..} =
    mconcat
      ["Marker" =: _lpccMarker, "MaxItems" =: _lpccMaxItems, "List=ALL"]

-- | /See:/ 'listProvisionedConcurrencyConfigsResponse' smart constructor.
data ListProvisionedConcurrencyConfigsResponse = ListProvisionedConcurrencyConfigsResponse'
  { _lpccrsProvisionedConcurrencyConfigs ::
      !( Maybe
           [ProvisionedConcurrencyConfigListItem]
       ),
    _lpccrsNextMarker ::
      !( Maybe
           Text
       ),
    _lpccrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'ListProvisionedConcurrencyConfigsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpccrsProvisionedConcurrencyConfigs' - A list of provisioned concurrency configurations.
--
-- * 'lpccrsNextMarker' - The pagination token that's included if more results are available.
--
-- * 'lpccrsResponseStatus' - -- | The response status code.
listProvisionedConcurrencyConfigsResponse ::
  -- | 'lpccrsResponseStatus'
  Int ->
  ListProvisionedConcurrencyConfigsResponse
listProvisionedConcurrencyConfigsResponse pResponseStatus_ =
  ListProvisionedConcurrencyConfigsResponse'
    { _lpccrsProvisionedConcurrencyConfigs =
        Nothing,
      _lpccrsNextMarker = Nothing,
      _lpccrsResponseStatus = pResponseStatus_
    }

-- | A list of provisioned concurrency configurations.
lpccrsProvisionedConcurrencyConfigs :: Lens' ListProvisionedConcurrencyConfigsResponse [ProvisionedConcurrencyConfigListItem]
lpccrsProvisionedConcurrencyConfigs = lens _lpccrsProvisionedConcurrencyConfigs (\s a -> s {_lpccrsProvisionedConcurrencyConfigs = a}) . _Default . _Coerce

-- | The pagination token that's included if more results are available.
lpccrsNextMarker :: Lens' ListProvisionedConcurrencyConfigsResponse (Maybe Text)
lpccrsNextMarker = lens _lpccrsNextMarker (\s a -> s {_lpccrsNextMarker = a})

-- | -- | The response status code.
lpccrsResponseStatus :: Lens' ListProvisionedConcurrencyConfigsResponse Int
lpccrsResponseStatus = lens _lpccrsResponseStatus (\s a -> s {_lpccrsResponseStatus = a})

instance NFData ListProvisionedConcurrencyConfigsResponse
