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
-- Module      : Network.AWS.Lightsail.GetDistributions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about one or more of your Amazon Lightsail content delivery network (CDN) distributions.
module Network.AWS.Lightsail.GetDistributions
  ( -- * Creating a Request
    getDistributions,
    GetDistributions,

    -- * Request Lenses
    gdDistributionName,
    gdPageToken,

    -- * Destructuring the Response
    getDistributionsResponse,
    GetDistributionsResponse,

    -- * Response Lenses
    gdrsNextPageToken,
    gdrsDistributions,
    gdrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDistributions' smart constructor.
data GetDistributions = GetDistributions'
  { _gdDistributionName ::
      !(Maybe Text),
    _gdPageToken :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetDistributions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdDistributionName' - The name of the distribution for which to return information. Use the @GetDistributions@ action to get a list of distribution names that you can specify. When omitted, the response includes all of your distributions in the AWS Region where the request is made.
--
-- * 'gdPageToken' - The token to advance to the next page of results from your request. To get a page token, perform an initial @GetDistributions@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
getDistributions ::
  GetDistributions
getDistributions =
  GetDistributions'
    { _gdDistributionName = Nothing,
      _gdPageToken = Nothing
    }

-- | The name of the distribution for which to return information. Use the @GetDistributions@ action to get a list of distribution names that you can specify. When omitted, the response includes all of your distributions in the AWS Region where the request is made.
gdDistributionName :: Lens' GetDistributions (Maybe Text)
gdDistributionName = lens _gdDistributionName (\s a -> s {_gdDistributionName = a})

-- | The token to advance to the next page of results from your request. To get a page token, perform an initial @GetDistributions@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
gdPageToken :: Lens' GetDistributions (Maybe Text)
gdPageToken = lens _gdPageToken (\s a -> s {_gdPageToken = a})

instance AWSRequest GetDistributions where
  type Rs GetDistributions = GetDistributionsResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          GetDistributionsResponse'
            <$> (x .?> "nextPageToken")
            <*> (x .?> "distributions" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable GetDistributions

instance NFData GetDistributions

instance ToHeaders GetDistributions where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.GetDistributions" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetDistributions where
  toJSON GetDistributions' {..} =
    object
      ( catMaybes
          [ ("distributionName" .=) <$> _gdDistributionName,
            ("pageToken" .=) <$> _gdPageToken
          ]
      )

instance ToPath GetDistributions where
  toPath = const "/"

instance ToQuery GetDistributions where
  toQuery = const mempty

-- | /See:/ 'getDistributionsResponse' smart constructor.
data GetDistributionsResponse = GetDistributionsResponse'
  { _gdrsNextPageToken ::
      !(Maybe Text),
    _gdrsDistributions ::
      !(Maybe [LightsailDistribution]),
    _gdrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetDistributionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdrsNextPageToken' - The token to advance to the next page of results from your request. A next page token is not returned if there are no more results to display. To get the next page of results, perform another @GetDistributions@ request and specify the next page token using the @pageToken@ parameter.
--
-- * 'gdrsDistributions' - An array of objects that describe your distributions.
--
-- * 'gdrsResponseStatus' - -- | The response status code.
getDistributionsResponse ::
  -- | 'gdrsResponseStatus'
  Int ->
  GetDistributionsResponse
getDistributionsResponse pResponseStatus_ =
  GetDistributionsResponse'
    { _gdrsNextPageToken = Nothing,
      _gdrsDistributions = Nothing,
      _gdrsResponseStatus = pResponseStatus_
    }

-- | The token to advance to the next page of results from your request. A next page token is not returned if there are no more results to display. To get the next page of results, perform another @GetDistributions@ request and specify the next page token using the @pageToken@ parameter.
gdrsNextPageToken :: Lens' GetDistributionsResponse (Maybe Text)
gdrsNextPageToken = lens _gdrsNextPageToken (\s a -> s {_gdrsNextPageToken = a})

-- | An array of objects that describe your distributions.
gdrsDistributions :: Lens' GetDistributionsResponse [LightsailDistribution]
gdrsDistributions = lens _gdrsDistributions (\s a -> s {_gdrsDistributions = a}) . _Default . _Coerce

-- | -- | The response status code.
gdrsResponseStatus :: Lens' GetDistributionsResponse Int
gdrsResponseStatus = lens _gdrsResponseStatus (\s a -> s {_gdrsResponseStatus = a})

instance NFData GetDistributionsResponse
