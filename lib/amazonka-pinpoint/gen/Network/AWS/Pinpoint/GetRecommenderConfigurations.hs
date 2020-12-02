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
-- Module      : Network.AWS.Pinpoint.GetRecommenderConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about all the recommender model configurations that are associated with your Amazon Pinpoint account.
module Network.AWS.Pinpoint.GetRecommenderConfigurations
  ( -- * Creating a Request
    getRecommenderConfigurations,
    GetRecommenderConfigurations,

    -- * Request Lenses
    grcToken,
    grcPageSize,

    -- * Destructuring the Response
    getRecommenderConfigurationsResponse,
    GetRecommenderConfigurationsResponse,

    -- * Response Lenses
    grcrsResponseStatus,
    grcrsListRecommenderConfigurationsResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getRecommenderConfigurations' smart constructor.
data GetRecommenderConfigurations = GetRecommenderConfigurations'
  { _grcToken ::
      !(Maybe Text),
    _grcPageSize :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetRecommenderConfigurations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grcToken' - The NextToken string that specifies which page of results to return in a paginated response.
--
-- * 'grcPageSize' - The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
getRecommenderConfigurations ::
  GetRecommenderConfigurations
getRecommenderConfigurations =
  GetRecommenderConfigurations'
    { _grcToken = Nothing,
      _grcPageSize = Nothing
    }

-- | The NextToken string that specifies which page of results to return in a paginated response.
grcToken :: Lens' GetRecommenderConfigurations (Maybe Text)
grcToken = lens _grcToken (\s a -> s {_grcToken = a})

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
grcPageSize :: Lens' GetRecommenderConfigurations (Maybe Text)
grcPageSize = lens _grcPageSize (\s a -> s {_grcPageSize = a})

instance AWSRequest GetRecommenderConfigurations where
  type
    Rs GetRecommenderConfigurations =
      GetRecommenderConfigurationsResponse
  request = get pinpoint
  response =
    receiveJSON
      ( \s h x ->
          GetRecommenderConfigurationsResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable GetRecommenderConfigurations

instance NFData GetRecommenderConfigurations

instance ToHeaders GetRecommenderConfigurations where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath GetRecommenderConfigurations where
  toPath = const "/v1/recommenders"

instance ToQuery GetRecommenderConfigurations where
  toQuery GetRecommenderConfigurations' {..} =
    mconcat ["token" =: _grcToken, "page-size" =: _grcPageSize]

-- | /See:/ 'getRecommenderConfigurationsResponse' smart constructor.
data GetRecommenderConfigurationsResponse = GetRecommenderConfigurationsResponse'
  { _grcrsResponseStatus ::
      !Int,
    _grcrsListRecommenderConfigurationsResponse ::
      !ListRecommenderConfigurationsResponse
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetRecommenderConfigurationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grcrsResponseStatus' - -- | The response status code.
--
-- * 'grcrsListRecommenderConfigurationsResponse' - Undocumented member.
getRecommenderConfigurationsResponse ::
  -- | 'grcrsResponseStatus'
  Int ->
  -- | 'grcrsListRecommenderConfigurationsResponse'
  ListRecommenderConfigurationsResponse ->
  GetRecommenderConfigurationsResponse
getRecommenderConfigurationsResponse
  pResponseStatus_
  pListRecommenderConfigurationsResponse_ =
    GetRecommenderConfigurationsResponse'
      { _grcrsResponseStatus =
          pResponseStatus_,
        _grcrsListRecommenderConfigurationsResponse =
          pListRecommenderConfigurationsResponse_
      }

-- | -- | The response status code.
grcrsResponseStatus :: Lens' GetRecommenderConfigurationsResponse Int
grcrsResponseStatus = lens _grcrsResponseStatus (\s a -> s {_grcrsResponseStatus = a})

-- | Undocumented member.
grcrsListRecommenderConfigurationsResponse :: Lens' GetRecommenderConfigurationsResponse ListRecommenderConfigurationsResponse
grcrsListRecommenderConfigurationsResponse = lens _grcrsListRecommenderConfigurationsResponse (\s a -> s {_grcrsListRecommenderConfigurationsResponse = a})

instance NFData GetRecommenderConfigurationsResponse
