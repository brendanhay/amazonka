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
-- Module      : Network.AWS.Pinpoint.GetSegmentVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the configuration, dimension, and other settings for all the versions of a specific segment that's associated with an application.
module Network.AWS.Pinpoint.GetSegmentVersions
  ( -- * Creating a Request
    getSegmentVersions,
    GetSegmentVersions,

    -- * Request Lenses
    gsvToken,
    gsvPageSize,
    gsvSegmentId,
    gsvApplicationId,

    -- * Destructuring the Response
    getSegmentVersionsResponse,
    GetSegmentVersionsResponse,

    -- * Response Lenses
    grsResponseStatus,
    grsSegmentsResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getSegmentVersions' smart constructor.
data GetSegmentVersions = GetSegmentVersions'
  { _gsvToken ::
      !(Maybe Text),
    _gsvPageSize :: !(Maybe Text),
    _gsvSegmentId :: !Text,
    _gsvApplicationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetSegmentVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsvToken' - The NextToken string that specifies which page of results to return in a paginated response.
--
-- * 'gsvPageSize' - The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- * 'gsvSegmentId' - The unique identifier for the segment.
--
-- * 'gsvApplicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
getSegmentVersions ::
  -- | 'gsvSegmentId'
  Text ->
  -- | 'gsvApplicationId'
  Text ->
  GetSegmentVersions
getSegmentVersions pSegmentId_ pApplicationId_ =
  GetSegmentVersions'
    { _gsvToken = Nothing,
      _gsvPageSize = Nothing,
      _gsvSegmentId = pSegmentId_,
      _gsvApplicationId = pApplicationId_
    }

-- | The NextToken string that specifies which page of results to return in a paginated response.
gsvToken :: Lens' GetSegmentVersions (Maybe Text)
gsvToken = lens _gsvToken (\s a -> s {_gsvToken = a})

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
gsvPageSize :: Lens' GetSegmentVersions (Maybe Text)
gsvPageSize = lens _gsvPageSize (\s a -> s {_gsvPageSize = a})

-- | The unique identifier for the segment.
gsvSegmentId :: Lens' GetSegmentVersions Text
gsvSegmentId = lens _gsvSegmentId (\s a -> s {_gsvSegmentId = a})

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
gsvApplicationId :: Lens' GetSegmentVersions Text
gsvApplicationId = lens _gsvApplicationId (\s a -> s {_gsvApplicationId = a})

instance AWSRequest GetSegmentVersions where
  type Rs GetSegmentVersions = GetSegmentVersionsResponse
  request = get pinpoint
  response =
    receiveJSON
      ( \s h x ->
          GetSegmentVersionsResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable GetSegmentVersions

instance NFData GetSegmentVersions

instance ToHeaders GetSegmentVersions where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath GetSegmentVersions where
  toPath GetSegmentVersions' {..} =
    mconcat
      [ "/v1/apps/",
        toBS _gsvApplicationId,
        "/segments/",
        toBS _gsvSegmentId,
        "/versions"
      ]

instance ToQuery GetSegmentVersions where
  toQuery GetSegmentVersions' {..} =
    mconcat ["token" =: _gsvToken, "page-size" =: _gsvPageSize]

-- | /See:/ 'getSegmentVersionsResponse' smart constructor.
data GetSegmentVersionsResponse = GetSegmentVersionsResponse'
  { _grsResponseStatus ::
      !Int,
    _grsSegmentsResponse ::
      !SegmentsResponse
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetSegmentVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grsResponseStatus' - -- | The response status code.
--
-- * 'grsSegmentsResponse' - Undocumented member.
getSegmentVersionsResponse ::
  -- | 'grsResponseStatus'
  Int ->
  -- | 'grsSegmentsResponse'
  SegmentsResponse ->
  GetSegmentVersionsResponse
getSegmentVersionsResponse pResponseStatus_ pSegmentsResponse_ =
  GetSegmentVersionsResponse'
    { _grsResponseStatus =
        pResponseStatus_,
      _grsSegmentsResponse = pSegmentsResponse_
    }

-- | -- | The response status code.
grsResponseStatus :: Lens' GetSegmentVersionsResponse Int
grsResponseStatus = lens _grsResponseStatus (\s a -> s {_grsResponseStatus = a})

-- | Undocumented member.
grsSegmentsResponse :: Lens' GetSegmentVersionsResponse SegmentsResponse
grsSegmentsResponse = lens _grsSegmentsResponse (\s a -> s {_grsSegmentsResponse = a})

instance NFData GetSegmentVersionsResponse
