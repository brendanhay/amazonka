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
-- Module      : Network.AWS.Pinpoint.GetSegments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the configuration, dimension, and other settings for all the segments that are associated with an application.
module Network.AWS.Pinpoint.GetSegments
  ( -- * Creating a Request
    getSegments,
    GetSegments,

    -- * Request Lenses
    gssToken,
    gssPageSize,
    gssApplicationId,

    -- * Destructuring the Response
    getSegmentsResponse,
    GetSegmentsResponse,

    -- * Response Lenses
    gsrsResponseStatus,
    gsrsSegmentsResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getSegments' smart constructor.
data GetSegments = GetSegments'
  { _gssToken :: !(Maybe Text),
    _gssPageSize :: !(Maybe Text),
    _gssApplicationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetSegments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gssToken' - The NextToken string that specifies which page of results to return in a paginated response.
--
-- * 'gssPageSize' - The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- * 'gssApplicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
getSegments ::
  -- | 'gssApplicationId'
  Text ->
  GetSegments
getSegments pApplicationId_ =
  GetSegments'
    { _gssToken = Nothing,
      _gssPageSize = Nothing,
      _gssApplicationId = pApplicationId_
    }

-- | The NextToken string that specifies which page of results to return in a paginated response.
gssToken :: Lens' GetSegments (Maybe Text)
gssToken = lens _gssToken (\s a -> s {_gssToken = a})

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
gssPageSize :: Lens' GetSegments (Maybe Text)
gssPageSize = lens _gssPageSize (\s a -> s {_gssPageSize = a})

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
gssApplicationId :: Lens' GetSegments Text
gssApplicationId = lens _gssApplicationId (\s a -> s {_gssApplicationId = a})

instance AWSRequest GetSegments where
  type Rs GetSegments = GetSegmentsResponse
  request = get pinpoint
  response =
    receiveJSON
      ( \s h x ->
          GetSegmentsResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable GetSegments

instance NFData GetSegments

instance ToHeaders GetSegments where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath GetSegments where
  toPath GetSegments' {..} =
    mconcat ["/v1/apps/", toBS _gssApplicationId, "/segments"]

instance ToQuery GetSegments where
  toQuery GetSegments' {..} =
    mconcat ["token" =: _gssToken, "page-size" =: _gssPageSize]

-- | /See:/ 'getSegmentsResponse' smart constructor.
data GetSegmentsResponse = GetSegmentsResponse'
  { _gsrsResponseStatus ::
      !Int,
    _gsrsSegmentsResponse :: !SegmentsResponse
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetSegmentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsrsResponseStatus' - -- | The response status code.
--
-- * 'gsrsSegmentsResponse' - Undocumented member.
getSegmentsResponse ::
  -- | 'gsrsResponseStatus'
  Int ->
  -- | 'gsrsSegmentsResponse'
  SegmentsResponse ->
  GetSegmentsResponse
getSegmentsResponse pResponseStatus_ pSegmentsResponse_ =
  GetSegmentsResponse'
    { _gsrsResponseStatus = pResponseStatus_,
      _gsrsSegmentsResponse = pSegmentsResponse_
    }

-- | -- | The response status code.
gsrsResponseStatus :: Lens' GetSegmentsResponse Int
gsrsResponseStatus = lens _gsrsResponseStatus (\s a -> s {_gsrsResponseStatus = a})

-- | Undocumented member.
gsrsSegmentsResponse :: Lens' GetSegmentsResponse SegmentsResponse
gsrsSegmentsResponse = lens _gsrsSegmentsResponse (\s a -> s {_gsrsSegmentsResponse = a})

instance NFData GetSegmentsResponse
