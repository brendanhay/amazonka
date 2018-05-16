{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetSegments
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to get information about your segments.
module Network.AWS.Pinpoint.GetSegments
    (
    -- * Creating a Request
      getSegments
    , GetSegments
    -- * Request Lenses
    , gssToken
    , gssPageSize
    , gssApplicationId

    -- * Destructuring the Response
    , getSegmentsResponse
    , GetSegmentsResponse
    -- * Response Lenses
    , gsrsResponseStatus
    , gsrsSegmentsResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getSegments' smart constructor.
data GetSegments = GetSegments'
  { _gssToken         :: !(Maybe Text)
  , _gssPageSize      :: !(Maybe Text)
  , _gssApplicationId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSegments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gssToken' - The NextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'gssPageSize' - The number of entries you want on each page in the response.
--
-- * 'gssApplicationId' - Undocumented member.
getSegments
    :: Text -- ^ 'gssApplicationId'
    -> GetSegments
getSegments pApplicationId_ =
  GetSegments'
    { _gssToken = Nothing
    , _gssPageSize = Nothing
    , _gssApplicationId = pApplicationId_
    }


-- | The NextToken string returned on a previous page that you use to get the next page of results in a paginated response.
gssToken :: Lens' GetSegments (Maybe Text)
gssToken = lens _gssToken (\ s a -> s{_gssToken = a})

-- | The number of entries you want on each page in the response.
gssPageSize :: Lens' GetSegments (Maybe Text)
gssPageSize = lens _gssPageSize (\ s a -> s{_gssPageSize = a})

-- | Undocumented member.
gssApplicationId :: Lens' GetSegments Text
gssApplicationId = lens _gssApplicationId (\ s a -> s{_gssApplicationId = a})

instance AWSRequest GetSegments where
        type Rs GetSegments = GetSegmentsResponse
        request = get pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 GetSegmentsResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable GetSegments where

instance NFData GetSegments where

instance ToHeaders GetSegments where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetSegments where
        toPath GetSegments'{..}
          = mconcat
              ["/v1/apps/", toBS _gssApplicationId, "/segments"]

instance ToQuery GetSegments where
        toQuery GetSegments'{..}
          = mconcat
              ["token" =: _gssToken, "page-size" =: _gssPageSize]

-- | /See:/ 'getSegmentsResponse' smart constructor.
data GetSegmentsResponse = GetSegmentsResponse'
  { _gsrsResponseStatus   :: !Int
  , _gsrsSegmentsResponse :: !SegmentsResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSegmentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsrsResponseStatus' - -- | The response status code.
--
-- * 'gsrsSegmentsResponse' - Undocumented member.
getSegmentsResponse
    :: Int -- ^ 'gsrsResponseStatus'
    -> SegmentsResponse -- ^ 'gsrsSegmentsResponse'
    -> GetSegmentsResponse
getSegmentsResponse pResponseStatus_ pSegmentsResponse_ =
  GetSegmentsResponse'
    { _gsrsResponseStatus = pResponseStatus_
    , _gsrsSegmentsResponse = pSegmentsResponse_
    }


-- | -- | The response status code.
gsrsResponseStatus :: Lens' GetSegmentsResponse Int
gsrsResponseStatus = lens _gsrsResponseStatus (\ s a -> s{_gsrsResponseStatus = a})

-- | Undocumented member.
gsrsSegmentsResponse :: Lens' GetSegmentsResponse SegmentsResponse
gsrsSegmentsResponse = lens _gsrsSegmentsResponse (\ s a -> s{_gsrsSegmentsResponse = a})

instance NFData GetSegmentsResponse where
