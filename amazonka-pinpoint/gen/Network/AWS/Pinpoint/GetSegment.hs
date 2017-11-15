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
-- Module      : Network.AWS.Pinpoint.GetSegment
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a segment.
module Network.AWS.Pinpoint.GetSegment
    (
    -- * Creating a Request
      getSegment
    , GetSegment
    -- * Request Lenses
    , gsSegmentId
    , gsApplicationId

    -- * Destructuring the Response
    , getSegmentResponse
    , GetSegmentResponse
    -- * Response Lenses
    , getrsResponseStatus
    , getrsSegmentResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getSegment' smart constructor.
data GetSegment = GetSegment'
  { _gsSegmentId     :: !Text
  , _gsApplicationId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSegment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsSegmentId' - Undocumented member.
--
-- * 'gsApplicationId' - Undocumented member.
getSegment
    :: Text -- ^ 'gsSegmentId'
    -> Text -- ^ 'gsApplicationId'
    -> GetSegment
getSegment pSegmentId_ pApplicationId_ =
  GetSegment' {_gsSegmentId = pSegmentId_, _gsApplicationId = pApplicationId_}


-- | Undocumented member.
gsSegmentId :: Lens' GetSegment Text
gsSegmentId = lens _gsSegmentId (\ s a -> s{_gsSegmentId = a});

-- | Undocumented member.
gsApplicationId :: Lens' GetSegment Text
gsApplicationId = lens _gsApplicationId (\ s a -> s{_gsApplicationId = a});

instance AWSRequest GetSegment where
        type Rs GetSegment = GetSegmentResponse
        request = get pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 GetSegmentResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable GetSegment where

instance NFData GetSegment where

instance ToHeaders GetSegment where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetSegment where
        toPath GetSegment'{..}
          = mconcat
              ["/v1/apps/", toBS _gsApplicationId, "/segments/",
               toBS _gsSegmentId]

instance ToQuery GetSegment where
        toQuery = const mempty

-- | /See:/ 'getSegmentResponse' smart constructor.
data GetSegmentResponse = GetSegmentResponse'
  { _getrsResponseStatus  :: !Int
  , _getrsSegmentResponse :: !SegmentResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSegmentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'getrsResponseStatus' - -- | The response status code.
--
-- * 'getrsSegmentResponse' - Undocumented member.
getSegmentResponse
    :: Int -- ^ 'getrsResponseStatus'
    -> SegmentResponse -- ^ 'getrsSegmentResponse'
    -> GetSegmentResponse
getSegmentResponse pResponseStatus_ pSegmentResponse_ =
  GetSegmentResponse'
  { _getrsResponseStatus = pResponseStatus_
  , _getrsSegmentResponse = pSegmentResponse_
  }


-- | -- | The response status code.
getrsResponseStatus :: Lens' GetSegmentResponse Int
getrsResponseStatus = lens _getrsResponseStatus (\ s a -> s{_getrsResponseStatus = a});

-- | Undocumented member.
getrsSegmentResponse :: Lens' GetSegmentResponse SegmentResponse
getrsSegmentResponse = lens _getrsSegmentResponse (\ s a -> s{_getrsSegmentResponse = a});

instance NFData GetSegmentResponse where
