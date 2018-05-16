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
-- Module      : Network.AWS.Pinpoint.GetSegmentVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a segment version.
module Network.AWS.Pinpoint.GetSegmentVersion
    (
    -- * Creating a Request
      getSegmentVersion
    , GetSegmentVersion
    -- * Request Lenses
    , gSegmentId
    , gVersion
    , gApplicationId

    -- * Destructuring the Response
    , getSegmentVersionResponse
    , GetSegmentVersionResponse
    -- * Response Lenses
    , gsvrsResponseStatus
    , gsvrsSegmentResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getSegmentVersion' smart constructor.
data GetSegmentVersion = GetSegmentVersion'
  { _gSegmentId     :: !Text
  , _gVersion       :: !Text
  , _gApplicationId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSegmentVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gSegmentId' - Undocumented member.
--
-- * 'gVersion' - Undocumented member.
--
-- * 'gApplicationId' - Undocumented member.
getSegmentVersion
    :: Text -- ^ 'gSegmentId'
    -> Text -- ^ 'gVersion'
    -> Text -- ^ 'gApplicationId'
    -> GetSegmentVersion
getSegmentVersion pSegmentId_ pVersion_ pApplicationId_ =
  GetSegmentVersion'
    { _gSegmentId = pSegmentId_
    , _gVersion = pVersion_
    , _gApplicationId = pApplicationId_
    }


-- | Undocumented member.
gSegmentId :: Lens' GetSegmentVersion Text
gSegmentId = lens _gSegmentId (\ s a -> s{_gSegmentId = a})

-- | Undocumented member.
gVersion :: Lens' GetSegmentVersion Text
gVersion = lens _gVersion (\ s a -> s{_gVersion = a})

-- | Undocumented member.
gApplicationId :: Lens' GetSegmentVersion Text
gApplicationId = lens _gApplicationId (\ s a -> s{_gApplicationId = a})

instance AWSRequest GetSegmentVersion where
        type Rs GetSegmentVersion = GetSegmentVersionResponse
        request = get pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 GetSegmentVersionResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable GetSegmentVersion where

instance NFData GetSegmentVersion where

instance ToHeaders GetSegmentVersion where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetSegmentVersion where
        toPath GetSegmentVersion'{..}
          = mconcat
              ["/v1/apps/", toBS _gApplicationId, "/segments/",
               toBS _gSegmentId, "/versions/", toBS _gVersion]

instance ToQuery GetSegmentVersion where
        toQuery = const mempty

-- | /See:/ 'getSegmentVersionResponse' smart constructor.
data GetSegmentVersionResponse = GetSegmentVersionResponse'
  { _gsvrsResponseStatus  :: !Int
  , _gsvrsSegmentResponse :: !SegmentResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSegmentVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsvrsResponseStatus' - -- | The response status code.
--
-- * 'gsvrsSegmentResponse' - Undocumented member.
getSegmentVersionResponse
    :: Int -- ^ 'gsvrsResponseStatus'
    -> SegmentResponse -- ^ 'gsvrsSegmentResponse'
    -> GetSegmentVersionResponse
getSegmentVersionResponse pResponseStatus_ pSegmentResponse_ =
  GetSegmentVersionResponse'
    { _gsvrsResponseStatus = pResponseStatus_
    , _gsvrsSegmentResponse = pSegmentResponse_
    }


-- | -- | The response status code.
gsvrsResponseStatus :: Lens' GetSegmentVersionResponse Int
gsvrsResponseStatus = lens _gsvrsResponseStatus (\ s a -> s{_gsvrsResponseStatus = a})

-- | Undocumented member.
gsvrsSegmentResponse :: Lens' GetSegmentVersionResponse SegmentResponse
gsvrsSegmentResponse = lens _gsvrsSegmentResponse (\ s a -> s{_gsvrsSegmentResponse = a})

instance NFData GetSegmentVersionResponse where
