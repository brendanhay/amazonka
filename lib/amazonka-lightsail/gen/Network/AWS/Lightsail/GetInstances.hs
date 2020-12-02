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
-- Module      : Network.AWS.Lightsail.GetInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all Amazon Lightsail virtual private servers, or /instances/ .
--
--
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetInstances
    (
    -- * Creating a Request
      getInstances
    , GetInstances
    -- * Request Lenses
    , giPageToken

    -- * Destructuring the Response
    , getInstancesResponse
    , GetInstancesResponse
    -- * Response Lenses
    , grsNextPageToken
    , grsInstances
    , grsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getInstances' smart constructor.
newtype GetInstances = GetInstances'
  { _giPageToken :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'giPageToken' - A token used for advancing to the next page of results from your get instances request.
getInstances
    :: GetInstances
getInstances = GetInstances' {_giPageToken = Nothing}


-- | A token used for advancing to the next page of results from your get instances request.
giPageToken :: Lens' GetInstances (Maybe Text)
giPageToken = lens _giPageToken (\ s a -> s{_giPageToken = a})

instance AWSPager GetInstances where
        page rq rs
          | stop (rs ^. grsNextPageToken) = Nothing
          | stop (rs ^. grsInstances) = Nothing
          | otherwise =
            Just $ rq & giPageToken .~ rs ^. grsNextPageToken

instance AWSRequest GetInstances where
        type Rs GetInstances = GetInstancesResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetInstancesResponse' <$>
                   (x .?> "nextPageToken") <*>
                     (x .?> "instances" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetInstances where

instance NFData GetInstances where

instance ToHeaders GetInstances where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetInstances" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetInstances where
        toJSON GetInstances'{..}
          = object
              (catMaybes [("pageToken" .=) <$> _giPageToken])

instance ToPath GetInstances where
        toPath = const "/"

instance ToQuery GetInstances where
        toQuery = const mempty

-- | /See:/ 'getInstancesResponse' smart constructor.
data GetInstancesResponse = GetInstancesResponse'
  { _grsNextPageToken  :: !(Maybe Text)
  , _grsInstances      :: !(Maybe [Instance])
  , _grsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grsNextPageToken' - A token used for advancing to the next page of results from your get instances request.
--
-- * 'grsInstances' - An array of key-value pairs containing information about your instances.
--
-- * 'grsResponseStatus' - -- | The response status code.
getInstancesResponse
    :: Int -- ^ 'grsResponseStatus'
    -> GetInstancesResponse
getInstancesResponse pResponseStatus_ =
  GetInstancesResponse'
    { _grsNextPageToken = Nothing
    , _grsInstances = Nothing
    , _grsResponseStatus = pResponseStatus_
    }


-- | A token used for advancing to the next page of results from your get instances request.
grsNextPageToken :: Lens' GetInstancesResponse (Maybe Text)
grsNextPageToken = lens _grsNextPageToken (\ s a -> s{_grsNextPageToken = a})

-- | An array of key-value pairs containing information about your instances.
grsInstances :: Lens' GetInstancesResponse [Instance]
grsInstances = lens _grsInstances (\ s a -> s{_grsInstances = a}) . _Default . _Coerce

-- | -- | The response status code.
grsResponseStatus :: Lens' GetInstancesResponse Int
grsResponseStatus = lens _grsResponseStatus (\ s a -> s{_grsResponseStatus = a})

instance NFData GetInstancesResponse where
