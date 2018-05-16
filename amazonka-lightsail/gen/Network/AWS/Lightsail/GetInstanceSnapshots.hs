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
-- Module      : Network.AWS.Lightsail.GetInstanceSnapshots
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all instance snapshots for the user's account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetInstanceSnapshots
    (
    -- * Creating a Request
      getInstanceSnapshots
    , GetInstanceSnapshots
    -- * Request Lenses
    , gisPageToken

    -- * Destructuring the Response
    , getInstanceSnapshotsResponse
    , GetInstanceSnapshotsResponse
    -- * Response Lenses
    , gissrsNextPageToken
    , gissrsInstanceSnapshots
    , gissrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getInstanceSnapshots' smart constructor.
newtype GetInstanceSnapshots = GetInstanceSnapshots'
  { _gisPageToken :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetInstanceSnapshots' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gisPageToken' - A token used for advancing to the next page of results from your get instance snapshots request.
getInstanceSnapshots
    :: GetInstanceSnapshots
getInstanceSnapshots = GetInstanceSnapshots' {_gisPageToken = Nothing}


-- | A token used for advancing to the next page of results from your get instance snapshots request.
gisPageToken :: Lens' GetInstanceSnapshots (Maybe Text)
gisPageToken = lens _gisPageToken (\ s a -> s{_gisPageToken = a})

instance AWSPager GetInstanceSnapshots where
        page rq rs
          | stop (rs ^. gissrsNextPageToken) = Nothing
          | stop (rs ^. gissrsInstanceSnapshots) = Nothing
          | otherwise =
            Just $ rq & gisPageToken .~ rs ^. gissrsNextPageToken

instance AWSRequest GetInstanceSnapshots where
        type Rs GetInstanceSnapshots =
             GetInstanceSnapshotsResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetInstanceSnapshotsResponse' <$>
                   (x .?> "nextPageToken") <*>
                     (x .?> "instanceSnapshots" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetInstanceSnapshots where

instance NFData GetInstanceSnapshots where

instance ToHeaders GetInstanceSnapshots where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetInstanceSnapshots" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetInstanceSnapshots where
        toJSON GetInstanceSnapshots'{..}
          = object
              (catMaybes [("pageToken" .=) <$> _gisPageToken])

instance ToPath GetInstanceSnapshots where
        toPath = const "/"

instance ToQuery GetInstanceSnapshots where
        toQuery = const mempty

-- | /See:/ 'getInstanceSnapshotsResponse' smart constructor.
data GetInstanceSnapshotsResponse = GetInstanceSnapshotsResponse'
  { _gissrsNextPageToken     :: !(Maybe Text)
  , _gissrsInstanceSnapshots :: !(Maybe [InstanceSnapshot])
  , _gissrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetInstanceSnapshotsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gissrsNextPageToken' - A token used for advancing to the next page of results from your get instance snapshots request.
--
-- * 'gissrsInstanceSnapshots' - An array of key-value pairs containing information about the results of your get instance snapshots request.
--
-- * 'gissrsResponseStatus' - -- | The response status code.
getInstanceSnapshotsResponse
    :: Int -- ^ 'gissrsResponseStatus'
    -> GetInstanceSnapshotsResponse
getInstanceSnapshotsResponse pResponseStatus_ =
  GetInstanceSnapshotsResponse'
    { _gissrsNextPageToken = Nothing
    , _gissrsInstanceSnapshots = Nothing
    , _gissrsResponseStatus = pResponseStatus_
    }


-- | A token used for advancing to the next page of results from your get instance snapshots request.
gissrsNextPageToken :: Lens' GetInstanceSnapshotsResponse (Maybe Text)
gissrsNextPageToken = lens _gissrsNextPageToken (\ s a -> s{_gissrsNextPageToken = a})

-- | An array of key-value pairs containing information about the results of your get instance snapshots request.
gissrsInstanceSnapshots :: Lens' GetInstanceSnapshotsResponse [InstanceSnapshot]
gissrsInstanceSnapshots = lens _gissrsInstanceSnapshots (\ s a -> s{_gissrsInstanceSnapshots = a}) . _Default . _Coerce

-- | -- | The response status code.
gissrsResponseStatus :: Lens' GetInstanceSnapshotsResponse Int
gissrsResponseStatus = lens _gissrsResponseStatus (\ s a -> s{_gissrsResponseStatus = a})

instance NFData GetInstanceSnapshotsResponse where
