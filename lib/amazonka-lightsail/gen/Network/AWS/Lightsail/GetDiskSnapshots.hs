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
-- Module      : Network.AWS.Lightsail.GetDiskSnapshots
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all block storage disk snapshots in your AWS account and region.
--
--
-- If you are describing a long list of disk snapshots, you can paginate the output to make the list more manageable. You can use the pageToken and nextPageToken values to retrieve the next items in the list.
--
module Network.AWS.Lightsail.GetDiskSnapshots
    (
    -- * Creating a Request
      getDiskSnapshots
    , GetDiskSnapshots
    -- * Request Lenses
    , gdsPageToken

    -- * Destructuring the Response
    , getDiskSnapshotsResponse
    , GetDiskSnapshotsResponse
    -- * Response Lenses
    , gdssrsNextPageToken
    , gdssrsDiskSnapshots
    , gdssrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDiskSnapshots' smart constructor.
newtype GetDiskSnapshots = GetDiskSnapshots'
  { _gdsPageToken :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDiskSnapshots' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdsPageToken' - A token used for advancing to the next page of results from your GetDiskSnapshots request.
getDiskSnapshots
    :: GetDiskSnapshots
getDiskSnapshots = GetDiskSnapshots' {_gdsPageToken = Nothing}


-- | A token used for advancing to the next page of results from your GetDiskSnapshots request.
gdsPageToken :: Lens' GetDiskSnapshots (Maybe Text)
gdsPageToken = lens _gdsPageToken (\ s a -> s{_gdsPageToken = a})

instance AWSRequest GetDiskSnapshots where
        type Rs GetDiskSnapshots = GetDiskSnapshotsResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetDiskSnapshotsResponse' <$>
                   (x .?> "nextPageToken") <*>
                     (x .?> "diskSnapshots" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetDiskSnapshots where

instance NFData GetDiskSnapshots where

instance ToHeaders GetDiskSnapshots where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetDiskSnapshots" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDiskSnapshots where
        toJSON GetDiskSnapshots'{..}
          = object
              (catMaybes [("pageToken" .=) <$> _gdsPageToken])

instance ToPath GetDiskSnapshots where
        toPath = const "/"

instance ToQuery GetDiskSnapshots where
        toQuery = const mempty

-- | /See:/ 'getDiskSnapshotsResponse' smart constructor.
data GetDiskSnapshotsResponse = GetDiskSnapshotsResponse'
  { _gdssrsNextPageToken  :: !(Maybe Text)
  , _gdssrsDiskSnapshots  :: !(Maybe [DiskSnapshot])
  , _gdssrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDiskSnapshotsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdssrsNextPageToken' - A token used for advancing to the next page of results from your GetDiskSnapshots request.
--
-- * 'gdssrsDiskSnapshots' - An array of objects containing information about all block storage disk snapshots.
--
-- * 'gdssrsResponseStatus' - -- | The response status code.
getDiskSnapshotsResponse
    :: Int -- ^ 'gdssrsResponseStatus'
    -> GetDiskSnapshotsResponse
getDiskSnapshotsResponse pResponseStatus_ =
  GetDiskSnapshotsResponse'
    { _gdssrsNextPageToken = Nothing
    , _gdssrsDiskSnapshots = Nothing
    , _gdssrsResponseStatus = pResponseStatus_
    }


-- | A token used for advancing to the next page of results from your GetDiskSnapshots request.
gdssrsNextPageToken :: Lens' GetDiskSnapshotsResponse (Maybe Text)
gdssrsNextPageToken = lens _gdssrsNextPageToken (\ s a -> s{_gdssrsNextPageToken = a})

-- | An array of objects containing information about all block storage disk snapshots.
gdssrsDiskSnapshots :: Lens' GetDiskSnapshotsResponse [DiskSnapshot]
gdssrsDiskSnapshots = lens _gdssrsDiskSnapshots (\ s a -> s{_gdssrsDiskSnapshots = a}) . _Default . _Coerce

-- | -- | The response status code.
gdssrsResponseStatus :: Lens' GetDiskSnapshotsResponse Int
gdssrsResponseStatus = lens _gdssrsResponseStatus (\ s a -> s{_gdssrsResponseStatus = a})

instance NFData GetDiskSnapshotsResponse where
