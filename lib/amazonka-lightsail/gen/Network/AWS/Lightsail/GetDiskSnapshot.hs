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
-- Module      : Network.AWS.Lightsail.GetDiskSnapshot
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific block storage disk snapshot.
--
--
module Network.AWS.Lightsail.GetDiskSnapshot
    (
    -- * Creating a Request
      getDiskSnapshot
    , GetDiskSnapshot
    -- * Request Lenses
    , gdsDiskSnapshotName

    -- * Destructuring the Response
    , getDiskSnapshotResponse
    , GetDiskSnapshotResponse
    -- * Response Lenses
    , gdsrsDiskSnapshot
    , gdsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDiskSnapshot' smart constructor.
newtype GetDiskSnapshot = GetDiskSnapshot'
  { _gdsDiskSnapshotName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDiskSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdsDiskSnapshotName' - The name of the disk snapshot (e.g., @my-disk-snapshot@ ).
getDiskSnapshot
    :: Text -- ^ 'gdsDiskSnapshotName'
    -> GetDiskSnapshot
getDiskSnapshot pDiskSnapshotName_ =
  GetDiskSnapshot' {_gdsDiskSnapshotName = pDiskSnapshotName_}


-- | The name of the disk snapshot (e.g., @my-disk-snapshot@ ).
gdsDiskSnapshotName :: Lens' GetDiskSnapshot Text
gdsDiskSnapshotName = lens _gdsDiskSnapshotName (\ s a -> s{_gdsDiskSnapshotName = a})

instance AWSRequest GetDiskSnapshot where
        type Rs GetDiskSnapshot = GetDiskSnapshotResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetDiskSnapshotResponse' <$>
                   (x .?> "diskSnapshot") <*> (pure (fromEnum s)))

instance Hashable GetDiskSnapshot where

instance NFData GetDiskSnapshot where

instance ToHeaders GetDiskSnapshot where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetDiskSnapshot" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDiskSnapshot where
        toJSON GetDiskSnapshot'{..}
          = object
              (catMaybes
                 [Just ("diskSnapshotName" .= _gdsDiskSnapshotName)])

instance ToPath GetDiskSnapshot where
        toPath = const "/"

instance ToQuery GetDiskSnapshot where
        toQuery = const mempty

-- | /See:/ 'getDiskSnapshotResponse' smart constructor.
data GetDiskSnapshotResponse = GetDiskSnapshotResponse'
  { _gdsrsDiskSnapshot   :: !(Maybe DiskSnapshot)
  , _gdsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDiskSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdsrsDiskSnapshot' - An object containing information about the disk snapshot.
--
-- * 'gdsrsResponseStatus' - -- | The response status code.
getDiskSnapshotResponse
    :: Int -- ^ 'gdsrsResponseStatus'
    -> GetDiskSnapshotResponse
getDiskSnapshotResponse pResponseStatus_ =
  GetDiskSnapshotResponse'
    {_gdsrsDiskSnapshot = Nothing, _gdsrsResponseStatus = pResponseStatus_}


-- | An object containing information about the disk snapshot.
gdsrsDiskSnapshot :: Lens' GetDiskSnapshotResponse (Maybe DiskSnapshot)
gdsrsDiskSnapshot = lens _gdsrsDiskSnapshot (\ s a -> s{_gdsrsDiskSnapshot = a})

-- | -- | The response status code.
gdsrsResponseStatus :: Lens' GetDiskSnapshotResponse Int
gdsrsResponseStatus = lens _gdsrsResponseStatus (\ s a -> s{_gdsrsResponseStatus = a})

instance NFData GetDiskSnapshotResponse where
