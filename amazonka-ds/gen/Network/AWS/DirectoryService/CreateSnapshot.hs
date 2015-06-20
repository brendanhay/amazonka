{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.DirectoryService.CreateSnapshot
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a snapshot of an existing directory.
--
-- You cannot take snapshots of extended or connected directories.
--
-- <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_CreateSnapshot.html>
module Network.AWS.DirectoryService.CreateSnapshot
    (
    -- * Request
      CreateSnapshot
    -- ** Request constructor
    , createSnapshot
    -- ** Request lenses
    , csName
    , csDirectoryId

    -- * Response
    , CreateSnapshotResponse
    -- ** Response constructor
    , createSnapshotResponse
    -- ** Response lenses
    , csrSnapshotId
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createSnapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csName'
--
-- * 'csDirectoryId'
data CreateSnapshot = CreateSnapshot'{_csName :: Maybe Text, _csDirectoryId :: Text} deriving (Eq, Read, Show)

-- | 'CreateSnapshot' smart constructor.
createSnapshot :: Text -> CreateSnapshot
createSnapshot pDirectoryId = CreateSnapshot'{_csName = Nothing, _csDirectoryId = pDirectoryId};

-- | The descriptive name to apply to the snapshot.
csName :: Lens' CreateSnapshot (Maybe Text)
csName = lens _csName (\ s a -> s{_csName = a});

-- | The identifier of the directory to take a snapshot of.
csDirectoryId :: Lens' CreateSnapshot Text
csDirectoryId = lens _csDirectoryId (\ s a -> s{_csDirectoryId = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest CreateSnapshot where
        type Sv CreateSnapshot = DirectoryService
        type Rs CreateSnapshot = CreateSnapshotResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateSnapshotResponse' <$> (x .?> "SnapshotId"))

instance ToHeaders CreateSnapshot where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.CreateSnapshot" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateSnapshot where
        toJSON CreateSnapshot'{..}
          = object
              ["Name" .= _csName, "DirectoryId" .= _csDirectoryId]

instance ToPath CreateSnapshot where
        toPath = const "/"

instance ToQuery CreateSnapshot where
        toQuery = const mempty

-- | /See:/ 'createSnapshotResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csrSnapshotId'
newtype CreateSnapshotResponse = CreateSnapshotResponse'{_csrSnapshotId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'CreateSnapshotResponse' smart constructor.
createSnapshotResponse :: CreateSnapshotResponse
createSnapshotResponse = CreateSnapshotResponse'{_csrSnapshotId = Nothing};

-- | The identifier of the snapshot that was created.
csrSnapshotId :: Lens' CreateSnapshotResponse (Maybe Text)
csrSnapshotId = lens _csrSnapshotId (\ s a -> s{_csrSnapshotId = a});
