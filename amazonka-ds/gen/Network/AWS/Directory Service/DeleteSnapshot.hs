{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Directory Service.DeleteSnapshot
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

-- | Deletes a directory snapshot.
--
-- <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_DeleteSnapshot.html>
module Network.AWS.Directory Service.DeleteSnapshot
    (
    -- * Request
      DeleteSnapshot
    -- ** Request constructor
    , deleteSnapshot
    -- ** Request lenses
    , dsSnapshotId

    -- * Response
    , DeleteSnapshotResponse
    -- ** Response constructor
    , deleteSnapshotResponse
    -- ** Response lenses
    , dsrSnapshotId
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.Directory Service.Types

-- | /See:/ 'deleteSnapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsSnapshotId'
newtype DeleteSnapshot = DeleteSnapshot'{_dsSnapshotId :: Text} deriving (Eq, Read, Show)

-- | 'DeleteSnapshot' smart constructor.
deleteSnapshot :: Text -> DeleteSnapshot
deleteSnapshot pSnapshotId = DeleteSnapshot'{_dsSnapshotId = pSnapshotId};

-- | The identifier of the directory snapshot to be deleted.
dsSnapshotId :: Lens' DeleteSnapshot Text
dsSnapshotId = lens _dsSnapshotId (\ s a -> s{_dsSnapshotId = a});

instance AWSRequest DeleteSnapshot where
        type Sv DeleteSnapshot = Directory Service
        type Rs DeleteSnapshot = DeleteSnapshotResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DeleteSnapshotResponse' <$> x .?> "SnapshotId")

instance ToHeaders DeleteSnapshot where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.DeleteSnapshot" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteSnapshot where
        toJSON DeleteSnapshot'{..}
          = object ["SnapshotId" .= _dsSnapshotId]

instance ToPath DeleteSnapshot where
        toPath = const "/"

instance ToQuery DeleteSnapshot where
        toQuery = const mempty

-- | /See:/ 'deleteSnapshotResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrSnapshotId'
newtype DeleteSnapshotResponse = DeleteSnapshotResponse'{_dsrSnapshotId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DeleteSnapshotResponse' smart constructor.
deleteSnapshotResponse :: DeleteSnapshotResponse
deleteSnapshotResponse = DeleteSnapshotResponse'{_dsrSnapshotId = Nothing};

-- | The identifier of the directory snapshot that was deleted.
dsrSnapshotId :: Lens' DeleteSnapshotResponse (Maybe Text)
dsrSnapshotId = lens _dsrSnapshotId (\ s a -> s{_dsrSnapshotId = a});
