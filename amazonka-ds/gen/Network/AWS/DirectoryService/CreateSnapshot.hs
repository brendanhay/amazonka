{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.CreateSnapshot
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of an existing directory.
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
    , csrsSnapshotId
    , csrsStatus
    ) where

import           Network.AWS.DirectoryService.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the inputs for the CreateSnapshot operation.
--
-- /See:/ 'createSnapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csName'
--
-- * 'csDirectoryId'
data CreateSnapshot = CreateSnapshot'
    { _csName        :: !(Maybe Text)
    , _csDirectoryId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateSnapshot' smart constructor.
createSnapshot :: Text -> CreateSnapshot
createSnapshot pDirectoryId_ =
    CreateSnapshot'
    { _csName = Nothing
    , _csDirectoryId = pDirectoryId_
    }

-- | The descriptive name to apply to the snapshot.
csName :: Lens' CreateSnapshot (Maybe Text)
csName = lens _csName (\ s a -> s{_csName = a});

-- | The identifier of the directory to take a snapshot of.
csDirectoryId :: Lens' CreateSnapshot Text
csDirectoryId = lens _csDirectoryId (\ s a -> s{_csDirectoryId = a});

instance AWSRequest CreateSnapshot where
        type Sv CreateSnapshot = DirectoryService
        type Rs CreateSnapshot = CreateSnapshotResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateSnapshotResponse' <$>
                   (x .?> "SnapshotId") <*> (pure (fromEnum s)))

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

-- | Contains the results of the CreateSnapshot operation.
--
-- /See:/ 'createSnapshotResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csrsSnapshotId'
--
-- * 'csrsStatus'
data CreateSnapshotResponse = CreateSnapshotResponse'
    { _csrsSnapshotId :: !(Maybe Text)
    , _csrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateSnapshotResponse' smart constructor.
createSnapshotResponse :: Int -> CreateSnapshotResponse
createSnapshotResponse pStatus_ =
    CreateSnapshotResponse'
    { _csrsSnapshotId = Nothing
    , _csrsStatus = pStatus_
    }

-- | The identifier of the snapshot that was created.
csrsSnapshotId :: Lens' CreateSnapshotResponse (Maybe Text)
csrsSnapshotId = lens _csrsSnapshotId (\ s a -> s{_csrsSnapshotId = a});

-- | FIXME: Undocumented member.
csrsStatus :: Lens' CreateSnapshotResponse Int
csrsStatus = lens _csrsStatus (\ s a -> s{_csrsStatus = a});
