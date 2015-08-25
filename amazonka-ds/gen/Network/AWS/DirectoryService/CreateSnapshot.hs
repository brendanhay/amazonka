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
-- Module      : Network.AWS.DirectoryService.CreateSnapshot
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of an existing directory.
--
-- You cannot take snapshots of extended or connected directories.
--
-- /See:/ <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_CreateSnapshot.html AWS API Reference> for CreateSnapshot.
module Network.AWS.DirectoryService.CreateSnapshot
    (
    -- * Creating a Request
      createSnapshot
    , CreateSnapshot
    -- * Request Lenses
    , csName
    , csDirectoryId

    -- * Destructuring the Response
    , createSnapshotResponse
    , CreateSnapshotResponse
    -- * Response Lenses
    , csrsSnapshotId
    , csrsStatus
    ) where

import           Network.AWS.DirectoryService.Types
import           Network.AWS.DirectoryService.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the inputs for the CreateSnapshot operation.
--
-- /See:/ 'createSnapshot' smart constructor.
data CreateSnapshot = CreateSnapshot'
    { _csName        :: !(Maybe Text)
    , _csDirectoryId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csName'
--
-- * 'csDirectoryId'
createSnapshot
    :: Text -- ^ 'csDirectoryId'
    -> CreateSnapshot
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
        type Rs CreateSnapshot = CreateSnapshotResponse
        request = postJSON directoryService
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
              (catMaybes
                 [("Name" .=) <$> _csName,
                  Just ("DirectoryId" .= _csDirectoryId)])

instance ToPath CreateSnapshot where
        toPath = const "/"

instance ToQuery CreateSnapshot where
        toQuery = const mempty

-- | Contains the results of the CreateSnapshot operation.
--
-- /See:/ 'createSnapshotResponse' smart constructor.
data CreateSnapshotResponse = CreateSnapshotResponse'
    { _csrsSnapshotId :: !(Maybe Text)
    , _csrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csrsSnapshotId'
--
-- * 'csrsStatus'
createSnapshotResponse
    :: Int -- ^ 'csrsStatus'
    -> CreateSnapshotResponse
createSnapshotResponse pStatus_ =
    CreateSnapshotResponse'
    { _csrsSnapshotId = Nothing
    , _csrsStatus = pStatus_
    }

-- | The identifier of the snapshot that was created.
csrsSnapshotId :: Lens' CreateSnapshotResponse (Maybe Text)
csrsSnapshotId = lens _csrsSnapshotId (\ s a -> s{_csrsSnapshotId = a});

-- | The response status code.
csrsStatus :: Lens' CreateSnapshotResponse Int
csrsStatus = lens _csrsStatus (\ s a -> s{_csrsStatus = a});
