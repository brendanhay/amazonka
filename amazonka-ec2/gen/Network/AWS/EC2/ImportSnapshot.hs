{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ImportSnapshot
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Imports a disk into an EBS snapshot.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ImportSnapshot.html>
module Network.AWS.EC2.ImportSnapshot
    (
    -- * Request
      ImportSnapshot
    -- ** Request constructor
    , importSnapshot
    -- ** Request lenses
    , isrqDiskContainer
    , isrqClientToken
    , isrqRoleName
    , isrqDryRun
    , isrqDescription
    , isrqClientData

    -- * Response
    , ImportSnapshotResponse
    -- ** Response constructor
    , importSnapshotResponse
    -- ** Response lenses
    , isrsSnapshotTaskDetail
    , isrsImportTaskId
    , isrsDescription
    , isrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'importSnapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'isrqDiskContainer'
--
-- * 'isrqClientToken'
--
-- * 'isrqRoleName'
--
-- * 'isrqDryRun'
--
-- * 'isrqDescription'
--
-- * 'isrqClientData'
data ImportSnapshot = ImportSnapshot'
    { _isrqDiskContainer :: !(Maybe SnapshotDiskContainer)
    , _isrqClientToken   :: !(Maybe Text)
    , _isrqRoleName      :: !(Maybe Text)
    , _isrqDryRun        :: !(Maybe Bool)
    , _isrqDescription   :: !(Maybe Text)
    , _isrqClientData    :: !(Maybe ClientData)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ImportSnapshot' smart constructor.
importSnapshot :: ImportSnapshot
importSnapshot =
    ImportSnapshot'
    { _isrqDiskContainer = Nothing
    , _isrqClientToken = Nothing
    , _isrqRoleName = Nothing
    , _isrqDryRun = Nothing
    , _isrqDescription = Nothing
    , _isrqClientData = Nothing
    }

-- | Information about the disk container.
isrqDiskContainer :: Lens' ImportSnapshot (Maybe SnapshotDiskContainer)
isrqDiskContainer = lens _isrqDiskContainer (\ s a -> s{_isrqDiskContainer = a});

-- | Token to enable idempotency for VM import requests.
isrqClientToken :: Lens' ImportSnapshot (Maybe Text)
isrqClientToken = lens _isrqClientToken (\ s a -> s{_isrqClientToken = a});

-- | The name of the role to use when not using the default role,
-- \'vmimport\'.
isrqRoleName :: Lens' ImportSnapshot (Maybe Text)
isrqRoleName = lens _isrqRoleName (\ s a -> s{_isrqRoleName = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
isrqDryRun :: Lens' ImportSnapshot (Maybe Bool)
isrqDryRun = lens _isrqDryRun (\ s a -> s{_isrqDryRun = a});

-- | The description string for the import snapshot task.
isrqDescription :: Lens' ImportSnapshot (Maybe Text)
isrqDescription = lens _isrqDescription (\ s a -> s{_isrqDescription = a});

-- | The client-specific data.
isrqClientData :: Lens' ImportSnapshot (Maybe ClientData)
isrqClientData = lens _isrqClientData (\ s a -> s{_isrqClientData = a});

instance AWSRequest ImportSnapshot where
        type Sv ImportSnapshot = EC2
        type Rs ImportSnapshot = ImportSnapshotResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 ImportSnapshotResponse' <$>
                   (x .@? "snapshotTaskDetail") <*>
                     (x .@? "importTaskId")
                     <*> (x .@? "description")
                     <*> (pure (fromEnum s)))

instance ToHeaders ImportSnapshot where
        toHeaders = const mempty

instance ToPath ImportSnapshot where
        toPath = const "/"

instance ToQuery ImportSnapshot where
        toQuery ImportSnapshot'{..}
          = mconcat
              ["Action" =: ("ImportSnapshot" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DiskContainer" =: _isrqDiskContainer,
               "ClientToken" =: _isrqClientToken,
               "RoleName" =: _isrqRoleName, "DryRun" =: _isrqDryRun,
               "Description" =: _isrqDescription,
               "ClientData" =: _isrqClientData]

-- | /See:/ 'importSnapshotResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'isrsSnapshotTaskDetail'
--
-- * 'isrsImportTaskId'
--
-- * 'isrsDescription'
--
-- * 'isrsStatus'
data ImportSnapshotResponse = ImportSnapshotResponse'
    { _isrsSnapshotTaskDetail :: !(Maybe SnapshotTaskDetail)
    , _isrsImportTaskId       :: !(Maybe Text)
    , _isrsDescription        :: !(Maybe Text)
    , _isrsStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ImportSnapshotResponse' smart constructor.
importSnapshotResponse :: Int -> ImportSnapshotResponse
importSnapshotResponse pStatus_ =
    ImportSnapshotResponse'
    { _isrsSnapshotTaskDetail = Nothing
    , _isrsImportTaskId = Nothing
    , _isrsDescription = Nothing
    , _isrsStatus = pStatus_
    }

-- | Information about the import snapshot task.
isrsSnapshotTaskDetail :: Lens' ImportSnapshotResponse (Maybe SnapshotTaskDetail)
isrsSnapshotTaskDetail = lens _isrsSnapshotTaskDetail (\ s a -> s{_isrsSnapshotTaskDetail = a});

-- | The ID of the import snapshot task.
isrsImportTaskId :: Lens' ImportSnapshotResponse (Maybe Text)
isrsImportTaskId = lens _isrsImportTaskId (\ s a -> s{_isrsImportTaskId = a});

-- | A description of the import snapshot task.
isrsDescription :: Lens' ImportSnapshotResponse (Maybe Text)
isrsDescription = lens _isrsDescription (\ s a -> s{_isrsDescription = a});

-- | FIXME: Undocumented member.
isrsStatus :: Lens' ImportSnapshotResponse Int
isrsStatus = lens _isrsStatus (\ s a -> s{_isrsStatus = a});
