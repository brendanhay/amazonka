{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.ImportSnapshot
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

-- | Imports a disk into an EBS snapshot.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ImportSnapshot.html>
module Network.AWS.EC2.ImportSnapshot
    (
    -- * Request
      ImportSnapshot
    -- ** Request constructor
    , importSnapshot
    -- ** Request lenses
    , isDiskContainer
    , isClientToken
    , isRoleName
    , isDryRun
    , isDescription
    , isClientData

    -- * Response
    , ImportSnapshotResponse
    -- ** Response constructor
    , importSnapshotResponse
    -- ** Response lenses
    , isrSnapshotTaskDetail
    , isrImportTaskId
    , isrDescription
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.EC2.Types

-- | /See:/ 'importSnapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'isDiskContainer'
--
-- * 'isClientToken'
--
-- * 'isRoleName'
--
-- * 'isDryRun'
--
-- * 'isDescription'
--
-- * 'isClientData'
data ImportSnapshot = ImportSnapshot'{_isDiskContainer :: Maybe SnapshotDiskContainer, _isClientToken :: Maybe Text, _isRoleName :: Maybe Text, _isDryRun :: Maybe Bool, _isDescription :: Maybe Text, _isClientData :: Maybe ClientData} deriving (Eq, Read, Show)

-- | 'ImportSnapshot' smart constructor.
importSnapshot :: ImportSnapshot
importSnapshot = ImportSnapshot'{_isDiskContainer = Nothing, _isClientToken = Nothing, _isRoleName = Nothing, _isDryRun = Nothing, _isDescription = Nothing, _isClientData = Nothing};

-- | Information about the disk container.
isDiskContainer :: Lens' ImportSnapshot (Maybe SnapshotDiskContainer)
isDiskContainer = lens _isDiskContainer (\ s a -> s{_isDiskContainer = a});

-- | Token to enable idempotency for VM import requests.
isClientToken :: Lens' ImportSnapshot (Maybe Text)
isClientToken = lens _isClientToken (\ s a -> s{_isClientToken = a});

-- | The name of the role to use when not using the default role,
-- \'vmimport\'.
isRoleName :: Lens' ImportSnapshot (Maybe Text)
isRoleName = lens _isRoleName (\ s a -> s{_isRoleName = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
isDryRun :: Lens' ImportSnapshot (Maybe Bool)
isDryRun = lens _isDryRun (\ s a -> s{_isDryRun = a});

-- | The description string for the import snapshot task.
isDescription :: Lens' ImportSnapshot (Maybe Text)
isDescription = lens _isDescription (\ s a -> s{_isDescription = a});

-- | The client-specific data.
isClientData :: Lens' ImportSnapshot (Maybe ClientData)
isClientData = lens _isClientData (\ s a -> s{_isClientData = a});

instance AWSRequest ImportSnapshot where
        type Sv ImportSnapshot = EC2
        type Rs ImportSnapshot = ImportSnapshotResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 ImportSnapshotResponse' <$>
                   x .@? "snapshotTaskDetail" <*> x .@? "importTaskId"
                     <*> x .@? "description")

instance ToHeaders ImportSnapshot where
        toHeaders = const mempty

instance ToPath ImportSnapshot where
        toPath = const "/"

instance ToQuery ImportSnapshot where
        toQuery ImportSnapshot'{..}
          = mconcat
              ["Action" =: ("ImportSnapshot" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DiskContainer" =: _isDiskContainer,
               "ClientToken" =: _isClientToken,
               "RoleName" =: _isRoleName, "DryRun" =: _isDryRun,
               "Description" =: _isDescription,
               "ClientData" =: _isClientData]

-- | /See:/ 'importSnapshotResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'isrSnapshotTaskDetail'
--
-- * 'isrImportTaskId'
--
-- * 'isrDescription'
data ImportSnapshotResponse = ImportSnapshotResponse'{_isrSnapshotTaskDetail :: Maybe SnapshotTaskDetail, _isrImportTaskId :: Maybe Text, _isrDescription :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ImportSnapshotResponse' smart constructor.
importSnapshotResponse :: ImportSnapshotResponse
importSnapshotResponse = ImportSnapshotResponse'{_isrSnapshotTaskDetail = Nothing, _isrImportTaskId = Nothing, _isrDescription = Nothing};

-- | Information about the import snapshot task.
isrSnapshotTaskDetail :: Lens' ImportSnapshotResponse (Maybe SnapshotTaskDetail)
isrSnapshotTaskDetail = lens _isrSnapshotTaskDetail (\ s a -> s{_isrSnapshotTaskDetail = a});

-- | The ID of the import snapshot task.
isrImportTaskId :: Lens' ImportSnapshotResponse (Maybe Text)
isrImportTaskId = lens _isrImportTaskId (\ s a -> s{_isrImportTaskId = a});

-- | A description of the import snapshot task.
isrDescription :: Lens' ImportSnapshotResponse (Maybe Text)
isrDescription = lens _isrDescription (\ s a -> s{_isrDescription = a});
