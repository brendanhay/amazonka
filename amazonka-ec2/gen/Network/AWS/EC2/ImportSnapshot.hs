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
-- Module      : Network.AWS.EC2.ImportSnapshot
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports a disk into an EBS snapshot.
--
--
module Network.AWS.EC2.ImportSnapshot
    (
    -- * Creating a Request
      importSnapshot
    , ImportSnapshot
    -- * Request Lenses
    , isDiskContainer
    , isClientToken
    , isRoleName
    , isDescription
    , isDryRun
    , isClientData

    -- * Destructuring the Response
    , importSnapshotResponse
    , ImportSnapshotResponse
    -- * Response Lenses
    , isrsSnapshotTaskDetail
    , isrsImportTaskId
    , isrsDescription
    , isrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for ImportSnapshot.
--
--
--
-- /See:/ 'importSnapshot' smart constructor.
data ImportSnapshot = ImportSnapshot'
  { _isDiskContainer :: !(Maybe SnapshotDiskContainer)
  , _isClientToken   :: !(Maybe Text)
  , _isRoleName      :: !(Maybe Text)
  , _isDescription   :: !(Maybe Text)
  , _isDryRun        :: !(Maybe Bool)
  , _isClientData    :: !(Maybe ClientData)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isDiskContainer' - Information about the disk container.
--
-- * 'isClientToken' - Token to enable idempotency for VM import requests.
--
-- * 'isRoleName' - The name of the role to use when not using the default role, 'vmimport'.
--
-- * 'isDescription' - The description string for the import snapshot task.
--
-- * 'isDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'isClientData' - The client-specific data.
importSnapshot
    :: ImportSnapshot
importSnapshot =
  ImportSnapshot'
    { _isDiskContainer = Nothing
    , _isClientToken = Nothing
    , _isRoleName = Nothing
    , _isDescription = Nothing
    , _isDryRun = Nothing
    , _isClientData = Nothing
    }


-- | Information about the disk container.
isDiskContainer :: Lens' ImportSnapshot (Maybe SnapshotDiskContainer)
isDiskContainer = lens _isDiskContainer (\ s a -> s{_isDiskContainer = a})

-- | Token to enable idempotency for VM import requests.
isClientToken :: Lens' ImportSnapshot (Maybe Text)
isClientToken = lens _isClientToken (\ s a -> s{_isClientToken = a})

-- | The name of the role to use when not using the default role, 'vmimport'.
isRoleName :: Lens' ImportSnapshot (Maybe Text)
isRoleName = lens _isRoleName (\ s a -> s{_isRoleName = a})

-- | The description string for the import snapshot task.
isDescription :: Lens' ImportSnapshot (Maybe Text)
isDescription = lens _isDescription (\ s a -> s{_isDescription = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
isDryRun :: Lens' ImportSnapshot (Maybe Bool)
isDryRun = lens _isDryRun (\ s a -> s{_isDryRun = a})

-- | The client-specific data.
isClientData :: Lens' ImportSnapshot (Maybe ClientData)
isClientData = lens _isClientData (\ s a -> s{_isClientData = a})

instance AWSRequest ImportSnapshot where
        type Rs ImportSnapshot = ImportSnapshotResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 ImportSnapshotResponse' <$>
                   (x .@? "snapshotTaskDetail") <*>
                     (x .@? "importTaskId")
                     <*> (x .@? "description")
                     <*> (pure (fromEnum s)))

instance Hashable ImportSnapshot where

instance NFData ImportSnapshot where

instance ToHeaders ImportSnapshot where
        toHeaders = const mempty

instance ToPath ImportSnapshot where
        toPath = const "/"

instance ToQuery ImportSnapshot where
        toQuery ImportSnapshot'{..}
          = mconcat
              ["Action" =: ("ImportSnapshot" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DiskContainer" =: _isDiskContainer,
               "ClientToken" =: _isClientToken,
               "RoleName" =: _isRoleName,
               "Description" =: _isDescription,
               "DryRun" =: _isDryRun, "ClientData" =: _isClientData]

-- | Contains the output for ImportSnapshot.
--
--
--
-- /See:/ 'importSnapshotResponse' smart constructor.
data ImportSnapshotResponse = ImportSnapshotResponse'
  { _isrsSnapshotTaskDetail :: !(Maybe SnapshotTaskDetail)
  , _isrsImportTaskId       :: !(Maybe Text)
  , _isrsDescription        :: !(Maybe Text)
  , _isrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isrsSnapshotTaskDetail' - Information about the import snapshot task.
--
-- * 'isrsImportTaskId' - The ID of the import snapshot task.
--
-- * 'isrsDescription' - A description of the import snapshot task.
--
-- * 'isrsResponseStatus' - -- | The response status code.
importSnapshotResponse
    :: Int -- ^ 'isrsResponseStatus'
    -> ImportSnapshotResponse
importSnapshotResponse pResponseStatus_ =
  ImportSnapshotResponse'
    { _isrsSnapshotTaskDetail = Nothing
    , _isrsImportTaskId = Nothing
    , _isrsDescription = Nothing
    , _isrsResponseStatus = pResponseStatus_
    }


-- | Information about the import snapshot task.
isrsSnapshotTaskDetail :: Lens' ImportSnapshotResponse (Maybe SnapshotTaskDetail)
isrsSnapshotTaskDetail = lens _isrsSnapshotTaskDetail (\ s a -> s{_isrsSnapshotTaskDetail = a})

-- | The ID of the import snapshot task.
isrsImportTaskId :: Lens' ImportSnapshotResponse (Maybe Text)
isrsImportTaskId = lens _isrsImportTaskId (\ s a -> s{_isrsImportTaskId = a})

-- | A description of the import snapshot task.
isrsDescription :: Lens' ImportSnapshotResponse (Maybe Text)
isrsDescription = lens _isrsDescription (\ s a -> s{_isrsDescription = a})

-- | -- | The response status code.
isrsResponseStatus :: Lens' ImportSnapshotResponse Int
isrsResponseStatus = lens _isrsResponseStatus (\ s a -> s{_isrsResponseStatus = a})

instance NFData ImportSnapshotResponse where
