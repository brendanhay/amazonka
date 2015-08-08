{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DeleteDirectory
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AWS Directory Service directory.
--
-- /See:/ <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_DeleteDirectory.html AWS API Reference> for DeleteDirectory.
module Network.AWS.DirectoryService.DeleteDirectory
    (
    -- * Creating a Request
      DeleteDirectory
    , deleteDirectory
    -- * Request Lenses
    , dDirectoryId

    -- * Destructuring the Response
    , DeleteDirectoryResponse
    , deleteDirectoryResponse
    -- * Response Lenses
    , drsDirectoryId
    , drsStatus
    ) where

import           Network.AWS.DirectoryService.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the inputs for the DeleteDirectory operation.
--
-- /See:/ 'deleteDirectory' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dDirectoryId'
newtype DeleteDirectory = DeleteDirectory'
    { _dDirectoryId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDirectory' smart constructor.
deleteDirectory :: Text -> DeleteDirectory
deleteDirectory pDirectoryId_ =
    DeleteDirectory'
    { _dDirectoryId = pDirectoryId_
    }

-- | The identifier of the directory to delete.
dDirectoryId :: Lens' DeleteDirectory Text
dDirectoryId = lens _dDirectoryId (\ s a -> s{_dDirectoryId = a});

instance AWSRequest DeleteDirectory where
        type Sv DeleteDirectory = DirectoryService
        type Rs DeleteDirectory = DeleteDirectoryResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DeleteDirectoryResponse' <$>
                   (x .?> "DirectoryId") <*> (pure (fromEnum s)))

instance ToHeaders DeleteDirectory where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.DeleteDirectory" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteDirectory where
        toJSON DeleteDirectory'{..}
          = object ["DirectoryId" .= _dDirectoryId]

instance ToPath DeleteDirectory where
        toPath = const "/"

instance ToQuery DeleteDirectory where
        toQuery = const mempty

-- | Contains the results of the DeleteDirectory operation.
--
-- /See:/ 'deleteDirectoryResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drsDirectoryId'
--
-- * 'drsStatus'
data DeleteDirectoryResponse = DeleteDirectoryResponse'
    { _drsDirectoryId :: !(Maybe Text)
    , _drsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDirectoryResponse' smart constructor.
deleteDirectoryResponse :: Int -> DeleteDirectoryResponse
deleteDirectoryResponse pStatus_ =
    DeleteDirectoryResponse'
    { _drsDirectoryId = Nothing
    , _drsStatus = pStatus_
    }

-- | The directory identifier.
drsDirectoryId :: Lens' DeleteDirectoryResponse (Maybe Text)
drsDirectoryId = lens _drsDirectoryId (\ s a -> s{_drsDirectoryId = a});

-- | Undocumented member.
drsStatus :: Lens' DeleteDirectoryResponse Int
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a});
