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
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AWS Directory Service directory.
--
-- <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_DeleteDirectory.html>
module Network.AWS.DirectoryService.DeleteDirectory
    (
    -- * Request
      DeleteDirectory
    -- ** Request constructor
    , deleteDirectory
    -- ** Request lenses
    , dDirectoryId

    -- * Response
    , DeleteDirectoryResponse
    -- ** Response constructor
    , deleteDirectoryResponse
    -- ** Response lenses
    , delrsDirectoryId
    , delrsStatus
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
-- * 'delrsDirectoryId'
--
-- * 'delrsStatus'
data DeleteDirectoryResponse = DeleteDirectoryResponse'
    { _delrsDirectoryId :: !(Maybe Text)
    , _delrsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDirectoryResponse' smart constructor.
deleteDirectoryResponse :: Int -> DeleteDirectoryResponse
deleteDirectoryResponse pStatus_ =
    DeleteDirectoryResponse'
    { _delrsDirectoryId = Nothing
    , _delrsStatus = pStatus_
    }

-- | The directory identifier.
delrsDirectoryId :: Lens' DeleteDirectoryResponse (Maybe Text)
delrsDirectoryId = lens _delrsDirectoryId (\ s a -> s{_delrsDirectoryId = a});

-- | FIXME: Undocumented member.
delrsStatus :: Lens' DeleteDirectoryResponse Int
delrsStatus = lens _delrsStatus (\ s a -> s{_delrsStatus = a});
