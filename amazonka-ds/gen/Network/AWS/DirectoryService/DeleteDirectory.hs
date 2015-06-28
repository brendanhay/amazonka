{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.DirectoryService.DeleteDirectory
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

-- | Deletes an AWS Directory Service directory.
--
-- <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_DeleteDirectory.html>
module Network.AWS.DirectoryService.DeleteDirectory
    (
    -- * Request
      DeleteDirectory
    -- ** Request constructor
    , deleteDirectory
    -- ** Request lenses
    , delDirectoryId

    -- * Response
    , DeleteDirectoryResponse
    -- ** Response constructor
    , deleteDirectoryResponse
    -- ** Response lenses
    , dDirectoryId
    , dStatus
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
-- * 'delDirectoryId'
newtype DeleteDirectory = DeleteDirectory'
    { _delDirectoryId :: Text
    } deriving (Eq,Read,Show)

-- | 'DeleteDirectory' smart constructor.
deleteDirectory :: Text -> DeleteDirectory
deleteDirectory pDirectoryId =
    DeleteDirectory'
    { _delDirectoryId = pDirectoryId
    }

-- | The identifier of the directory to delete.
delDirectoryId :: Lens' DeleteDirectory Text
delDirectoryId = lens _delDirectoryId (\ s a -> s{_delDirectoryId = a});

instance AWSRequest DeleteDirectory where
        type Sv DeleteDirectory = DirectoryService
        type Rs DeleteDirectory = DeleteDirectoryResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DeleteDirectoryResponse' <$>
                   (x .?> "DirectoryId") <*> (pure s))

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
          = object ["DirectoryId" .= _delDirectoryId]

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
-- * 'dDirectoryId'
--
-- * 'dStatus'
data DeleteDirectoryResponse = DeleteDirectoryResponse'
    { _dDirectoryId :: !(Maybe Text)
    , _dStatus      :: !Status
    } deriving (Eq,Read,Show)

-- | 'DeleteDirectoryResponse' smart constructor.
deleteDirectoryResponse :: Status -> DeleteDirectoryResponse
deleteDirectoryResponse pStatus =
    DeleteDirectoryResponse'
    { _dDirectoryId = Nothing
    , _dStatus = pStatus
    }

-- | The directory identifier.
dDirectoryId :: Lens' DeleteDirectoryResponse (Maybe Text)
dDirectoryId = lens _dDirectoryId (\ s a -> s{_dDirectoryId = a});

-- | FIXME: Undocumented member.
dStatus :: Lens' DeleteDirectoryResponse Status
dStatus = lens _dStatus (\ s a -> s{_dStatus = a});
