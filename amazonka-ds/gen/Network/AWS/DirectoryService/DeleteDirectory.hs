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
      deleteDirectory
    , DeleteDirectory
    -- * Request Lenses
    , delDirectoryId

    -- * Destructuring the Response
    , deleteDirectoryResponse
    , DeleteDirectoryResponse
    -- * Response Lenses
    , drsDirectoryId
    , drsResponseStatus
    ) where

import           Network.AWS.DirectoryService.Types
import           Network.AWS.DirectoryService.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the inputs for the < DeleteDirectory> operation.
--
-- /See:/ 'deleteDirectory' smart constructor.
newtype DeleteDirectory = DeleteDirectory'
    { _delDirectoryId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteDirectory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delDirectoryId'
deleteDirectory
    :: Text -- ^ 'delDirectoryId'
    -> DeleteDirectory
deleteDirectory pDirectoryId_ =
    DeleteDirectory'
    { _delDirectoryId = pDirectoryId_
    }

-- | The identifier of the directory to delete.
delDirectoryId :: Lens' DeleteDirectory Text
delDirectoryId = lens _delDirectoryId (\ s a -> s{_delDirectoryId = a});

instance AWSRequest DeleteDirectory where
        type Rs DeleteDirectory = DeleteDirectoryResponse
        request = postJSON directoryService
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
          = object
              (catMaybes [Just ("DirectoryId" .= _delDirectoryId)])

instance ToPath DeleteDirectory where
        toPath = const "/"

instance ToQuery DeleteDirectory where
        toQuery = const mempty

-- | Contains the results of the < DeleteDirectory> operation.
--
-- /See:/ 'deleteDirectoryResponse' smart constructor.
data DeleteDirectoryResponse = DeleteDirectoryResponse'
    { _drsDirectoryId    :: !(Maybe Text)
    , _drsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteDirectoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsDirectoryId'
--
-- * 'drsResponseStatus'
deleteDirectoryResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeleteDirectoryResponse
deleteDirectoryResponse pResponseStatus_ =
    DeleteDirectoryResponse'
    { _drsDirectoryId = Nothing
    , _drsResponseStatus = pResponseStatus_
    }

-- | The directory identifier.
drsDirectoryId :: Lens' DeleteDirectoryResponse (Maybe Text)
drsDirectoryId = lens _drsDirectoryId (\ s a -> s{_drsDirectoryId = a});

-- | The response status code.
drsResponseStatus :: Lens' DeleteDirectoryResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a});
