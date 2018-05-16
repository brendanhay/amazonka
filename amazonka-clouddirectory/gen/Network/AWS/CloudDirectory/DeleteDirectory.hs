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
-- Module      : Network.AWS.CloudDirectory.DeleteDirectory
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a directory. Only disabled directories can be deleted. A deleted directory cannot be undone. Exercise extreme caution when deleting directories.
--
--
module Network.AWS.CloudDirectory.DeleteDirectory
    (
    -- * Creating a Request
      deleteDirectory
    , DeleteDirectory
    -- * Request Lenses
    , delDirectoryARN

    -- * Destructuring the Response
    , deleteDirectoryResponse
    , DeleteDirectoryResponse
    -- * Response Lenses
    , ddrsResponseStatus
    , ddrsDirectoryARN
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDirectory' smart constructor.
newtype DeleteDirectory = DeleteDirectory'
  { _delDirectoryARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDirectory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delDirectoryARN' - The ARN of the directory to delete.
deleteDirectory
    :: Text -- ^ 'delDirectoryARN'
    -> DeleteDirectory
deleteDirectory pDirectoryARN_ =
  DeleteDirectory' {_delDirectoryARN = pDirectoryARN_}


-- | The ARN of the directory to delete.
delDirectoryARN :: Lens' DeleteDirectory Text
delDirectoryARN = lens _delDirectoryARN (\ s a -> s{_delDirectoryARN = a})

instance AWSRequest DeleteDirectory where
        type Rs DeleteDirectory = DeleteDirectoryResponse
        request = putJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 DeleteDirectoryResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "DirectoryArn"))

instance Hashable DeleteDirectory where

instance NFData DeleteDirectory where

instance ToHeaders DeleteDirectory where
        toHeaders DeleteDirectory'{..}
          = mconcat
              ["x-amz-data-partition" =# _delDirectoryARN]

instance ToJSON DeleteDirectory where
        toJSON = const (Object mempty)

instance ToPath DeleteDirectory where
        toPath
          = const "/amazonclouddirectory/2017-01-11/directory"

instance ToQuery DeleteDirectory where
        toQuery = const mempty

-- | /See:/ 'deleteDirectoryResponse' smart constructor.
data DeleteDirectoryResponse = DeleteDirectoryResponse'
  { _ddrsResponseStatus :: !Int
  , _ddrsDirectoryARN   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDirectoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddrsResponseStatus' - -- | The response status code.
--
-- * 'ddrsDirectoryARN' - The ARN of the deleted directory.
deleteDirectoryResponse
    :: Int -- ^ 'ddrsResponseStatus'
    -> Text -- ^ 'ddrsDirectoryARN'
    -> DeleteDirectoryResponse
deleteDirectoryResponse pResponseStatus_ pDirectoryARN_ =
  DeleteDirectoryResponse'
    {_ddrsResponseStatus = pResponseStatus_, _ddrsDirectoryARN = pDirectoryARN_}


-- | -- | The response status code.
ddrsResponseStatus :: Lens' DeleteDirectoryResponse Int
ddrsResponseStatus = lens _ddrsResponseStatus (\ s a -> s{_ddrsResponseStatus = a})

-- | The ARN of the deleted directory.
ddrsDirectoryARN :: Lens' DeleteDirectoryResponse Text
ddrsDirectoryARN = lens _ddrsDirectoryARN (\ s a -> s{_ddrsDirectoryARN = a})

instance NFData DeleteDirectoryResponse where
