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
-- Module      : Network.AWS.MediaStore.DeleteContainer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified container. Before you make a @DeleteContainer@ request, delete any objects in the container or in any folders in the container. You can delete only empty containers.
--
--
module Network.AWS.MediaStore.DeleteContainer
    (
    -- * Creating a Request
      deleteContainer
    , DeleteContainer
    -- * Request Lenses
    , dcContainerName

    -- * Destructuring the Response
    , deleteContainerResponse
    , DeleteContainerResponse
    -- * Response Lenses
    , dcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaStore.Types
import Network.AWS.MediaStore.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteContainer' smart constructor.
newtype DeleteContainer = DeleteContainer'
  { _dcContainerName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteContainer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcContainerName' - The name of the container to delete.
deleteContainer
    :: Text -- ^ 'dcContainerName'
    -> DeleteContainer
deleteContainer pContainerName_ =
  DeleteContainer' {_dcContainerName = pContainerName_}


-- | The name of the container to delete.
dcContainerName :: Lens' DeleteContainer Text
dcContainerName = lens _dcContainerName (\ s a -> s{_dcContainerName = a})

instance AWSRequest DeleteContainer where
        type Rs DeleteContainer = DeleteContainerResponse
        request = postJSON mediaStore
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteContainerResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteContainer where

instance NFData DeleteContainer where

instance ToHeaders DeleteContainer where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MediaStore_20170901.DeleteContainer" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteContainer where
        toJSON DeleteContainer'{..}
          = object
              (catMaybes
                 [Just ("ContainerName" .= _dcContainerName)])

instance ToPath DeleteContainer where
        toPath = const "/"

instance ToQuery DeleteContainer where
        toQuery = const mempty

-- | /See:/ 'deleteContainerResponse' smart constructor.
newtype DeleteContainerResponse = DeleteContainerResponse'
  { _dcrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteContainerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrsResponseStatus' - -- | The response status code.
deleteContainerResponse
    :: Int -- ^ 'dcrsResponseStatus'
    -> DeleteContainerResponse
deleteContainerResponse pResponseStatus_ =
  DeleteContainerResponse' {_dcrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dcrsResponseStatus :: Lens' DeleteContainerResponse Int
dcrsResponseStatus = lens _dcrsResponseStatus (\ s a -> s{_dcrsResponseStatus = a})

instance NFData DeleteContainerResponse where
