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
-- Module      : Network.AWS.MediaStoreData.DeleteObject
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an object at the specified path.
--
--
module Network.AWS.MediaStoreData.DeleteObject
    (
    -- * Creating a Request
      deleteObject
    , DeleteObject
    -- * Request Lenses
    , doPath

    -- * Destructuring the Response
    , deleteObjectResponse
    , DeleteObjectResponse
    -- * Response Lenses
    , dorsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaStoreData.Types
import Network.AWS.MediaStoreData.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteObject' smart constructor.
newtype DeleteObject = DeleteObject'
  { _doPath :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'doPath' - The path (including the file name) where the object is stored in the container. Format: <folder name>/<folder name>/<file name>
deleteObject
    :: Text -- ^ 'doPath'
    -> DeleteObject
deleteObject pPath_ = DeleteObject' {_doPath = pPath_}


-- | The path (including the file name) where the object is stored in the container. Format: <folder name>/<folder name>/<file name>
doPath :: Lens' DeleteObject Text
doPath = lens _doPath (\ s a -> s{_doPath = a})

instance AWSRequest DeleteObject where
        type Rs DeleteObject = DeleteObjectResponse
        request = delete mediaStoreData
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteObjectResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteObject where

instance NFData DeleteObject where

instance ToHeaders DeleteObject where
        toHeaders = const mempty

instance ToPath DeleteObject where
        toPath DeleteObject'{..}
          = mconcat ["/", toBS _doPath]

instance ToQuery DeleteObject where
        toQuery = const mempty

-- | /See:/ 'deleteObjectResponse' smart constructor.
newtype DeleteObjectResponse = DeleteObjectResponse'
  { _dorsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteObjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dorsResponseStatus' - -- | The response status code.
deleteObjectResponse
    :: Int -- ^ 'dorsResponseStatus'
    -> DeleteObjectResponse
deleteObjectResponse pResponseStatus_ =
  DeleteObjectResponse' {_dorsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dorsResponseStatus :: Lens' DeleteObjectResponse Int
dorsResponseStatus = lens _dorsResponseStatus (\ s a -> s{_dorsResponseStatus = a})

instance NFData DeleteObjectResponse where
