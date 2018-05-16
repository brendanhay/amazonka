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
-- Module      : Network.AWS.CloudDirectory.DeleteObject
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an object and its associated attributes. Only objects with no children and no parents can be deleted.
--
--
module Network.AWS.CloudDirectory.DeleteObject
    (
    -- * Creating a Request
      deleteObject
    , DeleteObject
    -- * Request Lenses
    , doDirectoryARN
    , doObjectReference

    -- * Destructuring the Response
    , deleteObjectResponse
    , DeleteObjectResponse
    -- * Response Lenses
    , dorsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteObject' smart constructor.
data DeleteObject = DeleteObject'
  { _doDirectoryARN    :: !Text
  , _doObjectReference :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'doDirectoryARN' - The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
--
-- * 'doObjectReference' - A reference that identifies the object.
deleteObject
    :: Text -- ^ 'doDirectoryARN'
    -> ObjectReference -- ^ 'doObjectReference'
    -> DeleteObject
deleteObject pDirectoryARN_ pObjectReference_ =
  DeleteObject'
    {_doDirectoryARN = pDirectoryARN_, _doObjectReference = pObjectReference_}


-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
doDirectoryARN :: Lens' DeleteObject Text
doDirectoryARN = lens _doDirectoryARN (\ s a -> s{_doDirectoryARN = a})

-- | A reference that identifies the object.
doObjectReference :: Lens' DeleteObject ObjectReference
doObjectReference = lens _doObjectReference (\ s a -> s{_doObjectReference = a})

instance AWSRequest DeleteObject where
        type Rs DeleteObject = DeleteObjectResponse
        request = putJSON cloudDirectory
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteObjectResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteObject where

instance NFData DeleteObject where

instance ToHeaders DeleteObject where
        toHeaders DeleteObject'{..}
          = mconcat ["x-amz-data-partition" =# _doDirectoryARN]

instance ToJSON DeleteObject where
        toJSON DeleteObject'{..}
          = object
              (catMaybes
                 [Just ("ObjectReference" .= _doObjectReference)])

instance ToPath DeleteObject where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/object/delete"

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
