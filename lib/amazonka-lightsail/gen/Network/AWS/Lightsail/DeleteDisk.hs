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
-- Module      : Network.AWS.Lightsail.DeleteDisk
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified block storage disk. The disk must be in the @available@ state (not attached to a Lightsail instance).
--
--
module Network.AWS.Lightsail.DeleteDisk
    (
    -- * Creating a Request
      deleteDisk
    , DeleteDisk
    -- * Request Lenses
    , dDiskName

    -- * Destructuring the Response
    , deleteDiskResponse
    , DeleteDiskResponse
    -- * Response Lenses
    , drsOperations
    , drsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDisk' smart constructor.
newtype DeleteDisk = DeleteDisk'
  { _dDiskName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDisk' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDiskName' - The unique name of the disk you want to delete (e.g., @my-disk@ ).
deleteDisk
    :: Text -- ^ 'dDiskName'
    -> DeleteDisk
deleteDisk pDiskName_ = DeleteDisk' {_dDiskName = pDiskName_}


-- | The unique name of the disk you want to delete (e.g., @my-disk@ ).
dDiskName :: Lens' DeleteDisk Text
dDiskName = lens _dDiskName (\ s a -> s{_dDiskName = a})

instance AWSRequest DeleteDisk where
        type Rs DeleteDisk = DeleteDiskResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 DeleteDiskResponse' <$>
                   (x .?> "operations" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DeleteDisk where

instance NFData DeleteDisk where

instance ToHeaders DeleteDisk where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.DeleteDisk" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteDisk where
        toJSON DeleteDisk'{..}
          = object
              (catMaybes [Just ("diskName" .= _dDiskName)])

instance ToPath DeleteDisk where
        toPath = const "/"

instance ToQuery DeleteDisk where
        toQuery = const mempty

-- | /See:/ 'deleteDiskResponse' smart constructor.
data DeleteDiskResponse = DeleteDiskResponse'
  { _drsOperations     :: !(Maybe [Operation])
  , _drsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDiskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsOperations' - An object describing the API operations.
--
-- * 'drsResponseStatus' - -- | The response status code.
deleteDiskResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeleteDiskResponse
deleteDiskResponse pResponseStatus_ =
  DeleteDiskResponse'
    {_drsOperations = Nothing, _drsResponseStatus = pResponseStatus_}


-- | An object describing the API operations.
drsOperations :: Lens' DeleteDiskResponse [Operation]
drsOperations = lens _drsOperations (\ s a -> s{_drsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteDiskResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DeleteDiskResponse where
