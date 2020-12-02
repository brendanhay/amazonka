{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteDisk
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified block storage disk. The disk must be in the @available@ state (not attached to a Lightsail instance).
--
--
-- The @delete disk@ operation supports tag-based access control via resource tags applied to the resource identified by @disk name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.DeleteDisk
  ( -- * Creating a Request
    deleteDisk,
    DeleteDisk,

    -- * Request Lenses
    dForceDeleteAddOns,
    dDiskName,

    -- * Destructuring the Response
    deleteDiskResponse,
    DeleteDiskResponse,

    -- * Response Lenses
    drsOperations,
    drsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDisk' smart constructor.
data DeleteDisk = DeleteDisk'
  { _dForceDeleteAddOns :: !(Maybe Bool),
    _dDiskName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteDisk' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dForceDeleteAddOns' - A Boolean value to indicate whether to delete the enabled add-ons for the disk.
--
-- * 'dDiskName' - The unique name of the disk you want to delete (e.g., @my-disk@ ).
deleteDisk ::
  -- | 'dDiskName'
  Text ->
  DeleteDisk
deleteDisk pDiskName_ =
  DeleteDisk'
    { _dForceDeleteAddOns = Nothing,
      _dDiskName = pDiskName_
    }

-- | A Boolean value to indicate whether to delete the enabled add-ons for the disk.
dForceDeleteAddOns :: Lens' DeleteDisk (Maybe Bool)
dForceDeleteAddOns = lens _dForceDeleteAddOns (\s a -> s {_dForceDeleteAddOns = a})

-- | The unique name of the disk you want to delete (e.g., @my-disk@ ).
dDiskName :: Lens' DeleteDisk Text
dDiskName = lens _dDiskName (\s a -> s {_dDiskName = a})

instance AWSRequest DeleteDisk where
  type Rs DeleteDisk = DeleteDiskResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          DeleteDiskResponse'
            <$> (x .?> "operations" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable DeleteDisk

instance NFData DeleteDisk

instance ToHeaders DeleteDisk where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("Lightsail_20161128.DeleteDisk" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteDisk where
  toJSON DeleteDisk' {..} =
    object
      ( catMaybes
          [ ("forceDeleteAddOns" .=) <$> _dForceDeleteAddOns,
            Just ("diskName" .= _dDiskName)
          ]
      )

instance ToPath DeleteDisk where
  toPath = const "/"

instance ToQuery DeleteDisk where
  toQuery = const mempty

-- | /See:/ 'deleteDiskResponse' smart constructor.
data DeleteDiskResponse = DeleteDiskResponse'
  { _drsOperations ::
      !(Maybe [Operation]),
    _drsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteDiskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsOperations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- * 'drsResponseStatus' - -- | The response status code.
deleteDiskResponse ::
  -- | 'drsResponseStatus'
  Int ->
  DeleteDiskResponse
deleteDiskResponse pResponseStatus_ =
  DeleteDiskResponse'
    { _drsOperations = Nothing,
      _drsResponseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
drsOperations :: Lens' DeleteDiskResponse [Operation]
drsOperations = lens _drsOperations (\s a -> s {_drsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteDiskResponse Int
drsResponseStatus = lens _drsResponseStatus (\s a -> s {_drsResponseStatus = a})

instance NFData DeleteDiskResponse
