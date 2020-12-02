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
-- Module      : Network.AWS.AppStream.DeleteImagePermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes permissions for the specified private image. After you delete permissions for an image, AWS accounts to which you previously granted these permissions can no longer use the image.
module Network.AWS.AppStream.DeleteImagePermissions
  ( -- * Creating a Request
    deleteImagePermissions,
    DeleteImagePermissions,

    -- * Request Lenses
    dipName,
    dipSharedAccountId,

    -- * Destructuring the Response
    deleteImagePermissionsResponse,
    DeleteImagePermissionsResponse,

    -- * Response Lenses
    diprsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteImagePermissions' smart constructor.
data DeleteImagePermissions = DeleteImagePermissions'
  { _dipName ::
      !Text,
    _dipSharedAccountId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteImagePermissions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dipName' - The name of the private image.
--
-- * 'dipSharedAccountId' - The 12-digit identifier of the AWS account for which to delete image permissions.
deleteImagePermissions ::
  -- | 'dipName'
  Text ->
  -- | 'dipSharedAccountId'
  Text ->
  DeleteImagePermissions
deleteImagePermissions pName_ pSharedAccountId_ =
  DeleteImagePermissions'
    { _dipName = pName_,
      _dipSharedAccountId = pSharedAccountId_
    }

-- | The name of the private image.
dipName :: Lens' DeleteImagePermissions Text
dipName = lens _dipName (\s a -> s {_dipName = a})

-- | The 12-digit identifier of the AWS account for which to delete image permissions.
dipSharedAccountId :: Lens' DeleteImagePermissions Text
dipSharedAccountId = lens _dipSharedAccountId (\s a -> s {_dipSharedAccountId = a})

instance AWSRequest DeleteImagePermissions where
  type Rs DeleteImagePermissions = DeleteImagePermissionsResponse
  request = postJSON appStream
  response =
    receiveEmpty
      ( \s h x ->
          DeleteImagePermissionsResponse' <$> (pure (fromEnum s))
      )

instance Hashable DeleteImagePermissions

instance NFData DeleteImagePermissions

instance ToHeaders DeleteImagePermissions where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("PhotonAdminProxyService.DeleteImagePermissions" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteImagePermissions where
  toJSON DeleteImagePermissions' {..} =
    object
      ( catMaybes
          [ Just ("Name" .= _dipName),
            Just ("SharedAccountId" .= _dipSharedAccountId)
          ]
      )

instance ToPath DeleteImagePermissions where
  toPath = const "/"

instance ToQuery DeleteImagePermissions where
  toQuery = const mempty

-- | /See:/ 'deleteImagePermissionsResponse' smart constructor.
newtype DeleteImagePermissionsResponse = DeleteImagePermissionsResponse'
  { _diprsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteImagePermissionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diprsResponseStatus' - -- | The response status code.
deleteImagePermissionsResponse ::
  -- | 'diprsResponseStatus'
  Int ->
  DeleteImagePermissionsResponse
deleteImagePermissionsResponse pResponseStatus_ =
  DeleteImagePermissionsResponse'
    { _diprsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
diprsResponseStatus :: Lens' DeleteImagePermissionsResponse Int
diprsResponseStatus = lens _diprsResponseStatus (\s a -> s {_diprsResponseStatus = a})

instance NFData DeleteImagePermissionsResponse
