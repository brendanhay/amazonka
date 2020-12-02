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
-- Module      : Network.AWS.WorkSpaces.ModifySelfservicePermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the self-service WorkSpace management capabilities for your users. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/enable-user-self-service-workspace-management.html Enable Self-Service WorkSpace Management Capabilities for Your Users> .
module Network.AWS.WorkSpaces.ModifySelfservicePermissions
  ( -- * Creating a Request
    modifySelfservicePermissions,
    ModifySelfservicePermissions,

    -- * Request Lenses
    mspResourceId,
    mspSelfservicePermissions,

    -- * Destructuring the Response
    modifySelfservicePermissionsResponse,
    ModifySelfservicePermissionsResponse,

    -- * Response Lenses
    msprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'modifySelfservicePermissions' smart constructor.
data ModifySelfservicePermissions = ModifySelfservicePermissions'
  { _mspResourceId ::
      !Text,
    _mspSelfservicePermissions ::
      !SelfservicePermissions
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifySelfservicePermissions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mspResourceId' - The identifier of the directory.
--
-- * 'mspSelfservicePermissions' - The permissions to enable or disable self-service capabilities.
modifySelfservicePermissions ::
  -- | 'mspResourceId'
  Text ->
  -- | 'mspSelfservicePermissions'
  SelfservicePermissions ->
  ModifySelfservicePermissions
modifySelfservicePermissions pResourceId_ pSelfservicePermissions_ =
  ModifySelfservicePermissions'
    { _mspResourceId = pResourceId_,
      _mspSelfservicePermissions = pSelfservicePermissions_
    }

-- | The identifier of the directory.
mspResourceId :: Lens' ModifySelfservicePermissions Text
mspResourceId = lens _mspResourceId (\s a -> s {_mspResourceId = a})

-- | The permissions to enable or disable self-service capabilities.
mspSelfservicePermissions :: Lens' ModifySelfservicePermissions SelfservicePermissions
mspSelfservicePermissions = lens _mspSelfservicePermissions (\s a -> s {_mspSelfservicePermissions = a})

instance AWSRequest ModifySelfservicePermissions where
  type
    Rs ModifySelfservicePermissions =
      ModifySelfservicePermissionsResponse
  request = postJSON workSpaces
  response =
    receiveEmpty
      ( \s h x ->
          ModifySelfservicePermissionsResponse' <$> (pure (fromEnum s))
      )

instance Hashable ModifySelfservicePermissions

instance NFData ModifySelfservicePermissions

instance ToHeaders ModifySelfservicePermissions where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("WorkspacesService.ModifySelfservicePermissions" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ModifySelfservicePermissions where
  toJSON ModifySelfservicePermissions' {..} =
    object
      ( catMaybes
          [ Just ("ResourceId" .= _mspResourceId),
            Just ("SelfservicePermissions" .= _mspSelfservicePermissions)
          ]
      )

instance ToPath ModifySelfservicePermissions where
  toPath = const "/"

instance ToQuery ModifySelfservicePermissions where
  toQuery = const mempty

-- | /See:/ 'modifySelfservicePermissionsResponse' smart constructor.
newtype ModifySelfservicePermissionsResponse = ModifySelfservicePermissionsResponse'
  { _msprsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifySelfservicePermissionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msprsResponseStatus' - -- | The response status code.
modifySelfservicePermissionsResponse ::
  -- | 'msprsResponseStatus'
  Int ->
  ModifySelfservicePermissionsResponse
modifySelfservicePermissionsResponse pResponseStatus_ =
  ModifySelfservicePermissionsResponse'
    { _msprsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
msprsResponseStatus :: Lens' ModifySelfservicePermissionsResponse Int
msprsResponseStatus = lens _msprsResponseStatus (\s a -> s {_msprsResponseStatus = a})

instance NFData ModifySelfservicePermissionsResponse
