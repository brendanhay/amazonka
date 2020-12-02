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
-- Module      : Network.AWS.WorkSpaces.ModifyWorkspaceAccessProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specifies which devices and operating systems users can use to access their WorkSpaces. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/update-directory-details.html#control-device-access Control Device Access> .
module Network.AWS.WorkSpaces.ModifyWorkspaceAccessProperties
  ( -- * Creating a Request
    modifyWorkspaceAccessProperties,
    ModifyWorkspaceAccessProperties,

    -- * Request Lenses
    mwapResourceId,
    mwapWorkspaceAccessProperties,

    -- * Destructuring the Response
    modifyWorkspaceAccessPropertiesResponse,
    ModifyWorkspaceAccessPropertiesResponse,

    -- * Response Lenses
    mwaprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'modifyWorkspaceAccessProperties' smart constructor.
data ModifyWorkspaceAccessProperties = ModifyWorkspaceAccessProperties'
  { _mwapResourceId ::
      !Text,
    _mwapWorkspaceAccessProperties ::
      !WorkspaceAccessProperties
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyWorkspaceAccessProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mwapResourceId' - The identifier of the directory.
--
-- * 'mwapWorkspaceAccessProperties' - The device types and operating systems to enable or disable for access.
modifyWorkspaceAccessProperties ::
  -- | 'mwapResourceId'
  Text ->
  -- | 'mwapWorkspaceAccessProperties'
  WorkspaceAccessProperties ->
  ModifyWorkspaceAccessProperties
modifyWorkspaceAccessProperties
  pResourceId_
  pWorkspaceAccessProperties_ =
    ModifyWorkspaceAccessProperties'
      { _mwapResourceId = pResourceId_,
        _mwapWorkspaceAccessProperties = pWorkspaceAccessProperties_
      }

-- | The identifier of the directory.
mwapResourceId :: Lens' ModifyWorkspaceAccessProperties Text
mwapResourceId = lens _mwapResourceId (\s a -> s {_mwapResourceId = a})

-- | The device types and operating systems to enable or disable for access.
mwapWorkspaceAccessProperties :: Lens' ModifyWorkspaceAccessProperties WorkspaceAccessProperties
mwapWorkspaceAccessProperties = lens _mwapWorkspaceAccessProperties (\s a -> s {_mwapWorkspaceAccessProperties = a})

instance AWSRequest ModifyWorkspaceAccessProperties where
  type
    Rs ModifyWorkspaceAccessProperties =
      ModifyWorkspaceAccessPropertiesResponse
  request = postJSON workSpaces
  response =
    receiveEmpty
      ( \s h x ->
          ModifyWorkspaceAccessPropertiesResponse' <$> (pure (fromEnum s))
      )

instance Hashable ModifyWorkspaceAccessProperties

instance NFData ModifyWorkspaceAccessProperties

instance ToHeaders ModifyWorkspaceAccessProperties where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "WorkspacesService.ModifyWorkspaceAccessProperties" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ModifyWorkspaceAccessProperties where
  toJSON ModifyWorkspaceAccessProperties' {..} =
    object
      ( catMaybes
          [ Just ("ResourceId" .= _mwapResourceId),
            Just
              ("WorkspaceAccessProperties" .= _mwapWorkspaceAccessProperties)
          ]
      )

instance ToPath ModifyWorkspaceAccessProperties where
  toPath = const "/"

instance ToQuery ModifyWorkspaceAccessProperties where
  toQuery = const mempty

-- | /See:/ 'modifyWorkspaceAccessPropertiesResponse' smart constructor.
newtype ModifyWorkspaceAccessPropertiesResponse = ModifyWorkspaceAccessPropertiesResponse'
  { _mwaprsResponseStatus ::
      Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'ModifyWorkspaceAccessPropertiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mwaprsResponseStatus' - -- | The response status code.
modifyWorkspaceAccessPropertiesResponse ::
  -- | 'mwaprsResponseStatus'
  Int ->
  ModifyWorkspaceAccessPropertiesResponse
modifyWorkspaceAccessPropertiesResponse pResponseStatus_ =
  ModifyWorkspaceAccessPropertiesResponse'
    { _mwaprsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
mwaprsResponseStatus :: Lens' ModifyWorkspaceAccessPropertiesResponse Int
mwaprsResponseStatus = lens _mwaprsResponseStatus (\s a -> s {_mwaprsResponseStatus = a})

instance NFData ModifyWorkspaceAccessPropertiesResponse
