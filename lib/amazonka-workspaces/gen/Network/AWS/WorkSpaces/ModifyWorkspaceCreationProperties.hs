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
-- Module      : Network.AWS.WorkSpaces.ModifyWorkspaceCreationProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify the default properties used to create WorkSpaces.
module Network.AWS.WorkSpaces.ModifyWorkspaceCreationProperties
  ( -- * Creating a Request
    modifyWorkspaceCreationProperties,
    ModifyWorkspaceCreationProperties,

    -- * Request Lenses
    mwcpResourceId,
    mwcpWorkspaceCreationProperties,

    -- * Destructuring the Response
    modifyWorkspaceCreationPropertiesResponse,
    ModifyWorkspaceCreationPropertiesResponse,

    -- * Response Lenses
    mwcprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'modifyWorkspaceCreationProperties' smart constructor.
data ModifyWorkspaceCreationProperties = ModifyWorkspaceCreationProperties'
  { _mwcpResourceId ::
      !Text,
    _mwcpWorkspaceCreationProperties ::
      !WorkspaceCreationProperties
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyWorkspaceCreationProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mwcpResourceId' - The identifier of the directory.
--
-- * 'mwcpWorkspaceCreationProperties' - The default properties for creating WorkSpaces.
modifyWorkspaceCreationProperties ::
  -- | 'mwcpResourceId'
  Text ->
  -- | 'mwcpWorkspaceCreationProperties'
  WorkspaceCreationProperties ->
  ModifyWorkspaceCreationProperties
modifyWorkspaceCreationProperties
  pResourceId_
  pWorkspaceCreationProperties_ =
    ModifyWorkspaceCreationProperties'
      { _mwcpResourceId =
          pResourceId_,
        _mwcpWorkspaceCreationProperties =
          pWorkspaceCreationProperties_
      }

-- | The identifier of the directory.
mwcpResourceId :: Lens' ModifyWorkspaceCreationProperties Text
mwcpResourceId = lens _mwcpResourceId (\s a -> s {_mwcpResourceId = a})

-- | The default properties for creating WorkSpaces.
mwcpWorkspaceCreationProperties :: Lens' ModifyWorkspaceCreationProperties WorkspaceCreationProperties
mwcpWorkspaceCreationProperties = lens _mwcpWorkspaceCreationProperties (\s a -> s {_mwcpWorkspaceCreationProperties = a})

instance AWSRequest ModifyWorkspaceCreationProperties where
  type
    Rs ModifyWorkspaceCreationProperties =
      ModifyWorkspaceCreationPropertiesResponse
  request = postJSON workSpaces
  response =
    receiveEmpty
      ( \s h x ->
          ModifyWorkspaceCreationPropertiesResponse' <$> (pure (fromEnum s))
      )

instance Hashable ModifyWorkspaceCreationProperties

instance NFData ModifyWorkspaceCreationProperties

instance ToHeaders ModifyWorkspaceCreationProperties where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "WorkspacesService.ModifyWorkspaceCreationProperties" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ModifyWorkspaceCreationProperties where
  toJSON ModifyWorkspaceCreationProperties' {..} =
    object
      ( catMaybes
          [ Just ("ResourceId" .= _mwcpResourceId),
            Just
              ( "WorkspaceCreationProperties"
                  .= _mwcpWorkspaceCreationProperties
              )
          ]
      )

instance ToPath ModifyWorkspaceCreationProperties where
  toPath = const "/"

instance ToQuery ModifyWorkspaceCreationProperties where
  toQuery = const mempty

-- | /See:/ 'modifyWorkspaceCreationPropertiesResponse' smart constructor.
newtype ModifyWorkspaceCreationPropertiesResponse = ModifyWorkspaceCreationPropertiesResponse'
  { _mwcprsResponseStatus ::
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

-- | Creates a value of 'ModifyWorkspaceCreationPropertiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mwcprsResponseStatus' - -- | The response status code.
modifyWorkspaceCreationPropertiesResponse ::
  -- | 'mwcprsResponseStatus'
  Int ->
  ModifyWorkspaceCreationPropertiesResponse
modifyWorkspaceCreationPropertiesResponse pResponseStatus_ =
  ModifyWorkspaceCreationPropertiesResponse'
    { _mwcprsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
mwcprsResponseStatus :: Lens' ModifyWorkspaceCreationPropertiesResponse Int
mwcprsResponseStatus = lens _mwcprsResponseStatus (\s a -> s {_mwcprsResponseStatus = a})

instance NFData ModifyWorkspaceCreationPropertiesResponse
