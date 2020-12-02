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
-- Module      : Network.AWS.WorkSpaces.UpdateConnectionAliasPermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shares or unshares a connection alias with one account by specifying whether that account has permission to associate the connection alias with a directory. If the association permission is granted, the connection alias is shared with that account. If the association permission is revoked, the connection alias is unshared with the account. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces> .
module Network.AWS.WorkSpaces.UpdateConnectionAliasPermission
  ( -- * Creating a Request
    updateConnectionAliasPermission,
    UpdateConnectionAliasPermission,

    -- * Request Lenses
    ucapAliasId,
    ucapConnectionAliasPermission,

    -- * Destructuring the Response
    updateConnectionAliasPermissionResponse,
    UpdateConnectionAliasPermissionResponse,

    -- * Response Lenses
    ucaprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'updateConnectionAliasPermission' smart constructor.
data UpdateConnectionAliasPermission = UpdateConnectionAliasPermission'
  { _ucapAliasId ::
      !Text,
    _ucapConnectionAliasPermission ::
      !ConnectionAliasPermission
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateConnectionAliasPermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucapAliasId' - The identifier of the connection alias that you want to update permissions for.
--
-- * 'ucapConnectionAliasPermission' - Indicates whether to share or unshare the connection alias with the specified AWS account.
updateConnectionAliasPermission ::
  -- | 'ucapAliasId'
  Text ->
  -- | 'ucapConnectionAliasPermission'
  ConnectionAliasPermission ->
  UpdateConnectionAliasPermission
updateConnectionAliasPermission
  pAliasId_
  pConnectionAliasPermission_ =
    UpdateConnectionAliasPermission'
      { _ucapAliasId = pAliasId_,
        _ucapConnectionAliasPermission = pConnectionAliasPermission_
      }

-- | The identifier of the connection alias that you want to update permissions for.
ucapAliasId :: Lens' UpdateConnectionAliasPermission Text
ucapAliasId = lens _ucapAliasId (\s a -> s {_ucapAliasId = a})

-- | Indicates whether to share or unshare the connection alias with the specified AWS account.
ucapConnectionAliasPermission :: Lens' UpdateConnectionAliasPermission ConnectionAliasPermission
ucapConnectionAliasPermission = lens _ucapConnectionAliasPermission (\s a -> s {_ucapConnectionAliasPermission = a})

instance AWSRequest UpdateConnectionAliasPermission where
  type
    Rs UpdateConnectionAliasPermission =
      UpdateConnectionAliasPermissionResponse
  request = postJSON workSpaces
  response =
    receiveEmpty
      ( \s h x ->
          UpdateConnectionAliasPermissionResponse' <$> (pure (fromEnum s))
      )

instance Hashable UpdateConnectionAliasPermission

instance NFData UpdateConnectionAliasPermission

instance ToHeaders UpdateConnectionAliasPermission where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "WorkspacesService.UpdateConnectionAliasPermission" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateConnectionAliasPermission where
  toJSON UpdateConnectionAliasPermission' {..} =
    object
      ( catMaybes
          [ Just ("AliasId" .= _ucapAliasId),
            Just
              ("ConnectionAliasPermission" .= _ucapConnectionAliasPermission)
          ]
      )

instance ToPath UpdateConnectionAliasPermission where
  toPath = const "/"

instance ToQuery UpdateConnectionAliasPermission where
  toQuery = const mempty

-- | /See:/ 'updateConnectionAliasPermissionResponse' smart constructor.
newtype UpdateConnectionAliasPermissionResponse = UpdateConnectionAliasPermissionResponse'
  { _ucaprsResponseStatus ::
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

-- | Creates a value of 'UpdateConnectionAliasPermissionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucaprsResponseStatus' - -- | The response status code.
updateConnectionAliasPermissionResponse ::
  -- | 'ucaprsResponseStatus'
  Int ->
  UpdateConnectionAliasPermissionResponse
updateConnectionAliasPermissionResponse pResponseStatus_ =
  UpdateConnectionAliasPermissionResponse'
    { _ucaprsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
ucaprsResponseStatus :: Lens' UpdateConnectionAliasPermissionResponse Int
ucaprsResponseStatus = lens _ucaprsResponseStatus (\s a -> s {_ucaprsResponseStatus = a})

instance NFData UpdateConnectionAliasPermissionResponse
