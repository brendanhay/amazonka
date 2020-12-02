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
-- Module      : Network.AWS.WorkSpaces.DeleteConnectionAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified connection alias. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces> .
--
--
-- /Important:/ __If you will no longer be using a fully qualified domain name (FQDN) as the registration code for your WorkSpaces users, you must take certain precautions to prevent potential security issues.__ For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html#cross-region-redirection-security-considerations Security Considerations if You Stop Using Cross-Region Redirection> .
module Network.AWS.WorkSpaces.DeleteConnectionAlias
  ( -- * Creating a Request
    deleteConnectionAlias,
    DeleteConnectionAlias,

    -- * Request Lenses
    dAliasId,

    -- * Destructuring the Response
    deleteConnectionAliasResponse,
    DeleteConnectionAliasResponse,

    -- * Response Lenses
    dcarsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'deleteConnectionAlias' smart constructor.
newtype DeleteConnectionAlias = DeleteConnectionAlias'
  { _dAliasId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteConnectionAlias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dAliasId' - The identifier of the connection alias to delete.
deleteConnectionAlias ::
  -- | 'dAliasId'
  Text ->
  DeleteConnectionAlias
deleteConnectionAlias pAliasId_ =
  DeleteConnectionAlias' {_dAliasId = pAliasId_}

-- | The identifier of the connection alias to delete.
dAliasId :: Lens' DeleteConnectionAlias Text
dAliasId = lens _dAliasId (\s a -> s {_dAliasId = a})

instance AWSRequest DeleteConnectionAlias where
  type Rs DeleteConnectionAlias = DeleteConnectionAliasResponse
  request = postJSON workSpaces
  response =
    receiveEmpty
      (\s h x -> DeleteConnectionAliasResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteConnectionAlias

instance NFData DeleteConnectionAlias

instance ToHeaders DeleteConnectionAlias where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("WorkspacesService.DeleteConnectionAlias" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteConnectionAlias where
  toJSON DeleteConnectionAlias' {..} =
    object (catMaybes [Just ("AliasId" .= _dAliasId)])

instance ToPath DeleteConnectionAlias where
  toPath = const "/"

instance ToQuery DeleteConnectionAlias where
  toQuery = const mempty

-- | /See:/ 'deleteConnectionAliasResponse' smart constructor.
newtype DeleteConnectionAliasResponse = DeleteConnectionAliasResponse'
  { _dcarsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteConnectionAliasResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcarsResponseStatus' - -- | The response status code.
deleteConnectionAliasResponse ::
  -- | 'dcarsResponseStatus'
  Int ->
  DeleteConnectionAliasResponse
deleteConnectionAliasResponse pResponseStatus_ =
  DeleteConnectionAliasResponse'
    { _dcarsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dcarsResponseStatus :: Lens' DeleteConnectionAliasResponse Int
dcarsResponseStatus = lens _dcarsResponseStatus (\s a -> s {_dcarsResponseStatus = a})

instance NFData DeleteConnectionAliasResponse
