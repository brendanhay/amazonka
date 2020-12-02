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
-- Module      : Network.AWS.WorkSpaces.DisassociateConnectionAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a connection alias from a directory. Disassociating a connection alias disables cross-Region redirection between two directories in different AWS Regions. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces> .
module Network.AWS.WorkSpaces.DisassociateConnectionAlias
  ( -- * Creating a Request
    disassociateConnectionAlias,
    DisassociateConnectionAlias,

    -- * Request Lenses
    dcaAliasId,

    -- * Destructuring the Response
    disassociateConnectionAliasResponse,
    DisassociateConnectionAliasResponse,

    -- * Response Lenses
    dcasrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'disassociateConnectionAlias' smart constructor.
newtype DisassociateConnectionAlias = DisassociateConnectionAlias'
  { _dcaAliasId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisassociateConnectionAlias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcaAliasId' - The identifier of the connection alias to disassociate.
disassociateConnectionAlias ::
  -- | 'dcaAliasId'
  Text ->
  DisassociateConnectionAlias
disassociateConnectionAlias pAliasId_ =
  DisassociateConnectionAlias' {_dcaAliasId = pAliasId_}

-- | The identifier of the connection alias to disassociate.
dcaAliasId :: Lens' DisassociateConnectionAlias Text
dcaAliasId = lens _dcaAliasId (\s a -> s {_dcaAliasId = a})

instance AWSRequest DisassociateConnectionAlias where
  type
    Rs DisassociateConnectionAlias =
      DisassociateConnectionAliasResponse
  request = postJSON workSpaces
  response =
    receiveEmpty
      ( \s h x ->
          DisassociateConnectionAliasResponse' <$> (pure (fromEnum s))
      )

instance Hashable DisassociateConnectionAlias

instance NFData DisassociateConnectionAlias

instance ToHeaders DisassociateConnectionAlias where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("WorkspacesService.DisassociateConnectionAlias" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DisassociateConnectionAlias where
  toJSON DisassociateConnectionAlias' {..} =
    object (catMaybes [Just ("AliasId" .= _dcaAliasId)])

instance ToPath DisassociateConnectionAlias where
  toPath = const "/"

instance ToQuery DisassociateConnectionAlias where
  toQuery = const mempty

-- | /See:/ 'disassociateConnectionAliasResponse' smart constructor.
newtype DisassociateConnectionAliasResponse = DisassociateConnectionAliasResponse'
  { _dcasrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisassociateConnectionAliasResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcasrsResponseStatus' - -- | The response status code.
disassociateConnectionAliasResponse ::
  -- | 'dcasrsResponseStatus'
  Int ->
  DisassociateConnectionAliasResponse
disassociateConnectionAliasResponse pResponseStatus_ =
  DisassociateConnectionAliasResponse'
    { _dcasrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dcasrsResponseStatus :: Lens' DisassociateConnectionAliasResponse Int
dcasrsResponseStatus = lens _dcasrsResponseStatus (\s a -> s {_dcasrsResponseStatus = a})

instance NFData DisassociateConnectionAliasResponse
