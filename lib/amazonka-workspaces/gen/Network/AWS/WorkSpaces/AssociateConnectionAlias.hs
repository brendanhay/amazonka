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
-- Module      : Network.AWS.WorkSpaces.AssociateConnectionAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified connection alias with the specified directory to enable cross-Region redirection. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces> .
module Network.AWS.WorkSpaces.AssociateConnectionAlias
  ( -- * Creating a Request
    associateConnectionAlias,
    AssociateConnectionAlias,

    -- * Request Lenses
    acaAliasId,
    acaResourceId,

    -- * Destructuring the Response
    associateConnectionAliasResponse,
    AssociateConnectionAliasResponse,

    -- * Response Lenses
    acarsConnectionIdentifier,
    acarsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'associateConnectionAlias' smart constructor.
data AssociateConnectionAlias = AssociateConnectionAlias'
  { _acaAliasId ::
      !Text,
    _acaResourceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociateConnectionAlias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acaAliasId' - The identifier of the connection alias.
--
-- * 'acaResourceId' - The identifier of the directory to associate the connection alias with.
associateConnectionAlias ::
  -- | 'acaAliasId'
  Text ->
  -- | 'acaResourceId'
  Text ->
  AssociateConnectionAlias
associateConnectionAlias pAliasId_ pResourceId_ =
  AssociateConnectionAlias'
    { _acaAliasId = pAliasId_,
      _acaResourceId = pResourceId_
    }

-- | The identifier of the connection alias.
acaAliasId :: Lens' AssociateConnectionAlias Text
acaAliasId = lens _acaAliasId (\s a -> s {_acaAliasId = a})

-- | The identifier of the directory to associate the connection alias with.
acaResourceId :: Lens' AssociateConnectionAlias Text
acaResourceId = lens _acaResourceId (\s a -> s {_acaResourceId = a})

instance AWSRequest AssociateConnectionAlias where
  type Rs AssociateConnectionAlias = AssociateConnectionAliasResponse
  request = postJSON workSpaces
  response =
    receiveJSON
      ( \s h x ->
          AssociateConnectionAliasResponse'
            <$> (x .?> "ConnectionIdentifier") <*> (pure (fromEnum s))
      )

instance Hashable AssociateConnectionAlias

instance NFData AssociateConnectionAlias

instance ToHeaders AssociateConnectionAlias where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("WorkspacesService.AssociateConnectionAlias" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON AssociateConnectionAlias where
  toJSON AssociateConnectionAlias' {..} =
    object
      ( catMaybes
          [ Just ("AliasId" .= _acaAliasId),
            Just ("ResourceId" .= _acaResourceId)
          ]
      )

instance ToPath AssociateConnectionAlias where
  toPath = const "/"

instance ToQuery AssociateConnectionAlias where
  toQuery = const mempty

-- | /See:/ 'associateConnectionAliasResponse' smart constructor.
data AssociateConnectionAliasResponse = AssociateConnectionAliasResponse'
  { _acarsConnectionIdentifier ::
      !(Maybe Text),
    _acarsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociateConnectionAliasResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acarsConnectionIdentifier' - The identifier of the connection alias association. You use the connection identifier in the DNS TXT record when you're configuring your DNS routing policies.
--
-- * 'acarsResponseStatus' - -- | The response status code.
associateConnectionAliasResponse ::
  -- | 'acarsResponseStatus'
  Int ->
  AssociateConnectionAliasResponse
associateConnectionAliasResponse pResponseStatus_ =
  AssociateConnectionAliasResponse'
    { _acarsConnectionIdentifier =
        Nothing,
      _acarsResponseStatus = pResponseStatus_
    }

-- | The identifier of the connection alias association. You use the connection identifier in the DNS TXT record when you're configuring your DNS routing policies.
acarsConnectionIdentifier :: Lens' AssociateConnectionAliasResponse (Maybe Text)
acarsConnectionIdentifier = lens _acarsConnectionIdentifier (\s a -> s {_acarsConnectionIdentifier = a})

-- | -- | The response status code.
acarsResponseStatus :: Lens' AssociateConnectionAliasResponse Int
acarsResponseStatus = lens _acarsResponseStatus (\s a -> s {_acarsResponseStatus = a})

instance NFData AssociateConnectionAliasResponse
