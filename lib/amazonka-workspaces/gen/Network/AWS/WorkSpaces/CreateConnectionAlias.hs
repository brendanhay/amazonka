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
-- Module      : Network.AWS.WorkSpaces.CreateConnectionAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the specified connection alias for use with cross-Region redirection. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces> .
module Network.AWS.WorkSpaces.CreateConnectionAlias
  ( -- * Creating a Request
    createConnectionAlias,
    CreateConnectionAlias,

    -- * Request Lenses
    ccaTags,
    ccaConnectionString,

    -- * Destructuring the Response
    createConnectionAliasResponse,
    CreateConnectionAliasResponse,

    -- * Response Lenses
    ccarsAliasId,
    ccarsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'createConnectionAlias' smart constructor.
data CreateConnectionAlias = CreateConnectionAlias'
  { _ccaTags ::
      !(Maybe [Tag]),
    _ccaConnectionString :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateConnectionAlias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccaTags' - The tags to associate with the connection alias.
--
-- * 'ccaConnectionString' - A connection string in the form of a fully qualified domain name (FQDN), such as @www.example.com@ . /Important:/ After you create a connection string, it is always associated to your AWS account. You cannot recreate the same connection string with a different account, even if you delete all instances of it from the original account. The connection string is globally reserved for your account.
createConnectionAlias ::
  -- | 'ccaConnectionString'
  Text ->
  CreateConnectionAlias
createConnectionAlias pConnectionString_ =
  CreateConnectionAlias'
    { _ccaTags = Nothing,
      _ccaConnectionString = pConnectionString_
    }

-- | The tags to associate with the connection alias.
ccaTags :: Lens' CreateConnectionAlias [Tag]
ccaTags = lens _ccaTags (\s a -> s {_ccaTags = a}) . _Default . _Coerce

-- | A connection string in the form of a fully qualified domain name (FQDN), such as @www.example.com@ . /Important:/ After you create a connection string, it is always associated to your AWS account. You cannot recreate the same connection string with a different account, even if you delete all instances of it from the original account. The connection string is globally reserved for your account.
ccaConnectionString :: Lens' CreateConnectionAlias Text
ccaConnectionString = lens _ccaConnectionString (\s a -> s {_ccaConnectionString = a})

instance AWSRequest CreateConnectionAlias where
  type Rs CreateConnectionAlias = CreateConnectionAliasResponse
  request = postJSON workSpaces
  response =
    receiveJSON
      ( \s h x ->
          CreateConnectionAliasResponse'
            <$> (x .?> "AliasId") <*> (pure (fromEnum s))
      )

instance Hashable CreateConnectionAlias

instance NFData CreateConnectionAlias

instance ToHeaders CreateConnectionAlias where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("WorkspacesService.CreateConnectionAlias" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateConnectionAlias where
  toJSON CreateConnectionAlias' {..} =
    object
      ( catMaybes
          [ ("Tags" .=) <$> _ccaTags,
            Just ("ConnectionString" .= _ccaConnectionString)
          ]
      )

instance ToPath CreateConnectionAlias where
  toPath = const "/"

instance ToQuery CreateConnectionAlias where
  toQuery = const mempty

-- | /See:/ 'createConnectionAliasResponse' smart constructor.
data CreateConnectionAliasResponse = CreateConnectionAliasResponse'
  { _ccarsAliasId ::
      !(Maybe Text),
    _ccarsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateConnectionAliasResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccarsAliasId' - The identifier of the connection alias.
--
-- * 'ccarsResponseStatus' - -- | The response status code.
createConnectionAliasResponse ::
  -- | 'ccarsResponseStatus'
  Int ->
  CreateConnectionAliasResponse
createConnectionAliasResponse pResponseStatus_ =
  CreateConnectionAliasResponse'
    { _ccarsAliasId = Nothing,
      _ccarsResponseStatus = pResponseStatus_
    }

-- | The identifier of the connection alias.
ccarsAliasId :: Lens' CreateConnectionAliasResponse (Maybe Text)
ccarsAliasId = lens _ccarsAliasId (\s a -> s {_ccarsAliasId = a})

-- | -- | The response status code.
ccarsResponseStatus :: Lens' CreateConnectionAliasResponse Int
ccarsResponseStatus = lens _ccarsResponseStatus (\s a -> s {_ccarsResponseStatus = a})

instance NFData CreateConnectionAliasResponse
