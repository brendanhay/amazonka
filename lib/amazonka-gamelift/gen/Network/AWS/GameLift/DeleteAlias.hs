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
-- Module      : Network.AWS.GameLift.DeleteAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an alias. This operation removes all record of the alias. Game clients attempting to access a server process using the deleted alias receive an error. To delete an alias, specify the alias ID to be deleted.
--
--
--     * 'CreateAlias'
--
--     * 'ListAliases'
--
--     * 'DescribeAlias'
--
--     * 'UpdateAlias'
--
--     * 'DeleteAlias'
--
--     * 'ResolveAlias'
module Network.AWS.GameLift.DeleteAlias
  ( -- * Creating a Request
    deleteAlias,
    DeleteAlias,

    -- * Request Lenses
    daAliasId,

    -- * Destructuring the Response
    deleteAliasResponse,
    DeleteAliasResponse,
  )
where

import Network.AWS.GameLift.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for a request operation.
--
--
--
-- /See:/ 'deleteAlias' smart constructor.
newtype DeleteAlias = DeleteAlias' {_daAliasId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteAlias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daAliasId' - A unique identifier of the alias that you want to delete. You can use either the alias ID or ARN value.
deleteAlias ::
  -- | 'daAliasId'
  Text ->
  DeleteAlias
deleteAlias pAliasId_ = DeleteAlias' {_daAliasId = pAliasId_}

-- | A unique identifier of the alias that you want to delete. You can use either the alias ID or ARN value.
daAliasId :: Lens' DeleteAlias Text
daAliasId = lens _daAliasId (\s a -> s {_daAliasId = a})

instance AWSRequest DeleteAlias where
  type Rs DeleteAlias = DeleteAliasResponse
  request = postJSON gameLift
  response = receiveNull DeleteAliasResponse'

instance Hashable DeleteAlias

instance NFData DeleteAlias

instance ToHeaders DeleteAlias where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("GameLift.DeleteAlias" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteAlias where
  toJSON DeleteAlias' {..} =
    object (catMaybes [Just ("AliasId" .= _daAliasId)])

instance ToPath DeleteAlias where
  toPath = const "/"

instance ToQuery DeleteAlias where
  toQuery = const mempty

-- | /See:/ 'deleteAliasResponse' smart constructor.
data DeleteAliasResponse = DeleteAliasResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteAliasResponse' with the minimum fields required to make a request.
deleteAliasResponse ::
  DeleteAliasResponse
deleteAliasResponse = DeleteAliasResponse'

instance NFData DeleteAliasResponse
