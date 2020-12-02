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
-- Module      : Network.AWS.IoT.DeleteAuthorizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an authorizer.
module Network.AWS.IoT.DeleteAuthorizer
  ( -- * Creating a Request
    deleteAuthorizer,
    DeleteAuthorizer,

    -- * Request Lenses
    dAuthorizerName,

    -- * Destructuring the Response
    deleteAuthorizerResponse,
    DeleteAuthorizerResponse,

    -- * Response Lenses
    daarsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteAuthorizer' smart constructor.
newtype DeleteAuthorizer = DeleteAuthorizer'
  { _dAuthorizerName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteAuthorizer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dAuthorizerName' - The name of the authorizer to delete.
deleteAuthorizer ::
  -- | 'dAuthorizerName'
  Text ->
  DeleteAuthorizer
deleteAuthorizer pAuthorizerName_ =
  DeleteAuthorizer' {_dAuthorizerName = pAuthorizerName_}

-- | The name of the authorizer to delete.
dAuthorizerName :: Lens' DeleteAuthorizer Text
dAuthorizerName = lens _dAuthorizerName (\s a -> s {_dAuthorizerName = a})

instance AWSRequest DeleteAuthorizer where
  type Rs DeleteAuthorizer = DeleteAuthorizerResponse
  request = delete ioT
  response =
    receiveEmpty
      (\s h x -> DeleteAuthorizerResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteAuthorizer

instance NFData DeleteAuthorizer

instance ToHeaders DeleteAuthorizer where
  toHeaders = const mempty

instance ToPath DeleteAuthorizer where
  toPath DeleteAuthorizer' {..} =
    mconcat ["/authorizer/", toBS _dAuthorizerName]

instance ToQuery DeleteAuthorizer where
  toQuery = const mempty

-- | /See:/ 'deleteAuthorizerResponse' smart constructor.
newtype DeleteAuthorizerResponse = DeleteAuthorizerResponse'
  { _daarsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteAuthorizerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daarsResponseStatus' - -- | The response status code.
deleteAuthorizerResponse ::
  -- | 'daarsResponseStatus'
  Int ->
  DeleteAuthorizerResponse
deleteAuthorizerResponse pResponseStatus_ =
  DeleteAuthorizerResponse'
    { _daarsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
daarsResponseStatus :: Lens' DeleteAuthorizerResponse Int
daarsResponseStatus = lens _daarsResponseStatus (\s a -> s {_daarsResponseStatus = a})

instance NFData DeleteAuthorizerResponse
