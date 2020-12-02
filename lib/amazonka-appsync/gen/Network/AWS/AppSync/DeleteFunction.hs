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
-- Module      : Network.AWS.AppSync.DeleteFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a @Function@ .
module Network.AWS.AppSync.DeleteFunction
  ( -- * Creating a Request
    deleteFunction,
    DeleteFunction,

    -- * Request Lenses
    dfApiId,
    dfFunctionId,

    -- * Destructuring the Response
    deleteFunctionResponse,
    DeleteFunctionResponse,

    -- * Response Lenses
    dfrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteFunction' smart constructor.
data DeleteFunction = DeleteFunction'
  { _dfApiId :: !Text,
    _dfFunctionId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteFunction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfApiId' - The GraphQL API ID.
--
-- * 'dfFunctionId' - The @Function@ ID.
deleteFunction ::
  -- | 'dfApiId'
  Text ->
  -- | 'dfFunctionId'
  Text ->
  DeleteFunction
deleteFunction pApiId_ pFunctionId_ =
  DeleteFunction' {_dfApiId = pApiId_, _dfFunctionId = pFunctionId_}

-- | The GraphQL API ID.
dfApiId :: Lens' DeleteFunction Text
dfApiId = lens _dfApiId (\s a -> s {_dfApiId = a})

-- | The @Function@ ID.
dfFunctionId :: Lens' DeleteFunction Text
dfFunctionId = lens _dfFunctionId (\s a -> s {_dfFunctionId = a})

instance AWSRequest DeleteFunction where
  type Rs DeleteFunction = DeleteFunctionResponse
  request = delete appSync
  response =
    receiveEmpty
      (\s h x -> DeleteFunctionResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteFunction

instance NFData DeleteFunction

instance ToHeaders DeleteFunction where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DeleteFunction where
  toPath DeleteFunction' {..} =
    mconcat
      ["/v1/apis/", toBS _dfApiId, "/functions/", toBS _dfFunctionId]

instance ToQuery DeleteFunction where
  toQuery = const mempty

-- | /See:/ 'deleteFunctionResponse' smart constructor.
newtype DeleteFunctionResponse = DeleteFunctionResponse'
  { _dfrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteFunctionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfrsResponseStatus' - -- | The response status code.
deleteFunctionResponse ::
  -- | 'dfrsResponseStatus'
  Int ->
  DeleteFunctionResponse
deleteFunctionResponse pResponseStatus_ =
  DeleteFunctionResponse' {_dfrsResponseStatus = pResponseStatus_}

-- | -- | The response status code.
dfrsResponseStatus :: Lens' DeleteFunctionResponse Int
dfrsResponseStatus = lens _dfrsResponseStatus (\s a -> s {_dfrsResponseStatus = a})

instance NFData DeleteFunctionResponse
