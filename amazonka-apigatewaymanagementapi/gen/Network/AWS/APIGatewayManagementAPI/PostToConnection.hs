{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGatewayManagementAPI.PostToConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends the provided data to the specified connection.
module Network.AWS.APIGatewayManagementAPI.PostToConnection
  ( -- * Creating a Request
    PostToConnection (..),
    newPostToConnection,

    -- * Request Lenses
    postToConnection_connectionId,
    postToConnection_data,

    -- * Destructuring the Response
    PostToConnectionResponse (..),
    newPostToConnectionResponse,
  )
where

import Network.AWS.APIGatewayManagementAPI.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPostToConnection' smart constructor.
data PostToConnection = PostToConnection'
  { -- | The identifier of the connection that a specific client is using.
    connectionId :: Core.Text,
    -- | The data to be sent to the client specified by its connection id.
    data' :: Core.ByteString
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PostToConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionId', 'postToConnection_connectionId' - The identifier of the connection that a specific client is using.
--
-- 'data'', 'postToConnection_data' - The data to be sent to the client specified by its connection id.
newPostToConnection ::
  -- | 'connectionId'
  Core.Text ->
  -- | 'data''
  Core.ByteString ->
  PostToConnection
newPostToConnection pConnectionId_ pData_ =
  PostToConnection'
    { connectionId = pConnectionId_,
      data' = pData_
    }

-- | The identifier of the connection that a specific client is using.
postToConnection_connectionId :: Lens.Lens' PostToConnection Core.Text
postToConnection_connectionId = Lens.lens (\PostToConnection' {connectionId} -> connectionId) (\s@PostToConnection' {} a -> s {connectionId = a} :: PostToConnection)

-- | The data to be sent to the client specified by its connection id.
postToConnection_data :: Lens.Lens' PostToConnection Core.ByteString
postToConnection_data = Lens.lens (\PostToConnection' {data'} -> data') (\s@PostToConnection' {} a -> s {data' = a} :: PostToConnection)

instance Core.AWSRequest PostToConnection where
  type
    AWSResponse PostToConnection =
      PostToConnectionResponse
  request = Request.postBody defaultService
  response =
    Response.receiveNull PostToConnectionResponse'

instance Core.Hashable PostToConnection

instance Core.NFData PostToConnection

instance Core.ToBody PostToConnection where
  toBody PostToConnection' {..} = Core.toBody data'

instance Core.ToHeaders PostToConnection where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath PostToConnection where
  toPath PostToConnection' {..} =
    Core.mconcat
      ["/@connections/", Core.toBS connectionId]

instance Core.ToQuery PostToConnection where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPostToConnectionResponse' smart constructor.
data PostToConnectionResponse = PostToConnectionResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PostToConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPostToConnectionResponse ::
  PostToConnectionResponse
newPostToConnectionResponse =
  PostToConnectionResponse'

instance Core.NFData PostToConnectionResponse
