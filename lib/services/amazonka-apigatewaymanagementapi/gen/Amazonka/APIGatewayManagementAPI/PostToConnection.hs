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
-- Module      : Amazonka.APIGatewayManagementAPI.PostToConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends the provided data to the specified connection.
module Amazonka.APIGatewayManagementAPI.PostToConnection
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

import Amazonka.APIGatewayManagementAPI.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPostToConnection' smart constructor.
data PostToConnection = PostToConnection'
  { -- | The identifier of the connection that a specific client is using.
    connectionId :: Prelude.Text,
    -- | The data to be sent to the client specified by its connection id.
    data' :: Prelude.ByteString
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'data''
  Prelude.ByteString ->
  PostToConnection
newPostToConnection pConnectionId_ pData_ =
  PostToConnection'
    { connectionId = pConnectionId_,
      data' = pData_
    }

-- | The identifier of the connection that a specific client is using.
postToConnection_connectionId :: Lens.Lens' PostToConnection Prelude.Text
postToConnection_connectionId = Lens.lens (\PostToConnection' {connectionId} -> connectionId) (\s@PostToConnection' {} a -> s {connectionId = a} :: PostToConnection)

-- | The data to be sent to the client specified by its connection id.
postToConnection_data :: Lens.Lens' PostToConnection Prelude.ByteString
postToConnection_data = Lens.lens (\PostToConnection' {data'} -> data') (\s@PostToConnection' {} a -> s {data' = a} :: PostToConnection)

instance Core.AWSRequest PostToConnection where
  type
    AWSResponse PostToConnection =
      PostToConnectionResponse
  request overrides =
    Request.postBody (overrides defaultService)
  response =
    Response.receiveNull PostToConnectionResponse'

instance Prelude.Hashable PostToConnection where
  hashWithSalt _salt PostToConnection' {..} =
    _salt
      `Prelude.hashWithSalt` connectionId
      `Prelude.hashWithSalt` data'

instance Prelude.NFData PostToConnection where
  rnf PostToConnection' {..} =
    Prelude.rnf connectionId
      `Prelude.seq` Prelude.rnf data'

instance Data.ToBody PostToConnection where
  toBody PostToConnection' {..} = Data.toBody data'

instance Data.ToHeaders PostToConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath PostToConnection where
  toPath PostToConnection' {..} =
    Prelude.mconcat
      ["/@connections/", Data.toBS connectionId]

instance Data.ToQuery PostToConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPostToConnectionResponse' smart constructor.
data PostToConnectionResponse = PostToConnectionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PostToConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPostToConnectionResponse ::
  PostToConnectionResponse
newPostToConnectionResponse =
  PostToConnectionResponse'

instance Prelude.NFData PostToConnectionResponse where
  rnf _ = ()
