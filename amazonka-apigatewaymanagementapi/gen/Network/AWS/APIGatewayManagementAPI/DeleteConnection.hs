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
-- Module      : Network.AWS.APIGatewayManagementAPI.DeleteConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete the connection with the provided id.
module Network.AWS.APIGatewayManagementAPI.DeleteConnection
  ( -- * Creating a Request
    DeleteConnection (..),
    newDeleteConnection,

    -- * Request Lenses
    deleteConnection_connectionId,

    -- * Destructuring the Response
    DeleteConnectionResponse (..),
    newDeleteConnectionResponse,
  )
where

import Network.AWS.APIGatewayManagementAPI.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteConnection' smart constructor.
data DeleteConnection = DeleteConnection'
  { connectionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionId', 'deleteConnection_connectionId' - Undocumented member.
newDeleteConnection ::
  -- | 'connectionId'
  Core.Text ->
  DeleteConnection
newDeleteConnection pConnectionId_ =
  DeleteConnection' {connectionId = pConnectionId_}

-- | Undocumented member.
deleteConnection_connectionId :: Lens.Lens' DeleteConnection Core.Text
deleteConnection_connectionId = Lens.lens (\DeleteConnection' {connectionId} -> connectionId) (\s@DeleteConnection' {} a -> s {connectionId = a} :: DeleteConnection)

instance Core.AWSRequest DeleteConnection where
  type
    AWSResponse DeleteConnection =
      DeleteConnectionResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteConnectionResponse'

instance Core.Hashable DeleteConnection

instance Core.NFData DeleteConnection

instance Core.ToHeaders DeleteConnection where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteConnection where
  toPath DeleteConnection' {..} =
    Core.mconcat
      ["/@connections/", Core.toBS connectionId]

instance Core.ToQuery DeleteConnection where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteConnectionResponse' smart constructor.
data DeleteConnectionResponse = DeleteConnectionResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteConnectionResponse ::
  DeleteConnectionResponse
newDeleteConnectionResponse =
  DeleteConnectionResponse'

instance Core.NFData DeleteConnectionResponse
