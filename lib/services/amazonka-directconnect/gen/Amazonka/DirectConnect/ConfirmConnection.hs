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
-- Module      : Amazonka.DirectConnect.ConfirmConnection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Confirms the creation of the specified hosted connection on an
-- interconnect.
--
-- Upon creation, the hosted connection is initially in the @Ordering@
-- state, and remains in this state until the owner confirms creation of
-- the hosted connection.
module Amazonka.DirectConnect.ConfirmConnection
  ( -- * Creating a Request
    ConfirmConnection (..),
    newConfirmConnection,

    -- * Request Lenses
    confirmConnection_connectionId,

    -- * Destructuring the Response
    ConfirmConnectionResponse (..),
    newConfirmConnectionResponse,

    -- * Response Lenses
    confirmConnectionResponse_connectionState,
    confirmConnectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newConfirmConnection' smart constructor.
data ConfirmConnection = ConfirmConnection'
  { -- | The ID of the hosted connection.
    connectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfirmConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionId', 'confirmConnection_connectionId' - The ID of the hosted connection.
newConfirmConnection ::
  -- | 'connectionId'
  Prelude.Text ->
  ConfirmConnection
newConfirmConnection pConnectionId_ =
  ConfirmConnection' {connectionId = pConnectionId_}

-- | The ID of the hosted connection.
confirmConnection_connectionId :: Lens.Lens' ConfirmConnection Prelude.Text
confirmConnection_connectionId = Lens.lens (\ConfirmConnection' {connectionId} -> connectionId) (\s@ConfirmConnection' {} a -> s {connectionId = a} :: ConfirmConnection)

instance Core.AWSRequest ConfirmConnection where
  type
    AWSResponse ConfirmConnection =
      ConfirmConnectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ConfirmConnectionResponse'
            Prelude.<$> (x Core..?> "connectionState")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ConfirmConnection where
  hashWithSalt _salt ConfirmConnection' {..} =
    _salt `Prelude.hashWithSalt` connectionId

instance Prelude.NFData ConfirmConnection where
  rnf ConfirmConnection' {..} = Prelude.rnf connectionId

instance Core.ToHeaders ConfirmConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.ConfirmConnection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ConfirmConnection where
  toJSON ConfirmConnection' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("connectionId" Core..= connectionId)]
      )

instance Core.ToPath ConfirmConnection where
  toPath = Prelude.const "/"

instance Core.ToQuery ConfirmConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newConfirmConnectionResponse' smart constructor.
data ConfirmConnectionResponse = ConfirmConnectionResponse'
  { -- | The state of the connection. The following are the possible values:
    --
    -- -   @ordering@: The initial state of a hosted connection provisioned on
    --     an interconnect. The connection stays in the ordering state until
    --     the owner of the hosted connection confirms or declines the
    --     connection order.
    --
    -- -   @requested@: The initial state of a standard connection. The
    --     connection stays in the requested state until the Letter of
    --     Authorization (LOA) is sent to the customer.
    --
    -- -   @pending@: The connection has been approved and is being
    --     initialized.
    --
    -- -   @available@: The network link is up and the connection is ready for
    --     use.
    --
    -- -   @down@: The network link is down.
    --
    -- -   @deleting@: The connection is being deleted.
    --
    -- -   @deleted@: The connection has been deleted.
    --
    -- -   @rejected@: A hosted connection in the @ordering@ state enters the
    --     @rejected@ state if it is deleted by the customer.
    --
    -- -   @unknown@: The state of the connection is not available.
    connectionState :: Prelude.Maybe ConnectionState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfirmConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionState', 'confirmConnectionResponse_connectionState' - The state of the connection. The following are the possible values:
--
-- -   @ordering@: The initial state of a hosted connection provisioned on
--     an interconnect. The connection stays in the ordering state until
--     the owner of the hosted connection confirms or declines the
--     connection order.
--
-- -   @requested@: The initial state of a standard connection. The
--     connection stays in the requested state until the Letter of
--     Authorization (LOA) is sent to the customer.
--
-- -   @pending@: The connection has been approved and is being
--     initialized.
--
-- -   @available@: The network link is up and the connection is ready for
--     use.
--
-- -   @down@: The network link is down.
--
-- -   @deleting@: The connection is being deleted.
--
-- -   @deleted@: The connection has been deleted.
--
-- -   @rejected@: A hosted connection in the @ordering@ state enters the
--     @rejected@ state if it is deleted by the customer.
--
-- -   @unknown@: The state of the connection is not available.
--
-- 'httpStatus', 'confirmConnectionResponse_httpStatus' - The response's http status code.
newConfirmConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ConfirmConnectionResponse
newConfirmConnectionResponse pHttpStatus_ =
  ConfirmConnectionResponse'
    { connectionState =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The state of the connection. The following are the possible values:
--
-- -   @ordering@: The initial state of a hosted connection provisioned on
--     an interconnect. The connection stays in the ordering state until
--     the owner of the hosted connection confirms or declines the
--     connection order.
--
-- -   @requested@: The initial state of a standard connection. The
--     connection stays in the requested state until the Letter of
--     Authorization (LOA) is sent to the customer.
--
-- -   @pending@: The connection has been approved and is being
--     initialized.
--
-- -   @available@: The network link is up and the connection is ready for
--     use.
--
-- -   @down@: The network link is down.
--
-- -   @deleting@: The connection is being deleted.
--
-- -   @deleted@: The connection has been deleted.
--
-- -   @rejected@: A hosted connection in the @ordering@ state enters the
--     @rejected@ state if it is deleted by the customer.
--
-- -   @unknown@: The state of the connection is not available.
confirmConnectionResponse_connectionState :: Lens.Lens' ConfirmConnectionResponse (Prelude.Maybe ConnectionState)
confirmConnectionResponse_connectionState = Lens.lens (\ConfirmConnectionResponse' {connectionState} -> connectionState) (\s@ConfirmConnectionResponse' {} a -> s {connectionState = a} :: ConfirmConnectionResponse)

-- | The response's http status code.
confirmConnectionResponse_httpStatus :: Lens.Lens' ConfirmConnectionResponse Prelude.Int
confirmConnectionResponse_httpStatus = Lens.lens (\ConfirmConnectionResponse' {httpStatus} -> httpStatus) (\s@ConfirmConnectionResponse' {} a -> s {httpStatus = a} :: ConfirmConnectionResponse)

instance Prelude.NFData ConfirmConnectionResponse where
  rnf ConfirmConnectionResponse' {..} =
    Prelude.rnf connectionState
      `Prelude.seq` Prelude.rnf httpStatus
