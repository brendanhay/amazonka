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
-- Module      : Amazonka.APIGatewayManagementAPI.DeleteConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete the connection with the provided id.
module Amazonka.APIGatewayManagementAPI.DeleteConnection
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

import Amazonka.APIGatewayManagementAPI.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteConnection' smart constructor.
data DeleteConnection = DeleteConnection'
  { connectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteConnection
newDeleteConnection pConnectionId_ =
  DeleteConnection' {connectionId = pConnectionId_}

-- | Undocumented member.
deleteConnection_connectionId :: Lens.Lens' DeleteConnection Prelude.Text
deleteConnection_connectionId = Lens.lens (\DeleteConnection' {connectionId} -> connectionId) (\s@DeleteConnection' {} a -> s {connectionId = a} :: DeleteConnection)

instance Core.AWSRequest DeleteConnection where
  type
    AWSResponse DeleteConnection =
      DeleteConnectionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteConnectionResponse'

instance Prelude.Hashable DeleteConnection where
  hashWithSalt _salt DeleteConnection' {..} =
    _salt `Prelude.hashWithSalt` connectionId

instance Prelude.NFData DeleteConnection where
  rnf DeleteConnection' {..} = Prelude.rnf connectionId

instance Data.ToHeaders DeleteConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteConnection where
  toPath DeleteConnection' {..} =
    Prelude.mconcat
      ["/@connections/", Data.toBS connectionId]

instance Data.ToQuery DeleteConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteConnectionResponse' smart constructor.
data DeleteConnectionResponse = DeleteConnectionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteConnectionResponse ::
  DeleteConnectionResponse
newDeleteConnectionResponse =
  DeleteConnectionResponse'

instance Prelude.NFData DeleteConnectionResponse where
  rnf _ = ()
