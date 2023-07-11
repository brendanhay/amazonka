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
-- Module      : Amazonka.AMP.DeleteAlertManagerDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an alert manager definition.
module Amazonka.AMP.DeleteAlertManagerDefinition
  ( -- * Creating a Request
    DeleteAlertManagerDefinition (..),
    newDeleteAlertManagerDefinition,

    -- * Request Lenses
    deleteAlertManagerDefinition_clientToken,
    deleteAlertManagerDefinition_workspaceId,

    -- * Destructuring the Response
    DeleteAlertManagerDefinitionResponse (..),
    newDeleteAlertManagerDefinitionResponse,
  )
where

import Amazonka.AMP.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a DeleteAlertManagerDefinition operation.
--
-- /See:/ 'newDeleteAlertManagerDefinition' smart constructor.
data DeleteAlertManagerDefinition = DeleteAlertManagerDefinition'
  { -- | Optional, unique, case-sensitive, user-provided identifier to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the workspace in which to delete the alert manager definition.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAlertManagerDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteAlertManagerDefinition_clientToken' - Optional, unique, case-sensitive, user-provided identifier to ensure the
-- idempotency of the request.
--
-- 'workspaceId', 'deleteAlertManagerDefinition_workspaceId' - The ID of the workspace in which to delete the alert manager definition.
newDeleteAlertManagerDefinition ::
  -- | 'workspaceId'
  Prelude.Text ->
  DeleteAlertManagerDefinition
newDeleteAlertManagerDefinition pWorkspaceId_ =
  DeleteAlertManagerDefinition'
    { clientToken =
        Prelude.Nothing,
      workspaceId = pWorkspaceId_
    }

-- | Optional, unique, case-sensitive, user-provided identifier to ensure the
-- idempotency of the request.
deleteAlertManagerDefinition_clientToken :: Lens.Lens' DeleteAlertManagerDefinition (Prelude.Maybe Prelude.Text)
deleteAlertManagerDefinition_clientToken = Lens.lens (\DeleteAlertManagerDefinition' {clientToken} -> clientToken) (\s@DeleteAlertManagerDefinition' {} a -> s {clientToken = a} :: DeleteAlertManagerDefinition)

-- | The ID of the workspace in which to delete the alert manager definition.
deleteAlertManagerDefinition_workspaceId :: Lens.Lens' DeleteAlertManagerDefinition Prelude.Text
deleteAlertManagerDefinition_workspaceId = Lens.lens (\DeleteAlertManagerDefinition' {workspaceId} -> workspaceId) (\s@DeleteAlertManagerDefinition' {} a -> s {workspaceId = a} :: DeleteAlertManagerDefinition)

instance Core.AWSRequest DeleteAlertManagerDefinition where
  type
    AWSResponse DeleteAlertManagerDefinition =
      DeleteAlertManagerDefinitionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteAlertManagerDefinitionResponse'

instance
  Prelude.Hashable
    DeleteAlertManagerDefinition
  where
  hashWithSalt _salt DeleteAlertManagerDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData DeleteAlertManagerDefinition where
  rnf DeleteAlertManagerDefinition' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf workspaceId

instance Data.ToHeaders DeleteAlertManagerDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteAlertManagerDefinition where
  toPath DeleteAlertManagerDefinition' {..} =
    Prelude.mconcat
      [ "/workspaces/",
        Data.toBS workspaceId,
        "/alertmanager/definition"
      ]

instance Data.ToQuery DeleteAlertManagerDefinition where
  toQuery DeleteAlertManagerDefinition' {..} =
    Prelude.mconcat ["clientToken" Data.=: clientToken]

-- | /See:/ 'newDeleteAlertManagerDefinitionResponse' smart constructor.
data DeleteAlertManagerDefinitionResponse = DeleteAlertManagerDefinitionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAlertManagerDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAlertManagerDefinitionResponse ::
  DeleteAlertManagerDefinitionResponse
newDeleteAlertManagerDefinitionResponse =
  DeleteAlertManagerDefinitionResponse'

instance
  Prelude.NFData
    DeleteAlertManagerDefinitionResponse
  where
  rnf _ = ()
