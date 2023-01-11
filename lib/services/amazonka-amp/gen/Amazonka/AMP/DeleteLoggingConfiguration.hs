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
-- Module      : Amazonka.AMP.DeleteLoggingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete logging configuration.
module Amazonka.AMP.DeleteLoggingConfiguration
  ( -- * Creating a Request
    DeleteLoggingConfiguration (..),
    newDeleteLoggingConfiguration,

    -- * Request Lenses
    deleteLoggingConfiguration_clientToken,
    deleteLoggingConfiguration_workspaceId,

    -- * Destructuring the Response
    DeleteLoggingConfigurationResponse (..),
    newDeleteLoggingConfigurationResponse,
  )
where

import Amazonka.AMP.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a DeleteLoggingConfiguration operation.
--
-- /See:/ 'newDeleteLoggingConfiguration' smart constructor.
data DeleteLoggingConfiguration = DeleteLoggingConfiguration'
  { -- | Optional, unique, case-sensitive, user-provided identifier to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the workspace to vend logs to.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLoggingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteLoggingConfiguration_clientToken' - Optional, unique, case-sensitive, user-provided identifier to ensure the
-- idempotency of the request.
--
-- 'workspaceId', 'deleteLoggingConfiguration_workspaceId' - The ID of the workspace to vend logs to.
newDeleteLoggingConfiguration ::
  -- | 'workspaceId'
  Prelude.Text ->
  DeleteLoggingConfiguration
newDeleteLoggingConfiguration pWorkspaceId_ =
  DeleteLoggingConfiguration'
    { clientToken =
        Prelude.Nothing,
      workspaceId = pWorkspaceId_
    }

-- | Optional, unique, case-sensitive, user-provided identifier to ensure the
-- idempotency of the request.
deleteLoggingConfiguration_clientToken :: Lens.Lens' DeleteLoggingConfiguration (Prelude.Maybe Prelude.Text)
deleteLoggingConfiguration_clientToken = Lens.lens (\DeleteLoggingConfiguration' {clientToken} -> clientToken) (\s@DeleteLoggingConfiguration' {} a -> s {clientToken = a} :: DeleteLoggingConfiguration)

-- | The ID of the workspace to vend logs to.
deleteLoggingConfiguration_workspaceId :: Lens.Lens' DeleteLoggingConfiguration Prelude.Text
deleteLoggingConfiguration_workspaceId = Lens.lens (\DeleteLoggingConfiguration' {workspaceId} -> workspaceId) (\s@DeleteLoggingConfiguration' {} a -> s {workspaceId = a} :: DeleteLoggingConfiguration)

instance Core.AWSRequest DeleteLoggingConfiguration where
  type
    AWSResponse DeleteLoggingConfiguration =
      DeleteLoggingConfigurationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteLoggingConfigurationResponse'

instance Prelude.Hashable DeleteLoggingConfiguration where
  hashWithSalt _salt DeleteLoggingConfiguration' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData DeleteLoggingConfiguration where
  rnf DeleteLoggingConfiguration' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf workspaceId

instance Data.ToHeaders DeleteLoggingConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteLoggingConfiguration where
  toPath DeleteLoggingConfiguration' {..} =
    Prelude.mconcat
      ["/workspaces/", Data.toBS workspaceId, "/logging"]

instance Data.ToQuery DeleteLoggingConfiguration where
  toQuery DeleteLoggingConfiguration' {..} =
    Prelude.mconcat ["clientToken" Data.=: clientToken]

-- | /See:/ 'newDeleteLoggingConfigurationResponse' smart constructor.
data DeleteLoggingConfigurationResponse = DeleteLoggingConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLoggingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteLoggingConfigurationResponse ::
  DeleteLoggingConfigurationResponse
newDeleteLoggingConfigurationResponse =
  DeleteLoggingConfigurationResponse'

instance
  Prelude.NFData
    DeleteLoggingConfigurationResponse
  where
  rnf _ = ()
