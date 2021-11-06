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
-- Module      : Amazonka.Proton.DeleteEnvironmentAccountConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- In an environment account, delete an environment account connection.
--
-- After you delete an environment account connection that’s in use by an
-- AWS Proton environment, AWS Proton /can’t/ manage the environment
-- infrastructure resources until a new environment account connection is
-- accepted for the environment account and associated environment. You\'re
-- responsible for cleaning up provisioned resources that remain without an
-- environment connection.
--
-- For more information, see
-- <https://docs.aws.amazon.com/proton/latest/adminguide/ag-env-account-connections.html Environment account connections>
-- in the /AWS Proton Administrator guide/.
module Amazonka.Proton.DeleteEnvironmentAccountConnection
  ( -- * Creating a Request
    DeleteEnvironmentAccountConnection (..),
    newDeleteEnvironmentAccountConnection,

    -- * Request Lenses
    deleteEnvironmentAccountConnection_id,

    -- * Destructuring the Response
    DeleteEnvironmentAccountConnectionResponse (..),
    newDeleteEnvironmentAccountConnectionResponse,

    -- * Response Lenses
    deleteEnvironmentAccountConnectionResponse_environmentAccountConnection,
    deleteEnvironmentAccountConnectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteEnvironmentAccountConnection' smart constructor.
data DeleteEnvironmentAccountConnection = DeleteEnvironmentAccountConnection'
  { -- | The ID of the environment account connection to delete.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEnvironmentAccountConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteEnvironmentAccountConnection_id' - The ID of the environment account connection to delete.
newDeleteEnvironmentAccountConnection ::
  -- | 'id'
  Prelude.Text ->
  DeleteEnvironmentAccountConnection
newDeleteEnvironmentAccountConnection pId_ =
  DeleteEnvironmentAccountConnection' {id = pId_}

-- | The ID of the environment account connection to delete.
deleteEnvironmentAccountConnection_id :: Lens.Lens' DeleteEnvironmentAccountConnection Prelude.Text
deleteEnvironmentAccountConnection_id = Lens.lens (\DeleteEnvironmentAccountConnection' {id} -> id) (\s@DeleteEnvironmentAccountConnection' {} a -> s {id = a} :: DeleteEnvironmentAccountConnection)

instance
  Core.AWSRequest
    DeleteEnvironmentAccountConnection
  where
  type
    AWSResponse DeleteEnvironmentAccountConnection =
      DeleteEnvironmentAccountConnectionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteEnvironmentAccountConnectionResponse'
            Prelude.<$> (x Core..?> "environmentAccountConnection")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteEnvironmentAccountConnection

instance
  Prelude.NFData
    DeleteEnvironmentAccountConnection

instance
  Core.ToHeaders
    DeleteEnvironmentAccountConnection
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AwsProton20200720.DeleteEnvironmentAccountConnection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    DeleteEnvironmentAccountConnection
  where
  toJSON DeleteEnvironmentAccountConnection' {..} =
    Core.object
      (Prelude.catMaybes [Prelude.Just ("id" Core..= id)])

instance
  Core.ToPath
    DeleteEnvironmentAccountConnection
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DeleteEnvironmentAccountConnection
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteEnvironmentAccountConnectionResponse' smart constructor.
data DeleteEnvironmentAccountConnectionResponse = DeleteEnvironmentAccountConnectionResponse'
  { -- | The environment account connection detail data that\'s returned by AWS
    -- Proton.
    environmentAccountConnection :: Prelude.Maybe EnvironmentAccountConnection,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEnvironmentAccountConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentAccountConnection', 'deleteEnvironmentAccountConnectionResponse_environmentAccountConnection' - The environment account connection detail data that\'s returned by AWS
-- Proton.
--
-- 'httpStatus', 'deleteEnvironmentAccountConnectionResponse_httpStatus' - The response's http status code.
newDeleteEnvironmentAccountConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteEnvironmentAccountConnectionResponse
newDeleteEnvironmentAccountConnectionResponse
  pHttpStatus_ =
    DeleteEnvironmentAccountConnectionResponse'
      { environmentAccountConnection =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The environment account connection detail data that\'s returned by AWS
-- Proton.
deleteEnvironmentAccountConnectionResponse_environmentAccountConnection :: Lens.Lens' DeleteEnvironmentAccountConnectionResponse (Prelude.Maybe EnvironmentAccountConnection)
deleteEnvironmentAccountConnectionResponse_environmentAccountConnection = Lens.lens (\DeleteEnvironmentAccountConnectionResponse' {environmentAccountConnection} -> environmentAccountConnection) (\s@DeleteEnvironmentAccountConnectionResponse' {} a -> s {environmentAccountConnection = a} :: DeleteEnvironmentAccountConnectionResponse)

-- | The response's http status code.
deleteEnvironmentAccountConnectionResponse_httpStatus :: Lens.Lens' DeleteEnvironmentAccountConnectionResponse Prelude.Int
deleteEnvironmentAccountConnectionResponse_httpStatus = Lens.lens (\DeleteEnvironmentAccountConnectionResponse' {httpStatus} -> httpStatus) (\s@DeleteEnvironmentAccountConnectionResponse' {} a -> s {httpStatus = a} :: DeleteEnvironmentAccountConnectionResponse)

instance
  Prelude.NFData
    DeleteEnvironmentAccountConnectionResponse
