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
-- Module      : Amazonka.FinSpace.DeleteKxEnvironment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the kdb environment. This action is irreversible. Deleting a kdb
-- environment will remove all the associated data and any services running
-- in it.
module Amazonka.FinSpace.DeleteKxEnvironment
  ( -- * Creating a Request
    DeleteKxEnvironment (..),
    newDeleteKxEnvironment,

    -- * Request Lenses
    deleteKxEnvironment_environmentId,

    -- * Destructuring the Response
    DeleteKxEnvironmentResponse (..),
    newDeleteKxEnvironmentResponse,

    -- * Response Lenses
    deleteKxEnvironmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteKxEnvironment' smart constructor.
data DeleteKxEnvironment = DeleteKxEnvironment'
  { -- | A unique identifier for the kdb environment.
    environmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteKxEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentId', 'deleteKxEnvironment_environmentId' - A unique identifier for the kdb environment.
newDeleteKxEnvironment ::
  -- | 'environmentId'
  Prelude.Text ->
  DeleteKxEnvironment
newDeleteKxEnvironment pEnvironmentId_ =
  DeleteKxEnvironment'
    { environmentId =
        pEnvironmentId_
    }

-- | A unique identifier for the kdb environment.
deleteKxEnvironment_environmentId :: Lens.Lens' DeleteKxEnvironment Prelude.Text
deleteKxEnvironment_environmentId = Lens.lens (\DeleteKxEnvironment' {environmentId} -> environmentId) (\s@DeleteKxEnvironment' {} a -> s {environmentId = a} :: DeleteKxEnvironment)

instance Core.AWSRequest DeleteKxEnvironment where
  type
    AWSResponse DeleteKxEnvironment =
      DeleteKxEnvironmentResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteKxEnvironmentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteKxEnvironment where
  hashWithSalt _salt DeleteKxEnvironment' {..} =
    _salt `Prelude.hashWithSalt` environmentId

instance Prelude.NFData DeleteKxEnvironment where
  rnf DeleteKxEnvironment' {..} =
    Prelude.rnf environmentId

instance Data.ToHeaders DeleteKxEnvironment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteKxEnvironment where
  toPath DeleteKxEnvironment' {..} =
    Prelude.mconcat
      ["/kx/environments/", Data.toBS environmentId]

instance Data.ToQuery DeleteKxEnvironment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteKxEnvironmentResponse' smart constructor.
data DeleteKxEnvironmentResponse = DeleteKxEnvironmentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteKxEnvironmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteKxEnvironmentResponse_httpStatus' - The response's http status code.
newDeleteKxEnvironmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteKxEnvironmentResponse
newDeleteKxEnvironmentResponse pHttpStatus_ =
  DeleteKxEnvironmentResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteKxEnvironmentResponse_httpStatus :: Lens.Lens' DeleteKxEnvironmentResponse Prelude.Int
deleteKxEnvironmentResponse_httpStatus = Lens.lens (\DeleteKxEnvironmentResponse' {httpStatus} -> httpStatus) (\s@DeleteKxEnvironmentResponse' {} a -> s {httpStatus = a} :: DeleteKxEnvironmentResponse)

instance Prelude.NFData DeleteKxEnvironmentResponse where
  rnf DeleteKxEnvironmentResponse' {..} =
    Prelude.rnf httpStatus
