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
-- Module      : Amazonka.SageMaker.DeleteWorkteam
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing work team. This operation can\'t be undone.
module Amazonka.SageMaker.DeleteWorkteam
  ( -- * Creating a Request
    DeleteWorkteam (..),
    newDeleteWorkteam,

    -- * Request Lenses
    deleteWorkteam_workteamName,

    -- * Destructuring the Response
    DeleteWorkteamResponse (..),
    newDeleteWorkteamResponse,

    -- * Response Lenses
    deleteWorkteamResponse_httpStatus,
    deleteWorkteamResponse_success,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeleteWorkteam' smart constructor.
data DeleteWorkteam = DeleteWorkteam'
  { -- | The name of the work team to delete.
    workteamName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorkteam' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workteamName', 'deleteWorkteam_workteamName' - The name of the work team to delete.
newDeleteWorkteam ::
  -- | 'workteamName'
  Prelude.Text ->
  DeleteWorkteam
newDeleteWorkteam pWorkteamName_ =
  DeleteWorkteam' {workteamName = pWorkteamName_}

-- | The name of the work team to delete.
deleteWorkteam_workteamName :: Lens.Lens' DeleteWorkteam Prelude.Text
deleteWorkteam_workteamName = Lens.lens (\DeleteWorkteam' {workteamName} -> workteamName) (\s@DeleteWorkteam' {} a -> s {workteamName = a} :: DeleteWorkteam)

instance Core.AWSRequest DeleteWorkteam where
  type
    AWSResponse DeleteWorkteam =
      DeleteWorkteamResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteWorkteamResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Success")
      )

instance Prelude.Hashable DeleteWorkteam where
  hashWithSalt _salt DeleteWorkteam' {..} =
    _salt `Prelude.hashWithSalt` workteamName

instance Prelude.NFData DeleteWorkteam where
  rnf DeleteWorkteam' {..} = Prelude.rnf workteamName

instance Data.ToHeaders DeleteWorkteam where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.DeleteWorkteam" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteWorkteam where
  toJSON DeleteWorkteam' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("WorkteamName" Data..= workteamName)]
      )

instance Data.ToPath DeleteWorkteam where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteWorkteam where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteWorkteamResponse' smart constructor.
data DeleteWorkteamResponse = DeleteWorkteamResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Returns @true@ if the work team was successfully deleted; otherwise,
    -- returns @false@.
    success :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorkteamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteWorkteamResponse_httpStatus' - The response's http status code.
--
-- 'success', 'deleteWorkteamResponse_success' - Returns @true@ if the work team was successfully deleted; otherwise,
-- returns @false@.
newDeleteWorkteamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'success'
  Prelude.Bool ->
  DeleteWorkteamResponse
newDeleteWorkteamResponse pHttpStatus_ pSuccess_ =
  DeleteWorkteamResponse'
    { httpStatus = pHttpStatus_,
      success = pSuccess_
    }

-- | The response's http status code.
deleteWorkteamResponse_httpStatus :: Lens.Lens' DeleteWorkteamResponse Prelude.Int
deleteWorkteamResponse_httpStatus = Lens.lens (\DeleteWorkteamResponse' {httpStatus} -> httpStatus) (\s@DeleteWorkteamResponse' {} a -> s {httpStatus = a} :: DeleteWorkteamResponse)

-- | Returns @true@ if the work team was successfully deleted; otherwise,
-- returns @false@.
deleteWorkteamResponse_success :: Lens.Lens' DeleteWorkteamResponse Prelude.Bool
deleteWorkteamResponse_success = Lens.lens (\DeleteWorkteamResponse' {success} -> success) (\s@DeleteWorkteamResponse' {} a -> s {success = a} :: DeleteWorkteamResponse)

instance Prelude.NFData DeleteWorkteamResponse where
  rnf DeleteWorkteamResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf success
