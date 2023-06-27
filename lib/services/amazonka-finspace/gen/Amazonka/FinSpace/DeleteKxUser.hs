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
-- Module      : Amazonka.FinSpace.DeleteKxUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user in the specified kdb environment.
module Amazonka.FinSpace.DeleteKxUser
  ( -- * Creating a Request
    DeleteKxUser (..),
    newDeleteKxUser,

    -- * Request Lenses
    deleteKxUser_userName,
    deleteKxUser_environmentId,

    -- * Destructuring the Response
    DeleteKxUserResponse (..),
    newDeleteKxUserResponse,

    -- * Response Lenses
    deleteKxUserResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteKxUser' smart constructor.
data DeleteKxUser = DeleteKxUser'
  { -- | A unique identifier for the user that you want to delete.
    userName :: Prelude.Text,
    -- | A unique identifier for the kdb environment.
    environmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteKxUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'deleteKxUser_userName' - A unique identifier for the user that you want to delete.
--
-- 'environmentId', 'deleteKxUser_environmentId' - A unique identifier for the kdb environment.
newDeleteKxUser ::
  -- | 'userName'
  Prelude.Text ->
  -- | 'environmentId'
  Prelude.Text ->
  DeleteKxUser
newDeleteKxUser pUserName_ pEnvironmentId_ =
  DeleteKxUser'
    { userName = pUserName_,
      environmentId = pEnvironmentId_
    }

-- | A unique identifier for the user that you want to delete.
deleteKxUser_userName :: Lens.Lens' DeleteKxUser Prelude.Text
deleteKxUser_userName = Lens.lens (\DeleteKxUser' {userName} -> userName) (\s@DeleteKxUser' {} a -> s {userName = a} :: DeleteKxUser)

-- | A unique identifier for the kdb environment.
deleteKxUser_environmentId :: Lens.Lens' DeleteKxUser Prelude.Text
deleteKxUser_environmentId = Lens.lens (\DeleteKxUser' {environmentId} -> environmentId) (\s@DeleteKxUser' {} a -> s {environmentId = a} :: DeleteKxUser)

instance Core.AWSRequest DeleteKxUser where
  type AWSResponse DeleteKxUser = DeleteKxUserResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteKxUserResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteKxUser where
  hashWithSalt _salt DeleteKxUser' {..} =
    _salt
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` environmentId

instance Prelude.NFData DeleteKxUser where
  rnf DeleteKxUser' {..} =
    Prelude.rnf userName
      `Prelude.seq` Prelude.rnf environmentId

instance Data.ToHeaders DeleteKxUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteKxUser where
  toPath DeleteKxUser' {..} =
    Prelude.mconcat
      [ "/kx/environments/",
        Data.toBS environmentId,
        "/users/",
        Data.toBS userName
      ]

instance Data.ToQuery DeleteKxUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteKxUserResponse' smart constructor.
data DeleteKxUserResponse = DeleteKxUserResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteKxUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteKxUserResponse_httpStatus' - The response's http status code.
newDeleteKxUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteKxUserResponse
newDeleteKxUserResponse pHttpStatus_ =
  DeleteKxUserResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteKxUserResponse_httpStatus :: Lens.Lens' DeleteKxUserResponse Prelude.Int
deleteKxUserResponse_httpStatus = Lens.lens (\DeleteKxUserResponse' {httpStatus} -> httpStatus) (\s@DeleteKxUserResponse' {} a -> s {httpStatus = a} :: DeleteKxUserResponse)

instance Prelude.NFData DeleteKxUserResponse where
  rnf DeleteKxUserResponse' {..} =
    Prelude.rnf httpStatus
