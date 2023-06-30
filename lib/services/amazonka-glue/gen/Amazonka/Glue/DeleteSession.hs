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
-- Module      : Amazonka.Glue.DeleteSession
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the session.
module Amazonka.Glue.DeleteSession
  ( -- * Creating a Request
    DeleteSession (..),
    newDeleteSession,

    -- * Request Lenses
    deleteSession_requestOrigin,
    deleteSession_id,

    -- * Destructuring the Response
    DeleteSessionResponse (..),
    newDeleteSessionResponse,

    -- * Response Lenses
    deleteSessionResponse_id,
    deleteSessionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSession' smart constructor.
data DeleteSession = DeleteSession'
  { -- | The name of the origin of the delete session request.
    requestOrigin :: Prelude.Maybe Prelude.Text,
    -- | The ID of the session to be deleted.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestOrigin', 'deleteSession_requestOrigin' - The name of the origin of the delete session request.
--
-- 'id', 'deleteSession_id' - The ID of the session to be deleted.
newDeleteSession ::
  -- | 'id'
  Prelude.Text ->
  DeleteSession
newDeleteSession pId_ =
  DeleteSession'
    { requestOrigin = Prelude.Nothing,
      id = pId_
    }

-- | The name of the origin of the delete session request.
deleteSession_requestOrigin :: Lens.Lens' DeleteSession (Prelude.Maybe Prelude.Text)
deleteSession_requestOrigin = Lens.lens (\DeleteSession' {requestOrigin} -> requestOrigin) (\s@DeleteSession' {} a -> s {requestOrigin = a} :: DeleteSession)

-- | The ID of the session to be deleted.
deleteSession_id :: Lens.Lens' DeleteSession Prelude.Text
deleteSession_id = Lens.lens (\DeleteSession' {id} -> id) (\s@DeleteSession' {} a -> s {id = a} :: DeleteSession)

instance Core.AWSRequest DeleteSession where
  type
    AWSResponse DeleteSession =
      DeleteSessionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSessionResponse'
            Prelude.<$> (x Data..?> "Id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSession where
  hashWithSalt _salt DeleteSession' {..} =
    _salt
      `Prelude.hashWithSalt` requestOrigin
      `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteSession where
  rnf DeleteSession' {..} =
    Prelude.rnf requestOrigin
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders DeleteSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.DeleteSession" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteSession where
  toJSON DeleteSession' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RequestOrigin" Data..=) Prelude.<$> requestOrigin,
            Prelude.Just ("Id" Data..= id)
          ]
      )

instance Data.ToPath DeleteSession where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSessionResponse' smart constructor.
data DeleteSessionResponse = DeleteSessionResponse'
  { -- | Returns the ID of the deleted session.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteSessionResponse_id' - Returns the ID of the deleted session.
--
-- 'httpStatus', 'deleteSessionResponse_httpStatus' - The response's http status code.
newDeleteSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSessionResponse
newDeleteSessionResponse pHttpStatus_ =
  DeleteSessionResponse'
    { id = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns the ID of the deleted session.
deleteSessionResponse_id :: Lens.Lens' DeleteSessionResponse (Prelude.Maybe Prelude.Text)
deleteSessionResponse_id = Lens.lens (\DeleteSessionResponse' {id} -> id) (\s@DeleteSessionResponse' {} a -> s {id = a} :: DeleteSessionResponse)

-- | The response's http status code.
deleteSessionResponse_httpStatus :: Lens.Lens' DeleteSessionResponse Prelude.Int
deleteSessionResponse_httpStatus = Lens.lens (\DeleteSessionResponse' {httpStatus} -> httpStatus) (\s@DeleteSessionResponse' {} a -> s {httpStatus = a} :: DeleteSessionResponse)

instance Prelude.NFData DeleteSessionResponse where
  rnf DeleteSessionResponse' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf httpStatus
