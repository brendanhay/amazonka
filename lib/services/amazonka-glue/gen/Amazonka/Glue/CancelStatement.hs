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
-- Module      : Amazonka.Glue.CancelStatement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the statement.
module Amazonka.Glue.CancelStatement
  ( -- * Creating a Request
    CancelStatement (..),
    newCancelStatement,

    -- * Request Lenses
    cancelStatement_requestOrigin,
    cancelStatement_sessionId,
    cancelStatement_id,

    -- * Destructuring the Response
    CancelStatementResponse (..),
    newCancelStatementResponse,

    -- * Response Lenses
    cancelStatementResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelStatement' smart constructor.
data CancelStatement = CancelStatement'
  { -- | The origin of the request to cancel the statement.
    requestOrigin :: Prelude.Maybe Prelude.Text,
    -- | The Session ID of the statement to be cancelled.
    sessionId :: Prelude.Text,
    -- | The ID of the statement to be cancelled.
    id :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestOrigin', 'cancelStatement_requestOrigin' - The origin of the request to cancel the statement.
--
-- 'sessionId', 'cancelStatement_sessionId' - The Session ID of the statement to be cancelled.
--
-- 'id', 'cancelStatement_id' - The ID of the statement to be cancelled.
newCancelStatement ::
  -- | 'sessionId'
  Prelude.Text ->
  -- | 'id'
  Prelude.Int ->
  CancelStatement
newCancelStatement pSessionId_ pId_ =
  CancelStatement'
    { requestOrigin = Prelude.Nothing,
      sessionId = pSessionId_,
      id = pId_
    }

-- | The origin of the request to cancel the statement.
cancelStatement_requestOrigin :: Lens.Lens' CancelStatement (Prelude.Maybe Prelude.Text)
cancelStatement_requestOrigin = Lens.lens (\CancelStatement' {requestOrigin} -> requestOrigin) (\s@CancelStatement' {} a -> s {requestOrigin = a} :: CancelStatement)

-- | The Session ID of the statement to be cancelled.
cancelStatement_sessionId :: Lens.Lens' CancelStatement Prelude.Text
cancelStatement_sessionId = Lens.lens (\CancelStatement' {sessionId} -> sessionId) (\s@CancelStatement' {} a -> s {sessionId = a} :: CancelStatement)

-- | The ID of the statement to be cancelled.
cancelStatement_id :: Lens.Lens' CancelStatement Prelude.Int
cancelStatement_id = Lens.lens (\CancelStatement' {id} -> id) (\s@CancelStatement' {} a -> s {id = a} :: CancelStatement)

instance Core.AWSRequest CancelStatement where
  type
    AWSResponse CancelStatement =
      CancelStatementResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelStatementResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelStatement where
  hashWithSalt _salt CancelStatement' {..} =
    _salt `Prelude.hashWithSalt` requestOrigin
      `Prelude.hashWithSalt` sessionId
      `Prelude.hashWithSalt` id

instance Prelude.NFData CancelStatement where
  rnf CancelStatement' {..} =
    Prelude.rnf requestOrigin
      `Prelude.seq` Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf id

instance Core.ToHeaders CancelStatement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.CancelStatement" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CancelStatement where
  toJSON CancelStatement' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RequestOrigin" Core..=) Prelude.<$> requestOrigin,
            Prelude.Just ("SessionId" Core..= sessionId),
            Prelude.Just ("Id" Core..= id)
          ]
      )

instance Core.ToPath CancelStatement where
  toPath = Prelude.const "/"

instance Core.ToQuery CancelStatement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelStatementResponse' smart constructor.
data CancelStatementResponse = CancelStatementResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelStatementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelStatementResponse_httpStatus' - The response's http status code.
newCancelStatementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelStatementResponse
newCancelStatementResponse pHttpStatus_ =
  CancelStatementResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
cancelStatementResponse_httpStatus :: Lens.Lens' CancelStatementResponse Prelude.Int
cancelStatementResponse_httpStatus = Lens.lens (\CancelStatementResponse' {httpStatus} -> httpStatus) (\s@CancelStatementResponse' {} a -> s {httpStatus = a} :: CancelStatementResponse)

instance Prelude.NFData CancelStatementResponse where
  rnf CancelStatementResponse' {..} =
    Prelude.rnf httpStatus
