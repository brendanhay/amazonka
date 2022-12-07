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
-- Module      : Amazonka.Glue.StopSession
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the session.
module Amazonka.Glue.StopSession
  ( -- * Creating a Request
    StopSession (..),
    newStopSession,

    -- * Request Lenses
    stopSession_requestOrigin,
    stopSession_id,

    -- * Destructuring the Response
    StopSessionResponse (..),
    newStopSessionResponse,

    -- * Response Lenses
    stopSessionResponse_id,
    stopSessionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopSession' smart constructor.
data StopSession = StopSession'
  { -- | The origin of the request.
    requestOrigin :: Prelude.Maybe Prelude.Text,
    -- | The ID of the session to be stopped.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestOrigin', 'stopSession_requestOrigin' - The origin of the request.
--
-- 'id', 'stopSession_id' - The ID of the session to be stopped.
newStopSession ::
  -- | 'id'
  Prelude.Text ->
  StopSession
newStopSession pId_ =
  StopSession'
    { requestOrigin = Prelude.Nothing,
      id = pId_
    }

-- | The origin of the request.
stopSession_requestOrigin :: Lens.Lens' StopSession (Prelude.Maybe Prelude.Text)
stopSession_requestOrigin = Lens.lens (\StopSession' {requestOrigin} -> requestOrigin) (\s@StopSession' {} a -> s {requestOrigin = a} :: StopSession)

-- | The ID of the session to be stopped.
stopSession_id :: Lens.Lens' StopSession Prelude.Text
stopSession_id = Lens.lens (\StopSession' {id} -> id) (\s@StopSession' {} a -> s {id = a} :: StopSession)

instance Core.AWSRequest StopSession where
  type AWSResponse StopSession = StopSessionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopSessionResponse'
            Prelude.<$> (x Data..?> "Id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopSession where
  hashWithSalt _salt StopSession' {..} =
    _salt `Prelude.hashWithSalt` requestOrigin
      `Prelude.hashWithSalt` id

instance Prelude.NFData StopSession where
  rnf StopSession' {..} =
    Prelude.rnf requestOrigin
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders StopSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.StopSession" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopSession where
  toJSON StopSession' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RequestOrigin" Data..=) Prelude.<$> requestOrigin,
            Prelude.Just ("Id" Data..= id)
          ]
      )

instance Data.ToPath StopSession where
  toPath = Prelude.const "/"

instance Data.ToQuery StopSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopSessionResponse' smart constructor.
data StopSessionResponse = StopSessionResponse'
  { -- | Returns the Id of the stopped session.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'stopSessionResponse_id' - Returns the Id of the stopped session.
--
-- 'httpStatus', 'stopSessionResponse_httpStatus' - The response's http status code.
newStopSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopSessionResponse
newStopSessionResponse pHttpStatus_ =
  StopSessionResponse'
    { id = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns the Id of the stopped session.
stopSessionResponse_id :: Lens.Lens' StopSessionResponse (Prelude.Maybe Prelude.Text)
stopSessionResponse_id = Lens.lens (\StopSessionResponse' {id} -> id) (\s@StopSessionResponse' {} a -> s {id = a} :: StopSessionResponse)

-- | The response's http status code.
stopSessionResponse_httpStatus :: Lens.Lens' StopSessionResponse Prelude.Int
stopSessionResponse_httpStatus = Lens.lens (\StopSessionResponse' {httpStatus} -> httpStatus) (\s@StopSessionResponse' {} a -> s {httpStatus = a} :: StopSessionResponse)

instance Prelude.NFData StopSessionResponse where
  rnf StopSessionResponse' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf httpStatus
