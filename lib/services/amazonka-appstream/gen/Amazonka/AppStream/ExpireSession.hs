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
-- Module      : Amazonka.AppStream.ExpireSession
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Immediately stops the specified streaming session.
module Amazonka.AppStream.ExpireSession
  ( -- * Creating a Request
    ExpireSession (..),
    newExpireSession,

    -- * Request Lenses
    expireSession_sessionId,

    -- * Destructuring the Response
    ExpireSessionResponse (..),
    newExpireSessionResponse,

    -- * Response Lenses
    expireSessionResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newExpireSession' smart constructor.
data ExpireSession = ExpireSession'
  { -- | The identifier of the streaming session.
    sessionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExpireSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionId', 'expireSession_sessionId' - The identifier of the streaming session.
newExpireSession ::
  -- | 'sessionId'
  Prelude.Text ->
  ExpireSession
newExpireSession pSessionId_ =
  ExpireSession' {sessionId = pSessionId_}

-- | The identifier of the streaming session.
expireSession_sessionId :: Lens.Lens' ExpireSession Prelude.Text
expireSession_sessionId = Lens.lens (\ExpireSession' {sessionId} -> sessionId) (\s@ExpireSession' {} a -> s {sessionId = a} :: ExpireSession)

instance Core.AWSRequest ExpireSession where
  type
    AWSResponse ExpireSession =
      ExpireSessionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          ExpireSessionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ExpireSession where
  hashWithSalt _salt ExpireSession' {..} =
    _salt `Prelude.hashWithSalt` sessionId

instance Prelude.NFData ExpireSession where
  rnf ExpireSession' {..} = Prelude.rnf sessionId

instance Data.ToHeaders ExpireSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.ExpireSession" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ExpireSession where
  toJSON ExpireSession' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("SessionId" Data..= sessionId)]
      )

instance Data.ToPath ExpireSession where
  toPath = Prelude.const "/"

instance Data.ToQuery ExpireSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newExpireSessionResponse' smart constructor.
data ExpireSessionResponse = ExpireSessionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExpireSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'expireSessionResponse_httpStatus' - The response's http status code.
newExpireSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ExpireSessionResponse
newExpireSessionResponse pHttpStatus_ =
  ExpireSessionResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
expireSessionResponse_httpStatus :: Lens.Lens' ExpireSessionResponse Prelude.Int
expireSessionResponse_httpStatus = Lens.lens (\ExpireSessionResponse' {httpStatus} -> httpStatus) (\s@ExpireSessionResponse' {} a -> s {httpStatus = a} :: ExpireSessionResponse)

instance Prelude.NFData ExpireSessionResponse where
  rnf ExpireSessionResponse' {..} =
    Prelude.rnf httpStatus
