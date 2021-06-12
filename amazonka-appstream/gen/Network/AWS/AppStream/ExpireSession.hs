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
-- Module      : Network.AWS.AppStream.ExpireSession
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Immediately stops the specified streaming session.
module Network.AWS.AppStream.ExpireSession
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

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newExpireSession' smart constructor.
data ExpireSession = ExpireSession'
  { -- | The identifier of the streaming session.
    sessionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  ExpireSession
newExpireSession pSessionId_ =
  ExpireSession' {sessionId = pSessionId_}

-- | The identifier of the streaming session.
expireSession_sessionId :: Lens.Lens' ExpireSession Core.Text
expireSession_sessionId = Lens.lens (\ExpireSession' {sessionId} -> sessionId) (\s@ExpireSession' {} a -> s {sessionId = a} :: ExpireSession)

instance Core.AWSRequest ExpireSession where
  type
    AWSResponse ExpireSession =
      ExpireSessionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          ExpireSessionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ExpireSession

instance Core.NFData ExpireSession

instance Core.ToHeaders ExpireSession where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.ExpireSession" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ExpireSession where
  toJSON ExpireSession' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("SessionId" Core..= sessionId)]
      )

instance Core.ToPath ExpireSession where
  toPath = Core.const "/"

instance Core.ToQuery ExpireSession where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newExpireSessionResponse' smart constructor.
data ExpireSessionResponse = ExpireSessionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ExpireSessionResponse
newExpireSessionResponse pHttpStatus_ =
  ExpireSessionResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
expireSessionResponse_httpStatus :: Lens.Lens' ExpireSessionResponse Core.Int
expireSessionResponse_httpStatus = Lens.lens (\ExpireSessionResponse' {httpStatus} -> httpStatus) (\s@ExpireSessionResponse' {} a -> s {httpStatus = a} :: ExpireSessionResponse)

instance Core.NFData ExpireSessionResponse
