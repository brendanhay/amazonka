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
-- Module      : Network.AWS.DeviceFarm.GetRemoteAccessSession
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a link to a currently running remote access session.
module Network.AWS.DeviceFarm.GetRemoteAccessSession
  ( -- * Creating a Request
    GetRemoteAccessSession (..),
    newGetRemoteAccessSession,

    -- * Request Lenses
    getRemoteAccessSession_arn,

    -- * Destructuring the Response
    GetRemoteAccessSessionResponse (..),
    newGetRemoteAccessSessionResponse,

    -- * Response Lenses
    getRemoteAccessSessionResponse_remoteAccessSession,
    getRemoteAccessSessionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to get information about the specified remote
-- access session.
--
-- /See:/ 'newGetRemoteAccessSession' smart constructor.
data GetRemoteAccessSession = GetRemoteAccessSession'
  { -- | The Amazon Resource Name (ARN) of the remote access session about which
    -- you want to get session information.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetRemoteAccessSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getRemoteAccessSession_arn' - The Amazon Resource Name (ARN) of the remote access session about which
-- you want to get session information.
newGetRemoteAccessSession ::
  -- | 'arn'
  Core.Text ->
  GetRemoteAccessSession
newGetRemoteAccessSession pArn_ =
  GetRemoteAccessSession' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the remote access session about which
-- you want to get session information.
getRemoteAccessSession_arn :: Lens.Lens' GetRemoteAccessSession Core.Text
getRemoteAccessSession_arn = Lens.lens (\GetRemoteAccessSession' {arn} -> arn) (\s@GetRemoteAccessSession' {} a -> s {arn = a} :: GetRemoteAccessSession)

instance Core.AWSRequest GetRemoteAccessSession where
  type
    AWSResponse GetRemoteAccessSession =
      GetRemoteAccessSessionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRemoteAccessSessionResponse'
            Core.<$> (x Core..?> "remoteAccessSession")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetRemoteAccessSession

instance Core.NFData GetRemoteAccessSession

instance Core.ToHeaders GetRemoteAccessSession where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.GetRemoteAccessSession" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetRemoteAccessSession where
  toJSON GetRemoteAccessSession' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.ToPath GetRemoteAccessSession where
  toPath = Core.const "/"

instance Core.ToQuery GetRemoteAccessSession where
  toQuery = Core.const Core.mempty

-- | Represents the response from the server that lists detailed information
-- about the remote access session.
--
-- /See:/ 'newGetRemoteAccessSessionResponse' smart constructor.
data GetRemoteAccessSessionResponse = GetRemoteAccessSessionResponse'
  { -- | A container that lists detailed information about the remote access
    -- session.
    remoteAccessSession :: Core.Maybe RemoteAccessSession,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetRemoteAccessSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'remoteAccessSession', 'getRemoteAccessSessionResponse_remoteAccessSession' - A container that lists detailed information about the remote access
-- session.
--
-- 'httpStatus', 'getRemoteAccessSessionResponse_httpStatus' - The response's http status code.
newGetRemoteAccessSessionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetRemoteAccessSessionResponse
newGetRemoteAccessSessionResponse pHttpStatus_ =
  GetRemoteAccessSessionResponse'
    { remoteAccessSession =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A container that lists detailed information about the remote access
-- session.
getRemoteAccessSessionResponse_remoteAccessSession :: Lens.Lens' GetRemoteAccessSessionResponse (Core.Maybe RemoteAccessSession)
getRemoteAccessSessionResponse_remoteAccessSession = Lens.lens (\GetRemoteAccessSessionResponse' {remoteAccessSession} -> remoteAccessSession) (\s@GetRemoteAccessSessionResponse' {} a -> s {remoteAccessSession = a} :: GetRemoteAccessSessionResponse)

-- | The response's http status code.
getRemoteAccessSessionResponse_httpStatus :: Lens.Lens' GetRemoteAccessSessionResponse Core.Int
getRemoteAccessSessionResponse_httpStatus = Lens.lens (\GetRemoteAccessSessionResponse' {httpStatus} -> httpStatus) (\s@GetRemoteAccessSessionResponse' {} a -> s {httpStatus = a} :: GetRemoteAccessSessionResponse)

instance Core.NFData GetRemoteAccessSessionResponse
