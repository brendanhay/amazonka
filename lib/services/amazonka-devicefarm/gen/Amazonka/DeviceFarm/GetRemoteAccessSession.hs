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
-- Module      : Amazonka.DeviceFarm.GetRemoteAccessSession
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a link to a currently running remote access session.
module Amazonka.DeviceFarm.GetRemoteAccessSession
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to get information about the specified remote
-- access session.
--
-- /See:/ 'newGetRemoteAccessSession' smart constructor.
data GetRemoteAccessSession = GetRemoteAccessSession'
  { -- | The Amazon Resource Name (ARN) of the remote access session about which
    -- you want to get session information.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetRemoteAccessSession
newGetRemoteAccessSession pArn_ =
  GetRemoteAccessSession' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the remote access session about which
-- you want to get session information.
getRemoteAccessSession_arn :: Lens.Lens' GetRemoteAccessSession Prelude.Text
getRemoteAccessSession_arn = Lens.lens (\GetRemoteAccessSession' {arn} -> arn) (\s@GetRemoteAccessSession' {} a -> s {arn = a} :: GetRemoteAccessSession)

instance Core.AWSRequest GetRemoteAccessSession where
  type
    AWSResponse GetRemoteAccessSession =
      GetRemoteAccessSessionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRemoteAccessSessionResponse'
            Prelude.<$> (x Core..?> "remoteAccessSession")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRemoteAccessSession where
  hashWithSalt _salt GetRemoteAccessSession' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData GetRemoteAccessSession where
  rnf GetRemoteAccessSession' {..} = Prelude.rnf arn

instance Core.ToHeaders GetRemoteAccessSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.GetRemoteAccessSession" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetRemoteAccessSession where
  toJSON GetRemoteAccessSession' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Core..= arn)]
      )

instance Core.ToPath GetRemoteAccessSession where
  toPath = Prelude.const "/"

instance Core.ToQuery GetRemoteAccessSession where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response from the server that lists detailed information
-- about the remote access session.
--
-- /See:/ 'newGetRemoteAccessSessionResponse' smart constructor.
data GetRemoteAccessSessionResponse = GetRemoteAccessSessionResponse'
  { -- | A container that lists detailed information about the remote access
    -- session.
    remoteAccessSession :: Prelude.Maybe RemoteAccessSession,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetRemoteAccessSessionResponse
newGetRemoteAccessSessionResponse pHttpStatus_ =
  GetRemoteAccessSessionResponse'
    { remoteAccessSession =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A container that lists detailed information about the remote access
-- session.
getRemoteAccessSessionResponse_remoteAccessSession :: Lens.Lens' GetRemoteAccessSessionResponse (Prelude.Maybe RemoteAccessSession)
getRemoteAccessSessionResponse_remoteAccessSession = Lens.lens (\GetRemoteAccessSessionResponse' {remoteAccessSession} -> remoteAccessSession) (\s@GetRemoteAccessSessionResponse' {} a -> s {remoteAccessSession = a} :: GetRemoteAccessSessionResponse)

-- | The response's http status code.
getRemoteAccessSessionResponse_httpStatus :: Lens.Lens' GetRemoteAccessSessionResponse Prelude.Int
getRemoteAccessSessionResponse_httpStatus = Lens.lens (\GetRemoteAccessSessionResponse' {httpStatus} -> httpStatus) (\s@GetRemoteAccessSessionResponse' {} a -> s {httpStatus = a} :: GetRemoteAccessSessionResponse)

instance
  Prelude.NFData
    GetRemoteAccessSessionResponse
  where
  rnf GetRemoteAccessSessionResponse' {..} =
    Prelude.rnf remoteAccessSession
      `Prelude.seq` Prelude.rnf httpStatus
