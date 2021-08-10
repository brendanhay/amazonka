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
-- Module      : Network.AWS.DeviceFarm.StopRemoteAccessSession
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Ends a specified remote access session.
module Network.AWS.DeviceFarm.StopRemoteAccessSession
  ( -- * Creating a Request
    StopRemoteAccessSession (..),
    newStopRemoteAccessSession,

    -- * Request Lenses
    stopRemoteAccessSession_arn,

    -- * Destructuring the Response
    StopRemoteAccessSessionResponse (..),
    newStopRemoteAccessSessionResponse,

    -- * Response Lenses
    stopRemoteAccessSessionResponse_remoteAccessSession,
    stopRemoteAccessSessionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to stop the remote access session.
--
-- /See:/ 'newStopRemoteAccessSession' smart constructor.
data StopRemoteAccessSession = StopRemoteAccessSession'
  { -- | The Amazon Resource Name (ARN) of the remote access session to stop.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopRemoteAccessSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'stopRemoteAccessSession_arn' - The Amazon Resource Name (ARN) of the remote access session to stop.
newStopRemoteAccessSession ::
  -- | 'arn'
  Prelude.Text ->
  StopRemoteAccessSession
newStopRemoteAccessSession pArn_ =
  StopRemoteAccessSession' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the remote access session to stop.
stopRemoteAccessSession_arn :: Lens.Lens' StopRemoteAccessSession Prelude.Text
stopRemoteAccessSession_arn = Lens.lens (\StopRemoteAccessSession' {arn} -> arn) (\s@StopRemoteAccessSession' {} a -> s {arn = a} :: StopRemoteAccessSession)

instance Core.AWSRequest StopRemoteAccessSession where
  type
    AWSResponse StopRemoteAccessSession =
      StopRemoteAccessSessionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopRemoteAccessSessionResponse'
            Prelude.<$> (x Core..?> "remoteAccessSession")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopRemoteAccessSession

instance Prelude.NFData StopRemoteAccessSession

instance Core.ToHeaders StopRemoteAccessSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.StopRemoteAccessSession" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StopRemoteAccessSession where
  toJSON StopRemoteAccessSession' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Core..= arn)]
      )

instance Core.ToPath StopRemoteAccessSession where
  toPath = Prelude.const "/"

instance Core.ToQuery StopRemoteAccessSession where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response from the server that describes the remote access
-- session when AWS Device Farm stops the session.
--
-- /See:/ 'newStopRemoteAccessSessionResponse' smart constructor.
data StopRemoteAccessSessionResponse = StopRemoteAccessSessionResponse'
  { -- | A container that represents the metadata from the service about the
    -- remote access session you are stopping.
    remoteAccessSession :: Prelude.Maybe RemoteAccessSession,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopRemoteAccessSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'remoteAccessSession', 'stopRemoteAccessSessionResponse_remoteAccessSession' - A container that represents the metadata from the service about the
-- remote access session you are stopping.
--
-- 'httpStatus', 'stopRemoteAccessSessionResponse_httpStatus' - The response's http status code.
newStopRemoteAccessSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopRemoteAccessSessionResponse
newStopRemoteAccessSessionResponse pHttpStatus_ =
  StopRemoteAccessSessionResponse'
    { remoteAccessSession =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A container that represents the metadata from the service about the
-- remote access session you are stopping.
stopRemoteAccessSessionResponse_remoteAccessSession :: Lens.Lens' StopRemoteAccessSessionResponse (Prelude.Maybe RemoteAccessSession)
stopRemoteAccessSessionResponse_remoteAccessSession = Lens.lens (\StopRemoteAccessSessionResponse' {remoteAccessSession} -> remoteAccessSession) (\s@StopRemoteAccessSessionResponse' {} a -> s {remoteAccessSession = a} :: StopRemoteAccessSessionResponse)

-- | The response's http status code.
stopRemoteAccessSessionResponse_httpStatus :: Lens.Lens' StopRemoteAccessSessionResponse Prelude.Int
stopRemoteAccessSessionResponse_httpStatus = Lens.lens (\StopRemoteAccessSessionResponse' {httpStatus} -> httpStatus) (\s@StopRemoteAccessSessionResponse' {} a -> s {httpStatus = a} :: StopRemoteAccessSessionResponse)

instance
  Prelude.NFData
    StopRemoteAccessSessionResponse
