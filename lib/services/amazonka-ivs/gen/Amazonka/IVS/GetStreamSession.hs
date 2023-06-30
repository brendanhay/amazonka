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
-- Module      : Amazonka.IVS.GetStreamSession
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets metadata on a specified stream.
module Amazonka.IVS.GetStreamSession
  ( -- * Creating a Request
    GetStreamSession (..),
    newGetStreamSession,

    -- * Request Lenses
    getStreamSession_streamId,
    getStreamSession_channelArn,

    -- * Destructuring the Response
    GetStreamSessionResponse (..),
    newGetStreamSessionResponse,

    -- * Response Lenses
    getStreamSessionResponse_streamSession,
    getStreamSessionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetStreamSession' smart constructor.
data GetStreamSession = GetStreamSession'
  { -- | Unique identifier for a live or previously live stream in the specified
    -- channel. If no @streamId@ is provided, this returns the most recent
    -- stream session for the channel, if it exists.
    streamId :: Prelude.Maybe Prelude.Text,
    -- | ARN of the channel resource
    channelArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStreamSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamId', 'getStreamSession_streamId' - Unique identifier for a live or previously live stream in the specified
-- channel. If no @streamId@ is provided, this returns the most recent
-- stream session for the channel, if it exists.
--
-- 'channelArn', 'getStreamSession_channelArn' - ARN of the channel resource
newGetStreamSession ::
  -- | 'channelArn'
  Prelude.Text ->
  GetStreamSession
newGetStreamSession pChannelArn_ =
  GetStreamSession'
    { streamId = Prelude.Nothing,
      channelArn = pChannelArn_
    }

-- | Unique identifier for a live or previously live stream in the specified
-- channel. If no @streamId@ is provided, this returns the most recent
-- stream session for the channel, if it exists.
getStreamSession_streamId :: Lens.Lens' GetStreamSession (Prelude.Maybe Prelude.Text)
getStreamSession_streamId = Lens.lens (\GetStreamSession' {streamId} -> streamId) (\s@GetStreamSession' {} a -> s {streamId = a} :: GetStreamSession)

-- | ARN of the channel resource
getStreamSession_channelArn :: Lens.Lens' GetStreamSession Prelude.Text
getStreamSession_channelArn = Lens.lens (\GetStreamSession' {channelArn} -> channelArn) (\s@GetStreamSession' {} a -> s {channelArn = a} :: GetStreamSession)

instance Core.AWSRequest GetStreamSession where
  type
    AWSResponse GetStreamSession =
      GetStreamSessionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStreamSessionResponse'
            Prelude.<$> (x Data..?> "streamSession")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetStreamSession where
  hashWithSalt _salt GetStreamSession' {..} =
    _salt
      `Prelude.hashWithSalt` streamId
      `Prelude.hashWithSalt` channelArn

instance Prelude.NFData GetStreamSession where
  rnf GetStreamSession' {..} =
    Prelude.rnf streamId
      `Prelude.seq` Prelude.rnf channelArn

instance Data.ToHeaders GetStreamSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetStreamSession where
  toJSON GetStreamSession' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("streamId" Data..=) Prelude.<$> streamId,
            Prelude.Just ("channelArn" Data..= channelArn)
          ]
      )

instance Data.ToPath GetStreamSession where
  toPath = Prelude.const "/GetStreamSession"

instance Data.ToQuery GetStreamSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetStreamSessionResponse' smart constructor.
data GetStreamSessionResponse = GetStreamSessionResponse'
  { -- | List of stream details.
    streamSession :: Prelude.Maybe StreamSession,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStreamSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamSession', 'getStreamSessionResponse_streamSession' - List of stream details.
--
-- 'httpStatus', 'getStreamSessionResponse_httpStatus' - The response's http status code.
newGetStreamSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetStreamSessionResponse
newGetStreamSessionResponse pHttpStatus_ =
  GetStreamSessionResponse'
    { streamSession =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of stream details.
getStreamSessionResponse_streamSession :: Lens.Lens' GetStreamSessionResponse (Prelude.Maybe StreamSession)
getStreamSessionResponse_streamSession = Lens.lens (\GetStreamSessionResponse' {streamSession} -> streamSession) (\s@GetStreamSessionResponse' {} a -> s {streamSession = a} :: GetStreamSessionResponse)

-- | The response's http status code.
getStreamSessionResponse_httpStatus :: Lens.Lens' GetStreamSessionResponse Prelude.Int
getStreamSessionResponse_httpStatus = Lens.lens (\GetStreamSessionResponse' {httpStatus} -> httpStatus) (\s@GetStreamSessionResponse' {} a -> s {httpStatus = a} :: GetStreamSessionResponse)

instance Prelude.NFData GetStreamSessionResponse where
  rnf GetStreamSessionResponse' {..} =
    Prelude.rnf streamSession
      `Prelude.seq` Prelude.rnf httpStatus
