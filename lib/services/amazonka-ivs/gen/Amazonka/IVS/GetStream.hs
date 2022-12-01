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
-- Module      : Amazonka.IVS.GetStream
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the active (live) stream on a specified channel.
module Amazonka.IVS.GetStream
  ( -- * Creating a Request
    GetStream (..),
    newGetStream,

    -- * Request Lenses
    getStream_channelArn,

    -- * Destructuring the Response
    GetStreamResponse (..),
    newGetStreamResponse,

    -- * Response Lenses
    getStreamResponse_stream,
    getStreamResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IVS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetStream' smart constructor.
data GetStream = GetStream'
  { -- | Channel ARN for stream to be accessed.
    channelArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelArn', 'getStream_channelArn' - Channel ARN for stream to be accessed.
newGetStream ::
  -- | 'channelArn'
  Prelude.Text ->
  GetStream
newGetStream pChannelArn_ =
  GetStream' {channelArn = pChannelArn_}

-- | Channel ARN for stream to be accessed.
getStream_channelArn :: Lens.Lens' GetStream Prelude.Text
getStream_channelArn = Lens.lens (\GetStream' {channelArn} -> channelArn) (\s@GetStream' {} a -> s {channelArn = a} :: GetStream)

instance Core.AWSRequest GetStream where
  type AWSResponse GetStream = GetStreamResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStreamResponse'
            Prelude.<$> (x Core..?> "stream")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetStream where
  hashWithSalt _salt GetStream' {..} =
    _salt `Prelude.hashWithSalt` channelArn

instance Prelude.NFData GetStream where
  rnf GetStream' {..} = Prelude.rnf channelArn

instance Core.ToHeaders GetStream where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetStream where
  toJSON GetStream' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("channelArn" Core..= channelArn)]
      )

instance Core.ToPath GetStream where
  toPath = Prelude.const "/GetStream"

instance Core.ToQuery GetStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetStreamResponse' smart constructor.
data GetStreamResponse = GetStreamResponse'
  { stream :: Prelude.Maybe Stream,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stream', 'getStreamResponse_stream' -
--
-- 'httpStatus', 'getStreamResponse_httpStatus' - The response's http status code.
newGetStreamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetStreamResponse
newGetStreamResponse pHttpStatus_ =
  GetStreamResponse'
    { stream = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- |
getStreamResponse_stream :: Lens.Lens' GetStreamResponse (Prelude.Maybe Stream)
getStreamResponse_stream = Lens.lens (\GetStreamResponse' {stream} -> stream) (\s@GetStreamResponse' {} a -> s {stream = a} :: GetStreamResponse)

-- | The response's http status code.
getStreamResponse_httpStatus :: Lens.Lens' GetStreamResponse Prelude.Int
getStreamResponse_httpStatus = Lens.lens (\GetStreamResponse' {httpStatus} -> httpStatus) (\s@GetStreamResponse' {} a -> s {httpStatus = a} :: GetStreamResponse)

instance Prelude.NFData GetStreamResponse where
  rnf GetStreamResponse' {..} =
    Prelude.rnf stream
      `Prelude.seq` Prelude.rnf httpStatus
