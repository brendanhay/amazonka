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
-- Module      : Amazonka.IVS.StopStream
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disconnects the incoming RTMPS stream for the specified channel. Can be
-- used in conjunction with DeleteStreamKey to prevent further streaming to
-- a channel.
--
-- Many streaming client-software libraries automatically reconnect a
-- dropped RTMPS session, so to stop the stream permanently, you may want
-- to first revoke the @streamKey@ attached to the channel.
module Amazonka.IVS.StopStream
  ( -- * Creating a Request
    StopStream (..),
    newStopStream,

    -- * Request Lenses
    stopStream_channelArn,

    -- * Destructuring the Response
    StopStreamResponse (..),
    newStopStreamResponse,

    -- * Response Lenses
    stopStreamResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopStream' smart constructor.
data StopStream = StopStream'
  { -- | ARN of the channel for which the stream is to be stopped.
    channelArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelArn', 'stopStream_channelArn' - ARN of the channel for which the stream is to be stopped.
newStopStream ::
  -- | 'channelArn'
  Prelude.Text ->
  StopStream
newStopStream pChannelArn_ =
  StopStream' {channelArn = pChannelArn_}

-- | ARN of the channel for which the stream is to be stopped.
stopStream_channelArn :: Lens.Lens' StopStream Prelude.Text
stopStream_channelArn = Lens.lens (\StopStream' {channelArn} -> channelArn) (\s@StopStream' {} a -> s {channelArn = a} :: StopStream)

instance Core.AWSRequest StopStream where
  type AWSResponse StopStream = StopStreamResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopStreamResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopStream where
  hashWithSalt _salt StopStream' {..} =
    _salt `Prelude.hashWithSalt` channelArn

instance Prelude.NFData StopStream where
  rnf StopStream' {..} = Prelude.rnf channelArn

instance Data.ToHeaders StopStream where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopStream where
  toJSON StopStream' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("channelArn" Data..= channelArn)]
      )

instance Data.ToPath StopStream where
  toPath = Prelude.const "/StopStream"

instance Data.ToQuery StopStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopStreamResponse' smart constructor.
data StopStreamResponse = StopStreamResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopStreamResponse_httpStatus' - The response's http status code.
newStopStreamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopStreamResponse
newStopStreamResponse pHttpStatus_ =
  StopStreamResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
stopStreamResponse_httpStatus :: Lens.Lens' StopStreamResponse Prelude.Int
stopStreamResponse_httpStatus = Lens.lens (\StopStreamResponse' {httpStatus} -> httpStatus) (\s@StopStreamResponse' {} a -> s {httpStatus = a} :: StopStreamResponse)

instance Prelude.NFData StopStreamResponse where
  rnf StopStreamResponse' {..} = Prelude.rnf httpStatus
