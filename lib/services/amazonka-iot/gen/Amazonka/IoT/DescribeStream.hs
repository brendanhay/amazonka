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
-- Module      : Amazonka.IoT.DescribeStream
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a stream.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DescribeStream>
-- action.
module Amazonka.IoT.DescribeStream
  ( -- * Creating a Request
    DescribeStream (..),
    newDescribeStream,

    -- * Request Lenses
    describeStream_streamId,

    -- * Destructuring the Response
    DescribeStreamResponse (..),
    newDescribeStreamResponse,

    -- * Response Lenses
    describeStreamResponse_streamInfo,
    describeStreamResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeStream' smart constructor.
data DescribeStream = DescribeStream'
  { -- | The stream ID.
    streamId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamId', 'describeStream_streamId' - The stream ID.
newDescribeStream ::
  -- | 'streamId'
  Prelude.Text ->
  DescribeStream
newDescribeStream pStreamId_ =
  DescribeStream' {streamId = pStreamId_}

-- | The stream ID.
describeStream_streamId :: Lens.Lens' DescribeStream Prelude.Text
describeStream_streamId = Lens.lens (\DescribeStream' {streamId} -> streamId) (\s@DescribeStream' {} a -> s {streamId = a} :: DescribeStream)

instance Core.AWSRequest DescribeStream where
  type
    AWSResponse DescribeStream =
      DescribeStreamResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStreamResponse'
            Prelude.<$> (x Data..?> "streamInfo")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeStream where
  hashWithSalt _salt DescribeStream' {..} =
    _salt `Prelude.hashWithSalt` streamId

instance Prelude.NFData DescribeStream where
  rnf DescribeStream' {..} = Prelude.rnf streamId

instance Data.ToHeaders DescribeStream where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeStream where
  toPath DescribeStream' {..} =
    Prelude.mconcat ["/streams/", Data.toBS streamId]

instance Data.ToQuery DescribeStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeStreamResponse' smart constructor.
data DescribeStreamResponse = DescribeStreamResponse'
  { -- | Information about the stream.
    streamInfo :: Prelude.Maybe StreamInfo,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamInfo', 'describeStreamResponse_streamInfo' - Information about the stream.
--
-- 'httpStatus', 'describeStreamResponse_httpStatus' - The response's http status code.
newDescribeStreamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeStreamResponse
newDescribeStreamResponse pHttpStatus_ =
  DescribeStreamResponse'
    { streamInfo =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the stream.
describeStreamResponse_streamInfo :: Lens.Lens' DescribeStreamResponse (Prelude.Maybe StreamInfo)
describeStreamResponse_streamInfo = Lens.lens (\DescribeStreamResponse' {streamInfo} -> streamInfo) (\s@DescribeStreamResponse' {} a -> s {streamInfo = a} :: DescribeStreamResponse)

-- | The response's http status code.
describeStreamResponse_httpStatus :: Lens.Lens' DescribeStreamResponse Prelude.Int
describeStreamResponse_httpStatus = Lens.lens (\DescribeStreamResponse' {httpStatus} -> httpStatus) (\s@DescribeStreamResponse' {} a -> s {httpStatus = a} :: DescribeStreamResponse)

instance Prelude.NFData DescribeStreamResponse where
  rnf DescribeStreamResponse' {..} =
    Prelude.rnf streamInfo
      `Prelude.seq` Prelude.rnf httpStatus
