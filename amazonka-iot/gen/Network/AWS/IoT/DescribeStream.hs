{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.DescribeStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a stream.
module Network.AWS.IoT.DescribeStream
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeStream' smart constructor.
data DescribeStream = DescribeStream'
  { -- | The stream ID.
    streamId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DescribeStream where
  type Rs DescribeStream = DescribeStreamResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStreamResponse'
            Prelude.<$> (x Prelude..?> "streamInfo")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeStream

instance Prelude.NFData DescribeStream

instance Prelude.ToHeaders DescribeStream where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeStream where
  toPath DescribeStream' {..} =
    Prelude.mconcat
      ["/streams/", Prelude.toBS streamId]

instance Prelude.ToQuery DescribeStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeStreamResponse' smart constructor.
data DescribeStreamResponse = DescribeStreamResponse'
  { -- | Information about the stream.
    streamInfo :: Prelude.Maybe StreamInfo,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DescribeStreamResponse
