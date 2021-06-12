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
-- Module      : Network.AWS.KinesisVideo.DescribeStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the most current information about the specified stream. You
-- must specify either the @StreamName@ or the @StreamARN@.
module Network.AWS.KinesisVideo.DescribeStream
  ( -- * Creating a Request
    DescribeStream (..),
    newDescribeStream,

    -- * Request Lenses
    describeStream_streamARN,
    describeStream_streamName,

    -- * Destructuring the Response
    DescribeStreamResponse (..),
    newDescribeStreamResponse,

    -- * Response Lenses
    describeStreamResponse_streamInfo,
    describeStreamResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeStream' smart constructor.
data DescribeStream = DescribeStream'
  { -- | The Amazon Resource Name (ARN) of the stream.
    streamARN :: Core.Maybe Core.Text,
    -- | The name of the stream.
    streamName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamARN', 'describeStream_streamARN' - The Amazon Resource Name (ARN) of the stream.
--
-- 'streamName', 'describeStream_streamName' - The name of the stream.
newDescribeStream ::
  DescribeStream
newDescribeStream =
  DescribeStream'
    { streamARN = Core.Nothing,
      streamName = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the stream.
describeStream_streamARN :: Lens.Lens' DescribeStream (Core.Maybe Core.Text)
describeStream_streamARN = Lens.lens (\DescribeStream' {streamARN} -> streamARN) (\s@DescribeStream' {} a -> s {streamARN = a} :: DescribeStream)

-- | The name of the stream.
describeStream_streamName :: Lens.Lens' DescribeStream (Core.Maybe Core.Text)
describeStream_streamName = Lens.lens (\DescribeStream' {streamName} -> streamName) (\s@DescribeStream' {} a -> s {streamName = a} :: DescribeStream)

instance Core.AWSRequest DescribeStream where
  type
    AWSResponse DescribeStream =
      DescribeStreamResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStreamResponse'
            Core.<$> (x Core..?> "StreamInfo")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeStream

instance Core.NFData DescribeStream

instance Core.ToHeaders DescribeStream where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON DescribeStream where
  toJSON DescribeStream' {..} =
    Core.object
      ( Core.catMaybes
          [ ("StreamARN" Core..=) Core.<$> streamARN,
            ("StreamName" Core..=) Core.<$> streamName
          ]
      )

instance Core.ToPath DescribeStream where
  toPath = Core.const "/describeStream"

instance Core.ToQuery DescribeStream where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeStreamResponse' smart constructor.
data DescribeStreamResponse = DescribeStreamResponse'
  { -- | An object that describes the stream.
    streamInfo :: Core.Maybe StreamInfo,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamInfo', 'describeStreamResponse_streamInfo' - An object that describes the stream.
--
-- 'httpStatus', 'describeStreamResponse_httpStatus' - The response's http status code.
newDescribeStreamResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeStreamResponse
newDescribeStreamResponse pHttpStatus_ =
  DescribeStreamResponse'
    { streamInfo = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that describes the stream.
describeStreamResponse_streamInfo :: Lens.Lens' DescribeStreamResponse (Core.Maybe StreamInfo)
describeStreamResponse_streamInfo = Lens.lens (\DescribeStreamResponse' {streamInfo} -> streamInfo) (\s@DescribeStreamResponse' {} a -> s {streamInfo = a} :: DescribeStreamResponse)

-- | The response's http status code.
describeStreamResponse_httpStatus :: Lens.Lens' DescribeStreamResponse Core.Int
describeStreamResponse_httpStatus = Lens.lens (\DescribeStreamResponse' {httpStatus} -> httpStatus) (\s@DescribeStreamResponse' {} a -> s {httpStatus = a} :: DescribeStreamResponse)

instance Core.NFData DescribeStreamResponse
