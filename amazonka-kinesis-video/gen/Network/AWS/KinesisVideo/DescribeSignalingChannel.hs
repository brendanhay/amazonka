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
-- Module      : Network.AWS.KinesisVideo.DescribeSignalingChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the most current information about the signaling channel. You
-- must specify either the name or the Amazon Resource Name (ARN) of the
-- channel that you want to describe.
module Network.AWS.KinesisVideo.DescribeSignalingChannel
  ( -- * Creating a Request
    DescribeSignalingChannel (..),
    newDescribeSignalingChannel,

    -- * Request Lenses
    describeSignalingChannel_channelName,
    describeSignalingChannel_channelARN,

    -- * Destructuring the Response
    DescribeSignalingChannelResponse (..),
    newDescribeSignalingChannelResponse,

    -- * Response Lenses
    describeSignalingChannelResponse_channelInfo,
    describeSignalingChannelResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeSignalingChannel' smart constructor.
data DescribeSignalingChannel = DescribeSignalingChannel'
  { -- | The name of the signaling channel that you want to describe.
    channelName :: Core.Maybe Core.Text,
    -- | The ARN of the signaling channel that you want to describe.
    channelARN :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSignalingChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelName', 'describeSignalingChannel_channelName' - The name of the signaling channel that you want to describe.
--
-- 'channelARN', 'describeSignalingChannel_channelARN' - The ARN of the signaling channel that you want to describe.
newDescribeSignalingChannel ::
  DescribeSignalingChannel
newDescribeSignalingChannel =
  DescribeSignalingChannel'
    { channelName =
        Core.Nothing,
      channelARN = Core.Nothing
    }

-- | The name of the signaling channel that you want to describe.
describeSignalingChannel_channelName :: Lens.Lens' DescribeSignalingChannel (Core.Maybe Core.Text)
describeSignalingChannel_channelName = Lens.lens (\DescribeSignalingChannel' {channelName} -> channelName) (\s@DescribeSignalingChannel' {} a -> s {channelName = a} :: DescribeSignalingChannel)

-- | The ARN of the signaling channel that you want to describe.
describeSignalingChannel_channelARN :: Lens.Lens' DescribeSignalingChannel (Core.Maybe Core.Text)
describeSignalingChannel_channelARN = Lens.lens (\DescribeSignalingChannel' {channelARN} -> channelARN) (\s@DescribeSignalingChannel' {} a -> s {channelARN = a} :: DescribeSignalingChannel)

instance Core.AWSRequest DescribeSignalingChannel where
  type
    AWSResponse DescribeSignalingChannel =
      DescribeSignalingChannelResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSignalingChannelResponse'
            Core.<$> (x Core..?> "ChannelInfo")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeSignalingChannel

instance Core.NFData DescribeSignalingChannel

instance Core.ToHeaders DescribeSignalingChannel where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON DescribeSignalingChannel where
  toJSON DescribeSignalingChannel' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ChannelName" Core..=) Core.<$> channelName,
            ("ChannelARN" Core..=) Core.<$> channelARN
          ]
      )

instance Core.ToPath DescribeSignalingChannel where
  toPath = Core.const "/describeSignalingChannel"

instance Core.ToQuery DescribeSignalingChannel where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeSignalingChannelResponse' smart constructor.
data DescribeSignalingChannelResponse = DescribeSignalingChannelResponse'
  { -- | A structure that encapsulates the specified signaling channel\'s
    -- metadata and properties.
    channelInfo :: Core.Maybe ChannelInfo,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSignalingChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelInfo', 'describeSignalingChannelResponse_channelInfo' - A structure that encapsulates the specified signaling channel\'s
-- metadata and properties.
--
-- 'httpStatus', 'describeSignalingChannelResponse_httpStatus' - The response's http status code.
newDescribeSignalingChannelResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeSignalingChannelResponse
newDescribeSignalingChannelResponse pHttpStatus_ =
  DescribeSignalingChannelResponse'
    { channelInfo =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that encapsulates the specified signaling channel\'s
-- metadata and properties.
describeSignalingChannelResponse_channelInfo :: Lens.Lens' DescribeSignalingChannelResponse (Core.Maybe ChannelInfo)
describeSignalingChannelResponse_channelInfo = Lens.lens (\DescribeSignalingChannelResponse' {channelInfo} -> channelInfo) (\s@DescribeSignalingChannelResponse' {} a -> s {channelInfo = a} :: DescribeSignalingChannelResponse)

-- | The response's http status code.
describeSignalingChannelResponse_httpStatus :: Lens.Lens' DescribeSignalingChannelResponse Core.Int
describeSignalingChannelResponse_httpStatus = Lens.lens (\DescribeSignalingChannelResponse' {httpStatus} -> httpStatus) (\s@DescribeSignalingChannelResponse' {} a -> s {httpStatus = a} :: DescribeSignalingChannelResponse)

instance Core.NFData DescribeSignalingChannelResponse
