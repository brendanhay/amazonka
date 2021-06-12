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
-- Module      : Network.AWS.IoTAnalytics.DescribeChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a channel.
module Network.AWS.IoTAnalytics.DescribeChannel
  ( -- * Creating a Request
    DescribeChannel (..),
    newDescribeChannel,

    -- * Request Lenses
    describeChannel_includeStatistics,
    describeChannel_channelName,

    -- * Destructuring the Response
    DescribeChannelResponse (..),
    newDescribeChannelResponse,

    -- * Response Lenses
    describeChannelResponse_statistics,
    describeChannelResponse_channel,
    describeChannelResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeChannel' smart constructor.
data DescribeChannel = DescribeChannel'
  { -- | If true, additional statistical information about the channel is
    -- included in the response. This feature cannot be used with a channel
    -- whose S3 storage is customer-managed.
    includeStatistics :: Core.Maybe Core.Bool,
    -- | The name of the channel whose information is retrieved.
    channelName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeStatistics', 'describeChannel_includeStatistics' - If true, additional statistical information about the channel is
-- included in the response. This feature cannot be used with a channel
-- whose S3 storage is customer-managed.
--
-- 'channelName', 'describeChannel_channelName' - The name of the channel whose information is retrieved.
newDescribeChannel ::
  -- | 'channelName'
  Core.Text ->
  DescribeChannel
newDescribeChannel pChannelName_ =
  DescribeChannel'
    { includeStatistics = Core.Nothing,
      channelName = pChannelName_
    }

-- | If true, additional statistical information about the channel is
-- included in the response. This feature cannot be used with a channel
-- whose S3 storage is customer-managed.
describeChannel_includeStatistics :: Lens.Lens' DescribeChannel (Core.Maybe Core.Bool)
describeChannel_includeStatistics = Lens.lens (\DescribeChannel' {includeStatistics} -> includeStatistics) (\s@DescribeChannel' {} a -> s {includeStatistics = a} :: DescribeChannel)

-- | The name of the channel whose information is retrieved.
describeChannel_channelName :: Lens.Lens' DescribeChannel Core.Text
describeChannel_channelName = Lens.lens (\DescribeChannel' {channelName} -> channelName) (\s@DescribeChannel' {} a -> s {channelName = a} :: DescribeChannel)

instance Core.AWSRequest DescribeChannel where
  type
    AWSResponse DescribeChannel =
      DescribeChannelResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeChannelResponse'
            Core.<$> (x Core..?> "statistics")
            Core.<*> (x Core..?> "channel")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeChannel

instance Core.NFData DescribeChannel

instance Core.ToHeaders DescribeChannel where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeChannel where
  toPath DescribeChannel' {..} =
    Core.mconcat ["/channels/", Core.toBS channelName]

instance Core.ToQuery DescribeChannel where
  toQuery DescribeChannel' {..} =
    Core.mconcat
      ["includeStatistics" Core.=: includeStatistics]

-- | /See:/ 'newDescribeChannelResponse' smart constructor.
data DescribeChannelResponse = DescribeChannelResponse'
  { -- | Statistics about the channel. Included if the @includeStatistics@
    -- parameter is set to @true@ in the request.
    statistics :: Core.Maybe ChannelStatistics,
    -- | An object that contains information about the channel.
    channel :: Core.Maybe Channel,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statistics', 'describeChannelResponse_statistics' - Statistics about the channel. Included if the @includeStatistics@
-- parameter is set to @true@ in the request.
--
-- 'channel', 'describeChannelResponse_channel' - An object that contains information about the channel.
--
-- 'httpStatus', 'describeChannelResponse_httpStatus' - The response's http status code.
newDescribeChannelResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeChannelResponse
newDescribeChannelResponse pHttpStatus_ =
  DescribeChannelResponse'
    { statistics = Core.Nothing,
      channel = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Statistics about the channel. Included if the @includeStatistics@
-- parameter is set to @true@ in the request.
describeChannelResponse_statistics :: Lens.Lens' DescribeChannelResponse (Core.Maybe ChannelStatistics)
describeChannelResponse_statistics = Lens.lens (\DescribeChannelResponse' {statistics} -> statistics) (\s@DescribeChannelResponse' {} a -> s {statistics = a} :: DescribeChannelResponse)

-- | An object that contains information about the channel.
describeChannelResponse_channel :: Lens.Lens' DescribeChannelResponse (Core.Maybe Channel)
describeChannelResponse_channel = Lens.lens (\DescribeChannelResponse' {channel} -> channel) (\s@DescribeChannelResponse' {} a -> s {channel = a} :: DescribeChannelResponse)

-- | The response's http status code.
describeChannelResponse_httpStatus :: Lens.Lens' DescribeChannelResponse Core.Int
describeChannelResponse_httpStatus = Lens.lens (\DescribeChannelResponse' {httpStatus} -> httpStatus) (\s@DescribeChannelResponse' {} a -> s {httpStatus = a} :: DescribeChannelResponse)

instance Core.NFData DescribeChannelResponse
