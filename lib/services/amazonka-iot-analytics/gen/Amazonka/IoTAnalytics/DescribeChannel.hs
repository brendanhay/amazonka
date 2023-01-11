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
-- Module      : Amazonka.IoTAnalytics.DescribeChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a channel.
module Amazonka.IoTAnalytics.DescribeChannel
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
    describeChannelResponse_channel,
    describeChannelResponse_statistics,
    describeChannelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeChannel' smart constructor.
data DescribeChannel = DescribeChannel'
  { -- | If true, additional statistical information about the channel is
    -- included in the response. This feature can\'t be used with a channel
    -- whose S3 storage is customer-managed.
    includeStatistics :: Prelude.Maybe Prelude.Bool,
    -- | The name of the channel whose information is retrieved.
    channelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeStatistics', 'describeChannel_includeStatistics' - If true, additional statistical information about the channel is
-- included in the response. This feature can\'t be used with a channel
-- whose S3 storage is customer-managed.
--
-- 'channelName', 'describeChannel_channelName' - The name of the channel whose information is retrieved.
newDescribeChannel ::
  -- | 'channelName'
  Prelude.Text ->
  DescribeChannel
newDescribeChannel pChannelName_ =
  DescribeChannel'
    { includeStatistics =
        Prelude.Nothing,
      channelName = pChannelName_
    }

-- | If true, additional statistical information about the channel is
-- included in the response. This feature can\'t be used with a channel
-- whose S3 storage is customer-managed.
describeChannel_includeStatistics :: Lens.Lens' DescribeChannel (Prelude.Maybe Prelude.Bool)
describeChannel_includeStatistics = Lens.lens (\DescribeChannel' {includeStatistics} -> includeStatistics) (\s@DescribeChannel' {} a -> s {includeStatistics = a} :: DescribeChannel)

-- | The name of the channel whose information is retrieved.
describeChannel_channelName :: Lens.Lens' DescribeChannel Prelude.Text
describeChannel_channelName = Lens.lens (\DescribeChannel' {channelName} -> channelName) (\s@DescribeChannel' {} a -> s {channelName = a} :: DescribeChannel)

instance Core.AWSRequest DescribeChannel where
  type
    AWSResponse DescribeChannel =
      DescribeChannelResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeChannelResponse'
            Prelude.<$> (x Data..?> "channel")
            Prelude.<*> (x Data..?> "statistics")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeChannel where
  hashWithSalt _salt DescribeChannel' {..} =
    _salt `Prelude.hashWithSalt` includeStatistics
      `Prelude.hashWithSalt` channelName

instance Prelude.NFData DescribeChannel where
  rnf DescribeChannel' {..} =
    Prelude.rnf includeStatistics
      `Prelude.seq` Prelude.rnf channelName

instance Data.ToHeaders DescribeChannel where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeChannel where
  toPath DescribeChannel' {..} =
    Prelude.mconcat
      ["/channels/", Data.toBS channelName]

instance Data.ToQuery DescribeChannel where
  toQuery DescribeChannel' {..} =
    Prelude.mconcat
      ["includeStatistics" Data.=: includeStatistics]

-- | /See:/ 'newDescribeChannelResponse' smart constructor.
data DescribeChannelResponse = DescribeChannelResponse'
  { -- | An object that contains information about the channel.
    channel :: Prelude.Maybe Channel,
    -- | Statistics about the channel. Included if the @includeStatistics@
    -- parameter is set to @true@ in the request.
    statistics :: Prelude.Maybe ChannelStatistics,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channel', 'describeChannelResponse_channel' - An object that contains information about the channel.
--
-- 'statistics', 'describeChannelResponse_statistics' - Statistics about the channel. Included if the @includeStatistics@
-- parameter is set to @true@ in the request.
--
-- 'httpStatus', 'describeChannelResponse_httpStatus' - The response's http status code.
newDescribeChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeChannelResponse
newDescribeChannelResponse pHttpStatus_ =
  DescribeChannelResponse'
    { channel = Prelude.Nothing,
      statistics = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains information about the channel.
describeChannelResponse_channel :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Channel)
describeChannelResponse_channel = Lens.lens (\DescribeChannelResponse' {channel} -> channel) (\s@DescribeChannelResponse' {} a -> s {channel = a} :: DescribeChannelResponse)

-- | Statistics about the channel. Included if the @includeStatistics@
-- parameter is set to @true@ in the request.
describeChannelResponse_statistics :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe ChannelStatistics)
describeChannelResponse_statistics = Lens.lens (\DescribeChannelResponse' {statistics} -> statistics) (\s@DescribeChannelResponse' {} a -> s {statistics = a} :: DescribeChannelResponse)

-- | The response's http status code.
describeChannelResponse_httpStatus :: Lens.Lens' DescribeChannelResponse Prelude.Int
describeChannelResponse_httpStatus = Lens.lens (\DescribeChannelResponse' {httpStatus} -> httpStatus) (\s@DescribeChannelResponse' {} a -> s {httpStatus = a} :: DescribeChannelResponse)

instance Prelude.NFData DescribeChannelResponse where
  rnf DescribeChannelResponse' {..} =
    Prelude.rnf channel
      `Prelude.seq` Prelude.rnf statistics
      `Prelude.seq` Prelude.rnf httpStatus
