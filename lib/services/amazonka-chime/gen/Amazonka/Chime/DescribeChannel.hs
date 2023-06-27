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
-- Module      : Amazonka.Chime.DescribeChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the full details of a channel in an Amazon Chime @AppInstance@.
--
-- The @x-amz-chime-bearer@ request header is mandatory. Use the
-- @AppInstanceUserArn@ of the user that makes the API call as the value in
-- the header.
module Amazonka.Chime.DescribeChannel
  ( -- * Creating a Request
    DescribeChannel (..),
    newDescribeChannel,

    -- * Request Lenses
    describeChannel_chimeBearer,
    describeChannel_channelArn,

    -- * Destructuring the Response
    DescribeChannelResponse (..),
    newDescribeChannelResponse,

    -- * Response Lenses
    describeChannelResponse_channel,
    describeChannelResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeChannel' smart constructor.
data DescribeChannel = DescribeChannel'
  { -- | The @AppInstanceUserArn@ of the user that makes the API call.
    chimeBearer :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the channel.
    channelArn :: Prelude.Text
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
-- 'chimeBearer', 'describeChannel_chimeBearer' - The @AppInstanceUserArn@ of the user that makes the API call.
--
-- 'channelArn', 'describeChannel_channelArn' - The ARN of the channel.
newDescribeChannel ::
  -- | 'channelArn'
  Prelude.Text ->
  DescribeChannel
newDescribeChannel pChannelArn_ =
  DescribeChannel'
    { chimeBearer = Prelude.Nothing,
      channelArn = pChannelArn_
    }

-- | The @AppInstanceUserArn@ of the user that makes the API call.
describeChannel_chimeBearer :: Lens.Lens' DescribeChannel (Prelude.Maybe Prelude.Text)
describeChannel_chimeBearer = Lens.lens (\DescribeChannel' {chimeBearer} -> chimeBearer) (\s@DescribeChannel' {} a -> s {chimeBearer = a} :: DescribeChannel)

-- | The ARN of the channel.
describeChannel_channelArn :: Lens.Lens' DescribeChannel Prelude.Text
describeChannel_channelArn = Lens.lens (\DescribeChannel' {channelArn} -> channelArn) (\s@DescribeChannel' {} a -> s {channelArn = a} :: DescribeChannel)

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
            Prelude.<$> (x Data..?> "Channel")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeChannel where
  hashWithSalt _salt DescribeChannel' {..} =
    _salt
      `Prelude.hashWithSalt` chimeBearer
      `Prelude.hashWithSalt` channelArn

instance Prelude.NFData DescribeChannel where
  rnf DescribeChannel' {..} =
    Prelude.rnf chimeBearer
      `Prelude.seq` Prelude.rnf channelArn

instance Data.ToHeaders DescribeChannel where
  toHeaders DescribeChannel' {..} =
    Prelude.mconcat
      ["x-amz-chime-bearer" Data.=# chimeBearer]

instance Data.ToPath DescribeChannel where
  toPath DescribeChannel' {..} =
    Prelude.mconcat
      ["/channels/", Data.toBS channelArn]

instance Data.ToQuery DescribeChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeChannelResponse' smart constructor.
data DescribeChannelResponse = DescribeChannelResponse'
  { -- | The channel details.
    channel :: Prelude.Maybe Channel,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channel', 'describeChannelResponse_channel' - The channel details.
--
-- 'httpStatus', 'describeChannelResponse_httpStatus' - The response's http status code.
newDescribeChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeChannelResponse
newDescribeChannelResponse pHttpStatus_ =
  DescribeChannelResponse'
    { channel = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The channel details.
describeChannelResponse_channel :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Channel)
describeChannelResponse_channel = Lens.lens (\DescribeChannelResponse' {channel} -> channel) (\s@DescribeChannelResponse' {} a -> s {channel = a} :: DescribeChannelResponse)

-- | The response's http status code.
describeChannelResponse_httpStatus :: Lens.Lens' DescribeChannelResponse Prelude.Int
describeChannelResponse_httpStatus = Lens.lens (\DescribeChannelResponse' {httpStatus} -> httpStatus) (\s@DescribeChannelResponse' {} a -> s {httpStatus = a} :: DescribeChannelResponse)

instance Prelude.NFData DescribeChannelResponse where
  rnf DescribeChannelResponse' {..} =
    Prelude.rnf channel
      `Prelude.seq` Prelude.rnf httpStatus
