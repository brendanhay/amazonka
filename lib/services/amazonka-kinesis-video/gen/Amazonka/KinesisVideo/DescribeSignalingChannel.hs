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
-- Module      : Amazonka.KinesisVideo.DescribeSignalingChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the most current information about the signaling channel. You
-- must specify either the name or the Amazon Resource Name (ARN) of the
-- channel that you want to describe.
module Amazonka.KinesisVideo.DescribeSignalingChannel
  ( -- * Creating a Request
    DescribeSignalingChannel (..),
    newDescribeSignalingChannel,

    -- * Request Lenses
    describeSignalingChannel_channelARN,
    describeSignalingChannel_channelName,

    -- * Destructuring the Response
    DescribeSignalingChannelResponse (..),
    newDescribeSignalingChannelResponse,

    -- * Response Lenses
    describeSignalingChannelResponse_channelInfo,
    describeSignalingChannelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideo.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeSignalingChannel' smart constructor.
data DescribeSignalingChannel = DescribeSignalingChannel'
  { -- | The ARN of the signaling channel that you want to describe.
    channelARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the signaling channel that you want to describe.
    channelName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSignalingChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelARN', 'describeSignalingChannel_channelARN' - The ARN of the signaling channel that you want to describe.
--
-- 'channelName', 'describeSignalingChannel_channelName' - The name of the signaling channel that you want to describe.
newDescribeSignalingChannel ::
  DescribeSignalingChannel
newDescribeSignalingChannel =
  DescribeSignalingChannel'
    { channelARN =
        Prelude.Nothing,
      channelName = Prelude.Nothing
    }

-- | The ARN of the signaling channel that you want to describe.
describeSignalingChannel_channelARN :: Lens.Lens' DescribeSignalingChannel (Prelude.Maybe Prelude.Text)
describeSignalingChannel_channelARN = Lens.lens (\DescribeSignalingChannel' {channelARN} -> channelARN) (\s@DescribeSignalingChannel' {} a -> s {channelARN = a} :: DescribeSignalingChannel)

-- | The name of the signaling channel that you want to describe.
describeSignalingChannel_channelName :: Lens.Lens' DescribeSignalingChannel (Prelude.Maybe Prelude.Text)
describeSignalingChannel_channelName = Lens.lens (\DescribeSignalingChannel' {channelName} -> channelName) (\s@DescribeSignalingChannel' {} a -> s {channelName = a} :: DescribeSignalingChannel)

instance Core.AWSRequest DescribeSignalingChannel where
  type
    AWSResponse DescribeSignalingChannel =
      DescribeSignalingChannelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSignalingChannelResponse'
            Prelude.<$> (x Data..?> "ChannelInfo")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSignalingChannel where
  hashWithSalt _salt DescribeSignalingChannel' {..} =
    _salt `Prelude.hashWithSalt` channelARN
      `Prelude.hashWithSalt` channelName

instance Prelude.NFData DescribeSignalingChannel where
  rnf DescribeSignalingChannel' {..} =
    Prelude.rnf channelARN
      `Prelude.seq` Prelude.rnf channelName

instance Data.ToHeaders DescribeSignalingChannel where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON DescribeSignalingChannel where
  toJSON DescribeSignalingChannel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ChannelARN" Data..=) Prelude.<$> channelARN,
            ("ChannelName" Data..=) Prelude.<$> channelName
          ]
      )

instance Data.ToPath DescribeSignalingChannel where
  toPath = Prelude.const "/describeSignalingChannel"

instance Data.ToQuery DescribeSignalingChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSignalingChannelResponse' smart constructor.
data DescribeSignalingChannelResponse = DescribeSignalingChannelResponse'
  { -- | A structure that encapsulates the specified signaling channel\'s
    -- metadata and properties.
    channelInfo :: Prelude.Maybe ChannelInfo,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeSignalingChannelResponse
newDescribeSignalingChannelResponse pHttpStatus_ =
  DescribeSignalingChannelResponse'
    { channelInfo =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that encapsulates the specified signaling channel\'s
-- metadata and properties.
describeSignalingChannelResponse_channelInfo :: Lens.Lens' DescribeSignalingChannelResponse (Prelude.Maybe ChannelInfo)
describeSignalingChannelResponse_channelInfo = Lens.lens (\DescribeSignalingChannelResponse' {channelInfo} -> channelInfo) (\s@DescribeSignalingChannelResponse' {} a -> s {channelInfo = a} :: DescribeSignalingChannelResponse)

-- | The response's http status code.
describeSignalingChannelResponse_httpStatus :: Lens.Lens' DescribeSignalingChannelResponse Prelude.Int
describeSignalingChannelResponse_httpStatus = Lens.lens (\DescribeSignalingChannelResponse' {httpStatus} -> httpStatus) (\s@DescribeSignalingChannelResponse' {} a -> s {httpStatus = a} :: DescribeSignalingChannelResponse)

instance
  Prelude.NFData
    DescribeSignalingChannelResponse
  where
  rnf DescribeSignalingChannelResponse' {..} =
    Prelude.rnf channelInfo
      `Prelude.seq` Prelude.rnf httpStatus
