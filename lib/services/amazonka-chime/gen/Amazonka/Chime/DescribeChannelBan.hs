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
-- Module      : Amazonka.Chime.DescribeChannelBan
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the full details of a channel ban.
--
-- The @x-amz-chime-bearer@ request header is mandatory. Use the
-- @AppInstanceUserArn@ of the user that makes the API call as the value in
-- the header.
module Amazonka.Chime.DescribeChannelBan
  ( -- * Creating a Request
    DescribeChannelBan (..),
    newDescribeChannelBan,

    -- * Request Lenses
    describeChannelBan_chimeBearer,
    describeChannelBan_channelArn,
    describeChannelBan_memberArn,

    -- * Destructuring the Response
    DescribeChannelBanResponse (..),
    newDescribeChannelBanResponse,

    -- * Response Lenses
    describeChannelBanResponse_channelBan,
    describeChannelBanResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeChannelBan' smart constructor.
data DescribeChannelBan = DescribeChannelBan'
  { -- | The @AppInstanceUserArn@ of the user that makes the API call.
    chimeBearer :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the channel from which the user is banned.
    channelArn :: Prelude.Text,
    -- | The ARN of the member being banned.
    memberArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeChannelBan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'chimeBearer', 'describeChannelBan_chimeBearer' - The @AppInstanceUserArn@ of the user that makes the API call.
--
-- 'channelArn', 'describeChannelBan_channelArn' - The ARN of the channel from which the user is banned.
--
-- 'memberArn', 'describeChannelBan_memberArn' - The ARN of the member being banned.
newDescribeChannelBan ::
  -- | 'channelArn'
  Prelude.Text ->
  -- | 'memberArn'
  Prelude.Text ->
  DescribeChannelBan
newDescribeChannelBan pChannelArn_ pMemberArn_ =
  DescribeChannelBan'
    { chimeBearer = Prelude.Nothing,
      channelArn = pChannelArn_,
      memberArn = pMemberArn_
    }

-- | The @AppInstanceUserArn@ of the user that makes the API call.
describeChannelBan_chimeBearer :: Lens.Lens' DescribeChannelBan (Prelude.Maybe Prelude.Text)
describeChannelBan_chimeBearer = Lens.lens (\DescribeChannelBan' {chimeBearer} -> chimeBearer) (\s@DescribeChannelBan' {} a -> s {chimeBearer = a} :: DescribeChannelBan)

-- | The ARN of the channel from which the user is banned.
describeChannelBan_channelArn :: Lens.Lens' DescribeChannelBan Prelude.Text
describeChannelBan_channelArn = Lens.lens (\DescribeChannelBan' {channelArn} -> channelArn) (\s@DescribeChannelBan' {} a -> s {channelArn = a} :: DescribeChannelBan)

-- | The ARN of the member being banned.
describeChannelBan_memberArn :: Lens.Lens' DescribeChannelBan Prelude.Text
describeChannelBan_memberArn = Lens.lens (\DescribeChannelBan' {memberArn} -> memberArn) (\s@DescribeChannelBan' {} a -> s {memberArn = a} :: DescribeChannelBan)

instance Core.AWSRequest DescribeChannelBan where
  type
    AWSResponse DescribeChannelBan =
      DescribeChannelBanResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeChannelBanResponse'
            Prelude.<$> (x Data..?> "ChannelBan")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeChannelBan where
  hashWithSalt _salt DescribeChannelBan' {..} =
    _salt `Prelude.hashWithSalt` chimeBearer
      `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` memberArn

instance Prelude.NFData DescribeChannelBan where
  rnf DescribeChannelBan' {..} =
    Prelude.rnf chimeBearer
      `Prelude.seq` Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf memberArn

instance Data.ToHeaders DescribeChannelBan where
  toHeaders DescribeChannelBan' {..} =
    Prelude.mconcat
      ["x-amz-chime-bearer" Data.=# chimeBearer]

instance Data.ToPath DescribeChannelBan where
  toPath DescribeChannelBan' {..} =
    Prelude.mconcat
      [ "/channels/",
        Data.toBS channelArn,
        "/bans/",
        Data.toBS memberArn
      ]

instance Data.ToQuery DescribeChannelBan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeChannelBanResponse' smart constructor.
data DescribeChannelBanResponse = DescribeChannelBanResponse'
  { -- | The details of the ban.
    channelBan :: Prelude.Maybe ChannelBan,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeChannelBanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelBan', 'describeChannelBanResponse_channelBan' - The details of the ban.
--
-- 'httpStatus', 'describeChannelBanResponse_httpStatus' - The response's http status code.
newDescribeChannelBanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeChannelBanResponse
newDescribeChannelBanResponse pHttpStatus_ =
  DescribeChannelBanResponse'
    { channelBan =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the ban.
describeChannelBanResponse_channelBan :: Lens.Lens' DescribeChannelBanResponse (Prelude.Maybe ChannelBan)
describeChannelBanResponse_channelBan = Lens.lens (\DescribeChannelBanResponse' {channelBan} -> channelBan) (\s@DescribeChannelBanResponse' {} a -> s {channelBan = a} :: DescribeChannelBanResponse)

-- | The response's http status code.
describeChannelBanResponse_httpStatus :: Lens.Lens' DescribeChannelBanResponse Prelude.Int
describeChannelBanResponse_httpStatus = Lens.lens (\DescribeChannelBanResponse' {httpStatus} -> httpStatus) (\s@DescribeChannelBanResponse' {} a -> s {httpStatus = a} :: DescribeChannelBanResponse)

instance Prelude.NFData DescribeChannelBanResponse where
  rnf DescribeChannelBanResponse' {..} =
    Prelude.rnf channelBan
      `Prelude.seq` Prelude.rnf httpStatus
