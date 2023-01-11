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
-- Module      : Amazonka.Chime.DescribeChannelMembership
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the full details of a user\'s channel membership.
--
-- The @x-amz-chime-bearer@ request header is mandatory. Use the
-- @AppInstanceUserArn@ of the user that makes the API call as the value in
-- the header.
module Amazonka.Chime.DescribeChannelMembership
  ( -- * Creating a Request
    DescribeChannelMembership (..),
    newDescribeChannelMembership,

    -- * Request Lenses
    describeChannelMembership_chimeBearer,
    describeChannelMembership_channelArn,
    describeChannelMembership_memberArn,

    -- * Destructuring the Response
    DescribeChannelMembershipResponse (..),
    newDescribeChannelMembershipResponse,

    -- * Response Lenses
    describeChannelMembershipResponse_channelMembership,
    describeChannelMembershipResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeChannelMembership' smart constructor.
data DescribeChannelMembership = DescribeChannelMembership'
  { -- | The @AppInstanceUserArn@ of the user that makes the API call.
    chimeBearer :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the channel.
    channelArn :: Prelude.Text,
    -- | The ARN of the member.
    memberArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeChannelMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'chimeBearer', 'describeChannelMembership_chimeBearer' - The @AppInstanceUserArn@ of the user that makes the API call.
--
-- 'channelArn', 'describeChannelMembership_channelArn' - The ARN of the channel.
--
-- 'memberArn', 'describeChannelMembership_memberArn' - The ARN of the member.
newDescribeChannelMembership ::
  -- | 'channelArn'
  Prelude.Text ->
  -- | 'memberArn'
  Prelude.Text ->
  DescribeChannelMembership
newDescribeChannelMembership pChannelArn_ pMemberArn_ =
  DescribeChannelMembership'
    { chimeBearer =
        Prelude.Nothing,
      channelArn = pChannelArn_,
      memberArn = pMemberArn_
    }

-- | The @AppInstanceUserArn@ of the user that makes the API call.
describeChannelMembership_chimeBearer :: Lens.Lens' DescribeChannelMembership (Prelude.Maybe Prelude.Text)
describeChannelMembership_chimeBearer = Lens.lens (\DescribeChannelMembership' {chimeBearer} -> chimeBearer) (\s@DescribeChannelMembership' {} a -> s {chimeBearer = a} :: DescribeChannelMembership)

-- | The ARN of the channel.
describeChannelMembership_channelArn :: Lens.Lens' DescribeChannelMembership Prelude.Text
describeChannelMembership_channelArn = Lens.lens (\DescribeChannelMembership' {channelArn} -> channelArn) (\s@DescribeChannelMembership' {} a -> s {channelArn = a} :: DescribeChannelMembership)

-- | The ARN of the member.
describeChannelMembership_memberArn :: Lens.Lens' DescribeChannelMembership Prelude.Text
describeChannelMembership_memberArn = Lens.lens (\DescribeChannelMembership' {memberArn} -> memberArn) (\s@DescribeChannelMembership' {} a -> s {memberArn = a} :: DescribeChannelMembership)

instance Core.AWSRequest DescribeChannelMembership where
  type
    AWSResponse DescribeChannelMembership =
      DescribeChannelMembershipResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeChannelMembershipResponse'
            Prelude.<$> (x Data..?> "ChannelMembership")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeChannelMembership where
  hashWithSalt _salt DescribeChannelMembership' {..} =
    _salt `Prelude.hashWithSalt` chimeBearer
      `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` memberArn

instance Prelude.NFData DescribeChannelMembership where
  rnf DescribeChannelMembership' {..} =
    Prelude.rnf chimeBearer
      `Prelude.seq` Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf memberArn

instance Data.ToHeaders DescribeChannelMembership where
  toHeaders DescribeChannelMembership' {..} =
    Prelude.mconcat
      ["x-amz-chime-bearer" Data.=# chimeBearer]

instance Data.ToPath DescribeChannelMembership where
  toPath DescribeChannelMembership' {..} =
    Prelude.mconcat
      [ "/channels/",
        Data.toBS channelArn,
        "/memberships/",
        Data.toBS memberArn
      ]

instance Data.ToQuery DescribeChannelMembership where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeChannelMembershipResponse' smart constructor.
data DescribeChannelMembershipResponse = DescribeChannelMembershipResponse'
  { -- | The details of the membership.
    channelMembership :: Prelude.Maybe ChannelMembership,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeChannelMembershipResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelMembership', 'describeChannelMembershipResponse_channelMembership' - The details of the membership.
--
-- 'httpStatus', 'describeChannelMembershipResponse_httpStatus' - The response's http status code.
newDescribeChannelMembershipResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeChannelMembershipResponse
newDescribeChannelMembershipResponse pHttpStatus_ =
  DescribeChannelMembershipResponse'
    { channelMembership =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the membership.
describeChannelMembershipResponse_channelMembership :: Lens.Lens' DescribeChannelMembershipResponse (Prelude.Maybe ChannelMembership)
describeChannelMembershipResponse_channelMembership = Lens.lens (\DescribeChannelMembershipResponse' {channelMembership} -> channelMembership) (\s@DescribeChannelMembershipResponse' {} a -> s {channelMembership = a} :: DescribeChannelMembershipResponse)

-- | The response's http status code.
describeChannelMembershipResponse_httpStatus :: Lens.Lens' DescribeChannelMembershipResponse Prelude.Int
describeChannelMembershipResponse_httpStatus = Lens.lens (\DescribeChannelMembershipResponse' {httpStatus} -> httpStatus) (\s@DescribeChannelMembershipResponse' {} a -> s {httpStatus = a} :: DescribeChannelMembershipResponse)

instance
  Prelude.NFData
    DescribeChannelMembershipResponse
  where
  rnf DescribeChannelMembershipResponse' {..} =
    Prelude.rnf channelMembership
      `Prelude.seq` Prelude.rnf httpStatus
