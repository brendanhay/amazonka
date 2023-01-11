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
-- Module      : Amazonka.Chime.DescribeChannelMembershipForAppInstanceUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of a channel based on the membership of the
-- specified @AppInstanceUser@.
--
-- The @x-amz-chime-bearer@ request header is mandatory. Use the
-- @AppInstanceUserArn@ of the user that makes the API call as the value in
-- the header.
module Amazonka.Chime.DescribeChannelMembershipForAppInstanceUser
  ( -- * Creating a Request
    DescribeChannelMembershipForAppInstanceUser (..),
    newDescribeChannelMembershipForAppInstanceUser,

    -- * Request Lenses
    describeChannelMembershipForAppInstanceUser_chimeBearer,
    describeChannelMembershipForAppInstanceUser_channelArn,
    describeChannelMembershipForAppInstanceUser_appInstanceUserArn,

    -- * Destructuring the Response
    DescribeChannelMembershipForAppInstanceUserResponse (..),
    newDescribeChannelMembershipForAppInstanceUserResponse,

    -- * Response Lenses
    describeChannelMembershipForAppInstanceUserResponse_channelMembership,
    describeChannelMembershipForAppInstanceUserResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeChannelMembershipForAppInstanceUser' smart constructor.
data DescribeChannelMembershipForAppInstanceUser = DescribeChannelMembershipForAppInstanceUser'
  { -- | The @AppInstanceUserArn@ of the user that makes the API call.
    chimeBearer :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the channel to which the user belongs.
    channelArn :: Prelude.Text,
    -- | The ARN of the user in a channel.
    appInstanceUserArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeChannelMembershipForAppInstanceUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'chimeBearer', 'describeChannelMembershipForAppInstanceUser_chimeBearer' - The @AppInstanceUserArn@ of the user that makes the API call.
--
-- 'channelArn', 'describeChannelMembershipForAppInstanceUser_channelArn' - The ARN of the channel to which the user belongs.
--
-- 'appInstanceUserArn', 'describeChannelMembershipForAppInstanceUser_appInstanceUserArn' - The ARN of the user in a channel.
newDescribeChannelMembershipForAppInstanceUser ::
  -- | 'channelArn'
  Prelude.Text ->
  -- | 'appInstanceUserArn'
  Prelude.Text ->
  DescribeChannelMembershipForAppInstanceUser
newDescribeChannelMembershipForAppInstanceUser
  pChannelArn_
  pAppInstanceUserArn_ =
    DescribeChannelMembershipForAppInstanceUser'
      { chimeBearer =
          Prelude.Nothing,
        channelArn = pChannelArn_,
        appInstanceUserArn =
          pAppInstanceUserArn_
      }

-- | The @AppInstanceUserArn@ of the user that makes the API call.
describeChannelMembershipForAppInstanceUser_chimeBearer :: Lens.Lens' DescribeChannelMembershipForAppInstanceUser (Prelude.Maybe Prelude.Text)
describeChannelMembershipForAppInstanceUser_chimeBearer = Lens.lens (\DescribeChannelMembershipForAppInstanceUser' {chimeBearer} -> chimeBearer) (\s@DescribeChannelMembershipForAppInstanceUser' {} a -> s {chimeBearer = a} :: DescribeChannelMembershipForAppInstanceUser)

-- | The ARN of the channel to which the user belongs.
describeChannelMembershipForAppInstanceUser_channelArn :: Lens.Lens' DescribeChannelMembershipForAppInstanceUser Prelude.Text
describeChannelMembershipForAppInstanceUser_channelArn = Lens.lens (\DescribeChannelMembershipForAppInstanceUser' {channelArn} -> channelArn) (\s@DescribeChannelMembershipForAppInstanceUser' {} a -> s {channelArn = a} :: DescribeChannelMembershipForAppInstanceUser)

-- | The ARN of the user in a channel.
describeChannelMembershipForAppInstanceUser_appInstanceUserArn :: Lens.Lens' DescribeChannelMembershipForAppInstanceUser Prelude.Text
describeChannelMembershipForAppInstanceUser_appInstanceUserArn = Lens.lens (\DescribeChannelMembershipForAppInstanceUser' {appInstanceUserArn} -> appInstanceUserArn) (\s@DescribeChannelMembershipForAppInstanceUser' {} a -> s {appInstanceUserArn = a} :: DescribeChannelMembershipForAppInstanceUser)

instance
  Core.AWSRequest
    DescribeChannelMembershipForAppInstanceUser
  where
  type
    AWSResponse
      DescribeChannelMembershipForAppInstanceUser =
      DescribeChannelMembershipForAppInstanceUserResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeChannelMembershipForAppInstanceUserResponse'
            Prelude.<$> (x Data..?> "ChannelMembership")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeChannelMembershipForAppInstanceUser
  where
  hashWithSalt
    _salt
    DescribeChannelMembershipForAppInstanceUser' {..} =
      _salt `Prelude.hashWithSalt` chimeBearer
        `Prelude.hashWithSalt` channelArn
        `Prelude.hashWithSalt` appInstanceUserArn

instance
  Prelude.NFData
    DescribeChannelMembershipForAppInstanceUser
  where
  rnf DescribeChannelMembershipForAppInstanceUser' {..} =
    Prelude.rnf chimeBearer
      `Prelude.seq` Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf appInstanceUserArn

instance
  Data.ToHeaders
    DescribeChannelMembershipForAppInstanceUser
  where
  toHeaders
    DescribeChannelMembershipForAppInstanceUser' {..} =
      Prelude.mconcat
        ["x-amz-chime-bearer" Data.=# chimeBearer]

instance
  Data.ToPath
    DescribeChannelMembershipForAppInstanceUser
  where
  toPath
    DescribeChannelMembershipForAppInstanceUser' {..} =
      Prelude.mconcat
        ["/channels/", Data.toBS channelArn]

instance
  Data.ToQuery
    DescribeChannelMembershipForAppInstanceUser
  where
  toQuery
    DescribeChannelMembershipForAppInstanceUser' {..} =
      Prelude.mconcat
        [ "app-instance-user-arn" Data.=: appInstanceUserArn,
          "scope=app-instance-user-membership"
        ]

-- | /See:/ 'newDescribeChannelMembershipForAppInstanceUserResponse' smart constructor.
data DescribeChannelMembershipForAppInstanceUserResponse = DescribeChannelMembershipForAppInstanceUserResponse'
  { -- | The channel to which a user belongs.
    channelMembership :: Prelude.Maybe ChannelMembershipForAppInstanceUserSummary,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeChannelMembershipForAppInstanceUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelMembership', 'describeChannelMembershipForAppInstanceUserResponse_channelMembership' - The channel to which a user belongs.
--
-- 'httpStatus', 'describeChannelMembershipForAppInstanceUserResponse_httpStatus' - The response's http status code.
newDescribeChannelMembershipForAppInstanceUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeChannelMembershipForAppInstanceUserResponse
newDescribeChannelMembershipForAppInstanceUserResponse
  pHttpStatus_ =
    DescribeChannelMembershipForAppInstanceUserResponse'
      { channelMembership =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The channel to which a user belongs.
describeChannelMembershipForAppInstanceUserResponse_channelMembership :: Lens.Lens' DescribeChannelMembershipForAppInstanceUserResponse (Prelude.Maybe ChannelMembershipForAppInstanceUserSummary)
describeChannelMembershipForAppInstanceUserResponse_channelMembership = Lens.lens (\DescribeChannelMembershipForAppInstanceUserResponse' {channelMembership} -> channelMembership) (\s@DescribeChannelMembershipForAppInstanceUserResponse' {} a -> s {channelMembership = a} :: DescribeChannelMembershipForAppInstanceUserResponse)

-- | The response's http status code.
describeChannelMembershipForAppInstanceUserResponse_httpStatus :: Lens.Lens' DescribeChannelMembershipForAppInstanceUserResponse Prelude.Int
describeChannelMembershipForAppInstanceUserResponse_httpStatus = Lens.lens (\DescribeChannelMembershipForAppInstanceUserResponse' {httpStatus} -> httpStatus) (\s@DescribeChannelMembershipForAppInstanceUserResponse' {} a -> s {httpStatus = a} :: DescribeChannelMembershipForAppInstanceUserResponse)

instance
  Prelude.NFData
    DescribeChannelMembershipForAppInstanceUserResponse
  where
  rnf
    DescribeChannelMembershipForAppInstanceUserResponse' {..} =
      Prelude.rnf channelMembership
        `Prelude.seq` Prelude.rnf httpStatus
