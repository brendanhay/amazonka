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
-- Module      : Amazonka.ChimeSDKMessaging.CreateChannelBan
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently bans a member from a channel. Moderators can\'t add banned
-- members to a channel. To undo a ban, you first have to
-- @DeleteChannelBan@, and then @CreateChannelMembership@. Bans are cleaned
-- up when you delete users or channels.
--
-- If you ban a user who is already part of a channel, that user is
-- automatically kicked from the channel.
--
-- The @x-amz-chime-bearer@ request header is mandatory. Use the
-- @AppInstanceUserArn@ of the user that makes the API call as the value in
-- the header.
module Amazonka.ChimeSDKMessaging.CreateChannelBan
  ( -- * Creating a Request
    CreateChannelBan (..),
    newCreateChannelBan,

    -- * Request Lenses
    createChannelBan_channelArn,
    createChannelBan_memberArn,
    createChannelBan_chimeBearer,

    -- * Destructuring the Response
    CreateChannelBanResponse (..),
    newCreateChannelBanResponse,

    -- * Response Lenses
    createChannelBanResponse_channelArn,
    createChannelBanResponse_member,
    createChannelBanResponse_httpStatus,
  )
where

import Amazonka.ChimeSDKMessaging.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateChannelBan' smart constructor.
data CreateChannelBan = CreateChannelBan'
  { -- | The ARN of the ban request.
    channelArn :: Prelude.Text,
    -- | The @AppInstanceUserArn@ of the member being banned.
    memberArn :: Prelude.Text,
    -- | The @AppInstanceUserArn@ of the user that makes the API call.
    chimeBearer :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateChannelBan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelArn', 'createChannelBan_channelArn' - The ARN of the ban request.
--
-- 'memberArn', 'createChannelBan_memberArn' - The @AppInstanceUserArn@ of the member being banned.
--
-- 'chimeBearer', 'createChannelBan_chimeBearer' - The @AppInstanceUserArn@ of the user that makes the API call.
newCreateChannelBan ::
  -- | 'channelArn'
  Prelude.Text ->
  -- | 'memberArn'
  Prelude.Text ->
  -- | 'chimeBearer'
  Prelude.Text ->
  CreateChannelBan
newCreateChannelBan
  pChannelArn_
  pMemberArn_
  pChimeBearer_ =
    CreateChannelBan'
      { channelArn = pChannelArn_,
        memberArn = pMemberArn_,
        chimeBearer = pChimeBearer_
      }

-- | The ARN of the ban request.
createChannelBan_channelArn :: Lens.Lens' CreateChannelBan Prelude.Text
createChannelBan_channelArn = Lens.lens (\CreateChannelBan' {channelArn} -> channelArn) (\s@CreateChannelBan' {} a -> s {channelArn = a} :: CreateChannelBan)

-- | The @AppInstanceUserArn@ of the member being banned.
createChannelBan_memberArn :: Lens.Lens' CreateChannelBan Prelude.Text
createChannelBan_memberArn = Lens.lens (\CreateChannelBan' {memberArn} -> memberArn) (\s@CreateChannelBan' {} a -> s {memberArn = a} :: CreateChannelBan)

-- | The @AppInstanceUserArn@ of the user that makes the API call.
createChannelBan_chimeBearer :: Lens.Lens' CreateChannelBan Prelude.Text
createChannelBan_chimeBearer = Lens.lens (\CreateChannelBan' {chimeBearer} -> chimeBearer) (\s@CreateChannelBan' {} a -> s {chimeBearer = a} :: CreateChannelBan)

instance Core.AWSRequest CreateChannelBan where
  type
    AWSResponse CreateChannelBan =
      CreateChannelBanResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateChannelBanResponse'
            Prelude.<$> (x Data..?> "ChannelArn")
            Prelude.<*> (x Data..?> "Member")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateChannelBan where
  hashWithSalt _salt CreateChannelBan' {..} =
    _salt `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` memberArn
      `Prelude.hashWithSalt` chimeBearer

instance Prelude.NFData CreateChannelBan where
  rnf CreateChannelBan' {..} =
    Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf memberArn
      `Prelude.seq` Prelude.rnf chimeBearer

instance Data.ToHeaders CreateChannelBan where
  toHeaders CreateChannelBan' {..} =
    Prelude.mconcat
      ["x-amz-chime-bearer" Data.=# chimeBearer]

instance Data.ToJSON CreateChannelBan where
  toJSON CreateChannelBan' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("MemberArn" Data..= memberArn)]
      )

instance Data.ToPath CreateChannelBan where
  toPath CreateChannelBan' {..} =
    Prelude.mconcat
      ["/channels/", Data.toBS channelArn, "/bans"]

instance Data.ToQuery CreateChannelBan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateChannelBanResponse' smart constructor.
data CreateChannelBanResponse = CreateChannelBanResponse'
  { -- | The ARN of the response to the ban request.
    channelArn :: Prelude.Maybe Prelude.Text,
    -- | The @ChannelArn@ and @BannedIdentity@ of the member in the ban response.
    member :: Prelude.Maybe Identity,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateChannelBanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelArn', 'createChannelBanResponse_channelArn' - The ARN of the response to the ban request.
--
-- 'member', 'createChannelBanResponse_member' - The @ChannelArn@ and @BannedIdentity@ of the member in the ban response.
--
-- 'httpStatus', 'createChannelBanResponse_httpStatus' - The response's http status code.
newCreateChannelBanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateChannelBanResponse
newCreateChannelBanResponse pHttpStatus_ =
  CreateChannelBanResponse'
    { channelArn =
        Prelude.Nothing,
      member = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the response to the ban request.
createChannelBanResponse_channelArn :: Lens.Lens' CreateChannelBanResponse (Prelude.Maybe Prelude.Text)
createChannelBanResponse_channelArn = Lens.lens (\CreateChannelBanResponse' {channelArn} -> channelArn) (\s@CreateChannelBanResponse' {} a -> s {channelArn = a} :: CreateChannelBanResponse)

-- | The @ChannelArn@ and @BannedIdentity@ of the member in the ban response.
createChannelBanResponse_member :: Lens.Lens' CreateChannelBanResponse (Prelude.Maybe Identity)
createChannelBanResponse_member = Lens.lens (\CreateChannelBanResponse' {member} -> member) (\s@CreateChannelBanResponse' {} a -> s {member = a} :: CreateChannelBanResponse)

-- | The response's http status code.
createChannelBanResponse_httpStatus :: Lens.Lens' CreateChannelBanResponse Prelude.Int
createChannelBanResponse_httpStatus = Lens.lens (\CreateChannelBanResponse' {httpStatus} -> httpStatus) (\s@CreateChannelBanResponse' {} a -> s {httpStatus = a} :: CreateChannelBanResponse)

instance Prelude.NFData CreateChannelBanResponse where
  rnf CreateChannelBanResponse' {..} =
    Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf member
      `Prelude.seq` Prelude.rnf httpStatus
