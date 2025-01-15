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
-- Module      : Amazonka.ChimeSDKMessaging.CreateChannelMembership
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a user to a channel. The @InvitedBy@ field in @ChannelMembership@
-- is derived from the request header. A channel member can:
--
-- -   List messages
--
-- -   Send messages
--
-- -   Receive messages
--
-- -   Edit their own messages
--
-- -   Leave the channel
--
-- Privacy settings impact this action as follows:
--
-- -   Public Channels: You do not need to be a member to list messages,
--     but you must be a member to send messages.
--
-- -   Private Channels: You must be a member to list or send messages.
--
-- The @x-amz-chime-bearer@ request header is mandatory. Use the
-- @AppInstanceUserArn@ of the user that makes the API call as the value in
-- the header.
module Amazonka.ChimeSDKMessaging.CreateChannelMembership
  ( -- * Creating a Request
    CreateChannelMembership (..),
    newCreateChannelMembership,

    -- * Request Lenses
    createChannelMembership_subChannelId,
    createChannelMembership_channelArn,
    createChannelMembership_memberArn,
    createChannelMembership_type,
    createChannelMembership_chimeBearer,

    -- * Destructuring the Response
    CreateChannelMembershipResponse (..),
    newCreateChannelMembershipResponse,

    -- * Response Lenses
    createChannelMembershipResponse_channelArn,
    createChannelMembershipResponse_member,
    createChannelMembershipResponse_subChannelId,
    createChannelMembershipResponse_httpStatus,
  )
where

import Amazonka.ChimeSDKMessaging.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateChannelMembership' smart constructor.
data CreateChannelMembership = CreateChannelMembership'
  { -- | The ID of the SubChannel in the request.
    --
    -- Only required when creating membership in a SubChannel for a moderator
    -- in an elastic channel.
    subChannelId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the channel to which you\'re adding users.
    channelArn :: Prelude.Text,
    -- | The @AppInstanceUserArn@ of the member you want to add to the channel.
    memberArn :: Prelude.Text,
    -- | The membership type of a user, @DEFAULT@ or @HIDDEN@. Default members
    -- are always returned as part of @ListChannelMemberships@. Hidden members
    -- are only returned if the type filter in @ListChannelMemberships@ equals
    -- @HIDDEN@. Otherwise hidden members are not returned. This is only
    -- supported by moderators.
    type' :: ChannelMembershipType,
    -- | The @AppInstanceUserArn@ of the user that makes the API call.
    chimeBearer :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateChannelMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subChannelId', 'createChannelMembership_subChannelId' - The ID of the SubChannel in the request.
--
-- Only required when creating membership in a SubChannel for a moderator
-- in an elastic channel.
--
-- 'channelArn', 'createChannelMembership_channelArn' - The ARN of the channel to which you\'re adding users.
--
-- 'memberArn', 'createChannelMembership_memberArn' - The @AppInstanceUserArn@ of the member you want to add to the channel.
--
-- 'type'', 'createChannelMembership_type' - The membership type of a user, @DEFAULT@ or @HIDDEN@. Default members
-- are always returned as part of @ListChannelMemberships@. Hidden members
-- are only returned if the type filter in @ListChannelMemberships@ equals
-- @HIDDEN@. Otherwise hidden members are not returned. This is only
-- supported by moderators.
--
-- 'chimeBearer', 'createChannelMembership_chimeBearer' - The @AppInstanceUserArn@ of the user that makes the API call.
newCreateChannelMembership ::
  -- | 'channelArn'
  Prelude.Text ->
  -- | 'memberArn'
  Prelude.Text ->
  -- | 'type''
  ChannelMembershipType ->
  -- | 'chimeBearer'
  Prelude.Text ->
  CreateChannelMembership
newCreateChannelMembership
  pChannelArn_
  pMemberArn_
  pType_
  pChimeBearer_ =
    CreateChannelMembership'
      { subChannelId =
          Prelude.Nothing,
        channelArn = pChannelArn_,
        memberArn = pMemberArn_,
        type' = pType_,
        chimeBearer = pChimeBearer_
      }

-- | The ID of the SubChannel in the request.
--
-- Only required when creating membership in a SubChannel for a moderator
-- in an elastic channel.
createChannelMembership_subChannelId :: Lens.Lens' CreateChannelMembership (Prelude.Maybe Prelude.Text)
createChannelMembership_subChannelId = Lens.lens (\CreateChannelMembership' {subChannelId} -> subChannelId) (\s@CreateChannelMembership' {} a -> s {subChannelId = a} :: CreateChannelMembership)

-- | The ARN of the channel to which you\'re adding users.
createChannelMembership_channelArn :: Lens.Lens' CreateChannelMembership Prelude.Text
createChannelMembership_channelArn = Lens.lens (\CreateChannelMembership' {channelArn} -> channelArn) (\s@CreateChannelMembership' {} a -> s {channelArn = a} :: CreateChannelMembership)

-- | The @AppInstanceUserArn@ of the member you want to add to the channel.
createChannelMembership_memberArn :: Lens.Lens' CreateChannelMembership Prelude.Text
createChannelMembership_memberArn = Lens.lens (\CreateChannelMembership' {memberArn} -> memberArn) (\s@CreateChannelMembership' {} a -> s {memberArn = a} :: CreateChannelMembership)

-- | The membership type of a user, @DEFAULT@ or @HIDDEN@. Default members
-- are always returned as part of @ListChannelMemberships@. Hidden members
-- are only returned if the type filter in @ListChannelMemberships@ equals
-- @HIDDEN@. Otherwise hidden members are not returned. This is only
-- supported by moderators.
createChannelMembership_type :: Lens.Lens' CreateChannelMembership ChannelMembershipType
createChannelMembership_type = Lens.lens (\CreateChannelMembership' {type'} -> type') (\s@CreateChannelMembership' {} a -> s {type' = a} :: CreateChannelMembership)

-- | The @AppInstanceUserArn@ of the user that makes the API call.
createChannelMembership_chimeBearer :: Lens.Lens' CreateChannelMembership Prelude.Text
createChannelMembership_chimeBearer = Lens.lens (\CreateChannelMembership' {chimeBearer} -> chimeBearer) (\s@CreateChannelMembership' {} a -> s {chimeBearer = a} :: CreateChannelMembership)

instance Core.AWSRequest CreateChannelMembership where
  type
    AWSResponse CreateChannelMembership =
      CreateChannelMembershipResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateChannelMembershipResponse'
            Prelude.<$> (x Data..?> "ChannelArn")
            Prelude.<*> (x Data..?> "Member")
            Prelude.<*> (x Data..?> "SubChannelId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateChannelMembership where
  hashWithSalt _salt CreateChannelMembership' {..} =
    _salt
      `Prelude.hashWithSalt` subChannelId
      `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` memberArn
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` chimeBearer

instance Prelude.NFData CreateChannelMembership where
  rnf CreateChannelMembership' {..} =
    Prelude.rnf subChannelId `Prelude.seq`
      Prelude.rnf channelArn `Prelude.seq`
        Prelude.rnf memberArn `Prelude.seq`
          Prelude.rnf type' `Prelude.seq`
            Prelude.rnf chimeBearer

instance Data.ToHeaders CreateChannelMembership where
  toHeaders CreateChannelMembership' {..} =
    Prelude.mconcat
      ["x-amz-chime-bearer" Data.=# chimeBearer]

instance Data.ToJSON CreateChannelMembership where
  toJSON CreateChannelMembership' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SubChannelId" Data..=) Prelude.<$> subChannelId,
            Prelude.Just ("MemberArn" Data..= memberArn),
            Prelude.Just ("Type" Data..= type')
          ]
      )

instance Data.ToPath CreateChannelMembership where
  toPath CreateChannelMembership' {..} =
    Prelude.mconcat
      ["/channels/", Data.toBS channelArn, "/memberships"]

instance Data.ToQuery CreateChannelMembership where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateChannelMembershipResponse' smart constructor.
data CreateChannelMembershipResponse = CreateChannelMembershipResponse'
  { -- | The ARN of the channel.
    channelArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN and metadata of the member being added.
    member :: Prelude.Maybe Identity,
    -- | The ID of the SubChannel in the response.
    subChannelId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateChannelMembershipResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelArn', 'createChannelMembershipResponse_channelArn' - The ARN of the channel.
--
-- 'member', 'createChannelMembershipResponse_member' - The ARN and metadata of the member being added.
--
-- 'subChannelId', 'createChannelMembershipResponse_subChannelId' - The ID of the SubChannel in the response.
--
-- 'httpStatus', 'createChannelMembershipResponse_httpStatus' - The response's http status code.
newCreateChannelMembershipResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateChannelMembershipResponse
newCreateChannelMembershipResponse pHttpStatus_ =
  CreateChannelMembershipResponse'
    { channelArn =
        Prelude.Nothing,
      member = Prelude.Nothing,
      subChannelId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the channel.
createChannelMembershipResponse_channelArn :: Lens.Lens' CreateChannelMembershipResponse (Prelude.Maybe Prelude.Text)
createChannelMembershipResponse_channelArn = Lens.lens (\CreateChannelMembershipResponse' {channelArn} -> channelArn) (\s@CreateChannelMembershipResponse' {} a -> s {channelArn = a} :: CreateChannelMembershipResponse)

-- | The ARN and metadata of the member being added.
createChannelMembershipResponse_member :: Lens.Lens' CreateChannelMembershipResponse (Prelude.Maybe Identity)
createChannelMembershipResponse_member = Lens.lens (\CreateChannelMembershipResponse' {member} -> member) (\s@CreateChannelMembershipResponse' {} a -> s {member = a} :: CreateChannelMembershipResponse)

-- | The ID of the SubChannel in the response.
createChannelMembershipResponse_subChannelId :: Lens.Lens' CreateChannelMembershipResponse (Prelude.Maybe Prelude.Text)
createChannelMembershipResponse_subChannelId = Lens.lens (\CreateChannelMembershipResponse' {subChannelId} -> subChannelId) (\s@CreateChannelMembershipResponse' {} a -> s {subChannelId = a} :: CreateChannelMembershipResponse)

-- | The response's http status code.
createChannelMembershipResponse_httpStatus :: Lens.Lens' CreateChannelMembershipResponse Prelude.Int
createChannelMembershipResponse_httpStatus = Lens.lens (\CreateChannelMembershipResponse' {httpStatus} -> httpStatus) (\s@CreateChannelMembershipResponse' {} a -> s {httpStatus = a} :: CreateChannelMembershipResponse)

instance
  Prelude.NFData
    CreateChannelMembershipResponse
  where
  rnf CreateChannelMembershipResponse' {..} =
    Prelude.rnf channelArn `Prelude.seq`
      Prelude.rnf member `Prelude.seq`
        Prelude.rnf subChannelId `Prelude.seq`
          Prelude.rnf httpStatus
