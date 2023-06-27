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
-- Module      : Amazonka.Chime.CreateChannelMembership
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a user to a channel. The @InvitedBy@ response field is derived from
-- the request header. A channel member can:
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
module Amazonka.Chime.CreateChannelMembership
  ( -- * Creating a Request
    CreateChannelMembership (..),
    newCreateChannelMembership,

    -- * Request Lenses
    createChannelMembership_chimeBearer,
    createChannelMembership_channelArn,
    createChannelMembership_memberArn,
    createChannelMembership_type,

    -- * Destructuring the Response
    CreateChannelMembershipResponse (..),
    newCreateChannelMembershipResponse,

    -- * Response Lenses
    createChannelMembershipResponse_channelArn,
    createChannelMembershipResponse_member,
    createChannelMembershipResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateChannelMembership' smart constructor.
data CreateChannelMembership = CreateChannelMembership'
  { -- | The @AppInstanceUserArn@ of the user that makes the API call.
    chimeBearer :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the channel to which you\'re adding users.
    channelArn :: Prelude.Text,
    -- | The ARN of the member you want to add to the channel.
    memberArn :: Prelude.Text,
    -- | The membership type of a user, @DEFAULT@ or @HIDDEN@. Default members
    -- are always returned as part of @ListChannelMemberships@. Hidden members
    -- are only returned if the type filter in @ListChannelMemberships@ equals
    -- @HIDDEN@. Otherwise hidden members are not returned. This is only
    -- supported by moderators.
    type' :: ChannelMembershipType
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
-- 'chimeBearer', 'createChannelMembership_chimeBearer' - The @AppInstanceUserArn@ of the user that makes the API call.
--
-- 'channelArn', 'createChannelMembership_channelArn' - The ARN of the channel to which you\'re adding users.
--
-- 'memberArn', 'createChannelMembership_memberArn' - The ARN of the member you want to add to the channel.
--
-- 'type'', 'createChannelMembership_type' - The membership type of a user, @DEFAULT@ or @HIDDEN@. Default members
-- are always returned as part of @ListChannelMemberships@. Hidden members
-- are only returned if the type filter in @ListChannelMemberships@ equals
-- @HIDDEN@. Otherwise hidden members are not returned. This is only
-- supported by moderators.
newCreateChannelMembership ::
  -- | 'channelArn'
  Prelude.Text ->
  -- | 'memberArn'
  Prelude.Text ->
  -- | 'type''
  ChannelMembershipType ->
  CreateChannelMembership
newCreateChannelMembership
  pChannelArn_
  pMemberArn_
  pType_ =
    CreateChannelMembership'
      { chimeBearer =
          Prelude.Nothing,
        channelArn = pChannelArn_,
        memberArn = pMemberArn_,
        type' = pType_
      }

-- | The @AppInstanceUserArn@ of the user that makes the API call.
createChannelMembership_chimeBearer :: Lens.Lens' CreateChannelMembership (Prelude.Maybe Prelude.Text)
createChannelMembership_chimeBearer = Lens.lens (\CreateChannelMembership' {chimeBearer} -> chimeBearer) (\s@CreateChannelMembership' {} a -> s {chimeBearer = a} :: CreateChannelMembership)

-- | The ARN of the channel to which you\'re adding users.
createChannelMembership_channelArn :: Lens.Lens' CreateChannelMembership Prelude.Text
createChannelMembership_channelArn = Lens.lens (\CreateChannelMembership' {channelArn} -> channelArn) (\s@CreateChannelMembership' {} a -> s {channelArn = a} :: CreateChannelMembership)

-- | The ARN of the member you want to add to the channel.
createChannelMembership_memberArn :: Lens.Lens' CreateChannelMembership Prelude.Text
createChannelMembership_memberArn = Lens.lens (\CreateChannelMembership' {memberArn} -> memberArn) (\s@CreateChannelMembership' {} a -> s {memberArn = a} :: CreateChannelMembership)

-- | The membership type of a user, @DEFAULT@ or @HIDDEN@. Default members
-- are always returned as part of @ListChannelMemberships@. Hidden members
-- are only returned if the type filter in @ListChannelMemberships@ equals
-- @HIDDEN@. Otherwise hidden members are not returned. This is only
-- supported by moderators.
createChannelMembership_type :: Lens.Lens' CreateChannelMembership ChannelMembershipType
createChannelMembership_type = Lens.lens (\CreateChannelMembership' {type'} -> type') (\s@CreateChannelMembership' {} a -> s {type' = a} :: CreateChannelMembership)

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
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateChannelMembership where
  hashWithSalt _salt CreateChannelMembership' {..} =
    _salt
      `Prelude.hashWithSalt` chimeBearer
      `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` memberArn
      `Prelude.hashWithSalt` type'

instance Prelude.NFData CreateChannelMembership where
  rnf CreateChannelMembership' {..} =
    Prelude.rnf chimeBearer
      `Prelude.seq` Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf memberArn
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders CreateChannelMembership where
  toHeaders CreateChannelMembership' {..} =
    Prelude.mconcat
      ["x-amz-chime-bearer" Data.=# chimeBearer]

instance Data.ToJSON CreateChannelMembership where
  toJSON CreateChannelMembership' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("MemberArn" Data..= memberArn),
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
      httpStatus = pHttpStatus_
    }

-- | The ARN of the channel.
createChannelMembershipResponse_channelArn :: Lens.Lens' CreateChannelMembershipResponse (Prelude.Maybe Prelude.Text)
createChannelMembershipResponse_channelArn = Lens.lens (\CreateChannelMembershipResponse' {channelArn} -> channelArn) (\s@CreateChannelMembershipResponse' {} a -> s {channelArn = a} :: CreateChannelMembershipResponse)

-- | The ARN and metadata of the member being added.
createChannelMembershipResponse_member :: Lens.Lens' CreateChannelMembershipResponse (Prelude.Maybe Identity)
createChannelMembershipResponse_member = Lens.lens (\CreateChannelMembershipResponse' {member} -> member) (\s@CreateChannelMembershipResponse' {} a -> s {member = a} :: CreateChannelMembershipResponse)

-- | The response's http status code.
createChannelMembershipResponse_httpStatus :: Lens.Lens' CreateChannelMembershipResponse Prelude.Int
createChannelMembershipResponse_httpStatus = Lens.lens (\CreateChannelMembershipResponse' {httpStatus} -> httpStatus) (\s@CreateChannelMembershipResponse' {} a -> s {httpStatus = a} :: CreateChannelMembershipResponse)

instance
  Prelude.NFData
    CreateChannelMembershipResponse
  where
  rnf CreateChannelMembershipResponse' {..} =
    Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf member
      `Prelude.seq` Prelude.rnf httpStatus
