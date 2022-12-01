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
-- Module      : Amazonka.Chime.DeleteChannelMembership
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a member from a channel.
--
-- The @x-amz-chime-bearer@ request header is mandatory. Use the
-- @AppInstanceUserArn@ of the user that makes the API call as the value in
-- the header.
module Amazonka.Chime.DeleteChannelMembership
  ( -- * Creating a Request
    DeleteChannelMembership (..),
    newDeleteChannelMembership,

    -- * Request Lenses
    deleteChannelMembership_chimeBearer,
    deleteChannelMembership_channelArn,
    deleteChannelMembership_memberArn,

    -- * Destructuring the Response
    DeleteChannelMembershipResponse (..),
    newDeleteChannelMembershipResponse,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteChannelMembership' smart constructor.
data DeleteChannelMembership = DeleteChannelMembership'
  { -- | The @AppInstanceUserArn@ of the user that makes the API call.
    chimeBearer :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the channel from which you want to remove the user.
    channelArn :: Prelude.Text,
    -- | The ARN of the member that you\'re removing from the channel.
    memberArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteChannelMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'chimeBearer', 'deleteChannelMembership_chimeBearer' - The @AppInstanceUserArn@ of the user that makes the API call.
--
-- 'channelArn', 'deleteChannelMembership_channelArn' - The ARN of the channel from which you want to remove the user.
--
-- 'memberArn', 'deleteChannelMembership_memberArn' - The ARN of the member that you\'re removing from the channel.
newDeleteChannelMembership ::
  -- | 'channelArn'
  Prelude.Text ->
  -- | 'memberArn'
  Prelude.Text ->
  DeleteChannelMembership
newDeleteChannelMembership pChannelArn_ pMemberArn_ =
  DeleteChannelMembership'
    { chimeBearer =
        Prelude.Nothing,
      channelArn = pChannelArn_,
      memberArn = pMemberArn_
    }

-- | The @AppInstanceUserArn@ of the user that makes the API call.
deleteChannelMembership_chimeBearer :: Lens.Lens' DeleteChannelMembership (Prelude.Maybe Prelude.Text)
deleteChannelMembership_chimeBearer = Lens.lens (\DeleteChannelMembership' {chimeBearer} -> chimeBearer) (\s@DeleteChannelMembership' {} a -> s {chimeBearer = a} :: DeleteChannelMembership)

-- | The ARN of the channel from which you want to remove the user.
deleteChannelMembership_channelArn :: Lens.Lens' DeleteChannelMembership Prelude.Text
deleteChannelMembership_channelArn = Lens.lens (\DeleteChannelMembership' {channelArn} -> channelArn) (\s@DeleteChannelMembership' {} a -> s {channelArn = a} :: DeleteChannelMembership)

-- | The ARN of the member that you\'re removing from the channel.
deleteChannelMembership_memberArn :: Lens.Lens' DeleteChannelMembership Prelude.Text
deleteChannelMembership_memberArn = Lens.lens (\DeleteChannelMembership' {memberArn} -> memberArn) (\s@DeleteChannelMembership' {} a -> s {memberArn = a} :: DeleteChannelMembership)

instance Core.AWSRequest DeleteChannelMembership where
  type
    AWSResponse DeleteChannelMembership =
      DeleteChannelMembershipResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteChannelMembershipResponse'

instance Prelude.Hashable DeleteChannelMembership where
  hashWithSalt _salt DeleteChannelMembership' {..} =
    _salt `Prelude.hashWithSalt` chimeBearer
      `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` memberArn

instance Prelude.NFData DeleteChannelMembership where
  rnf DeleteChannelMembership' {..} =
    Prelude.rnf chimeBearer
      `Prelude.seq` Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf memberArn

instance Core.ToHeaders DeleteChannelMembership where
  toHeaders DeleteChannelMembership' {..} =
    Prelude.mconcat
      ["x-amz-chime-bearer" Core.=# chimeBearer]

instance Core.ToPath DeleteChannelMembership where
  toPath DeleteChannelMembership' {..} =
    Prelude.mconcat
      [ "/channels/",
        Core.toBS channelArn,
        "/memberships/",
        Core.toBS memberArn
      ]

instance Core.ToQuery DeleteChannelMembership where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteChannelMembershipResponse' smart constructor.
data DeleteChannelMembershipResponse = DeleteChannelMembershipResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteChannelMembershipResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteChannelMembershipResponse ::
  DeleteChannelMembershipResponse
newDeleteChannelMembershipResponse =
  DeleteChannelMembershipResponse'

instance
  Prelude.NFData
    DeleteChannelMembershipResponse
  where
  rnf _ = ()
