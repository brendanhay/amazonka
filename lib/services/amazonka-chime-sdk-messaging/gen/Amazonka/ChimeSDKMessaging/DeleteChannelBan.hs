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
-- Module      : Amazonka.ChimeSDKMessaging.DeleteChannelBan
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a user from a channel\'s ban list.
--
-- The @x-amz-chime-bearer@ request header is mandatory. Use the
-- @AppInstanceUserArn@ of the user that makes the API call as the value in
-- the header.
module Amazonka.ChimeSDKMessaging.DeleteChannelBan
  ( -- * Creating a Request
    DeleteChannelBan (..),
    newDeleteChannelBan,

    -- * Request Lenses
    deleteChannelBan_channelArn,
    deleteChannelBan_memberArn,
    deleteChannelBan_chimeBearer,

    -- * Destructuring the Response
    DeleteChannelBanResponse (..),
    newDeleteChannelBanResponse,
  )
where

import Amazonka.ChimeSDKMessaging.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteChannelBan' smart constructor.
data DeleteChannelBan = DeleteChannelBan'
  { -- | The ARN of the channel from which the @AppInstanceUser@ was banned.
    channelArn :: Prelude.Text,
    -- | The ARN of the @AppInstanceUser@ that you want to reinstate.
    memberArn :: Prelude.Text,
    -- | The @AppInstanceUserArn@ of the user that makes the API call.
    chimeBearer :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteChannelBan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelArn', 'deleteChannelBan_channelArn' - The ARN of the channel from which the @AppInstanceUser@ was banned.
--
-- 'memberArn', 'deleteChannelBan_memberArn' - The ARN of the @AppInstanceUser@ that you want to reinstate.
--
-- 'chimeBearer', 'deleteChannelBan_chimeBearer' - The @AppInstanceUserArn@ of the user that makes the API call.
newDeleteChannelBan ::
  -- | 'channelArn'
  Prelude.Text ->
  -- | 'memberArn'
  Prelude.Text ->
  -- | 'chimeBearer'
  Prelude.Text ->
  DeleteChannelBan
newDeleteChannelBan
  pChannelArn_
  pMemberArn_
  pChimeBearer_ =
    DeleteChannelBan'
      { channelArn = pChannelArn_,
        memberArn = pMemberArn_,
        chimeBearer = pChimeBearer_
      }

-- | The ARN of the channel from which the @AppInstanceUser@ was banned.
deleteChannelBan_channelArn :: Lens.Lens' DeleteChannelBan Prelude.Text
deleteChannelBan_channelArn = Lens.lens (\DeleteChannelBan' {channelArn} -> channelArn) (\s@DeleteChannelBan' {} a -> s {channelArn = a} :: DeleteChannelBan)

-- | The ARN of the @AppInstanceUser@ that you want to reinstate.
deleteChannelBan_memberArn :: Lens.Lens' DeleteChannelBan Prelude.Text
deleteChannelBan_memberArn = Lens.lens (\DeleteChannelBan' {memberArn} -> memberArn) (\s@DeleteChannelBan' {} a -> s {memberArn = a} :: DeleteChannelBan)

-- | The @AppInstanceUserArn@ of the user that makes the API call.
deleteChannelBan_chimeBearer :: Lens.Lens' DeleteChannelBan Prelude.Text
deleteChannelBan_chimeBearer = Lens.lens (\DeleteChannelBan' {chimeBearer} -> chimeBearer) (\s@DeleteChannelBan' {} a -> s {chimeBearer = a} :: DeleteChannelBan)

instance Core.AWSRequest DeleteChannelBan where
  type
    AWSResponse DeleteChannelBan =
      DeleteChannelBanResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteChannelBanResponse'

instance Prelude.Hashable DeleteChannelBan where
  hashWithSalt _salt DeleteChannelBan' {..} =
    _salt
      `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` memberArn
      `Prelude.hashWithSalt` chimeBearer

instance Prelude.NFData DeleteChannelBan where
  rnf DeleteChannelBan' {..} =
    Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf memberArn
      `Prelude.seq` Prelude.rnf chimeBearer

instance Data.ToHeaders DeleteChannelBan where
  toHeaders DeleteChannelBan' {..} =
    Prelude.mconcat
      ["x-amz-chime-bearer" Data.=# chimeBearer]

instance Data.ToPath DeleteChannelBan where
  toPath DeleteChannelBan' {..} =
    Prelude.mconcat
      [ "/channels/",
        Data.toBS channelArn,
        "/bans/",
        Data.toBS memberArn
      ]

instance Data.ToQuery DeleteChannelBan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteChannelBanResponse' smart constructor.
data DeleteChannelBanResponse = DeleteChannelBanResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteChannelBanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteChannelBanResponse ::
  DeleteChannelBanResponse
newDeleteChannelBanResponse =
  DeleteChannelBanResponse'

instance Prelude.NFData DeleteChannelBanResponse where
  rnf _ = ()
