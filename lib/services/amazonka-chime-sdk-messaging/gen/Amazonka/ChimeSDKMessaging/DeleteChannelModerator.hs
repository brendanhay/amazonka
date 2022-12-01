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
-- Module      : Amazonka.ChimeSDKMessaging.DeleteChannelModerator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a channel moderator.
--
-- The @x-amz-chime-bearer@ request header is mandatory. Use the
-- @AppInstanceUserArn@ of the user that makes the API call as the value in
-- the header.
module Amazonka.ChimeSDKMessaging.DeleteChannelModerator
  ( -- * Creating a Request
    DeleteChannelModerator (..),
    newDeleteChannelModerator,

    -- * Request Lenses
    deleteChannelModerator_channelArn,
    deleteChannelModerator_channelModeratorArn,
    deleteChannelModerator_chimeBearer,

    -- * Destructuring the Response
    DeleteChannelModeratorResponse (..),
    newDeleteChannelModeratorResponse,
  )
where

import Amazonka.ChimeSDKMessaging.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteChannelModerator' smart constructor.
data DeleteChannelModerator = DeleteChannelModerator'
  { -- | The ARN of the channel.
    channelArn :: Prelude.Text,
    -- | The @AppInstanceUserArn@ of the moderator being deleted.
    channelModeratorArn :: Prelude.Text,
    -- | The @AppInstanceUserArn@ of the user that makes the API call.
    chimeBearer :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteChannelModerator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelArn', 'deleteChannelModerator_channelArn' - The ARN of the channel.
--
-- 'channelModeratorArn', 'deleteChannelModerator_channelModeratorArn' - The @AppInstanceUserArn@ of the moderator being deleted.
--
-- 'chimeBearer', 'deleteChannelModerator_chimeBearer' - The @AppInstanceUserArn@ of the user that makes the API call.
newDeleteChannelModerator ::
  -- | 'channelArn'
  Prelude.Text ->
  -- | 'channelModeratorArn'
  Prelude.Text ->
  -- | 'chimeBearer'
  Prelude.Text ->
  DeleteChannelModerator
newDeleteChannelModerator
  pChannelArn_
  pChannelModeratorArn_
  pChimeBearer_ =
    DeleteChannelModerator'
      { channelArn = pChannelArn_,
        channelModeratorArn = pChannelModeratorArn_,
        chimeBearer = pChimeBearer_
      }

-- | The ARN of the channel.
deleteChannelModerator_channelArn :: Lens.Lens' DeleteChannelModerator Prelude.Text
deleteChannelModerator_channelArn = Lens.lens (\DeleteChannelModerator' {channelArn} -> channelArn) (\s@DeleteChannelModerator' {} a -> s {channelArn = a} :: DeleteChannelModerator)

-- | The @AppInstanceUserArn@ of the moderator being deleted.
deleteChannelModerator_channelModeratorArn :: Lens.Lens' DeleteChannelModerator Prelude.Text
deleteChannelModerator_channelModeratorArn = Lens.lens (\DeleteChannelModerator' {channelModeratorArn} -> channelModeratorArn) (\s@DeleteChannelModerator' {} a -> s {channelModeratorArn = a} :: DeleteChannelModerator)

-- | The @AppInstanceUserArn@ of the user that makes the API call.
deleteChannelModerator_chimeBearer :: Lens.Lens' DeleteChannelModerator Prelude.Text
deleteChannelModerator_chimeBearer = Lens.lens (\DeleteChannelModerator' {chimeBearer} -> chimeBearer) (\s@DeleteChannelModerator' {} a -> s {chimeBearer = a} :: DeleteChannelModerator)

instance Core.AWSRequest DeleteChannelModerator where
  type
    AWSResponse DeleteChannelModerator =
      DeleteChannelModeratorResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteChannelModeratorResponse'

instance Prelude.Hashable DeleteChannelModerator where
  hashWithSalt _salt DeleteChannelModerator' {..} =
    _salt `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` channelModeratorArn
      `Prelude.hashWithSalt` chimeBearer

instance Prelude.NFData DeleteChannelModerator where
  rnf DeleteChannelModerator' {..} =
    Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf channelModeratorArn
      `Prelude.seq` Prelude.rnf chimeBearer

instance Core.ToHeaders DeleteChannelModerator where
  toHeaders DeleteChannelModerator' {..} =
    Prelude.mconcat
      ["x-amz-chime-bearer" Core.=# chimeBearer]

instance Core.ToPath DeleteChannelModerator where
  toPath DeleteChannelModerator' {..} =
    Prelude.mconcat
      [ "/channels/",
        Core.toBS channelArn,
        "/moderators/",
        Core.toBS channelModeratorArn
      ]

instance Core.ToQuery DeleteChannelModerator where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteChannelModeratorResponse' smart constructor.
data DeleteChannelModeratorResponse = DeleteChannelModeratorResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteChannelModeratorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteChannelModeratorResponse ::
  DeleteChannelModeratorResponse
newDeleteChannelModeratorResponse =
  DeleteChannelModeratorResponse'

instance
  Prelude.NFData
    DeleteChannelModeratorResponse
  where
  rnf _ = ()
