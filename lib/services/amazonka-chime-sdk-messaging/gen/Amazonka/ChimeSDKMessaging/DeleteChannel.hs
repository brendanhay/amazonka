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
-- Module      : Amazonka.ChimeSDKMessaging.DeleteChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Immediately makes a channel and its memberships inaccessible and marks
-- them for deletion. This is an irreversible process.
--
-- The @x-amz-chime-bearer@ request header is mandatory. Use the ARN of the
-- @AppInstanceUserArn@ or @AppInstanceBot@ that makes the API call as the
-- value in the header.
module Amazonka.ChimeSDKMessaging.DeleteChannel
  ( -- * Creating a Request
    DeleteChannel (..),
    newDeleteChannel,

    -- * Request Lenses
    deleteChannel_channelArn,
    deleteChannel_chimeBearer,

    -- * Destructuring the Response
    DeleteChannelResponse (..),
    newDeleteChannelResponse,
  )
where

import Amazonka.ChimeSDKMessaging.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteChannel' smart constructor.
data DeleteChannel = DeleteChannel'
  { -- | The ARN of the channel being deleted.
    channelArn :: Prelude.Text,
    -- | The ARN of the @AppInstanceUser@ or @AppInstanceBot@ that makes the API
    -- call.
    chimeBearer :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelArn', 'deleteChannel_channelArn' - The ARN of the channel being deleted.
--
-- 'chimeBearer', 'deleteChannel_chimeBearer' - The ARN of the @AppInstanceUser@ or @AppInstanceBot@ that makes the API
-- call.
newDeleteChannel ::
  -- | 'channelArn'
  Prelude.Text ->
  -- | 'chimeBearer'
  Prelude.Text ->
  DeleteChannel
newDeleteChannel pChannelArn_ pChimeBearer_ =
  DeleteChannel'
    { channelArn = pChannelArn_,
      chimeBearer = pChimeBearer_
    }

-- | The ARN of the channel being deleted.
deleteChannel_channelArn :: Lens.Lens' DeleteChannel Prelude.Text
deleteChannel_channelArn = Lens.lens (\DeleteChannel' {channelArn} -> channelArn) (\s@DeleteChannel' {} a -> s {channelArn = a} :: DeleteChannel)

-- | The ARN of the @AppInstanceUser@ or @AppInstanceBot@ that makes the API
-- call.
deleteChannel_chimeBearer :: Lens.Lens' DeleteChannel Prelude.Text
deleteChannel_chimeBearer = Lens.lens (\DeleteChannel' {chimeBearer} -> chimeBearer) (\s@DeleteChannel' {} a -> s {chimeBearer = a} :: DeleteChannel)

instance Core.AWSRequest DeleteChannel where
  type
    AWSResponse DeleteChannel =
      DeleteChannelResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteChannelResponse'

instance Prelude.Hashable DeleteChannel where
  hashWithSalt _salt DeleteChannel' {..} =
    _salt
      `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` chimeBearer

instance Prelude.NFData DeleteChannel where
  rnf DeleteChannel' {..} =
    Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf chimeBearer

instance Data.ToHeaders DeleteChannel where
  toHeaders DeleteChannel' {..} =
    Prelude.mconcat
      ["x-amz-chime-bearer" Data.=# chimeBearer]

instance Data.ToPath DeleteChannel where
  toPath DeleteChannel' {..} =
    Prelude.mconcat
      ["/channels/", Data.toBS channelArn]

instance Data.ToQuery DeleteChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteChannelResponse' smart constructor.
data DeleteChannelResponse = DeleteChannelResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteChannelResponse ::
  DeleteChannelResponse
newDeleteChannelResponse = DeleteChannelResponse'

instance Prelude.NFData DeleteChannelResponse where
  rnf _ = ()
