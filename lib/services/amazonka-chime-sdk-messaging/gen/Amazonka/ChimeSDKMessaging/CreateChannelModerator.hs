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
-- Module      : Amazonka.ChimeSDKMessaging.CreateChannelModerator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new @ChannelModerator@. A channel moderator can:
--
-- -   Add and remove other members of the channel.
--
-- -   Add and remove other moderators of the channel.
--
-- -   Add and remove user bans for the channel.
--
-- -   Redact messages in the channel.
--
-- -   List messages in the channel.
--
-- The @x-amz-chime-bearer@ request header is mandatory. Use the
-- @AppInstanceUserArn@ of the user that makes the API call as the value in
-- the header.
module Amazonka.ChimeSDKMessaging.CreateChannelModerator
  ( -- * Creating a Request
    CreateChannelModerator (..),
    newCreateChannelModerator,

    -- * Request Lenses
    createChannelModerator_channelArn,
    createChannelModerator_channelModeratorArn,
    createChannelModerator_chimeBearer,

    -- * Destructuring the Response
    CreateChannelModeratorResponse (..),
    newCreateChannelModeratorResponse,

    -- * Response Lenses
    createChannelModeratorResponse_channelArn,
    createChannelModeratorResponse_channelModerator,
    createChannelModeratorResponse_httpStatus,
  )
where

import Amazonka.ChimeSDKMessaging.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateChannelModerator' smart constructor.
data CreateChannelModerator = CreateChannelModerator'
  { -- | The ARN of the channel.
    channelArn :: Prelude.Text,
    -- | The @AppInstanceUserArn@ of the moderator.
    channelModeratorArn :: Prelude.Text,
    -- | The @AppInstanceUserArn@ of the user that makes the API call.
    chimeBearer :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateChannelModerator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelArn', 'createChannelModerator_channelArn' - The ARN of the channel.
--
-- 'channelModeratorArn', 'createChannelModerator_channelModeratorArn' - The @AppInstanceUserArn@ of the moderator.
--
-- 'chimeBearer', 'createChannelModerator_chimeBearer' - The @AppInstanceUserArn@ of the user that makes the API call.
newCreateChannelModerator ::
  -- | 'channelArn'
  Prelude.Text ->
  -- | 'channelModeratorArn'
  Prelude.Text ->
  -- | 'chimeBearer'
  Prelude.Text ->
  CreateChannelModerator
newCreateChannelModerator
  pChannelArn_
  pChannelModeratorArn_
  pChimeBearer_ =
    CreateChannelModerator'
      { channelArn = pChannelArn_,
        channelModeratorArn = pChannelModeratorArn_,
        chimeBearer = pChimeBearer_
      }

-- | The ARN of the channel.
createChannelModerator_channelArn :: Lens.Lens' CreateChannelModerator Prelude.Text
createChannelModerator_channelArn = Lens.lens (\CreateChannelModerator' {channelArn} -> channelArn) (\s@CreateChannelModerator' {} a -> s {channelArn = a} :: CreateChannelModerator)

-- | The @AppInstanceUserArn@ of the moderator.
createChannelModerator_channelModeratorArn :: Lens.Lens' CreateChannelModerator Prelude.Text
createChannelModerator_channelModeratorArn = Lens.lens (\CreateChannelModerator' {channelModeratorArn} -> channelModeratorArn) (\s@CreateChannelModerator' {} a -> s {channelModeratorArn = a} :: CreateChannelModerator)

-- | The @AppInstanceUserArn@ of the user that makes the API call.
createChannelModerator_chimeBearer :: Lens.Lens' CreateChannelModerator Prelude.Text
createChannelModerator_chimeBearer = Lens.lens (\CreateChannelModerator' {chimeBearer} -> chimeBearer) (\s@CreateChannelModerator' {} a -> s {chimeBearer = a} :: CreateChannelModerator)

instance Core.AWSRequest CreateChannelModerator where
  type
    AWSResponse CreateChannelModerator =
      CreateChannelModeratorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateChannelModeratorResponse'
            Prelude.<$> (x Data..?> "ChannelArn")
            Prelude.<*> (x Data..?> "ChannelModerator")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateChannelModerator where
  hashWithSalt _salt CreateChannelModerator' {..} =
    _salt
      `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` channelModeratorArn
      `Prelude.hashWithSalt` chimeBearer

instance Prelude.NFData CreateChannelModerator where
  rnf CreateChannelModerator' {..} =
    Prelude.rnf channelArn `Prelude.seq`
      Prelude.rnf channelModeratorArn `Prelude.seq`
        Prelude.rnf chimeBearer

instance Data.ToHeaders CreateChannelModerator where
  toHeaders CreateChannelModerator' {..} =
    Prelude.mconcat
      ["x-amz-chime-bearer" Data.=# chimeBearer]

instance Data.ToJSON CreateChannelModerator where
  toJSON CreateChannelModerator' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ChannelModeratorArn" Data..= channelModeratorArn)
          ]
      )

instance Data.ToPath CreateChannelModerator where
  toPath CreateChannelModerator' {..} =
    Prelude.mconcat
      ["/channels/", Data.toBS channelArn, "/moderators"]

instance Data.ToQuery CreateChannelModerator where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateChannelModeratorResponse' smart constructor.
data CreateChannelModeratorResponse = CreateChannelModeratorResponse'
  { -- | The ARN of the channel.
    channelArn :: Prelude.Maybe Prelude.Text,
    -- | The ARNs of the channel and the moderator.
    channelModerator :: Prelude.Maybe Identity,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateChannelModeratorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelArn', 'createChannelModeratorResponse_channelArn' - The ARN of the channel.
--
-- 'channelModerator', 'createChannelModeratorResponse_channelModerator' - The ARNs of the channel and the moderator.
--
-- 'httpStatus', 'createChannelModeratorResponse_httpStatus' - The response's http status code.
newCreateChannelModeratorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateChannelModeratorResponse
newCreateChannelModeratorResponse pHttpStatus_ =
  CreateChannelModeratorResponse'
    { channelArn =
        Prelude.Nothing,
      channelModerator = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the channel.
createChannelModeratorResponse_channelArn :: Lens.Lens' CreateChannelModeratorResponse (Prelude.Maybe Prelude.Text)
createChannelModeratorResponse_channelArn = Lens.lens (\CreateChannelModeratorResponse' {channelArn} -> channelArn) (\s@CreateChannelModeratorResponse' {} a -> s {channelArn = a} :: CreateChannelModeratorResponse)

-- | The ARNs of the channel and the moderator.
createChannelModeratorResponse_channelModerator :: Lens.Lens' CreateChannelModeratorResponse (Prelude.Maybe Identity)
createChannelModeratorResponse_channelModerator = Lens.lens (\CreateChannelModeratorResponse' {channelModerator} -> channelModerator) (\s@CreateChannelModeratorResponse' {} a -> s {channelModerator = a} :: CreateChannelModeratorResponse)

-- | The response's http status code.
createChannelModeratorResponse_httpStatus :: Lens.Lens' CreateChannelModeratorResponse Prelude.Int
createChannelModeratorResponse_httpStatus = Lens.lens (\CreateChannelModeratorResponse' {httpStatus} -> httpStatus) (\s@CreateChannelModeratorResponse' {} a -> s {httpStatus = a} :: CreateChannelModeratorResponse)

instance
  Prelude.NFData
    CreateChannelModeratorResponse
  where
  rnf CreateChannelModeratorResponse' {..} =
    Prelude.rnf channelArn `Prelude.seq`
      Prelude.rnf channelModerator `Prelude.seq`
        Prelude.rnf httpStatus
