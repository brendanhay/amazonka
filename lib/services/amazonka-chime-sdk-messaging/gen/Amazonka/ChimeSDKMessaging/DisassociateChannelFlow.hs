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
-- Module      : Amazonka.ChimeSDKMessaging.DisassociateChannelFlow
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a channel flow from all its channels. Once disassociated,
-- all messages to that channel stop going through the channel flow
-- processor.
--
-- Only administrators or channel moderators can disassociate a channel
-- flow. The @x-amz-chime-bearer@ request header is mandatory. Use the
-- @AppInstanceUserArn@ of the user that makes the API call as the value in
-- the header.
module Amazonka.ChimeSDKMessaging.DisassociateChannelFlow
  ( -- * Creating a Request
    DisassociateChannelFlow (..),
    newDisassociateChannelFlow,

    -- * Request Lenses
    disassociateChannelFlow_channelArn,
    disassociateChannelFlow_channelFlowArn,
    disassociateChannelFlow_chimeBearer,

    -- * Destructuring the Response
    DisassociateChannelFlowResponse (..),
    newDisassociateChannelFlowResponse,
  )
where

import Amazonka.ChimeSDKMessaging.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateChannelFlow' smart constructor.
data DisassociateChannelFlow = DisassociateChannelFlow'
  { -- | The ARN of the channel.
    channelArn :: Prelude.Text,
    -- | The ARN of the channel flow.
    channelFlowArn :: Prelude.Text,
    -- | The @AppInstanceUserArn@ of the user making the API call.
    chimeBearer :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateChannelFlow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelArn', 'disassociateChannelFlow_channelArn' - The ARN of the channel.
--
-- 'channelFlowArn', 'disassociateChannelFlow_channelFlowArn' - The ARN of the channel flow.
--
-- 'chimeBearer', 'disassociateChannelFlow_chimeBearer' - The @AppInstanceUserArn@ of the user making the API call.
newDisassociateChannelFlow ::
  -- | 'channelArn'
  Prelude.Text ->
  -- | 'channelFlowArn'
  Prelude.Text ->
  -- | 'chimeBearer'
  Prelude.Text ->
  DisassociateChannelFlow
newDisassociateChannelFlow
  pChannelArn_
  pChannelFlowArn_
  pChimeBearer_ =
    DisassociateChannelFlow'
      { channelArn = pChannelArn_,
        channelFlowArn = pChannelFlowArn_,
        chimeBearer = pChimeBearer_
      }

-- | The ARN of the channel.
disassociateChannelFlow_channelArn :: Lens.Lens' DisassociateChannelFlow Prelude.Text
disassociateChannelFlow_channelArn = Lens.lens (\DisassociateChannelFlow' {channelArn} -> channelArn) (\s@DisassociateChannelFlow' {} a -> s {channelArn = a} :: DisassociateChannelFlow)

-- | The ARN of the channel flow.
disassociateChannelFlow_channelFlowArn :: Lens.Lens' DisassociateChannelFlow Prelude.Text
disassociateChannelFlow_channelFlowArn = Lens.lens (\DisassociateChannelFlow' {channelFlowArn} -> channelFlowArn) (\s@DisassociateChannelFlow' {} a -> s {channelFlowArn = a} :: DisassociateChannelFlow)

-- | The @AppInstanceUserArn@ of the user making the API call.
disassociateChannelFlow_chimeBearer :: Lens.Lens' DisassociateChannelFlow Prelude.Text
disassociateChannelFlow_chimeBearer = Lens.lens (\DisassociateChannelFlow' {chimeBearer} -> chimeBearer) (\s@DisassociateChannelFlow' {} a -> s {chimeBearer = a} :: DisassociateChannelFlow)

instance Core.AWSRequest DisassociateChannelFlow where
  type
    AWSResponse DisassociateChannelFlow =
      DisassociateChannelFlowResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DisassociateChannelFlowResponse'

instance Prelude.Hashable DisassociateChannelFlow where
  hashWithSalt _salt DisassociateChannelFlow' {..} =
    _salt `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` channelFlowArn
      `Prelude.hashWithSalt` chimeBearer

instance Prelude.NFData DisassociateChannelFlow where
  rnf DisassociateChannelFlow' {..} =
    Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf channelFlowArn
      `Prelude.seq` Prelude.rnf chimeBearer

instance Data.ToHeaders DisassociateChannelFlow where
  toHeaders DisassociateChannelFlow' {..} =
    Prelude.mconcat
      ["x-amz-chime-bearer" Data.=# chimeBearer]

instance Data.ToPath DisassociateChannelFlow where
  toPath DisassociateChannelFlow' {..} =
    Prelude.mconcat
      [ "/channels/",
        Data.toBS channelArn,
        "/channel-flow/",
        Data.toBS channelFlowArn
      ]

instance Data.ToQuery DisassociateChannelFlow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateChannelFlowResponse' smart constructor.
data DisassociateChannelFlowResponse = DisassociateChannelFlowResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateChannelFlowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateChannelFlowResponse ::
  DisassociateChannelFlowResponse
newDisassociateChannelFlowResponse =
  DisassociateChannelFlowResponse'

instance
  Prelude.NFData
    DisassociateChannelFlowResponse
  where
  rnf _ = ()
