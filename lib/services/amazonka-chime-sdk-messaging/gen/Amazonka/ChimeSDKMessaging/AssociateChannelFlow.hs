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
-- Module      : Amazonka.ChimeSDKMessaging.AssociateChannelFlow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a channel flow with a channel. Once associated, all messages
-- to that channel go through channel flow processors. To stop processing,
-- use the @DisassociateChannelFlow@ API.
--
-- Only administrators or channel moderators can associate a channel flow.
-- The @x-amz-chime-bearer@ request header is mandatory. Use the
-- @AppInstanceUserArn@ of the user that makes the API call as the value in
-- the header.
module Amazonka.ChimeSDKMessaging.AssociateChannelFlow
  ( -- * Creating a Request
    AssociateChannelFlow (..),
    newAssociateChannelFlow,

    -- * Request Lenses
    associateChannelFlow_channelArn,
    associateChannelFlow_channelFlowArn,
    associateChannelFlow_chimeBearer,

    -- * Destructuring the Response
    AssociateChannelFlowResponse (..),
    newAssociateChannelFlowResponse,
  )
where

import Amazonka.ChimeSDKMessaging.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateChannelFlow' smart constructor.
data AssociateChannelFlow = AssociateChannelFlow'
  { -- | The ARN of the channel.
    channelArn :: Prelude.Text,
    -- | The ARN of the channel flow.
    channelFlowArn :: Prelude.Text,
    -- | The @AppInstanceUserArn@ of the user making the API call.
    chimeBearer :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateChannelFlow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelArn', 'associateChannelFlow_channelArn' - The ARN of the channel.
--
-- 'channelFlowArn', 'associateChannelFlow_channelFlowArn' - The ARN of the channel flow.
--
-- 'chimeBearer', 'associateChannelFlow_chimeBearer' - The @AppInstanceUserArn@ of the user making the API call.
newAssociateChannelFlow ::
  -- | 'channelArn'
  Prelude.Text ->
  -- | 'channelFlowArn'
  Prelude.Text ->
  -- | 'chimeBearer'
  Prelude.Text ->
  AssociateChannelFlow
newAssociateChannelFlow
  pChannelArn_
  pChannelFlowArn_
  pChimeBearer_ =
    AssociateChannelFlow'
      { channelArn = pChannelArn_,
        channelFlowArn = pChannelFlowArn_,
        chimeBearer = pChimeBearer_
      }

-- | The ARN of the channel.
associateChannelFlow_channelArn :: Lens.Lens' AssociateChannelFlow Prelude.Text
associateChannelFlow_channelArn = Lens.lens (\AssociateChannelFlow' {channelArn} -> channelArn) (\s@AssociateChannelFlow' {} a -> s {channelArn = a} :: AssociateChannelFlow)

-- | The ARN of the channel flow.
associateChannelFlow_channelFlowArn :: Lens.Lens' AssociateChannelFlow Prelude.Text
associateChannelFlow_channelFlowArn = Lens.lens (\AssociateChannelFlow' {channelFlowArn} -> channelFlowArn) (\s@AssociateChannelFlow' {} a -> s {channelFlowArn = a} :: AssociateChannelFlow)

-- | The @AppInstanceUserArn@ of the user making the API call.
associateChannelFlow_chimeBearer :: Lens.Lens' AssociateChannelFlow Prelude.Text
associateChannelFlow_chimeBearer = Lens.lens (\AssociateChannelFlow' {chimeBearer} -> chimeBearer) (\s@AssociateChannelFlow' {} a -> s {chimeBearer = a} :: AssociateChannelFlow)

instance Core.AWSRequest AssociateChannelFlow where
  type
    AWSResponse AssociateChannelFlow =
      AssociateChannelFlowResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveNull AssociateChannelFlowResponse'

instance Prelude.Hashable AssociateChannelFlow where
  hashWithSalt _salt AssociateChannelFlow' {..} =
    _salt
      `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` channelFlowArn
      `Prelude.hashWithSalt` chimeBearer

instance Prelude.NFData AssociateChannelFlow where
  rnf AssociateChannelFlow' {..} =
    Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf channelFlowArn
      `Prelude.seq` Prelude.rnf chimeBearer

instance Data.ToHeaders AssociateChannelFlow where
  toHeaders AssociateChannelFlow' {..} =
    Prelude.mconcat
      ["x-amz-chime-bearer" Data.=# chimeBearer]

instance Data.ToJSON AssociateChannelFlow where
  toJSON AssociateChannelFlow' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ChannelFlowArn" Data..= channelFlowArn)
          ]
      )

instance Data.ToPath AssociateChannelFlow where
  toPath AssociateChannelFlow' {..} =
    Prelude.mconcat
      ["/channels/", Data.toBS channelArn, "/channel-flow"]

instance Data.ToQuery AssociateChannelFlow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateChannelFlowResponse' smart constructor.
data AssociateChannelFlowResponse = AssociateChannelFlowResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateChannelFlowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAssociateChannelFlowResponse ::
  AssociateChannelFlowResponse
newAssociateChannelFlowResponse =
  AssociateChannelFlowResponse'

instance Prelude.NFData AssociateChannelFlowResponse where
  rnf _ = ()
