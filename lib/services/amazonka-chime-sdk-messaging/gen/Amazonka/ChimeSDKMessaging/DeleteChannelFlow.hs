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
-- Module      : Amazonka.ChimeSDKMessaging.DeleteChannelFlow
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a channel flow, an irreversible process. This is a developer
-- API.
--
-- This API works only when the channel flow is not associated with any
-- channel. To get a list of all channels that a channel flow is associated
-- with, use the @ListChannelsAssociatedWithChannelFlow@ API. Use the
-- @DisassociateChannelFlow@ API to disassociate a channel flow from all
-- channels.
module Amazonka.ChimeSDKMessaging.DeleteChannelFlow
  ( -- * Creating a Request
    DeleteChannelFlow (..),
    newDeleteChannelFlow,

    -- * Request Lenses
    deleteChannelFlow_channelFlowArn,

    -- * Destructuring the Response
    DeleteChannelFlowResponse (..),
    newDeleteChannelFlowResponse,
  )
where

import Amazonka.ChimeSDKMessaging.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteChannelFlow' smart constructor.
data DeleteChannelFlow = DeleteChannelFlow'
  { -- | The ARN of the channel flow.
    channelFlowArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteChannelFlow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelFlowArn', 'deleteChannelFlow_channelFlowArn' - The ARN of the channel flow.
newDeleteChannelFlow ::
  -- | 'channelFlowArn'
  Prelude.Text ->
  DeleteChannelFlow
newDeleteChannelFlow pChannelFlowArn_ =
  DeleteChannelFlow'
    { channelFlowArn =
        pChannelFlowArn_
    }

-- | The ARN of the channel flow.
deleteChannelFlow_channelFlowArn :: Lens.Lens' DeleteChannelFlow Prelude.Text
deleteChannelFlow_channelFlowArn = Lens.lens (\DeleteChannelFlow' {channelFlowArn} -> channelFlowArn) (\s@DeleteChannelFlow' {} a -> s {channelFlowArn = a} :: DeleteChannelFlow)

instance Core.AWSRequest DeleteChannelFlow where
  type
    AWSResponse DeleteChannelFlow =
      DeleteChannelFlowResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteChannelFlowResponse'

instance Prelude.Hashable DeleteChannelFlow where
  hashWithSalt _salt DeleteChannelFlow' {..} =
    _salt `Prelude.hashWithSalt` channelFlowArn

instance Prelude.NFData DeleteChannelFlow where
  rnf DeleteChannelFlow' {..} =
    Prelude.rnf channelFlowArn

instance Core.ToHeaders DeleteChannelFlow where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteChannelFlow where
  toPath DeleteChannelFlow' {..} =
    Prelude.mconcat
      ["/channel-flows/", Core.toBS channelFlowArn]

instance Core.ToQuery DeleteChannelFlow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteChannelFlowResponse' smart constructor.
data DeleteChannelFlowResponse = DeleteChannelFlowResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteChannelFlowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteChannelFlowResponse ::
  DeleteChannelFlowResponse
newDeleteChannelFlowResponse =
  DeleteChannelFlowResponse'

instance Prelude.NFData DeleteChannelFlowResponse where
  rnf _ = ()
