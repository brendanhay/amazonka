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
-- Module      : Network.AWS.Connect.UpdateQueueOutboundCallerConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Updates the outbound caller ID name, number, and outbound whisper flow
-- for a specified queue.
module Network.AWS.Connect.UpdateQueueOutboundCallerConfig
  ( -- * Creating a Request
    UpdateQueueOutboundCallerConfig (..),
    newUpdateQueueOutboundCallerConfig,

    -- * Request Lenses
    updateQueueOutboundCallerConfig_instanceId,
    updateQueueOutboundCallerConfig_queueId,
    updateQueueOutboundCallerConfig_outboundCallerConfig,

    -- * Destructuring the Response
    UpdateQueueOutboundCallerConfigResponse (..),
    newUpdateQueueOutboundCallerConfigResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateQueueOutboundCallerConfig' smart constructor.
data UpdateQueueOutboundCallerConfig = UpdateQueueOutboundCallerConfig'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text,
    -- | The identifier for the queue.
    queueId :: Core.Text,
    -- | The outbound caller ID name, number, and outbound whisper flow.
    outboundCallerConfig :: OutboundCallerConfig
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateQueueOutboundCallerConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'updateQueueOutboundCallerConfig_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'queueId', 'updateQueueOutboundCallerConfig_queueId' - The identifier for the queue.
--
-- 'outboundCallerConfig', 'updateQueueOutboundCallerConfig_outboundCallerConfig' - The outbound caller ID name, number, and outbound whisper flow.
newUpdateQueueOutboundCallerConfig ::
  -- | 'instanceId'
  Core.Text ->
  -- | 'queueId'
  Core.Text ->
  -- | 'outboundCallerConfig'
  OutboundCallerConfig ->
  UpdateQueueOutboundCallerConfig
newUpdateQueueOutboundCallerConfig
  pInstanceId_
  pQueueId_
  pOutboundCallerConfig_ =
    UpdateQueueOutboundCallerConfig'
      { instanceId =
          pInstanceId_,
        queueId = pQueueId_,
        outboundCallerConfig =
          pOutboundCallerConfig_
      }

-- | The identifier of the Amazon Connect instance.
updateQueueOutboundCallerConfig_instanceId :: Lens.Lens' UpdateQueueOutboundCallerConfig Core.Text
updateQueueOutboundCallerConfig_instanceId = Lens.lens (\UpdateQueueOutboundCallerConfig' {instanceId} -> instanceId) (\s@UpdateQueueOutboundCallerConfig' {} a -> s {instanceId = a} :: UpdateQueueOutboundCallerConfig)

-- | The identifier for the queue.
updateQueueOutboundCallerConfig_queueId :: Lens.Lens' UpdateQueueOutboundCallerConfig Core.Text
updateQueueOutboundCallerConfig_queueId = Lens.lens (\UpdateQueueOutboundCallerConfig' {queueId} -> queueId) (\s@UpdateQueueOutboundCallerConfig' {} a -> s {queueId = a} :: UpdateQueueOutboundCallerConfig)

-- | The outbound caller ID name, number, and outbound whisper flow.
updateQueueOutboundCallerConfig_outboundCallerConfig :: Lens.Lens' UpdateQueueOutboundCallerConfig OutboundCallerConfig
updateQueueOutboundCallerConfig_outboundCallerConfig = Lens.lens (\UpdateQueueOutboundCallerConfig' {outboundCallerConfig} -> outboundCallerConfig) (\s@UpdateQueueOutboundCallerConfig' {} a -> s {outboundCallerConfig = a} :: UpdateQueueOutboundCallerConfig)

instance
  Core.AWSRequest
    UpdateQueueOutboundCallerConfig
  where
  type
    AWSResponse UpdateQueueOutboundCallerConfig =
      UpdateQueueOutboundCallerConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      UpdateQueueOutboundCallerConfigResponse'

instance
  Core.Hashable
    UpdateQueueOutboundCallerConfig

instance Core.NFData UpdateQueueOutboundCallerConfig

instance
  Core.ToHeaders
    UpdateQueueOutboundCallerConfig
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateQueueOutboundCallerConfig where
  toJSON UpdateQueueOutboundCallerConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "OutboundCallerConfig"
                  Core..= outboundCallerConfig
              )
          ]
      )

instance Core.ToPath UpdateQueueOutboundCallerConfig where
  toPath UpdateQueueOutboundCallerConfig' {..} =
    Core.mconcat
      [ "/queues/",
        Core.toBS instanceId,
        "/",
        Core.toBS queueId,
        "/outbound-caller-config"
      ]

instance Core.ToQuery UpdateQueueOutboundCallerConfig where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateQueueOutboundCallerConfigResponse' smart constructor.
data UpdateQueueOutboundCallerConfigResponse = UpdateQueueOutboundCallerConfigResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateQueueOutboundCallerConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateQueueOutboundCallerConfigResponse ::
  UpdateQueueOutboundCallerConfigResponse
newUpdateQueueOutboundCallerConfigResponse =
  UpdateQueueOutboundCallerConfigResponse'

instance
  Core.NFData
    UpdateQueueOutboundCallerConfigResponse
