{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateQueueOutboundCallerConfig' smart constructor.
data UpdateQueueOutboundCallerConfig = UpdateQueueOutboundCallerConfig'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text,
    -- | The identifier for the queue.
    queueId :: Prelude.Text,
    -- | The outbound caller ID name, number, and outbound whisper flow.
    outboundCallerConfig :: OutboundCallerConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'queueId'
  Prelude.Text ->
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
updateQueueOutboundCallerConfig_instanceId :: Lens.Lens' UpdateQueueOutboundCallerConfig Prelude.Text
updateQueueOutboundCallerConfig_instanceId = Lens.lens (\UpdateQueueOutboundCallerConfig' {instanceId} -> instanceId) (\s@UpdateQueueOutboundCallerConfig' {} a -> s {instanceId = a} :: UpdateQueueOutboundCallerConfig)

-- | The identifier for the queue.
updateQueueOutboundCallerConfig_queueId :: Lens.Lens' UpdateQueueOutboundCallerConfig Prelude.Text
updateQueueOutboundCallerConfig_queueId = Lens.lens (\UpdateQueueOutboundCallerConfig' {queueId} -> queueId) (\s@UpdateQueueOutboundCallerConfig' {} a -> s {queueId = a} :: UpdateQueueOutboundCallerConfig)

-- | The outbound caller ID name, number, and outbound whisper flow.
updateQueueOutboundCallerConfig_outboundCallerConfig :: Lens.Lens' UpdateQueueOutboundCallerConfig OutboundCallerConfig
updateQueueOutboundCallerConfig_outboundCallerConfig = Lens.lens (\UpdateQueueOutboundCallerConfig' {outboundCallerConfig} -> outboundCallerConfig) (\s@UpdateQueueOutboundCallerConfig' {} a -> s {outboundCallerConfig = a} :: UpdateQueueOutboundCallerConfig)

instance
  Prelude.AWSRequest
    UpdateQueueOutboundCallerConfig
  where
  type
    Rs UpdateQueueOutboundCallerConfig =
      UpdateQueueOutboundCallerConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      UpdateQueueOutboundCallerConfigResponse'

instance
  Prelude.Hashable
    UpdateQueueOutboundCallerConfig

instance
  Prelude.NFData
    UpdateQueueOutboundCallerConfig

instance
  Prelude.ToHeaders
    UpdateQueueOutboundCallerConfig
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    UpdateQueueOutboundCallerConfig
  where
  toJSON UpdateQueueOutboundCallerConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "OutboundCallerConfig"
                  Prelude..= outboundCallerConfig
              )
          ]
      )

instance
  Prelude.ToPath
    UpdateQueueOutboundCallerConfig
  where
  toPath UpdateQueueOutboundCallerConfig' {..} =
    Prelude.mconcat
      [ "/queues/",
        Prelude.toBS instanceId,
        "/",
        Prelude.toBS queueId,
        "/outbound-caller-config"
      ]

instance
  Prelude.ToQuery
    UpdateQueueOutboundCallerConfig
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateQueueOutboundCallerConfigResponse' smart constructor.
data UpdateQueueOutboundCallerConfigResponse = UpdateQueueOutboundCallerConfigResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateQueueOutboundCallerConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateQueueOutboundCallerConfigResponse ::
  UpdateQueueOutboundCallerConfigResponse
newUpdateQueueOutboundCallerConfigResponse =
  UpdateQueueOutboundCallerConfigResponse'

instance
  Prelude.NFData
    UpdateQueueOutboundCallerConfigResponse
