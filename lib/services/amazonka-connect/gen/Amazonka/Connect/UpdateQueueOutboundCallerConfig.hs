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
-- Module      : Amazonka.Connect.UpdateQueueOutboundCallerConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
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
--
-- If the number being used in the input is claimed to a traffic
-- distribution group, and you are calling this API using an instance in
-- the Amazon Web Services Region where the traffic distribution group was
-- created, you can use either a full phone number ARN or UUID value for
-- the @OutboundCallerIdNumberId@ value of the
-- <https://docs.aws.amazon.com/connect/latest/APIReference/API_OutboundCallerConfig OutboundCallerConfig>
-- request body parameter. However, if the number is claimed to a traffic
-- distribution group and you are calling this API using an instance in the
-- alternate Amazon Web Services Region associated with the traffic
-- distribution group, you must provide a full phone number ARN. If a UUID
-- is provided in this scenario, you will receive a
-- @ResourceNotFoundException@.
module Amazonka.Connect.UpdateQueueOutboundCallerConfig
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

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateQueueOutboundCallerConfig' smart constructor.
data UpdateQueueOutboundCallerConfig = UpdateQueueOutboundCallerConfig'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier for the queue.
    queueId :: Prelude.Text,
    -- | The outbound caller ID name, number, and outbound whisper flow.
    outboundCallerConfig :: OutboundCallerConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateQueueOutboundCallerConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'updateQueueOutboundCallerConfig_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
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

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
updateQueueOutboundCallerConfig_instanceId :: Lens.Lens' UpdateQueueOutboundCallerConfig Prelude.Text
updateQueueOutboundCallerConfig_instanceId = Lens.lens (\UpdateQueueOutboundCallerConfig' {instanceId} -> instanceId) (\s@UpdateQueueOutboundCallerConfig' {} a -> s {instanceId = a} :: UpdateQueueOutboundCallerConfig)

-- | The identifier for the queue.
updateQueueOutboundCallerConfig_queueId :: Lens.Lens' UpdateQueueOutboundCallerConfig Prelude.Text
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      UpdateQueueOutboundCallerConfigResponse'

instance
  Prelude.Hashable
    UpdateQueueOutboundCallerConfig
  where
  hashWithSalt
    _salt
    UpdateQueueOutboundCallerConfig' {..} =
      _salt `Prelude.hashWithSalt` instanceId
        `Prelude.hashWithSalt` queueId
        `Prelude.hashWithSalt` outboundCallerConfig

instance
  Prelude.NFData
    UpdateQueueOutboundCallerConfig
  where
  rnf UpdateQueueOutboundCallerConfig' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf queueId
      `Prelude.seq` Prelude.rnf outboundCallerConfig

instance
  Core.ToHeaders
    UpdateQueueOutboundCallerConfig
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateQueueOutboundCallerConfig where
  toJSON UpdateQueueOutboundCallerConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "OutboundCallerConfig"
                  Core..= outboundCallerConfig
              )
          ]
      )

instance Core.ToPath UpdateQueueOutboundCallerConfig where
  toPath UpdateQueueOutboundCallerConfig' {..} =
    Prelude.mconcat
      [ "/queues/",
        Core.toBS instanceId,
        "/",
        Core.toBS queueId,
        "/outbound-caller-config"
      ]

instance Core.ToQuery UpdateQueueOutboundCallerConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateQueueOutboundCallerConfigResponse' smart constructor.
data UpdateQueueOutboundCallerConfigResponse = UpdateQueueOutboundCallerConfigResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
