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
-- Module      : Amazonka.ChimeSDKMessaging.UpdateChannelFlow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates channel flow attributes. This is a developer API.
module Amazonka.ChimeSDKMessaging.UpdateChannelFlow
  ( -- * Creating a Request
    UpdateChannelFlow (..),
    newUpdateChannelFlow,

    -- * Request Lenses
    updateChannelFlow_channelFlowArn,
    updateChannelFlow_processors,
    updateChannelFlow_name,

    -- * Destructuring the Response
    UpdateChannelFlowResponse (..),
    newUpdateChannelFlowResponse,

    -- * Response Lenses
    updateChannelFlowResponse_channelFlowArn,
    updateChannelFlowResponse_httpStatus,
  )
where

import Amazonka.ChimeSDKMessaging.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateChannelFlow' smart constructor.
data UpdateChannelFlow = UpdateChannelFlow'
  { -- | The ARN of the channel flow.
    channelFlowArn :: Prelude.Text,
    -- | Information about the processor Lambda functions
    processors :: Prelude.NonEmpty Processor,
    -- | The name of the channel flow.
    name :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateChannelFlow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelFlowArn', 'updateChannelFlow_channelFlowArn' - The ARN of the channel flow.
--
-- 'processors', 'updateChannelFlow_processors' - Information about the processor Lambda functions
--
-- 'name', 'updateChannelFlow_name' - The name of the channel flow.
newUpdateChannelFlow ::
  -- | 'channelFlowArn'
  Prelude.Text ->
  -- | 'processors'
  Prelude.NonEmpty Processor ->
  -- | 'name'
  Prelude.Text ->
  UpdateChannelFlow
newUpdateChannelFlow
  pChannelFlowArn_
  pProcessors_
  pName_ =
    UpdateChannelFlow'
      { channelFlowArn =
          pChannelFlowArn_,
        processors = Lens.coerced Lens.# pProcessors_,
        name = Data._Sensitive Lens.# pName_
      }

-- | The ARN of the channel flow.
updateChannelFlow_channelFlowArn :: Lens.Lens' UpdateChannelFlow Prelude.Text
updateChannelFlow_channelFlowArn = Lens.lens (\UpdateChannelFlow' {channelFlowArn} -> channelFlowArn) (\s@UpdateChannelFlow' {} a -> s {channelFlowArn = a} :: UpdateChannelFlow)

-- | Information about the processor Lambda functions
updateChannelFlow_processors :: Lens.Lens' UpdateChannelFlow (Prelude.NonEmpty Processor)
updateChannelFlow_processors = Lens.lens (\UpdateChannelFlow' {processors} -> processors) (\s@UpdateChannelFlow' {} a -> s {processors = a} :: UpdateChannelFlow) Prelude.. Lens.coerced

-- | The name of the channel flow.
updateChannelFlow_name :: Lens.Lens' UpdateChannelFlow Prelude.Text
updateChannelFlow_name = Lens.lens (\UpdateChannelFlow' {name} -> name) (\s@UpdateChannelFlow' {} a -> s {name = a} :: UpdateChannelFlow) Prelude.. Data._Sensitive

instance Core.AWSRequest UpdateChannelFlow where
  type
    AWSResponse UpdateChannelFlow =
      UpdateChannelFlowResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateChannelFlowResponse'
            Prelude.<$> (x Data..?> "ChannelFlowArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateChannelFlow where
  hashWithSalt _salt UpdateChannelFlow' {..} =
    _salt
      `Prelude.hashWithSalt` channelFlowArn
      `Prelude.hashWithSalt` processors
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateChannelFlow where
  rnf UpdateChannelFlow' {..} =
    Prelude.rnf channelFlowArn
      `Prelude.seq` Prelude.rnf processors
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders UpdateChannelFlow where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateChannelFlow where
  toJSON UpdateChannelFlow' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Processors" Data..= processors),
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath UpdateChannelFlow where
  toPath UpdateChannelFlow' {..} =
    Prelude.mconcat
      ["/channel-flows/", Data.toBS channelFlowArn]

instance Data.ToQuery UpdateChannelFlow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateChannelFlowResponse' smart constructor.
data UpdateChannelFlowResponse = UpdateChannelFlowResponse'
  { -- | The ARN of the channel flow.
    channelFlowArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateChannelFlowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelFlowArn', 'updateChannelFlowResponse_channelFlowArn' - The ARN of the channel flow.
--
-- 'httpStatus', 'updateChannelFlowResponse_httpStatus' - The response's http status code.
newUpdateChannelFlowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateChannelFlowResponse
newUpdateChannelFlowResponse pHttpStatus_ =
  UpdateChannelFlowResponse'
    { channelFlowArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the channel flow.
updateChannelFlowResponse_channelFlowArn :: Lens.Lens' UpdateChannelFlowResponse (Prelude.Maybe Prelude.Text)
updateChannelFlowResponse_channelFlowArn = Lens.lens (\UpdateChannelFlowResponse' {channelFlowArn} -> channelFlowArn) (\s@UpdateChannelFlowResponse' {} a -> s {channelFlowArn = a} :: UpdateChannelFlowResponse)

-- | The response's http status code.
updateChannelFlowResponse_httpStatus :: Lens.Lens' UpdateChannelFlowResponse Prelude.Int
updateChannelFlowResponse_httpStatus = Lens.lens (\UpdateChannelFlowResponse' {httpStatus} -> httpStatus) (\s@UpdateChannelFlowResponse' {} a -> s {httpStatus = a} :: UpdateChannelFlowResponse)

instance Prelude.NFData UpdateChannelFlowResponse where
  rnf UpdateChannelFlowResponse' {..} =
    Prelude.rnf channelFlowArn
      `Prelude.seq` Prelude.rnf httpStatus
