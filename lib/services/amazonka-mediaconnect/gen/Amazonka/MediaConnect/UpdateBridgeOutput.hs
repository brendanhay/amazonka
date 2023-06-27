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
-- Module      : Amazonka.MediaConnect.UpdateBridgeOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing bridge output.
module Amazonka.MediaConnect.UpdateBridgeOutput
  ( -- * Creating a Request
    UpdateBridgeOutput (..),
    newUpdateBridgeOutput,

    -- * Request Lenses
    updateBridgeOutput_networkOutput,
    updateBridgeOutput_outputName,
    updateBridgeOutput_bridgeArn,

    -- * Destructuring the Response
    UpdateBridgeOutputResponse (..),
    newUpdateBridgeOutputResponse,

    -- * Response Lenses
    updateBridgeOutputResponse_bridgeArn,
    updateBridgeOutputResponse_output,
    updateBridgeOutputResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The fields that you want to update in the bridge output.
--
-- /See:/ 'newUpdateBridgeOutput' smart constructor.
data UpdateBridgeOutput = UpdateBridgeOutput'
  { networkOutput :: Prelude.Maybe UpdateBridgeNetworkOutputRequest,
    -- | The name of the bridge output that you want to update.
    outputName :: Prelude.Text,
    -- | The ARN of the bridge that you want to update.
    bridgeArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBridgeOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkOutput', 'updateBridgeOutput_networkOutput' - Undocumented member.
--
-- 'outputName', 'updateBridgeOutput_outputName' - The name of the bridge output that you want to update.
--
-- 'bridgeArn', 'updateBridgeOutput_bridgeArn' - The ARN of the bridge that you want to update.
newUpdateBridgeOutput ::
  -- | 'outputName'
  Prelude.Text ->
  -- | 'bridgeArn'
  Prelude.Text ->
  UpdateBridgeOutput
newUpdateBridgeOutput pOutputName_ pBridgeArn_ =
  UpdateBridgeOutput'
    { networkOutput =
        Prelude.Nothing,
      outputName = pOutputName_,
      bridgeArn = pBridgeArn_
    }

-- | Undocumented member.
updateBridgeOutput_networkOutput :: Lens.Lens' UpdateBridgeOutput (Prelude.Maybe UpdateBridgeNetworkOutputRequest)
updateBridgeOutput_networkOutput = Lens.lens (\UpdateBridgeOutput' {networkOutput} -> networkOutput) (\s@UpdateBridgeOutput' {} a -> s {networkOutput = a} :: UpdateBridgeOutput)

-- | The name of the bridge output that you want to update.
updateBridgeOutput_outputName :: Lens.Lens' UpdateBridgeOutput Prelude.Text
updateBridgeOutput_outputName = Lens.lens (\UpdateBridgeOutput' {outputName} -> outputName) (\s@UpdateBridgeOutput' {} a -> s {outputName = a} :: UpdateBridgeOutput)

-- | The ARN of the bridge that you want to update.
updateBridgeOutput_bridgeArn :: Lens.Lens' UpdateBridgeOutput Prelude.Text
updateBridgeOutput_bridgeArn = Lens.lens (\UpdateBridgeOutput' {bridgeArn} -> bridgeArn) (\s@UpdateBridgeOutput' {} a -> s {bridgeArn = a} :: UpdateBridgeOutput)

instance Core.AWSRequest UpdateBridgeOutput where
  type
    AWSResponse UpdateBridgeOutput =
      UpdateBridgeOutputResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBridgeOutputResponse'
            Prelude.<$> (x Data..?> "bridgeArn")
            Prelude.<*> (x Data..?> "output")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateBridgeOutput where
  hashWithSalt _salt UpdateBridgeOutput' {..} =
    _salt
      `Prelude.hashWithSalt` networkOutput
      `Prelude.hashWithSalt` outputName
      `Prelude.hashWithSalt` bridgeArn

instance Prelude.NFData UpdateBridgeOutput where
  rnf UpdateBridgeOutput' {..} =
    Prelude.rnf networkOutput
      `Prelude.seq` Prelude.rnf outputName
      `Prelude.seq` Prelude.rnf bridgeArn

instance Data.ToHeaders UpdateBridgeOutput where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateBridgeOutput where
  toJSON UpdateBridgeOutput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("networkOutput" Data..=)
              Prelude.<$> networkOutput
          ]
      )

instance Data.ToPath UpdateBridgeOutput where
  toPath UpdateBridgeOutput' {..} =
    Prelude.mconcat
      [ "/v1/bridges/",
        Data.toBS bridgeArn,
        "/outputs/",
        Data.toBS outputName
      ]

instance Data.ToQuery UpdateBridgeOutput where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateBridgeOutputResponse' smart constructor.
data UpdateBridgeOutputResponse = UpdateBridgeOutputResponse'
  { -- | The Amazon Resource Number (ARN) of the bridge.
    bridgeArn :: Prelude.Maybe Prelude.Text,
    -- | The output that you updated.
    output :: Prelude.Maybe BridgeOutput,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBridgeOutputResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bridgeArn', 'updateBridgeOutputResponse_bridgeArn' - The Amazon Resource Number (ARN) of the bridge.
--
-- 'output', 'updateBridgeOutputResponse_output' - The output that you updated.
--
-- 'httpStatus', 'updateBridgeOutputResponse_httpStatus' - The response's http status code.
newUpdateBridgeOutputResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateBridgeOutputResponse
newUpdateBridgeOutputResponse pHttpStatus_ =
  UpdateBridgeOutputResponse'
    { bridgeArn =
        Prelude.Nothing,
      output = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Number (ARN) of the bridge.
updateBridgeOutputResponse_bridgeArn :: Lens.Lens' UpdateBridgeOutputResponse (Prelude.Maybe Prelude.Text)
updateBridgeOutputResponse_bridgeArn = Lens.lens (\UpdateBridgeOutputResponse' {bridgeArn} -> bridgeArn) (\s@UpdateBridgeOutputResponse' {} a -> s {bridgeArn = a} :: UpdateBridgeOutputResponse)

-- | The output that you updated.
updateBridgeOutputResponse_output :: Lens.Lens' UpdateBridgeOutputResponse (Prelude.Maybe BridgeOutput)
updateBridgeOutputResponse_output = Lens.lens (\UpdateBridgeOutputResponse' {output} -> output) (\s@UpdateBridgeOutputResponse' {} a -> s {output = a} :: UpdateBridgeOutputResponse)

-- | The response's http status code.
updateBridgeOutputResponse_httpStatus :: Lens.Lens' UpdateBridgeOutputResponse Prelude.Int
updateBridgeOutputResponse_httpStatus = Lens.lens (\UpdateBridgeOutputResponse' {httpStatus} -> httpStatus) (\s@UpdateBridgeOutputResponse' {} a -> s {httpStatus = a} :: UpdateBridgeOutputResponse)

instance Prelude.NFData UpdateBridgeOutputResponse where
  rnf UpdateBridgeOutputResponse' {..} =
    Prelude.rnf bridgeArn
      `Prelude.seq` Prelude.rnf output
      `Prelude.seq` Prelude.rnf httpStatus
