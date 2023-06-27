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
-- Module      : Amazonka.MediaConnect.UpdateBridge
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the bridge
module Amazonka.MediaConnect.UpdateBridge
  ( -- * Creating a Request
    UpdateBridge (..),
    newUpdateBridge,

    -- * Request Lenses
    updateBridge_egressGatewayBridge,
    updateBridge_ingressGatewayBridge,
    updateBridge_sourceFailoverConfig,
    updateBridge_bridgeArn,

    -- * Destructuring the Response
    UpdateBridgeResponse (..),
    newUpdateBridgeResponse,

    -- * Response Lenses
    updateBridgeResponse_bridge,
    updateBridgeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to update the bridge.
--
-- /See:/ 'newUpdateBridge' smart constructor.
data UpdateBridge = UpdateBridge'
  { egressGatewayBridge :: Prelude.Maybe UpdateEgressGatewayBridgeRequest,
    ingressGatewayBridge :: Prelude.Maybe UpdateIngressGatewayBridgeRequest,
    sourceFailoverConfig :: Prelude.Maybe UpdateFailoverConfig,
    -- | The Amazon Resource Number (ARN) of the bridge that you want to update.
    bridgeArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBridge' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'egressGatewayBridge', 'updateBridge_egressGatewayBridge' - Undocumented member.
--
-- 'ingressGatewayBridge', 'updateBridge_ingressGatewayBridge' - Undocumented member.
--
-- 'sourceFailoverConfig', 'updateBridge_sourceFailoverConfig' - Undocumented member.
--
-- 'bridgeArn', 'updateBridge_bridgeArn' - The Amazon Resource Number (ARN) of the bridge that you want to update.
newUpdateBridge ::
  -- | 'bridgeArn'
  Prelude.Text ->
  UpdateBridge
newUpdateBridge pBridgeArn_ =
  UpdateBridge'
    { egressGatewayBridge =
        Prelude.Nothing,
      ingressGatewayBridge = Prelude.Nothing,
      sourceFailoverConfig = Prelude.Nothing,
      bridgeArn = pBridgeArn_
    }

-- | Undocumented member.
updateBridge_egressGatewayBridge :: Lens.Lens' UpdateBridge (Prelude.Maybe UpdateEgressGatewayBridgeRequest)
updateBridge_egressGatewayBridge = Lens.lens (\UpdateBridge' {egressGatewayBridge} -> egressGatewayBridge) (\s@UpdateBridge' {} a -> s {egressGatewayBridge = a} :: UpdateBridge)

-- | Undocumented member.
updateBridge_ingressGatewayBridge :: Lens.Lens' UpdateBridge (Prelude.Maybe UpdateIngressGatewayBridgeRequest)
updateBridge_ingressGatewayBridge = Lens.lens (\UpdateBridge' {ingressGatewayBridge} -> ingressGatewayBridge) (\s@UpdateBridge' {} a -> s {ingressGatewayBridge = a} :: UpdateBridge)

-- | Undocumented member.
updateBridge_sourceFailoverConfig :: Lens.Lens' UpdateBridge (Prelude.Maybe UpdateFailoverConfig)
updateBridge_sourceFailoverConfig = Lens.lens (\UpdateBridge' {sourceFailoverConfig} -> sourceFailoverConfig) (\s@UpdateBridge' {} a -> s {sourceFailoverConfig = a} :: UpdateBridge)

-- | The Amazon Resource Number (ARN) of the bridge that you want to update.
updateBridge_bridgeArn :: Lens.Lens' UpdateBridge Prelude.Text
updateBridge_bridgeArn = Lens.lens (\UpdateBridge' {bridgeArn} -> bridgeArn) (\s@UpdateBridge' {} a -> s {bridgeArn = a} :: UpdateBridge)

instance Core.AWSRequest UpdateBridge where
  type AWSResponse UpdateBridge = UpdateBridgeResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBridgeResponse'
            Prelude.<$> (x Data..?> "bridge")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateBridge where
  hashWithSalt _salt UpdateBridge' {..} =
    _salt
      `Prelude.hashWithSalt` egressGatewayBridge
      `Prelude.hashWithSalt` ingressGatewayBridge
      `Prelude.hashWithSalt` sourceFailoverConfig
      `Prelude.hashWithSalt` bridgeArn

instance Prelude.NFData UpdateBridge where
  rnf UpdateBridge' {..} =
    Prelude.rnf egressGatewayBridge
      `Prelude.seq` Prelude.rnf ingressGatewayBridge
      `Prelude.seq` Prelude.rnf sourceFailoverConfig
      `Prelude.seq` Prelude.rnf bridgeArn

instance Data.ToHeaders UpdateBridge where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateBridge where
  toJSON UpdateBridge' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("egressGatewayBridge" Data..=)
              Prelude.<$> egressGatewayBridge,
            ("ingressGatewayBridge" Data..=)
              Prelude.<$> ingressGatewayBridge,
            ("sourceFailoverConfig" Data..=)
              Prelude.<$> sourceFailoverConfig
          ]
      )

instance Data.ToPath UpdateBridge where
  toPath UpdateBridge' {..} =
    Prelude.mconcat
      ["/v1/bridges/", Data.toBS bridgeArn]

instance Data.ToQuery UpdateBridge where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateBridgeResponse' smart constructor.
data UpdateBridgeResponse = UpdateBridgeResponse'
  { bridge :: Prelude.Maybe Bridge,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBridgeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bridge', 'updateBridgeResponse_bridge' - Undocumented member.
--
-- 'httpStatus', 'updateBridgeResponse_httpStatus' - The response's http status code.
newUpdateBridgeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateBridgeResponse
newUpdateBridgeResponse pHttpStatus_ =
  UpdateBridgeResponse'
    { bridge = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateBridgeResponse_bridge :: Lens.Lens' UpdateBridgeResponse (Prelude.Maybe Bridge)
updateBridgeResponse_bridge = Lens.lens (\UpdateBridgeResponse' {bridge} -> bridge) (\s@UpdateBridgeResponse' {} a -> s {bridge = a} :: UpdateBridgeResponse)

-- | The response's http status code.
updateBridgeResponse_httpStatus :: Lens.Lens' UpdateBridgeResponse Prelude.Int
updateBridgeResponse_httpStatus = Lens.lens (\UpdateBridgeResponse' {httpStatus} -> httpStatus) (\s@UpdateBridgeResponse' {} a -> s {httpStatus = a} :: UpdateBridgeResponse)

instance Prelude.NFData UpdateBridgeResponse where
  rnf UpdateBridgeResponse' {..} =
    Prelude.rnf bridge
      `Prelude.seq` Prelude.rnf httpStatus
