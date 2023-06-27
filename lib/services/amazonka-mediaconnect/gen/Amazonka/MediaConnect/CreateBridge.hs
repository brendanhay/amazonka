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
-- Module      : Amazonka.MediaConnect.CreateBridge
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new bridge. The request must include one source.
module Amazonka.MediaConnect.CreateBridge
  ( -- * Creating a Request
    CreateBridge (..),
    newCreateBridge,

    -- * Request Lenses
    createBridge_egressGatewayBridge,
    createBridge_ingressGatewayBridge,
    createBridge_outputs,
    createBridge_sourceFailoverConfig,
    createBridge_sources,
    createBridge_placementArn,
    createBridge_name,

    -- * Destructuring the Response
    CreateBridgeResponse (..),
    newCreateBridgeResponse,

    -- * Response Lenses
    createBridgeResponse_bridge,
    createBridgeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Creates a new bridge. The request must include one source.
--
-- /See:/ 'newCreateBridge' smart constructor.
data CreateBridge = CreateBridge'
  { -- | Create a bridge with the egress bridge type. An egress bridge is a
    -- cloud-to-ground bridge. The content comes from an existing MediaConnect
    -- flow and is delivered to your premises.
    egressGatewayBridge :: Prelude.Maybe AddEgressGatewayBridgeRequest,
    -- | Create a bridge with the ingress bridge type. An ingress bridge is a
    -- ground-to-cloud bridge. The content originates at your premises and is
    -- delivered to the cloud.
    ingressGatewayBridge :: Prelude.Maybe AddIngressGatewayBridgeRequest,
    -- | The outputs that you want to add to this bridge.
    outputs :: Prelude.Maybe [AddBridgeOutputRequest],
    -- | The settings for source failover.
    sourceFailoverConfig :: Prelude.Maybe FailoverConfig,
    -- | The sources that you want to add to this bridge.
    sources :: [AddBridgeSourceRequest],
    -- | The bridge placement Amazon Resource Number (ARN).
    placementArn :: Prelude.Text,
    -- | The name of the bridge. This name can not be modified after the bridge
    -- is created.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBridge' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'egressGatewayBridge', 'createBridge_egressGatewayBridge' - Create a bridge with the egress bridge type. An egress bridge is a
-- cloud-to-ground bridge. The content comes from an existing MediaConnect
-- flow and is delivered to your premises.
--
-- 'ingressGatewayBridge', 'createBridge_ingressGatewayBridge' - Create a bridge with the ingress bridge type. An ingress bridge is a
-- ground-to-cloud bridge. The content originates at your premises and is
-- delivered to the cloud.
--
-- 'outputs', 'createBridge_outputs' - The outputs that you want to add to this bridge.
--
-- 'sourceFailoverConfig', 'createBridge_sourceFailoverConfig' - The settings for source failover.
--
-- 'sources', 'createBridge_sources' - The sources that you want to add to this bridge.
--
-- 'placementArn', 'createBridge_placementArn' - The bridge placement Amazon Resource Number (ARN).
--
-- 'name', 'createBridge_name' - The name of the bridge. This name can not be modified after the bridge
-- is created.
newCreateBridge ::
  -- | 'placementArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CreateBridge
newCreateBridge pPlacementArn_ pName_ =
  CreateBridge'
    { egressGatewayBridge =
        Prelude.Nothing,
      ingressGatewayBridge = Prelude.Nothing,
      outputs = Prelude.Nothing,
      sourceFailoverConfig = Prelude.Nothing,
      sources = Prelude.mempty,
      placementArn = pPlacementArn_,
      name = pName_
    }

-- | Create a bridge with the egress bridge type. An egress bridge is a
-- cloud-to-ground bridge. The content comes from an existing MediaConnect
-- flow and is delivered to your premises.
createBridge_egressGatewayBridge :: Lens.Lens' CreateBridge (Prelude.Maybe AddEgressGatewayBridgeRequest)
createBridge_egressGatewayBridge = Lens.lens (\CreateBridge' {egressGatewayBridge} -> egressGatewayBridge) (\s@CreateBridge' {} a -> s {egressGatewayBridge = a} :: CreateBridge)

-- | Create a bridge with the ingress bridge type. An ingress bridge is a
-- ground-to-cloud bridge. The content originates at your premises and is
-- delivered to the cloud.
createBridge_ingressGatewayBridge :: Lens.Lens' CreateBridge (Prelude.Maybe AddIngressGatewayBridgeRequest)
createBridge_ingressGatewayBridge = Lens.lens (\CreateBridge' {ingressGatewayBridge} -> ingressGatewayBridge) (\s@CreateBridge' {} a -> s {ingressGatewayBridge = a} :: CreateBridge)

-- | The outputs that you want to add to this bridge.
createBridge_outputs :: Lens.Lens' CreateBridge (Prelude.Maybe [AddBridgeOutputRequest])
createBridge_outputs = Lens.lens (\CreateBridge' {outputs} -> outputs) (\s@CreateBridge' {} a -> s {outputs = a} :: CreateBridge) Prelude.. Lens.mapping Lens.coerced

-- | The settings for source failover.
createBridge_sourceFailoverConfig :: Lens.Lens' CreateBridge (Prelude.Maybe FailoverConfig)
createBridge_sourceFailoverConfig = Lens.lens (\CreateBridge' {sourceFailoverConfig} -> sourceFailoverConfig) (\s@CreateBridge' {} a -> s {sourceFailoverConfig = a} :: CreateBridge)

-- | The sources that you want to add to this bridge.
createBridge_sources :: Lens.Lens' CreateBridge [AddBridgeSourceRequest]
createBridge_sources = Lens.lens (\CreateBridge' {sources} -> sources) (\s@CreateBridge' {} a -> s {sources = a} :: CreateBridge) Prelude.. Lens.coerced

-- | The bridge placement Amazon Resource Number (ARN).
createBridge_placementArn :: Lens.Lens' CreateBridge Prelude.Text
createBridge_placementArn = Lens.lens (\CreateBridge' {placementArn} -> placementArn) (\s@CreateBridge' {} a -> s {placementArn = a} :: CreateBridge)

-- | The name of the bridge. This name can not be modified after the bridge
-- is created.
createBridge_name :: Lens.Lens' CreateBridge Prelude.Text
createBridge_name = Lens.lens (\CreateBridge' {name} -> name) (\s@CreateBridge' {} a -> s {name = a} :: CreateBridge)

instance Core.AWSRequest CreateBridge where
  type AWSResponse CreateBridge = CreateBridgeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBridgeResponse'
            Prelude.<$> (x Data..?> "bridge")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateBridge where
  hashWithSalt _salt CreateBridge' {..} =
    _salt
      `Prelude.hashWithSalt` egressGatewayBridge
      `Prelude.hashWithSalt` ingressGatewayBridge
      `Prelude.hashWithSalt` outputs
      `Prelude.hashWithSalt` sourceFailoverConfig
      `Prelude.hashWithSalt` sources
      `Prelude.hashWithSalt` placementArn
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateBridge where
  rnf CreateBridge' {..} =
    Prelude.rnf egressGatewayBridge
      `Prelude.seq` Prelude.rnf ingressGatewayBridge
      `Prelude.seq` Prelude.rnf outputs
      `Prelude.seq` Prelude.rnf sourceFailoverConfig
      `Prelude.seq` Prelude.rnf sources
      `Prelude.seq` Prelude.rnf placementArn
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateBridge where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateBridge where
  toJSON CreateBridge' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("egressGatewayBridge" Data..=)
              Prelude.<$> egressGatewayBridge,
            ("ingressGatewayBridge" Data..=)
              Prelude.<$> ingressGatewayBridge,
            ("outputs" Data..=) Prelude.<$> outputs,
            ("sourceFailoverConfig" Data..=)
              Prelude.<$> sourceFailoverConfig,
            Prelude.Just ("sources" Data..= sources),
            Prelude.Just ("placementArn" Data..= placementArn),
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateBridge where
  toPath = Prelude.const "/v1/bridges"

instance Data.ToQuery CreateBridge where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateBridgeResponse' smart constructor.
data CreateBridgeResponse = CreateBridgeResponse'
  { bridge :: Prelude.Maybe Bridge,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBridgeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bridge', 'createBridgeResponse_bridge' - Undocumented member.
--
-- 'httpStatus', 'createBridgeResponse_httpStatus' - The response's http status code.
newCreateBridgeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateBridgeResponse
newCreateBridgeResponse pHttpStatus_ =
  CreateBridgeResponse'
    { bridge = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createBridgeResponse_bridge :: Lens.Lens' CreateBridgeResponse (Prelude.Maybe Bridge)
createBridgeResponse_bridge = Lens.lens (\CreateBridgeResponse' {bridge} -> bridge) (\s@CreateBridgeResponse' {} a -> s {bridge = a} :: CreateBridgeResponse)

-- | The response's http status code.
createBridgeResponse_httpStatus :: Lens.Lens' CreateBridgeResponse Prelude.Int
createBridgeResponse_httpStatus = Lens.lens (\CreateBridgeResponse' {httpStatus} -> httpStatus) (\s@CreateBridgeResponse' {} a -> s {httpStatus = a} :: CreateBridgeResponse)

instance Prelude.NFData CreateBridgeResponse where
  rnf CreateBridgeResponse' {..} =
    Prelude.rnf bridge
      `Prelude.seq` Prelude.rnf httpStatus
