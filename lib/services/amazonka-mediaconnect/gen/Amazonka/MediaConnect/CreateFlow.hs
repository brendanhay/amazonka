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
-- Module      : Amazonka.MediaConnect.CreateFlow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new flow. The request must include one source. The request
-- optionally can include outputs (up to 50) and entitlements (up to 50).
module Amazonka.MediaConnect.CreateFlow
  ( -- * Creating a Request
    CreateFlow (..),
    newCreateFlow,

    -- * Request Lenses
    createFlow_availabilityZone,
    createFlow_entitlements,
    createFlow_maintenance,
    createFlow_mediaStreams,
    createFlow_outputs,
    createFlow_source,
    createFlow_sourceFailoverConfig,
    createFlow_sources,
    createFlow_vpcInterfaces,
    createFlow_name,

    -- * Destructuring the Response
    CreateFlowResponse (..),
    newCreateFlowResponse,

    -- * Response Lenses
    createFlowResponse_flow,
    createFlowResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Creates a new flow. The request must include one source. The request
-- optionally can include outputs (up to 50) and entitlements (up to 50).
--
-- /See:/ 'newCreateFlow' smart constructor.
data CreateFlow = CreateFlow'
  { -- | The Availability Zone that you want to create the flow in. These options
    -- are limited to the Availability Zones within the current AWS Region.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The entitlements that you want to grant on a flow.
    entitlements :: Prelude.Maybe [GrantEntitlementRequest],
    maintenance :: Prelude.Maybe AddMaintenance,
    -- | The media streams that you want to add to the flow. You can associate
    -- these media streams with sources and outputs on the flow.
    mediaStreams :: Prelude.Maybe [AddMediaStreamRequest],
    -- | The outputs that you want to add to this flow.
    outputs :: Prelude.Maybe [AddOutputRequest],
    source :: Prelude.Maybe SetSourceRequest,
    sourceFailoverConfig :: Prelude.Maybe FailoverConfig,
    sources :: Prelude.Maybe [SetSourceRequest],
    -- | The VPC interfaces you want on the flow.
    vpcInterfaces :: Prelude.Maybe [VpcInterfaceRequest],
    -- | The name of the flow.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFlow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZone', 'createFlow_availabilityZone' - The Availability Zone that you want to create the flow in. These options
-- are limited to the Availability Zones within the current AWS Region.
--
-- 'entitlements', 'createFlow_entitlements' - The entitlements that you want to grant on a flow.
--
-- 'maintenance', 'createFlow_maintenance' - Undocumented member.
--
-- 'mediaStreams', 'createFlow_mediaStreams' - The media streams that you want to add to the flow. You can associate
-- these media streams with sources and outputs on the flow.
--
-- 'outputs', 'createFlow_outputs' - The outputs that you want to add to this flow.
--
-- 'source', 'createFlow_source' - Undocumented member.
--
-- 'sourceFailoverConfig', 'createFlow_sourceFailoverConfig' - Undocumented member.
--
-- 'sources', 'createFlow_sources' - Undocumented member.
--
-- 'vpcInterfaces', 'createFlow_vpcInterfaces' - The VPC interfaces you want on the flow.
--
-- 'name', 'createFlow_name' - The name of the flow.
newCreateFlow ::
  -- | 'name'
  Prelude.Text ->
  CreateFlow
newCreateFlow pName_ =
  CreateFlow'
    { availabilityZone = Prelude.Nothing,
      entitlements = Prelude.Nothing,
      maintenance = Prelude.Nothing,
      mediaStreams = Prelude.Nothing,
      outputs = Prelude.Nothing,
      source = Prelude.Nothing,
      sourceFailoverConfig = Prelude.Nothing,
      sources = Prelude.Nothing,
      vpcInterfaces = Prelude.Nothing,
      name = pName_
    }

-- | The Availability Zone that you want to create the flow in. These options
-- are limited to the Availability Zones within the current AWS Region.
createFlow_availabilityZone :: Lens.Lens' CreateFlow (Prelude.Maybe Prelude.Text)
createFlow_availabilityZone = Lens.lens (\CreateFlow' {availabilityZone} -> availabilityZone) (\s@CreateFlow' {} a -> s {availabilityZone = a} :: CreateFlow)

-- | The entitlements that you want to grant on a flow.
createFlow_entitlements :: Lens.Lens' CreateFlow (Prelude.Maybe [GrantEntitlementRequest])
createFlow_entitlements = Lens.lens (\CreateFlow' {entitlements} -> entitlements) (\s@CreateFlow' {} a -> s {entitlements = a} :: CreateFlow) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createFlow_maintenance :: Lens.Lens' CreateFlow (Prelude.Maybe AddMaintenance)
createFlow_maintenance = Lens.lens (\CreateFlow' {maintenance} -> maintenance) (\s@CreateFlow' {} a -> s {maintenance = a} :: CreateFlow)

-- | The media streams that you want to add to the flow. You can associate
-- these media streams with sources and outputs on the flow.
createFlow_mediaStreams :: Lens.Lens' CreateFlow (Prelude.Maybe [AddMediaStreamRequest])
createFlow_mediaStreams = Lens.lens (\CreateFlow' {mediaStreams} -> mediaStreams) (\s@CreateFlow' {} a -> s {mediaStreams = a} :: CreateFlow) Prelude.. Lens.mapping Lens.coerced

-- | The outputs that you want to add to this flow.
createFlow_outputs :: Lens.Lens' CreateFlow (Prelude.Maybe [AddOutputRequest])
createFlow_outputs = Lens.lens (\CreateFlow' {outputs} -> outputs) (\s@CreateFlow' {} a -> s {outputs = a} :: CreateFlow) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createFlow_source :: Lens.Lens' CreateFlow (Prelude.Maybe SetSourceRequest)
createFlow_source = Lens.lens (\CreateFlow' {source} -> source) (\s@CreateFlow' {} a -> s {source = a} :: CreateFlow)

-- | Undocumented member.
createFlow_sourceFailoverConfig :: Lens.Lens' CreateFlow (Prelude.Maybe FailoverConfig)
createFlow_sourceFailoverConfig = Lens.lens (\CreateFlow' {sourceFailoverConfig} -> sourceFailoverConfig) (\s@CreateFlow' {} a -> s {sourceFailoverConfig = a} :: CreateFlow)

-- | Undocumented member.
createFlow_sources :: Lens.Lens' CreateFlow (Prelude.Maybe [SetSourceRequest])
createFlow_sources = Lens.lens (\CreateFlow' {sources} -> sources) (\s@CreateFlow' {} a -> s {sources = a} :: CreateFlow) Prelude.. Lens.mapping Lens.coerced

-- | The VPC interfaces you want on the flow.
createFlow_vpcInterfaces :: Lens.Lens' CreateFlow (Prelude.Maybe [VpcInterfaceRequest])
createFlow_vpcInterfaces = Lens.lens (\CreateFlow' {vpcInterfaces} -> vpcInterfaces) (\s@CreateFlow' {} a -> s {vpcInterfaces = a} :: CreateFlow) Prelude.. Lens.mapping Lens.coerced

-- | The name of the flow.
createFlow_name :: Lens.Lens' CreateFlow Prelude.Text
createFlow_name = Lens.lens (\CreateFlow' {name} -> name) (\s@CreateFlow' {} a -> s {name = a} :: CreateFlow)

instance Core.AWSRequest CreateFlow where
  type AWSResponse CreateFlow = CreateFlowResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFlowResponse'
            Prelude.<$> (x Data..?> "flow")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFlow where
  hashWithSalt _salt CreateFlow' {..} =
    _salt
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` entitlements
      `Prelude.hashWithSalt` maintenance
      `Prelude.hashWithSalt` mediaStreams
      `Prelude.hashWithSalt` outputs
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` sourceFailoverConfig
      `Prelude.hashWithSalt` sources
      `Prelude.hashWithSalt` vpcInterfaces
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateFlow where
  rnf CreateFlow' {..} =
    Prelude.rnf availabilityZone `Prelude.seq`
      Prelude.rnf entitlements `Prelude.seq`
        Prelude.rnf maintenance `Prelude.seq`
          Prelude.rnf mediaStreams `Prelude.seq`
            Prelude.rnf outputs `Prelude.seq`
              Prelude.rnf source `Prelude.seq`
                Prelude.rnf sourceFailoverConfig `Prelude.seq`
                  Prelude.rnf sources `Prelude.seq`
                    Prelude.rnf vpcInterfaces `Prelude.seq`
                      Prelude.rnf name

instance Data.ToHeaders CreateFlow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateFlow where
  toJSON CreateFlow' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("availabilityZone" Data..=)
              Prelude.<$> availabilityZone,
            ("entitlements" Data..=) Prelude.<$> entitlements,
            ("maintenance" Data..=) Prelude.<$> maintenance,
            ("mediaStreams" Data..=) Prelude.<$> mediaStreams,
            ("outputs" Data..=) Prelude.<$> outputs,
            ("source" Data..=) Prelude.<$> source,
            ("sourceFailoverConfig" Data..=)
              Prelude.<$> sourceFailoverConfig,
            ("sources" Data..=) Prelude.<$> sources,
            ("vpcInterfaces" Data..=) Prelude.<$> vpcInterfaces,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateFlow where
  toPath = Prelude.const "/v1/flows"

instance Data.ToQuery CreateFlow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFlowResponse' smart constructor.
data CreateFlowResponse = CreateFlowResponse'
  { flow :: Prelude.Maybe Flow,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFlowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flow', 'createFlowResponse_flow' - Undocumented member.
--
-- 'httpStatus', 'createFlowResponse_httpStatus' - The response's http status code.
newCreateFlowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFlowResponse
newCreateFlowResponse pHttpStatus_ =
  CreateFlowResponse'
    { flow = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createFlowResponse_flow :: Lens.Lens' CreateFlowResponse (Prelude.Maybe Flow)
createFlowResponse_flow = Lens.lens (\CreateFlowResponse' {flow} -> flow) (\s@CreateFlowResponse' {} a -> s {flow = a} :: CreateFlowResponse)

-- | The response's http status code.
createFlowResponse_httpStatus :: Lens.Lens' CreateFlowResponse Prelude.Int
createFlowResponse_httpStatus = Lens.lens (\CreateFlowResponse' {httpStatus} -> httpStatus) (\s@CreateFlowResponse' {} a -> s {httpStatus = a} :: CreateFlowResponse)

instance Prelude.NFData CreateFlowResponse where
  rnf CreateFlowResponse' {..} =
    Prelude.rnf flow `Prelude.seq`
      Prelude.rnf httpStatus
