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
-- Module      : Amazonka.GroundStation.RegisterAgent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For use by AWS Ground Station Agent and shouldn\'t be called directly.
--
-- Registers a new agent with AWS Ground Station.
module Amazonka.GroundStation.RegisterAgent
  ( -- * Creating a Request
    RegisterAgent (..),
    newRegisterAgent,

    -- * Request Lenses
    registerAgent_agentDetails,
    registerAgent_discoveryData,

    -- * Destructuring the Response
    RegisterAgentResponse (..),
    newRegisterAgentResponse,

    -- * Response Lenses
    registerAgentResponse_agentId,
    registerAgentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRegisterAgent' smart constructor.
data RegisterAgent = RegisterAgent'
  { -- | Detailed information about the agent being registered.
    agentDetails :: AgentDetails,
    -- | Data for associating an agent with the capabilities it is managing.
    discoveryData :: DiscoveryData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterAgent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentDetails', 'registerAgent_agentDetails' - Detailed information about the agent being registered.
--
-- 'discoveryData', 'registerAgent_discoveryData' - Data for associating an agent with the capabilities it is managing.
newRegisterAgent ::
  -- | 'agentDetails'
  AgentDetails ->
  -- | 'discoveryData'
  DiscoveryData ->
  RegisterAgent
newRegisterAgent pAgentDetails_ pDiscoveryData_ =
  RegisterAgent'
    { agentDetails = pAgentDetails_,
      discoveryData = pDiscoveryData_
    }

-- | Detailed information about the agent being registered.
registerAgent_agentDetails :: Lens.Lens' RegisterAgent AgentDetails
registerAgent_agentDetails = Lens.lens (\RegisterAgent' {agentDetails} -> agentDetails) (\s@RegisterAgent' {} a -> s {agentDetails = a} :: RegisterAgent)

-- | Data for associating an agent with the capabilities it is managing.
registerAgent_discoveryData :: Lens.Lens' RegisterAgent DiscoveryData
registerAgent_discoveryData = Lens.lens (\RegisterAgent' {discoveryData} -> discoveryData) (\s@RegisterAgent' {} a -> s {discoveryData = a} :: RegisterAgent)

instance Core.AWSRequest RegisterAgent where
  type
    AWSResponse RegisterAgent =
      RegisterAgentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterAgentResponse'
            Prelude.<$> (x Data..?> "agentId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterAgent where
  hashWithSalt _salt RegisterAgent' {..} =
    _salt
      `Prelude.hashWithSalt` agentDetails
      `Prelude.hashWithSalt` discoveryData

instance Prelude.NFData RegisterAgent where
  rnf RegisterAgent' {..} =
    Prelude.rnf agentDetails
      `Prelude.seq` Prelude.rnf discoveryData

instance Data.ToHeaders RegisterAgent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RegisterAgent where
  toJSON RegisterAgent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("agentDetails" Data..= agentDetails),
            Prelude.Just
              ("discoveryData" Data..= discoveryData)
          ]
      )

instance Data.ToPath RegisterAgent where
  toPath = Prelude.const "/agent"

instance Data.ToQuery RegisterAgent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterAgentResponse' smart constructor.
data RegisterAgentResponse = RegisterAgentResponse'
  { -- | UUID of registered agent.
    agentId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterAgentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentId', 'registerAgentResponse_agentId' - UUID of registered agent.
--
-- 'httpStatus', 'registerAgentResponse_httpStatus' - The response's http status code.
newRegisterAgentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterAgentResponse
newRegisterAgentResponse pHttpStatus_ =
  RegisterAgentResponse'
    { agentId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | UUID of registered agent.
registerAgentResponse_agentId :: Lens.Lens' RegisterAgentResponse (Prelude.Maybe Prelude.Text)
registerAgentResponse_agentId = Lens.lens (\RegisterAgentResponse' {agentId} -> agentId) (\s@RegisterAgentResponse' {} a -> s {agentId = a} :: RegisterAgentResponse)

-- | The response's http status code.
registerAgentResponse_httpStatus :: Lens.Lens' RegisterAgentResponse Prelude.Int
registerAgentResponse_httpStatus = Lens.lens (\RegisterAgentResponse' {httpStatus} -> httpStatus) (\s@RegisterAgentResponse' {} a -> s {httpStatus = a} :: RegisterAgentResponse)

instance Prelude.NFData RegisterAgentResponse where
  rnf RegisterAgentResponse' {..} =
    Prelude.rnf agentId
      `Prelude.seq` Prelude.rnf httpStatus
