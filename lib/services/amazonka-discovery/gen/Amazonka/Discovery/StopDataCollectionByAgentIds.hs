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
-- Module      : Amazonka.Discovery.StopDataCollectionByAgentIds
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Instructs the specified agents to stop collecting data.
module Amazonka.Discovery.StopDataCollectionByAgentIds
  ( -- * Creating a Request
    StopDataCollectionByAgentIds (..),
    newStopDataCollectionByAgentIds,

    -- * Request Lenses
    stopDataCollectionByAgentIds_agentIds,

    -- * Destructuring the Response
    StopDataCollectionByAgentIdsResponse (..),
    newStopDataCollectionByAgentIdsResponse,

    -- * Response Lenses
    stopDataCollectionByAgentIdsResponse_agentsConfigurationStatus,
    stopDataCollectionByAgentIdsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Discovery.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopDataCollectionByAgentIds' smart constructor.
data StopDataCollectionByAgentIds = StopDataCollectionByAgentIds'
  { -- | The IDs of the agents from which to stop collecting data.
    agentIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopDataCollectionByAgentIds' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentIds', 'stopDataCollectionByAgentIds_agentIds' - The IDs of the agents from which to stop collecting data.
newStopDataCollectionByAgentIds ::
  StopDataCollectionByAgentIds
newStopDataCollectionByAgentIds =
  StopDataCollectionByAgentIds'
    { agentIds =
        Prelude.mempty
    }

-- | The IDs of the agents from which to stop collecting data.
stopDataCollectionByAgentIds_agentIds :: Lens.Lens' StopDataCollectionByAgentIds [Prelude.Text]
stopDataCollectionByAgentIds_agentIds = Lens.lens (\StopDataCollectionByAgentIds' {agentIds} -> agentIds) (\s@StopDataCollectionByAgentIds' {} a -> s {agentIds = a} :: StopDataCollectionByAgentIds) Prelude.. Lens.coerced

instance Core.AWSRequest StopDataCollectionByAgentIds where
  type
    AWSResponse StopDataCollectionByAgentIds =
      StopDataCollectionByAgentIdsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopDataCollectionByAgentIdsResponse'
            Prelude.<$> ( x
                            Data..?> "agentsConfigurationStatus"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StopDataCollectionByAgentIds
  where
  hashWithSalt _salt StopDataCollectionByAgentIds' {..} =
    _salt `Prelude.hashWithSalt` agentIds

instance Prelude.NFData StopDataCollectionByAgentIds where
  rnf StopDataCollectionByAgentIds' {..} =
    Prelude.rnf agentIds

instance Data.ToHeaders StopDataCollectionByAgentIds where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSPoseidonService_V2015_11_01.StopDataCollectionByAgentIds" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopDataCollectionByAgentIds where
  toJSON StopDataCollectionByAgentIds' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("agentIds" Data..= agentIds)]
      )

instance Data.ToPath StopDataCollectionByAgentIds where
  toPath = Prelude.const "/"

instance Data.ToQuery StopDataCollectionByAgentIds where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopDataCollectionByAgentIdsResponse' smart constructor.
data StopDataCollectionByAgentIdsResponse = StopDataCollectionByAgentIdsResponse'
  { -- | Information about the agents that were instructed to stop collecting
    -- data. Information includes the agent ID, a description of the operation
    -- performed, and whether the agent configuration was updated.
    agentsConfigurationStatus :: Prelude.Maybe [AgentConfigurationStatus],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopDataCollectionByAgentIdsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentsConfigurationStatus', 'stopDataCollectionByAgentIdsResponse_agentsConfigurationStatus' - Information about the agents that were instructed to stop collecting
-- data. Information includes the agent ID, a description of the operation
-- performed, and whether the agent configuration was updated.
--
-- 'httpStatus', 'stopDataCollectionByAgentIdsResponse_httpStatus' - The response's http status code.
newStopDataCollectionByAgentIdsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopDataCollectionByAgentIdsResponse
newStopDataCollectionByAgentIdsResponse pHttpStatus_ =
  StopDataCollectionByAgentIdsResponse'
    { agentsConfigurationStatus =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the agents that were instructed to stop collecting
-- data. Information includes the agent ID, a description of the operation
-- performed, and whether the agent configuration was updated.
stopDataCollectionByAgentIdsResponse_agentsConfigurationStatus :: Lens.Lens' StopDataCollectionByAgentIdsResponse (Prelude.Maybe [AgentConfigurationStatus])
stopDataCollectionByAgentIdsResponse_agentsConfigurationStatus = Lens.lens (\StopDataCollectionByAgentIdsResponse' {agentsConfigurationStatus} -> agentsConfigurationStatus) (\s@StopDataCollectionByAgentIdsResponse' {} a -> s {agentsConfigurationStatus = a} :: StopDataCollectionByAgentIdsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
stopDataCollectionByAgentIdsResponse_httpStatus :: Lens.Lens' StopDataCollectionByAgentIdsResponse Prelude.Int
stopDataCollectionByAgentIdsResponse_httpStatus = Lens.lens (\StopDataCollectionByAgentIdsResponse' {httpStatus} -> httpStatus) (\s@StopDataCollectionByAgentIdsResponse' {} a -> s {httpStatus = a} :: StopDataCollectionByAgentIdsResponse)

instance
  Prelude.NFData
    StopDataCollectionByAgentIdsResponse
  where
  rnf StopDataCollectionByAgentIdsResponse' {..} =
    Prelude.rnf agentsConfigurationStatus
      `Prelude.seq` Prelude.rnf httpStatus
