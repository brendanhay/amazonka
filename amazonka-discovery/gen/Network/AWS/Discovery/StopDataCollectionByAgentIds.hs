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
-- Module      : Network.AWS.Discovery.StopDataCollectionByAgentIds
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Instructs the specified agents or connectors to stop collecting data.
module Network.AWS.Discovery.StopDataCollectionByAgentIds
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

import qualified Network.AWS.Core as Core
import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopDataCollectionByAgentIds' smart constructor.
data StopDataCollectionByAgentIds = StopDataCollectionByAgentIds'
  { -- | The IDs of the agents or connectors from which to stop collecting data.
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
-- 'agentIds', 'stopDataCollectionByAgentIds_agentIds' - The IDs of the agents or connectors from which to stop collecting data.
newStopDataCollectionByAgentIds ::
  StopDataCollectionByAgentIds
newStopDataCollectionByAgentIds =
  StopDataCollectionByAgentIds'
    { agentIds =
        Prelude.mempty
    }

-- | The IDs of the agents or connectors from which to stop collecting data.
stopDataCollectionByAgentIds_agentIds :: Lens.Lens' StopDataCollectionByAgentIds [Prelude.Text]
stopDataCollectionByAgentIds_agentIds = Lens.lens (\StopDataCollectionByAgentIds' {agentIds} -> agentIds) (\s@StopDataCollectionByAgentIds' {} a -> s {agentIds = a} :: StopDataCollectionByAgentIds) Prelude.. Lens._Coerce

instance Core.AWSRequest StopDataCollectionByAgentIds where
  type
    AWSResponse StopDataCollectionByAgentIds =
      StopDataCollectionByAgentIdsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopDataCollectionByAgentIdsResponse'
            Prelude.<$> ( x Core..?> "agentsConfigurationStatus"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StopDataCollectionByAgentIds

instance Prelude.NFData StopDataCollectionByAgentIds

instance Core.ToHeaders StopDataCollectionByAgentIds where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSPoseidonService_V2015_11_01.StopDataCollectionByAgentIds" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StopDataCollectionByAgentIds where
  toJSON StopDataCollectionByAgentIds' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("agentIds" Core..= agentIds)]
      )

instance Core.ToPath StopDataCollectionByAgentIds where
  toPath = Prelude.const "/"

instance Core.ToQuery StopDataCollectionByAgentIds where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopDataCollectionByAgentIdsResponse' smart constructor.
data StopDataCollectionByAgentIdsResponse = StopDataCollectionByAgentIdsResponse'
  { -- | Information about the agents or connector that were instructed to stop
    -- collecting data. Information includes the agent\/connector ID, a
    -- description of the operation performed, and whether the agent\/connector
    -- configuration was updated.
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
-- 'agentsConfigurationStatus', 'stopDataCollectionByAgentIdsResponse_agentsConfigurationStatus' - Information about the agents or connector that were instructed to stop
-- collecting data. Information includes the agent\/connector ID, a
-- description of the operation performed, and whether the agent\/connector
-- configuration was updated.
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

-- | Information about the agents or connector that were instructed to stop
-- collecting data. Information includes the agent\/connector ID, a
-- description of the operation performed, and whether the agent\/connector
-- configuration was updated.
stopDataCollectionByAgentIdsResponse_agentsConfigurationStatus :: Lens.Lens' StopDataCollectionByAgentIdsResponse (Prelude.Maybe [AgentConfigurationStatus])
stopDataCollectionByAgentIdsResponse_agentsConfigurationStatus = Lens.lens (\StopDataCollectionByAgentIdsResponse' {agentsConfigurationStatus} -> agentsConfigurationStatus) (\s@StopDataCollectionByAgentIdsResponse' {} a -> s {agentsConfigurationStatus = a} :: StopDataCollectionByAgentIdsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
stopDataCollectionByAgentIdsResponse_httpStatus :: Lens.Lens' StopDataCollectionByAgentIdsResponse Prelude.Int
stopDataCollectionByAgentIdsResponse_httpStatus = Lens.lens (\StopDataCollectionByAgentIdsResponse' {httpStatus} -> httpStatus) (\s@StopDataCollectionByAgentIdsResponse' {} a -> s {httpStatus = a} :: StopDataCollectionByAgentIdsResponse)

instance
  Prelude.NFData
    StopDataCollectionByAgentIdsResponse
