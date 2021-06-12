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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopDataCollectionByAgentIds' smart constructor.
data StopDataCollectionByAgentIds = StopDataCollectionByAgentIds'
  { -- | The IDs of the agents or connectors from which to stop collecting data.
    agentIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.mempty
    }

-- | The IDs of the agents or connectors from which to stop collecting data.
stopDataCollectionByAgentIds_agentIds :: Lens.Lens' StopDataCollectionByAgentIds [Core.Text]
stopDataCollectionByAgentIds_agentIds = Lens.lens (\StopDataCollectionByAgentIds' {agentIds} -> agentIds) (\s@StopDataCollectionByAgentIds' {} a -> s {agentIds = a} :: StopDataCollectionByAgentIds) Core.. Lens._Coerce

instance Core.AWSRequest StopDataCollectionByAgentIds where
  type
    AWSResponse StopDataCollectionByAgentIds =
      StopDataCollectionByAgentIdsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopDataCollectionByAgentIdsResponse'
            Core.<$> ( x Core..?> "agentsConfigurationStatus"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StopDataCollectionByAgentIds

instance Core.NFData StopDataCollectionByAgentIds

instance Core.ToHeaders StopDataCollectionByAgentIds where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSPoseidonService_V2015_11_01.StopDataCollectionByAgentIds" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopDataCollectionByAgentIds where
  toJSON StopDataCollectionByAgentIds' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("agentIds" Core..= agentIds)]
      )

instance Core.ToPath StopDataCollectionByAgentIds where
  toPath = Core.const "/"

instance Core.ToQuery StopDataCollectionByAgentIds where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStopDataCollectionByAgentIdsResponse' smart constructor.
data StopDataCollectionByAgentIdsResponse = StopDataCollectionByAgentIdsResponse'
  { -- | Information about the agents or connector that were instructed to stop
    -- collecting data. Information includes the agent\/connector ID, a
    -- description of the operation performed, and whether the agent\/connector
    -- configuration was updated.
    agentsConfigurationStatus :: Core.Maybe [AgentConfigurationStatus],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  StopDataCollectionByAgentIdsResponse
newStopDataCollectionByAgentIdsResponse pHttpStatus_ =
  StopDataCollectionByAgentIdsResponse'
    { agentsConfigurationStatus =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the agents or connector that were instructed to stop
-- collecting data. Information includes the agent\/connector ID, a
-- description of the operation performed, and whether the agent\/connector
-- configuration was updated.
stopDataCollectionByAgentIdsResponse_agentsConfigurationStatus :: Lens.Lens' StopDataCollectionByAgentIdsResponse (Core.Maybe [AgentConfigurationStatus])
stopDataCollectionByAgentIdsResponse_agentsConfigurationStatus = Lens.lens (\StopDataCollectionByAgentIdsResponse' {agentsConfigurationStatus} -> agentsConfigurationStatus) (\s@StopDataCollectionByAgentIdsResponse' {} a -> s {agentsConfigurationStatus = a} :: StopDataCollectionByAgentIdsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
stopDataCollectionByAgentIdsResponse_httpStatus :: Lens.Lens' StopDataCollectionByAgentIdsResponse Core.Int
stopDataCollectionByAgentIdsResponse_httpStatus = Lens.lens (\StopDataCollectionByAgentIdsResponse' {httpStatus} -> httpStatus) (\s@StopDataCollectionByAgentIdsResponse' {} a -> s {httpStatus = a} :: StopDataCollectionByAgentIdsResponse)

instance
  Core.NFData
    StopDataCollectionByAgentIdsResponse
