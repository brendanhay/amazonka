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
-- Module      : Amazonka.GroundStation.UpdateAgentStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For use by AWS Ground Station Agent and shouldn\'t be called directly.
--
-- Update the status of the agent.
module Amazonka.GroundStation.UpdateAgentStatus
  ( -- * Creating a Request
    UpdateAgentStatus (..),
    newUpdateAgentStatus,

    -- * Request Lenses
    updateAgentStatus_agentId,
    updateAgentStatus_aggregateStatus,
    updateAgentStatus_componentStatuses,
    updateAgentStatus_taskId,

    -- * Destructuring the Response
    UpdateAgentStatusResponse (..),
    newUpdateAgentStatusResponse,

    -- * Response Lenses
    updateAgentStatusResponse_httpStatus,
    updateAgentStatusResponse_agentId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAgentStatus' smart constructor.
data UpdateAgentStatus = UpdateAgentStatus'
  { -- | UUID of agent to update.
    agentId :: Prelude.Text,
    -- | Aggregate status for agent.
    aggregateStatus :: AggregateStatus,
    -- | List of component statuses for agent.
    componentStatuses :: [ComponentStatusData],
    -- | GUID of agent task.
    taskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAgentStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentId', 'updateAgentStatus_agentId' - UUID of agent to update.
--
-- 'aggregateStatus', 'updateAgentStatus_aggregateStatus' - Aggregate status for agent.
--
-- 'componentStatuses', 'updateAgentStatus_componentStatuses' - List of component statuses for agent.
--
-- 'taskId', 'updateAgentStatus_taskId' - GUID of agent task.
newUpdateAgentStatus ::
  -- | 'agentId'
  Prelude.Text ->
  -- | 'aggregateStatus'
  AggregateStatus ->
  -- | 'taskId'
  Prelude.Text ->
  UpdateAgentStatus
newUpdateAgentStatus
  pAgentId_
  pAggregateStatus_
  pTaskId_ =
    UpdateAgentStatus'
      { agentId = pAgentId_,
        aggregateStatus = pAggregateStatus_,
        componentStatuses = Prelude.mempty,
        taskId = pTaskId_
      }

-- | UUID of agent to update.
updateAgentStatus_agentId :: Lens.Lens' UpdateAgentStatus Prelude.Text
updateAgentStatus_agentId = Lens.lens (\UpdateAgentStatus' {agentId} -> agentId) (\s@UpdateAgentStatus' {} a -> s {agentId = a} :: UpdateAgentStatus)

-- | Aggregate status for agent.
updateAgentStatus_aggregateStatus :: Lens.Lens' UpdateAgentStatus AggregateStatus
updateAgentStatus_aggregateStatus = Lens.lens (\UpdateAgentStatus' {aggregateStatus} -> aggregateStatus) (\s@UpdateAgentStatus' {} a -> s {aggregateStatus = a} :: UpdateAgentStatus)

-- | List of component statuses for agent.
updateAgentStatus_componentStatuses :: Lens.Lens' UpdateAgentStatus [ComponentStatusData]
updateAgentStatus_componentStatuses = Lens.lens (\UpdateAgentStatus' {componentStatuses} -> componentStatuses) (\s@UpdateAgentStatus' {} a -> s {componentStatuses = a} :: UpdateAgentStatus) Prelude.. Lens.coerced

-- | GUID of agent task.
updateAgentStatus_taskId :: Lens.Lens' UpdateAgentStatus Prelude.Text
updateAgentStatus_taskId = Lens.lens (\UpdateAgentStatus' {taskId} -> taskId) (\s@UpdateAgentStatus' {} a -> s {taskId = a} :: UpdateAgentStatus)

instance Core.AWSRequest UpdateAgentStatus where
  type
    AWSResponse UpdateAgentStatus =
      UpdateAgentStatusResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAgentStatusResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "agentId")
      )

instance Prelude.Hashable UpdateAgentStatus where
  hashWithSalt _salt UpdateAgentStatus' {..} =
    _salt
      `Prelude.hashWithSalt` agentId
      `Prelude.hashWithSalt` aggregateStatus
      `Prelude.hashWithSalt` componentStatuses
      `Prelude.hashWithSalt` taskId

instance Prelude.NFData UpdateAgentStatus where
  rnf UpdateAgentStatus' {..} =
    Prelude.rnf agentId
      `Prelude.seq` Prelude.rnf aggregateStatus
      `Prelude.seq` Prelude.rnf componentStatuses
      `Prelude.seq` Prelude.rnf taskId

instance Data.ToHeaders UpdateAgentStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateAgentStatus where
  toJSON UpdateAgentStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("aggregateStatus" Data..= aggregateStatus),
            Prelude.Just
              ("componentStatuses" Data..= componentStatuses),
            Prelude.Just ("taskId" Data..= taskId)
          ]
      )

instance Data.ToPath UpdateAgentStatus where
  toPath UpdateAgentStatus' {..} =
    Prelude.mconcat ["/agent/", Data.toBS agentId]

instance Data.ToQuery UpdateAgentStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAgentStatusResponse' smart constructor.
data UpdateAgentStatusResponse = UpdateAgentStatusResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | UUID of updated agent.
    agentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAgentStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateAgentStatusResponse_httpStatus' - The response's http status code.
--
-- 'agentId', 'updateAgentStatusResponse_agentId' - UUID of updated agent.
newUpdateAgentStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'agentId'
  Prelude.Text ->
  UpdateAgentStatusResponse
newUpdateAgentStatusResponse pHttpStatus_ pAgentId_ =
  UpdateAgentStatusResponse'
    { httpStatus =
        pHttpStatus_,
      agentId = pAgentId_
    }

-- | The response's http status code.
updateAgentStatusResponse_httpStatus :: Lens.Lens' UpdateAgentStatusResponse Prelude.Int
updateAgentStatusResponse_httpStatus = Lens.lens (\UpdateAgentStatusResponse' {httpStatus} -> httpStatus) (\s@UpdateAgentStatusResponse' {} a -> s {httpStatus = a} :: UpdateAgentStatusResponse)

-- | UUID of updated agent.
updateAgentStatusResponse_agentId :: Lens.Lens' UpdateAgentStatusResponse Prelude.Text
updateAgentStatusResponse_agentId = Lens.lens (\UpdateAgentStatusResponse' {agentId} -> agentId) (\s@UpdateAgentStatusResponse' {} a -> s {agentId = a} :: UpdateAgentStatusResponse)

instance Prelude.NFData UpdateAgentStatusResponse where
  rnf UpdateAgentStatusResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf agentId
