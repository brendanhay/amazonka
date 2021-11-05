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
-- Module      : Network.AWS.DataSync.DeleteAgent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an agent. To specify which agent to delete, use the Amazon
-- Resource Name (ARN) of the agent in your request. The operation
-- disassociates the agent from your Amazon Web Services account. However,
-- it doesn\'t delete the agent virtual machine (VM) from your on-premises
-- environment.
module Network.AWS.DataSync.DeleteAgent
  ( -- * Creating a Request
    DeleteAgent (..),
    newDeleteAgent,

    -- * Request Lenses
    deleteAgent_agentArn,

    -- * Destructuring the Response
    DeleteAgentResponse (..),
    newDeleteAgentResponse,

    -- * Response Lenses
    deleteAgentResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DataSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | DeleteAgentRequest
--
-- /See:/ 'newDeleteAgent' smart constructor.
data DeleteAgent = DeleteAgent'
  { -- | The Amazon Resource Name (ARN) of the agent to delete. Use the
    -- @ListAgents@ operation to return a list of agents for your account and
    -- Amazon Web Services Region.
    agentArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAgent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentArn', 'deleteAgent_agentArn' - The Amazon Resource Name (ARN) of the agent to delete. Use the
-- @ListAgents@ operation to return a list of agents for your account and
-- Amazon Web Services Region.
newDeleteAgent ::
  -- | 'agentArn'
  Prelude.Text ->
  DeleteAgent
newDeleteAgent pAgentArn_ =
  DeleteAgent' {agentArn = pAgentArn_}

-- | The Amazon Resource Name (ARN) of the agent to delete. Use the
-- @ListAgents@ operation to return a list of agents for your account and
-- Amazon Web Services Region.
deleteAgent_agentArn :: Lens.Lens' DeleteAgent Prelude.Text
deleteAgent_agentArn = Lens.lens (\DeleteAgent' {agentArn} -> agentArn) (\s@DeleteAgent' {} a -> s {agentArn = a} :: DeleteAgent)

instance Core.AWSRequest DeleteAgent where
  type AWSResponse DeleteAgent = DeleteAgentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAgentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAgent

instance Prelude.NFData DeleteAgent

instance Core.ToHeaders DeleteAgent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("FmrsService.DeleteAgent" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteAgent where
  toJSON DeleteAgent' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("AgentArn" Core..= agentArn)]
      )

instance Core.ToPath DeleteAgent where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteAgent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAgentResponse' smart constructor.
data DeleteAgentResponse = DeleteAgentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAgentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAgentResponse_httpStatus' - The response's http status code.
newDeleteAgentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAgentResponse
newDeleteAgentResponse pHttpStatus_ =
  DeleteAgentResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteAgentResponse_httpStatus :: Lens.Lens' DeleteAgentResponse Prelude.Int
deleteAgentResponse_httpStatus = Lens.lens (\DeleteAgentResponse' {httpStatus} -> httpStatus) (\s@DeleteAgentResponse' {} a -> s {httpStatus = a} :: DeleteAgentResponse)

instance Prelude.NFData DeleteAgentResponse
