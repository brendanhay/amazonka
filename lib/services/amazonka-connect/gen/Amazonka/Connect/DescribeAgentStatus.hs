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
-- Module      : Amazonka.Connect.DescribeAgentStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Describes an agent status.
module Amazonka.Connect.DescribeAgentStatus
  ( -- * Creating a Request
    DescribeAgentStatus (..),
    newDescribeAgentStatus,

    -- * Request Lenses
    describeAgentStatus_instanceId,
    describeAgentStatus_agentStatusId,

    -- * Destructuring the Response
    DescribeAgentStatusResponse (..),
    newDescribeAgentStatusResponse,

    -- * Response Lenses
    describeAgentStatusResponse_agentStatus,
    describeAgentStatusResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAgentStatus' smart constructor.
data DescribeAgentStatus = DescribeAgentStatus'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier for the agent status.
    agentStatusId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAgentStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'describeAgentStatus_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'agentStatusId', 'describeAgentStatus_agentStatusId' - The identifier for the agent status.
newDescribeAgentStatus ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'agentStatusId'
  Prelude.Text ->
  DescribeAgentStatus
newDescribeAgentStatus pInstanceId_ pAgentStatusId_ =
  DescribeAgentStatus'
    { instanceId = pInstanceId_,
      agentStatusId = pAgentStatusId_
    }

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
describeAgentStatus_instanceId :: Lens.Lens' DescribeAgentStatus Prelude.Text
describeAgentStatus_instanceId = Lens.lens (\DescribeAgentStatus' {instanceId} -> instanceId) (\s@DescribeAgentStatus' {} a -> s {instanceId = a} :: DescribeAgentStatus)

-- | The identifier for the agent status.
describeAgentStatus_agentStatusId :: Lens.Lens' DescribeAgentStatus Prelude.Text
describeAgentStatus_agentStatusId = Lens.lens (\DescribeAgentStatus' {agentStatusId} -> agentStatusId) (\s@DescribeAgentStatus' {} a -> s {agentStatusId = a} :: DescribeAgentStatus)

instance Core.AWSRequest DescribeAgentStatus where
  type
    AWSResponse DescribeAgentStatus =
      DescribeAgentStatusResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAgentStatusResponse'
            Prelude.<$> (x Data..?> "AgentStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAgentStatus where
  hashWithSalt _salt DescribeAgentStatus' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` agentStatusId

instance Prelude.NFData DescribeAgentStatus where
  rnf DescribeAgentStatus' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf agentStatusId

instance Data.ToHeaders DescribeAgentStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeAgentStatus where
  toPath DescribeAgentStatus' {..} =
    Prelude.mconcat
      [ "/agent-status/",
        Data.toBS instanceId,
        "/",
        Data.toBS agentStatusId
      ]

instance Data.ToQuery DescribeAgentStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAgentStatusResponse' smart constructor.
data DescribeAgentStatusResponse = DescribeAgentStatusResponse'
  { -- | The agent status.
    agentStatus :: Prelude.Maybe AgentStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAgentStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentStatus', 'describeAgentStatusResponse_agentStatus' - The agent status.
--
-- 'httpStatus', 'describeAgentStatusResponse_httpStatus' - The response's http status code.
newDescribeAgentStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAgentStatusResponse
newDescribeAgentStatusResponse pHttpStatus_ =
  DescribeAgentStatusResponse'
    { agentStatus =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The agent status.
describeAgentStatusResponse_agentStatus :: Lens.Lens' DescribeAgentStatusResponse (Prelude.Maybe AgentStatus)
describeAgentStatusResponse_agentStatus = Lens.lens (\DescribeAgentStatusResponse' {agentStatus} -> agentStatus) (\s@DescribeAgentStatusResponse' {} a -> s {agentStatus = a} :: DescribeAgentStatusResponse)

-- | The response's http status code.
describeAgentStatusResponse_httpStatus :: Lens.Lens' DescribeAgentStatusResponse Prelude.Int
describeAgentStatusResponse_httpStatus = Lens.lens (\DescribeAgentStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeAgentStatusResponse' {} a -> s {httpStatus = a} :: DescribeAgentStatusResponse)

instance Prelude.NFData DescribeAgentStatusResponse where
  rnf DescribeAgentStatusResponse' {..} =
    Prelude.rnf agentStatus
      `Prelude.seq` Prelude.rnf httpStatus
