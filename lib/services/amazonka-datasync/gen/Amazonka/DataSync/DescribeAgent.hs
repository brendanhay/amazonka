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
-- Module      : Amazonka.DataSync.DescribeAgent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata such as the name, the network interfaces, and the
-- status (that is, whether the agent is running or not) for an agent. To
-- specify which agent to describe, use the Amazon Resource Name (ARN) of
-- the agent in your request.
module Amazonka.DataSync.DescribeAgent
  ( -- * Creating a Request
    DescribeAgent (..),
    newDescribeAgent,

    -- * Request Lenses
    describeAgent_agentArn,

    -- * Destructuring the Response
    DescribeAgentResponse (..),
    newDescribeAgentResponse,

    -- * Response Lenses
    describeAgentResponse_agentArn,
    describeAgentResponse_creationTime,
    describeAgentResponse_endpointType,
    describeAgentResponse_lastConnectionTime,
    describeAgentResponse_name,
    describeAgentResponse_privateLinkConfig,
    describeAgentResponse_status,
    describeAgentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | DescribeAgent
--
-- /See:/ 'newDescribeAgent' smart constructor.
data DescribeAgent = DescribeAgent'
  { -- | The Amazon Resource Name (ARN) of the agent to describe.
    agentArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAgent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentArn', 'describeAgent_agentArn' - The Amazon Resource Name (ARN) of the agent to describe.
newDescribeAgent ::
  -- | 'agentArn'
  Prelude.Text ->
  DescribeAgent
newDescribeAgent pAgentArn_ =
  DescribeAgent' {agentArn = pAgentArn_}

-- | The Amazon Resource Name (ARN) of the agent to describe.
describeAgent_agentArn :: Lens.Lens' DescribeAgent Prelude.Text
describeAgent_agentArn = Lens.lens (\DescribeAgent' {agentArn} -> agentArn) (\s@DescribeAgent' {} a -> s {agentArn = a} :: DescribeAgent)

instance Core.AWSRequest DescribeAgent where
  type
    AWSResponse DescribeAgent =
      DescribeAgentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAgentResponse'
            Prelude.<$> (x Data..?> "AgentArn")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "EndpointType")
            Prelude.<*> (x Data..?> "LastConnectionTime")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "PrivateLinkConfig")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAgent where
  hashWithSalt _salt DescribeAgent' {..} =
    _salt `Prelude.hashWithSalt` agentArn

instance Prelude.NFData DescribeAgent where
  rnf DescribeAgent' {..} = Prelude.rnf agentArn

instance Data.ToHeaders DescribeAgent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("FmrsService.DescribeAgent" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeAgent where
  toJSON DescribeAgent' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("AgentArn" Data..= agentArn)]
      )

instance Data.ToPath DescribeAgent where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeAgent where
  toQuery = Prelude.const Prelude.mempty

-- | DescribeAgentResponse
--
-- /See:/ 'newDescribeAgentResponse' smart constructor.
data DescribeAgentResponse = DescribeAgentResponse'
  { -- | The Amazon Resource Name (ARN) of the agent.
    agentArn :: Prelude.Maybe Prelude.Text,
    -- | The time that the agent was activated (that is, created in your
    -- account).
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The type of endpoint that your agent is connected to. If the endpoint is
    -- a VPC endpoint, the agent is not accessible over the public internet.
    endpointType :: Prelude.Maybe EndpointType,
    -- | The time that the agent last connected to DataSync.
    lastConnectionTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the agent.
    name :: Prelude.Maybe Prelude.Text,
    -- | The subnet and the security group that DataSync used to access a VPC
    -- endpoint.
    privateLinkConfig :: Prelude.Maybe PrivateLinkConfig,
    -- | The status of the agent. If the status is ONLINE, then the agent is
    -- configured properly and is available to use. The Running status is the
    -- normal running status for an agent. If the status is OFFLINE, the
    -- agent\'s VM is turned off or the agent is in an unhealthy state. When
    -- the issue that caused the unhealthy state is resolved, the agent returns
    -- to ONLINE status.
    status :: Prelude.Maybe AgentStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAgentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentArn', 'describeAgentResponse_agentArn' - The Amazon Resource Name (ARN) of the agent.
--
-- 'creationTime', 'describeAgentResponse_creationTime' - The time that the agent was activated (that is, created in your
-- account).
--
-- 'endpointType', 'describeAgentResponse_endpointType' - The type of endpoint that your agent is connected to. If the endpoint is
-- a VPC endpoint, the agent is not accessible over the public internet.
--
-- 'lastConnectionTime', 'describeAgentResponse_lastConnectionTime' - The time that the agent last connected to DataSync.
--
-- 'name', 'describeAgentResponse_name' - The name of the agent.
--
-- 'privateLinkConfig', 'describeAgentResponse_privateLinkConfig' - The subnet and the security group that DataSync used to access a VPC
-- endpoint.
--
-- 'status', 'describeAgentResponse_status' - The status of the agent. If the status is ONLINE, then the agent is
-- configured properly and is available to use. The Running status is the
-- normal running status for an agent. If the status is OFFLINE, the
-- agent\'s VM is turned off or the agent is in an unhealthy state. When
-- the issue that caused the unhealthy state is resolved, the agent returns
-- to ONLINE status.
--
-- 'httpStatus', 'describeAgentResponse_httpStatus' - The response's http status code.
newDescribeAgentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAgentResponse
newDescribeAgentResponse pHttpStatus_ =
  DescribeAgentResponse'
    { agentArn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      endpointType = Prelude.Nothing,
      lastConnectionTime = Prelude.Nothing,
      name = Prelude.Nothing,
      privateLinkConfig = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the agent.
describeAgentResponse_agentArn :: Lens.Lens' DescribeAgentResponse (Prelude.Maybe Prelude.Text)
describeAgentResponse_agentArn = Lens.lens (\DescribeAgentResponse' {agentArn} -> agentArn) (\s@DescribeAgentResponse' {} a -> s {agentArn = a} :: DescribeAgentResponse)

-- | The time that the agent was activated (that is, created in your
-- account).
describeAgentResponse_creationTime :: Lens.Lens' DescribeAgentResponse (Prelude.Maybe Prelude.UTCTime)
describeAgentResponse_creationTime = Lens.lens (\DescribeAgentResponse' {creationTime} -> creationTime) (\s@DescribeAgentResponse' {} a -> s {creationTime = a} :: DescribeAgentResponse) Prelude.. Lens.mapping Data._Time

-- | The type of endpoint that your agent is connected to. If the endpoint is
-- a VPC endpoint, the agent is not accessible over the public internet.
describeAgentResponse_endpointType :: Lens.Lens' DescribeAgentResponse (Prelude.Maybe EndpointType)
describeAgentResponse_endpointType = Lens.lens (\DescribeAgentResponse' {endpointType} -> endpointType) (\s@DescribeAgentResponse' {} a -> s {endpointType = a} :: DescribeAgentResponse)

-- | The time that the agent last connected to DataSync.
describeAgentResponse_lastConnectionTime :: Lens.Lens' DescribeAgentResponse (Prelude.Maybe Prelude.UTCTime)
describeAgentResponse_lastConnectionTime = Lens.lens (\DescribeAgentResponse' {lastConnectionTime} -> lastConnectionTime) (\s@DescribeAgentResponse' {} a -> s {lastConnectionTime = a} :: DescribeAgentResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the agent.
describeAgentResponse_name :: Lens.Lens' DescribeAgentResponse (Prelude.Maybe Prelude.Text)
describeAgentResponse_name = Lens.lens (\DescribeAgentResponse' {name} -> name) (\s@DescribeAgentResponse' {} a -> s {name = a} :: DescribeAgentResponse)

-- | The subnet and the security group that DataSync used to access a VPC
-- endpoint.
describeAgentResponse_privateLinkConfig :: Lens.Lens' DescribeAgentResponse (Prelude.Maybe PrivateLinkConfig)
describeAgentResponse_privateLinkConfig = Lens.lens (\DescribeAgentResponse' {privateLinkConfig} -> privateLinkConfig) (\s@DescribeAgentResponse' {} a -> s {privateLinkConfig = a} :: DescribeAgentResponse)

-- | The status of the agent. If the status is ONLINE, then the agent is
-- configured properly and is available to use. The Running status is the
-- normal running status for an agent. If the status is OFFLINE, the
-- agent\'s VM is turned off or the agent is in an unhealthy state. When
-- the issue that caused the unhealthy state is resolved, the agent returns
-- to ONLINE status.
describeAgentResponse_status :: Lens.Lens' DescribeAgentResponse (Prelude.Maybe AgentStatus)
describeAgentResponse_status = Lens.lens (\DescribeAgentResponse' {status} -> status) (\s@DescribeAgentResponse' {} a -> s {status = a} :: DescribeAgentResponse)

-- | The response's http status code.
describeAgentResponse_httpStatus :: Lens.Lens' DescribeAgentResponse Prelude.Int
describeAgentResponse_httpStatus = Lens.lens (\DescribeAgentResponse' {httpStatus} -> httpStatus) (\s@DescribeAgentResponse' {} a -> s {httpStatus = a} :: DescribeAgentResponse)

instance Prelude.NFData DescribeAgentResponse where
  rnf DescribeAgentResponse' {..} =
    Prelude.rnf agentArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf endpointType
      `Prelude.seq` Prelude.rnf lastConnectionTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf privateLinkConfig
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
