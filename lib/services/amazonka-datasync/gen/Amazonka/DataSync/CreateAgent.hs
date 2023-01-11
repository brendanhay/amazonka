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
-- Module      : Amazonka.DataSync.CreateAgent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates an DataSync agent that you have deployed in your storage
-- environment. The activation process associates your agent with your
-- account. In the activation process, you specify information such as the
-- Amazon Web Services Region that you want to activate the agent in. You
-- activate the agent in the Amazon Web Services Region where your target
-- locations (in Amazon S3 or Amazon EFS) reside. Your tasks are created in
-- this Amazon Web Services Region.
--
-- You can activate the agent in a VPC (virtual private cloud) or provide
-- the agent access to a VPC endpoint so you can run tasks without going
-- over the public internet.
--
-- You can use an agent for more than one location. If a task uses multiple
-- agents, all of them need to have status AVAILABLE for the task to run.
-- If you use multiple agents for a source location, the status of all the
-- agents must be AVAILABLE for the task to run.
--
-- Agents are automatically updated by Amazon Web Services on a regular
-- basis, using a mechanism that ensures minimal interruption to your
-- tasks.
module Amazonka.DataSync.CreateAgent
  ( -- * Creating a Request
    CreateAgent (..),
    newCreateAgent,

    -- * Request Lenses
    createAgent_agentName,
    createAgent_securityGroupArns,
    createAgent_subnetArns,
    createAgent_tags,
    createAgent_vpcEndpointId,
    createAgent_activationKey,

    -- * Destructuring the Response
    CreateAgentResponse (..),
    newCreateAgentResponse,

    -- * Response Lenses
    createAgentResponse_agentArn,
    createAgentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | CreateAgentRequest
--
-- /See:/ 'newCreateAgent' smart constructor.
data CreateAgent = CreateAgent'
  { -- | The name you configured for your agent. This value is a text reference
    -- that is used to identify the agent in the console.
    agentName :: Prelude.Maybe Prelude.Text,
    -- | The ARNs of the security groups used to protect your data transfer task
    -- subnets. See
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/API_Ec2Config.html#DataSync-Type-Ec2Config-SecurityGroupArns SecurityGroupArns>.
    securityGroupArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The Amazon Resource Names (ARNs) of the subnets in which DataSync will
    -- create elastic network interfaces for each data transfer task. The agent
    -- that runs a task must be private. When you start a task that is
    -- associated with an agent created in a VPC, or one that has access to an
    -- IP address in a VPC, then the task is also private. In this case,
    -- DataSync creates four network interfaces for each task in your subnet.
    -- For a data transfer to work, the agent must be able to route to all
    -- these four network interfaces.
    subnetArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The key-value pair that represents the tag that you want to associate
    -- with the agent. The value can be an empty string. This value helps you
    -- manage, filter, and search for your agents.
    --
    -- Valid characters for key and value are letters, spaces, and numbers
    -- representable in UTF-8 format, and the following special characters: + -
    -- = . _ : \/ \@.
    tags :: Prelude.Maybe [TagListEntry],
    -- | The ID of the VPC (virtual private cloud) endpoint that the agent has
    -- access to. This is the client-side VPC endpoint, also called a
    -- PrivateLink. If you don\'t have a PrivateLink VPC endpoint, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/endpoint-service.html#create-endpoint-service Creating a VPC Endpoint Service Configuration>
    -- in the Amazon VPC User Guide.
    --
    -- VPC endpoint ID looks like this: @vpce-01234d5aff67890e1@.
    vpcEndpointId :: Prelude.Maybe Prelude.Text,
    -- | Your agent activation key. You can get the activation key either by
    -- sending an HTTP GET request with redirects that enable you to get the
    -- agent IP address (port 80). Alternatively, you can get it from the
    -- DataSync console.
    --
    -- The redirect URL returned in the response provides you the activation
    -- key for your agent in the query string parameter @activationKey@. It
    -- might also include other activation-related parameters; however, these
    -- are merely defaults. The arguments you pass to this API call determine
    -- the actual configuration of your agent.
    --
    -- For more information, see Activating an Agent in the /DataSync User
    -- Guide./
    activationKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAgent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentName', 'createAgent_agentName' - The name you configured for your agent. This value is a text reference
-- that is used to identify the agent in the console.
--
-- 'securityGroupArns', 'createAgent_securityGroupArns' - The ARNs of the security groups used to protect your data transfer task
-- subnets. See
-- <https://docs.aws.amazon.com/datasync/latest/userguide/API_Ec2Config.html#DataSync-Type-Ec2Config-SecurityGroupArns SecurityGroupArns>.
--
-- 'subnetArns', 'createAgent_subnetArns' - The Amazon Resource Names (ARNs) of the subnets in which DataSync will
-- create elastic network interfaces for each data transfer task. The agent
-- that runs a task must be private. When you start a task that is
-- associated with an agent created in a VPC, or one that has access to an
-- IP address in a VPC, then the task is also private. In this case,
-- DataSync creates four network interfaces for each task in your subnet.
-- For a data transfer to work, the agent must be able to route to all
-- these four network interfaces.
--
-- 'tags', 'createAgent_tags' - The key-value pair that represents the tag that you want to associate
-- with the agent. The value can be an empty string. This value helps you
-- manage, filter, and search for your agents.
--
-- Valid characters for key and value are letters, spaces, and numbers
-- representable in UTF-8 format, and the following special characters: + -
-- = . _ : \/ \@.
--
-- 'vpcEndpointId', 'createAgent_vpcEndpointId' - The ID of the VPC (virtual private cloud) endpoint that the agent has
-- access to. This is the client-side VPC endpoint, also called a
-- PrivateLink. If you don\'t have a PrivateLink VPC endpoint, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/endpoint-service.html#create-endpoint-service Creating a VPC Endpoint Service Configuration>
-- in the Amazon VPC User Guide.
--
-- VPC endpoint ID looks like this: @vpce-01234d5aff67890e1@.
--
-- 'activationKey', 'createAgent_activationKey' - Your agent activation key. You can get the activation key either by
-- sending an HTTP GET request with redirects that enable you to get the
-- agent IP address (port 80). Alternatively, you can get it from the
-- DataSync console.
--
-- The redirect URL returned in the response provides you the activation
-- key for your agent in the query string parameter @activationKey@. It
-- might also include other activation-related parameters; however, these
-- are merely defaults. The arguments you pass to this API call determine
-- the actual configuration of your agent.
--
-- For more information, see Activating an Agent in the /DataSync User
-- Guide./
newCreateAgent ::
  -- | 'activationKey'
  Prelude.Text ->
  CreateAgent
newCreateAgent pActivationKey_ =
  CreateAgent'
    { agentName = Prelude.Nothing,
      securityGroupArns = Prelude.Nothing,
      subnetArns = Prelude.Nothing,
      tags = Prelude.Nothing,
      vpcEndpointId = Prelude.Nothing,
      activationKey = pActivationKey_
    }

-- | The name you configured for your agent. This value is a text reference
-- that is used to identify the agent in the console.
createAgent_agentName :: Lens.Lens' CreateAgent (Prelude.Maybe Prelude.Text)
createAgent_agentName = Lens.lens (\CreateAgent' {agentName} -> agentName) (\s@CreateAgent' {} a -> s {agentName = a} :: CreateAgent)

-- | The ARNs of the security groups used to protect your data transfer task
-- subnets. See
-- <https://docs.aws.amazon.com/datasync/latest/userguide/API_Ec2Config.html#DataSync-Type-Ec2Config-SecurityGroupArns SecurityGroupArns>.
createAgent_securityGroupArns :: Lens.Lens' CreateAgent (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createAgent_securityGroupArns = Lens.lens (\CreateAgent' {securityGroupArns} -> securityGroupArns) (\s@CreateAgent' {} a -> s {securityGroupArns = a} :: CreateAgent) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Names (ARNs) of the subnets in which DataSync will
-- create elastic network interfaces for each data transfer task. The agent
-- that runs a task must be private. When you start a task that is
-- associated with an agent created in a VPC, or one that has access to an
-- IP address in a VPC, then the task is also private. In this case,
-- DataSync creates four network interfaces for each task in your subnet.
-- For a data transfer to work, the agent must be able to route to all
-- these four network interfaces.
createAgent_subnetArns :: Lens.Lens' CreateAgent (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createAgent_subnetArns = Lens.lens (\CreateAgent' {subnetArns} -> subnetArns) (\s@CreateAgent' {} a -> s {subnetArns = a} :: CreateAgent) Prelude.. Lens.mapping Lens.coerced

-- | The key-value pair that represents the tag that you want to associate
-- with the agent. The value can be an empty string. This value helps you
-- manage, filter, and search for your agents.
--
-- Valid characters for key and value are letters, spaces, and numbers
-- representable in UTF-8 format, and the following special characters: + -
-- = . _ : \/ \@.
createAgent_tags :: Lens.Lens' CreateAgent (Prelude.Maybe [TagListEntry])
createAgent_tags = Lens.lens (\CreateAgent' {tags} -> tags) (\s@CreateAgent' {} a -> s {tags = a} :: CreateAgent) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the VPC (virtual private cloud) endpoint that the agent has
-- access to. This is the client-side VPC endpoint, also called a
-- PrivateLink. If you don\'t have a PrivateLink VPC endpoint, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/endpoint-service.html#create-endpoint-service Creating a VPC Endpoint Service Configuration>
-- in the Amazon VPC User Guide.
--
-- VPC endpoint ID looks like this: @vpce-01234d5aff67890e1@.
createAgent_vpcEndpointId :: Lens.Lens' CreateAgent (Prelude.Maybe Prelude.Text)
createAgent_vpcEndpointId = Lens.lens (\CreateAgent' {vpcEndpointId} -> vpcEndpointId) (\s@CreateAgent' {} a -> s {vpcEndpointId = a} :: CreateAgent)

-- | Your agent activation key. You can get the activation key either by
-- sending an HTTP GET request with redirects that enable you to get the
-- agent IP address (port 80). Alternatively, you can get it from the
-- DataSync console.
--
-- The redirect URL returned in the response provides you the activation
-- key for your agent in the query string parameter @activationKey@. It
-- might also include other activation-related parameters; however, these
-- are merely defaults. The arguments you pass to this API call determine
-- the actual configuration of your agent.
--
-- For more information, see Activating an Agent in the /DataSync User
-- Guide./
createAgent_activationKey :: Lens.Lens' CreateAgent Prelude.Text
createAgent_activationKey = Lens.lens (\CreateAgent' {activationKey} -> activationKey) (\s@CreateAgent' {} a -> s {activationKey = a} :: CreateAgent)

instance Core.AWSRequest CreateAgent where
  type AWSResponse CreateAgent = CreateAgentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAgentResponse'
            Prelude.<$> (x Data..?> "AgentArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAgent where
  hashWithSalt _salt CreateAgent' {..} =
    _salt `Prelude.hashWithSalt` agentName
      `Prelude.hashWithSalt` securityGroupArns
      `Prelude.hashWithSalt` subnetArns
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcEndpointId
      `Prelude.hashWithSalt` activationKey

instance Prelude.NFData CreateAgent where
  rnf CreateAgent' {..} =
    Prelude.rnf agentName
      `Prelude.seq` Prelude.rnf securityGroupArns
      `Prelude.seq` Prelude.rnf subnetArns
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcEndpointId
      `Prelude.seq` Prelude.rnf activationKey

instance Data.ToHeaders CreateAgent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("FmrsService.CreateAgent" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAgent where
  toJSON CreateAgent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AgentName" Data..=) Prelude.<$> agentName,
            ("SecurityGroupArns" Data..=)
              Prelude.<$> securityGroupArns,
            ("SubnetArns" Data..=) Prelude.<$> subnetArns,
            ("Tags" Data..=) Prelude.<$> tags,
            ("VpcEndpointId" Data..=) Prelude.<$> vpcEndpointId,
            Prelude.Just
              ("ActivationKey" Data..= activationKey)
          ]
      )

instance Data.ToPath CreateAgent where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateAgent where
  toQuery = Prelude.const Prelude.mempty

-- | CreateAgentResponse
--
-- /See:/ 'newCreateAgentResponse' smart constructor.
data CreateAgentResponse = CreateAgentResponse'
  { -- | The Amazon Resource Name (ARN) of the agent. Use the @ListAgents@
    -- operation to return a list of agents for your account and Amazon Web
    -- Services Region.
    agentArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAgentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentArn', 'createAgentResponse_agentArn' - The Amazon Resource Name (ARN) of the agent. Use the @ListAgents@
-- operation to return a list of agents for your account and Amazon Web
-- Services Region.
--
-- 'httpStatus', 'createAgentResponse_httpStatus' - The response's http status code.
newCreateAgentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAgentResponse
newCreateAgentResponse pHttpStatus_ =
  CreateAgentResponse'
    { agentArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the agent. Use the @ListAgents@
-- operation to return a list of agents for your account and Amazon Web
-- Services Region.
createAgentResponse_agentArn :: Lens.Lens' CreateAgentResponse (Prelude.Maybe Prelude.Text)
createAgentResponse_agentArn = Lens.lens (\CreateAgentResponse' {agentArn} -> agentArn) (\s@CreateAgentResponse' {} a -> s {agentArn = a} :: CreateAgentResponse)

-- | The response's http status code.
createAgentResponse_httpStatus :: Lens.Lens' CreateAgentResponse Prelude.Int
createAgentResponse_httpStatus = Lens.lens (\CreateAgentResponse' {httpStatus} -> httpStatus) (\s@CreateAgentResponse' {} a -> s {httpStatus = a} :: CreateAgentResponse)

instance Prelude.NFData CreateAgentResponse where
  rnf CreateAgentResponse' {..} =
    Prelude.rnf agentArn
      `Prelude.seq` Prelude.rnf httpStatus
