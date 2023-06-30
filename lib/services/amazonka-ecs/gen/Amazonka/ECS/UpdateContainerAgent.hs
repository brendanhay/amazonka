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
-- Module      : Amazonka.ECS.UpdateContainerAgent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the Amazon ECS container agent on a specified container
-- instance. Updating the Amazon ECS container agent doesn\'t interrupt
-- running tasks or services on the container instance. The process for
-- updating the agent differs depending on whether your container instance
-- was launched with the Amazon ECS-optimized AMI or another operating
-- system.
--
-- The @UpdateContainerAgent@ API isn\'t supported for container instances
-- using the Amazon ECS-optimized Amazon Linux 2 (arm64) AMI. To update the
-- container agent, you can update the @ecs-init@ package. This updates the
-- agent. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/agent-update-ecs-ami.html Updating the Amazon ECS container agent>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- Agent updates with the @UpdateContainerAgent@ API operation do not apply
-- to Windows container instances. We recommend that you launch new
-- container instances to update the agent version in your Windows
-- clusters.
--
-- The @UpdateContainerAgent@ API requires an Amazon ECS-optimized AMI or
-- Amazon Linux AMI with the @ecs-init@ service installed and running. For
-- help updating the Amazon ECS container agent on other operating systems,
-- see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html#manually_update_agent Manually updating the Amazon ECS container agent>
-- in the /Amazon Elastic Container Service Developer Guide/.
module Amazonka.ECS.UpdateContainerAgent
  ( -- * Creating a Request
    UpdateContainerAgent (..),
    newUpdateContainerAgent,

    -- * Request Lenses
    updateContainerAgent_cluster,
    updateContainerAgent_containerInstance,

    -- * Destructuring the Response
    UpdateContainerAgentResponse (..),
    newUpdateContainerAgentResponse,

    -- * Response Lenses
    updateContainerAgentResponse_containerInstance,
    updateContainerAgentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateContainerAgent' smart constructor.
data UpdateContainerAgent = UpdateContainerAgent'
  { -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- your container instance is running on. If you do not specify a cluster,
    -- the default cluster is assumed.
    cluster :: Prelude.Maybe Prelude.Text,
    -- | The container instance ID or full ARN entries for the container instance
    -- where you would like to update the Amazon ECS container agent.
    containerInstance :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateContainerAgent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'updateContainerAgent_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that
-- your container instance is running on. If you do not specify a cluster,
-- the default cluster is assumed.
--
-- 'containerInstance', 'updateContainerAgent_containerInstance' - The container instance ID or full ARN entries for the container instance
-- where you would like to update the Amazon ECS container agent.
newUpdateContainerAgent ::
  -- | 'containerInstance'
  Prelude.Text ->
  UpdateContainerAgent
newUpdateContainerAgent pContainerInstance_ =
  UpdateContainerAgent'
    { cluster = Prelude.Nothing,
      containerInstance = pContainerInstance_
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- your container instance is running on. If you do not specify a cluster,
-- the default cluster is assumed.
updateContainerAgent_cluster :: Lens.Lens' UpdateContainerAgent (Prelude.Maybe Prelude.Text)
updateContainerAgent_cluster = Lens.lens (\UpdateContainerAgent' {cluster} -> cluster) (\s@UpdateContainerAgent' {} a -> s {cluster = a} :: UpdateContainerAgent)

-- | The container instance ID or full ARN entries for the container instance
-- where you would like to update the Amazon ECS container agent.
updateContainerAgent_containerInstance :: Lens.Lens' UpdateContainerAgent Prelude.Text
updateContainerAgent_containerInstance = Lens.lens (\UpdateContainerAgent' {containerInstance} -> containerInstance) (\s@UpdateContainerAgent' {} a -> s {containerInstance = a} :: UpdateContainerAgent)

instance Core.AWSRequest UpdateContainerAgent where
  type
    AWSResponse UpdateContainerAgent =
      UpdateContainerAgentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateContainerAgentResponse'
            Prelude.<$> (x Data..?> "containerInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateContainerAgent where
  hashWithSalt _salt UpdateContainerAgent' {..} =
    _salt
      `Prelude.hashWithSalt` cluster
      `Prelude.hashWithSalt` containerInstance

instance Prelude.NFData UpdateContainerAgent where
  rnf UpdateContainerAgent' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf containerInstance

instance Data.ToHeaders UpdateContainerAgent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.UpdateContainerAgent" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateContainerAgent where
  toJSON UpdateContainerAgent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cluster" Data..=) Prelude.<$> cluster,
            Prelude.Just
              ("containerInstance" Data..= containerInstance)
          ]
      )

instance Data.ToPath UpdateContainerAgent where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateContainerAgent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateContainerAgentResponse' smart constructor.
data UpdateContainerAgentResponse = UpdateContainerAgentResponse'
  { -- | The container instance that the container agent was updated for.
    containerInstance :: Prelude.Maybe ContainerInstance,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateContainerAgentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerInstance', 'updateContainerAgentResponse_containerInstance' - The container instance that the container agent was updated for.
--
-- 'httpStatus', 'updateContainerAgentResponse_httpStatus' - The response's http status code.
newUpdateContainerAgentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateContainerAgentResponse
newUpdateContainerAgentResponse pHttpStatus_ =
  UpdateContainerAgentResponse'
    { containerInstance =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The container instance that the container agent was updated for.
updateContainerAgentResponse_containerInstance :: Lens.Lens' UpdateContainerAgentResponse (Prelude.Maybe ContainerInstance)
updateContainerAgentResponse_containerInstance = Lens.lens (\UpdateContainerAgentResponse' {containerInstance} -> containerInstance) (\s@UpdateContainerAgentResponse' {} a -> s {containerInstance = a} :: UpdateContainerAgentResponse)

-- | The response's http status code.
updateContainerAgentResponse_httpStatus :: Lens.Lens' UpdateContainerAgentResponse Prelude.Int
updateContainerAgentResponse_httpStatus = Lens.lens (\UpdateContainerAgentResponse' {httpStatus} -> httpStatus) (\s@UpdateContainerAgentResponse' {} a -> s {httpStatus = a} :: UpdateContainerAgentResponse)

instance Prelude.NFData UpdateContainerAgentResponse where
  rnf UpdateContainerAgentResponse' {..} =
    Prelude.rnf containerInstance
      `Prelude.seq` Prelude.rnf httpStatus
