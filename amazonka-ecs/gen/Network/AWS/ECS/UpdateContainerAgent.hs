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
-- Module      : Network.AWS.ECS.UpdateContainerAgent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the Amazon ECS container agent on a specified container
-- instance. Updating the Amazon ECS container agent does not interrupt
-- running tasks or services on the container instance. The process for
-- updating the agent differs depending on whether your container instance
-- was launched with the Amazon ECS-optimized AMI or another operating
-- system.
--
-- @UpdateContainerAgent@ requires the Amazon ECS-optimized AMI or Amazon
-- Linux with the @ecs-init@ service installed and running. For help
-- updating the Amazon ECS container agent on other operating systems, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html#manually_update_agent Manually Updating the Amazon ECS Container Agent>
-- in the /Amazon Elastic Container Service Developer Guide/.
module Network.AWS.ECS.UpdateContainerAgent
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

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateContainerAgent' smart constructor.
data UpdateContainerAgent = UpdateContainerAgent'
  { -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- your container instance is running on. If you do not specify a cluster,
    -- the default cluster is assumed.
    cluster :: Prelude.Maybe Prelude.Text,
    -- | The container instance ID or full ARN entries for the container instance
    -- on which you would like to update the Amazon ECS container agent.
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
-- on which you would like to update the Amazon ECS container agent.
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
-- on which you would like to update the Amazon ECS container agent.
updateContainerAgent_containerInstance :: Lens.Lens' UpdateContainerAgent Prelude.Text
updateContainerAgent_containerInstance = Lens.lens (\UpdateContainerAgent' {containerInstance} -> containerInstance) (\s@UpdateContainerAgent' {} a -> s {containerInstance = a} :: UpdateContainerAgent)

instance Core.AWSRequest UpdateContainerAgent where
  type
    AWSResponse UpdateContainerAgent =
      UpdateContainerAgentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateContainerAgentResponse'
            Prelude.<$> (x Core..?> "containerInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateContainerAgent

instance Prelude.NFData UpdateContainerAgent

instance Core.ToHeaders UpdateContainerAgent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.UpdateContainerAgent" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateContainerAgent where
  toJSON UpdateContainerAgent' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("cluster" Core..=) Prelude.<$> cluster,
            Prelude.Just
              ("containerInstance" Core..= containerInstance)
          ]
      )

instance Core.ToPath UpdateContainerAgent where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateContainerAgent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateContainerAgentResponse' smart constructor.
data UpdateContainerAgentResponse = UpdateContainerAgentResponse'
  { -- | The container instance for which the container agent was updated.
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
-- 'containerInstance', 'updateContainerAgentResponse_containerInstance' - The container instance for which the container agent was updated.
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

-- | The container instance for which the container agent was updated.
updateContainerAgentResponse_containerInstance :: Lens.Lens' UpdateContainerAgentResponse (Prelude.Maybe ContainerInstance)
updateContainerAgentResponse_containerInstance = Lens.lens (\UpdateContainerAgentResponse' {containerInstance} -> containerInstance) (\s@UpdateContainerAgentResponse' {} a -> s {containerInstance = a} :: UpdateContainerAgentResponse)

-- | The response's http status code.
updateContainerAgentResponse_httpStatus :: Lens.Lens' UpdateContainerAgentResponse Prelude.Int
updateContainerAgentResponse_httpStatus = Lens.lens (\UpdateContainerAgentResponse' {httpStatus} -> httpStatus) (\s@UpdateContainerAgentResponse' {} a -> s {httpStatus = a} :: UpdateContainerAgentResponse)

instance Prelude.NFData UpdateContainerAgentResponse
