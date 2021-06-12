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
-- Module      : Network.AWS.ECS.DeregisterTaskDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters the specified task definition by family and revision. Upon
-- deregistration, the task definition is marked as @INACTIVE@. Existing
-- tasks and services that reference an @INACTIVE@ task definition continue
-- to run without disruption. Existing services that reference an
-- @INACTIVE@ task definition can still scale up or down by modifying the
-- service\'s desired count.
--
-- You cannot use an @INACTIVE@ task definition to run new tasks or create
-- new services, and you cannot update an existing service to reference an
-- @INACTIVE@ task definition. However, there may be up to a 10-minute
-- window following deregistration where these restrictions have not yet
-- taken effect.
--
-- At this time, @INACTIVE@ task definitions remain discoverable in your
-- account indefinitely. However, this behavior is subject to change in the
-- future, so you should not rely on @INACTIVE@ task definitions persisting
-- beyond the lifecycle of any associated tasks and services.
module Network.AWS.ECS.DeregisterTaskDefinition
  ( -- * Creating a Request
    DeregisterTaskDefinition (..),
    newDeregisterTaskDefinition,

    -- * Request Lenses
    deregisterTaskDefinition_taskDefinition,

    -- * Destructuring the Response
    DeregisterTaskDefinitionResponse (..),
    newDeregisterTaskDefinitionResponse,

    -- * Response Lenses
    deregisterTaskDefinitionResponse_taskDefinition,
    deregisterTaskDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeregisterTaskDefinition' smart constructor.
data DeregisterTaskDefinition = DeregisterTaskDefinition'
  { -- | The @family@ and @revision@ (@family:revision@) or full Amazon Resource
    -- Name (ARN) of the task definition to deregister. You must specify a
    -- @revision@.
    taskDefinition :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeregisterTaskDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskDefinition', 'deregisterTaskDefinition_taskDefinition' - The @family@ and @revision@ (@family:revision@) or full Amazon Resource
-- Name (ARN) of the task definition to deregister. You must specify a
-- @revision@.
newDeregisterTaskDefinition ::
  -- | 'taskDefinition'
  Core.Text ->
  DeregisterTaskDefinition
newDeregisterTaskDefinition pTaskDefinition_ =
  DeregisterTaskDefinition'
    { taskDefinition =
        pTaskDefinition_
    }

-- | The @family@ and @revision@ (@family:revision@) or full Amazon Resource
-- Name (ARN) of the task definition to deregister. You must specify a
-- @revision@.
deregisterTaskDefinition_taskDefinition :: Lens.Lens' DeregisterTaskDefinition Core.Text
deregisterTaskDefinition_taskDefinition = Lens.lens (\DeregisterTaskDefinition' {taskDefinition} -> taskDefinition) (\s@DeregisterTaskDefinition' {} a -> s {taskDefinition = a} :: DeregisterTaskDefinition)

instance Core.AWSRequest DeregisterTaskDefinition where
  type
    AWSResponse DeregisterTaskDefinition =
      DeregisterTaskDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeregisterTaskDefinitionResponse'
            Core.<$> (x Core..?> "taskDefinition")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeregisterTaskDefinition

instance Core.NFData DeregisterTaskDefinition

instance Core.ToHeaders DeregisterTaskDefinition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.DeregisterTaskDefinition" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeregisterTaskDefinition where
  toJSON DeregisterTaskDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("taskDefinition" Core..= taskDefinition)
          ]
      )

instance Core.ToPath DeregisterTaskDefinition where
  toPath = Core.const "/"

instance Core.ToQuery DeregisterTaskDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeregisterTaskDefinitionResponse' smart constructor.
data DeregisterTaskDefinitionResponse = DeregisterTaskDefinitionResponse'
  { -- | The full description of the deregistered task.
    taskDefinition :: Core.Maybe TaskDefinition,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeregisterTaskDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskDefinition', 'deregisterTaskDefinitionResponse_taskDefinition' - The full description of the deregistered task.
--
-- 'httpStatus', 'deregisterTaskDefinitionResponse_httpStatus' - The response's http status code.
newDeregisterTaskDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeregisterTaskDefinitionResponse
newDeregisterTaskDefinitionResponse pHttpStatus_ =
  DeregisterTaskDefinitionResponse'
    { taskDefinition =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The full description of the deregistered task.
deregisterTaskDefinitionResponse_taskDefinition :: Lens.Lens' DeregisterTaskDefinitionResponse (Core.Maybe TaskDefinition)
deregisterTaskDefinitionResponse_taskDefinition = Lens.lens (\DeregisterTaskDefinitionResponse' {taskDefinition} -> taskDefinition) (\s@DeregisterTaskDefinitionResponse' {} a -> s {taskDefinition = a} :: DeregisterTaskDefinitionResponse)

-- | The response's http status code.
deregisterTaskDefinitionResponse_httpStatus :: Lens.Lens' DeregisterTaskDefinitionResponse Core.Int
deregisterTaskDefinitionResponse_httpStatus = Lens.lens (\DeregisterTaskDefinitionResponse' {httpStatus} -> httpStatus) (\s@DeregisterTaskDefinitionResponse' {} a -> s {httpStatus = a} :: DeregisterTaskDefinitionResponse)

instance Core.NFData DeregisterTaskDefinitionResponse
