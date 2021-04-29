{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeregisterTaskDefinition' smart constructor.
data DeregisterTaskDefinition = DeregisterTaskDefinition'
  { -- | The @family@ and @revision@ (@family:revision@) or full Amazon Resource
    -- Name (ARN) of the task definition to deregister. You must specify a
    -- @revision@.
    taskDefinition :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeregisterTaskDefinition
newDeregisterTaskDefinition pTaskDefinition_ =
  DeregisterTaskDefinition'
    { taskDefinition =
        pTaskDefinition_
    }

-- | The @family@ and @revision@ (@family:revision@) or full Amazon Resource
-- Name (ARN) of the task definition to deregister. You must specify a
-- @revision@.
deregisterTaskDefinition_taskDefinition :: Lens.Lens' DeregisterTaskDefinition Prelude.Text
deregisterTaskDefinition_taskDefinition = Lens.lens (\DeregisterTaskDefinition' {taskDefinition} -> taskDefinition) (\s@DeregisterTaskDefinition' {} a -> s {taskDefinition = a} :: DeregisterTaskDefinition)

instance Prelude.AWSRequest DeregisterTaskDefinition where
  type
    Rs DeregisterTaskDefinition =
      DeregisterTaskDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeregisterTaskDefinitionResponse'
            Prelude.<$> (x Prelude..?> "taskDefinition")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeregisterTaskDefinition

instance Prelude.NFData DeregisterTaskDefinition

instance Prelude.ToHeaders DeregisterTaskDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonEC2ContainerServiceV20141113.DeregisterTaskDefinition" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeregisterTaskDefinition where
  toJSON DeregisterTaskDefinition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("taskDefinition" Prelude..= taskDefinition)
          ]
      )

instance Prelude.ToPath DeregisterTaskDefinition where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeregisterTaskDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterTaskDefinitionResponse' smart constructor.
data DeregisterTaskDefinitionResponse = DeregisterTaskDefinitionResponse'
  { -- | The full description of the deregistered task.
    taskDefinition :: Prelude.Maybe TaskDefinition,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeregisterTaskDefinitionResponse
newDeregisterTaskDefinitionResponse pHttpStatus_ =
  DeregisterTaskDefinitionResponse'
    { taskDefinition =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The full description of the deregistered task.
deregisterTaskDefinitionResponse_taskDefinition :: Lens.Lens' DeregisterTaskDefinitionResponse (Prelude.Maybe TaskDefinition)
deregisterTaskDefinitionResponse_taskDefinition = Lens.lens (\DeregisterTaskDefinitionResponse' {taskDefinition} -> taskDefinition) (\s@DeregisterTaskDefinitionResponse' {} a -> s {taskDefinition = a} :: DeregisterTaskDefinitionResponse)

-- | The response's http status code.
deregisterTaskDefinitionResponse_httpStatus :: Lens.Lens' DeregisterTaskDefinitionResponse Prelude.Int
deregisterTaskDefinitionResponse_httpStatus = Lens.lens (\DeregisterTaskDefinitionResponse' {httpStatus} -> httpStatus) (\s@DeregisterTaskDefinitionResponse' {} a -> s {httpStatus = a} :: DeregisterTaskDefinitionResponse)

instance
  Prelude.NFData
    DeregisterTaskDefinitionResponse
