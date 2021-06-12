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
-- Module      : Network.AWS.CodeDeploy.PutLifecycleEventHookExecutionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the result of a Lambda validation function. The function validates
-- lifecycle hooks during a deployment that uses the AWS Lambda or Amazon
-- ECS compute platform. For AWS Lambda deployments, the available
-- lifecycle hooks are @BeforeAllowTraffic@ and @AfterAllowTraffic@. For
-- Amazon ECS deployments, the available lifecycle hooks are
-- @BeforeInstall@, @AfterInstall@, @AfterAllowTestTraffic@,
-- @BeforeAllowTraffic@, and @AfterAllowTraffic@. Lambda validation
-- functions return @Succeeded@ or @Failed@. For more information, see
-- <https://docs.aws.amazon.com/codedeploy/latest/userguide/reference-appspec-file-structure-hooks.html#appspec-hooks-lambda AppSpec \'hooks\' Section for an AWS Lambda Deployment>
-- and
-- <https://docs.aws.amazon.com/codedeploy/latest/userguide/reference-appspec-file-structure-hooks.html#appspec-hooks-ecs AppSpec \'hooks\' Section for an Amazon ECS Deployment>.
module Network.AWS.CodeDeploy.PutLifecycleEventHookExecutionStatus
  ( -- * Creating a Request
    PutLifecycleEventHookExecutionStatus (..),
    newPutLifecycleEventHookExecutionStatus,

    -- * Request Lenses
    putLifecycleEventHookExecutionStatus_deploymentId,
    putLifecycleEventHookExecutionStatus_status,
    putLifecycleEventHookExecutionStatus_lifecycleEventHookExecutionId,

    -- * Destructuring the Response
    PutLifecycleEventHookExecutionStatusResponse (..),
    newPutLifecycleEventHookExecutionStatusResponse,

    -- * Response Lenses
    putLifecycleEventHookExecutionStatusResponse_lifecycleEventHookExecutionId,
    putLifecycleEventHookExecutionStatusResponse_httpStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutLifecycleEventHookExecutionStatus' smart constructor.
data PutLifecycleEventHookExecutionStatus = PutLifecycleEventHookExecutionStatus'
  { -- | The unique ID of a deployment. Pass this ID to a Lambda function that
    -- validates a deployment lifecycle event.
    deploymentId :: Core.Maybe Core.Text,
    -- | The result of a Lambda function that validates a deployment lifecycle
    -- event (@Succeeded@ or @Failed@).
    status :: Core.Maybe LifecycleEventStatus,
    -- | The execution ID of a deployment\'s lifecycle hook. A deployment
    -- lifecycle hook is specified in the @hooks@ section of the AppSpec file.
    lifecycleEventHookExecutionId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutLifecycleEventHookExecutionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentId', 'putLifecycleEventHookExecutionStatus_deploymentId' - The unique ID of a deployment. Pass this ID to a Lambda function that
-- validates a deployment lifecycle event.
--
-- 'status', 'putLifecycleEventHookExecutionStatus_status' - The result of a Lambda function that validates a deployment lifecycle
-- event (@Succeeded@ or @Failed@).
--
-- 'lifecycleEventHookExecutionId', 'putLifecycleEventHookExecutionStatus_lifecycleEventHookExecutionId' - The execution ID of a deployment\'s lifecycle hook. A deployment
-- lifecycle hook is specified in the @hooks@ section of the AppSpec file.
newPutLifecycleEventHookExecutionStatus ::
  PutLifecycleEventHookExecutionStatus
newPutLifecycleEventHookExecutionStatus =
  PutLifecycleEventHookExecutionStatus'
    { deploymentId =
        Core.Nothing,
      status = Core.Nothing,
      lifecycleEventHookExecutionId =
        Core.Nothing
    }

-- | The unique ID of a deployment. Pass this ID to a Lambda function that
-- validates a deployment lifecycle event.
putLifecycleEventHookExecutionStatus_deploymentId :: Lens.Lens' PutLifecycleEventHookExecutionStatus (Core.Maybe Core.Text)
putLifecycleEventHookExecutionStatus_deploymentId = Lens.lens (\PutLifecycleEventHookExecutionStatus' {deploymentId} -> deploymentId) (\s@PutLifecycleEventHookExecutionStatus' {} a -> s {deploymentId = a} :: PutLifecycleEventHookExecutionStatus)

-- | The result of a Lambda function that validates a deployment lifecycle
-- event (@Succeeded@ or @Failed@).
putLifecycleEventHookExecutionStatus_status :: Lens.Lens' PutLifecycleEventHookExecutionStatus (Core.Maybe LifecycleEventStatus)
putLifecycleEventHookExecutionStatus_status = Lens.lens (\PutLifecycleEventHookExecutionStatus' {status} -> status) (\s@PutLifecycleEventHookExecutionStatus' {} a -> s {status = a} :: PutLifecycleEventHookExecutionStatus)

-- | The execution ID of a deployment\'s lifecycle hook. A deployment
-- lifecycle hook is specified in the @hooks@ section of the AppSpec file.
putLifecycleEventHookExecutionStatus_lifecycleEventHookExecutionId :: Lens.Lens' PutLifecycleEventHookExecutionStatus (Core.Maybe Core.Text)
putLifecycleEventHookExecutionStatus_lifecycleEventHookExecutionId = Lens.lens (\PutLifecycleEventHookExecutionStatus' {lifecycleEventHookExecutionId} -> lifecycleEventHookExecutionId) (\s@PutLifecycleEventHookExecutionStatus' {} a -> s {lifecycleEventHookExecutionId = a} :: PutLifecycleEventHookExecutionStatus)

instance
  Core.AWSRequest
    PutLifecycleEventHookExecutionStatus
  where
  type
    AWSResponse PutLifecycleEventHookExecutionStatus =
      PutLifecycleEventHookExecutionStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutLifecycleEventHookExecutionStatusResponse'
            Core.<$> (x Core..?> "lifecycleEventHookExecutionId")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    PutLifecycleEventHookExecutionStatus

instance
  Core.NFData
    PutLifecycleEventHookExecutionStatus

instance
  Core.ToHeaders
    PutLifecycleEventHookExecutionStatus
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.PutLifecycleEventHookExecutionStatus" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    PutLifecycleEventHookExecutionStatus
  where
  toJSON PutLifecycleEventHookExecutionStatus' {..} =
    Core.object
      ( Core.catMaybes
          [ ("deploymentId" Core..=) Core.<$> deploymentId,
            ("status" Core..=) Core.<$> status,
            ("lifecycleEventHookExecutionId" Core..=)
              Core.<$> lifecycleEventHookExecutionId
          ]
      )

instance
  Core.ToPath
    PutLifecycleEventHookExecutionStatus
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    PutLifecycleEventHookExecutionStatus
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutLifecycleEventHookExecutionStatusResponse' smart constructor.
data PutLifecycleEventHookExecutionStatusResponse = PutLifecycleEventHookExecutionStatusResponse'
  { -- | The execution ID of the lifecycle event hook. A hook is specified in the
    -- @hooks@ section of the deployment\'s AppSpec file.
    lifecycleEventHookExecutionId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutLifecycleEventHookExecutionStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lifecycleEventHookExecutionId', 'putLifecycleEventHookExecutionStatusResponse_lifecycleEventHookExecutionId' - The execution ID of the lifecycle event hook. A hook is specified in the
-- @hooks@ section of the deployment\'s AppSpec file.
--
-- 'httpStatus', 'putLifecycleEventHookExecutionStatusResponse_httpStatus' - The response's http status code.
newPutLifecycleEventHookExecutionStatusResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutLifecycleEventHookExecutionStatusResponse
newPutLifecycleEventHookExecutionStatusResponse
  pHttpStatus_ =
    PutLifecycleEventHookExecutionStatusResponse'
      { lifecycleEventHookExecutionId =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The execution ID of the lifecycle event hook. A hook is specified in the
-- @hooks@ section of the deployment\'s AppSpec file.
putLifecycleEventHookExecutionStatusResponse_lifecycleEventHookExecutionId :: Lens.Lens' PutLifecycleEventHookExecutionStatusResponse (Core.Maybe Core.Text)
putLifecycleEventHookExecutionStatusResponse_lifecycleEventHookExecutionId = Lens.lens (\PutLifecycleEventHookExecutionStatusResponse' {lifecycleEventHookExecutionId} -> lifecycleEventHookExecutionId) (\s@PutLifecycleEventHookExecutionStatusResponse' {} a -> s {lifecycleEventHookExecutionId = a} :: PutLifecycleEventHookExecutionStatusResponse)

-- | The response's http status code.
putLifecycleEventHookExecutionStatusResponse_httpStatus :: Lens.Lens' PutLifecycleEventHookExecutionStatusResponse Core.Int
putLifecycleEventHookExecutionStatusResponse_httpStatus = Lens.lens (\PutLifecycleEventHookExecutionStatusResponse' {httpStatus} -> httpStatus) (\s@PutLifecycleEventHookExecutionStatusResponse' {} a -> s {httpStatus = a} :: PutLifecycleEventHookExecutionStatusResponse)

instance
  Core.NFData
    PutLifecycleEventHookExecutionStatusResponse
