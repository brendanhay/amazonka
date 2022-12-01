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
-- Module      : Amazonka.CodeDeploy.PutLifecycleEventHookExecutionStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the result of a Lambda validation function. The function validates
-- lifecycle hooks during a deployment that uses the Lambda or Amazon ECS
-- compute platform. For Lambda deployments, the available lifecycle hooks
-- are @BeforeAllowTraffic@ and @AfterAllowTraffic@. For Amazon ECS
-- deployments, the available lifecycle hooks are @BeforeInstall@,
-- @AfterInstall@, @AfterAllowTestTraffic@, @BeforeAllowTraffic@, and
-- @AfterAllowTraffic@. Lambda validation functions return @Succeeded@ or
-- @Failed@. For more information, see
-- <https://docs.aws.amazon.com/codedeploy/latest/userguide/reference-appspec-file-structure-hooks.html#appspec-hooks-lambda AppSpec \'hooks\' Section for an Lambda Deployment>
-- and
-- <https://docs.aws.amazon.com/codedeploy/latest/userguide/reference-appspec-file-structure-hooks.html#appspec-hooks-ecs AppSpec \'hooks\' Section for an Amazon ECS Deployment>.
module Amazonka.CodeDeploy.PutLifecycleEventHookExecutionStatus
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

import Amazonka.CodeDeploy.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutLifecycleEventHookExecutionStatus' smart constructor.
data PutLifecycleEventHookExecutionStatus = PutLifecycleEventHookExecutionStatus'
  { -- | The unique ID of a deployment. Pass this ID to a Lambda function that
    -- validates a deployment lifecycle event.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The result of a Lambda function that validates a deployment lifecycle
    -- event. The values listed in __Valid Values__ are valid for lifecycle
    -- statuses in general; however, only @Succeeded@ and @Failed@ can be
    -- passed successfully in your API call.
    status :: Prelude.Maybe LifecycleEventStatus,
    -- | The execution ID of a deployment\'s lifecycle hook. A deployment
    -- lifecycle hook is specified in the @hooks@ section of the AppSpec file.
    lifecycleEventHookExecutionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- event. The values listed in __Valid Values__ are valid for lifecycle
-- statuses in general; however, only @Succeeded@ and @Failed@ can be
-- passed successfully in your API call.
--
-- 'lifecycleEventHookExecutionId', 'putLifecycleEventHookExecutionStatus_lifecycleEventHookExecutionId' - The execution ID of a deployment\'s lifecycle hook. A deployment
-- lifecycle hook is specified in the @hooks@ section of the AppSpec file.
newPutLifecycleEventHookExecutionStatus ::
  PutLifecycleEventHookExecutionStatus
newPutLifecycleEventHookExecutionStatus =
  PutLifecycleEventHookExecutionStatus'
    { deploymentId =
        Prelude.Nothing,
      status = Prelude.Nothing,
      lifecycleEventHookExecutionId =
        Prelude.Nothing
    }

-- | The unique ID of a deployment. Pass this ID to a Lambda function that
-- validates a deployment lifecycle event.
putLifecycleEventHookExecutionStatus_deploymentId :: Lens.Lens' PutLifecycleEventHookExecutionStatus (Prelude.Maybe Prelude.Text)
putLifecycleEventHookExecutionStatus_deploymentId = Lens.lens (\PutLifecycleEventHookExecutionStatus' {deploymentId} -> deploymentId) (\s@PutLifecycleEventHookExecutionStatus' {} a -> s {deploymentId = a} :: PutLifecycleEventHookExecutionStatus)

-- | The result of a Lambda function that validates a deployment lifecycle
-- event. The values listed in __Valid Values__ are valid for lifecycle
-- statuses in general; however, only @Succeeded@ and @Failed@ can be
-- passed successfully in your API call.
putLifecycleEventHookExecutionStatus_status :: Lens.Lens' PutLifecycleEventHookExecutionStatus (Prelude.Maybe LifecycleEventStatus)
putLifecycleEventHookExecutionStatus_status = Lens.lens (\PutLifecycleEventHookExecutionStatus' {status} -> status) (\s@PutLifecycleEventHookExecutionStatus' {} a -> s {status = a} :: PutLifecycleEventHookExecutionStatus)

-- | The execution ID of a deployment\'s lifecycle hook. A deployment
-- lifecycle hook is specified in the @hooks@ section of the AppSpec file.
putLifecycleEventHookExecutionStatus_lifecycleEventHookExecutionId :: Lens.Lens' PutLifecycleEventHookExecutionStatus (Prelude.Maybe Prelude.Text)
putLifecycleEventHookExecutionStatus_lifecycleEventHookExecutionId = Lens.lens (\PutLifecycleEventHookExecutionStatus' {lifecycleEventHookExecutionId} -> lifecycleEventHookExecutionId) (\s@PutLifecycleEventHookExecutionStatus' {} a -> s {lifecycleEventHookExecutionId = a} :: PutLifecycleEventHookExecutionStatus)

instance
  Core.AWSRequest
    PutLifecycleEventHookExecutionStatus
  where
  type
    AWSResponse PutLifecycleEventHookExecutionStatus =
      PutLifecycleEventHookExecutionStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutLifecycleEventHookExecutionStatusResponse'
            Prelude.<$> (x Core..?> "lifecycleEventHookExecutionId")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutLifecycleEventHookExecutionStatus
  where
  hashWithSalt
    _salt
    PutLifecycleEventHookExecutionStatus' {..} =
      _salt `Prelude.hashWithSalt` deploymentId
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` lifecycleEventHookExecutionId

instance
  Prelude.NFData
    PutLifecycleEventHookExecutionStatus
  where
  rnf PutLifecycleEventHookExecutionStatus' {..} =
    Prelude.rnf deploymentId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf lifecycleEventHookExecutionId

instance
  Core.ToHeaders
    PutLifecycleEventHookExecutionStatus
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.PutLifecycleEventHookExecutionStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    PutLifecycleEventHookExecutionStatus
  where
  toJSON PutLifecycleEventHookExecutionStatus' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("deploymentId" Core..=) Prelude.<$> deploymentId,
            ("status" Core..=) Prelude.<$> status,
            ("lifecycleEventHookExecutionId" Core..=)
              Prelude.<$> lifecycleEventHookExecutionId
          ]
      )

instance
  Core.ToPath
    PutLifecycleEventHookExecutionStatus
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    PutLifecycleEventHookExecutionStatus
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutLifecycleEventHookExecutionStatusResponse' smart constructor.
data PutLifecycleEventHookExecutionStatusResponse = PutLifecycleEventHookExecutionStatusResponse'
  { -- | The execution ID of the lifecycle event hook. A hook is specified in the
    -- @hooks@ section of the deployment\'s AppSpec file.
    lifecycleEventHookExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  PutLifecycleEventHookExecutionStatusResponse
newPutLifecycleEventHookExecutionStatusResponse
  pHttpStatus_ =
    PutLifecycleEventHookExecutionStatusResponse'
      { lifecycleEventHookExecutionId =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The execution ID of the lifecycle event hook. A hook is specified in the
-- @hooks@ section of the deployment\'s AppSpec file.
putLifecycleEventHookExecutionStatusResponse_lifecycleEventHookExecutionId :: Lens.Lens' PutLifecycleEventHookExecutionStatusResponse (Prelude.Maybe Prelude.Text)
putLifecycleEventHookExecutionStatusResponse_lifecycleEventHookExecutionId = Lens.lens (\PutLifecycleEventHookExecutionStatusResponse' {lifecycleEventHookExecutionId} -> lifecycleEventHookExecutionId) (\s@PutLifecycleEventHookExecutionStatusResponse' {} a -> s {lifecycleEventHookExecutionId = a} :: PutLifecycleEventHookExecutionStatusResponse)

-- | The response's http status code.
putLifecycleEventHookExecutionStatusResponse_httpStatus :: Lens.Lens' PutLifecycleEventHookExecutionStatusResponse Prelude.Int
putLifecycleEventHookExecutionStatusResponse_httpStatus = Lens.lens (\PutLifecycleEventHookExecutionStatusResponse' {httpStatus} -> httpStatus) (\s@PutLifecycleEventHookExecutionStatusResponse' {} a -> s {httpStatus = a} :: PutLifecycleEventHookExecutionStatusResponse)

instance
  Prelude.NFData
    PutLifecycleEventHookExecutionStatusResponse
  where
  rnf PutLifecycleEventHookExecutionStatusResponse' {..} =
    Prelude.rnf lifecycleEventHookExecutionId
      `Prelude.seq` Prelude.rnf httpStatus
