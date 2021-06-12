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
-- Module      : Network.AWS.CodeDeploy.GetDeploymentTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a deployment target.
module Network.AWS.CodeDeploy.GetDeploymentTarget
  ( -- * Creating a Request
    GetDeploymentTarget (..),
    newGetDeploymentTarget,

    -- * Request Lenses
    getDeploymentTarget_deploymentId,
    getDeploymentTarget_targetId,

    -- * Destructuring the Response
    GetDeploymentTargetResponse (..),
    newGetDeploymentTargetResponse,

    -- * Response Lenses
    getDeploymentTargetResponse_deploymentTarget,
    getDeploymentTargetResponse_httpStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDeploymentTarget' smart constructor.
data GetDeploymentTarget = GetDeploymentTarget'
  { -- | The unique ID of a deployment.
    deploymentId :: Core.Maybe Core.Text,
    -- | The unique ID of a deployment target.
    targetId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDeploymentTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentId', 'getDeploymentTarget_deploymentId' - The unique ID of a deployment.
--
-- 'targetId', 'getDeploymentTarget_targetId' - The unique ID of a deployment target.
newGetDeploymentTarget ::
  GetDeploymentTarget
newGetDeploymentTarget =
  GetDeploymentTarget'
    { deploymentId = Core.Nothing,
      targetId = Core.Nothing
    }

-- | The unique ID of a deployment.
getDeploymentTarget_deploymentId :: Lens.Lens' GetDeploymentTarget (Core.Maybe Core.Text)
getDeploymentTarget_deploymentId = Lens.lens (\GetDeploymentTarget' {deploymentId} -> deploymentId) (\s@GetDeploymentTarget' {} a -> s {deploymentId = a} :: GetDeploymentTarget)

-- | The unique ID of a deployment target.
getDeploymentTarget_targetId :: Lens.Lens' GetDeploymentTarget (Core.Maybe Core.Text)
getDeploymentTarget_targetId = Lens.lens (\GetDeploymentTarget' {targetId} -> targetId) (\s@GetDeploymentTarget' {} a -> s {targetId = a} :: GetDeploymentTarget)

instance Core.AWSRequest GetDeploymentTarget where
  type
    AWSResponse GetDeploymentTarget =
      GetDeploymentTargetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeploymentTargetResponse'
            Core.<$> (x Core..?> "deploymentTarget")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetDeploymentTarget

instance Core.NFData GetDeploymentTarget

instance Core.ToHeaders GetDeploymentTarget where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.GetDeploymentTarget" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetDeploymentTarget where
  toJSON GetDeploymentTarget' {..} =
    Core.object
      ( Core.catMaybes
          [ ("deploymentId" Core..=) Core.<$> deploymentId,
            ("targetId" Core..=) Core.<$> targetId
          ]
      )

instance Core.ToPath GetDeploymentTarget where
  toPath = Core.const "/"

instance Core.ToQuery GetDeploymentTarget where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetDeploymentTargetResponse' smart constructor.
data GetDeploymentTargetResponse = GetDeploymentTargetResponse'
  { -- | A deployment target that contains information about a deployment such as
    -- its status, lifecycle events, and when it was last updated. It also
    -- contains metadata about the deployment target. The deployment target
    -- metadata depends on the deployment target\'s type (@instanceTarget@,
    -- @lambdaTarget@, or @ecsTarget@).
    deploymentTarget :: Core.Maybe DeploymentTarget,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDeploymentTargetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentTarget', 'getDeploymentTargetResponse_deploymentTarget' - A deployment target that contains information about a deployment such as
-- its status, lifecycle events, and when it was last updated. It also
-- contains metadata about the deployment target. The deployment target
-- metadata depends on the deployment target\'s type (@instanceTarget@,
-- @lambdaTarget@, or @ecsTarget@).
--
-- 'httpStatus', 'getDeploymentTargetResponse_httpStatus' - The response's http status code.
newGetDeploymentTargetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetDeploymentTargetResponse
newGetDeploymentTargetResponse pHttpStatus_ =
  GetDeploymentTargetResponse'
    { deploymentTarget =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A deployment target that contains information about a deployment such as
-- its status, lifecycle events, and when it was last updated. It also
-- contains metadata about the deployment target. The deployment target
-- metadata depends on the deployment target\'s type (@instanceTarget@,
-- @lambdaTarget@, or @ecsTarget@).
getDeploymentTargetResponse_deploymentTarget :: Lens.Lens' GetDeploymentTargetResponse (Core.Maybe DeploymentTarget)
getDeploymentTargetResponse_deploymentTarget = Lens.lens (\GetDeploymentTargetResponse' {deploymentTarget} -> deploymentTarget) (\s@GetDeploymentTargetResponse' {} a -> s {deploymentTarget = a} :: GetDeploymentTargetResponse)

-- | The response's http status code.
getDeploymentTargetResponse_httpStatus :: Lens.Lens' GetDeploymentTargetResponse Core.Int
getDeploymentTargetResponse_httpStatus = Lens.lens (\GetDeploymentTargetResponse' {httpStatus} -> httpStatus) (\s@GetDeploymentTargetResponse' {} a -> s {httpStatus = a} :: GetDeploymentTargetResponse)

instance Core.NFData GetDeploymentTargetResponse
