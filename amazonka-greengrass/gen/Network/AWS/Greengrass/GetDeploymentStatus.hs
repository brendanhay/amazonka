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
-- Module      : Network.AWS.Greengrass.GetDeploymentStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the status of a deployment.
module Network.AWS.Greengrass.GetDeploymentStatus
  ( -- * Creating a Request
    GetDeploymentStatus (..),
    newGetDeploymentStatus,

    -- * Request Lenses
    getDeploymentStatus_groupId,
    getDeploymentStatus_deploymentId,

    -- * Destructuring the Response
    GetDeploymentStatusResponse (..),
    newGetDeploymentStatusResponse,

    -- * Response Lenses
    getDeploymentStatusResponse_deploymentType,
    getDeploymentStatusResponse_updatedAt,
    getDeploymentStatusResponse_deploymentStatus,
    getDeploymentStatusResponse_errorMessage,
    getDeploymentStatusResponse_errorDetails,
    getDeploymentStatusResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDeploymentStatus' smart constructor.
data GetDeploymentStatus = GetDeploymentStatus'
  { -- | The ID of the Greengrass group.
    groupId :: Core.Text,
    -- | The ID of the deployment.
    deploymentId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDeploymentStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupId', 'getDeploymentStatus_groupId' - The ID of the Greengrass group.
--
-- 'deploymentId', 'getDeploymentStatus_deploymentId' - The ID of the deployment.
newGetDeploymentStatus ::
  -- | 'groupId'
  Core.Text ->
  -- | 'deploymentId'
  Core.Text ->
  GetDeploymentStatus
newGetDeploymentStatus pGroupId_ pDeploymentId_ =
  GetDeploymentStatus'
    { groupId = pGroupId_,
      deploymentId = pDeploymentId_
    }

-- | The ID of the Greengrass group.
getDeploymentStatus_groupId :: Lens.Lens' GetDeploymentStatus Core.Text
getDeploymentStatus_groupId = Lens.lens (\GetDeploymentStatus' {groupId} -> groupId) (\s@GetDeploymentStatus' {} a -> s {groupId = a} :: GetDeploymentStatus)

-- | The ID of the deployment.
getDeploymentStatus_deploymentId :: Lens.Lens' GetDeploymentStatus Core.Text
getDeploymentStatus_deploymentId = Lens.lens (\GetDeploymentStatus' {deploymentId} -> deploymentId) (\s@GetDeploymentStatus' {} a -> s {deploymentId = a} :: GetDeploymentStatus)

instance Core.AWSRequest GetDeploymentStatus where
  type
    AWSResponse GetDeploymentStatus =
      GetDeploymentStatusResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeploymentStatusResponse'
            Core.<$> (x Core..?> "DeploymentType")
            Core.<*> (x Core..?> "UpdatedAt")
            Core.<*> (x Core..?> "DeploymentStatus")
            Core.<*> (x Core..?> "ErrorMessage")
            Core.<*> (x Core..?> "ErrorDetails" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetDeploymentStatus

instance Core.NFData GetDeploymentStatus

instance Core.ToHeaders GetDeploymentStatus where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetDeploymentStatus where
  toPath GetDeploymentStatus' {..} =
    Core.mconcat
      [ "/greengrass/groups/",
        Core.toBS groupId,
        "/deployments/",
        Core.toBS deploymentId,
        "/status"
      ]

instance Core.ToQuery GetDeploymentStatus where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetDeploymentStatusResponse' smart constructor.
data GetDeploymentStatusResponse = GetDeploymentStatusResponse'
  { -- | The type of the deployment.
    deploymentType :: Core.Maybe DeploymentType,
    -- | The time, in milliseconds since the epoch, when the deployment status
    -- was updated.
    updatedAt :: Core.Maybe Core.Text,
    -- | The status of the deployment: \'\'InProgress\'\', \'\'Building\'\',
    -- \'\'Success\'\', or \'\'Failure\'\'.
    deploymentStatus :: Core.Maybe Core.Text,
    -- | Error message
    errorMessage :: Core.Maybe Core.Text,
    -- | Error details
    errorDetails :: Core.Maybe [ErrorDetail],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDeploymentStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentType', 'getDeploymentStatusResponse_deploymentType' - The type of the deployment.
--
-- 'updatedAt', 'getDeploymentStatusResponse_updatedAt' - The time, in milliseconds since the epoch, when the deployment status
-- was updated.
--
-- 'deploymentStatus', 'getDeploymentStatusResponse_deploymentStatus' - The status of the deployment: \'\'InProgress\'\', \'\'Building\'\',
-- \'\'Success\'\', or \'\'Failure\'\'.
--
-- 'errorMessage', 'getDeploymentStatusResponse_errorMessage' - Error message
--
-- 'errorDetails', 'getDeploymentStatusResponse_errorDetails' - Error details
--
-- 'httpStatus', 'getDeploymentStatusResponse_httpStatus' - The response's http status code.
newGetDeploymentStatusResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetDeploymentStatusResponse
newGetDeploymentStatusResponse pHttpStatus_ =
  GetDeploymentStatusResponse'
    { deploymentType =
        Core.Nothing,
      updatedAt = Core.Nothing,
      deploymentStatus = Core.Nothing,
      errorMessage = Core.Nothing,
      errorDetails = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The type of the deployment.
getDeploymentStatusResponse_deploymentType :: Lens.Lens' GetDeploymentStatusResponse (Core.Maybe DeploymentType)
getDeploymentStatusResponse_deploymentType = Lens.lens (\GetDeploymentStatusResponse' {deploymentType} -> deploymentType) (\s@GetDeploymentStatusResponse' {} a -> s {deploymentType = a} :: GetDeploymentStatusResponse)

-- | The time, in milliseconds since the epoch, when the deployment status
-- was updated.
getDeploymentStatusResponse_updatedAt :: Lens.Lens' GetDeploymentStatusResponse (Core.Maybe Core.Text)
getDeploymentStatusResponse_updatedAt = Lens.lens (\GetDeploymentStatusResponse' {updatedAt} -> updatedAt) (\s@GetDeploymentStatusResponse' {} a -> s {updatedAt = a} :: GetDeploymentStatusResponse)

-- | The status of the deployment: \'\'InProgress\'\', \'\'Building\'\',
-- \'\'Success\'\', or \'\'Failure\'\'.
getDeploymentStatusResponse_deploymentStatus :: Lens.Lens' GetDeploymentStatusResponse (Core.Maybe Core.Text)
getDeploymentStatusResponse_deploymentStatus = Lens.lens (\GetDeploymentStatusResponse' {deploymentStatus} -> deploymentStatus) (\s@GetDeploymentStatusResponse' {} a -> s {deploymentStatus = a} :: GetDeploymentStatusResponse)

-- | Error message
getDeploymentStatusResponse_errorMessage :: Lens.Lens' GetDeploymentStatusResponse (Core.Maybe Core.Text)
getDeploymentStatusResponse_errorMessage = Lens.lens (\GetDeploymentStatusResponse' {errorMessage} -> errorMessage) (\s@GetDeploymentStatusResponse' {} a -> s {errorMessage = a} :: GetDeploymentStatusResponse)

-- | Error details
getDeploymentStatusResponse_errorDetails :: Lens.Lens' GetDeploymentStatusResponse (Core.Maybe [ErrorDetail])
getDeploymentStatusResponse_errorDetails = Lens.lens (\GetDeploymentStatusResponse' {errorDetails} -> errorDetails) (\s@GetDeploymentStatusResponse' {} a -> s {errorDetails = a} :: GetDeploymentStatusResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getDeploymentStatusResponse_httpStatus :: Lens.Lens' GetDeploymentStatusResponse Core.Int
getDeploymentStatusResponse_httpStatus = Lens.lens (\GetDeploymentStatusResponse' {httpStatus} -> httpStatus) (\s@GetDeploymentStatusResponse' {} a -> s {httpStatus = a} :: GetDeploymentStatusResponse)

instance Core.NFData GetDeploymentStatusResponse
