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
-- Module      : Network.AWS.CodeDeploy.BatchGetDeploymentGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more deployment groups.
module Network.AWS.CodeDeploy.BatchGetDeploymentGroups
  ( -- * Creating a Request
    BatchGetDeploymentGroups (..),
    newBatchGetDeploymentGroups,

    -- * Request Lenses
    batchGetDeploymentGroups_applicationName,
    batchGetDeploymentGroups_deploymentGroupNames,

    -- * Destructuring the Response
    BatchGetDeploymentGroupsResponse (..),
    newBatchGetDeploymentGroupsResponse,

    -- * Response Lenses
    batchGetDeploymentGroupsResponse_deploymentGroupsInfo,
    batchGetDeploymentGroupsResponse_errorMessage,
    batchGetDeploymentGroupsResponse_httpStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @BatchGetDeploymentGroups@ operation.
--
-- /See:/ 'newBatchGetDeploymentGroups' smart constructor.
data BatchGetDeploymentGroups = BatchGetDeploymentGroups'
  { -- | The name of an AWS CodeDeploy application associated with the applicable
    -- IAM user or AWS account.
    applicationName :: Core.Text,
    -- | The names of the deployment groups.
    deploymentGroupNames :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchGetDeploymentGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'batchGetDeploymentGroups_applicationName' - The name of an AWS CodeDeploy application associated with the applicable
-- IAM user or AWS account.
--
-- 'deploymentGroupNames', 'batchGetDeploymentGroups_deploymentGroupNames' - The names of the deployment groups.
newBatchGetDeploymentGroups ::
  -- | 'applicationName'
  Core.Text ->
  BatchGetDeploymentGroups
newBatchGetDeploymentGroups pApplicationName_ =
  BatchGetDeploymentGroups'
    { applicationName =
        pApplicationName_,
      deploymentGroupNames = Core.mempty
    }

-- | The name of an AWS CodeDeploy application associated with the applicable
-- IAM user or AWS account.
batchGetDeploymentGroups_applicationName :: Lens.Lens' BatchGetDeploymentGroups Core.Text
batchGetDeploymentGroups_applicationName = Lens.lens (\BatchGetDeploymentGroups' {applicationName} -> applicationName) (\s@BatchGetDeploymentGroups' {} a -> s {applicationName = a} :: BatchGetDeploymentGroups)

-- | The names of the deployment groups.
batchGetDeploymentGroups_deploymentGroupNames :: Lens.Lens' BatchGetDeploymentGroups [Core.Text]
batchGetDeploymentGroups_deploymentGroupNames = Lens.lens (\BatchGetDeploymentGroups' {deploymentGroupNames} -> deploymentGroupNames) (\s@BatchGetDeploymentGroups' {} a -> s {deploymentGroupNames = a} :: BatchGetDeploymentGroups) Core.. Lens._Coerce

instance Core.AWSRequest BatchGetDeploymentGroups where
  type
    AWSResponse BatchGetDeploymentGroups =
      BatchGetDeploymentGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetDeploymentGroupsResponse'
            Core.<$> ( x Core..?> "deploymentGroupsInfo"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "errorMessage")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable BatchGetDeploymentGroups

instance Core.NFData BatchGetDeploymentGroups

instance Core.ToHeaders BatchGetDeploymentGroups where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.BatchGetDeploymentGroups" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON BatchGetDeploymentGroups where
  toJSON BatchGetDeploymentGroups' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("applicationName" Core..= applicationName),
            Core.Just
              ( "deploymentGroupNames"
                  Core..= deploymentGroupNames
              )
          ]
      )

instance Core.ToPath BatchGetDeploymentGroups where
  toPath = Core.const "/"

instance Core.ToQuery BatchGetDeploymentGroups where
  toQuery = Core.const Core.mempty

-- | Represents the output of a @BatchGetDeploymentGroups@ operation.
--
-- /See:/ 'newBatchGetDeploymentGroupsResponse' smart constructor.
data BatchGetDeploymentGroupsResponse = BatchGetDeploymentGroupsResponse'
  { -- | Information about the deployment groups.
    deploymentGroupsInfo :: Core.Maybe [DeploymentGroupInfo],
    -- | Information about errors that might have occurred during the API call.
    errorMessage :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchGetDeploymentGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentGroupsInfo', 'batchGetDeploymentGroupsResponse_deploymentGroupsInfo' - Information about the deployment groups.
--
-- 'errorMessage', 'batchGetDeploymentGroupsResponse_errorMessage' - Information about errors that might have occurred during the API call.
--
-- 'httpStatus', 'batchGetDeploymentGroupsResponse_httpStatus' - The response's http status code.
newBatchGetDeploymentGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BatchGetDeploymentGroupsResponse
newBatchGetDeploymentGroupsResponse pHttpStatus_ =
  BatchGetDeploymentGroupsResponse'
    { deploymentGroupsInfo =
        Core.Nothing,
      errorMessage = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the deployment groups.
batchGetDeploymentGroupsResponse_deploymentGroupsInfo :: Lens.Lens' BatchGetDeploymentGroupsResponse (Core.Maybe [DeploymentGroupInfo])
batchGetDeploymentGroupsResponse_deploymentGroupsInfo = Lens.lens (\BatchGetDeploymentGroupsResponse' {deploymentGroupsInfo} -> deploymentGroupsInfo) (\s@BatchGetDeploymentGroupsResponse' {} a -> s {deploymentGroupsInfo = a} :: BatchGetDeploymentGroupsResponse) Core.. Lens.mapping Lens._Coerce

-- | Information about errors that might have occurred during the API call.
batchGetDeploymentGroupsResponse_errorMessage :: Lens.Lens' BatchGetDeploymentGroupsResponse (Core.Maybe Core.Text)
batchGetDeploymentGroupsResponse_errorMessage = Lens.lens (\BatchGetDeploymentGroupsResponse' {errorMessage} -> errorMessage) (\s@BatchGetDeploymentGroupsResponse' {} a -> s {errorMessage = a} :: BatchGetDeploymentGroupsResponse)

-- | The response's http status code.
batchGetDeploymentGroupsResponse_httpStatus :: Lens.Lens' BatchGetDeploymentGroupsResponse Core.Int
batchGetDeploymentGroupsResponse_httpStatus = Lens.lens (\BatchGetDeploymentGroupsResponse' {httpStatus} -> httpStatus) (\s@BatchGetDeploymentGroupsResponse' {} a -> s {httpStatus = a} :: BatchGetDeploymentGroupsResponse)

instance Core.NFData BatchGetDeploymentGroupsResponse
